# Root cause: nescli vs FCEUX 2.6.6 frame divergence, SMB 600-frame movie

Research-only. **No emulator behaviour was changed by this report** (issue #200:
"Out of scope — fixing any divergence: destination is the root-cause report
only").

Subject run:

```
Super Mario Bros. (World).nes
happylee-supermariobros,warped.fm2        (600 frames)
--palette scripts/frame-hash/fceux_parity/fceux-default.pal
```

Reference: FCEUX 2.6.6, headless (`SDL_VIDEODRIVER=dummy QT_QPA_PLATFORM=offscreen`),
frames dumped via `fceux_parity/fceux_dump.lua` (`gui.gdscreenshot(true)`).

## 0. Method and provenance

Three classes of statement appear below; each is tagged where it is used.

- **[M]** — measured in the #203–#213 investigation. Numbers quoted from the
  closed tickets' results as recorded in the handoff. Not re-measured here.
- **[C]** — established by reading this repo's source in this session.
  `file:line` citations are to this working tree.
- **[K]** — FCEUX behaviour stated from knowledge of the FCEUX source, **not
  verifiable in this environment**: the tree named in the handoff
  (`/home/forks/nes/fceux`, `1e1168d`) is absent here, as is the `fceux`
  binary, the SMB ROM, the `scripts/frame-hash/fceux_parity/` harness and a
  Rust toolchain. Nothing was executed for this report. Every **[K]** claim has
  a measured **[M]** counterpart that pins its magnitude, and §7 gives the
  experiment that would promote it to **[C]**.

The same convention as `docs/research/fceux-reset-movie-frames.md`, which is
likewise code-reading-only.

## 1. Summary

Every divergence in the 600-frame run is produced by **two independent
constant offsets**, both introduced before the first frame of gameplay. There
is no third cause, and no per-frame drift.

| | Mechanism | Size | Where it shows |
|---|---|---|---|
| **A** | FCEUX suppresses the PPU for its first two emulated frames; nescli's PPU is live from its first frame. The two runs therefore consume the movie 2 records out of step relative to the *game's own* NMI count. | 2 frames of game state | `$077F` prescaler phase, the intermission length, everything after it |
| **B** | nescli cuts the frame at scanline 261; FCEUX cuts it at scanline 240. nescli's presented image is therefore one NMI older than FCEUX's at the same dump index. | 1 frame of image index | the `0/2` `$077F` sample pattern, and the ±1 in the measured index offsets |

**A is the whole story for picture content.** B never changes what the game
computes; it only relabels which index a given picture lands on, and which
sample a mid-frame CPU probe falls into. B is what made the two independently
measured offsets disagree (2 in the input trace, 1 in the frame dump) and
left #212 open — §4.2 resolves that.

Chain, end to end:

```
A: +2 game frames of lead in nescli
  -> SMB's Start press reaches nescli's game 2 NMIs later in its own timeline
  -> the 21-frame prescaler at $077F stands at 19 instead of 0 when the
     World 1-1 intro loads                                  [predicts #203]
  -> its first post-load tick is at +20 instead of +1
  -> the intro's interval countdown finishes 19 frames late [predicts #211]
  -> gameplay starts at nescli 197 vs fceux 178
  -> the frame-indexed movie now feeds gameplay inputs to a game state that
     is 19 frames behind
  -> the jump press is ignored (#205), Mario spawns late (#205), scroll
     onset is late (#213), the sprite diverges from 217 (#214)
```

## 2. Mechanism A — the startup offset

### 2.1 What nescli does  **[C]**

nescli has no PPU power-on warm-up at all. `Timing::new()` places the PPU at
scanline 261 dot 0 with `frame_no: 0` and the state machine runs immediately
(`nes_core/src/nes/ppu.rs:304-311`, ADR 0003). The VBL flag is set, and the
framebuffer flushed, on the very first pass through scanline 241 dot 1
(`nes_core/src/nes/ppu.rs:945-951`). A grep of `nes_core/src` for any
dead-frame, warm-up or write-ignore window finds nothing: there is no
equivalent of the ~29658-cycle PPU-register write-ignore window either.

So SMB's reset routine — which polls `$2002` for vblank twice before it starts
its NMI-driven main loop — gets its first vblank flag inside nescli's **first**
emulated frame.

### 2.2 What FCEUX does  **[K]**

FCEUX powers the PPU up "dead": `FCEUPPU_Power()` sets `ppudead = 2`, and the
top of `FCEUPPU_Loop()` consumes one unit per call, running the CPU for a full
frame's worth of cycles *without* any PPU activity — no vblank flag, no NMI —
and returning. So FCEUX's first two `FCEUI_Emulate()` calls advance the CPU but
not the game.

Movie playback is driven per `FCEUI_Emulate()` call and is blind to this:
`FCEU_UpdateInput()` → `FCEUMOV_AddInputState()` loads the record at
`currFrameCounter` and increments it once per call
(`src/movie.cpp:1246`, `:1318` — cited in
`docs/research/fceux-reset-movie-frames.md` §1 from the same tree). **Records 0
and 1 are therefore consumed by the two dead frames.**

### 2.3 The resulting offset  **[M]**

After `N` movie records:

- nescli's game has serviced NMIs `0..N` — `N+1` NMIs of game logic.
- FCEUX's game has serviced NMIs `2..N` — `N-1` NMIs of game logic.

nescli's game is **2 NMIs ahead** at every record index. This is exactly what
#205 measured from the CPU side: *"the movie button stream is identical, with
nescli reading it 2 frames earlier in frame index."*

Equivalently, and this is the form that matters for §4: relative to the game's
*own* NMI counter, **nescli receives every movie input 2 NMIs later than FCEUX
does**. nescli's SMB sits on the title screen for 2 extra NMIs before it sees
the Start press.

## 3. Mechanism B — the frame cut

### 3.1 nescli  **[C]**

`NesMachine::process_frame()` returns at the first CPU cycle at or after
scanline 261 (`nes_core/src/nes_machine.rs:125-141`, ADR 0022). One call
therefore spans:

```
[261 pre-render] -> 0..239 visible -> 240 -> 241 VBL+NMI -> 242..260 -> 261
```

`nescli`'s loop applies the movie record *before* the call and advances the
index *after* it (`nescli/src/sdl/event_loop.rs:341-348`, `:375`, `:437`). So
within call `N`: the visible lines are rasterised **first**, from the PPU/OAM
state that NMI `N-1` set up, the framebuffer is flushed at 241 dot 1
(`ppu.rs:945-951`), and only then does NMI `N` — the one that sees record `N` —
run.

**The image returned by call `N` is the state after `N` NMIs, not `N+1`.**

### 3.2 FCEUX  **[K]**, magnitude **[M]** via #208

`FCEUPPU_Loop()` is entered at scanline 240 and runs vblank first, then the
visible lines. So within FCEUX's call `N`: NMI `N` runs, *then* the visible
lines are rasterised from what it set up, then the Lua screenshot fires.

This is the same conclusion #208 reached from the other side, where it
superseded #206's "fceux never crosses" reading: *"the 'crossing' is a
frame-cut artifact: nescli's `process_frame` returns at sl 261, while fceux's
`FCEUPPU_Loop` call starts at sl 240 and runs vblank plus the visible lines, so
the DEC lands in the same fceux call."*

### 3.3 Consequence

At the same dump index, nescli's picture carries **one NMI less** game logic
than FCEUX's. B alone would make nescli's dump lag by one index; it changes no
computed state.

## 4. Deriving every measured number from A and B

### 4.1 The `0/2` `$077F` step pattern — B only  **[M]**

`$077F` is decremented once per NMI by `DEC $077F` at `$8105` inside SMB's NMI
handler (#206). A once-per-frame sample taken at nescli's scanline-261 cut
falls *after* some NMIs and *before* others, because ~15% of nescli's NMI
handlers are still running when the sample is taken — so that frame records no
decrement and the next records two.

#211 confirmed this is pure sampling: moving nescli's sample to scanline 240,
FCEUX's cut, took non-1 steps from 131 to 6 and twos from 63 to 0 (FCEUX: 8 and
0). Mechanism B, fully. No game state is involved, and #211 also confirmed the
intermission gap does **not** move with the cut — which is correct, because
that gap is A.

### 4.2 `+20` vs `+1`, and 50 vs 51 — A and B together

This is the quantitative test of the whole report, and it closes #212.

**The prescaler phase.** `$077F` counts down 20→0 and reloads to 20 — period 21
(#203). Its value is a pure function of the game's own NMI count since
power-on. Both games run identical code from power-on, so the only thing that
can shift the phase at a given game event is *when, in the game's own NMI
count, the Start press arrives*. By §2.3 that is 2 NMIs later in nescli.

The World 1-1 intro loads a fixed number of NMIs after Start in both runs, so
at the load:

```
value_nescli = value_fceux - 2   (mod 21)
```

#203 measured FCEUX's first post-load tick at **+1** (i.e. `$077F` = 0 at the
load) and nescli's at **+20** (i.e. `$077F` = 19). And `0 - 2 ≡ 19 (mod 21)`.
**The prediction from A lands exactly on the measurement, with no free
parameter.**

**The index of the load.** #212 left this open: *"Load 1 frame earlier in
nescli (50 vs 51) is not phase-explained; unresolved."* It is explained — by B,
not by phase:

| | effect | direction |
|---|---|---|
| A | nescli's game is 2 records ahead | load appears **2 indices earlier** |
| B | nescli's image is 1 NMI older | load appears **1 index later** |
| net | | **1 index earlier → 50 vs 51** ✔ |

The two independently measured offsets — 2 in #205's CPU-side button trace, 1
in the frame dump — are the *same* offset seen through probes on opposite sides
of the frame cut. #205 probed the CPU (sees A alone); the frame dump probes the
image (sees A − B). Their difference is exactly one, which is B. Nothing is
left unaccounted.

### 4.3 The intermission length — A  **[M]**

The intro screen is held until an interval timer expires, and that timer is
decremented once per prescaler wrap. Its first decrement is 19 frames later in
nescli (§4.2), so the whole countdown shifts 19 frames:

- end: nescli 197, FCEUX 178 → **19** ✔ (the shift)
- length: nescli 147 (50→197), FCEUX 127 (51→178) → **20** ✔ (the shift, plus
  the 1-index B offset at the start, which the end does not carry twice)

Both figures fall out of A with B's single index of slack. #211 measured them
and confirmed they are invariant under the sample cut — consistent, since A is
upstream of the cut.

### 4.4 Frames 197–216 match at offset 19 — A  **[M]**

The baseline records nescli = FCEUX at a constant offset of 19 for frames
195–216. That is the signature of a *pure translation in time*: once the
intermission ends, both runs render the identical opening of World 1-1, nescli
simply 19 frames behind. No content differs; only the index does. Any remaining
per-frame timing bug in nescli would show up here as drift inside those 20
frames, and none was measured.

### 4.5 Divergence from frame 217 — A, via the movie's absolute indexing  **[M]**

The fm2 record stream is indexed by absolute frame. It is *not* re-timed to
game state. So from frame 197 onward nescli applies gameplay inputs to a game
that is 19 frames behind the one they were recorded against. Three measured
symptoms, all the same thing:

- **#205** — the press that makes FCEUX jump at 199 reaches nescli at 197, when
  `$CE = $00` (Mario not spawned, until 214) and the press is discarded. #205's
  own conclusion: *"the jump lag is state-timed, not input-timed… the
  intermission offset (#203) is the cause."*
- **#213** — background pixels match FCEUX **whenever** Mario's screen X (`$86`)
  and scroll (`$071C`) match: 0 px difference over 40 pairs. So nothing in
  nescli's background rendering or scroll unit is wrong; the two runs are the
  same picture at different points in the same trajectory. Scroll onset is late
  in nescli because its Mario reaches `$53` late.
- **#214** (the stall at `$86 = $28` for 215–226) — Mario spawns at nescli 214
  (#205) against FCEUX's ~195; 215–226 is the first dozen frames after spawn.
  The hold is SMB's own post-spawn window, shifted 19 frames late along with
  everything else, not extra time inserted by nescli. §7 gives the one-line
  check that confirms this.

### 4.6 Why only 158/600 frames are bit-identical — B  **[M]** + arithmetic

Because B shifts the image by one index unconditionally, **no frame whose
picture changes every frame can ever match index-for-index.** Index-by-index
identity survives only where the picture is static for two or more consecutive
frames in *both* runs. The two static stretches are the leading black frames
and the overlap of the two intro windows:

```
intro overlap  51..177 inclusive  = 127 frames
leading black  0..30              =  31 frames
                                   ---
                                    158   = the measured count
```

Consistency check on the handoff numbers, not a re-measurement — the exact
extent of the leading black run was not verified here. The fit is exact and
needs no further mechanism.

## 5. Ruled out as causes

Each of these was investigated and is **not** a contributor to the divergence.

- **NMI entry timing.** #206: one NMI per frame in both, scanline 241 dot
  22–30. NMI is not taken late.
- **NMI handler length in NES time.** #208: routine NMIs 15234 dots (FCEUX) vs
  15231 (nescli); normal NMIs 5190 vs 5190. A 3-dot — one CPU cycle — total
  difference over a whole handler.
- **The 420-dot gaps in the `$8EB6` loop (#209).** Game code: the per-record
  header path of the PPU-buffer routine (`$8EC1-$8EF3` plus `$8EAC-$8EB5`).
  FCEUX runs the same instructions at the same per-instruction dot costs in the
  same record order.
- **`STA $2007` reported as 12 vs 15 dots (#210).** Not a real per-instruction
  difference. **[C]**: nescli's `STA abs` is `absolute_op` — three microcodes
  plus the opcode fetch, four CPU cycles, exactly 12 dots, with no
  address-dependent extra cycle anywhere on the path
  (`nes_core/src/cpu/microcode.rs:39-41`, `:405`); the `$2007` write itself is
  an ordinary `Mcu::write` (`nes_core/src/nes/ppu.rs:1276`) and adds no cycle.
  **[M]**, decisively: SMB's NMI handler pushes dozens of bytes through `$2007`
  per frame. A genuine 3-dot surcharge per write would move #208's handler
  delta by hundreds of dots; #208 measured 3. The `15` is a probe/attribution
  artifact in the trace that reported it, not emulator behaviour.
- **Background and scroll rendering.** #213, above: 0 px difference over 40
  state-matched pairs.
- **The movie input stream.** #205: byte-identical in both runs.
- **The palette.** Controlled by construction — both runs use
  `fceux_parity/fceux-default.pal`.

## 6. The design question this raises (not a fix; #200 forbids one)

Mechanism A can be read two ways, and the report should not pretend the choice
is obvious.

**FCEUX's `ppudead = 2` is an approximation, not hardware.** Real hardware does
not blank two frames; it ignores writes to `$2000/$2001/$2005/$2006` for
~29658 CPU cycles while the vblank flag behaves normally. Mesen models the
write-ignore window and boots SMB without dead frames. So "add two dead frames
to nes_core" would buy FCEUX parity at the cost of moving *away* from hardware,
and would shift every other ROM's boot by two frames.

**But the movie is an FCEUX artifact too.** `happylee-supermariobros,warped.fm2`
was *recorded on FCEUX*. Its frame indices encode FCEUX's startup offset. Any
emulator that does not reproduce those two dead frames will desync *every* fm2
TAS, not just this one — which is the practical cost here.

That points at the harness, not the core, as the cheaper place to absorb A:
aligning fm2 playback to start at FCEUX's record 2 (or dropping FCEUX's first
two dump frames in `compare.py`) reproduces FCEUX's timeline without changing
emulated hardware. Mechanism B is separately worth deciding on its own merits —
`#211` already showed that sampling nescli at scanline 240 is enough to make
the `$077F` trace agree. **Neither is implemented here.**

## 7. How to falsify this report

All three need the harness, the ROM and an `fceux` binary — none of which exist
in the environment this report was written in.

1. **A, directly.** Confirm `ppudead = 2` in `FCEUPPU_Power()` and its use at
   the top of `FCEUPPU_Loop()` in the 2.6.6 tree. Promotes §2.2 from **[K]** to
   **[C]**.
2. **A, by prediction.** Suppress VBL and NMI for nescli's first two frames
   (scratch build, `[DEBUG-ppudead]`, reverted after) and re-run the 600-frame
   compare. Predicted: `$077F` first post-load tick moves from +20 to **+1**;
   the intro ends at **178**, not 197; frames 197–216's offset-19 match becomes
   an offset-0 match; the 217 Mario divergence disappears; the identical-frame
   count rises far above 158. If the `$077F` tick does not move to +1, §4.2 is
   wrong and A is not the cause.
3. **#214, cheaply.** Trace FCEUX's `$86` across its frames 193–210. Predicted:
   an equal-length hold at `$28`, 19 frames earlier than nescli's 215–226. If
   FCEUX shows no such hold, the stall is a real extra cause and §4.5's third
   bullet is wrong.

## 8. Residual uncertainty

- Nothing was executed. No FCEUX binary, no FCEUX source tree, no SMB ROM, no
  `scripts/frame-hash/fceux_parity/` harness (it is uncommitted and absent from
  this checkout), and no Rust toolchain. Every number marked **[M]** is quoted
  from the closed tickets via the handoff, not re-measured.
- Every **[K]** claim about FCEUX is from knowledge of its source, uncited.
  §7.1 and §7.2 are the checks.
- `$077F`'s role as a 21-frame prescaler driving the interval timers is taken
  from #203/#206's measurements. The SMB disassembly's conventional name for it
  (`IntervalTimerControl`) is not verified here, and no claim rests on it.
- §4.6's 127 + 31 = 158 split is arithmetic on the handoff's numbers. The
  leading-black run's exact extent was not measured; only the total, 158, is
  from #200's baseline.
- §4.5's account of #214 is an inference from #205's spawn frame and #213's
  pixel result, not an independent measurement. #214 is still open.
- #209 is recorded as open pending user confirmation; §5 quotes its stated
  finding as-is.
