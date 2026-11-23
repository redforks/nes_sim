# #224: root-cause report — nescli vs fceux SMB frame divergence

Parent: #217 (spec), part of #200. No behavior change in `nescli`/`nes_core`.

> Path note: spec left report location open. Maintainer default applied:
> `docs/research/224-root-cause-report.md`, beside #219/#220/#222/#223.
> Harness note: `scripts/frame-hash/fceux_parity/baseline.sh` (#218) already
> merged at `248be9d`; this ticket commits report only.

## 0. Headline and reproduction

One command reproduces every pixel number below (#218):

```bash
scripts/frame-hash/fceux_parity/baseline.sh /tmp/smoke/base600
# outputs: env.txt nescli.log fceux.log nes_rgb/ fc_rgb/ per_frame.tsv offsets.txt summary.txt
```

Inputs: `Super Mario Bros. (World).nes` (NROM, CRC32 `d445f698`),
movie `happylee-supermariobros,warped.fm2` (17868 inputs), palette
`scripts/frame-hash/fceux_parity/fceux-default.pal` (192 bytes raw),
600 frames, fceux 2.6.6 (`SDL_VIDEODRIVER=dummy QT_QPA_PLATFORM=offscreen`),
nescli `run --movie --no-throttle --trim-after 600 --headless --palette`.

Result (`summary.txt`): **identical=158 total=600**.
`per_frame.tsv` runs (`frame: status`):

| frames | status | length |
|---|---|---|
| 0–10 | DIFF | 11 |
| 11–26 | MATCH | 16 |
| 27–28 | DIFF | 2 |
| 29–31 | MATCH | 3 |
| 32–34 | DIFF | 3 |
| 35–41 | MATCH | 7 |
| 42 | DIFF | 1 |
| 43 | MATCH | 1 |
| 44–45 | DIFF | 2 |
| 46–49 | MATCH | 4 |
| 50 | DIFF | 1 |
| 51–177 | MATCH | 127 |
| 178–599 | DIFF | 422 |

`offsets.txt` (`align.py`, exact-match search d in [-40,40]):
intermission 50–196 offsets sweep −40…+1; jump window 197–217 offsets
−31…−19; isolated hits at 211/214/217–220/225/232–234 (`NONE` = no match
in window). Full per-segment offsets in `offsets.txt`.

Frame-cut convention (applies to every RAM number below): nescli
`process_frame` returns at end of scanline 261; fceux loop entered at
scanline 240. An NMI-handler store landing "next frame" under the nescli
cut lands same-frame in fceux. Cut-sensitive findings reported at both
cuts; cut-only findings labelled ARTIFACT.

## 1. Range-to-cause map

DIFF = same-index RGB differ. MATCH = byte-identical RGB.

| range | what | cause | mechanism | evidence | child |
|---|---|---|---|---|---|
| 0–10 DIFF | boot/reset picture | quiescence, no bug claimed | no NMI frames 0,1,2,4,5; skipped-poll frames 9; crossing poll idx 10 sl3 | #221 crossing/no-poll lists | #221 |
| 11–26 MATCH | title, inputs $00 | — (match) | crossing polls idx 14–25 read $00 rows, invisible | #221(a) | #221 |
| 27–28 DIFF | 2 transient frames | upstream flag writes land 1–2f early (write space) | INC/STA first-change cluster 30–41, delta 1 | #220 §2/§4 | #220 |
| 29–31 MATCH | — (match) | — | — | baseline | #218 |
| 32–34 DIFF | 3 transient frames | same cluster as 27–28 | `$0722/$0772/$0774` writes head-31/32 vs fc 33–34 | #220 §2 | #220 |
| 35–41 MATCH | — (match) | — | — | baseline | #218 |
| 42 DIFF | single frame | upstream flags change early | INC `$82F2/$82F5`, STA `$9C09/$8E91` tail-41/head-42 vs fc 42 (0–1) | #220 §2 | #220 |
| 43 MATCH | — (match) | — | — | baseline | #218 |
| 44–45 DIFF | 2 transient frames | upstream init repeats early | `$0774→01/$0772 00→01/$0742→02` tail-43/head-44 vs fc 44 | #220 §2 | #220 |
| 46–49 MATCH | — (match) | — | — | baseline | #218 |
| 50 DIFF | intermission load 1f early | C2 separate cause, not phase | `$07A0` 00→07 nes 50 vs fc 51, cut-independent | #212, #215, #220 §3 | OPEN O1 |
| 51–177 MATCH | 127 intermission frames match | timer divergence pixel-invisible | `$07A0/$077F` RAM ticks differ but screen identical | #203, #219 | — |
| 178–196 DIFF | intermission end gap | C1 prescaler phase (+20) + C2 early load (−1) | ticks every 21f both sides; first tick +20 vs +1; end 197 vs 178 | #203, #211, #219 | F1/F2 |
| 197 DIFF | `$06FC` holdover `$81` | C4 skipped poll (one frame) | NMI ran with no poll; fc g=199 `$01` vs nes 197 `$81`; sole +2-alignment break in 151–239 | #221(b) | F4 |
| 198–209 DIFF | crossing polls, game-invisible | C3 long handler, late poll sl16–18 | NMI entry on time sl241; rows all Right, `$06FC=01` either alignment | #214, #221(a) | none |
| 197–217 DIFF | jump press lost | C5 state-timed, not input-timed | input streams identical 0/600; Mario absent `CE=00` until ~214; press row 197 reaches no Mario | #205, #221(b) | F2 (via C1+C2) |
| 214/215–226 DIFF | Mario stall X=`$28` | C6 start-of-walk stall, 12f vs 7f | fc same stall 196–202 (7f); extra 5f frozen unexplained | #222 §2/§3 | F5 + O4 |
| 231–599 DIFF | scroll + all later frames | consequence of C6 (late X=`$53`) | fc `$071C` leaves 00 at 231; nes X `$2A` then, `$53` only after 261; bg 0px held-equal whenever X/scroll match | #213, #222 §4 | none |
| 232–234 | isolated exact hits (`-25/-27/NONE`) | alignment transients, no new cause | scroll-offset drift; dx = −(`$071C`) exactly | #222 §1 | none |

51–177 MATCH while RAM timers diverge is expected: the `$07A0/$077F`
state is not on screen during intermission. The screen diverges only when
the extra 20 timer frames delay gameplay entry (178+).

## 2. Confirmed causes

### C1. Intermission prescaler phase (+20 frames)

Same 21-frame period both emulators. nescli first `$07A0` tick +20 after
load (70 vs load 50, sl261) vs fceux +1 (52 vs 51). Every later tick +18;
end 197 (`00→07`) vs 178 (`01→07`); length 147 vs 127. `$077F` underflow
069→070 vs 051→052, same period. Cut-independent: sl240 moves first-tick
offset by exactly 1 (18→19) and nothing else (#219 §2). Cause is `$077F`
prescaler phase against the load (#211).
Evidence: `nes_timer.txt`/`n211.txt`/`fc_timer.txt` via
`/tmp/nes217-219-check219.py` (all PASS). Children: #203, #211, #219.

### C2. Intermission load 1 frame early (separate from phase)

`$07A0` 00→07 at nes 50 vs fc 51 at both cuts. Trigger: `STA $07A0`
(`$88A7`, routine `$889D`) pc `$88AA`, fc framecount 50; nescli window
tail-49/head-50, independently pc `$88AA` (#215, corroborated not
re-measured in #220 — nescli STA unlogged, see O1). Phase cannot explain
it (#212). Children: #212, #215, #220.

### C3. NMI poll crossing is real and game-invisible

NMI entry always sl241 when it fires (never late). Polls land sl16–18
past the sl261 cut when the handler runs long: idx 198–209 (NMI 197–208),
also idx 10, 14–25, 47; no-poll frames 9, 13, 28, 46, 197, 357; no-NMI
frames 0, 1, 2, 4, 5. Game-visible effect: none in any crossing frame —
idx 198–209 rows all Right (`$06FC=01` under either alignment); A-press
rows read at normal polls (idx 196 sl249, 217–219 sl250–252).
Mechanism tests (kept): `strobe_fall_freezes_row_for_in_progress_poll`
(`nes_core/src/nes/controller/tests.rs`),
`nmi_poll_after_frame_cut_reads_next_frame_row`
(`nes_core/src/nes_machine.rs`). Children: #214, #221.

### C4. Skipped poll at frame 197 (one-frame `$06FC` break)

`$06FC` strict +2 alignment (fc[g]==nes[g−2]) holds 599/600 over all
frames; the single mismatch is fc 199 (`$01`) vs nes 197 (`$81`
holdover) — NMI ran with no poll that frame. Crossing frames 198–209 are
value-identical under either alignment. Longer handler does not explain
151–239: polls sit sl248–256 elsewhere. Child: #221.

### C5. Jump lag is state-timed

Fceux joy[g] == nes row[g−2], 0/600 mismatches: same button stream.
Fceux Mario grounded g=196–198, jumps g=198 (`CE` B0→AC); nescli Mario
absent (`CE=00`) until ~214–215, first jump 220–221. Row-197 Right+A
reaches no Mario in nescli. The "lag" is the intermission gap (C1+C2),
not input latency. Children: #205, #221.

### C6. Stall starts first, scroll follows (direction fixed)

Nescli X sticks `$28` from 215 (spawn 214); fceux same stall 196–202
(7f). Fceux scroll `$071C` leaves 00 at 231 (X=`$53`); nescli X `$2A`
then. Stall precedes scroll by 16 same-index frames; in fc timeline walk
ends 202, scroll starts 231. Scroll cannot cause the stall. Held-equal
re-check (22 pairs): 0 differing bg pixels whenever X and scroll match;
dx=0 m0≥0.992 pre-scroll, dx=−(`$071C`) after. Background renders
correctly. Children: #213, #222. Remainder OPEN (O4).

### C7. `$8EB6` 420/423-dot stretches are game code (confirmed)

Per routine NMI: 111 hits = 100×54 + 7×423 + 3×420 + one main-game gap;
fceux same multiset, same cyclic order. Budget: span 9621 + 51 last-iter
= 9672 of 10041 routine-minus-normal delta; loop ~59%, records ~37%.
ROM decode verified byte-for-byte (`B0 01 C8 B1 00 8D 07 20 CA D0 F5`;
`BCS $8EA5` taken=420, `AND #$FB` path=423). No correction to #209.
Child: #223 (#209 already closed). Residual OPEN (O5).

### C8. `STA $2007` 12 vs 15 dots: closed, cause found

15-dot readings are the `LDA ($00),Y` page-cross dummy read
(`CrossPageBehavior::FirstClock`, `indexed_with_op_cycle`;
fceux `GetIYRD`+`ADDCYC(1)`), not STA cost. STA absolute always 12 dots;
PPU `$2007` write arm takes no clock. Caveat: pointer values from fceux
memory, not nescli. Child: #223 (#210 already closed).

### ARTIFACTS (no fix)

- `$077F` 0/2 pattern: frame-cut sampling (#211: sl240 → 6 non-1 vs 131).
- `00` end sample at 196: cut artifact (sl240: `01→07` direct, tick train
  identical) (#212).
- Lone zero-NMI sample 45 vs 44: cut shift of #206 frames.
- NMI 197–208 polls landing next frame: cut attribution (#214).

## 3. Open items (each with missing measurement)

- O1 (T1 remainder): nescli `STA $07A0` PC/dot stamp. #220 probe sat in
  `Cpu::write_mem`, blind to STA absolute (`Mem::write` path). Missing:
  `Mcu::write`-level probe with scanline/dot stamp, same ROM both sides
  (w220 mixed `World` vs `Japan,USA`), including `$077F` reload 00→14.
- O2: #215 nescli sample +1 (e.g. `$0722` nes 32 vs write-space head-31)
  unexplained under both cuts. Missing: re-check `process_frame` call
  numbering vs sl261 sampling point.
- O3: `$0789` STA→DEC lag differs (fc next frame, nes same frame; delta 2
  vs 1–2). Missing: dot stamps on the 18-refresh ordering.
- O4 (T6 remainder): extra 5 frozen stall frames (12 vs 7). `$006D=00`,
  `$0070–$0074` constant — bytes that matter outside #213 probe set.
  Missing: per-frame walk-velocity/timer dump nes 214–227 / fc 195–208
  at sl240 cut + NMI code-path taken on 215 vs 216+.
- O5 (T4 remainder): 369-dot unattributed residual in routine-minus-normal
  budget. Missing: account or bound it.
- O6: `$077F` reload vs load coincidence (cut240-49: 00, cut240-50: 14)
  never probed. Fold into O1 re-run.

T2 closed by #221 (single skipped-poll frame + state-timed gap). T3 closed
by #221 (crossing game-invisible). T5 closed by #223 (page-cross cause).

## 4. Follow-up fix drafts (one per confirmed cause, file on agreement)

- F1 (C1 phase): align `$077F` prescaler phase with load — find where
  reload/tick phase is set relative to `$07A0` load order. Evidence #219.
- F2 (C2 early load): trace `STA $07A0` one frame early to its upstream
  flag write (O1 dot-stamps first). Evidence #220.
- F3 (C2 upstream): non-constant 30–41→42–45 write offset — sub-frame
  write timing, not constant lag. Evidence #220 §4.
- F4 (C4 skipped poll): why NMI at frame 197 runs with no poll (branch
  condition). One-frame `$06FC` holdover. Evidence #221(b).
- F5 (C6 stall): extra 5 frozen frames — walk-accel vs idle path / timer
  phase (O4 dump first). Evidence #222 §3.
- No fix: C3 (correct behavior, tested), C7 (game code), C8 (correct),
  ARTIFACTS (convention), 231+ tail (consequence of C1+C2+C6).

## 5. Verification

```bash
grep -rn "DEBUG-" nes_core nescli   # must print nothing
just unit-test                       # must stay green
git status --short                   # report file only
```

Link check (every `docs/research/*.md`, script path, and `/tmp` trace
cited above must resolve; `/tmp` traces are the retained original dumps,
not committed):

```bash
python3 - <<'EOF'
import re, pathlib
p = pathlib.Path("docs/research/224-root-cause-report.md")
t = p.read_text()
refs = set(re.findall(r'(docs/research/[\w.-]+\.md|scripts/[\w/.-]+)', t))
missing = [r for r in refs if not pathlib.Path(r).exists()]
print("missing:", missing or "none")
EOF
```

Worktree `/tmp/nes217-224`, branch `ticket/224-report`, base `cbb4382`.
Report branch/commit/checklist: branch `ticket/224-report`; commit on push;
checklist: spec criteria §"Acceptance criteria" (#224) — range map §1,
evidence links §2, open measurements §3, fix drafts §4, docs-only diff §5.
Do NOT merge to integration, do NOT close #224 (maintainer closes).
