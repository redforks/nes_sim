# PPU open-bus latch decay: is `PPU_OPEN_BUS_DECAY_TICKS` right?

Research-only. No code changed. Primary sources: local code (`file:line`),
the offline NESdev wiki mirror at `nesdev-wiki/wikipages/` (a November 2024
snapshot, cited as `nesdev-wiki/wikipages/PPU_registers.xhtml`, i.e. the
matching `nesdev.org/wiki/...` page), the nesdev forum mirror at
`archive.nes.science/nesdev-forums/` (canonical thread URLs are given too),
blargg's `ppu_open_bus` test ROM shipped in `~/nes/nes-test-roms/`, and
reference emulator sources (Mesen2, FCEUX).

Access caveat: **`nesdev.org` and `forums.nesdev.org` return HTTP 403
(Cloudflare) from this environment**, and the local wiki mirror is a snapshot
that predates the sentence quoted in ticket #166. Anything quoted from the
*live* wiki or from non-archived forum threads is marked **[snippet]** below:
it comes from a search engine's index of the live page, not from a page fetch.

## 1. What the constant actually means

- `nes_core/src/nes/ppu.rs:31` — `const PPU_OPEN_BUS_DECAY_TICKS: u64 =
  3_221_591 * crate::SYSTEM_CYCLES_PER_PPU_CYCLE;`
- `nes_core/src/lib.rs:57` — `SYSTEM_CYCLES_PER_PPU_CYCLE: u64 = 1`, so the
  multiplier is a no-op and the constant is **3,221,591 PPU dots**.
- `nes_core/src/lib.rs:58` — `PPU_CYCLES_PER_CPU_CYCLE: u64 = 3` (one CPU
  cycle = 3 dots), so 3,221,591 dots = **1,073,864 CPU cycles**.
- CPU clock 1,789,773 Hz (`nes_core/src/nes/apu/mixer.rs:1`,
  `nes_core/src/nes/mapper/mmc5_audio.rs:27`); NTSC dot clock
  21.477272/4 = 5.369318 MHz.

3,221,591 / 5,369,318 = **0.600000 s** exactly (0.6 × 1,789,773 =
1,073,863.8 CPU cycles). So the constant is a faithful encoding of "600 ms";
the question is only whether 600 ms is the right number.

The clock is a cumulative PPU dot counter (`nes_core/src/nes/ppu.rs:228-230`,
incremented once per `Ppu::tick`, `nes_core/src/nes/ppu.rs:751`), so the
deadline arithmetic is in dots and is unit-correct. There is no PAL timing in
`nes_core` (no PAL/NTSC switch found), so the seconds value is unambiguous;
for the record, 600 ms at the PAL dot clock (5.320342 MHz) would be 3,192,205
dots — the repo constant would read 0.605 s if PAL were ever added.

Decay model in code (all per-bit, one shared deadline per refresh):
`nes_core/src/nes/ppu/registers.rs:228-252` (`current_bus_latch`,
`apply_bus_decay`), `:267-277` (`refresh_bus_latch` — all 8 bits get the same
deadline), `:280-298` (`refresh_bus_latch_bits` — only masked bits re-armed).
Field doc: `nes_core/src/nes/ppu/registers.rs:159-169` ("~0.6 s, ~36 frames").

## 2. What the sources actually say

### 2.1 Current wiki wording (the ticket's excerpt) — **[snippet]**

Both PPU pages now read:

> "Reading any readable port (PPUSTATUS, OAMDATA, or PPUDATA) also fills the
> latch with the bits read. Reading a nominally "write-only" register returns
> the latch's current value, as do the unused bits of PPUSTATUS. **At least one
> bit in this value decays after 3ms to 30ms, faster when the PPU is warm**."

Sources: https://www.nesdev.org/wiki/PPU_registers and
https://www.nesdev.org/wiki/PPU_programmer_reference (also mirrored at
`wiki.nesdev.org/w/index.php/PPU_registers`). Snippet returned by the search
index, verbatim; the pages themselves could not be fetched (403). I could not
determine **who** made that edit or what it cites — the wiki's edit history and
`action=raw` are both behind the same 403, and the November 2024 mirror in this
repo does not contain the sentence (see 2.2).

### 2.2 Same sentence in the local mirror (pre-edit) — verified

`nesdev-wiki/wikipages/PPU_registers.xhtml` and
`nesdev-wiki/wikipages/PPU_programmer_reference.xhtml`:

> "This value begins to decay after a frame or so, faster once the PPU has
> warmed up, and it is likely that values with alternating bit patterns (such as
> $55 or $AA) will decay faster. [2]"

So the wiki's own earlier claim was already **sub-frame** ("after a frame or
so"), not 0.6 s, and the mirror's citation [2] is lidnariq's forum post
(2.3). The 3–30 ms rewrite is a quantification of the same claim, not a new
finding. No "3 ms"/"30 ms" string exists anywhere in the 2595-page mirror
(`grep -r "3 ?ms|30 ?ms|milliseconds"` hits only PPU_OAM's "~1.3ms",
Famicom Network System, MK5060 sleep, NSFe) — so the numbers are post-Nov-2024.

### 2.3 lidnariq, "Riding the open bus" (#143801) — verified, full text

Canonical: https://forums.nesdev.org/viewtopic.php?p=143801#p143801
Fetched via mirror: https://archive.nes.science/nesdev-forums/f2/t12549.xhtml

Context: tepples' test ROM reads a `$3F` value written to `$2002` back through
`$2006` mirrors, "and `$3F16` after waiting a frame", expecting `$3F` all five
times. lidnariq's reply:

> "Fascinatingly, after a few resets, the last byte of the five is consistently
> 1F or 0F (randomly one of the two, more often the former). A power cycle with
> three seconds off returns it to $3F, a reset after a few seconds returns it to
> $1F or $0F."
>
> "I think remember previous statements that this "latch" is actually the large
> capacitance of the very long traces from the pins to the various functions
> inside the PPU: what visual2c02 calls _io_db0 through _io_db7. Certain values
> ($55, $AA) should be less stable than others ($FF). _io_db0 is adjacent to
> pclk0 and pclk1, but since those run 180° out of phase, I'm not clear what the
> net effect would be, if anything."
>

What this establishes, with no interpretation needed:

- **No absolute time is given** in the post. The only time anchors are "after
  waiting a frame" and "three seconds off".
- **Decay is per-bit and dissimilar within one byte**: `$3F` (0011_1111) became
  `$1F` (0001_1111, bit 5 lost) or `$0F` (0000_1111, bits 5 and 4 lost) while
  bits 3-0 kept their 1s. So on a warm PPU at least one bit dies **inside one
  frame (16.6 ms)**, while other bits of the same byte survive it. That single
  observation already falsifies a uniform 0.6 s per-bit deadline.
- **Temperature/warm-up matters**: cold (3 s powered off) the latch still held
  `$3F` after a frame; after a few seconds of running it did not.
- **Pattern dependence is a hypothesis**, not a measurement ("should be less
  stable"), attributed to trace capacitance and adjacent `pclk` lines.

### 2.4 blargg's `ppu_open_bus` test ROM — verified (the origin of 0.6 s)

`~/nes/nes-test-roms/ppu_open_bus/readme.txt:8-19` (author:
`ppu_open_bus/readme.txt:90`, "Shay Green <gblargg@gmail.com>"):

> "The PPU effectively has a "decay register", an 8-bit register. Each bit can
> be refreshed with a 0 or 1. If a bit isn't refreshed with a 1 for about 600
> milliseconds, it will decay to 0 (some decay sooner, depending on the NES and
> temperature)."

This is the only place in the corpus that says "600 milliseconds", and the repo
constant is a direct transcription of it. Two things matter:

- "about 600 milliseconds" is stated as the hold time **of a bit that is not
  refreshed**, i.e. an upper bound on how long any 1 survives — "some decay
  sooner" concedes the spread that the wiki now quantifies as 3–30 ms.
- The ROM's assertions are much weaker than 600 ms. `source/ppu_open_bus.s:26-29`:
  `set_test 3,"Decay value should become zero by one second"` then
  `delay_msec 1000` and expect 0; `:42-48` (test 5) and `:63-71` (test 7) poll
  in 100 × 10 ms steps and only require 0 at the end. **Nothing in the ROM
  distinguishes 3 ms from 600 ms.** That is why the repo passes
  `ppu_open_bus` (`nes-test-roms/test_roms.xml:543-546`) with either value, and
  why the current constant is untested.

### 2.5 Hardware measurements in the forums — **[snippet]**

"Tests for ACE speedruns that rely on PPU's internal open bus",
https://forums.nesdev.org/viewtopic.php?t=24639 (not archived in the mirror;
quotes are from the search index):

> "Then divide by the CPU speed of the specific console you're measuring on
> (1.789773MHz for NTSC, 1.662607MHz for PAL, 1.773447 for PAL famiclone, other
> values happen too) … Results are astoundingly variable. My NTSC NES starts
> with fastest raw counts around 0xA00 (21ms) and as it warms up speeds up to
> 0x900 (19ms). Other people testing see fastest decays initially around 0x600
> (12ms) and as theirs warms up it speeds up to around 0x200 (4ms)."

> "The PPU open bus we're talking about here is open bus for the PPU's CPU
> interface and is only affected by register access … Indeed, consider
> lidnariq's test ROM above that is measuring this decay."

This is the only hardware-test data I found, and it brackets the wiki's 3–30 ms
(measured 4–21 ms; 3 ms and 30 ms are the outer envelope). **lidnariq's decay
test ROM in that thread is the hardware test ROM the ticket asks for** — it
reports raw cycle counts per bit, which is exactly the measurement needed.

Related threads (also unarchived, **[snippet]**), useful only as corroboration:

- https://forums.nesdev.org/viewtopic.php?t=24936 — "PPU open bus eventually
  decays, but it takes at least a few milliseconds after the value was last
  set".
- https://forums.nesdev.org/viewtopic.php?t=23768 — "The value is definitely
  known to decay over the course of several seconds, but short-term it should
  be consistent" (i.e. sub-frame reads are reliable even though long-term decay
  is observed; consistent with 2.3's "after waiting a frame" being the first
  visible loss).

### 2.6 The 1.3 ms OAM figure is a different mechanism

`nesdev-wiki/wikipages/PPU_OAM.xhtml` (also `PPU_registers.xhtml`,
"Dynamic RAM decay"):

> "When rendering is turned off, or during vertical blanking between frames, the
> OAM memory will hold stable values for a short period before it begins to
> decay. It will last at least as long as an NTSC vertical blank interval
> (~1.3ms), but not much longer than this."

> "Because OAM decay is more or less random, and with timing that is sensitive to
> temperature or other environmental factors, it is not something a game could
> normally rely on. Most emulators do not simulate the decay, and suffer no
> compatibility problems as a result."

That is **OAM DRAM** (refreshed by rendering), not the `_io_db*` capacitance
latch. Its 1.3 ms lower bound must not be transplanted onto the open-bus latch;
it is listed in the table only because the ticket mentions it.

## 3. Reference emulator implementations

| Emulator | Model | Source |
| --- | --- | --- |
| Mesen2 | Per-bit decay stamps, **3 frames** ("conservative"): `if(_frameCount - _openBusDecayStamp[i] > 3) { /* Decay bits to 0 after 3 frames */ }` with the comment "This is a very conservative estimate, individual bits tend to decay much faster than this." | `/mnt/ext/code/Mesen2/Core/NES/NesPpu.cpp:221-249` (decay at `:241-244`), stamp array `Core/NES/BaseNesPpu.h:113` |
| FCEUX | **No decay at all**: `PPUGenLatch` is a plain byte, refreshed on writes and on `$2002`/`$2004`/`$2007` reads, never aged | `/home/forks/nes/fceux/src/ppu.cpp:350` (decl), `:585-593` (`$2002` read), `:725-731` (`$2004`/`A200x`) |
| ares, Nintendulator | **not verified** — no local checkout, and the ares/Nintendulator paths I probed on GitHub returned 404s (ares has since restructured its NES core out of `ares/ares/component/video/rp2c02/`) | — |

Takeaway: of the two emulators I could read, one uses **50 ms** (Mesen2, 3
frames, deliberately conservative) and one uses **∞** (FCEUX). Nobody uses
600 ms, and nobody models 3–30 ms. Mesen2 is also the repo's stated reference
for other open-bus details (`nes_core/src/nes/ppu.rs:35-39`,
`nes_core/src/nes/ppu.rs:1164-1167`), so its 3-frame figure is the closest
existing precedent for "conservative but finite".

## 4. Verdict

**The constant is wrong, by roughly 20×–200×** — but it is not arbitrary: it is
a faithful transcription of blargg's readme, which describes the *slowest*
hold time rather than the *earliest* decay, and the ticket's "3 ms to 30 ms"
figure is the live wiki's quantification of the same phenomenon (the mirror's
pre-edit wording, "after a frame or so", already pointed sub-frame, and
lidnariq's own data shows a bit dying inside one frame).

- Per-bit decay: **confirmed** (lidnariq 2.3; wiki "at least one bit"; blargg
  "each bit can be refreshed"; repo already models per-bit deadlines).
- A single uniform constant: **not justifiable**. The spread is ~1 frame to
  ~36 frames depending on bit, console and temperature.
- ~0.6 s: defensible **only** as a cold-machine worst-case upper bound, and
  only for the slowest bit. It cannot be defended as the behaviour of a warm
  console, which is the state any game observes after its first few seconds.
- Practical exposure is small (it needs a PPU-register-read-free span of ≥3 ms
  before something reads the latch), but it is real for `$2002` poll loops,
  which refresh only bits 7-5.

## 5. Timing table (NTSC, 5.369318 MHz dot clock, 3 dots/CPU cycle)

One frame = 341 × 262 = 89,342 dots = 16.639 ms
(`nesdev-wiki/wikipages/Cycle_reference_chart.xhtml`: "341 × 262 = 89342").

| Candidate | ms | PPU dots | CPU cycles | Frames | Source |
| --- | --- | --- | --- | --- | --- |
| OAM DRAM floor (different mechanism) | 1.3 | 6,980 | 2,327 | 0.08 | PPU_OAM.xhtml "~1.3ms" |
| Wiki lower bound ("at least one bit") | 3 | 16,108 | 5,369 | 0.18 | live wiki **[snippet]** |
| Fastest measured (warm console) | 4 | 21,477 | 7,159 | 0.24 | forum t=24639 **[snippet]** |
| Mesen2 (3 frames, "conservative") | 50.0 | 268,026 | 89,342 | 3.00 | Mesen2 `NesPpu.cpp:241-244` |
| Fastest measured (cold console) | 12 | 64,432 | 21,477 | 0.72 | forum t=24639 **[snippet]** |
| One frame ("after a frame or so") | 16.64 | 89,342 | 29,781 | 1.00 | wiki mirror 2.2; lidnariq 2.3 |
| Slowest measured (warm console) | 19 | 102,017 | 34,006 | 1.14 | forum t=24639 **[snippet]** |
| Slowest measured (cold console) | 21 | 112,756 | 37,585 | 1.26 | forum t=24639 **[snippet]** |
| Wiki upper bound | 30 | 161,080 | 53,693 | 1.80 | live wiki **[snippet]** |
| **Repo constant today** | **600** | **3,221,591** | **1,073,864** | **36.06** | `nes_core/src/nes/ppu.rs:31`; blargg readme |

## 6. Which real-read behaviours change if the constant is corrected

All PPU-latch sites funnel through three call shapes
(`nes_core/src/nes/ppu.rs:1182-1193`):

- `$2002`: `(status_bits & 0xE0) | (current_bus_latch() & 0x1F)`, then
  `refresh_bus_latch_bits(0xE0, status_bits)` — `nes_core/src/nes/ppu.rs:944-945`.
  **This is the case the ticket is about**: a `bit $2002 / bpl` poll loop
  re-arms bits 7-5 only, so bits 4-0 are pure decay. A poll loop spanning more
  than the decay window must start returning 0 in bits 4-0; at 600 ms it never
  does, at 3–30 ms it does after a few thousand iterations (~1 ms per 600
  iterations of `bit $2002` + `bpl`).
- `$2007`, palette range: `(masked & 0x3F) | (current_bus_latch() & 0xC0)`, then
  `refresh_bus_latch_bits(0x3F, …)` — `nes_core/src/nes/ppu.rs:1169-1170`. After
  a decay window the two high bits read 0 rather than the last byte on the PPU
  data bus (this is `ppu_open_bus` tests 8/9 territory).
- Write-only registers and `$2000/$2001/$2003/$2005/$2006` mirrors: plain
  `current_bus_latch()` — `nes_core/src/nes/ppu.rs:954`. These read the whole
  decaying byte, so they are the most sensitive observers: the
  `$2006`-readback trick that tepples/lidnariq used (2.3) depends on this.
- Every PPU register **write** refreshes all 8 bits
  (`nes_core/src/nes/ppu.rs:998-999`), and `$2004`/non-palette `$2007` reads
  refresh all 8 bits (`:950`, `:1175`) — unaffected by the constant except in
  the long gaps between refreshes.
- Not affected: the **CPU-side** open bus (`open_bus`, `nes_core/src/nes.rs:28`,
  `:296-305`), which is a different latch (last CPU read value; `$4015`
  expansion-status path at `nes_core/src/nes.rs:296`). It has no decay in this
  repo, and no source I found quantifies CPU-bus decay, so "stamper/`$4015`
  open bus" is out of scope for this constant.
- Also worth noting: `GLOSSARY.md:280` documents the constant's role, so a
  change needs a glossary touch-up there, and
  `nes_core/src/nes/ppu/registers.rs:159-169` carries the "~0.6 s, ~36 frames"
  claim that motivates the number.

Compatibility risk of correcting it is low: none of the 20 local commercial
ROMs depend on sub-600 ms survival as far as the sources show, and the only
test ROM in the corpus (`ppu_open_bus`) is insensitive to the value (2.4).

## 7. Recommended constant(s)

One number cannot model this. The minimum honest model is:

1. **Per-bit deadlines in a documented window**, deterministic, e.g. each bit
   gets a fixed offset inside 3–30 ms so that bits do not all expire on the same
   dot (hardware never does). A simple shape that keeps the existing lazy
   `apply_bus_decay` design: keep one `PPU_OPEN_BUS_DECAY_TICKS` and derive
   per-bit ticks as `base + bit_index * step`, with
   `base ≈ 3 ms (16,108 dots)` and `base + 7*step ≈ 30 ms`.
2. **A separate "cold" tier is optional.** If cold-start fidelity matters, note
   that the pre-warm hold time can approach blargg's 600 ms; today's constant
   can be kept as the cold bound and retired once a warm model exists. My
   recommendation is to model warm only (the state games observe) and document
   the cold case as out of scope.
3. **Pick a central value that is defensible in isolation**, so that a reader
   who rejects the per-bit model still gets a sane constant. The measured
   midpoint is ~12 ms (64,432 dots, 21,477 CPU cycles). Mesen2's 3 frames
   (50 ms, 268,026 dots) is the safest "conservative but finite" alternative if
   compat risk is weighted above parity.

Suggested names/values, warm model, if a single number is kept:

```rust
/// PPU open-bus (`_io_db*`) latch decay. Hardware: "At least one bit in this
/// value decays after 3ms to 30ms, faster when the PPU is warm"
/// (nesdev wiki PPU_registers); measured 4–21 ms across consoles (forum
/// t=24639). 12 ms is the measured midpoint; blargg's ppu_open_bus readme's
/// "about 600 milliseconds" is the cold, slowest-bit bound, not the norm.
const PPU_OPEN_BUS_DECAY_TICKS: u64 = 64_432; // 12 ms at 5.369318 MHz
```

and per-bit spread (if item 1 is adopted) as
`PPU_OPEN_BUS_DECAY_TICKS + bit as u64 * PPU_OPEN_BUS_DECAY_STEP_TICKS` with
`PPU_OPEN_BUS_DECAY_STEP_TICKS ≈ 8_060` dots (1.5 ms), giving 12 ms … 22.5 ms
(a subrange of the measured 4–21 ms, deliberately inside the wiki's 3–30 ms).

## 8. Suggested unit-test shape

The current tests pin behaviour, not the value: `test_open_bus_bits_decay_to_zero`
(`nes_core/src/nes/ppu/tests.rs:130-137`) and
`test_status_read_only_refreshes_high_bits` (`:139-152`) only assert at
`PPU_OPEN_BUS_DECAY_TICKS` itself, so they pass for 3 ms, 600 ms or 60 s.
Add:

1. **Scale test** (catches a wrong-magnitude edit):
   `assert!(PPU_OPEN_BUS_DECAY_TICKS * 1_000_000 / 5_369_318 >= 3);`
   `assert!(PPU_OPEN_BUS_DECAY_TICKS * 1_000_000 / 5_369_318 <= 30);`
   (integer ms, no float; the 5,369,318 becomes a named `PPU_DOT_HZ` const).
2. **Sub-frame observation test**: write `$3F` to `$2002`, advance the cycle
   counter by exactly one frame (89,342 dots), and assert that at least one of
   bits 4-5 is gone while reading through a `$2006` mirror — this encodes
   lidnariq's `$3F → $1F/$0F` observation (`archive.nes.science/…/f2/t12549`,
   post #143801) and fails today's 600 ms constant.
3. **Poll-loop test**: `$2002` poll loop refreshed only via `0xE0` for a span of
   the documented upper bound (30 ms = 161,080 dots) must return 0 in bits 4-0
   (`nes_core/src/nes/ppu.rs:944-945`), while a loop shorter than the lower
   bound (3 ms = 16,108 dots) must preserve them.
4. **Per-bit independence** already exists
   (`nes_core/src/nes/ppu/registers/tests.rs:34-62`); extend it to assert two
   bits of the same byte can have different deadlines, which is the property
   that makes a single constant wrong.

## 9. Open questions / what a hardware test ROM must measure

- **Per-bit rates, all 8 bits, one pattern at a time.** Existing data (2.3)
  shows bits 5 and 4 dying before bits 3-0 for `$3F`, but no source gives a
  full per-bit table or a bit ordering — lidnariq's ROM in t=24639 reports
  "raw counts" per bit, but I could not read its output.
- **Pattern dependence.** `$55`/`$AA` "should" decay faster than `$FF` (2.3) —
  unmeasured as far as I can tell. A ROM must test `$FF`, `$00`-refreshed,
  `$55`, `$AA` on the same console and temperature.
- **Read-vs-refresh matrix.** Which reads refresh which bits is already
  documented and implemented (blargg readme table, `ppu_open_bus` tests 6-11);
  a decay ROM must not disturb it, so it should measure through a `$2006`
  mirror (no refresh) rather than `$2002` (refreshes 7-5).
- **Temperature curve.** "Faster when the PPU is warm" is qualitative only. A
  ROM can at least log elapsed time since power-on alongside each measurement
  so warm/cold pairs from the same console are comparable.
- **Upper tail.** Is 600 ms ever right, or is blargg's number a cold-machine
  artifact? A ROM run cold (power off ≥ 30 s) measuring the slowest bit would
  settle it; without that, 600 ms should be treated as folklore and modelled,
  if at all, only as a cold bound.
- **Unverified in this report**: who wrote the live wiki's 3–30 ms sentence and
  what it cites; the full text of t=24639 / t=24936 / t=23768 (403 + not
  archived); ares and Nintendulator behaviour (no local source).

## Sources

- https://www.nesdev.org/wiki/PPU_registers — live wording "decays after 3ms to
  30ms" (search-index snippet; page 403 here)
- https://www.nesdev.org/wiki/PPU_programmer_reference — same sentence
- `nesdev-wiki/wikipages/PPU_registers.xhtml`, `PPU_programmer_reference.xhtml`
  — pre-edit wording "begins to decay after a frame or so … [2]" (verified)
- `nesdev-wiki/wikipages/PPU_OAM.xhtml` — OAM DRAM decay, "~1.3ms" (verified)
- `nesdev-wiki/wikipages/Cycle_reference_chart.xhtml` — "341 × 262 = 89342"
- https://forums.nesdev.org/viewtopic.php?p=143801#p143801 — lidnariq, verified
  via https://archive.nes.science/nesdev-forums/f2/t12549.xhtml
- https://forums.nesdev.org/viewtopic.php?t=24639 — ACE open-bus decay
  measurements (snippet)
- https://forums.nesdev.org/viewtopic.php?t=24936 ,
  https://forums.nesdev.org/viewtopic.php?t=23768 — corroborating (snippet)
- `~/nes/nes-test-roms/ppu_open_bus/readme.txt:8-19` and
  `source/ppu_open_bus.s:26-71` — blargg's decay test ROM (verified)
- `/mnt/ext/code/Mesen2/Core/NES/NesPpu.cpp:221-249` — 3-frame conservative
  decay; `/mnt/ext/code/Mesen2/Core/NES/BaseNesPpu.h:113` — per-bit stamps
- `/home/forks/nes/fceux/src/ppu.cpp:350,585-593,725-731` — `PPUGenLatch`, no
  decay

## Implementation (follow-up, 2026-11-21)

Landed per-bit decay in `nes_core/src/nes/ppu.rs:31-64` and
`nes_core/src/nes/ppu/registers.rs:271-308`:

- `PPU_OPEN_BUS_DECAY_TICKS = 16_108` dots (3 ms, bit 7) and
  `PPU_OPEN_BUS_DECAY_STEP_TICKS = 20_710` dots (last bit 30 ms), applied by
  `open_bus_decay_ticks(bit)`; the `$3F` -> `$1F` -> `$0F` progression is
  reproduced exactly.
- Tests pinning it: `open_bus_decay_window_matches_hardware`,
  `open_bus_decay_is_per_bit_at_one_frame`, `status_poll_loop_decays_low_bits`
  (`nes_core/src/nes/ppu/tests.rs`) and
  `bits_of_one_latch_decay_on_different_deadlines`
  (`nes_core/src/nes/ppu/registers/tests.rs`).
- Changed real-read behaviour: `$2002` bits 4-0 (and `$2007` palette bits 6-7,
  write-only register mirrors) now clear after 3-30 ms without the matching
  refresh instead of surviving ~600 ms. `just passed` still passes, including
  `ppu_open_bus` (which only requires decay within 1 s).
