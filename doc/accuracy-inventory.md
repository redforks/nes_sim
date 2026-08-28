# nes_core accuracy inventory

Research ticket: [redforks/nes_sim#21](https://github.com/redforks/nes_sim/issues/21), part of map [#20](https://github.com/redforks/nes_sim/issues/20). Facts as of commit `754beab` (2026-08-28).

Harness: `nes_cpu_test` (recipes in `justfile`), binary at `/tmp/nes-sim-target/release/nes_cpu_test` (built 2026-08-28, post-`754beab`). Suites marked **[smoke]** were re-run against that binary while writing this document; the rest are cited from commit/issue evidence.

## 1. Wired suites

Everything wired lives under four `passed_*` groups feeding `just passed` (`justfile:671-674`). The `todo_tests` group **no longer exists** — its last entry (zapper_tests) graduated in `54d5e8f`, which deleted the section (`git show 54d5e8f -- justfile`: "todo_tests removed (its only entry graduated)"). There are currently **zero known-failing suites wired into the justfile**.

### CPU — `passed_cpu_tests` (justfile:662)

| Suite | Recipe(s) | Status | Evidence |
|---|---|---|---|
| 6502 functional test | `cpu-test` | green [smoke] | justfile:3-4 |
| instr_misc | `instr_misc` | green | justfile:6 |
| instr_test v3 / v5 (aggregates `all_instrs.nes`) | `instr_test-v3`, `instr_test-v5` | green | justfile:9-13 |
| nes_instr_test (11 rom_singles) | `nes_instr_test` | green | justfile:15-39 |
| instr_timing | `instr_timing` | green | justfile:62 (excluded from todo in `593a9d7`) |
| cpu_interrupts_v2 (aggregate incl. 4-irq_and_dma) | `cpu_interrupts_v2` | green [smoke] | justfile:41 (`811e81b` fixed 4-irq_and_dma) |
| branch_timing_tests 1-3 | `branch_timing_tests` | green | justfile:44-54 |
| cpu_dummy_reads / writes | `cpu_dummy_reads`, `cpu_dummy_writes` | green | justfile:113-123 |
| cpu_exec_space (apu, ppuio) | `cpu_exec_space` | green | justfile:125-132 |
| cpu_reset | `cpu_reset` | green | justfile:134-141 |
| cpu_timing_test6 | `cpu_timing_test6` | green | justfile:143 (excluded from todo in `593a9d7`) |
| nestest | `nestest` | green | justfile:56 |
| blargg_nes_cpu_test5 (cpu.nes + official.nes) | `blargg_nes_cpu_test5` | green [smoke] | justfile:179-186 (`7fbe942`) |
| tetanes flag_concurrency, exec_space | `imported_cpu_misc` | green | justfile:538-548 |

### PPU — `passed_ppu_tests` (justfile:665)

| Suite | Recipe(s) | Status | Evidence |
|---|---|---|---|
| ppu_vbl_nmi (aggregate) | `ppu_vbl_nmi` | green [smoke] | justfile:65 (`d5c33bd` fixed NMI sample phase/dispatch latency/odd-frame skip) |
| vbl_nmi_timing 1-7 | `vbl_nmi_timing` | green [smoke] | justfile:74-111 (`19fa922` fixed $2002/$2000 same-tick races) |
| nmi_sync demo_ntsc | `nmi_sync` | green [smoke] | justfile:68 (`a415a81`) |
| sprite_hit_tests_2005.10.05 1-11 | `sprite_hit_tests` | green | justfile:246-280 |
| sprite_overflow_tests 1-5 | `sprite_overflow_tests` | green | justfile:282-298 (`cc92c90` fixed 3.Timing) |
| oam_read, oam_stress, ppu_open_bus, ppu_read_buffer, scanline | individual | green | justfile:146-165, 364 |
| sprdma_and_dmc_dma | `sprdma_and_dmc_dma` | green [smoke] | justfile:317-323 (`266a8e8`, `0ebb8d1`) |
| blargg_ppu_tests_2005.09.15b (5 ROMs) | `blargg_ppu_tests` | green | justfile:158-194 (`0799625`; power_up_palette passes via blargg's power-up palette table) |
| tetanes spr_hit_extra (8 ROMs) | `spr_hit_extra` | green | justfile:550-575 |
| tetanes visual (240pee, color, ntsc_torture, palette) | `imported_ppu_visual` | green | justfile:577-590 (blessed PNGs) |

### APU — `passed_apu_tests` (justfile:668)

| Suite | Recipe(s) | Status | Evidence |
|---|---|---|---|
| blargg_apu_2005.07.30 — **all 11 ROMs** incl. len_halt_timing, len_reload_timing | `blargg_apu_tests` | green [smoke: 01/10/11 re-run] | justfile:202-236 (`a9b991a`: "all 11 of 2005.07.30 green") |
| apu_mixer (dmc/noise/square/triangle) | `apu_mixer` | green | justfile:325-338 |
| apu_reset 1-6 | `apu_reset` | green | justfile:340-359 |
| apu_test (aggregate) | `apu_test` | green | justfile:361-362 |
| dmc_tests (buffer_retained, latency, status, status_irq) | `dmc_tests` | green | justfile:95-108 (`a54f9b5`) |
| dmc_dma_during_read4 | `dmc_dma_during_read4` | green [smoke] | justfile:300-315 (`1df68c8`) |
| dpcmletterbox (Mesen frame match) | `dpcmletterbox` | green [smoke] | justfile:243-244 (`a510966`) |
| tetanes imported_apu_misc (16 ROMs) | `imported_apu_misc` | green | justfile:485-536 |
| pitch captures (noise/square/triangle) | `passed_audio_manual_ok` | green | justfile:602-615 (`6d7386b`, `e802f5a`) |
| volume_tests vs blessed capture | `volume_tests` | green | justfile:624-629 (`e9b8c42`) |

Note: the justfile comment at `justfile:199-201` still claims len_halt_timing ($03) / len_reload_timing ($04) "stay out of passed_apu_tests until fixed" — stale since `a9b991a` wired all 11 into `blargg_apu_tests`; both re-ran green during this inventory.

### Mapper / input — `passed_mapper`, `passed_input_tests` (justfile:659, 649)

| Suite | Recipe(s) | Status | Evidence |
|---|---|---|---|
| mmc3_test 1-6 (incl. 6-MMC6 rev-A) | `mmc3_tests` | green | justfile:387-406 (`39c1bdd` promoted 6-MMC6) |
| mmc3_test_2 rom_singles 1-6 | `mmc3_test2` | green | justfile:408-427 |
| mmc3_irq_tests 1-4, 6 | `mmc3_irq_tests` | green | justfile:367-385 — **5.MMC3_rev_A excluded** (see below) |
| bntest h/v/aorom | `bntest` | green | justfile:431-441 |
| MMC1_A12 | `mmc1-a12` | green | justfile:443-444 |
| vrctests — all 9 VRC2/4 ROMs | `vrc2-and-4-roms` | green | justfile:446-474 (issue [#6](https://github.com/redforks/nes_sim/issues/6): all PASS at frame 4/5 image match) |
| big_chr_ram (blessed frames 10/80) | `big_chr_ram` | green | justfile:655-656 (`f9640f1`) |
| zapper_flip/light/stream/trigger | `passed_input_tests` | green | justfile:636-649 (`54d5e8f`) |

**Known-failing / excluded, wired harness:**
- `5.MMC3_rev_A` (`mmc3_irq_tests/5.MMC3_rev_A.nes`): recipe `mmc3_irq_test_5` exists (justfile:379-380) but is excluded from the `mmc3_irq_tests` group (justfile:385) since `10f672b`. Re-ran 2026-08-28 with the current binary: **fails/times out**. nes_core implements MMC3 revision-B IRQ semantics by default; `39c1bdd` added a PRG-signature sniffer that routes `6-MMC3_alt`/`6-MMC6` to revision A but did not extend it to this ROM.

### Unit tests
`just unit-test` = `cargo test --lib --workspace` (justfile:59-60); 650 nes_core lib tests as of `754beab`.

## 2. Unwired but on disk

All `test-roms/**/*.nes` (the tetanes import + vrctests + bntest) are wired. Everything below lives in the sibling checkout `../nes-test-roms/` and has **no justfile recipe**. Multi-ROM suites whose *aggregate* is wired (instr_test-v5 singles, ppu_vbl_nmi singles, cpu_interrupts_v2 singles, apu_test rom_singles 1-8, instr_timing 1-2, instr_misc 01-04, nes_instr_test) are counted as wired via the aggregate; the genuinely unwired ROMs are:

- **APU / audio**: `pal_apu_tests/` (10 ROMs — PAL domain, out of scope per map #20); `soundtest/SNDTEST.NES`; `stress/NEStress.NES`; `apu_mixer_recordings/` (reference mp3s for apu_mixer, not ROMs).
- **PPU**: `full_palette/` (3 ROMs); `240pee/240pee-bnrom.nes` (BNROM variant; the NROM `_240pee.nes` is wired); `tvpassfail/tv.nes`; `window5/` (2 ROMs); `scrolltest/scroll.nes`; `spritecans-2011/spritecans.nes`; `scanline-a1/`.
- **CPU/interrupts**: `fdsirqtests/*.fds` (FDS format — harness loads iNES only).
- **Mappers**: `mmc5test/`, `mmc5test_v2/`, `exram/mmc5exram.nes` (MMC5, unimplemented mapper); `m22chrbankingtest/0-127.nes` (VRC2a); `nrom368/` (2 ROMs).
- **Input/peripherals**: `PaddleTest3/PaddleTest.nes` (Vaus); `vaus-test/vaus-test.nes`; `read_joy3/` (4 ROMs).
- **Commercial/demo smoke ROMs**: `other/` (38 ROMs incl. nestest siblings, BladeBuster, minipack…), `nes15-1.0.0/`, `ny2011/`, `blargg_litewall/` (5 ROMs), `stars_se/`, `stomper/`, `tutor/`.
- **PAL variants**: `nmi_sync/demo_pal.nes` (NTSC only is wired).

Map #1 ([#1](https://github.com/redforks/nes_sim/issues/1)) explicitly fenced "unwired-suite audits" and "commercial smoke tests" out of scope; re-opening suite expansion is allowed only if [#29](https://github.com/redforks/nes_sim/issues/29) finds it load-bearing.

## 3. Capture baselines & re-bless flow

Three baseline families, all committed in-repo:

1. **Rendered-frame PNGs** — `nes_cpu_test/src/png-exps/` (30 files): `dpcmletterbox.png` (Mesen-rendered reference, `a510966`), nmi_sync expected frames (×2), vrctest21s1…25s3 (×9, frame 4/5 image match), mmc1_a12-exp, big_chr_ram-f10/f80, tetanes visual (`_240pee`, `color`, `ntsc_torture`, `palette`), zapper blessed frames (×9). Matching is `PngFrameMatch` at frame numbers quoted from `tetanes-core/test_roms/ppu/tests.json` (`nes_cpu_test/src/image.rs:478` region); dpcmletterbox matches "anywhere in the demo's scroll cycle" (justfile:238-244).
2. **Audio WAVs** — `test-roms/apu/captures/{noise,square,triangle}_pitch.wav` (pitch ROMs, 600-frame dumps, byte-compared via `cmp`, justfile:602-613) and `test-roms/apu/volume_tests/volumes.wav` (justfile:624-626). Deterministic across runs (`e802f5a`), so any mixer/APU change that alters these ROMs' output fails the run.
3. **Nametable text verdicts** — blargg 2005 PPU/APU suites decode on-screen result codes (`$01` = pass) via the shared `NametableConsole` magic-word machine (justfile:188-201; `0799625`, `522d925`).

**Re-bless flow:** frames — run with `--dump-frame` (the `FramePngDump` blessing machine, `nes_cpu_test/src/image.rs:478-489`), then commit the new PNG into `nes_cpu_test/src/png-exps/`. Audio — run with `--dump-audio <path>` and commit the WAV over the blessed capture. Because the frame captures assert exact pixels, re-blessing after a legitimate fix is a deliberate review step; the pitch/volume captures are amplitude-sensitive (the 2× gain removal forced a re-bless, `ee87665`).

## 4. Documented residual gaps

- **MMC3 revision-A IRQ ROM** — `5.MMC3_rev_A` fails (verified 2026-08-28); revision-B semantics are the default, revision A reached only via the PRG sniffer for two other ROMs (`39c1bdd`). Not documented anywhere except the justfile exclusion — this inventory is its record.
- **One-dot APU IRQ sampling skew** — `NesMachine::tick` samples `apu_irq_pending()` before `tick_apu()`, delaying frame/DMC IRQ transitions to the CPU's IRQ input by one dot; reordering fails `cpu_interrupts_v2` 3-nmi_and_irq, so the skew is a stated invariant at the decision site, alongside the cartridge-level per-CPU-cycle latch quantization (`754beab`, issue [#19](https://github.com/redforks/nes_sim/issues/19)).
- **Residual VRC prescaler phasing** — exact 114/114/113 prescaler sequencing deferred by ADR-0005 (`docs/adr/0005-vrc4-irq-enable-acknowledge-model.md`); map #1's fog item "cleared untriggered" — zero ROM failures under the uniform 341-dot accumulator ([#6](https://github.com/redforks/nes_sim/issues/6) "Fog outcome"). Facts only; no ticket implements it (map #20 out-of-scope).
- **Sprite per-pixel CHR read cadence** — ADR-0002 (`docs/adr/0002-tile-cache.md`): TileCache scopes to background; "sprite pattern reads remain per-pixel until a follow-up change," so mapper `read_chr` side effects fire per pixel (not per-tile) on the sprite path.
- **ADR-0001** (`docs/adr/0001-secondary-oam.md`): double-buffered secondary OAM — settled decision, no residual gap; scanline-0 sprite evaluation now runs on pre-render 261.
- **ADR-0003** (`docs/adr/0003-cold-start-scanline-261.md`): cold-start at scanline 261; settled (six first-frame correctness gaps closed).
- **doc/dma.md** — reference model (nesdev wiki dump) for OAM/DMC DMA cadence, load/reload scheduling, DMC-during-OAM overlap, register conflicts, and the abort/unexpected-DMA bugs; conformance pins exist in-tree (`682cd2b` "Add DMC-during-OAM conformance tests for doc/dma.md examples", `1df68c8` halt-cycle bus reads / joypad contiguity / $2007 double-read). The DMA *bugs* section (aborted DMA on implicit/explicit stop, unexpected reload DMA on RP2A03H/late-G) is reference material; no in-tree test pins it.
- **Map #1 scope fences** ([#1](https://github.com/redforks/nes_sim/issues/1) resolution): board-level cartridge inaccuracies, unwired-suite audits, commercial smoke tests, External Connector hardware — recorded as out of scope, not gaps owed by this map.
- **Domain language** for mapper IRQs / APU timers: CONTEXT.md (`5c4619f`).

## 5. Candidate acceptance gates for structural refactors

Criteria: fast, deterministic, self-reporting (no re-bless on a faithful refactor), covers CPU/PPU/APU/DMA interrupt+DMA timing.

| Gate | What it pins | Cost | Suitability |
|---|---|---|---|
| `just cpu_interrupts_v2` | CLI latency, NMI/BRK, NMI/IRQ, IRQ+DMA, branch-delays-IRQ | seconds; aggregate ROM, self-reporting | **Primary gate for interrupt-window/IRQ-rail refactors** (#22, #25, #27): covers the CPU↔APU↔PPU interrupt handshake including DMA interaction; the one-dot skew invariant is pinned here |
| `just vbl_nmi_timing ppu_vbl_nmi` | VBL flag races, NMI suppression/enable timing, odd-frame skip | seconds | Primary PPU-interrupt gate for run-to-boundary / per-dot contract work (#24, #27) |
| `just blargg_apu_tests` | APU length counter + frame counter + IRQ timing (11 ROMs) | ~2 s (slowest green 0.07 s, `522d925`) | **Primary APU gate**; `apu_reset`/`dmc_tests` extend to reset & DMC buffer semantics |
| `just dmc_dma_during_read4 sprdma_and_dmc_dma` | DMC DMA halt/dummy/alignment cycles, $4016/$2007 conflicts, OAM+DMC overlap | seconds | **Primary DMA-arbitration gate** for the Bus-module candidate (#26); directly exercises doc/dma.md's cadence model |
| `just mmc3` + `just vrc2-and-4-roms` | Mapper IRQ clocking (A12 filtering, scanline timing, VRC E/A model) | seconds | Gate for PPU-tick/cartridge-seam refactors (#25, cartridge-capability PRD); all green except the documented rev-A ROM |
| `just blargg_ppu_tests blargg_nes_cpu_test5` | PPU register semantics; full CPU incl. unofficial opcodes | seconds | Broad smoke gate; cheap enough for every refactor |
| `just dpcmletterbox nmi_sync` + pitch/volume captures | Pixel-exact frame parity vs Mesen; audio bit-stream parity | seconds each | Strong **but re-bless-coupled**: faithful refactors should pass unchanged; any diff means rendering/audio output changed and needs deliberate re-bless. Use as the accuracy-critical "no output drift" gate, not a hygiene gate |
| `just unit-test` | 650 lib unit tests | seconds | Hygiene gate: internals-only refactors that must not change ROM-visible behavior run the ROM suites above unchanged plus this |

Suggested pairing for the spec's sequencing (feeds [#30](https://github.com/redforks/nes_sim/issues/30), decided in [#29](https://github.com/redforks/nes_sim/issues/29)):
- **Accuracy-critical refactors** (interrupt windows, DMA arbitration, per-dot contract): `cpu_interrupts_v2` + `vbl_nmi_timing` + `ppu_vbl_nmi` + `blargg_apu_tests` + `dmc_dma_during_read4` + `sprdma_and_dmc_dma` + `mmc3` + `vrc2-and-4-roms` — all self-reporting, ~1 min total, no re-bless risk.
- **Hygiene refactors**: `unit-test` + `just blargg_nes_cpu_test5` + `blargg_ppu_tests` as smoke; full `just passed` before landing.
- **Render/audio-touching changes**: add the capture gates (`dpcmletterbox`, `nmi_sync`, pitch/volume `cmp`) — the only wired suites that can fail without a self-reported error code.

Status of the /tmp harness: `/tmp/nes-sim-target/release/nes_cpu_test` exists and was rebuilt post-`754beab` (mtime 2026-08-28 07:36) — all [smoke] rows above ran against it. If it is absent in a future session, `just build_nes_cpu_test` rebuilds it.
