# MMC5 acceptance: what pass means for the three local ROMs

Research for #68. No code changed; this doc gives a later session exact
commands, expected screens/hashes, and the tetanes-harness reuse verdict.

## ROM identity (verified 2026-11-10, `md5sum` + `cmp`)

| Local ROM | md5 | Header (`file`) | Tetanes twin |
|---|---|---|---|
| `../nes-test-roms/mmc5test/mmc5test.nes` (v1, Drag) | `9efb6c197b07f1c1ffe1b5bf294a1325` | 1x16K PRG, 1x8k CHR, mapper 5, H-mirror | none |
| `../nes-test-roms/mmc5test_v2/mmc5test.nes` (v2, AWJ) | `71e3a914deeea5b2cd2d4976f8fae814` | 2x16K PRG, 2x8k CHR, mapper 5, H-mirror | `tetanes-core/test_roms/mapper/m005_exrom/basics.nes`, byte-IDENTICAL (`cmp`) |
| `../nes-test-roms/exram/mmc5exram.nes` (Quietust) | `7d3636151f3693e431ac7fb6b4965faa` | 1x16K PRG, 1x8k CHR, mapper 5, V-mirror | `tetanes-core/test_roms/mapper/m005_exrom/exram.nes`, byte-IDENTICAL (`cmp`) |

Source: `mmc5test_v2/mmc5test.asm` (bass assembler, `arch snes.cpu`);
`exram/mmc5exram.asm` + build recipe `exram/mmc5exram.imk`
(`MAKEINES/mm5exram.nes, MAPR 5, VMIR 1`); v1 has no local source.
Emulator_tests wiki (offline `nesdevwiki.zip`, `wikipages/Emulator_tests.xhtml`):
lists `mmc5test` (Drag, "MMC5 scanline counter"), `mmc5test_v2` (AWJ),
`exram` (Quietust) as visual/manual tests with no automated criteria.

## 1. mmc5test_v2 = tetanes `basics` (joypad matrix ROM)

Screen: "MMC5 CHR BANK TEST / REGISTER SETTINGS" with live fields
OBJ PATTERN TABLE, BG PATTERN TABLE, 0123456789AB bank row,
BANK SET ORDER, CURRENT CHR BANKS (OBJ/BG/DATA hex), CONTROLS help,
4 test sprites. Each NMI does one task; only newly-pressed buttons act,
one button per frame (each handler ends in `rti`).

Joypad matrix (from `mmc5test.asm` NMI handler; Down unused):

| Button | Effect | Visible change |
|---|---|---|
| A | toggle OBJ pattern table (`ppuctrl ^= $08`) | OBJ PATTERN TABLE 000<->001, sprites change |
| B | toggle BG pattern table (`ppuctrl ^= $10`) | BG PATTERN TABLE field flips |
| Select | toggle EXRAM mode (`$5104` 0/1 + palette swap) | attribute/palette change |
| Start | toggle OBJ size (`ppuctrl ^= $20`, 8x8 <-> 8x16) | sprite row changes |
| Up | toggle fill mode (`mirroring ^= $ff`, `$5105`) | nametable/fill change |
| Left / Right | cycle 4 bank-write orders (`BankOrderData`, 12 regs `$5120+x`) | CURRENT CHR BANKS row changes |
| Down | nothing | — |

What pass means: each press visibly changes its field/row; DATA row reads back
last byte of each 1K CHR bank (`CHRAddrTable $03..$1f`), proving 8x1K CHR
banking through `$5120+` in all 4 write orders. A later session blesses 7
snapshots (one per row below) under `nes_cpu_test/src/png-exps/`.

Tetanes choreography (`m005_exrom/tests.json`, `basics`), directly reusable:

| frame | action | snapshot |
|---|---|---|
| 10 | — | boot screen |
| 11 | press A | — |
| 14 | — | `obj_table` 5835282650047076017 |
| 15 | press B | — |
| 18 | — | `bg_table` 14203633185999536969 |
| 19 | press Start | — |
| 22 | — | `obj_size` 14397147232086588632 |
| 23 | press Select | — |
| 26 | — | `exram` 14992356073095190656 |
| 27 | press Up | — |
| 30 | — | `fill` 8833133164801041322 |
| 31 | press Up (fill back off) | — |
| 33 | press Left | — |
| 36 | — | `bank_left` 13357435064697061420 |
| 37 | press Right | — |
| 39 | press Right | — |
| 42 | — | `bank_right` 10119745483606927626 |

CLI translation (`--press` holds from that frame, matching tetanes semantics
per `nes_cpu_test/src/main.rs` docs):
`--press a@11 --press b@15 --press start@19 --press select@23 --press up@27 --press up@31 --press left@33 --press right@37 --press right@39`.
NOTE: `--press` never releases, so the frame-31 second `up@` press is a no-op
in our harness (button already held since 27) while tetanes re-presses it.
For exact parity the harness needs tap (press+release) support, or the recipe
blesses `fill`-off via a separate run. Simplest decision-ready rule: bless the
6 snapshots that hold-semantics can reach (10/14/18/22/26/36/42 minus the
fill-toggle-off at 30), and cover fill-off with a second run without `up@27`.

## 2. exram = tetanes `exram` (no input, deterministic animation)

Screen: "MMC5 Executable ExRAM Test" text + 6 animated copper bars.
`reset` copies raster code (`rast_start1..rast_end1`) into ExRAM `$5C00`,
sets `$5104=$02`, then NMI `JSR do_rast` executes FROM ExRAM every vblank;
scanloop rewrites `$2000/$2001` mid-scanline for the bars. No joypad read.

What pass means: text screen renders AND bars animate smoothly — this proves
CPU fetch/execute from ExRAM (`$5C00-$5FFF`), `$5104` mode, and cycle-timed
`$2000/$2001` writes. Freeze/tear/wrong colors = fail. Deterministic given
frame number (bars derive from frame-counted `rbars` increments).

Tetanes frames (`m005_exrom/tests.json`, `exram`): 10 / 15 / 45 / 100.
Bless 4 PNGs at those frames from the local ROM; any run matching all 4 passes.

## 3. mmc5test v1 (Drag scanline-counter test, no source, no tetanes entry)

What pass means (minimal, honest): ROM boots to its scanline-counter display
without crashing/hanging, and the display is stable across ~100 frames.
Bless 1 boot PNG (frame 10) via `--dump-frame`. Do NOT claim IRQ-timing
coverage: the ROM's actual counter assertions are unknown without the binary
being further reverse-engineered — that is follow-up work, not acceptance.

## tetanes-harness reuse verdict: choreography YES, hashes NO

- REUSE: `tests.json` frame numbers + button sequence for both ROMs (local
  files are byte-identical to tetanes' — `cmp` clean, same md5s).
- DO NOT reuse the u64 `hash` values: tetanes hashes its own (NTSC-filtered —
  the exram case's first action is `SetVideoFilter: Ntsc`) framebuffer with a
  different algorithm; our verdict device is `PngFrameMatch` (pixel threshold
  50/1020) against blessed PNGs in `nes_cpu_test/src/png-exps/`.
- Recipe per ROM: `cargo run -p nes_cpu_test -- -f <rom> [--press ...]`
  with a new `create_exp_png_machine` branch in `nes_cpu_test/src/image.rs`
  (pattern: `big_chr_ram`/`dpcmletterbox`), PNGs blessed via
  `--dump-frame <n> --dump-out <png>`.
- ROUTING GOTCHA: v1 and v2 share basename `mmc5test.nes` — route on path
  substring (`mmc5test_v2/`, `exram/`), never basename alone.

## Exact commands for the implementing session

```sh
# bless (one PNG per snapshot frame; repeat for frames 10,14,18,22,26,36,42)
cargo run -p nes_cpu_test -- -f ../nes-test-roms/mmc5test_v2/mmc5test.nes \
  --press a@11 --press b@15 --press start@19 --press select@23 \
  --press up@27 --press left@33 --press right@37 --press right@39 \
  --dump-frame 14 --dump-out nes_cpu_test/src/png-exps/mmc5-basics-f14.png
# exram (no input)
for f in 10 15 45 100; do
cargo run -p nes_cpu_test -- -f ../nes-test-roms/exram/mmc5exram.nes \
  --dump-frame $f --dump-out nes_cpu_test/src/png-exps/mmc5-exram-f$f.png
done
# v1 boot screen
cargo run -p nes_cpu_test -- -f ../nes-test-roms/mmc5test/mmc5test.nes \
  --dump-frame 10 --dump-out nes_cpu_test/src/png-exps/mmc5-v1-f10.png
```

Expected results after wiring `image.rs`: exit 0 = all PNGs matched within
threshold at any vblank (see `PngFrameMatch`); `Timeout` fires otherwise.
