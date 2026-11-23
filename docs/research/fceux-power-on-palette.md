# fceux 2.6.6 vs nes_core: power-on palette RAM and PPU init (#202)

Scope: read-only research for redforks/nes_sim issue #202 (wayfinder map #200).
Question: which palette RAM / PPU register init values does fceux (commit 1e1168d)
use at power-on, which does `nes_core` use, and which difference explains the
observed backdrop colours (Super Mario Bros. movie):

- fceux: black at frame 0, grey (117,117,117) at frames 1-10.
- nescli: backdrop (0,69,0) at frames 0-8.

Backdrop colour = palette entry 0 = `$3F00` (`PALRAM[0x00]`) when rendering is on.

Path conventions: fceux paths are under `/home/forks/nes/fceux/`. nes_core paths are
under `nes_core/src/` in this worktree. `tools/fceux_parity/fceux-default.pal` exists
only as untracked in the main checkout `/home/forks/nes/nes_sim/` (it is not in this
worktree).

## 1. fceux 2.6.6 power-on (commit 1e1168d)

Power sequence: `PowerNES()` calls `FCEUPPU_Power()` (`src/fceu.cpp:1048`).

### Palette RAM and other PPU memory

`FCEUPPU_Power()` (`src/ppu.cpp:1734-1748`):

```c
FCEU_MemoryRand(NTARAM, 0x800, true);   // ppu.cpp:1738
FCEU_MemoryRand(PALRAM, 0x20, true);    // ppu.cpp:1739
FCEU_MemoryRand(SPRAM, 0x100, true);    // ppu.cpp:1740
for (x = 0; x < 0x20; ++x) PALRAM[x] &= 0x3F;   // ppu.cpp:1742
UPALRAM[0] = PALRAM[0x04];              // ppu.cpp:1743
UPALRAM[1] = PALRAM[0x08];              // ppu.cpp:1744
UPALRAM[2] = PALRAM[0x0C];              // ppu.cpp:1745
PALRAM[0x0C] = PALRAM[0x08] = PALRAM[0x04] = PALRAM[0x00];   // ppu.cpp:1746
PALRAM[0x1C] = PALRAM[0x18] = PALRAM[0x14] = PALRAM[0x10];   // ppu.cpp:1747
FCEUPPU_Reset();                        // ppu.cpp:1748
```

`FCEU_MemoryRand(ptr, size, default_zero)` (`src/fceu.cpp:979-1000`):

- `RAMInitOption` defaults to 0 (`src/fceu.cpp:936`). The Qt config default is also 0
  (`src/drivers/Qt/config.cpp:552`).
- With `default_zero == true`, option 0 gives `v = 0x00` (`src/fceu.cpp:988-989`).
  Option 2 also gives `0x00`. Option 1 gives `0xFF`. Option 3 is random.
- So with the default setting, `PALRAM[0x00..0x1F]` = `0x00` before the `&= 0x3F`
  mask. The mask keeps it `0x00`. Every palette byte is zero at power-on, and the
  mirror and `UPALRAM` copies are zero too.

Result: **fceux power-on `$3F00..$3F1F` = all `$00`**. The backdrop index is `$00`.

`FCEUPPU_Reset()` (`src/ppu.cpp:1721-1732`) sets:

- `VRAMBuffer = PPU[0] = PPU[1] = PPU_status = PPU[3] = 0`
- `PPUSPL = 0`, `PPUGenLatch = 0`, `RefreshAddr = TempAddr = 0`, `vtoggle = 0`
- `ppudead = 2` (`src/ppu.cpp:1727`), `kook = 0`, `idleSynch = 1`
- `new_ppu_reset = true`, so `ppur.reset()` and `spr_read.reset()` run on the first
  frame (`src/ppu.cpp:2106-2113`).

### Rendering of the backdrop

- The backdrop is read through `READPAL` (`src/ppu.cpp:61`), which reads `PALRAM` with
  the grayscale mask.
- The default NTSC-style palette is `palette[512]` in `src/palettes/palettes.h:63`.
  Entries are built with `P64(x) = ((x)<<2)|((x>>4)&3)` (`src/palettes/palettes.h:60`).
- `palette[0]` = `P64(0x1D)` x3 = **(117,117,117)** (`src/palettes/palettes.h:64`).
- `palette[9]` = `P64(0x00), P64(0x11), P64(0x00)` = **(0,69,0)**.
- `default_palette[0] = palette` is selected unless NTSC or user/game palettes apply
  (`src/palette.cpp:62-66`, `:511-530`).

### Frame 0 and the first frames (ppudead)

- Static `ppudead = 1` (`src/ppu.cpp:325`). `FCEUPPU_Reset()` resets it to 2.
- The newppu path `FCEUX_PPU_Loop` (`src/ppu.cpp:2116-2127`) does not draw while
  `ppudead` is set. It only advances the PPU and decrements `ppudead`.
- The legacy path `FCEUPPU_Loop` (`src/ppu.cpp:1778-1780`) fills `XBuf` with `0x80`
  while `ppudead` is set. That is not the default path.
- Consequence: the first two emulated frames do not write `XBuf` on the newppu path,
  so frame 0 shows the stale framebuffer. This is a hypothesis for the black frame,
  not verified (see unknowns).

## 2. nes_core power-on (this worktree, HEAD 6e6237f)

### Palette RAM

`Palette::default()` (`nes_core/src/nes/ppu/palette.rs:53-70`):

```rust
data: [
    0x09, 0x01, 0x00, 0x01, 0x00, 0x02, 0x02, 0x0D, //   palette.rs:63
    0x08, 0x10, 0x08, 0x24, 0x00, 0x00, 0x04, 0x2C, //
    0x09, 0x01, 0x34, 0x03, 0x00, 0x04, 0x00, 0x14, //
    0x08, 0x3A, 0x00, 0x02, 0x00, 0x20, 0x2C, 0x08, //   palette.rs:66
],
```

- The comment (`palette.rs:54-58`) says these are blargg's `power_up_palette` table values.
- `$3F00` = `0x09` (`palette.rs:63`). The four mirrors `$3F10/14/18/1C` alias
  `$3F00/04/08/0C` (`addr_to_index`, `palette.rs:72-79`).
- The test `power_up_palette_matches_blargg_table` (`palette.rs:127-141`) pins the
  same 32 bytes.
- Constructed by `Ppu::new()` at `nes_core/src/nes/ppu.rs:489` and by `reset_hard()`
  at `nes_core/src/nes/ppu.rs:540`.

### Colour theme

- `ColorTheme::default()` loads `include_bytes!("./default.pal")` (`palette.rs:31`).
  This file is `nes_core/src/nes/ppu/default.pal` (192 bytes).
- Set at `nes_core/src/nes/ppu.rs:492`. `reset_hard()` keeps the theme.
- `default.pal` index 9 = **(11,72,0)**, not (0,69,0). Index 0 = (102,102,102).
- `tools/fceux_parity/fceux-default.pal` (main checkout, untracked) has index 0 =
  (117,117,117) and index 9 = **(0,69,0)**, matching fceux `palette[]`.

### Other PPU state

- `Registers::new()` (`nes_core/src/nes/ppu/registers.rs:174-188`). `PpuCtrl`,
  `PpuMask`, and `PpuStatus` derive `Default`, so they start as zero (`registers.rs:7`,
  `:38`, `:132`). `vram_addr`, `temp_vram_addr`, `fine_x`, `write_toggle` start at 0.
- `Oam::default()` zeroes all 64 sprites (`nes_core/src/nes/ppu/oam.rs:104-110`).
- Framebuffer power-up is `[0,0,0,0]` per pixel (`nes_core/src/nes/ppu.rs:493`).
  `nes_core/AGENTS.md:51` documents the power-up framebuffer as opaque black
  `[0,0,0,255]` after hard reset.

## 3. Explanation of the observed colours

1. **Grey (117,117,117) at frames 1-10 (fceux).** fceux zeros `PALRAM` at power-on
   (`src/ppu.cpp:1739` + `src/fceu.cpp:988-989`). The backdrop is index `$00`, and
   `palette[0x00]` = (117,117,117) (`src/palettes/palettes.h:64`). The grey is the
   palette-RAM-zero colour, not an emulator background.

2. **Backdrop (0,69,0) for frames 0-8 (nescli).** nes_core sets `$3F00 = $09`
   (`palette.rs:63`). Under `tools/fceux_parity/fceux-default.pal` index 9 this is
   (0,69,0). Under the built-in `default.pal` index 9 it is (11,72,0).

3. **The difference that explains the grey/green split is the palette RAM power-up
   value.** fceux: all `$00`. nes_core: blargg's measured table, `$3F00 = $09`.
   Both are sourced from "power-on" values, but the fceux one is an emulator
   zero-fill, not a hardware measurement.

4. **Black at frame 0** is not explained by palette RAM. fceux skips drawing for
   `ppudead` frames (`src/ppu.cpp:2116-2127`), so frame 0 may show the stale
   framebuffer. nes_core frame 0 already shows the backdrop colour, so it is not
   black at frame 0 here. This is the open part of the issue.

## 4. Unknowns

- Not verified by running either emulator. No build or run was done.
- Whether nescli's observed (0,69,0) came from `--palette tools/fceux_parity/fceux-default.pal`.
  The built-in `default.pal` gives (11,72,0). The run command was not in the issue text.
- Frame indexing. fceux `ppudead` = 2 suggests frames 0-1 are not drawn on the newppu
  path. The issue's "frame 0" may be 0-based or 1-based, so the frame boundary is not
  pinned.
- Source of the black colour at fceux frame 0: the stale `XBuf` contents, or the
  driver's presentation before the first drawn frame. Not traced.
- Whether real NES hardware matches blargg's `$09...` table. The power-up palette is
  not documented in a local nesdev wiki copy (`/home/forks/nes/nesdev-wiki` is absent),
  and `https://www.nesdev.org/wiki/PPU_power_up_state` could not be fetched.
  `palette.rs:54-58` relies on blargg's test ROM, not on wiki text.
- `RAMInitOption` at runtime: the default is 0 in the source. The user's fceux
  configuration (`SDL.RamInitMethod`, `config.cpp:552`) was not checked. Options 1
  and 3 would change the palette init.
