# Const-generic framebuffer with zoom factor, removing image crate from nes_core

`nes_core`'s `ImageRender` previously wrapped `image::RgbaImage` (heap `Vec<u8>` sized at runtime via `new(w,h)`) and every consumer typed `Ppu<ImageRender>` concretely. Recording at 4× required `RecordRender` to replicate each PPU `set_pixel` 16 times manually; SDL upload used `RgbaImage::as_bytes/dimensions`, and `MachineView::borrow_image` exposed `&RgbaImage`. This ADR adopts `ImageRender<const N: usize = 1>` owning `Box<[[u8;4]; 256*240*N*N]>` (row-major `256*N × 240*N`), where `N` is the Zoom Factor replicating each logical `set_pixel(x,y)` to an `N×N` block; drops `image` from `nes_core` (kept at workspace for `nes_cpu_test`/`nescli` PNG shims); removes `Clone` from `ImageRender` (multi-MiB copy); and makes `SdlRender<const N>` generic. Chosen over keeping `RgbaImage` (runtime size, extra dep, manual replication) and over flat `Box<[u8; W*H*4]>` (typed pixels avoid slice math and keep `pixel_brightness` branchless).

## Considered options

- **Keep `RgbaImage` + runtime `new(w,h)`**: Familiar, no const-generic propagation to `Ppu`/`NesMcu`/`MachineView`, but requires `image` in `nes_core`, manual 4× loop in `RecordRender`, and runtime dimension bookkeeping.
- **Flat `Box<[u8; 256*240*4*N*N]>`**: Direct SDL upload, but every `pixel_brightness`/`get_pixel` needs `*4` arithmetic; typed `[[u8;4]]` gives `buf[idx] = color` and `bytemuck::cast_slice` for zero-copy bytes with no extra cost.
- **Typed `Box<[[u8;4]; 256*240*N*N]>` with `N` replication (this ADR)**: Compile-time canvas size, `set_pixel` replicates `N×N`, accessors `width()/height()/as_bytes()/as_pixels()/pixel_brightness()` without `image`, `SdlRender<N>` covers both `1×` and `4×` cases, `N=0` rejected via `const { assert!(N>0) }`.

## Decision

`ImageRender<const N: usize = 1>` as above; `new()`/`default()` only (no `new(w,h)`); silent-ignore OOB with caller-guaranteed `x<256,y<240`; runtime `assert!(N>0)` guard (const assert not allowed in generic const); no `Clone`; `nes_core/Cargo.toml` removes `image`, `bytemuck` stays for `cast_slice`.

## Consequences

- `ImageRender` == `ImageRender<1>` stays spellable via default generic; `RecordRender.buffer: ImageRender<4>` and `SdlRender<N>` replace manual replication.
- `MachineView::borrow_image() -> &RgbaImage` removed in favor of `renderer().as_bytes()/as_pixels()/width()/height()`; `FramePngDump`/`PngFrameMatch` build one-off `RgbaImage::from_raw` from bytes.
- `Ppu` tests compare `as_pixels()[y*W + x]` not `get_pixel`.
- `cargo tree -p nes_core` no longer pulls `image`/`png` crates.
