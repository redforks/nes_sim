# NES SDL Adapter

Thin SDL2 adapter that turns a rendered frame plus overlay placements into a
window picture. `nescli` owns the presentation policy; this crate owns the SDL
calls.

## Module Map

- `src/lib.rs` - crate surface: `SdlRender<N>` (the `Render` backend used by
  `run`), `SdlAudioDriver`, `map_keycode_to_button`, the audio constants, and
  the `sdl2` re-export. It re-exports `FramePresenter` and `present_frame`.
- `src/presentation.rs` - SDL-free planning. `plan()` builds a
  `FramePresentation` from an `ImageRender<N>`, `OverlayPlacement`s and the
  destination size. `FramePresentation`, `OverlayPlacement` and `OverlayIcon`
  are the data types. No window is needed, so this is unit-tested directly.
- `src/presenter.rs` - `FramePresenter`, the only SDL draw code, plus the
  `present_frame` helper.

## Presentation seam

The seam is plan then draw:

1. A caller builds an `ImageRender<N>` and a list of `OverlayPlacement`s in
   logical 1x 256x240 coordinates, then calls
   `present_frame(canvas, presenter, frame, overlays)`.
2. `present_frame` queries the renderer output size, calls `plan()` to get a
   `FramePresentation`, and hands it to `FramePresenter::present`.
3. `present` uploads the frame into the reused texture, scale-copies it to the
   whole destination rect, draws each overlay icon texture in order, then
   presents.

`plan` is pure and testable without SDL; keep it that way. Put new pixel or
policy decisions in `plan` (or in `nescli`), not in `present`.

## Texture ownership rule

`nes_sdl` enables sdl2's `unsafe_textures` feature, so a `Texture` does not
borrow its `TextureCreator`. `FramePresenter` therefore owns:

- one frame texture, created on the first `present` and reused until the frame
  size changes; and
- one texture per overlay icon, created lazily on first draw and reused for the
  life of the canvas.

Do not create textures per frame: the reuse is the point of ADR-0016.
`set_overlay_icons` takes an `Rc<[OverlayIcon]>` so the caller and the presenter
share one icon table instead of cloning it.

`ScaleMode::Nearest` is set on the frame texture for the exact integer playback
upscale. Reused textures are valid only while their owning `Canvas` lives;
`FramePresenter` is a field beside the canvas, never a texture owner across
canvases.

## Testing

- `cargo test -p nes_sdl` runs the pure `plan` tests (no window).
- SDL-backed behaviour (overlay blend parity, the default scale mode) is tested
  in `nescli`'s bin tests (`nescli/src/actions/play_movie.rs`), which create a
  hidden window with a software renderer and read pixels back. Those tests
  self-skip when SDL cannot initialise and serialise on a mutex because sdl2
  allows `init` on only one thread per process.
