# Presentation plan and canvas-owned reused frame texture

## Status

accepted

## Context

`SdlRender<N>` and playback's `RecordRender` each created a fresh sdl2 streaming texture every frame, uploaded the whole framebuffer into it, and copied it to the whole render target. Per-frame `SDL_CreateTexture` was windowed self-time 1.2% of playback, and `Texture::update` plus libc memcpy a further 4.3%. The pixel-replication zoom and the overlay compositing were tangled with the draw calls, so no part of what goes on screen could be tested without opening a window. Without sdl2's `unsafe_textures` feature a `Texture` borrows its `TextureCreator`, which borrows the `Canvas`, so a reusable texture cannot live beside the canvas that owns the renderer.

## Decision

- Split presentation into a pure plan and a thin SDL adapter.
  - `nes_sdl::presentation::plan<N>(frame: &ImageRender<N>, overlays: &[OverlayPlacement], dst_size: (u32, u32)) -> FramePresentation<'_>` is SDL-free and unit-testable without a window. It returns the frame bytes and dimensions (Zoom Factor `N` applied), the destination rect (the whole window), and the overlay placements scaled from logical 1x 256x240 coordinates into destination pixels.
  - `FramePresentation { bytes, width, height, dst, overlays }` and `OverlayPlacement { icon, dst }` carry the result.
  - The plan is generic over the Framebuffer Zoom Factor `N`, so the display path can switch from the current 4x frame to a 1x frame without changing the seam.
- The SDL adapter `nes_sdl::presenter::FramePresenter` owns the frame texture. It creates the texture on first present and reuses it for the life of the canvas, recreating it only when the frame dimensions change. Each frame it does one `Texture::update` and one `Canvas::copy` to the plan's destination rect.
- Enable the sdl2 dependency feature `unsafe_textures` in `nes_sdl`. It makes the `Texture` owned by the `Canvas` and free of a lifetime, so the reused texture is expressible with zero `unsafe` code in this repository. No `unsafe` block is added anywhere.
- Apply the seam and the reuse to both display paths: `SdlRender` (`run`) and `RecordRender` (playback window).
- Playback renders the logical 1x frame (`ImageRender<1>`); the destination rect is the whole 1024x960 window, so SDL performs one nearest-neighbour scaled copy and the display is an exact integer 4x upscale of the former 4x framebuffer. The overlay is drawn by SDL on top.

## Considered Options

- **Keep per-frame `create_texture_streaming`**: no feature flag and no texture-ownership question, but pays a driver allocation every frame and keeps presentation untestable.
- **Reuse a texture without `unsafe_textures`**: not expressible; the `Texture` borrows its `TextureCreator`, which borrows the `Canvas`, so the two cannot coexist in one struct.
- **Write the texture lifetime plumbing by hand (`unsafe` `SDL_DestroyTexture` or raw pointers)**: rejected; the sdl2 feature is the supported way and keeps this repository free of `unsafe`.
- **A `FramePresenter` that owns the `Canvas` as well as the texture**: would force every caller's canvas into the adapter; keeping the adapter as a field that borrows the `Canvas` per call fits both `SdlRender` and `RecordRender`.

## Consequences

- Windowed self-time no longer contains `create_texture_streaming`; the only frame texture is created once.
- The plan is unit-tested without a window: frame bytes and dimensions, the destination rect, and overlay scaling.
- ADR-0012's Zoom Factor stays a capability of `ImageRender<N>`; the playback display path no longer uses it, and ADR-0012 records that revision.
- Overlay icon placements flow through `plan` as data; the overlay policy (hold counters, icon table, 18-frame hold) stays in `nescli`.
- The window draws the gamepad overlay as SDL textures instead of writing icon pixels into the Framebuffer. `GamepadOverlay` projects its state into pure `OverlayPlacement` values (logical 1x 256x240 coordinates), and `FramePresenter` owns one texture per icon index, created lazily by `set_overlay_icons` on first use and reused across frames. The `--output` recording path still composites the icons host-side into its 4x scratch frame, so the recorded artefact is byte-identical.
- The recorded `--output` artefact and the `run` 2x window picture are unchanged.

## Playback at 1x with SDL scaling and a 4x recording scratch

Spec #38, ticket #43. The frame texture is the logical 256x240 picture; the only frame-size work on the display path is one `Texture::update` and one nearest `Canvas::copy` to the 1024x960 window rect.

Recording keeps its contract with ffmpeg: raw RGBA at 1024x960. So the recording path owns a separate 1024x960 scratch frame and builds it host-side by upscaling the 1x frame 4x (`upscale_into`, exactly the replication `ImageRender<4>::set_pixel` performs) and compositing the overlay icons with the same straight-alpha blend as before. It reuses one scratch allocation across frames.

Why a separate scratch frame instead of the old shared 4x buffer:

- The display frame must stay 1x — that is the performance win. A single buffer cannot be both the 1x display frame and the 1024x960 recording frame without reintroducing per-pixel replication (or a 4x upload) on the display path.
- The recording frame is built only for frames actually sent to ffmpeg. The trim and fast-forward send decision runs before frame construction, so trimmed and skipped frames are never upscaled, and fast-forward recording does less work than before.
- Because the upscale is byte-for-byte the replication the 4x framebuffer used to hold, and the overlay composite is unchanged, the recorded artefact is byte-identical. Verified with sha256 on the reference movie: plain `--trim-after 600` and `--gamepad-overlay --trim-after 600`, before and after, and the `--auto-fast-forward` recording too.
