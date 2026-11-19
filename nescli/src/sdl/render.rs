//! Unified NES render path (ticket #122).
//!
//! `NesRender` is the single `Render` implementation serving both the
//! interactive `run` loop and movie playback: the recording (`video_tx`) and
//! overlay (`enabled`) slots are simply inactive in interactive mode, so the
//! display path presents exactly what the old `SdlRender` presented. The 1x
//! display buffer, the reused 4x recording scratch frame and the reused
//! `FramePresenter` (ADR 0016: no per-frame texture creation) are retained
//! verbatim from the playback `RecordRender` this type was renamed from.

use super::presentation::{OverlayIcon, OverlayPlacement};
use super::presenter::{FramePresenter, present_frame};
use image::{imageops, load_from_memory};
use nes_core::movie::GamepadInput;
use nes_core::nes::ppu::{NES_HEIGHT, NES_WIDTH};
use sdl2::{render::Canvas, video::Window};
use std::{rc::Rc, sync::mpsc};

pub(crate) struct GamepadOverlay {
    a: bool,
    b: bool,
    up: bool,
    down: bool,
    left: bool,
    right: bool,
    disp_a: bool,
    disp_b: bool,
    disp_up: bool,
    disp_down: bool,
    disp_left: bool,
    disp_right: bool,
    hold_dir: u8,
    hold_ab: u8,
}

impl GamepadOverlay {
    pub(crate) fn new() -> Self {
        Self {
            a: false,
            b: false,
            up: false,
            down: false,
            left: false,
            right: false,
            disp_a: false,
            disp_b: false,
            disp_up: false,
            disp_down: false,
            disp_left: false,
            disp_right: false,
            hold_dir: 0,
            hold_ab: 0,
        }
    }

    pub(crate) fn set_input(&mut self, input: &GamepadInput) {
        self.a = input.a();
        self.b = input.b();
        self.up = input.up();
        self.down = input.down();
        self.left = input.left();
        self.right = input.right();

        let any_dir = self.up || self.down || self.left || self.right;
        if any_dir {
            self.disp_up = self.up;
            self.disp_down = self.down;
            self.disp_left = self.left;
            self.disp_right = self.right;
            self.hold_dir = 18;
        }

        let any_ab = self.a || self.b;
        if any_ab {
            self.disp_a = self.a;
            self.disp_b = self.b;
            self.hold_ab = 18;
        }
    }

    /// Advance the hold counters and project the icons to show this frame, in
    /// logical 1x 256x240 coordinates. Pure: it touches no pixels and needs no
    /// display, so the hold countdown and the icon table are unit-testable.
    pub(crate) fn project(&mut self, layout: OverlayIconLayout) -> Vec<OverlayPlacement> {
        let draw_dir = self.hold_dir > 0;
        let draw_ab = self.hold_ab > 0;
        if draw_dir {
            self.hold_dir -= 1;
        }
        if draw_ab {
            self.hold_ab -= 1;
        }
        let mut icons = Vec::with_capacity(3);
        if !draw_dir && !draw_ab {
            return icons;
        }
        let slot = |x: i32| (x, layout.y, OVERLAY_ICON_SIZE, OVERLAY_ICON_SIZE);
        if let Some(direction_idx) = Self::direction_icon_index(
            draw_dir && self.disp_up,
            draw_dir && self.disp_down,
            draw_dir && self.disp_left,
            draw_dir && self.disp_right,
        ) {
            icons.push(OverlayPlacement {
                icon: direction_idx,
                dst: slot(layout.dir_x),
            });
        }
        if draw_ab && self.disp_a {
            icons.push(OverlayPlacement {
                icon: 0,
                dst: slot(layout.ab_x),
            });
        }
        if draw_ab && self.disp_b {
            icons.push(OverlayPlacement {
                icon: 1,
                dst: slot(layout.b_x),
            });
        }
        icons
    }

    pub(crate) fn direction_icon_index(
        up: bool,
        down: bool,
        left: bool,
        right: bool,
    ) -> Option<usize> {
        match (up, down, left, right) {
            (false, false, false, false) => None,
            (false, false, false, true) => Some(4),
            (false, false, true, false) => Some(3),
            (false, false, true, true) => Some(6),
            (false, true, false, false) => Some(2),
            (false, true, false, true) => Some(9),
            (false, true, true, false) => Some(7),
            (false, true, true, true) => Some(12),
            (true, false, false, false) => Some(5),
            (true, false, false, true) => Some(10),
            (true, false, true, false) => Some(8),
            (true, false, true, true) => Some(14),
            (true, true, false, false) => Some(11),
            (true, true, false, true) => Some(15),
            (true, true, true, false) => Some(13),
            (true, true, true, true) => Some(16),
        }
    }
}

/// Logical 1x 256x240 x-positions of one player's three overlay icon slots.
#[derive(Debug, Clone, Copy)]
pub(crate) struct OverlayIconLayout {
    dir_x: i32,
    ab_x: i32,
    b_x: i32,
    y: i32,
}

/// Dual-player layout: player 1's icons on the left.
pub(crate) const OVERLAY_LAYOUT_LEFT: OverlayIconLayout = OverlayIconLayout {
    dir_x: 2,
    ab_x: 38,
    b_x: 20,
    y: 222,
};

/// Single-player layout, also player 2's side of the dual-player layout.
pub(crate) const OVERLAY_LAYOUT_RIGHT: OverlayIconLayout = OverlayIconLayout {
    dir_x: 205,
    ab_x: 238,
    b_x: 222,
    y: 222,
};

/// Sprite-sheet crop size in pixels: one square icon per artwork cell.
const OVERLAY_SHEET_ICON_SIZE: u32 = 64;

/// Overlay icon size in logical 1x pixels: a sprite-sheet crop divided by the
/// playback Zoom Factor of 4.
const OVERLAY_ICON_SIZE: u32 = OVERLAY_SHEET_ICON_SIZE / 4;

/// Which overlay icons to show this frame and where, in logical 1x
/// coordinates. Advances both players' hold counters exactly once per frame.
pub(crate) fn project_overlay_icons(
    p1: &mut GamepadOverlay,
    p2: &mut GamepadOverlay,
    dual_player: bool,
) -> Vec<OverlayPlacement> {
    let mut icons = Vec::new();
    if dual_player {
        icons.extend(p1.project(OVERLAY_LAYOUT_LEFT));
        icons.extend(p2.project(OVERLAY_LAYOUT_RIGHT));
    } else {
        icons.extend(p1.project(OVERLAY_LAYOUT_RIGHT));
    }
    icons
}

/// Recording frame size in pixels: the playback window, an exact 4x upscale
/// of the logical 1x frame.
pub(crate) const RECORD_WIDTH: usize = 1024;
pub(crate) const RECORD_HEIGHT: usize = 960;

/// Expand a logical 1x playback frame into the 1024x960 RGBA recording
/// scratch frame, replicating each logical pixel into a 4x4 block. This is
/// exactly the replication `ImageRender::set_pixel` performs, so a
/// recording built from the 1x frame is byte-identical to one built from a
/// 4x frame.
pub(crate) fn upscale_into(src: &[u8], dst: &mut [u8]) {
    debug_assert_eq!(src.len(), NES_WIDTH * NES_HEIGHT * 4);
    debug_assert_eq!(dst.len(), RECORD_WIDTH * RECORD_HEIGHT * 4);
    let src_stride = NES_WIDTH * 4;
    let dst_stride = RECORD_WIDTH * 4;
    for y in 0..NES_HEIGHT {
        for x in 0..NES_WIDTH {
            let pixel = &src[y * src_stride + x * 4..][..4];
            for dy in 0..4 {
                let row = (y * 4 + dy) * dst_stride;
                for dx in 0..4 {
                    dst[row + (x * 4 + dx) * 4..][..4].copy_from_slice(pixel);
                }
            }
        }
    }
}

/// Composite overlay icons into a recording frame (RGBA bytes). The recording
/// artefact keeps today's shape: the 4x 1024x960 frame with the overlay baked
/// in, straight-alpha blended by the host.
pub(crate) fn composite_overlay_icons(
    bytes: &mut [u8],
    placements: &[OverlayPlacement],
    icons: &[OverlayIcon],
) {
    let stride = RECORD_WIDTH as i64;
    let height = RECORD_HEIGHT as i64;
    let scale = stride / NES_WIDTH as i64;
    for placement in placements {
        let Some(icon) = icons.get(placement.icon) else {
            continue;
        };
        blit_icon(
            bytes,
            stride,
            height,
            icon,
            placement.dst.0 as i64 * scale,
            placement.dst.1 as i64 * scale,
        );
    }
}

/// Blit one RGBA icon over an RGBA frame with straight-alpha blending.
fn blit_icon(
    bytes: &mut [u8],
    stride: i64,
    height: i64,
    icon: &OverlayIcon,
    dst_x: i64,
    dst_y: i64,
) {
    let iw = icon.width as i64;
    let ih = icon.height as i64;
    for iy in 0..ih {
        for ix in 0..iw {
            let dx = dst_x + ix;
            let dy = dst_y + iy;
            if dx < 0 || dy < 0 || dx >= stride || dy >= height {
                continue;
            }
            let src = &icon.bytes[((iy * iw + ix) * 4) as usize..][..4];
            if src[3] == 0 {
                continue;
            }
            let dst = &mut bytes[(dy * stride + dx) as usize * 4..][..4];
            if src[3] == 255 {
                dst.copy_from_slice(src);
            } else {
                // alpha blend src over dst (straight alpha, non-premul)
                let a = src[3] as u32;
                let inv = 255 - a;
                for c in 0..3 {
                    dst[c] = ((src[c] as u32 * a + dst[c] as u32 * inv) / 255) as u8;
                }
                dst[3] = 255;
            }
        }
    }
}

pub(crate) struct NesRender {
    /// 1024x960 recording frame: raw RGBA pixels in the format fed to ffmpeg
    /// (`RECORD_WIDTH * RECORD_HEIGHT * 4` bytes). The allocation is reused
    /// across frames; each frame actually sent is cloned into a fresh buffer
    /// for the encoder thread.
    record_scratch: Vec<u8>,
    canvas: Option<Canvas<Window>>,
    presenter: FramePresenter,
    video_tx: Option<mpsc::Sender<Vec<u8>>>,
    icons: Rc<[OverlayIcon]>,
    dual_player: bool,
    enabled: bool,
    p1: GamepadOverlay,
    p2: GamepadOverlay,
    ff_skip: bool,
    send_policy: crate::recording_send::RecordingSendPolicy,
}

impl std::fmt::Debug for NesRender {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NesRender").finish_non_exhaustive()
    }
}

impl NesRender {
    /// Build the unified render path: a 1x display buffer, a reused 4x
    /// recording scratch frame, and a reused `FramePresenter` sharing one
    /// overlay icon table. Recording/overlay slots stay inactive until
    /// configured: `video_tx = None` skips the upscale/send and
    /// `enabled = false` skips overlay projection/compositing, so an
    /// interactive consumer presents exactly what `SdlRender` presented.
    pub(crate) fn new(
        canvas: Option<Canvas<Window>>,
        video_tx: Option<mpsc::Sender<Vec<u8>>>,
        trim_frames: u32,
    ) -> Self {
        // One shared icon table: the presenter draws its texture sources
        // while the recording composite reads the same pixels.
        let icons: Rc<[OverlayIcon]> = Rc::from(load_overlay_icons());
        let mut presenter = FramePresenter::new();
        presenter.set_overlay_icons(icons.clone());
        Self {
            record_scratch: vec![0u8; RECORD_WIDTH * RECORD_HEIGHT * 4],
            canvas,
            presenter,
            video_tx,
            icons,
            dual_player: false,
            enabled: false,
            p1: GamepadOverlay::new(),
            p2: GamepadOverlay::new(),
            ff_skip: false,
            send_policy: crate::recording_send::RecordingSendPolicy::new(trim_frames),
        }
    }

    pub(crate) fn set_fast_forward(&mut self, enabled: bool) {
        self.ff_skip = enabled;
    }

    pub(crate) fn set_dual_player(&mut self, dual: bool) {
        self.dual_player = dual;
    }

    pub(crate) fn set_overlay_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    pub(crate) fn set_input(&mut self, port0: &GamepadInput, port1: &GamepadInput) {
        self.p1.set_input(port0);
        self.p2.set_input(port1);
    }
}

impl NesRender {
    pub(crate) fn present_frame_bytes(&mut self, frame_bytes: &[u8]) {
        // Decide whether this frame reaches the recording before building any
        // recording frame, so a trimmed or fast-forwarded frame is never
        // upscaled. The window still presents every frame.
        let send_video = self.send_policy.should_send(self.ff_skip);

        // Advance the hold counters and derive the icon placements once per
        // frame. The window draws them as textures; recording composites them
        // into its own 4x scratch frame so the display frame stays clean.
        let overlays = if self.enabled {
            project_overlay_icons(&mut self.p1, &mut self.p2, self.dual_player)
        } else {
            Vec::new()
        };

        if let Some(ref mut canvas) = self.canvas {
            present_frame(canvas, &mut self.presenter, frame_bytes, &overlays);
        }

        if let Some(ref tx) = self.video_tx
            && send_video
        {
            upscale_into(frame_bytes, &mut self.record_scratch);
            if self.enabled {
                composite_overlay_icons(&mut self.record_scratch, &overlays, &self.icons);
            }
            let _ = tx.send(self.record_scratch.clone());
        }
    }
}

/// Decode the gamepad sprite sheet into the fixed overlay icon table (17
/// square crops: 0 = A, 1 = B, 2..=16 = the held-direction combinations).
pub(crate) fn load_overlay_icons() -> Vec<OverlayIcon> {
    let sprite_sheet = load_from_memory(include_bytes!("../../design/gamepad_sprite.png"))
        .expect("failed to decode gamepad sprite")
        .into_rgba8();
    assert_eq!(sprite_sheet.width(), 17 * OVERLAY_SHEET_ICON_SIZE);
    assert_eq!(sprite_sheet.height(), OVERLAY_SHEET_ICON_SIZE);
    (0..17)
        .map(|i| {
            let icon = imageops::crop_imm(
                &sprite_sheet,
                i * OVERLAY_SHEET_ICON_SIZE,
                0,
                OVERLAY_SHEET_ICON_SIZE,
                OVERLAY_SHEET_ICON_SIZE,
            )
            .to_image();
            OverlayIcon {
                bytes: icon.into_raw(),
                width: OVERLAY_SHEET_ICON_SIZE,
                height: OVERLAY_SHEET_ICON_SIZE,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sdl::presentation::blank_frame;

    /// Interactive shape: no canvas, no recording sender. `finish` must
    /// present nothing and send nothing without panicking.
    #[test]
    fn inactive_render_finish_is_a_noop() {
        let frame = blank_frame();
        let mut render = NesRender::new(None, None, 0);
        render.present_frame_bytes(&frame);
    }

    /// With a recording sender, one finished frame arrives as the plain 4x
    /// upscale of the display buffer (overlay disabled by default).
    #[test]
    fn active_sender_receives_the_plain_upscale() {
        let (tx, rx) = mpsc::channel();
        let mut render = NesRender::new(None, Some(tx), 0);
        let mut frame = blank_frame();
        frame[0..4].copy_from_slice(&[10, 20, 30, 255]);
        render.present_frame_bytes(&frame);
        let sent = rx.try_recv().expect("active sender must receive one frame");
        assert_eq!(sent.len(), RECORD_WIDTH * RECORD_HEIGHT * 4);
        let mut expected = vec![0u8; RECORD_WIDTH * RECORD_HEIGHT * 4];
        upscale_into(&frame, &mut expected);
        assert_eq!(sent, expected);
    }

    /// The trim policy gates the recording sender: the first `trim_frames`
    /// finishes send nothing.
    #[test]
    fn trim_frames_skip_the_first_frames() {
        let (tx, rx) = mpsc::channel();
        let mut render = NesRender::new(None, Some(tx), 2);
        let frame = blank_frame();
        render.present_frame_bytes(&frame);
        render.present_frame_bytes(&frame);
        assert!(rx.try_recv().is_err(), "trimmed frames must not send");
        render.present_frame_bytes(&frame);
        assert!(rx.try_recv().is_ok(), "post-trim frame must send");
    }
    #[test]
    fn bk2_inputs_drive_the_shared_overlay_seam() {
        // Overlay parity (#103): BK2-parsed inputs project player-1 icons
        // left and player-2 icons right through the unchanged overlay path.
        // Frame 2 of the inputs fixture is P1 Up+A with P2 Left+Start.
        let data = include_bytes!("../../../testdata/bk2/bk2-inputs-5f.bk2");
        let movie = nes_core::bk2::parse(data).expect("inputs BK2 must parse");
        let mut p1 = GamepadOverlay::new();
        let mut p2 = GamepadOverlay::new();
        p1.set_input(&movie.input_logs[2].port0);
        p2.set_input(&movie.input_logs[2].port1);
        let icons = project_overlay_icons(&mut p1, &mut p2, true);
        // P1 Up projects direction icon 5 on the left; P1 A projects icon 0
        // beside it; P2 Left projects direction icon 3 on the right.
        assert!(
            icons.iter().any(|p| p.icon == 5 && p.dst.0 == 2),
            "{icons:?}"
        );
        assert!(
            icons.iter().any(|p| p.icon == 0 && p.dst.0 == 38),
            "{icons:?}"
        );
        assert!(
            icons.iter().any(|p| p.icon == 3 && p.dst.0 == 205),
            "{icons:?}"
        );
    }

    // --- gamepad overlay placement projection ------------------------------

    const PAD_A: u8 = 1 << 0;
    const PAD_B: u8 = 1 << 1;
    const PAD_UP: u8 = 1 << 4;
    const PAD_DOWN: u8 = 1 << 5;
    const PAD_LEFT: u8 = 1 << 6;
    const PAD_RIGHT: u8 = 1 << 7;

    fn pad(bits: u8) -> GamepadInput {
        GamepadInput::from_bytes([bits])
    }

    /// The sprite sheet's fixed icon ids: 0 = A, 1 = B, 2..=16 = the fifteen
    /// held-direction combinations. This is artwork, not a restatement of the
    /// implementation.
    const DIRECTION_ICON_TABLE: [(bool, bool, bool, bool, Option<usize>); 16] = [
        (false, false, false, false, None),
        (false, false, false, true, Some(4)),
        (false, false, true, false, Some(3)),
        (false, false, true, true, Some(6)),
        (false, true, false, false, Some(2)),
        (false, true, false, true, Some(9)),
        (false, true, true, false, Some(7)),
        (false, true, true, true, Some(12)),
        (true, false, false, false, Some(5)),
        (true, false, false, true, Some(10)),
        (true, false, true, false, Some(8)),
        (true, false, true, true, Some(14)),
        (true, true, false, false, Some(11)),
        (true, true, false, true, Some(15)),
        (true, true, true, false, Some(13)),
        (true, true, true, true, Some(16)),
    ];

    #[test]
    fn direction_icon_table_covers_all_sixteen_combinations_plus_none() {
        for (up, down, left, right, expected) in DIRECTION_ICON_TABLE {
            assert_eq!(
                GamepadOverlay::direction_icon_index(up, down, left, right),
                expected,
                "up={up} down={down} left={left} right={right}"
            );
        }
    }

    #[test]
    fn no_held_button_projects_nothing() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(0));
        assert!(p.project(OVERLAY_LAYOUT_RIGHT).is_empty());
    }

    #[test]
    fn every_direction_combination_projects_its_table_icon() {
        for (up, down, left, right, expected) in DIRECTION_ICON_TABLE {
            let Some(expected) = expected else {
                continue;
            };
            let mut bits = 0u8;
            if up {
                bits |= PAD_UP;
            }
            if down {
                bits |= PAD_DOWN;
            }
            if left {
                bits |= PAD_LEFT;
            }
            if right {
                bits |= PAD_RIGHT;
            }
            let mut p = GamepadOverlay::new();
            p.set_input(&pad(bits));
            assert_eq!(
                p.project(OVERLAY_LAYOUT_RIGHT),
                vec![OverlayPlacement {
                    icon: expected,
                    dst: (205, 222, 16, 16),
                }],
                "up={up} down={down} left={left} right={right}"
            );
        }
    }

    #[test]
    fn direction_icon_holds_for_eighteen_frames_then_disappears() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_UP));
        for frame in 0..18 {
            let icons = p.project(OVERLAY_LAYOUT_RIGHT);
            assert_eq!(icons.len(), 1, "icon still shown on frame {frame}");
            assert_eq!(icons[0].icon, 5);
        }
        assert!(
            p.project(OVERLAY_LAYOUT_RIGHT).is_empty(),
            "icon disappears on the 19th frame"
        );
    }

    #[test]
    fn a_and_b_icons_project_onto_their_slots() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_A | PAD_B));
        assert_eq!(
            p.project(OVERLAY_LAYOUT_RIGHT),
            vec![
                OverlayPlacement {
                    icon: 0,
                    dst: (238, 222, 16, 16),
                },
                OverlayPlacement {
                    icon: 1,
                    dst: (222, 222, 16, 16),
                },
            ]
        );
    }

    #[test]
    fn a_alone_projects_the_a_icon_only() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_A));
        assert_eq!(
            p.project(OVERLAY_LAYOUT_RIGHT),
            vec![OverlayPlacement {
                icon: 0,
                dst: (238, 222, 16, 16),
            }]
        );
    }

    #[test]
    fn b_alone_projects_the_b_icon_only() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_B));
        assert_eq!(
            p.project(OVERLAY_LAYOUT_RIGHT),
            vec![OverlayPlacement {
                icon: 1,
                dst: (222, 222, 16, 16),
            }]
        );
    }

    #[test]
    fn ab_icons_hold_for_eighteen_frames_then_disappear() {
        let mut p = GamepadOverlay::new();
        p.set_input(&pad(PAD_A));
        for frame in 0..18 {
            assert_eq!(
                p.project(OVERLAY_LAYOUT_RIGHT).len(),
                1,
                "icon still shown on frame {frame}"
            );
        }
        assert!(p.project(OVERLAY_LAYOUT_RIGHT).is_empty());
    }

    #[test]
    fn single_player_projects_into_the_right_hand_layout() {
        let mut p1 = GamepadOverlay::new();
        p1.set_input(&pad(PAD_A));
        let icons = project_overlay_icons(&mut p1, &mut GamepadOverlay::new(), false);
        assert_eq!(
            icons,
            vec![OverlayPlacement {
                icon: 0,
                dst: (238, 222, 16, 16),
            }]
        );
    }

    #[test]
    fn dual_player_projects_player_one_left_and_player_two_right() {
        let mut p1 = GamepadOverlay::new();
        p1.set_input(&pad(PAD_A));
        let mut p2 = GamepadOverlay::new();
        p2.set_input(&pad(PAD_B));
        let icons = project_overlay_icons(&mut p1, &mut p2, true);
        assert_eq!(
            icons,
            vec![
                OverlayPlacement {
                    icon: 0,
                    dst: (38, 222, 16, 16),
                },
                OverlayPlacement {
                    icon: 1,
                    dst: (222, 222, 16, 16),
                },
            ]
        );
    }

    #[test]
    fn dual_player_with_no_input_projects_nothing() {
        let mut p1 = GamepadOverlay::new();
        let mut p2 = GamepadOverlay::new();
        assert!(project_overlay_icons(&mut p1, &mut p2, true).is_empty());
    }
}
