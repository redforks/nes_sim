//! Terminal video sinks: sixel and kitty graphics (ticket: --video-out).
//!
//! Zero new dependencies: sixel is raw palette + RLE escapes, kitty is a
//! PNG (via the existing `image` crate) plus a hand-rolled base64 writer.
//! Pixels come from stdout only; all status lines stay on stderr (`eprintln!`).
//! The 1x 256x240 RGBA frame passed in already has the gamepad overlay baked
//! by `NesRender` (downsampled from the reused 4x recording scratch).

use std::io::Write;

pub const FRAME_W: usize = 256;
pub const FRAME_H: usize = 240;

/// Terminal video selector. `Sdl` lives in the SDL path; this enum covers the
/// terminal half. The clap `ValueEnum` lives on the re-export in `sdl.rs`
/// (single source); this type is the codec without CLI glue.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum TerminalVideo {
    #[default]
    Sixel,
    Kitty,
}

/// RAII alternate-screen + hide-cursor guard. Normal exit restores the
/// terminal via `Drop`. SIGINT (Ctrl-C) terminates without unwinding, so a
/// killed frame may need one `reset`; per-frame output deliberately avoids
/// raw mode so the shell stays usable.
pub struct TerminalGuard {
    active: bool,
}

impl TerminalGuard {
    pub fn enter() -> Self {
        let mut out = std::io::stdout();
        let _ = out.write_all(b"\x1b[?1049h\x1b[H\x1b[?25l");
        let _ = out.flush();
        Self { active: true }
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        if self.active {
            let mut out = std::io::stdout();
            let _ = out.write_all(b"\x1b[?25h\x1b[?1049l");
            let _ = out.flush();
        }
    }
}

pub struct TerminalPresenter {
    mode: TerminalVideo,
    // Owns the alternate-screen guard for the life of the run.
    _guard: TerminalGuard,
}

impl TerminalPresenter {
    pub fn new(mode: TerminalVideo) -> Self {
        Self {
            mode,
            _guard: TerminalGuard::enter(),
        }
    }

    /// Encode one baked 1x frame and write it to stdout with cursor-home.
    pub fn present(&mut self, frame_1x: &[u8]) {
        debug_assert_eq!(frame_1x.len(), FRAME_W * FRAME_H * 4);
        let mut out = std::io::stdout();
        let _ = out.write_all(b"\x1b[H");
        match self.mode {
            TerminalVideo::Sixel => {
                let bytes = sixel_encode(frame_1x);
                let _ = out.write_all(&bytes);
            }
            TerminalVideo::Kitty => {
                kitty_present(frame_1x, &mut out);
            }
        }
        let _ = out.flush();
    }
}

// --- base64 (kitty needs it; no new crate) --------------------------------

const B64: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

pub(crate) fn base64_encode(input: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(input.len().div_ceil(3) * 4);
    for chunk in input.chunks(3) {
        let b0 = chunk[0] as u32;
        let b1 = if chunk.len() > 1 { chunk[1] as u32 } else { 0 };
        let b2 = if chunk.len() > 2 { chunk[2] as u32 } else { 0 };
        let n = (b0 << 16) | (b1 << 8) | b2;
        out.push(B64[((n >> 18) & 63) as usize]);
        out.push(B64[((n >> 12) & 63) as usize]);
        out.push(if chunk.len() > 1 {
            B64[((n >> 6) & 63) as usize]
        } else {
            b'='
        });
        out.push(if chunk.len() > 2 {
            B64[(n & 63) as usize]
        } else {
            b'='
        });
    }
    out
}

fn png_bytes_1x(frame_1x: &[u8]) -> Vec<u8> {
    use image::{ImageBuffer, Rgba, codecs::png::PngEncoder};
    use std::io::Cursor;
    let img: ImageBuffer<Rgba<u8>, _> =
        ImageBuffer::from_raw(FRAME_W as u32, FRAME_H as u32, frame_1x.to_vec())
            .expect("1x frame has exactly 256x240 RGBA bytes");
    let mut buf = Vec::new();
    let enc = PngEncoder::new(Cursor::new(&mut buf));
    use image::ImageEncoder as _;
    enc.write_image(
        img.as_raw(),
        FRAME_W as u32,
        FRAME_H as u32,
        image::ExtendedColorType::Rgba8,
    )
    .expect("PNG encode of in-memory frame");
    buf
}

fn kitty_present(frame_1x: &[u8], out: &mut impl Write) {
    let png = png_bytes_1x(frame_1x);
    let b64 = base64_encode(&png);
    // Kitty chunked transfer: 4096-byte payloads, m=1 while more follows.
    let mut first = true;
    let mut i = 0;
    while i < b64.len() {
        let end = (i + 4096).min(b64.len());
        let last = end == b64.len();
        if first {
            let _ = out.write_all(
                format!(
                    "\x1b_Ga=T,f=100,s={},v={},m={};",
                    FRAME_W,
                    FRAME_H,
                    if last { 0 } else { 1 }
                )
                .as_bytes(),
            );
            first = false;
        } else {
            let _ = out.write_all(if last { b"\x1b_Gm=0;" } else { b"\x1b_Gm=1;" });
        }
        let _ = out.write_all(&b64[i..end]);
        let _ = out.write_all(b"\x1b\\");
        i = end;
    }
}

// --- sixel (216-color web palette, banded RLE) -----------------------------

/// Map one RGB pixel to the 6x6x6 web palette index 0..216.
fn rgb_to_216(r: u8, g: u8, b: u8) -> usize {
    let r6 = r as usize * 5 / 255;
    let g6 = g as usize * 5 / 255;
    let b6 = b as usize * 5 / 255;
    r6 * 36 + g6 * 6 + b6
}

fn palette_100(idx: usize) -> (u8, u8, u8) {
    let r6 = idx / 36;
    let g6 = (idx % 36) / 6;
    let b6 = idx % 6;
    (
        (r6 * 100 / 5) as u8,
        (g6 * 100 / 5) as u8,
        (b6 * 100 / 5) as u8,
    )
}

/// Encode a baked 1x RGBA frame as one DECSIXEL sequence. Alpha is ignored
/// (frames are opaque; overlay blends to 255 already).
pub(crate) fn sixel_encode(frame: &[u8]) -> Vec<u8> {
    debug_assert_eq!(frame.len(), FRAME_W * FRAME_H * 4);
    let mut out = Vec::with_capacity(64 * 1024);
    out.extend_from_slice(b"\x1bPq\"1;1");
    // Palette: #0..#215 as RGB 0..100.
    for c in 0..216 {
        let (r, g, b) = palette_100(c);
        out.extend_from_slice(format!("#{c};2;{r};{g};{b}").as_bytes());
    }
    // Pixel index map, one byte per pixel.
    let mut idx = vec![0u8; FRAME_W * FRAME_H];
    for (i, px) in frame.as_chunks::<4>().0.iter().enumerate() {
        idx[i] = rgb_to_216(px[0], px[1], px[2]) as u8;
        // NB: 216 > u8? No: 216 fits in u8 (max 215). Cast safe.
    }
    // Bands of 6 rows.
    let bands = FRAME_H.div_ceil(6);
    for band in 0..bands {
        let y0 = band * 6;
        // Colors used in this band, first-seen order (cap keeps escapes sane).
        let mut seen = [false; 216];
        let mut order: Vec<usize> = Vec::new();
        for y in y0..(y0 + 6).min(FRAME_H) {
            for x in 0..FRAME_W {
                let c = idx[y * FRAME_W + x] as usize;
                if !seen[c] {
                    seen[c] = true;
                    order.push(c);
                    if order.len() == 64 {
                        break;
                    }
                }
            }
            if order.len() == 64 {
                break;
            }
        }
        // Fallback: everything maps into the capped set since rgb_to_216 only
        // yields 0..216 and the cap only truncates exotic gradients; pixels
        // whose color missed the cap reuse color 0 for this band.
        for (ci, &c) in order.iter().enumerate() {
            out.extend_from_slice(format!("#{c}").as_bytes());
            // 256 column masks for this color.
            let mut run_val: u8 = 0;
            let mut run_len: usize = 0;
            let flush = |out: &mut Vec<u8>, v: u8, n: usize| {
                if n == 0 {
                    return;
                }
                let ch = (v + 63) as char;
                if n > 3 {
                    out.extend_from_slice(format!("!{n}{ch}").as_bytes());
                } else {
                    for _ in 0..n {
                        out.push(ch as u8);
                    }
                }
            };
            for x in 0..FRAME_W {
                let mut mask: u8 = 0;
                for dy in 0..6 {
                    let y = y0 + dy;
                    if y >= FRAME_H {
                        break;
                    }
                    let mut pc = idx[y * FRAME_W + x] as usize;
                    if !seen[pc] || !order.contains(&pc) {
                        pc = 0;
                    }
                    if pc == c {
                        mask |= 1 << dy;
                    }
                }
                if run_len == 0 {
                    run_val = mask;
                    run_len = 1;
                } else if mask == run_val && run_len < 1000 {
                    run_len += 1;
                } else {
                    flush(&mut out, run_val, run_len);
                    run_val = mask;
                    run_len = 1;
                }
            }
            flush(&mut out, run_val, run_len);
            if ci + 1 < order.len() {
                out.push(b'$'); // carriage return, same band next color
            }
        }
        if band + 1 < bands {
            out.push(b'-'); // next band
        }
    }
    out.extend_from_slice(b"\x1b\\");
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn solid_frame(r: u8, g: u8, b: u8) -> Vec<u8> {
        let mut f = vec![0u8; FRAME_W * FRAME_H * 4];
        for px in f.chunks_exact_mut(4) {
            px[0] = r;
            px[1] = g;
            px[2] = b;
            px[3] = 255;
        }
        f
    }

    #[test]
    fn base64_matches_rfc_vectors() {
        assert_eq!(base64_encode(b""), b"");
        assert_eq!(base64_encode(b"f"), b"Zg==");
        assert_eq!(base64_encode(b"fo"), b"Zm8=");
        assert_eq!(base64_encode(b"foo"), b"Zm9v");
        assert_eq!(base64_encode(b"foob"), b"Zm9vYg==");
    }

    #[test]
    fn sixel_sequence_framing_and_palette() {
        let f = solid_frame(255, 0, 0);
        let s = sixel_encode(&f);
        assert!(s.starts_with(b"\x1bPq"), "missing sixel introducer");
        assert!(s.ends_with(b"\x1b\\"), "missing sixel terminator");
        assert!(s.windows(10).any(|w| w == b"#0;2;0;0;0"), "palette missing");
        assert!(s.len() > 1024, "solid frame still carries palette");
    }

    #[test]
    fn web_palette_covers_corners() {
        assert_eq!(rgb_to_216(0, 0, 0), 0);
        assert_eq!(rgb_to_216(255, 255, 255), 215);
        assert_eq!(rgb_to_216(255, 0, 0), 5 * 36);
    }

    #[test]
    fn kitty_png_bytes_are_a_png() {
        let f = solid_frame(1, 2, 3);
        let png = png_bytes_1x(&f);
        assert_eq!(&png[0..8], b"\x89PNG\r\n\x1a\n");
    }
}
