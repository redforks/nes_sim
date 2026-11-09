//! Batch palette-index -> RGBA conversion for the vblank drain (ticket
//! #65, design basis: `docs/research/simd-batch-conversion.md`).
//!
//! Tier 0 (always): a per-segment 64-entry final-colour LUT. Grayscale
//! (index-domain), colour lookup, and emphasis (RGB-domain) are folded
//! into one table per drain segment by calling the same
//! `grayscale_index` / `color` / `apply_effects` functions as the
//! per-pixel path, so the batched output agrees with the scalar
//! per-pixel path by construction.
//!
//! Tier 1 (x86_64 only): an SSSE3 `_mm_shuffle_epi8` 16-lane byte-table
//! gather over the LUT's channel planes, behind
//! `is_x86_feature_detected!("ssse3")` runtime dispatch into a
//! `#[target_feature(enable = "ssse3")]` function, with the Tier-0
//! scalar gather as fallback. The LUT is 4 x 16-byte tables per
//! channel; a lane's index selects the quarter from its high bits and
//! shuffles within it.
//!
//! Deliberately not done:
//! - Portable SIMD (`core::simd`): nightly-only on this stable
//!   toolchain (research note section 2).
//! - AVX2 gather (Tier 2): optional, needs measured justification;
//!   deferred to perf validation (#66).
//! - wasm32 SIMD128: enablement is compile-time only with no runtime
//!   detection, and the default `nes_web` build does not enable
//!   `+simd128`; a SIMD128 module would trap on engines without SIMD
//!   support. The scalar Tier-0 path is the web path.
//! - Any change to the `Render` trait: the drain still emits one
//!   `set_pixel` per logical pixel, so Zoom Factor replication and
//!   the Presentation Plan stay downstream and unchanged.
//!
//! Performance comparison against the scalar drain is deferred to
//! the perf-validation ticket (#66); this ticket claims no speed-up
//! numbers, only byte-identical output.

use super::palette::ColorTheme;
use super::registers::PpuMask;

/// Segments shorter than this bypass the LUT build and resolve per
/// pixel (research note section 2 guard for pathological `$2001`
/// write rates). Starting guess, pending #66 measurement.
pub(crate) const SMALL_SEGMENT_PIXELS: usize = 64;

/// Per-segment final-colour table: `[u32; 64]` LE words (RGBA byte
/// order, as [`super::palette::Pixel::to_u32`]) for the scalar gather,
/// plus per-channel planes (`[channel][index]`) for the SSSE3 shuffle
/// gather. Alpha is always `0xFF` (see `Pixel::new`).
#[derive(Debug, Clone, Copy)]
pub(crate) struct FinalColourLut {
    pub words: [u32; 64],
    // Only consumed by the x86_64 SSSE3 gather; on other targets
    // (incl. wasm32) the scalar gather uses `words` alone.
    #[allow(dead_code)]
    planes: [[u8; 64]; 3],
}

/// Build the final-colour LUT for one segment's mask from the same
/// three stages as the per-pixel path:
/// `apply_effects(color(grayscale_index(idx)))` for `idx in 0..64`.
pub(crate) fn build_final_colour_lut(theme: &ColorTheme, mask: PpuMask) -> FinalColourLut {
    let mut words = [0u32; 64];
    let mut planes = [[0u8; 64]; 3];
    for (idx, word) in words.iter_mut().enumerate() {
        let pixel = mask.apply_effects(theme.color(mask.grayscale_index(idx as u8)));
        *word = pixel.to_u32();
        for channel in 0..3 {
            planes[channel][idx] = pixel.0[channel];
        }
    }
    FinalColourLut { words, planes }
}

/// Scalar Tier-0 gather: `out[i] = lut[indices[i] & 0x3F]`.
/// The `& 0x3F` mask matches `ColorTheme::color` (which masks with
/// `& 0x3F`), so indices above 63 (e.g. a raw palette-RAM byte from
/// the disabled backdrop path) resolve exactly as the per-pixel path.
pub(crate) fn convert_scalar(indices: &[u8], lut: &FinalColourLut, out: &mut [u32]) {
    assert_eq!(indices.len(), out.len(), "indices/out length mismatch");
    for (dst, &index) in out.iter_mut().zip(indices) {
        *dst = lut.words[(index & 0x3F) as usize];
    }
}

/// Dispatching convert: SSSE3 shuffle gather on x86_64 when the CPU
/// supports it (detected once per call, no per-pixel branching),
/// scalar gather otherwise (including all wasm32 builds).
pub(crate) fn convert(indices: &[u8], lut: &FinalColourLut, out: &mut [u32]) {
    assert_eq!(indices.len(), out.len(), "indices/out length mismatch");
    #[cfg(target_arch = "x86_64")]
    {
        if std::is_x86_feature_detected!("ssse3") {
            // SAFETY: guarded by the runtime SSSE3 check above.
            unsafe { convert_ssse3(indices, lut, out) };
            return;
        }
    }
    convert_scalar(indices, lut, out);
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "ssse3")]
pub(crate) unsafe fn convert_ssse3(indices: &[u8], lut: &FinalColourLut, out: &mut [u32]) {
    unsafe {
        use std::arch::x86_64::*;
        let mask6 = _mm_set1_epi8(0x3F);
        let mask_low = _mm_set1_epi8(0x0F);
        let mask_high = _mm_set1_epi8(0x03);
        let zero = _mm_setzero_si128();
        let mut chunks = indices.chunks_exact(16);
        let mut out_chunks = out.chunks_exact_mut(16);
        for (chunk, dst) in (&mut chunks).zip(&mut out_chunks) {
            // SAFETY: chunk is exactly 16 bytes / dst exactly 16 words;
            // the load/store pointers below are in-bounds for both.
            let idx = _mm_and_si128(_mm_loadu_si128(chunk.as_ptr() as *const __m128i), mask6);
            let low = _mm_and_si128(idx, mask_low);
            // Per-byte (idx >> 4) & 3: srli_epi16 shifts 16-bit lanes, but
            // idx bytes are <= 0x3F so the cross-byte spill lands in bits
            // >= 4 of the low byte, which the & 0x03 mask clears.
            let high = _mm_and_si128(_mm_srli_epi16(idx, 4), mask_high);
            let eq0 = _mm_cmpeq_epi8(high, zero);
            let eq1 = _mm_cmpeq_epi8(high, _mm_set1_epi8(1));
            let eq2 = _mm_cmpeq_epi8(high, _mm_set1_epi8(2));
            let mut rgb = [[0u8; 16]; 3];
            for channel in 0..3 {
                let plane = &lut.planes[channel];
                // 4 x 16-byte tables; quarter selected by high bits.
                let t0 = _mm_shuffle_epi8(_mm_loadu_si128(plane.as_ptr() as *const __m128i), low);
                let t1 = _mm_shuffle_epi8(
                    _mm_loadu_si128(plane.as_ptr().add(16) as *const __m128i),
                    low,
                );
                let t2 = _mm_shuffle_epi8(
                    _mm_loadu_si128(plane.as_ptr().add(32) as *const __m128i),
                    low,
                );
                let t3 = _mm_shuffle_epi8(
                    _mm_loadu_si128(plane.as_ptr().add(48) as *const __m128i),
                    low,
                );
                let v = _mm_blendv_epi8(t3, t2, eq2);
                let v = _mm_blendv_epi8(v, t1, eq1);
                let v = _mm_blendv_epi8(v, t0, eq0);
                _mm_storeu_si128(rgb[channel].as_mut_ptr() as *mut __m128i, v);
            }
            for i in 0..16 {
                dst[i] = u32::from_le_bytes([rgb[0][i], rgb[1][i], rgb[2][i], 0xFF]);
            }
        }
        let rem = chunks.remainder();
        let rem_out = out_chunks.into_remainder();
        convert_scalar(rem, lut, rem_out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nes::ppu::palette::Pixel;

    fn theme() -> ColorTheme {
        // Distinct colour per entry so any index mix-up is visible.
        let mut buf = [0u8; 192];
        for i in 0..64 {
            buf[i * 3] = (i * 4) as u8;
            buf[i * 3 + 1] = (255 - i * 3) as u8;
            buf[i * 3 + 2] = (i * 7) as u8;
        }
        ColorTheme::load_from_pal_buf(&buf)
    }

    fn masks() -> [PpuMask; 4] {
        [
            PpuMask::new(),
            PpuMask::new().with_grayscale(true),
            PpuMask::new().with_red_tint(true),
            PpuMask::new()
                .with_grayscale(true)
                .with_green_tint(true)
                .with_blue_tint(true),
        ]
    }

    #[test]
    fn lut_matches_per_pixel_resolution_for_all_masks() {
        let theme = theme();
        for mask in masks() {
            let lut = build_final_colour_lut(&theme, mask);
            for idx in 0..64u8 {
                let want = mask
                    .apply_effects(theme.color(mask.grayscale_index(idx)))
                    .to_u32();
                assert_eq!(lut.words[idx as usize], want, "mask {mask:?} idx {idx}");
            }
        }
    }

    #[test]
    fn scalar_convert_masks_index_to_six_bits() {
        let theme = theme();
        let lut = build_final_colour_lut(&theme, PpuMask::new());
        // Indices above 63 (incl. the 0xFF empty-slot sentinel) wrap
        // exactly as ColorTheme::color's & 0x3F does on the per-pixel path.
        let indices: Vec<u8> = (0..=255u8).collect();
        let mut out = vec![0u32; indices.len()];
        convert_scalar(&indices, &lut, &mut out);
        for (i, &index) in indices.iter().enumerate() {
            assert_eq!(
                out[i],
                lut.words[(index & 0x3F) as usize],
                "index {index:#04x}"
            );
            assert_eq!(out[i], theme.color(index).to_u32());
        }
    }

    #[test]
    fn convert_matches_scalar_for_all_indices_and_masks() {
        let theme = theme();
        let indices: Vec<u8> = (0..=255u8).collect();
        for mask in masks() {
            let lut = build_final_colour_lut(&theme, mask);
            let mut scalar = vec![0u32; indices.len()];
            let mut auto = vec![0u32; indices.len()];
            convert_scalar(&indices, &lut, &mut scalar);
            convert(&indices, &lut, &mut auto);
            assert_eq!(auto, scalar, "mask {mask:?}");
        }
    }

    #[test]
    fn convert_matches_scalar_for_odd_lengths() {
        let theme = theme();
        let lut = build_final_colour_lut(&theme, masks()[3]);
        // Deterministic pseudo-random index pattern.
        let indices: Vec<u8> = (0..1000u32).map(|i| ((i * 37 + 11) % 256) as u8).collect();
        for len in [0usize, 1, 15, 16, 17, 31, 32, 33, 63, 100, 255, 256, 1000] {
            let mut scalar = vec![0u32; len];
            let mut auto = vec![0u32; len];
            convert_scalar(&indices[..len], &lut, &mut scalar);
            convert(&indices[..len], &lut, &mut auto);
            assert_eq!(auto, scalar, "len {len}");
        }
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn ssse3_convert_matches_scalar() {
        if !std::is_x86_feature_detected!("ssse3") {
            return;
        }
        let theme = theme();
        let indices: Vec<u8> = (0..=255u8).collect();
        for mask in masks() {
            let lut = build_final_colour_lut(&theme, mask);
            let mut scalar = vec![0u32; indices.len()];
            let mut simd = vec![0u32; indices.len()];
            convert_scalar(&indices, &lut, &mut scalar);
            // SAFETY: guarded by the runtime check above.
            unsafe { convert_ssse3(&indices, &lut, &mut simd) };
            assert_eq!(simd, scalar, "mask {mask:?}");
            // Alpha is always opaque.
            for word in &simd {
                assert_eq!(Pixel(word.to_le_bytes()).0[3], 0xFF);
            }
        }
    }
}
