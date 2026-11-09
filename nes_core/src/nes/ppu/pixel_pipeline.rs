//! Retired: the two-dot pixel output pipeline.
//!
//! Ticket #62 (batch PPU palette-index rendering) retires this module as a
//! per-pixel delay stage. Per-dot geometry now writes only the palette
//! index into the PPU's flat `256 * 240` buffer (`y * 256 + x`, allocated
//! once), and the frame drains in linear raster order at the VBL set dot
//! before `finish()`. The drain's linear order preserves the raster order
//! this pipeline used to provide; any dot-level grayscale/emphasis landing
//! offset is modelled as a stored split position (ticket #63), not as
//! pipeline slots.
//!
//! The in-flight-pixel mechanics that used to live here (`PendingPixel`,
//! `take_due`, `push`, `clear`) are gone with it; see
//! `Ppu::drain_palette_index_buffer` in `ppu.rs` for the replacement.
