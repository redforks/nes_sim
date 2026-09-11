# Cold-start: Initial PPU scanline at 261 (pre-render)

`Timing::new()` previously initialized `scanline = 0` (start of visible rendering). This skipped the pre-render scanline (261) — the hardware phase that prepares background scroll anchors, vertical scroll reload, and MMC3 A12 notifications before the first visible scanline.

We decided to set initial scanline to **261** (dot 0, pre-render line). This models the PPU's Cold-start phase: a construction-only interval from `Ppu::new()` until the first 261→0 wrap, during which the PPU state machine runs through the pre-render line and transitions naturally into frame 0.

**Rejected alternatives:**
- **Starting at scanline 0 (status quo):** omits vertical scroll reload (dots 280-304), horizontal scroll reload (dot 257), MMC3 A12 toggles, and VBlank flag clearing. Four distinct correctness gaps, each requiring workarounds or special-case code in the first-frame rendering path. (Sprite evaluation is deliberately *not* among them: the nesdev wiki's PPU sprite evaluation page notes that "sprite evaluation does not happen on the pre-render scanline", so the pre-render line has no sprite-evaluation duty to prepare.)
- **Starting at scanline 241 (VBlank):** provides VBlank/NMI initialization but still skips the pre-render line: scanline 0 still starts without background activation (and misses the pre-render scroll/MMC3 duties). Worse: CPU would see an immediate NMI at dot 1, which conflicts with the real-hardware behavior where the CPU inhibits NMI briefly after reset.
- **Starting at scanline 240 (post-render):** transitions into VBlank naturally but still skips pre-render line — same scroll/MMC3 gaps as 0 or 241.
- **Deferred Cold-start (wait for rendering-enabled):** complex state machine tracking "has first real pre-render line run yet." Unnecessary: when rendering is disabled (default after construction), the pre-render line runs silently without side effects, and the next pre-render line after rendering is enabled does the useful work. The positional guarantee — being at scanline 261 — is sufficient.

**Consequences:**
- `Timing::new()` sets `scanline: 261`. No new fields or methods — Cold-start is a domain concept, not a code artifact.
- `frame_no` now returns 1 during the first visible frame (0 during Cold-start, increments at the 261→0 wrap). Previously returned 0 during the first visible frame. This is a 1-off shift consistent with "frame_no == number of completed frame wraps."
- `Ppu::reset()` is unaffected — keeps its existing behavior (clears `odd_frame`, leaves scanline unchanged). Cold-start is construction-only.
- When rendering is disabled at construction, the first pre-render line runs as a no-op (VBlank flag already false; scroll reloads gated on `rendering_enabled`; sprite evaluation never runs on the pre-render line). The actual rendering setup happens on the next pre-render line after software enables rendering via `$2001`.
