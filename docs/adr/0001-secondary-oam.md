# Double-buffered secondary OAM for sprite pixel lookup

`find_sprite_pixel()` previously scanned all 64 primary OAM entries on every pixel, re-evaluating Y-range on each call for an O(64×256) per-scanline cost. Real NES hardware evaluates sprite visibility in advance (dots 65–256) into an internal 8-entry secondary OAM, then rasterizes from that small buffer.

We decided to model the real hardware's secondary OAM with a double-buffered design: `current_scanline_oam` (read by rendering) and `next_scanline_oam` (populated by sprite overflow evaluation). The swap happens at dot 0 of visible scanlines 0-239 and the pre-render line 261; `find_sprite_pixel` iterates only the ≤8 pre-filtered sprites.

**Rejected alternatives:**
- Single-buffer (populate inline during rendering): requires full OAM scan mid-render, defeating the optimization.
- `[Sprite; 8]` fixed array: Vec with capacity 8 avoids bounds brittleness when ≤8 entries are pushed.

**Consequences:**
- Sprite evaluation runs only on visible scanlines 0-239. The pre-render line (261) evaluates nothing — the nesdev wiki's PPU sprite evaluation page states: "Sprite evaluation does not happen on the pre-render scanline. Because evaluation applies to the next line's sprite rendering, no sprites will be rendered on the first scanline" — so scanline 0 renders with an empty secondary OAM (matching Mesen2's `_spriteCount = 0`).
- The dot-0 swap runs on visible lines 0-239 and the pre-render line 261: the 261 swap flushes the stale line-240 evaluation (from line 239) on an invisible line and moves the empty next-buffer into `current_scanline_oam`, so scanline 0 renders nothing. Swaps on lines 240-260 are skipped as unobservable (no rendering there, and `$2004` readback falls through to primary OAM outside visible lines).
- `evaluate_sprite_from_secondary` skips the Y-range check (already verified during evaluation).
