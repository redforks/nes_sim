# Double-buffered secondary OAM for sprite pixel lookup

`find_sprite_pixel()` previously scanned all 64 primary OAM entries on every pixel, re-evaluating Y-range on each call for an O(64×256) per-scanline cost. Real NES hardware evaluates sprite visibility in advance (dots 65–256) into an internal 8-entry secondary OAM, then rasterizes from that small buffer.

We decided to model the real hardware's secondary OAM with a double-buffered design: `current_scanline_oam` (read by rendering) and `next_scanline_oam` (populated by sprite overflow evaluation). The swap happens at dot 0 of every scanline; `find_sprite_pixel` iterates only the ≤8 pre-filtered sprites.

**Rejected alternatives:**
- Single-buffer (populate inline during rendering): requires full OAM scan mid-render, defeating the optimization.
- `[Sprite; 8]` fixed array: Vec with capacity 8 avoids bounds brittleness when ≤8 entries are pushed.

**Consequences:**
- Sprite evaluation now runs on scanline 261 (pre-render) so scanline 0 has sprites.
- `evaluate_sprite_from_secondary` skips the Y-range check (already verified during evaluation).
