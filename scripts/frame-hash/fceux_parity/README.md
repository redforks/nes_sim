# SMB baseline: nescli vs fceux 2.6.6 (ticket #218)

Single command:

```bash
scripts/frame-hash/fceux_parity/baseline.sh [outdir]
```

Defaults: ROM `Super Mario Bros. (World).nes`, movie
`happylee-supermariobros,warped.fm2`, `NFRAMES=600`, palette
`fceux-default.pal`, fceux `/usr/bin/fceux` 2.6.6. Override via
`ROM=`, `MOVIE=`, `PALETTE=`, `NFRAMES=`, `FCEUX=`, `NESCLI_BIN=`
(unset = cargo-built release binary).

The `ROM`/`MOVIE` defaults are machine-specific
(`/home/forks/nes/...`); on any other machine `ROM=` and `MOVIE=`
env overrides are required.

Expected: `identical: 158 / 600`. Outputs in `<outdir>`: `env.txt`
(versions, hashes, commands), `per_frame.tsv` (MATCH|DIFF per frame),
`offsets.txt` (exact-match offset runs + `intermission` 50-196 and
`jump-window` 197-217 segments), `summary.txt` (stable, no timestamps),
`nes_rgb/`, `fc_rgb/`, logs.

Frame-cut convention: nescli ends each frame at scanline 261
(`process_frame` return, dot 0-2); fceux enters `FCEUPPU_Loop` at
scanline 240 (vblank + visible). A store landing "next frame" under
the nescli cut lands in the same fceux call. Sampling at scanline 240
removes the 0/2 `$077F` artifact but not the intermission gap
(load 50 vs 51, end 197 vs 178).

Files: `baseline.sh` (this ticket); `fceux_dump.lua`
(`gui.gdscreenshot(true)` per-frame GD dump); `bin/ffmpeg` (fake
ffmpeg shim: `DUMP_DIR` RGB dump, not the `FP_OUT` hasher in
`scripts/frame-hash/bin/` — the `PATH` override in `baseline.sh`
means `ffmpeg` on this script's `PATH` is the shim, not real ffmpeg); `convert_gd.py` (GD strip 11-byte header to
RGB); `compare.py` (byte compare + first-5 diff PNGs); `align.py`
(offset search, default dirs, `N` from argv); `fceux-default.pal`
(192 bytes, md5 `68479d93aef7cf31c2927f436b10a54a`).

Determinism: run twice, `diff` `summary.txt`, `per_frame.tsv`,
`offsets.txt` — identical.
