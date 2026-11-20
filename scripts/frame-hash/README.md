# frame-hash harness (#156)

- `frame_hash.sh <nescli> <outdir>`: runs SMB (600f), Jackal (1500f), VBL basics (600f, idle movie) headless; writes `<outdir>/<rom>.frames` (per-frame MD5 lines `idx bytes md5`).
- `bin/ffmpeg`: fake ffmpeg (Python) that hashes raw RGBA frames; `FP_OUT` sets the output prefix. nescli has no frame dump.
- `compare.sh <outdir>`: diffs against `baseline/pre_<rom>.hashes` (pre-#132, `44addbd`).
- Each run is killed once N frame lines are written (nescli never exits after trim). ~13 s for all three ROMs.
