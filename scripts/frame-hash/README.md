# frame-hash harness (#156)

- `frame_hash.sh <nescli> <outdir>`: runs SMB (600f), Jackal (1500f), VBL basics (600f, idle movie), nmi_sync (600f, idle movie, `demo_ntsc.nes`), and vbl_wait (VBL basics, 600f, `movies/vbl_reset.fm2`: soft resets at frames 100/200/300/400/500, re-entering the ROM's `init_wait_vbl` $2002 poll at different frame phases) headless; writes `<outdir>/<rom>.frames` (per-frame MD5 lines `idx bytes md5`).
- `bin/ffmpeg`: fake ffmpeg (Python) that hashes raw RGBA frames; `FP_OUT` sets the output prefix. nescli has no frame dump.
- `compare.sh <outdir>`: diffs against `baseline/pre_<rom>.hashes` (pre-#132, `44addbd`).
- Each run is killed once N frame lines are written (nescli never exits after trim). ~13 s for all three ROMs.
- nmi_sync and vbl_wait baselines are pre-#132 (`44addbd`). They MATCH on the freeze build `0bff505` too, so they do not catch the freeze (#163). Use SMB/Jackal for that.
