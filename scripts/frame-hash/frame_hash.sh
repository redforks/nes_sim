#!/bin/bash
# Usage: frame_hash.sh <nescli-binary> <outdir>
# Runs the three acceptance ROMs headless and writes per-frame MD5 logs to <outdir>/<name>.frames.
# Frames come from the fake ffmpeg in bin/ (hashes each raw RGBA frame; writes "TOTAL <n>" when done).
# nescli hangs after trim exit and never closes the pipe, so each run is killed once <frames> frame lines are written.
set -u
BIN=$1; OUT=$2
HERE=$(cd "$(dirname "$0")" && pwd)
R=/home/forks/nes/nes-test-roms
H=/home/forks/nes
mkdir -p "$OUT"
export PATH="$HERE/bin:$PATH"
run() { # name rom movie frames
  local name=$1 rom=$2 movie=$3 frames=$4
  rm -f "$OUT/$name.frames" "$OUT/$name.audio"
  FP_OUT="$OUT/$name" "$BIN" "$rom" run --movie "$movie" --no-throttle --headless \
      --trim-after "$frames" -o "$OUT/$name.mp4" >"$OUT/$name.nescli.log" 2>&1 &
  local pid=$! waited=0
  while [ "$(grep -c '^[0-9]' "$OUT/$name.frames" 2>/dev/null || echo 0)" -lt "$frames" ]; do
    sleep 0.5; waited=$((waited+1))
    if [ $waited -gt 240 ]; then break; fi   # 120 s cap
  done
  kill -9 $pid 2>/dev/null; wait $pid 2>/dev/null; pkill -9 -f "$HERE/bin/ffmpeg" 2>/dev/null
  echo "$name frames=$(grep -c '^[0-9]' "$OUT/$name.frames" 2>/dev/null) waited=${waited}"
}
run smb600 "$H/Super Mario Bros. (Japan, USA).nes" "$H/happylee-supermariobros,warped.fm2" 600
run jackal1500 "$H/Jackal (USA).nes" "$H/zyr2288,aiqiyou-jackal-2p.fm2" 1500
run vbl_basics "$R/ppu_vbl_nmi/rom_singles/01-vbl_basics.nes" $HERE/movies/idle.fm2 600
