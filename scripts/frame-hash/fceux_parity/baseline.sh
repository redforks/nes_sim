#!/bin/bash
# Baseline: nescli vs fceux 2.6.6 SMB movie comparison (ticket #218).
# One command reproduces 158/600 identical frames, a per-frame table,
# and alignment offsets for the intermission segment and frames 197-217.
#
# Usage: baseline.sh [outdir]
#   Out dir defaults to ./out-baseline under this script's directory.
#
# Env overrides (defaults shown):
#   ROM      "/home/forks/nes/Super Mario Bros. (World).nes"
#   MOVIE    "/home/forks/nes/happylee-supermariobros,warped.fm2"
#   PALETTE  "<this-dir>/fceux-default.pal"
#   NFRAMES  600
#   FCEUX    /usr/bin/fceux          (want 2.6.6)
#   NESCLI_BIN (default: cargo-built release binary)
#
# Outputs in <outdir>:
#   env.txt        fceux version, palette/ROM/movie hashes, env vars, commands
#   nescli.log     nescli run log            fceux.log  fceux run log
#   nes_rgb/       nescli frames f%05d.rgb   fc_rgb/    fceux frames
#   per_frame.tsv  "frame<TAB>match" for every frame (MATCH|DIFF)
#   offsets.txt    exact-match offset runs over all frames + segments
#   summary.txt    identical/differing counts (stable: no timestamps)
#
# Frame-cut convention (printed below and in README.md):
#   nescli process_frame returns at the end of scanline 261 (dot 0-2 of the
#   next visible frame). fceux FCEUPPU_Loop is entered at scanline 240 and
#   runs vblank plus the visible lines. So an NMI handler store that lands
#   "in the next frame" under the nescli cut lands in the same fceux call.
#   Sampling nescli state at scanline 240 removes the 0/2 $077F artifact
#   but does NOT move the intermission gap (load 50 vs 51, end 197 vs 178).
set -u

HERE=$(cd "$(dirname "$0")" && pwd)
OUT=${1:-"$HERE/out-baseline"}
ROM=${ROM:-"/home/forks/nes/Super Mario Bros. (World).nes"}
MOVIE=${MOVIE:-"/home/forks/nes/happylee-supermariobros,warped.fm2"}
PALETTE=${PALETTE:-"$HERE/fceux-default.pal"}
NFRAMES=${NFRAMES:-600}
FCEUX=${FCEUX:-/usr/bin/fceux}

[ -f "$ROM" ] || { echo "missing ROM: $ROM (set ROM=)"; exit 1; }
[ -f "$MOVIE" ] || { echo "missing movie: $MOVIE (set MOVIE=)"; exit 1; }
[ -f "$PALETTE" ] || { echo "missing palette: $PALETTE (set PALETTE=)"; exit 1; }
command -v python3 >/dev/null || { echo "need python3"; exit 1; }
python3 -c "import PIL.Image" 2>/dev/null || { echo "need python3-PIL"; exit 1; }

if [ -z "${NESCLI_BIN:-}" ]; then
  ROOT=$(cd "$HERE/../../.." && pwd)
  cargo build -q --release -p nescli --manifest-path "$ROOT/Cargo.toml" || exit 1
  NESCLI_BIN="$ROOT/target/release/nescli"
fi

mkdir -p "$OUT/nes_rgb" "$OUT/fc_gd" "$OUT/fc_rgb"

FCEUX_VERSION=$("$FCEUX" --version 2>&1 | grep -m1 -o "2\.[0-9.]*" || echo unknown)
{
  echo "fceux: $FCEUX ($FCEUX_VERSION)"
  echo "palette: $PALETTE md5=$(md5sum "$PALETTE" | cut -d' ' -f1) bytes=$(stat -c%s "$PALETTE")"
  echo "rom: $ROM md5=$(md5sum "$ROM" | cut -d' ' -f1)"
  echo "movie: $MOVIE inputs=$(grep -c '^|' "$MOVIE")"
  echo "nframes: $NFRAMES"
  echo "env: SDL_VIDEODRIVER=${SDL_VIDEODRIVER:-<unset>} QT_QPA_PLATFORM=${QT_QPA_PLATFORM:-<unset>}"
  echo "nescli: $NESCLI_BIN rev=$(git -C "$HERE" rev-parse --short HEAD 2>/dev/null || echo unknown)"
  echo "frame-cut: nescli ends frame at scanline 261; fceux loop starts at scanline 240"
} > "$OUT/env.txt"

echo "== nescli ($NFRAMES frames) =="
# WARNING: $HERE/bin/ffmpeg is a FAKE ffmpeg shim (RGB dump via DUMP_DIR),
# not real ffmpeg. It shadows the real ffmpeg on PATH for this call only.
DUMP_DIR="$OUT/nes_rgb" PATH="$HERE/bin:$PATH" \
  "$NESCLI_BIN" run "$ROM" --movie "$MOVIE" --no-throttle \
  --trim-after "$NFRAMES" --headless --palette "$PALETTE" \
  --output "$OUT/nes.mp4" >"$OUT/nescli.log" 2>&1
echo "nescli rc=$? rgb=$(ls "$OUT"/nes_rgb/f*.rgb 2>/dev/null | wc -l)"

echo "== fceux $FCEUX_VERSION ($NFRAMES frames) =="
DUMP_DIR="$OUT/fc_gd" NFRAMES="$NFRAMES" \
  SDL_VIDEODRIVER=dummy QT_QPA_PLATFORM=offscreen \
  "$FCEUX" --loadlua "$HERE/fceux_dump.lua" --playmov "$MOVIE" "$ROM" \
  >"$OUT/fceux.log" 2>&1
echo "fceux rc=$? gd=$(ls "$OUT"/fc_gd/f*.gd 2>/dev/null | wc -l)"
python3 "$HERE/convert_gd.py" "$OUT/fc_gd" "$OUT/fc_rgb" >>"$OUT/fceux.log" 2>&1

echo "== compare =="
python3 "$HERE/compare.py" "$OUT/nes_rgb" "$OUT/fc_rgb" "$OUT/report" | tee "$OUT/compare.txt"
python3 - "$OUT/nes_rgb" "$OUT/fc_rgb" "$NFRAMES" > "$OUT/per_frame.tsv" <<'EOF'
import sys
a_dir, b_dir, n = sys.argv[1], sys.argv[2], int(sys.argv[3])
for i in range(n):
    p = 'f%05d.rgb' % i
    try:
        a = open(a_dir + '/' + p, 'rb').read()
        b = open(b_dir + '/' + p, 'rb').read()
    except OSError:
        print('%d\tMISSING' % i)
        continue
    print('%d\t%s' % (i, 'MATCH' if a == b else 'DIFF'))
EOF
python3 - "$OUT/nes_rgb" "$OUT/fc_rgb" "$NFRAMES" > "$OUT/offsets.txt" <<'EOF'
import sys
a_dir, b_dir, n = sys.argv[1], sys.argv[2], int(sys.argv[3])
W, H, SPAN = 256, 240, 40
A = [open('%s/f%%05d.rgb' % a_dir % i, 'rb').read() for i in range(n)]
B = [open('%s/f%%05d.rgb' % b_dir % i, 'rb').read() for i in range(n)]
off = []
for i in range(n):
    d = next((x for x in range(-SPAN, SPAN + 1) if 0 <= i + x < n and A[i] == B[i + x]), None)
    off.append(d)
prev = None
for i, d in enumerate(off):
    if d != prev:
        print('nes %d exact-match fc offset=%s' % (i, ('%+d' % d) if d is not None else 'NONE'))
        prev = d
def seg(lo, hi, tag):
    got = sorted(set(x for x in off[lo:hi + 1] if x is not None))
    print('%s frames %d-%d offsets=%s' % (tag, lo, hi, ','.join('%+d' % x for x in got) or 'NONE'))
seg(50, 196, 'intermission')
seg(197, 217, 'jump-window')
EOF
cat "$OUT/offsets.txt"

SAME=$(grep -c $'\tMATCH' "$OUT/per_frame.tsv")
echo "identical: $SAME / $NFRAMES differing: $((NFRAMES - SAME))"
echo "identical=$SAME total=$NFRAMES" > "$OUT/summary.txt"
echo "wrote $OUT/{env.txt,nescli.log,fceux.log,compare.txt,per_frame.tsv,offsets.txt,summary.txt}"
