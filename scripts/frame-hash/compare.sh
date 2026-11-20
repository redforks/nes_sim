#!/bin/bash
# Usage: compare.sh <outdir>   (outdir produced by frame_hash.sh)
# Compares each ROM's per-frame hashes with baseline/pre_<rom>.hashes (pre-#132 44addbd).
HERE=$(cd "$(dirname "$0")" && pwd); OUT=$1
for r in smb600 jackal1500 vbl_basics; do
  grep '^[0-9]' "$OUT/$r.frames" > "$OUT/$r.hashes"
  n=$(wc -l < "$OUT/$r.hashes"); d=$(awk '{print $3}' "$OUT/$r.hashes" | sort -u | wc -l)
  if cmp -s "$OUT/$r.hashes" "$HERE/baseline/pre_$r.hashes"; then v=MATCH; else
    v="DIFF first_frame=$(diff "$HERE/baseline/pre_$r.hashes" "$OUT/$r.hashes" | head -1 | sed 's/[acd].*//;s/,.*//' )"; fi
  echo "$r frames=$n distinct=$d $v"
done
