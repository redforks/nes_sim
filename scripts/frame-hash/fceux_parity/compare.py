#!/usr/bin/env python3
# Usage: compare.py <nes_rgb_dir> <fceux_rgb_dir> [report_dir]
# Per-frame compare of f%05d.rgb (256x240 RGB). Prints summary, first mismatches with pixel counts,
# and writes diff PNGs for first few mismatches if report_dir given.
import glob
import os
import sys

from PIL import Image

a_dir = sys.argv[1]
b_dir = sys.argv[2]
rep = sys.argv[3] if len(sys.argv) > 3 else None
W, H = 256, 240
try:
    names = sorted(os.path.basename(p) for p in glob.glob(os.path.join(a_dir, 'f*.rgb')))
except OSError as e:
    print(f'fatal: cannot list {a_dir}: {e}')
    sys.exit(1)
same = 0
diffs = []
for n in names:
    pa = os.path.join(a_dir, n)
    pb = os.path.join(b_dir, n)
    if not os.path.exists(pb):
        print('missing fceux', n)
        break
    try:
        with open(pa, 'rb') as f:
            a = f.read()
        with open(pb, 'rb') as f:
            b = f.read()
    except OSError as e:
        print(f'skipping {n}: {e}')
        continue
    if a == b:
        same += 1
        continue
    # count differing pixels and bbox
    cnt = 0
    minx = W
    maxx = -1
    miny = H
    maxy = -1
    for y in range(H):
        ra = a[y * W * 3:(y + 1) * W * 3]
        rb = b[y * W * 3:(y + 1) * W * 3]
        if ra == rb:
            continue
        for x in range(W):
            if ra[x * 3:x * 3 + 3] != rb[x * 3:x * 3 + 3]:
                cnt += 1
                minx = min(minx, x)
                maxx = max(maxx, x)
                miny = min(miny, y)
                maxy = max(maxy, y)
    diffs.append((n, cnt, (minx, miny, maxx, maxy)))
    if rep and len(diffs) <= 5:
        try:
            os.makedirs(rep, exist_ok=True)
        except OSError as e:
            print(f'fatal: cannot create {rep}: {e}')
            sys.exit(1)
        base = n[:-4]
        Image.frombytes('RGB', (W, H), a).save(os.path.join(rep, base + '_nes.png'))
        Image.frombytes('RGB', (W, H), b).save(os.path.join(rep, base + '_fceux.png'))
        d = bytearray(W * H * 3)
        for i in range(W * H):
            if a[i * 3:i * 3 + 3] != b[i * 3:i * 3 + 3]:
                d[i * 3:i * 3 + 3] = b'\xff\x00\x00'
            else:
                d[i * 3:i * 3 + 3] = bytes(v // 4 for v in a[i * 3:i * 3 + 3])
        Image.frombytes('RGB', (W, H), bytes(d)).save(os.path.join(rep, base + '_diff.png'))
print('frames compared:', len(names), 'identical:', same, 'differing:', len(diffs))
for n, cnt, bb in diffs[:15]:
    print(n, f'pixels_differ={cnt}', f'bbox(x0,y0,x1,y1)={bb}')
if len(diffs) > 15:
    print(f'... {len(diffs) - 15} more')
