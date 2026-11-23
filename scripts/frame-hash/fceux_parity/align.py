#!/usr/bin/env python3
# Usage: align.py [nes_rgb_dir] [fc_rgb_dir] [nframes]
# Exact-match offset search d in [-40,40] per frame. Prints runs where
# nes[i] == fc[i+d], i.e. the per-frame alignment offset between the two runs.
import os
import sys

from PIL import Image, ImageChops

W, H = 256, 240
try:
    nes_dir = sys.argv[1] if len(sys.argv) > 1 else 'nes_rgb'
    fc_dir = sys.argv[2] if len(sys.argv) > 2 else 'fc_rgb'
    N = int(sys.argv[3]) if len(sys.argv) > 3 else 600
except (IndexError, ValueError) as e:
    print(f'fatal: bad arguments: {e}')
    sys.exit(1)


def load_frame(path):
    try:
        with open(path, 'rb') as f:
            return f.read()
    except OSError as e:
        print(f'fatal: cannot read {path}: {e}')
        sys.exit(1)


def img(p):
    return Image.frombytes('RGB', (W, H), load_frame(p))


nes = [load_frame(os.path.join(nes_dir, f'f{i:05d}.rgb')) for i in range(N)]
fc = [load_frame(os.path.join(fc_dir, f'f{i:05d}.rgb')) for i in range(N)]


def ndiff(a, b):
    if a == b:
        return 0
    a_img = Image.frombytes('RGB', (W, H), a)
    b_img = Image.frombytes('RGB', (W, H), b)
    m = ImageChops.difference(a_img, b_img).convert('L').point(lambda v: 255 if v else 0)
    return sum(1 for v in m.getdata() if v)  # fine: only for mismatched frames


prev = None
out = []
for i in range(N):
    best = None
    for d in range(-40, 41):
        j = i + d
        if not (0 <= j < N):
            continue
        if nes[i] == fc[j]:
            best = (0, d)
            break
    if best is None:
        best = (None, None)
    out.append((i, best))
# print exact-match runs
prev = None
for i, (_c, d) in enumerate(out):
    key = d
    if key != prev:
        print(f'nes {i} exact-match fc offset={d:+d}' if d is not None else f'nes {i} exact-match fc offset=NONE')
        prev = key
