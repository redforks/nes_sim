#!/usr/bin/env python3
# Usage: convert_gd.py <gd_dir> <rgb_out_dir> : GD truecolor (11-byte header, A,R,G,B per px) -> 256x240 RGB f%05d.rgb
import glob
import os
import sys

src = sys.argv[1]
dst = sys.argv[2]
try:
    os.makedirs(dst, exist_ok=True)
except OSError as e:
    print(f'fatal: cannot create {dst}: {e}')
    sys.exit(1)
try:
    paths = sorted(glob.glob(os.path.join(src, 'f*.gd')))
except OSError as e:
    print(f'fatal: cannot list {src}: {e}')
    sys.exit(1)
for p in paths:
    try:
        with open(p, 'rb') as f:
            d = f.read()
    except OSError as e:
        print(f'fatal: cannot read {p}: {e}')
        sys.exit(1)
    assert d[0:2] == b'\xff\xfe' and int.from_bytes(d[2:4], 'big') == 256 \
        and int.from_bytes(d[4:6], 'big') == 240, p
    px = d[11:]
    rgb = bytearray(256 * 240 * 3)
    rgb[0::3] = px[1::4]
    rgb[1::3] = px[2::4]
    rgb[2::3] = px[3::4]
    name = os.path.basename(p)[:-3] + '.rgb'
    try:
        with open(os.path.join(dst, name), 'wb') as f:
            f.write(rgb)
    except OSError as e:
        print(f'fatal: cannot write {name}: {e}')
        sys.exit(1)
print('converted', len(paths))
