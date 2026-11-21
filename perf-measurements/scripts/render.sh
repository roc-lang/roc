#!/bin/bash
# Render every chart SVG to PNG and stack each variant's three panels into one image.
cd ~/.cache/perf
export TMPDIR=$HOME/.cache/perf/t
for suffix in "" -linear-time -linear; do
  for c in check build test; do
    google-chrome --headless=new --disable-gpu --no-sandbox --hide-scrollbars --user-data-dir=$HOME/.cache/perf/t/ud --force-device-scale-factor=2 --window-size=560,420 --screenshot=$PWD/chart-$c$suffix.png file://$PWD/chart-$c$suffix.svg >/dev/null 2>&1
  done
  python3 -I -c "
import sys
from PIL import Image
s = sys.argv[1]
ims = [Image.open(f'chart-{c}{s}.png') for c in ('check', 'build', 'test')]
out = Image.new('RGB', (ims[0].width, sum(i.height for i in ims)), '#fcfcfb')
y = 0
for i in ims:
    out.paste(i, (0, y)); y += i.height
out.save(f'roc-build-timings{s}.png')
" "$suffix"
done
