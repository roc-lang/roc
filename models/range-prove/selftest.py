"""Offline part of `run.sh selftest`.

1. Every rewrite in tests/basic.dump (a real dump of tests/basic.roc) re-proves.
2. A copy with every claim falsified (each cmp/ovf truth flipped, each switch
   value moved, each `proven` turned into `crash`) gets a countermodel for
   every falsified rewrite. This shows the validator can reject each claim
   kind rather than accepting everything.

Usage: python3 selftest.py OUTDIR
"""
import os
import re
import sys

import rpv

HERE = os.path.dirname(os.path.abspath(__file__))
FALSIFY = {
    'cmp': lambda a, b: ('cmp', 1 - a, b),
    'ovf': lambda a, b: ('ovf', 1 - a, b),
    'switch': lambda a, b: ('switch', a + 1, b),
    'proven': lambda a, b: ('crash', a, b),
}


def main():
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)
    fails = 0
    good = os.path.join(HERE, 'tests', 'basic.dump')
    counts, lines = rpv.validate(good)
    n = sum(counts.values())
    if counts.get('ok') != n or n == 0:
        print(f'FAIL basic.dump: expected every rewrite ok, got {counts}')
        for l in lines:
            print(f'  {l[:300]}')
        fails += 1
    else:
        print(f'ok   basic.dump: {n} rewrites re-proved')

    bad = os.path.join(out, 'falsified.dump')
    want = set()
    with open(good) as f, open(bad, 'w') as g:
        for line in f:
            t = line.split()
            if t and t[0] == 'E' and t[1] in FALSIFY:
                kind, a, b = FALSIFY[t[1]](int(t[3]), int(t[4]))
                line = f'E {kind} {t[2]} {a} {b}\n'
                want.add((kind, int(t[2])))
            g.write(line)
    counts, lines = rpv.validate(bad)
    got = set()
    for l in lines:
        m = re.match(r'p\d+ r\d+ (\w+) s(\d+) .*: cex', l)
        if m:
            got.add((m.group(1), int(m.group(2))))
    missed = want - got
    if missed or not want:
        print(f'FAIL falsified.dump: no countermodel for {sorted(missed)}')
        fails += 1
    else:
        print(f'ok   falsified.dump: {len(want)} falsified rewrites all rejected')
    sys.exit(1 if fails else 0)


if __name__ == '__main__':
    main()
