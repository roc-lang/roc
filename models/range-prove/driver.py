"""Drive the compiler for run.sh.

  python3 driver.py check ROC OUTDIR FILE.roc...
      Build each program at --opt=speed with ROC_RANGE_PROVE_DUMP set and
      re-prove every rewrite (OUTDIR/<name>/dump.txt).

  python3 driver.py fuzz ROC OUTDIR PLATFORM COUNT [--seed S] [--domain D]
      Generate COUNT programs (gen.py), build each at --opt=dev and
      --opt=speed, compare their output on the generator's inputs, and
      re-prove every rewrite of the speed build. PLATFORM is the absolute path
      of the platform's main.roc. Each program lands in
      OUTDIR/<domain>-<seed>/ with dump.txt and log.txt.

Both exit 1 on any countermodel, output difference, or program that does not
check or build. fuzz also exits 1 if the whole run dumped no rewrites, which
means ROC does not have the exporter.
"""
import argparse
import os
import subprocess
import sys
import time

import gen
import rpv


def sh(cmd, env=None, timeout=900, inp=None):
    e = dict(os.environ)
    if env:
        e.update(env)
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, env=e, timeout=timeout, input=inp,
                           stdin=None if inp is not None else subprocess.DEVNULL)
        return p.returncode, p.stdout, p.stderr
    except subprocess.TimeoutExpired:
        return 'timeout', '', ''


def build_with_dump(roc, src, exe, dump):
    """Build `src` at --opt=speed with ROC_RANGE_PROVE_DUMP set and keep the
    blocks of stderr in `dump`. Returns (ok, stderr).

    Each block is printed by one call, so it is contiguous, but it can start
    in the middle of a line: compiler warnings are written to stderr
    concurrently, so the block is cut out by its markers, not by lines."""
    rc, _, err = sh([roc, 'build', '--opt=speed', '--no-cache', f'--output={exe}', src],
                    env={'ROC_RANGE_PROVE_DUMP': '1'})
    with open(dump, 'w') as f:
        f.writelines(m + '\n' for m in rpv.DUMP_BLOCK.findall(err))
    return rc in (0, 2) and os.path.exists(exe), err


def one(roc, platform, outdir, seed, domain, deadline):
    d = os.path.join(outdir, f'{domain}-{seed}')
    os.makedirs(d, exist_ok=True)
    with open(os.path.join(d, 'log.txt'), 'w') as log:
        return one_in(roc, platform, d, seed, domain, deadline, log)


def one_in(roc, platform, d, seed, domain, deadline, log):
    for f in ('dev', 'speed'):
        if os.path.exists(os.path.join(d, f)):
            os.remove(os.path.join(d, f))
    src = os.path.join(d, 'p.roc')
    with open(src, 'w') as f:
        f.write(gen.program(seed, domain, os.path.relpath(platform, d)))
    rc, out, err = sh([roc, 'check', src])
    if rc not in (0, 2):  # 2: warnings only (generated code leaves variables unused)
        log.write('check-failed\n' + (out + err)[-3000:])
        return 'check-failed', {}, []
    rc1, _, e1 = sh([roc, 'build', '--opt=dev', '--no-cache', f'--output={d}/dev', src])
    ok2, e2 = build_with_dump(roc, src, f'{d}/speed', os.path.join(d, 'dump.txt'))
    if not (rc1 in (0, 2) and os.path.exists(f'{d}/dev') and ok2):
        log.write(f'build-failed\n--- dev\n{e1[-2000:]}\n--- speed\n{e2[-2000:]}\n')
        return 'build-failed', {}, []
    status = 'same'
    for inp in gen.inputs(seed, domain):
        a = sh([f'{d}/dev'], inp=inp + '\n', timeout=600)
        b = sh([f'{d}/speed'], inp=inp + '\n', timeout=600)
        if (a[0], a[1]) != (b[0], b[1]):
            status = 'diff'
            log.write(f'DIFF input={inp!r}\n dev rc={a[0]} out={a[1]!r} err={a[2][-300:]!r}\n'
                      f' speed rc={b[0]} out={b[1]!r} err={b[2][-300:]!r}\n')
    counts, lines = rpv.validate(os.path.join(d, 'dump.txt'), deadline=deadline)
    log.write(' '.join(f'{k}={n}' for k, n in sorted(counts.items())) + '\n')
    log.writelines(l + '\n' for l in lines)
    return status, counts, [l for l in lines if ': cex' in l]


def check(a):
    bad = 0
    for src in a.files:
        name = os.path.splitext(os.path.basename(src))[0]
        d = os.path.join(a.outdir, name)
        os.makedirs(d, exist_ok=True)
        t = time.time()
        dump = os.path.join(d, 'dump.txt')
        ok, err = build_with_dump(a.roc, src, os.path.join(d, name), dump)
        if not ok:
            bad += 1
            print(f'{src}: build failed\n{err[-2000:]}', flush=True)
            continue
        counts, lines = rpv.validate(dump, deadline=a.deadline)
        if counts.get('cex'):
            bad += 1
        for l in lines:
            print(f'  {l[:400]}', flush=True)
        summary = ' '.join(f'{k}={n}' for k, n in sorted(counts.items())) or 'no rewrites dumped'
        print(f'{src}: {summary} {time.time() - t:.0f}s', flush=True)
    sys.exit(1 if bad else 0)


def fuzz(a):
    bad = 0
    total = {}
    for seed in range(a.seed, a.seed + a.count):
        t = time.time()
        status, counts, cex = one(a.roc, a.platform, a.outdir, seed, a.domain, a.deadline)
        for k, n in counts.items():
            total[k] = total.get(k, 0) + n
        if status != 'same' or cex:
            bad += 1
        summary = ' '.join(f'{k}={n}' for k, n in sorted(counts.items()))
        print(f'{a.domain}-{seed} {status} {summary} {time.time() - t:.0f}s', flush=True)
        for l in cex:
            print(f'  {l[:400]}', flush=True)
    print(f'{a.count} programs, {bad} with a problem; rewrites: ' +
          (' '.join(f'{k}={n}' for k, n in sorted(total.items())) or 'none'))
    if not total:
        print('no rewrites were dumped: is ROC built with ROC_RANGE_PROVE_DUMP?')
        bad += 1
    sys.exit(1 if bad else 0)


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest='cmd', required=True)
    c = sub.add_parser('check')
    c.add_argument('roc')
    c.add_argument('outdir')
    c.add_argument('files', nargs='+')
    f = sub.add_parser('fuzz')
    f.add_argument('roc')
    f.add_argument('outdir')
    f.add_argument('platform')
    f.add_argument('count', type=int)
    f.add_argument('--seed', type=int, default=1)
    f.add_argument('--domain', choices=gen.DOMAINS, default='general')
    for p in (c, f):
        p.add_argument('--deadline', type=float, default=30.0, help='seconds per round and pass (default 30)')
    a = ap.parse_args()
    check(a) if a.cmd == 'check' else fuzz(a)


if __name__ == '__main__':
    main()
