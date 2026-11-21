"""Differential run of one program: `roc test` at --opt=dev against --opt=speed and against
--opt=dev --specialize=yes.

usage: python3 -I opt_diff.py ROC FILE.roc

Compares the pass/fail summary and the multiset of `[dbg]` lines. Exits 1 on a difference.
This catches what the per-pass checker cannot see: passes after ARC and the backends.
"""
import re
import subprocess
import sys


def run(roc, path, *flags):
    try:
        p = subprocess.run([roc, "test", "--no-cache", *flags, path], capture_output=True, text=True,
                           timeout=600, stdin=subprocess.DEVNULL)
        out = p.stdout + p.stderr
    except subprocess.TimeoutExpired:
        return ("timeout",), []
    dbg = sorted(l for l in out.splitlines() if l.startswith("[dbg]"))
    m = re.search(r"(\d+) passed\s+(\d+) failed\s+(\d+) compiler errors", out)
    if m:
        summary = tuple(int(x) for x in m.groups())
    else:
        m = re.search(r"All \((\d+)\) tests passed", out)
        summary = (int(m.group(1)), 0, 0) if m else ("crash", out[-400:])
    return summary, dbg


def main():
    roc, path = sys.argv[1], sys.argv[2]
    base = run(roc, path, "--opt=dev")
    rc = 0
    for flags in (("--opt=speed",), ("--opt=dev", "--specialize=yes")):
        other = run(roc, path, *flags)
        if other != base:
            rc = 1
            print("DIFF %s vs --opt=dev: summary %s vs %s" % (" ".join(flags), other[0], base[0]))
            a, b = set(base[1]), set(other[1])
            for l in sorted(a - b)[:10]:
                print("  dev only : " + l)
            for l in sorted(b - a)[:10]:
                print("  %s only: %s" % (" ".join(flags), l))
    return rc


if __name__ == "__main__":
    sys.exit(main())
