"""Cross-check the Python prototype against the Maude model.

For random programs in the language both share (no calls or branches), this
checks that:

1. both compute the same forward mode facts;
2. Maude's exhaustive interleaving search finds no bad state under the
   forward analysis;
3. with no atomics at all, Maude finds a bad state exactly when the Python
   race oracle reports a racing event, which validates the oracle against
   real interleavings.

Usage:
    python3 bridge.py [count] [seed]
"""

from __future__ import annotations

import os
import random
import re
import subprocess
import sys
import tempfile

from fuzz import Gen
from rcproto import Forward, Machine, Program, racy_events, Analysis

HERE = os.path.dirname(os.path.abspath(__file__))
MAUDE_DIR = os.path.join(HERE, "..", "maude")


class NoAtomics(Analysis):
    name = "none"

    def __init__(self, prog):
        pass

    def atomic(self, ctx, local, path):
        return False


class SmallGen(Gen):
    """Only the constructs the Maude model has, in small programs."""

    def body(self, live, budget, fork_depth, allow_proj=True):
        rng = self.rng
        saved = rng.choices

        def choices(population, weights=None, k=1):
            allowed = {"mkleaf", "mknode", "cp", "proj", "drop", "setf", "host", "fork"}
            pw = [(p, w) for p, w in zip(population, weights) if p in allowed]
            if fork_depth >= 1:
                pw = [(p, w) for p, w in pw if p != "fork"]
            return saved([p for p, _ in pw], weights=[w for _, w in pw], k=k)

        rng.choices = choices
        try:
            return super().body(live, budget, fork_depth, allow_proj)
        finally:
            rng.choices = saved


def gen_small(rng):
    g = SmallGen(rng, {})
    main, live = g.body({}, rng.randint(2, 4), 0)
    for v in sorted(live):
        main.append(("drop", v))
    return Program({}, main)


def q(x):
    return "'" + x


def code(body):
    if not body:
        return "nop"
    return " ; ".join(stmt(s) for s in body)


def stmt(st):
    k = st[0]
    if k == "mk":
        fs = ", ".join(f"{lab} : {q(y)}" for lab, y in st[2])
        return f"mk({q(st[1])}, {'(' + fs + ')' if fs else 'none'})"
    if k == "cp":
        return f"cp({q(st[1])}, {q(st[2])})"
    if k == "proj":
        return f"proj({q(st[1])}, {q(st[2])}, {st[3]})"
    if k == "drop":
        return f"drop({q(st[1])})"
    if k == "setf":
        return f"setf({q(st[1])}, {q(st[2])}, {st[3]}, {q(st[4])})"
    if k == "host":
        return f"host({q(st[1])})"
    if k == "fork":
        chs = []
        for ch in st[1]:
            binds = " & ".join(f"({q(p)} <- {q(a)})" for p, a in ch.binds) or "nob"
            chs.append(f"ch({binds}, {code(ch.body)}, {q(ch.ret)}, {q(ch.bindto)})")
        return "fork(" + " || ".join(chs) + ")"
    raise ValueError(st)


def maude(script, timeout):
    with tempfile.NamedTemporaryFile("w", suffix=".maude", dir=MAUDE_DIR, delete=False) as f:
        f.write(script)
        path = f.name
    try:
        try:
            out = subprocess.run(["maude", "-no-banner", "-no-wrap", "-batch", os.path.basename(path)],
                                 cwd=MAUDE_DIR, stdin=subprocess.DEVNULL, capture_output=True,
                                 text=True, timeout=timeout)
        except subprocess.TimeoutExpired:
            return None
        return out.stdout + out.stderr
    finally:
        os.unlink(path)


FACT = re.compile(r"m\('([A-Za-z0-9]+), ([^)]*)\)")


def parse_facts(text):
    facts = set()
    for name, path in FACT.findall(text):
        p = () if path.strip() == "eps" else tuple(path.split())
        facts.add((name, p))
    return facts


def python_facts(prog):
    f = Forward(prog).facts
    return {(x[1], p) for x, p in f.items()} | {("host", ())}


def check_one(prog, timeout):
    c = code(prog.main)
    script = "\n".join([
        "load rc.maude",
        f"red forward({c}) .",
        f"search [1] init({c}, forward({c})) =>! CF:Conf such that not ok(CF:Conf) .",
        f"search [1] init({c}, noAnalysis({c})) =>! CF:Conf such that not ok(CF:Conf) .",
        "quit",
    ])
    out = maude(script, timeout)
    if out is None:
        return None
    if "Warning" in out:
        raise RuntimeError(out[:3000])
    blocks = [b for b in out.split("==========================================")
              if "reduce in" in b or "search [1] in" in b]
    assert len(blocks) == 3, out[:2000]
    return blocks


def main(argv):
    count = int(argv[1]) if len(argv) > 1 else 60
    seed = int(argv[2]) if len(argv) > 2 else 7
    timeout = int(argv[3]) if len(argv) > 3 else 60
    rng = random.Random(seed)
    agree = sound = oracle_agree = checked = skipped = with_races = 0
    for i in range(count):
        prog = gen_small(rng)
        blocks = check_one(prog, timeout)
        if blocks is None:
            skipped += 1
            continue
        checked += 1
        red, s_fwd, s_none = blocks
        mf = parse_facts(red.split("result", 1)[1])
        pf = python_facts(prog)
        if mf == pf:
            agree += 1
        else:
            print(f"program {i}: facts differ\n  maude  {sorted(mf)}\n  python {sorted(pf)}\n  {code(prog.main)}")
        if "No solution" in s_fwd:
            sound += 1
        else:
            print(f"program {i}: forward unsound in Maude\n  {code(prog.main)}")
        m = Machine(prog, NoAtomics(prog))
        m.run()
        py_racy = bool(racy_events(m.events))
        maude_bad = "Solution 1" in s_none
        with_races += py_racy
        if py_racy == maude_bad:
            oracle_agree += 1
        else:
            print(f"program {i}: oracle {py_racy} vs Maude {maude_bad}\n  {code(prog.main)}")
    print(f"programs: {count} (seed {seed}); checked {checked}, skipped after {timeout}s: {skipped}; with races: {with_races}")
    print(f"forward facts agree: {agree}/{checked}")
    print(f"forward race-free in every interleaving (Maude): {sound}/{checked}")
    print(f"race oracle agrees with Maude's search: {oracle_agree}/{checked}")
    return 0 if agree == sound == oracle_agree == checked else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
