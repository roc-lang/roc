#!/usr/bin/env python3
"""Mutation test for the Lifted IR checker.

Each mutant changes one id in a real export: an expression's or pattern's type,
a `local` reference, or a direct callee. The checker should reject it. A mutant
that swaps a type for a structurally equal one, or a local for another of the
same type, is not a bug the checker can catch, so every undetected mutant is
listed for review rather than counted as a failure.

Usage: mutate_lifted.py CHECKER OUT_DIR COUNT SEED FILE.lifted...
"""
import os
import random
import subprocess
import sys


def mutate(lines, rng):
    """Return (kind, line index, new line), or None if this export offers no mutation of the chosen kind."""
    ntypes = int(next(l for l in lines if l.startswith("types ")).split()[1])
    nfns = int(next(l for l in lines if l.startswith("fns ")).split()[1])
    locals_ = [l.split()[1] for l in lines if l.startswith("l ")]
    sites = []
    for i, line in enumerate(lines):
        t = line.split(" ")
        if t[0] in ("e", "p"):
            sites.append(("type", i))
        if t[0] == "e" and t[3] == "local":
            sites.append(("local", i))
        if t[0] == "e" and t[3] == "call_proc" and t[4] != "-":
            sites.append(("callee", i))
    kind, i = rng.choice(sites)
    t = lines[i].split(" ")
    if kind == "type":
        if ntypes < 2:
            return None
        t[2] = str((int(t[2]) + rng.randrange(1, ntypes)) % ntypes)
    elif kind == "local":
        others = [x for x in locals_ if x != t[4]]
        if not others:
            return None
        t[4] = rng.choice(others)
    else:
        if nfns < 2:
            return None
        t[4] = str((int(t[4]) + rng.randrange(1, nfns)) % nfns)
    return kind, i, " ".join(t)


def main():
    if len(sys.argv) < 6:
        sys.exit(__doc__.strip().splitlines()[-1])
    checker, out, count, seed, files = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4]), sys.argv[5:]
    os.makedirs(out, exist_ok=True)
    rng = random.Random(seed)
    mutants = []
    for k in range(count):
        src = rng.choice(files)
        with open(src) as f:
            lines = f.read().split("\n")
        m = mutate(lines, rng)
        if m is None:
            continue
        kind, i, new = m
        lines[i] = new
        path = os.path.join(out, f"m{k}.lifted")
        with open(path, "w") as f:
            f.write("\n".join(lines))
        mutants.append((path, kind, src, i + 1))
    if not mutants:
        sys.exit("no mutants")
    res = subprocess.run([checker, "lifted"] + [m[0] for m in mutants], capture_output=True, text=True).stdout
    status = {l.split("\t")[0]: l.split("\t")[1] for l in res.split("\n") if l and not l.startswith(" ")}
    tally = {}
    for path, kind, src, line in mutants:
        caught = status.get(path) in ("VIOL", "PARSE_ERROR")
        tally.setdefault(kind, [0, 0])[0 if caught else 1] += 1
        if not caught:
            print(f"undetected\t{kind}\t{src}:{line}\t{path}")
    for kind, (c, u) in sorted(tally.items()):
        print(f"{kind}\tcaught {c}\tmissed {u}")


if __name__ == "__main__":
    main()
