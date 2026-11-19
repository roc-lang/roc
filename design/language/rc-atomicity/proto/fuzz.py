"""Typed random program generator and soundness/precision fuzzer.

Programs are composed from small typed constructs (construction, copy,
projection, drop, in-place update, hosted calls, calls, nested forks), as
AGENTS.md requires of fuzzers. Shapes track which locals have fields:
'L' (no fields), ('N', left, right), or 'U' (unknown, from a call).

Usage:
    python3 fuzz.py [count] [seed]
"""

from __future__ import annotations

import random
import sys

from rcproto import (
    AllAtomic, Analysis, Borrow, Child, Forward, ForwardCtx, ForwardEdge, ForwardPts, Machine, Proc,
    Program, Score, Unify, racy_events, score,
)


def depth(s):
    return 1 + max(depth(s[1]), depth(s[2])) if isinstance(s, tuple) else 0


# Recursive library procedures, written in the IR.
WALK = Proc("walk", ["t"], [
    ("ifnode", "t",
     [("proj", "a", "t", "l"), ("call", "u", "walk", ("a",)), ("drop", "u"), ("drop", "a"),
      ("proj", "b", "t", "r"), ("call", "v", "walk", ("b",)), ("drop", "v"), ("drop", "b"),
      ("mk", "res", ())],
     [("mk", "res", ())]),
    ("drop", "t"),
], "res")

# Path-copying insert at the leftmost leaf.
INSL = Proc("insl", ["t", "n"], [
    ("ifnode", "t",
     [("proj", "a", "t", "l"), ("call", "a2", "insl", ("a", "n")), ("drop", "a"),
      ("setf", "res", "t", "l", "a2"), ("drop", "a2")],
     [("cp", "res", "n"), ("drop", "t")]),
    ("drop", "n"),
], "res")


class Gen:
    def __init__(self, rng, helpers):
        self.rng = rng
        self.helpers = helpers  # name -> arity of generated shape-agnostic procs
        self.n = 0

    def fresh(self, base="v"):
        self.n += 1
        return f"{base}{self.n}"

    def body(self, live, budget, fork_depth, allow_proj=True):
        """Generate statements over `live` (name -> shape); returns (stmts, live)."""
        rng = self.rng
        out = []
        live = dict(live)
        for _ in range(budget):
            names = list(live)
            choice = rng.choices(
                ["mkleaf", "mknode", "cp", "proj", "drop", "setf", "host", "walk", "insl", "helper", "fork"],
                weights=[3, 4, 2, 4, 2, 3, 1, 2, 2, 2, 3 if fork_depth < 2 else 0])[0]
            if choice == "mkleaf" or not names:
                x = self.fresh()
                out.append(("mk", x, ()))
                live[x] = "L"
            elif choice == "mknode":
                a, b = rng.choice(names), rng.choice(names)
                if max(depth(live[a]), depth(live[b])) >= 3:
                    continue
                x = self.fresh()
                out.append(("mk", x, (("l", a), ("r", b))))
                live[x] = ("N", live[a], live[b])
            elif choice == "cp":
                y = rng.choice(names)
                x = self.fresh()
                out.append(("cp", x, y))
                live[x] = live[y]
            elif choice == "proj":
                nodes = [y for y in names if isinstance(live[y], tuple)]
                if not nodes or not allow_proj:
                    continue
                y = rng.choice(nodes)
                lab = rng.choice("lr")
                x = self.fresh()
                out.append(("proj", x, y, lab))
                live[x] = live[y][1] if lab == "l" else live[y][2]
            elif choice == "drop":
                if len(names) < 2:
                    continue
                y = rng.choice(names)
                out.append(("drop", y))
                del live[y]
            elif choice == "setf":
                nodes = [y for y in names if isinstance(live[y], tuple)]
                if not nodes:
                    continue
                y = rng.choice(nodes)
                others = [z for z in names if z != y]
                if not others:
                    continue
                z = rng.choice(others)
                if depth(live[z]) >= 3:
                    continue
                lab = rng.choice("lr")
                x = self.fresh()
                out.append(("setf", x, y, lab, z))
                s = live.pop(y)
                live[x] = ("N", live[z], s[2]) if lab == "l" else ("N", s[1], live[z])
            elif choice == "host":
                out.append(("host", rng.choice(names)))
            elif choice == "walk":
                x = self.fresh()
                out.append(("call", x, "walk", (rng.choice(names),)))
                live[x] = "L"
            elif choice == "insl":
                x = self.fresh()
                out.append(("call", x, "insl", (rng.choice(names), rng.choice(names))))
                live[x] = "U"
            elif choice == "helper":
                if not self.helpers:
                    continue
                f = rng.choice(sorted(self.helpers))
                args = tuple(rng.choice(names) for _ in range(self.helpers[f]))
                x = self.fresh()
                out.append(("call", x, f, args))
                live[x] = "U"
            elif choice == "fork":
                children = []
                for _ in range(rng.choice([2, 2, 3])):
                    k = rng.choice([0, 1, 1, 2])
                    args = rng.sample(names, min(k, len(names)))
                    binds = [(self.fresh("p"), a) for a in args]
                    cbody, clive = self.body({p: live[a] for p, a in binds}, rng.randint(1, 5),
                                             fork_depth + 1, allow_proj)
                    if clive and rng.random() < 0.6:
                        ret = rng.choice(sorted(clive))
                    else:
                        ret = self.fresh("t")
                        cbody.append(("mk", ret, ()))
                        clive[ret] = "L"
                    for v in sorted(clive):
                        if v != ret:
                            cbody.append(("drop", v))
                    b = self.fresh("r")
                    children.append((Child(binds, cbody, ret, b), clive[ret]))
                out.append(("fork", [c for c, _ in children]))
                for c, shape in children:
                    live[c.bindto] = shape
        return out, live


def gen_program(rng):
    helpers = {}
    procs = {"walk": WALK, "insl": INSL}
    g = Gen(rng, helpers)
    for k in range(rng.randint(0, 2)):
        name = f"h{k}"
        params = [f"q{i}" for i in range(rng.randint(1, 2))]
        body, live = g.body({p: "U" for p in params}, rng.randint(1, 5), 1)
        ret = g.fresh("ret")
        if live:
            a = rng.choice(sorted(live))
            body.append(("mk", ret, (("l", a), ("r", rng.choice(sorted(live))))))
        else:
            body.append(("mk", ret, ()))
        for v in sorted(live):
            body.append(("drop", v))
        procs[name] = Proc(name, params, body, ret)
        helpers[name] = len(params)
    main, live = g.body({}, rng.randint(4, 14), 0)
    for v in sorted(live):
        main.append(("drop", v))
    return Program(procs, main)


class NoAtomics(Analysis):
    name = "none"

    def __init__(self, prog):
        pass

    def atomic(self, ctx, local, path):
        return False


ANALYSES = [NoAtomics, AllAtomic, Unify, Forward, ForwardCtx, ForwardPts]
ORDER = [("forward", "unify"), ("forward_ctx", "forward"), ("forward_pts", "forward_ctx"),
         ("forward_edge", "forward_pts")]


def score_trace(events, racy, bits, **extra):
    s = Score(events=len(events), **extra)
    for i, (e, a) in enumerate(zip(events, bits)):
        if e.local == ("host", "host"):
            continue
        s.atomic += a
        s.racy += i in racy
        s.false_atomic += a and i not in racy
        s.unsound += (not a) and i in racy
    return s


def same_events(a, b):
    key = lambda e: (e.op, e.obj, e.local, e.path, e.party)
    return [key(e) for e in a] == [key(e) for e in b]


def check(prog):
    """Score every analysis on one trace; check soundness and the precision order.

    Static analyses never change the trace, so each is scored on the shared
    one. Edge modes and Lean-style marking decide at run time; lending changes
    the trace itself, so it gets its own race oracle run.
    """
    results = {}
    trace = None
    for cls in ANALYSES:
        an = cls(prog)
        m = Machine(prog, an)
        m.run()
        if trace is None:
            trace = m.events
            racy = racy_events(trace)
        assert same_events(m.events, trace)
        bits = [True if e.local == ("host", "host") else an.atomic(e.ctx, e.local, e.path) for e in m.events]
        results[cls.name] = (score_trace(trace, racy, bits, inplace=m.inplace), bits)
    m = Machine(prog, ForwardEdge(prog), edge=True)
    m.run()
    assert same_events(m.events, trace)
    bits = [e.atomic for e in m.events]
    results["forward_edge"] = (score_trace(trace, racy, bits, tests=m.tests), bits)
    lean, _ = score(prog, None, lean=True)
    results["lean"] = (lean, None)
    # Lending: borrowed captures and parameters, on the trace they produce.
    borrow = Borrow(prog)
    for name, cls, edge in [("lent_pts", ForwardPts, False), ("lent_edge", ForwardEdge, True)]:
        an = cls(prog)
        m = Machine(prog, an, edge=edge, borrow=borrow)
        m.run()
        lracy = racy_events(m.events)
        bits = [e.atomic if edge else (e.local == ("host", "host") or an.atomic(e.ctx, e.local, e.path))
                for e in m.events]
        results[name] = (score_trace(m.events, lracy, bits, tests=m.tests), bits)
    order = tuple(all((not b) or c for b, c in zip(results[x][1], results[y][1])) for x, y in ORDER)
    return results, order


NAMES = ["none", "all_atomic", "unify", "forward", "forward_ctx", "forward_pts", "forward_edge", "lean",
         "lent_pts", "lent_edge"]


def run(count, seed):
    rng = random.Random(seed)
    totals = {name: Score() for name in NAMES}
    failures = []
    order_fail = [0] * len(ORDER)
    with_races = 0
    for k in range(count):
        prog = gen_program(rng)
        results, order = check(prog)
        if results["none"][0].racy:
            with_races += 1
        for name, (s, _) in results.items():
            totals[name].add(s)
            if name != "none" and s.unsound:
                failures.append((k, name, s))
        for i, ok in enumerate(order):
            order_fail[i] += not ok
    return totals, failures, order_fail, with_races


def main(argv):
    count = int(argv[1]) if len(argv) > 1 else 2000
    seed = int(argv[2]) if len(argv) > 2 else 1
    totals, failures, order_fail, with_races = run(count, seed)
    print(f"programs: {count} (seed {seed}), with racing events: {with_races}")
    print("precision order violations: " + ", ".join(
        f"{x} not within {y}: {n}" for (x, y), n in zip(ORDER, order_fail)))
    print(f"{'analysis':<12} {'events':>9} {'atomic':>9} {'racy':>7} {'false atomic':>13} {'unsound':>8} "
          f"{'tests':>9} {'walked':>8} {'reuse lost':>11}")
    for name, s in totals.items():
        print(f"{name:<12} {s.events:>9} {s.atomic:>9} {s.racy:>7} {s.false_atomic:>13} {s.unsound:>8} "
              f"{s.tests:>9} {s.walked:>8} {s.reuse_lost:>11}")
    if failures:
        print(f"UNSOUND: {len(failures)} (first: program {failures[0][0]}, {failures[0][1]})")
        return 1
    if any(order_fail):
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
