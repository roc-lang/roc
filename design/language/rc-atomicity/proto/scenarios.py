"""Demonstration programs for the claims in concurrency-rc-atomicity.md.

These are illustrations, not a fuzzer (fuzz.py composes random programs).
Each one isolates one effect and prints, per analysis, how many count updates
are atomic, how many can really race, how many are atomic without any
possible race ("false atomic"), and, for Lean-style marking, the extra costs.

Usage:
    python3 scenarios.py
"""

from __future__ import annotations

import sys

from fuzz import INSL, WALK, check
from rcproto import Child, Proc, Program

LIB = {"walk": WALK, "insl": INSL}


def build_tree(prefix, n, out):
    """Statements that build a left spine of n nodes in local `{prefix}0` by path copying."""
    out.append(("mk", f"{prefix}0", ()))
    for i in range(n):
        out.append(("mk", f"{prefix}leaf{i}", ()))
        out.append(("mk", f"{prefix}node{i}", (("l", f"{prefix}leaf{i}"), ("r", f"{prefix}leaf{i}"))))
        out.append(("drop", f"{prefix}leaf{i}"))
        out.append(("call", f"{prefix}{i + 1}", "insl", (f"{prefix}{i}", f"{prefix}node{i}")))
        out.append(("drop", f"{prefix}node{i}"))
        out.append(("drop", f"{prefix}{i}"))
    return f"{prefix}{n}"


def child(binds, body, ret, bindto):
    return Child(binds, body, ret, bindto)


def spreading():
    """The parent builds a large private tree with `insl`; two children use
    `insl` on a small shared value. Only the children's work can race."""
    main = []
    t = build_tree("t", 12, main)
    main.append(("mk", "s", ()))
    kids = []
    for i in range(2):
        body = [("mk", f"x{i}", ()), ("call", f"y{i}", "insl", (f"x{i}", f"p{i}")), ("drop", f"p{i}"),
                ("drop", f"x{i}")]
        kids.append(child([(f"p{i}", "s")], body, f"y{i}", f"r{i}"))
    main.append(("fork", kids))
    t2 = build_tree("u", 12, main)
    main += [("call", "w", "walk", (t,)), ("drop", "w"), ("call", "w2", "walk", (t2,)), ("drop", "w2"),
             ("drop", t), ("drop", t2), ("drop", "s"), ("drop", "r0"), ("drop", "r1")]
    return Program(dict(LIB), main)


def moves():
    """Each child receives its own large tree, built by the parent: nothing is shared."""
    main = []
    roots = [build_tree(f"m{i}_", 10, main) for i in range(2)]
    kids = []
    for i, r in enumerate(roots):
        body = [("call", f"w{i}", "walk", (f"p{i}",)), ("drop", f"p{i}")]
        kids.append(child([(f"p{i}", r)], body, f"w{i}", f"r{i}"))
    main.append(("fork", kids))
    main += [("drop", r) for r in roots] + [("drop", "r0"), ("drop", "r1")]
    return Program(dict(LIB), main)


def path_copy_shared():
    """Each child inserts into the same shared tree by path copying: new nodes
    along the path, shared subtrees everywhere else."""
    main = []
    t = build_tree("t", 10, main)
    kids = []
    for i in range(2):
        body = [("mk", f"n{i}", ()), ("call", f"u{i}", "insl", (f"p{i}", f"n{i}")),
                ("call", f"w{i}", "walk", (f"u{i}",)), ("drop", f"u{i}"), ("drop", f"n{i}"), ("drop", f"p{i}")]
        kids.append(child([(f"p{i}", t)], body, f"w{i}", f"r{i}"))
    main.append(("fork", kids))
    main += [("drop", t), ("drop", "r0"), ("drop", "r1")]
    return Program(dict(LIB), main)


def shared_read():
    """Both children walk the same tree. Every update in the children races."""
    main = []
    t = build_tree("t", 10, main)
    kids = [child([(f"p{i}", t)], [("call", f"w{i}", "walk", (f"p{i}",)), ("drop", f"p{i}")], f"w{i}", f"r{i}")
            for i in range(2)]
    main.append(("fork", kids))
    main += [("drop", t), ("drop", "r0"), ("drop", "r1")]
    return Program(dict(LIB), main)


def host_data():
    """A value the host keeps, and an alias made before the hosted call."""
    main = []
    t = build_tree("t", 6, main)
    main += [("cp", "a", t), ("host", "a"), ("call", "w", "walk", (t,)), ("drop", "w"),
             ("drop", "a"), ("drop", t)]
    return Program(dict(LIB), main)


SCENARIOS = [spreading, moves, path_copy_shared, shared_read, host_data]


def main():
    for make in SCENARIOS:
        prog = make()
        results, order = check(prog)
        print(f"\n{make.__name__}: {make.__doc__.split(chr(10))[0]}")
        print(f"  {'analysis':<13} {'events':>7} {'atomic':>7} {'racy':>6} {'false atomic':>13} "
              f"{'unsound':>8} {'tests':>7} {'walked':>7} {'reuse lost':>11}")
        for name in ["unify", "forward", "forward_ctx", "forward_pts", "forward_edge", "lean",
                     "lent_pts", "lent_edge"]:
            s = results[name][0]
            print(f"  {name:<13} {s.events:>7} {s.atomic:>7} {s.racy:>6} {s.false_atomic:>13} "
                  f"{s.unsound:>8} {s.tests:>7} {s.walked:>7} {s.reuse_lost:>11}")
        assert all(results[n][0].unsound == 0 for n in results if n != "none")
        assert all(order)
    return 0


if __name__ == "__main__":
    sys.exit(main())
