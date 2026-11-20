"""Plans: inert descriptions of concurrent work (RFC, "Concurrent loops").

A plan is an ordinary value. Building, storing or returning one runs nothing;
only a concurrent loop runs it, and the running set it creates lives in that
loop alone. In Roc a plan is the opaque builtin `Concurrently(a)`; here it is a
tag tree built by the pure builtins in combinators.py:

    Each(list of closures)   one entry per closure, in list order
    Cat(p, q)                p's entries, then q's
    Limit(n, p)              at most n of p's entries run at once
    Shield(p)                p's entries are not cancelled by a loop exit once
                             they have begun; the exit waits for them instead
    Background(p)            p's entries do not keep the loop going: once only
                             background entries are left, the loop cancels
                             them, as at an exit, and ends

`flatten` turns a plan into its entries in input order, and finds the limit
at the plan's root, if its root is a Limit: entries a loop body spawns count
against that limit and no other. Every entry gets its
child index when the loop starts, so a child's identity (its path in the task
tree) does not depend on when the schedule starts it.
"""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass
class Entry:
    clo: object  # an owned closure until the entry starts or is discarded
    limits: tuple  # (limit node id, n) for every Limit node around the entry
    shield: bool
    index: int = -1  # child index in the owner's path
    task: object = None
    state: str = "unstarted"  # unstarted running arrived cancelled crashed discarded
    background: bool = False  # spawned entries never are


def flatten(heap, plan, fresh_limit):
    """The entries of a plan, in input order, and the (limit id, n) at its
    root, or None. Consumes the plan."""
    out, roots = [], []

    def go(v, limits, shield, background):
        o = heap.obj(v)
        if o.tag == "Each":
            for c in heap.obj(o.fields[0]).fields:
                heap.dup(c)
                out.append(Entry(c, limits, shield, background=background))
        elif o.tag == "Cat":
            go(o.fields[0], limits, shield, background)
            go(o.fields[1], limits, shield, background)
        elif o.tag == "Limit":
            n = o.fields[0]
            # The builtin that makes a Limit crashes on n < 1 (a user error),
            # so a plan never holds one.
            assert n >= 1, f"Limit({n}) in a plan"
            lim = (fresh_limit(), n)
            if v == plan:
                roots.append(lim)
            go(o.fields[1], limits + (lim,), shield, background)
        elif o.tag == "Shield":
            go(o.fields[0], limits, True, background)
        elif o.tag == "Background":
            go(o.fields[0], limits, shield, True)
        else:
            raise AssertionError(f"not a plan: {o.tag}")

    go(plan, (), False, False)
    heap.drop(plan)
    return out, (roots[0] if roots else None)


@dataclass
class Group:
    gid: int
    owner: object
    entries: list = field(default_factory=list)
    closing: bool = False  # an exit ran: start nothing more
    ending: bool = False  # only background entries were left: they were cancelled, and Ended follows
    root_limit: tuple = None  # (limit id, n) of a Limit at the plan's root: spawned entries count against it
    open: bool = True  # Ended not yet reported
