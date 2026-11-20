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

`flatten` turns a plan into its entries in input order. Every entry gets its
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


def flatten(heap, plan, fresh_limit):
    """The entries of a plan, in input order. Consumes the plan."""
    out = []

    def go(v, limits, shield):
        o = heap.obj(v)
        if o.tag == "Each":
            for c in heap.obj(o.fields[0]).fields:
                heap.dup(c)
                out.append(Entry(c, limits, shield))
        elif o.tag == "Cat":
            go(o.fields[0], limits, shield)
            go(o.fields[1], limits, shield)
        elif o.tag == "Limit":
            n = o.fields[0]
            # The builtin that makes a Limit crashes on n < 1 (a user error),
            # so a plan never holds one.
            assert n >= 1, f"Limit({n}) in a plan"
            go(o.fields[1], limits + ((fresh_limit(), n),), shield)
        elif o.tag == "Shield":
            go(o.fields[0], limits, True)
        else:
            raise AssertionError(f"not a plan: {o.tag}")

    go(plan, (), False)
    heap.drop(plan)
    return out


@dataclass
class Group:
    gid: int
    owner: object
    entries: list = field(default_factory=list)
    closing: bool = False  # an exit ran: start nothing more
    open: bool = True  # Ended not yet reported
