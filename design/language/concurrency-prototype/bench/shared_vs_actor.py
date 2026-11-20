"""Operation counts for a semaphore and a pool written three ways: as a server
task that owns its state (concurrency-sessions.md, library.SOURCE), as the
runtime's semaphore cell (sem/acquire), and as a shared variable
(concurrency-shared-variables.md, library.SHARED_SOURCE).

Counts, not times: tasks spawned, task resumes, heap allocations, messages,
and for shared variables the steps run and the steps run again after a wait.
"shared, store", "shared, resume" and "shared, both" are the same shared
variable under alternative wake rules that skip a waiter whose state holds
the value it waited on (Shared.skip_unchanged).
The prototype interprets the IR, so its timings would say nothing about
compiled code.

    python3 bench/shared_vs_actor.py
"""

from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
sys.setrecursionlimit(100000)

import library  # noqa: E402
import runtime  # noqa: E402
from host import InputOrderPolicy, PausePolicy, RandomPolicy, run_tasks  # noqa: E402
from pipeline import build  # noqa: E402

COMMON = """
(proc job (k) eff (let a (host work k)) (ret a))
(proc use_res (k res) eff (let a (host work res)) (ret (add a k)))
"""


def clients(n, make):
    return "(list " + " ".join(make(k) for k in range(1, n + 1)) + ")"


def sem_actor(n):
    return f"""
    (proc client (requests k) eff (let v (call with_permit requests (clo job k))) (ret v))
    (proc clients (requests) eff (let r (call all! {clients(n, lambda k: f"(clo client requests {k})")})) (ret r))
    (proc main () eff (let r (call sem_with 1 (clo clients))) (ret 0))
    """


def sem_cell(n):
    return f"""
    (proc client (s k) eff (let v (call with_cell_permit s (clo job k))) (ret v))
    (proc main () eff
      (let s (sem 1))
      (let r (call all! {clients(n, lambda k: f"(clo client s {k})")}))
      (ret 0))
    """


def sem_shared(n):
    return f"""
    (proc client (s k) eff (let v (call sv_with_permit s (clo job k))) (ret v))
    (proc main () eff
      (let s (shared 1))
      (let r (call all! {clients(n, lambda k: f"(clo client s {k})")}))
      (ret 0))
    """


def pool_actor(n):
    return f"""
    (proc borrower (requests k) eff (let v (call with_resource requests (clo use_res k))) (ret v))
    (proc body (requests) eff (let r (call all! {clients(n, lambda k: f"(clo borrower requests {k})")})) (ret r))
    (proc main () eff (let r (call pool_with 2 (clo body))) (ret 0))
    """


def pool_shared(n):
    return f"""
    (proc user (p k) eff (let v (call sv_with_resource p (clo use_res k))) (ret v))
    (proc main () eff
      (let p (shared (tup (list 1 2) 0)))
      (let r (call all! {clients(n, lambda k: f"(clo user p {k})")}))
      (ret 0))
    """


# The last element is the wake rule (Shared.skip_unchanged): False wakes
# every waiter at each store; "store" readies only waiters whose state
# changed; "resume" skips, just before resuming one, a waiter whose state is
# back to what it waited on; "both" does both.
CASES = [
    ("semaphore", "server task", sem_actor, False),
    ("semaphore", "semaphore cell", sem_cell, False),
    ("semaphore", "shared variable", sem_shared, False),
    ("semaphore", "shared, store", sem_shared, "store"),
    ("semaphore", "shared, resume", sem_shared, "resume"),
    ("semaphore", "shared, both", sem_shared, "both"),
    ("pool", "server task", pool_actor, False),
    ("pool", "shared variable", pool_shared, False),
    ("pool", "shared, store", pool_shared, "store"),
    ("pool", "shared, resume", pool_shared, "resume"),
    ("pool", "shared, both", pool_shared, "both"),
]

POLICIES = [("pause", PausePolicy, "Tasks"), ("input order", InputOrderPolicy, "Tasks"),
            ("random parallel", lambda: RandomPolicy(7, parallel=True), "ParallelTasks")]


def measure(src, mk, conc, skip):
    lows = build(library.SOURCE + library.SHARED_SOURCE + COMMON + src)[1]
    attach = runtime.Runtime.attach

    def attach_skip(self, engine, prog):
        attach(self, engine, prog)
        self.shared.skip_unchanged = skip

    runtime.Runtime.attach = attach_skip
    try:
        r = run_tasks(lows["lss"], "unwind", mk(), conc)
    finally:
        runtime.Runtime.attach = attach
    if r.result != ("done", 0):
        # A server task deadlocks under the input-order schedule, as it does
        # in a None build (concurrency-sessions.md, "On one stack").
        assert "deadlock" in str(r.result), r.result
        return None
    rt = r.runtime
    return {
        "spawns": rt.get("spawns", 0),
        "resumes": rt.get("resumes", 0) - rt.get("resumes skipped", 0),
        "allocs": r.heap.get("alloc", 0),
        "messages": rt.get("messages", 0) + rt.get("posts", 0),
        "steps": rt.get("steps run", 0),
        "again": rt.get("steps run again", 0),
    }


def main():
    cols = ["spawns", "resumes", "allocs", "messages", "steps", "again"]
    print(f"{'library':<10} {'written as':<16} {'schedule':<16} {'clients':>7} " + " ".join(f"{c:>8}" for c in cols))
    for n in (3, 10, 30):
        for lib, how, src, skip in CASES:
            for pname, mk, conc in POLICIES:
                m = measure(src(n), mk, conc, skip)
                row = f"{lib:<10} {how:<16} {pname:<16} {n:>7} "
                print(row + ("deadlock" if m is None else " ".join(f"{m[c]:>8}" for c in cols)))
        print()
    return 0


if __name__ == "__main__":
    sys.exit(main())
