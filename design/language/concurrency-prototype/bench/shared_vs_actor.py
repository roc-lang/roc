"""Operation counts for a semaphore and a pool written three ways: as a server
task that owns its state (concurrency-sessions.md, library.SOURCE), as the
runtime's semaphore cell (sem/acquire), and as a shared variable
(concurrency-shared-variables.md, library.SHARED_SOURCE).

Counts, not times: tasks spawned, task resumes (every dispatch of a task by
the host, including a readied waiter that only runs its update function and
parks again), heap allocations and messages; and for Shared, the update
functions run by tasks, the probes the runtime runs for a waiter without
storing, and the state comparisons that park a dispatched waiter again. The
"shared" rows differ only in the wake rule; see CASES.
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
from host import CoroutinePolicy, InputOrderPolicy, PausePolicy, RandomPolicy, run_tasks  # noqa: E402
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


# The last element is the wake rule: (Shared.wake_rule, Shared.skip_unchanged).
#   all        a store readies every waiter, which runs its function again
#              when it is dispatched
#   all+cmp    the same, but a dispatched waiter whose state equals the one
#              it waited on parks again without running its function
#   first      a store probes the waiters, oldest first, and readies only the first
#              that would not wait; one that then does not store probes on (the rule)
#   latest     the same, most recent first (the earlier rule)
CASES = [
    ("semaphore", "server task", sem_actor, None),
    ("semaphore", "semaphore cell", sem_cell, None),
    ("semaphore", "shared, all", sem_shared, ("all", False)),
    ("semaphore", "shared, all+cmp", sem_shared, ("all", "resume")),
    ("semaphore", "shared, first", sem_shared, ("first", False)),
    ("semaphore", "shared, latest", sem_shared, ("latest", False)),
    ("pool", "server task", pool_actor, None),
    ("pool", "shared, all", pool_shared, ("all", False)),
    ("pool", "shared, all+cmp", pool_shared, ("all", "resume")),
    ("pool", "shared, first", pool_shared, ("first", False)),
    ("pool", "shared, latest", pool_shared, ("latest", False)),
]

POLICIES = [("pause", PausePolicy, "Tasks"), ("input order", InputOrderPolicy, "Tasks"),
            ("coroutine", CoroutinePolicy, "Tasks"),
            ("random parallel", lambda: RandomPolicy(7, parallel=True), "ParallelTasks")]


def measure(src, mk, conc, rule):
    lows = build(library.SOURCE + library.SHARED_SOURCE + COMMON + src)[1]
    attach = runtime.Runtime.attach

    def attach_skip(self, engine, prog):
        attach(self, engine, prog)
        if rule is not None:
            self.shared.wake_rule, self.shared.skip_unchanged = rule

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
        "resumes": rt.get("resumes", 0),
        "allocs": r.heap.get("alloc", 0),
        "messages": rt.get("messages", 0) + rt.get("posts", 0),
        "f runs": rt.get("steps run", 0),
        "probes": rt.get("probes", 0),
        "compares": rt.get("resumes skipped", 0),
    }


def main():
    cols = ["spawns", "resumes", "allocs", "messages", "f runs", "probes", "compares"]
    print(f"{'library':<10} {'written as':<16} {'schedule':<16} {'clients':>7} " + " ".join(f"{c:>8}" for c in cols))
    for n in (3, 10, 30):
        for lib, how, src, rule in CASES:
            for pname, mk, conc in POLICIES:
                m = measure(src(n), mk, conc, rule)
                row = f"{lib:<10} {how:<16} {pname:<16} {n:>7} "
                print(row + ("deadlock" if m is None else " ".join(f"{m[c]:>8}" for c in cols)))
        print()
    return 0


if __name__ == "__main__":
    sys.exit(main())
