"""The platform-obligation conformance harness (RFC, "Platform obligations").

The harness is the runtime's monitors (runtime.py) and the exact heap check
(heap.py): a host linked against them has every ABI event checked as it
happens. This file is the adversarial half: one host per way of breaking an
obligation, each run over a few scenarios that give it the chance. Every
broken host must be caught, with the obligation named, and the conforming host
must pass the same scenarios.

Two kinds of host are modeled beyond the plain one:
    ConfinedHost  its allocator releases an allocation only on the thread
                  that made it, so it must keep tasks on one thread (P6)
    RegionHost    it keeps aborted operations' arguments until they finish,
                  and reclaims a crashed root's memory in bulk, leaks
                  included, once nothing outside the tree can reach it (P9)

P2, handoff ordering, needs real threads: bench/wake_tsan.c checks it under
ThreadSanitizer.

Usage:
    python3 conformance.py
"""

from __future__ import annotations

import sys

from heap import HeapError
from host import Host, PausePolicy, RandomPolicy, ReadyPolicy, run_tasks
from pipeline import build
from runtime import Violation

SCENARIOS = {
    "sequential": """
        (proc main () eff
          (let a (host read 1))
          (let b (host fetch 2))
          (let n (host slow 3))
          (let m (host send "hello"))
          (ret (tup a b n m)))
    """,
    "race": """
        (proc slow_child (s) eff (ret (host send s)))
        (proc main () eff
          (let fs (list (clo slow_child "bb") (clo slow_child "ccc")))
          (ret (call race! (clo slow_child "a") fs)))
    """,
    "crash": """
        (proc main () eff
          (let a (host slow 1))
          (crash "boom"))
    """,
    "crash_below_a_caller": """
        (proc inner (n) eff (let x (host slow n)) (crash "boom"))
        (proc main () eff
          (let keep (str "kept"))
          (let v (call inner 1))
          (ret (tup keep v)))
    """,
    "crash_with_pending": """
        (proc sender (s) eff (ret (host send s)))
        (proc boom () eff (let x (host slow 1)) (crash "boom"))
        (proc main () eff
          (let fs (list (clo sender "bb") (clo sender "ccc")))
          (ret (call race! (clo boom) fs)))
    """,
    "all": """
        (proc child (n) eff (ret (host slow n)))
        (proc main () eff
          (let fs (list (clo child 1) (clo child 2) (clo child 3)))
          (ret (call all! fs)))
    """,
}


# ---------------------------------------------------------------- host kinds


class ConfinedHost(Host):
    def __init__(self, rt, heap, policy):
        super().__init__(rt, heap, policy)
        heap.confined = True


class RegionHost(Host):
    reclaim_at_crash = False  # the broken variant: before late completions finish

    def __init__(self, rt, heap, policy):
        super().__init__(rt, heap, policy)
        self.crashed = False

    def abort(self, t):
        # P5 allows releasing an aborted operation's arguments when it
        # finishes instead: this host does, so it holds them meanwhile.
        for op in self.ops:
            if op.task is t and not op.aborted:
                op.aborted = True
                self.stats["aborted"] += 1
                return
        raise Violation(f"P4: abort of task {t.id} with no pending operation")

    def complete(self, op):
        if op.aborted:
            self.ops.remove(op)
            for a in op.args:
                self.heap.drop(a)
            return
        super().complete(op)

    def finish_root(self, status):
        super().finish_root(status)
        if status == "CRASHED":
            self.crashed = True
            if self.reclaim_at_crash:
                self.reclaim()

    def drained(self):
        if self.crashed and not self.reclaim_at_crash:
            self.reclaim()

    def reclaim(self):
        tree = set(self.rt.tasks)
        heap = self.heap
        heap.bulk_free([i for i, o in heap.objs.items() if not o.freed and o.owner in tree])
        self.rt.leaked = []  # reclaimed: a region host leaks nothing


class EagerRegionHost(RegionHost):
    reclaim_at_crash = True


CONFORMING = [
    # (host, policies, concurrency)
    (Host, lambda: [ReadyPolicy(), PausePolicy(), RandomPolicy(3), RandomPolicy(4, parallel=True)], None),
    (ConfinedHost, lambda: [ReadyPolicy(), PausePolicy(), RandomPolicy(3)], "Tasks"),
    (RegionHost, lambda: [ReadyPolicy(), PausePolicy(), RandomPolicy(3), RandomPolicy(4, parallel=True)], None),
]


# ---------------------------------------------------------------- broken hosts
# Each is (obligation, what it does wrong, host class, policy factory, concurrency).


class ResumeTwice(PausePolicy):
    def choose(self, host):
        act = super().choose(host)
        if act and act[0] == "resume":
            host.queue.append(host.queue[act[1]])
        return act


class ResumeUnreadied(PausePolicy):
    """Resumes a parked task that nothing readied."""

    def choose(self, host):
        act = super().choose(host)
        if act and act[0] == "complete" and host.root is not None and host.root.wake == "parked":
            host.queue.append(host.root)
            return ("resume", len(host.queue) - 1)
        return act


class ResumeInsideHosted(RandomPolicy):
    """Runs another resume from inside a hosted function, in a Tasks build."""

    def __init__(self):
        super().__init__(5, parallel=True)

    def on_call(self, host, t, name):
        return "pending"


class WakeTwice(Host):
    def complete(self, op):
        super().complete(op)
        if not op.aborted:
            self.rt.task_wake(op.task)


class WakeBeforeWrite(Host):
    def complete(self, op):
        if not op.aborted:
            self.rt.task_wake(op.task)
        super().complete(op)


class WriteTwice(Host):
    def complete(self, op):
        super().complete(op)
        if not op.aborted:
            self.rt.write_slot(op.task, 0)


class WriteAfterAbort(Host):
    def complete(self, op):
        if op.aborted:
            self.ops.remove(op)
            self.rt.write_slot(op.task, 0)
            return
        super().complete(op)


class WakeAfterAbort(Host):
    def complete(self, op):
        if op.aborted:
            self.ops.remove(op)
            self.rt.task_wake(op.task)
            return
        super().complete(op)


class ReleaseArgsTwice(Host):
    def complete(self, op):
        args = None if op.aborted else list(op.args)
        super().complete(op)
        for a in args or []:
            self.heap.drop(a)


class KeepArgs(Host):
    def complete(self, op):
        if not op.aborted:
            for a in op.args:
                self.heap.dup(a)
        super().complete(op)


class KeepAbortedArgs(Host):
    def abort(self, t):
        for op in self.ops:
            if op.task is t and not op.aborted:
                op.aborted = True
                op.args = None  # forgets to release them
                return


class NeverComplete(PausePolicy):
    def choose(self, host):
        if host.queue:
            return ("resume", 0)
        return None


class FreeRootTwice(Host):
    def finish_root(self, status):
        super().finish_root(status)
        self.rt.free_root(self.root)


class ReadWrongOutcome(Host):
    def finish_root(self, status):
        self.rt.crash_message(self.root)
        super().finish_root(status)


BROKEN = [
    ("P1", "resumes a task twice for one ready", None, ResumeTwice, "Tasks", "P1"),
    ("P1", "resumes a parked task nothing readied", None, ResumeUnreadied, "Tasks", "P1"),
    ("P10", "resumes another task inside a hosted call in a Tasks build", None, ResumeInsideHosted, "Tasks",
     "P10"),
    ("P3", "calls roc_task_wake twice for one result", WakeTwice, PausePolicy, "Tasks", "P3"),
    ("P3", "calls roc_task_wake before writing *out", WakeBeforeWrite, PausePolicy, "Tasks", "P3"),
    ("P3", "writes *out twice", WriteTwice, PausePolicy, "Tasks", "P3"),
    ("P4", "writes *out after roc_task_abort", WriteAfterAbort, PausePolicy, "Tasks", "P3/P4"),
    ("P4", "wakes after roc_task_abort", WakeAfterAbort, PausePolicy, "Tasks", "P3/P4"),
    ("P5", "releases hosted arguments twice", ReleaseArgsTwice, PausePolicy, "Tasks", "heap"),
    ("P5", "never releases hosted arguments", KeepArgs, PausePolicy, "Tasks", "heap"),
    ("P5", "never releases an aborted operation's arguments", KeepAbortedArgs, PausePolicy, "Tasks", "heap"),
    ("P7", "never completes a pending operation", None, NeverComplete, "Tasks", "R6"),
    ("P6", "runs tasks on several threads with a thread-confined allocator", ConfinedHost,
     lambda: RandomPolicy(4, parallel=True), "ParallelTasks", "heap: P6"),
    ("P8", "frees a root twice", FreeRootTwice, PausePolicy, "Tasks", "P8"),
    ("P9", "reclaims a crashed root's region while an aborted operation still holds its arguments",
     EagerRegionHost, PausePolicy, "Tasks", "heap"),
    ("P8", "reads a crash message of a root that did not crash", ReadWrongOutcome, PausePolicy, "Tasks", "P8"),
]


def run_one(lows, host_cls, policy, conc, mode="lss", engine="unwind"):
    """None if the run conformed, else what caught it."""
    try:
        run_tasks(lows[mode], engine, policy, conc, host_cls=host_cls)
    except Violation as v:
        return str(v)
    except HeapError as e:
        return f"heap: {e}"
    return None


def suite():
    """Yields (obligation, description, caught by, expected tag) per broken host,
    and checks the conforming host passes every scenario."""
    built = {name: build(src)[1] for name, src in SCENARIOS.items()}
    for host_cls, policies, conc in CONFORMING:
        for name, lows in built.items():
            for mode in ("lss", "boxy"):
                for engine in ("unwind", "stack"):
                    for pol in policies():
                        c = conc or ("ParallelTasks" if pol.parallel else "Tasks")
                        got = run_one(lows, host_cls, pol, c, mode, engine)
                        if got is not None:
                            raise AssertionError(f"conforming {host_cls.__name__} rejected on {name}: {got}")
    for obligation, what, host_cls, pol, conc, tag in BROKEN:
        caught = None
        for name, lows in built.items():
            caught = run_one(lows, host_cls, pol(), conc)
            if caught is not None:
                break
        yield obligation, what, caught, tag


def main():
    bad = 0
    for obligation, what, caught, tag in suite():
        ok = caught is not None and caught.startswith(tag)
        bad += not ok
        print(f"{obligation:<4} {'caught' if ok else 'MISSED':<7} {what}: {caught}")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
