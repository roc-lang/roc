"""Hosts: hosted functions, schedulers, and the harness that runs a program.

Hosted functions are deterministic in their arguments, so two runs that make
the same calls get the same results whatever the schedule.

Policies (how the host schedules):
    ready        every suspending call completes at once (ROC_READY)
    pause        every suspending call returns ROC_PENDING; FIFO resumes
    input_order  always act on the task earliest in input order (C9, G3)
    random       random mixes of the above, completions before the hosted
                 function returns, root cancellation, and (ParallelTasks)
                 other threads' resumes run from inside a hosted call
"""

from __future__ import annotations

import random
from collections import Counter
from dataclasses import dataclass, field

from engines import NoneEngine, ProgramCrash, StackEngine, UnwindEngine
from heap import Heap
from runtime import CANCELLED, CRASHED, DONE, PENDING, Runtime, Violation


def _read(heap, i):
    if i % 4 != 0:
        return heap.alloc("tag", "Ok", [i * 3 + 1])
    return heap.alloc("tag", "Err", [heap.alloc("str", f"e{i}", [])])


HOSTED = {
    # name: (suspending, result from argument data)
    "read": (True, _read),
    "slow": (True, lambda heap, i: i + 7),
    "fetch": (True, lambda heap, i: heap.alloc("str", f"s{i}", [])),
    "send": (True, lambda heap, s: len(s[1])),  # takes a heap argument
    "log": (False, lambda heap, s: len(s[1])),
    "tick": (False, lambda heap, i: i + 1),
}
SUSPENDING = {k: v[0] for k, v in HOSTED.items()}


@dataclass
class Op:
    task: object
    name: str
    args: list
    aborted: bool = False


class Policy:
    parallel = False

    def on_call(self, host, t, name):
        return "pending"

    def during_call(self, host):
        pass

    def choose(self, host):
        live = [i for i, op in enumerate(host.ops) if not op.aborted]
        if host.queue:
            return ("resume", 0)
        if live:
            return ("complete", live[0])
        if host.ops:
            return ("complete", 0)
        return None


class ReadyPolicy(Policy):
    name = "ready"

    def on_call(self, host, t, name):
        return "ready"


class PausePolicy(Policy):
    name = "pause"


class InputOrderPolicy(Policy):
    name = "input_order"

    def choose(self, host):
        cands = [(t.path, 0, ("resume", i)) for i, t in enumerate(host.queue)]
        cands += [(op.task.path, 1, ("complete", i)) for i, op in enumerate(host.ops) if not op.aborted]
        if cands:
            return min(cands, key=lambda c: (c[0], c[1]))[2]
        if host.ops:
            return ("complete", 0)
        return None


class RandomPolicy(Policy):
    name = "random"

    def __init__(self, seed, parallel=False, p_cancel=0.0):
        self.rng = random.Random(seed)
        self.parallel = parallel
        self.p_cancel = p_cancel
        self.depth = 0

    def on_call(self, host, t, name):
        return self.rng.choices(["ready", "pending", "early"], [3, 5, 2])[0]

    def during_call(self, host):
        if not self.parallel or self.depth >= 3:
            return
        self.depth += 1
        for _ in range(self.rng.choice([0, 0, 1, 2, 3])):
            if not host.step():
                break
        self.depth -= 1

    def choose(self, host):
        rng = self.rng
        if host.root_status == PENDING and not host.root_cancelled and rng.random() < self.p_cancel:
            return ("cancel_root",)
        acts = [("resume", i) for i in range(len(host.queue))] + [("complete", i) for i in range(len(host.ops))]
        return rng.choice(acts) if acts else None


class Host:
    def __init__(self, rt, heap, policy):
        self.rt, self.heap, self.policy = rt, heap, policy
        self.queue = []
        self.ops = []
        self.effects = []
        self.stats = Counter()
        self.root = None
        self.root_status = PENDING
        self.root_cancelled = False
        self.result = None
        rt.host = self

    # -- what the runtime calls --
    def thread_for(self, t, depth):
        """The thread a resume runs on: nested resumes are other threads."""
        return depth

    def suspending(self, name):
        return SUSPENDING[name]

    def call(self, t, name, args):
        heap = self.heap
        data = [heap.data(a) for a in args]
        self.effects.append((t.path, name, tuple(data)))
        susp, fn = HOSTED[name]
        d = self.policy.on_call(self, t, name) if susp else "ready"
        if d == "ready":
            v = fn(heap, *data)
            for a in args:
                heap.drop(a)
            return ("ready", v)
        op = Op(t, name, args)
        self.ops.append(op)
        self.stats["pending"] += 1
        if d == "early":
            self.stats["early"] += 1
            self.complete(op)
        self.policy.during_call(self)
        return ("pending",)

    def ready(self, t):
        self.queue.append(t)

    def abort(self, t):
        for op in self.ops:
            if op.task is t and not op.aborted:
                op.aborted = True
                for a in op.args:  # P5: arguments released exactly once
                    self.heap.drop(a)
                op.args = None
                self.stats["aborted"] += 1
                return
        raise Violation(f"P4: abort of task {t.id} with no pending operation")

    # -- host side --
    def complete(self, op):
        self.ops.remove(op)
        heap = self.heap
        if op.aborted:
            # The abandoned operation finished anyway: release its result.
            return
        fn = HOSTED[op.name][1]
        outer, heap.task = heap.task, op.task.id  # the result belongs to the task it is for
        v = fn(heap, *[heap.data(a) for a in op.args])
        heap.task = outer
        for a in op.args:
            heap.drop(a)
        self.rt.write_slot(op.task, v)
        self.rt.task_wake(op.task)

    def drained(self):
        """Every operation, aborted ones included, has finished."""

    def step(self):
        act = self.policy.choose(self)
        if act is None:
            return False
        if act[0] == "cancel_root":
            self.root_cancelled = True
            self.rt.cancel_root(self.root)
            return True
        if act[0] == "complete":
            self.complete(self.ops[act[1]])
            return True
        t = self.queue.pop(act[1])
        status = self.rt.resume(t)
        if t is self.root and status != PENDING:
            self.finish_root(status)
        return True

    def finish_root(self, status):
        rt, root = self.rt, self.root
        self.root_status = status
        if status == DONE:
            self.result = ("done", self.heap.data(root.outcome[1]))
        elif status == CRASHED:
            self.result = ("crashed", rt.crash_message(root))
        else:
            self.result = ("cancelled",)
        rt.free_root(root)


@dataclass
class Run:
    result: tuple
    effects: list
    heap: Counter
    engine: Counter
    runtime: Counter
    host: Counter
    leaked_live: int = 0
    extra: dict = field(default_factory=dict)


def run_tasks(lowering, engine_kind, policy, concurrency="Tasks", max_steps=200000, host_cls=None):
    """Run a lowered program as a root task under a host policy; check the
    runtime invariants and the exact heap accounting afterwards."""
    prog = lowering.prog
    heap = Heap()
    rt = Runtime(heap, concurrency)
    host = (host_cls or Host)(rt, heap, policy)
    if engine_kind == "unwind":
        eng = UnwindEngine(prog, lowering, heap, rt)
    else:
        eng = StackEngine(prog, heap, rt)
    rt.engine = eng
    clo = heap.alloc("clo", prog.entry, [])
    host.root = rt.start_root(clo)
    host.queue.append(host.root)
    steps = 0
    while host.root_status == PENDING:
        if not host.step():
            raise Violation("R6: no task can run and the root has not finished")
        steps += 1
        if steps > max_steps:
            raise Violation("R6: the run did not terminate")
    while host.ops:  # late completions of aborted operations
        host.complete(host.ops[0])
    host.drained()
    rt.check_quiescent()
    live = heap.verify(rt.leaked)
    return Run(host.result, host.effects, heap.stats, getattr(eng, "stats", Counter()), rt.stats, host.stats,
               leaked_live=live)


class SyncHost:
    """The host of a None build: every hosted call completes before it returns."""

    def __init__(self, heap):
        self.heap = heap
        self.effects = []

    def call_sync(self, path, name, args):
        heap = self.heap
        data = [heap.data(a) for a in args]
        self.effects.append((path, name, tuple(data)))
        v = HOSTED[name][1](heap, *data)
        for a in args:
            heap.drop(a)
        return v


def run_none(prog):
    heap = Heap()
    host = SyncHost(heap)
    eng = NoneEngine(prog, heap, host)
    try:
        v = eng.run()
    except ProgramCrash as c:
        return Run(("crashed", c.msg), host.effects, heap.stats, Counter(), Counter(), Counter(),
                   extra={"supervised": c.supervised})
    result = ("done", heap.data(v))
    heap.drop(v)
    heap.verify([])
    return Run(result, host.effects, heap.stats, Counter({"steps": eng.steps}), Counter(), Counter())
