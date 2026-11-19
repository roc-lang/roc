"""The task runtime: wake protocol, task tree, cancellation and crashes.

This is the Roc side of the host ABI (RFC, "Host ABI"): `resume`, `wake`,
`cancel_root`, `free_root` and `crash_message` are what the host calls; the
host implements `ready` and `abort`. It is shared by the stackful and the
unwinding engines, so differential runs isolate the lowering.

Resumes are atomic steps here; a ParallelTasks host interleaves them by
running other tasks' resumes from inside a hosted call (another thread, as far
as this task can tell). The finer interleavings of the wake state itself are
model-checked in maude/wake.maude.

The monitors check the runtime invariants R1-R5 and the platform obligations
as they happen; a violation raises `Violation` naming the invariant.
"""

from __future__ import annotations

from collections import Counter

from heap import Handle

PENDING, DONE, CANCELLED, CRASHED = "PENDING", "DONE", "CANCELLED", "CRASHED"

# The wake protocol.
#   "flag"        the RFC as first written: a wake that arrives while the task
#                 runs sets "woken", and the next park continues instead.
#   "flag_clear"  flag, plus a wait clears "woken" before checking.
#   "counted"     the wake word counts undelivered completions; parking
#                 succeeds only when nothing is deliverable, and a wake
#                 readies the task only if it was parked.
# The fuzzer breaks "flag" (a wake consumed synchronously goes stale);
# maude/wake.maude breaks "flag_clear" under real thread interleavings and
# verifies "counted". The others stay selectable for the regression tests.
PROTOCOL = "counted"


class Violation(Exception):
    pass


def require(cond, code, msg):
    if not cond:
        raise Violation(f"{code}: {msg}")


class Task:
    __slots__ = ("id", "parent", "path", "children", "closure", "phase", "wake", "cancel_req",
                 "waiting", "op_pending", "slot", "reports", "await_kids", "outcome", "dead",
                 "gen", "frames", "records", "native", "pending", "spawned", "parks", "readies",
                 "freed", "status", "cell", "op_written", "op_woken", "sup", "final")

    def __init__(self, tid, parent, path, closure):
        self.id, self.parent, self.path, self.closure = tid, parent, path, closure
        self.children = []
        self.phase = "new"  # new live finishing cancelling crashing finished
        self.wake = "queued"  # queued running parked woken (None once finished)
        self.cancel_req = False
        self.waiting = None  # None, "host" or "children"
        self.op_pending = False
        self.slot = None
        self.reports = []  # (child, report value), oldest first
        self.await_kids = []
        self.outcome = None
        self.dead = False  # runs no more Roc code
        self.gen = None
        self.frames = []
        self.records = []
        self.native = []
        self.pending = False
        self.spawned = 0
        self.parks = 0
        self.readies = 0
        self.freed = False
        self.status = PENDING
        self.cell = None  # the pending cell for deferred erased calls, in task storage (C4)
        self.op_written = False  # the pending operation's *out has been written
        self.op_woken = False  # ... and roc_task_wake has been called for it
        self.sup = None  # the open supervise! call: its max_restarts and the children it started (R7)
        self.final = None  # the data of this task's result or crash message, kept for R7


class Runtime:
    def __init__(self, heap, concurrency):
        assert concurrency in ("Tasks", "ParallelTasks")
        self.heap, self.concurrency = heap, concurrency
        self.engine = None
        self.host = None
        self.tasks = {}
        self.active = []  # tasks whose resume is in progress, outermost first
        self.leaked = []  # values owned by crashed tasks' native frames
        self.stats = Counter()

    # ------------------------------------------------------------------ ABI
    def start_root(self, closure):
        t = self.new_task(None, (), closure)
        t.readies = 1  # the host queues a new root itself
        self.stats["roots"] += 1
        return t

    def resume(self, t):
        """roc_task_resume."""
        require(t.wake == "queued", "P1/R2", f"task {t.id} resumed while {t.wake}")
        require(t not in self.active, "R2", f"task {t.id} re-entered")
        if self.active and self.concurrency == "Tasks":
            raise Violation("P10: two resumes at once in a Tasks build")
        self.active.append(t)
        t.wake = "running"
        self.stats["resumes"] += 1
        heap = self.heap
        outer = heap.task, heap.thread
        # A resume nested inside another task's hosted call runs on another thread.
        heap.task, heap.thread = t.id, self.host.thread_for(t, len(self.active) - 1)
        try:
            while True:
                if t.phase == "finished":
                    break
                if t.cancel_req and t.phase in ("new", "live"):
                    self.finish_code_cancelled(t)
                    break
                if t.phase in ("cancelling", "crashing", "finishing"):
                    # Readied only to report a terminal status (roots).
                    break
                out = self.engine.start(t) if t.phase == "new" else self.engine.resume(t)
                t.phase = "live"
                if out[0] == "done":
                    self.finish_code_done(t, out[1])
                    break
                if out[0] == "crash":
                    self.finish_code_crashed(t, out[1], out[2])
                    break
                # Pending: park, unless cancelled or woken meanwhile.
                if t.cancel_req:
                    self.finish_code_cancelled(t)
                    break
                if PROTOCOL == "counted":
                    if self.deliverable(t):
                        self.stats["woken_while_running"] += 1
                        continue
                elif t.wake == "woken":
                    t.wake = "running"
                    self.stats["woken_while_running"] += 1
                    continue
                t.wake = "parked"
                t.parks += 1
                return PENDING
        finally:
            self.active.remove(t)
            heap.task, heap.thread = outer
        if t.phase == "finished":
            t.wake = None
            return t.status
        # Finished code but waiting on its subtree: nothing to run until then.
        t.wake = "parked"
        t.parks += 1
        return PENDING

    def write_slot(self, t, value):
        """The host writes *out (completion, step 1)."""
        require(t.op_pending, "P3/P4", f"write to task {t.id} with no pending operation")
        t.op_pending = False
        t.op_written = True
        t.slot = value

    def task_wake(self, t):
        """roc_task_wake: the host says the pending operation's *out is written."""
        require(t.wake is not None and t.waiting == "host", "P3/P4",
                f"roc_task_wake of task {t.id}, which waits on no hosted operation")
        require(t.op_written, "P3", f"roc_task_wake of task {t.id} before its result was written")
        require(not t.op_woken, "P3", f"roc_task_wake of task {t.id} twice for one operation")
        t.op_woken = True
        self.wake(t)

    def wake(self, t):
        """roc_task_wake, and the wake a finishing child sends its parent."""
        require(t.wake is not None, "P3", f"wake of finished task {t.id}")
        if t.wake == "parked":
            t.wake = "queued"
            t.readies += 1
            self.stats["readies"] += 1
            self.host.ready(t)
        elif t.wake == "running" and PROTOCOL != "counted":
            t.wake = "woken"
        # woken or queued: already recorded

    def cancel_root(self, root):
        require(root.parent is None, "P8", "roc_task_cancel on a child")
        self.cancel_task(root)

    def crash_message(self, root):
        require(root.status == CRASHED, "P8", "crash message of a root that did not crash")
        return self.heap.data(root.outcome[1])

    def free_root(self, root):
        require(root.phase == "finished" and not root.freed, "P8", f"bad roc_task_free of {root.id}")
        root.freed = True
        if root.outcome[0] == "done":
            self.heap.drop(root.outcome[1])
        elif root.outcome[0] == "crashed":
            self.heap.drop(root.outcome[1])

    # ------------------------------------------------- engine operations
    def step(self, t):
        require(not t.dead, "R4/R5", f"task {t.id} ran Roc code after it was cancelled or crashed")
        require(t in self.active, "R2", f"task {t.id} ran outside a resume")

    def hosted(self, t, name, args):
        suspending = self.host.suspending(name)
        if suspending:
            t.waiting, t.op_pending = "host", True
            t.op_written = t.op_woken = False
        r = self.host.call(t, name, args)
        if r[0] == "ready":
            t.waiting, t.op_pending = None, False
        else:
            require(suspending, "P3", f"{name} does not suspend but returned PENDING")
        return r

    def sup_begin(self, t, max_restarts):
        """An engine is about to call supervise!."""
        require(t.sup is None, "R7", f"task {t.id} entered supervise! twice at once")
        t.sup = (max_restarts, [])

    def sup_end(self, t, v):
        """supervise! returned v: from the last child, which completed (R7)."""
        mx, kids = t.sup
        t.sup = None
        require(kids and kids[-1].status == DONE, "R7", f"supervise! in task {t.id} returned without a child result")
        require(self.heap.data(v) == kids[-1].final, "R7", "supervise! returned something other than its child's result")
        self.stats["R7 returned"] += 1

    def spawn(self, t, closure):
        if t.sup is not None:
            mx, kids = t.sup
            require(len(kids) <= mx, "R7", f"supervise! in task {t.id} restarted more than {mx} times")
            require(not kids or kids[-1].status == CRASHED, "R7",
                    f"supervise! in task {t.id} restarted a child that did not crash")
        c = self.new_task(t, t.path + (t.spawned,), closure)
        t.spawned += 1
        t.children.append(c)
        self.stats["spawns"] += 1
        if t.sup is not None:
            t.sup[1].append(c)
        self.host.ready(c)
        c.readies += 1
        return Handle(c.id)

    def await_any(self, t, kids):
        heap = self.heap
        t.await_kids = [h.task for h in heap.obj(kids).fields]
        heap.drop(kids)
        # A wait clears any recorded wake *before* it checks for reports. A
        # report always precedes its wake, so a wake recorded earlier is
        # either for a report this check consumes, or stale. Without this a
        # wake consumed synchronously (e.g. the task cancelled a parked child
        # and then read its report at once) would make a later wait continue
        # instead of parking, and resume with nothing to deliver.
        if t.wake == "woken" and PROTOCOL == "flag_clear":
            t.wake = "running"
            self.stats["wakes_cleared_at_wait"] += 1
        r = self.take_report(t)
        if r is not None:
            return ("ready", r)
        require(any(self.tasks[c].phase != "finished" for c in t.await_kids), "R6",
                f"task {t.id} awaits with nothing left to report")
        t.waiting = "children"
        return ("pending",)

    def cancel(self, t, h):
        self.cancel_task(self.tasks[h.task])

    def resume_value(self, t):
        if t.waiting == "host":
            require(not t.op_pending, "R1", f"task {t.id} resumed before its result was written")
            v, t.slot, t.waiting = t.slot, None, None
            return v
        if t.waiting == "children":
            r = self.take_report(t)
            require(r is not None, "R1", f"task {t.id} resumed with no report")
            t.waiting = None
            return r
        raise Violation(f"R1: task {t.id} resumed while waiting on nothing")

    def deliverable(self, t):
        """The counted wake word is nonzero: a completion is waiting to be delivered."""
        if t.waiting == "host":
            return not t.op_pending
        if t.waiting == "children":
            return any(c.id in t.await_kids for c, _ in t.reports)
        return False

    # ----------------------------------------------------------- internals
    def new_task(self, parent, path, closure):
        tid = len(self.tasks) + 1
        t = Task(tid, parent, path, closure)
        self.tasks[tid] = t
        return t

    def take_report(self, t):
        for k, (c, rep) in enumerate(t.reports):
            if c.id in t.await_kids:
                del t.reports[k]
                i = t.await_kids.index(c.id)
                kind = rep[0]
                if kind == "done":
                    return self.heap.alloc("tag", "Done", [i, rep[1]])
                if kind == "cancelled":
                    return self.heap.alloc("tag", "Cancelled", [i])
                return self.heap.alloc("tag", "Crashed", [i, rep[1]])
        return None

    def cancel_task(self, c):
        if c.phase == "finished" or c.cancel_req:
            return
        c.cancel_req = True
        self.stats["cancel_requests"] += 1
        if c.phase in ("finishing", "cancelling", "crashing"):
            return
        if c.wake == "parked":
            self.finish_code_cancelled(c)
        # queued, running or woken: handled at its next resume or park (R4)

    def release_state(self, t):
        """Drop everything a task's code owned, exactly."""
        if t.waiting == "host":
            if t.op_pending:
                self.host.abort(t)
                t.op_pending = False
            if t.slot is not None:
                self.heap.drop(t.slot)
                t.slot = None
        t.waiting = None
        self.engine.drop_paused(t)
        for _, rep in t.reports:
            if rep[0] in ("done", "crashed"):
                self.heap.drop(rep[1])
        t.reports = []

    def finish_code_cancelled(self, t):
        t.dead = True
        self.release_state(t)
        t.phase, t.outcome = "cancelling", ("cancelled",)
        self.stats["cancelled"] += 1
        for c in t.children:
            self.cancel_task(c)
        self.check_finished(t)

    def finish_code_done(self, t, v):
        t.dead = True
        require(all(c.phase == "finished" for c in t.children), "R3",
                f"task {t.id} returned with live children")
        require(not t.reports, "R3", f"task {t.id} returned with unread reports")
        t.phase, t.outcome = "finishing", ("done", v)
        self.check_finished(t)

    def finish_code_crashed(self, t, msg, leaked):
        if t.sup is not None:
            # A crash inside supervise! is its giving up: after exactly
            # max_restarts restarts, with the last child's message.
            mx, kids = t.sup
            require(len(kids) == mx + 1 and all(k.status == CRASHED for k in kids), "R7",
                    f"supervise! in task {t.id} gave up after {len(kids) - 1} of {mx} restarts")
            require(self.heap.data(msg) == kids[-1].final, "R7", "supervise! crashed with another message")
            self.stats["R7 gave up"] += 1
        t.dead = True
        self.leaked += leaked
        self.stats["crashes"] += 1
        self.release_state(t)
        t.phase, t.outcome = "crashing", ("crashed", msg)
        for c in t.children:
            self.cancel_task(c)
        self.check_finished(t)

    def check_finished(self, t):
        if t.phase not in ("finishing", "cancelling", "crashing"):
            return
        if any(c.phase != "finished" for c in t.children):
            return
        t.phase = "finished"
        kind = t.outcome[0]
        if kind in ("done", "crashed"):
            t.final = self.heap.data(t.outcome[1])
        t.status = {"done": DONE, "cancelled": CANCELLED, "crashed": CRASHED}[kind]
        p = t.parent
        if t not in self.active:
            if p is None:
                if t.wake == "parked":
                    self.wake(t)  # readied once more to report its terminal status
            else:
                t.wake = None
        if p is None:
            return
        rep = t.outcome
        if p.phase in ("cancelling", "crashing"):
            if kind in ("done", "crashed"):
                self.heap.drop(rep[1])
            self.check_finished(p)
        else:
            require(p.phase == "live", "R3", f"report to task {p.id} in phase {p.phase}")
            p.reports.append((t, rep))
            self.wake(p)

    # ------------------------------------------------------------ checks
    def check_quiescent(self):
        """After the roots are freed: every task finished, readiness exact (R1)."""
        for t in self.tasks.values():
            require(t.phase == "finished", "R6", f"task {t.id} never finished ({t.phase})")
            require(t.wake is None, "R1", f"task {t.id} left {t.wake}")
            # One ready per park at most (a park ended by cancellation gets none),
            # plus the initial one.
            require(t.readies <= t.parks + 1, "R1", f"task {t.id}: {t.readies} readies for {t.parks} parks")
