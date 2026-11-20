"""Shared state (concurrency-shared-variables.md).

A `Shared(s)` handle points to a cell: a heap object of kind "shared" whose
one field is the state, so the heap's exact accounting covers what cells
hold. The IR operations, which a compiler's post-check IR would make explicit
from calls to the builtins:

    shared(v)                 `Shared.new!(v)`: a new cell holding v
    read(c)                   a plain read, outside any step: the value now
    step(f, [cells], [args])  an atomic step. f is a pure procedure called
                              with the cells' values, then args. It returns
                                  Set(r, v1, ..., vn)   commit vi to cell i
                                  Keep(r)               commit nothing
                                  Wait                  wait
                              and the step's value is r: `update!`, whose
                              Set(new, r) is Set(r, new) here. A step that crashes
                              commits nothing. A step that waits parks the
                              task until a commit to one of its cells, then
                              runs again from the start: a suspension site
    defer(f, [cells], [args]) push a deferred step onto the task's defer
                              stack: the entry of `finally!`
    errdefer(f, [cells], [args])
                              the same, for a step that runs only when the
                              task is cancelled or crashes: `on_failure!`
    undefer()                 pop the innermost deferred step and run it (an
                              errdefer is dropped unrun): the return from
                              `finally!` or `on_failure!`

The cells and arguments are consumed. While a step waits, the task's wait
state owns them, so cancelling it drops them. A deferred step's cells and
arguments are owned by its defer stack entry until it runs.

Deferred steps that are still pending when a task is cancelled or crashes run
after its descendants have finished, innermost first. A deferred step that
crashes commits nothing and poisons its cells: every later step, read or
waiter on them crashes.

The RFC's `update!` takes one handle. A step here may take several cells,
which the generator uses to exercise the lock order that a future
multi-handle update would need. It passes one value per position and commits
positions in order, so it never passes one cell twice to a step.

Steps run atomically here because Python runs one thing at a time. The lock
protocol that makes them atomic under ParallelTasks (footprints locked in one
global order, waiters registered before the locks are released) is checked
separately, over every interleaving, in shared_model.py.
"""

from __future__ import annotations

from collections import Counter

from ir import expr_vars

POISONED = "this shared state was poisoned when a cleanup crashed"
WAIT_DEADLOCK = "deadlock: no task can ever change the shared state this task waits for"

STEP_OPS = ("step", "defer", "errdefer")


class PureCrash(Exception):
    def __init__(self, msg, leaked):
        super().__init__("crash")
        self.msg, self.leaked = msg, leaked


class Cell:
    __slots__ = ("cid", "poisoned", "waiters", "version", "port", "rid", "released")

    def __init__(self, cid):
        self.cid = cid
        self.rid = None  # the heap object holding the state
        self.released = False  # no live task held it, so its state was dropped (V9)
        self.poisoned = False
        self.waiters = []  # tasks parked at a step whose footprint has this cell
        self.version = 0  # commits so far (a None build waits for it to change)
        self.port = None


class Step:
    """A step's operands, owned by whoever holds this: the op while it runs,
    a waiting task's wait state, or a defer stack entry."""

    __slots__ = ("f", "cells", "args", "dirty", "on_error", "seen", "baton")

    def __init__(self, f, cells, args, on_error=False):
        self.f, self.cells, self.args = f, cells, args
        self.on_error = on_error  # an errdefer: runs only on cancellation or a crash
        self.dirty = False  # a commit to one of its cells since it began waiting
        self.seen = None  # skip_unchanged: the values it waited on
        self.baton = False  # wake_rule "first": readied by a probe; passes the wake on unless it stores


class Shared:
    def __init__(self, prog, heap, stats, sessions, wake):
        self.prog, self.heap, self.stats, self.sessions = prog, heap, stats, sessions
        self.wake = wake  # wake(task): a commit readies a waiter
        self.leak = lambda values: None  # the runtime's: values a crashed probe leaked
        self.cells = {}
        self.committed = False  # whether the last run stored
        # Mutation switches for the regression tests.
        self.no_poison = False  # a deferred step that crashes poisons nothing
        self.no_wake = False  # a commit readies no waiter
        self.no_pass_on = False  # a waiter readied by a probe that ends without storing readies no other
        self.no_store_holders = False  # a stored state's cells do not gain the cell's holders
        # An alternative to the wake rule below, for bench/shared_vs_actor.py.
        # A step is a pure function of its cells' values and its arguments,
        # so a waiter whose cells hold the values it waited on would wait
        # again. "resume": a dispatched waiter whose values are back to those
        # it waited on parks again without running its function. "store": a
        # commit readies only waiters whose values changed. "both" does both.
        # None of these saves a dispatch under contention.
        self.skip_unchanged = False
        # Which waiters a store readies (concurrency-shared-variables.md,
        # "Runtime"). "latest", the rule: the runtime probes the waiters,
        # most recently waiting first, running each one's pure function
        # without storing, and readies only the first that would not wait; a
        # readied waiter that then ends without storing (it keeps, waits
        # again, crashes or is cancelled first) probes on from there. That
        # order is the one stack of a None build, where the latest waiter is
        # on top (C9, V8). "first" probes oldest first instead, which a None
        # build cannot match. "all": every waiter is readied and runs its
        # function again when dispatched, which costs n**2 dispatches under
        # contention.
        self.wake_rule = "latest"

    def cell(self, r):
        return self.cells[self.heap.obj(r).tag]

    def state(self, r):
        """The state of cell r. A released cell is one that no live task held,
        so no task can reach it (V9)."""
        assert not self.cell(r).released, "V9: a task reached a cell that was released"
        return self.heap.obj(r).fields[0]

    # ------------------------------------------------------------ operations
    def new(self, v, holder=None):
        c = Cell(len(self.cells) + 1)
        self.cells[c.cid] = c
        self.stats["shared variables"] += 1
        r = self.heap.alloc("shared", c.cid, [v])
        c.rid = r.id
        c.port = self.sessions.register("shared", r, c, holder)
        return r

    def read(self, r):
        """A plain read. Consumes the cell reference. ("ready", v) or ("crash", msg)."""
        heap = self.heap
        c = self.cell(r)
        self.stats["shared reads"] += 1
        if c.poisoned:
            heap.drop(r)
            return ("crash", heap.alloc("str", POISONED, []), [])
        v = self.state(r)
        heap.dup(v)
        heap.drop(r)
        return ("ready", v)

    def run(self, st):
        """Run step st once. It keeps owning its operands.
        ("ready", r), ("wait",) or ("crash", msg, leaked)."""
        heap = self.heap
        cs = [self.cell(r) for r in st.cells]
        if any(c.poisoned for c in cs):
            self.stats["poisoned steps"] += 1
            return ("crash", heap.alloc("str", POISONED, []), [])
        vals = []
        for r in st.cells:
            v = self.state(r)
            heap.dup(v)
            vals.append(v)
        for a in st.args:
            heap.dup(a)
        self.stats["steps run"] += 1
        try:
            out = run_pure(self.prog, heap, st.f, vals + list(st.args))
        except PureCrash as pc:
            self.stats["steps crashed"] += 1
            return ("crash", pc.msg, pc.leaked)
        o = heap.obj(out)
        fields = list(o.fields)
        for f in fields:
            heap.dup(f)
        tag = o.tag
        heap.drop(out)
        self.committed = False
        if tag == "Wait":
            self.stats["waits"] += 1
            return ("wait",)
        if tag == "Keep":
            return ("ready", fields[0])
        self.committed = True
        assert tag == "Set" and len(fields) == len(st.cells) + 1, f"a step returned {tag}"
        r, new = fields[0], fields[1:]
        self.stats["commits"] += 1
        for ref, c, v in zip(st.cells, cs, new):
            self.state(ref)
            o = heap.obj(ref)
            old, o.fields[0] = o.fields[0], v
            if not self.no_store_holders:
                self.sessions.transfer(v, c.port.holders)
            heap.drop(old)
            c.version += 1
            self.ready_waiters(c)
        return ("ready", r)

    def values(self, st):
        return tuple(self.heap.data(self.state(r)) for r in st.cells)

    def unchanged(self, st, at):
        mode = "both" if self.skip_unchanged is True else self.skip_unchanged
        return mode in (at, "both") and st.seen == self.values(st)

    def probe(self, st):
        """wake_rule "first": run st's function on the current state without
        storing anything. True if it would not wait (a crash counts: the
        waiter is readied and crashes when it runs)."""
        heap = self.heap
        if any(self.cell(r).poisoned for r in st.cells):
            return True
        vals = []
        for r in st.cells:
            v = self.state(r)
            heap.dup(v)
            vals.append(v)
        for a in st.args:
            heap.dup(a)
        self.stats["probes"] += 1
        try:
            out = run_pure(self.prog, heap, st.f, vals + list(st.args))
        except PureCrash as pc:
            heap.drop(pc.msg)
            self.leak(pc.leaked)
            return True
        waits = heap.obj(out).tag == "Wait"
        heap.drop(out)
        if waits:
            self.stats["probes that would wait"] += 1
        return not waits

    def pass_on(self, cells):
        """wake_rule "first": ready the first waiter on each cell (a Cell) that
        would not wait."""
        if self.no_wake:
            return
        for c in cells:
            order = list(c.waiters) if self.wake_rule == "first" else list(reversed(c.waiters))
            for w in order:
                st = w.wait_on
                if self.probe(st):
                    st.dirty, st.baton = True, True
                    self.unpark(w, st)
                    self.wake(w)
                    break

    def ready_waiters(self, c):
        if self.no_wake:
            return
        if self.wake_rule in ("first", "latest"):
            self.pass_on([c])
            return
        ws, c.waiters = c.waiters, []
        for w in ws:
            st = w.wait_on
            if self.unchanged(st, "store"):
                self.stats["wakes skipped"] += 1
                c.waiters.append(w)
                continue
            st.dirty = True
            for r in st.cells:  # it waits on its other cells no longer
                o = self.cell(r)
                if w in o.waiters:
                    o.waiters.remove(w)
            self.wake(w)

    def park(self, t, st):
        st.dirty = False
        if self.skip_unchanged:
            st.seen = self.values(st)
        for r in st.cells:
            c = self.cell(r)
            if t not in c.waiters:
                c.waiters.append(t)

    def unpark(self, t, st):
        for r in st.cells:
            c = self.cell(r)
            if t in c.waiters:
                c.waiters.remove(t)

    def release(self, st):
        for v in list(st.cells) + list(st.args):
            self.heap.drop(v)

    def poison(self, st):
        """A deferred step crashed: its cells are poisoned, and their waiters
        are readied so that they crash too."""
        if self.no_poison:
            return
        for r in st.cells:
            c = self.cell(r)
            if not c.poisoned:
                c.poisoned = True
                self.stats["cells poisoned"] += 1
            ws, c.waiters = c.waiters, []
            for w in ws:
                w.wait_on.dirty = True
                self.unpark(w, w.wait_on)
                self.wake(w)

    def exit(self, st):
        """The normal exit edge of a deferred step's block: run it, or drop an
        errdefer unrun."""
        if st.on_error:
            self.stats["errdefers dropped"] += 1
            self.release(st)
            return None
        return self.run_deferred(st)

    def run_deferred(self, st):
        """Run a deferred step and release its operands. None, or (msg, leaked)
        if it crashed (its cells are then poisoned)."""
        self.stats["deferred steps run"] += 1
        r = self.run(st)
        assert r[0] != "wait", "a deferred step waited"
        if r[0] == "ready":
            self.heap.drop(r[1])
            self.release(st)
            return None
        self.stats["deferred steps crashed"] += 1
        self.poison(st)
        self.release(st)
        return (r[1], r[2])


# ---------------------------------------------------------------------------
# The pure procedures a step calls
# ---------------------------------------------------------------------------


def run_pure(prog, heap, pname, args):
    """Run a pure procedure to completion, tracking what each frame owns so a
    crash reports exactly what it leaks."""
    frames = []
    return _pure(prog, heap, pname, args, frames)


def _owned(frames):
    out = []
    for env, counts in frames:
        for var, n in counts.items():
            out += [env[var]] * n
    return out


def _pure(prog, heap, pname, args, frames):
    from engines import eval_simple, open_closure, take_match

    proc = prog.procs[pname]
    assert not proc.eff, f"a step called effectful {pname}"
    env = dict(zip(proc.params, args))
    counts = Counter({p: 1 for p in proc.params})
    frame = [env, counts]
    frames.append(frame)
    block = proc.body
    try:
        while True:
            env, counts = frame
            for st in block.stmts:
                if st[0] == "dup":
                    heap.dup(env[st[1]])
                    counts[st[1]] += 1
                    continue
                if st[0] == "drop":
                    heap.drop(env[st[1]])
                    counts[st[1]] -= 1
                    continue
                _, x, e, sid = st
                for a in expr_vars(e):
                    counts[a] -= 1
                k = e[0]
                if k == "call":
                    v = _pure(prog, heap, e[1], [env[a] for a in e[2]], frames)
                elif k == "callc":
                    assert not e[3], "a step called an effectful closure"
                    f, caps = open_closure(heap, env[e[1]])
                    v = _pure(prog, heap, f, caps + [env[a] for a in e[2]], frames)
                else:
                    v = eval_simple(heap, env, e)
                env[x] = v
                counts[x] = 1
            tm = block.term
            k = tm[0]
            if k == "ret":
                counts[tm[1]] -= 1
                return env[tm[1]]
            if k in ("tail", "tailc"):
                if k == "tail":
                    f, vals = tm[1], [env[a] for a in tm[2]]
                else:
                    f, caps = open_closure(heap, env[tm[1]])
                    vals = caps + [env[a] for a in tm[2]]
                callee = prog.procs[f]
                assert not callee.eff, f"a step called effectful {f}"
                frame[0] = dict(zip(callee.params, vals))
                frame[1] = Counter({p: 1 for p in callee.params})
                block = callee.body
            elif k == "if":
                counts[tm[1]] -= 1
                block = tm[2] if env[tm[1]] else tm[3]
            elif k == "match":
                counts[tm[1]] -= 1
                block, frame[0] = take_match(heap, env, tm)
                for _, vs, blk in tm[2]:
                    if blk is block:
                        for v in vs:
                            frame[1][v] = 1
            elif k == "crash":
                counts[tm[1]] -= 1
                raise PureCrash(env[tm[1]], _owned(frames))
    finally:
        frames.remove(frame)


# ---------------------------------------------------------------------------
# Static checks
# ---------------------------------------------------------------------------


class SharedError(Exception):
    pass


def certify_defers(prog):
    """V3 and V5, statically: every step and deferred step calls a pure
    procedure, shared-variable operations appear only in effectful procedures,
    and on every path through a procedure each `defer` is matched by an
    `undefer` before the procedure returns or replaces its frame. A `crash`
    may leave deferred steps pending; the runtime runs them."""
    for p in prog.procs.values():
        _cert(prog, p, p.body, 0)


def _cert(prog, p, b, depth):
    for st in b.stmts:
        if st[0] != "let":
            continue
        e = st[2]
        k = e[0]
        if k in ("shared", "read", "step", "defer", "errdefer", "undefer") and not p.eff:
            raise SharedError(f"pure {p.name} uses a shared variable ({k})")
        if k in STEP_OPS:
            f = prog.procs.get(e[1])
            if f is None or f.eff:
                raise SharedError(f"{p.name}: {k} calls {e[1]}, which is not a pure procedure")
        if k in ("defer", "errdefer"):
            depth += 1
        elif k == "undefer":
            if depth == 0:
                raise SharedError(f"{p.name}: undefer with no deferred step pending")
            depth -= 1
    t = b.term
    if t[0] in ("ret", "tail", "tailc") and depth:
        raise SharedError(f"{p.name}: {t[0]} with {depth} deferred steps pending")
    if t[0] == "if":
        _cert(prog, p, t[2], depth)
        _cert(prog, p, t[3], depth)
    elif t[0] == "match":
        for _, _, blk in t[2]:
            _cert(prog, p, blk, depth)
