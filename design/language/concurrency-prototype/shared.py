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
                              Set(new, r) is Set(r, new) here. The step moves
                              each cell's state out into f's update form (see
                              `update_forms`), which puts it back on Keep and
                              Wait. A step that crashes leaves its cells
                              empty and poisons them. A step that waits parks
                              the task until a commit to one of its cells,
                              then runs again from the start: a suspension
                              site
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
after its descendants have finished, innermost first. A step or deferred step
that crashes commits nothing and poisons its cells: every later step, read or
waiter on them crashes. The states it was given are in its crashed frames,
which leak them (R5).

A probe (see "Waking") runs the function as written on states that the cells
keep, and discards the result. It is the reference for the RFC's probe form,
which decides the same tag without building Set's state or answer; that form
is not modeled.

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

import itertools
from collections import Counter

from heap import Ref
from ir import Block, Proc, expr_vars

POISONED_BY_UPDATE = "this shared state was poisoned when an update crashed"
POISONED_BY_CLEANUP = "this shared state was poisoned when a cleanup crashed"
WAIT_DEADLOCK = "deadlock: no task can ever change the shared state this task waits for"

STEP_OPS = ("step", "defer", "errdefer")
EMPTY = None  # a cell's state while a step holds it, and after it crashed


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
        self.poisoned = None  # the poison message, once an update or cleanup crashed
        self.waiters = []  # tasks parked at a step whose footprint has this cell
        self.version = 0  # commits so far (a None build waits for it to change)
        self.port = None


class Step:
    """A step's operands, owned by whoever holds this: the op while it runs,
    a waiting task's wait state, or a defer stack entry."""

    __slots__ = ("f", "cells", "args", "dirty", "on_error", "seen", "baton", "since", "probed")

    def __init__(self, f, cells, args, on_error=False):
        self.f, self.cells, self.args = f, cells, args
        self.on_error = on_error  # an errdefer: runs only on cancellation or a crash
        self.dirty = False  # a commit to one of its cells since it began waiting
        self.seen = None  # skip_unchanged: the values it waited on
        self.baton = False  # readied by a probe; passes the wake on unless it stores
        self.since = None  # when it began waiting: its place among a cell's waiters
        self.probed = None  # the cells' versions when a probe readied it (V10)


class Shared:
    def __init__(self, prog, heap, stats, sessions, wake):
        self.prog, self.heap, self.stats, self.sessions = prog, heap, stats, sessions
        self.wake = wake  # wake(task): a commit readies a waiter
        self.leak = lambda values: None  # the runtime's: values a crashed probe leaked
        self.cells = {}
        self.committed = False  # whether the last run stored
        # Mutation switches for the regression tests.
        # A step or deferred step that crashes poisons nothing; the runtime
        # keeps a reference to each state it moves out and puts it back: the
        # earlier rule, under which an update borrowed the state.
        self.no_poison = False
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
        # "Waking"). "first", the rule: the runtime probes the waiters, oldest
        # first, running each one's pure function without storing, and
        # readies only the first that would not wait; a readied waiter that
        # then ends without storing (it keeps, waits again, crashes or is
        # cancelled first) probes on from there. A readied waiter that waits
        # again keeps its place. "latest", the earlier rule, probes the most
        # recent waiter first, and a waiter that waits again moves to the most
        # recent place: the order of a None build on one plain stack. "all":
        # every waiter is readied and runs its function again when
        # dispatched, which costs n**2 dispatches under contention.
        self.wake_rule = "first"
        self.wait_seq = 0

    def cell(self, r):
        return self.cells[self.heap.obj(r).tag]

    def state(self, r):
        """The state of cell r. A released cell is one that no live task held,
        so no task can reach it (V9). A cell is empty only while a step runs
        and after a step that crashed poisoned it."""
        c = self.cell(r)
        assert not c.released, "V9: a task reached a cell that was released"
        v = self.heap.obj(r).fields[0]
        assert v is not EMPTY, "a task reached an empty cell that was not poisoned"
        return v

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
            return ("crash", heap.alloc("str", c.poisoned, []), [])
        v = self.state(r)
        heap.dup(v)
        heap.drop(r)
        return ("ready", v)

    def run(self, st, cleanup=False):
        """Run step st once. It keeps owning its operands.
        ("ready", r), ("wait",) or ("crash", msg, leaked)."""
        heap = self.heap
        cs = [self.cell(r) for r in st.cells]
        poisoned = [c.poisoned for c in cs if c.poisoned]
        if poisoned:
            self.stats["poisoned steps"] += 1
            return ("crash", heap.alloc("str", poisoned[0], []), [])
        # Move each state out: the cell holds nothing while f runs, so f
        # holds the state's reference.
        moved = []
        for r in st.cells:
            self.state(r)
            o = heap.obj(r)
            moved.append(o.fields[0])
            o.fields[0] = EMPTY
        if self.no_poison:
            for v in moved:
                heap.dup(v)
        for a in st.args:
            heap.dup(a)
        self.stats["steps run"] += 1
        probed, st.probed = st.probed, None
        try:
            out = run_pure(self.prog, heap, update_form(st.f, len(st.cells)), moved + list(st.args))
        except PureCrash as pc:
            self.stats["steps crashed"] += 1
            if not cleanup:
                self.stats["updates crashed"] += 1
            if self.no_poison:
                self.put_back(st, moved)
            else:
                self.poison(st, POISONED_BY_CLEANUP if cleanup else POISONED_BY_UPDATE)
            return ("crash", pc.msg, pc.leaked)
        o = heap.obj(out)
        fields = list(o.fields)
        for f in fields:
            heap.dup(f)
        tag = o.tag
        heap.drop(out)
        self.committed = False
        n = len(st.cells)
        if tag in ("Keep", "Wait"):
            back = fields[-n:] if n else []
            assert all(_same(b, m) for b, m in zip(back, moved)), \
                f"V10: {st.f}'s update form put back a state it was not given"
            if self.no_poison:
                for v in moved:
                    heap.drop(v)
            self.put_back(st, back)
        if tag == "Wait":
            assert len(fields) == n, f"a step returned {tag} with {len(fields)} fields"
            self.stats["waits"] += 1
            if probed is not None:
                assert probed != tuple(c.version for c in cs), \
                    f"V10: {st.f} waits on the state its probe found would not wait"
            return ("wait",)
        if probed is not None and probed == tuple(c.version for c in cs):
            self.stats["probes confirmed by the update"] += 1
        if tag == "Keep":
            assert len(fields) == n + 1, f"a step returned {tag} with {len(fields)} fields"
            return ("ready", fields[0])
        self.committed = True
        assert tag == "Set" and len(fields) == n + 1, f"a step returned {tag}"
        if self.no_poison:
            for v in moved:
                heap.drop(v)
        r, new = fields[0], fields[1:]
        self.stats["commits"] += 1
        for ref, c, v in zip(st.cells, cs, new):
            o = heap.obj(ref)
            assert o.fields[0] is EMPTY, "a step stored to a cell it had not emptied"
            o.fields[0] = v
            if not self.no_store_holders:
                self.sessions.transfer(v, c.port.holders)
            c.version += 1
        for c in cs:  # every cell holds its new state before any waiter is probed
            self.ready_waiters(c)
        return ("ready", r)

    def put_back(self, st, states):
        for ref, v in zip(st.cells, states):
            o = self.heap.obj(ref)
            assert o.fields[0] is EMPTY, "a state was put back into a cell that was not empty"
            o.fields[0] = v

    def values(self, st):
        return tuple(self.heap.data(self.state(r)) for r in st.cells)

    def unchanged(self, st, at):
        mode = "both" if self.skip_unchanged is True else self.skip_unchanged
        return mode in (at, "both") and st.seen == self.values(st)

    def probe(self, st):
        """Run st's function on the current state without
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
        """Ready the first waiter on each cell (a Cell), in the wake rule's
        order, that would not wait."""
        if self.no_wake:
            return
        for c in cells:
            order = list(c.waiters) if self.wake_rule == "first" else list(reversed(c.waiters))
            for w in order:
                st = w.wait_on
                if self.probe(st):
                    st.dirty, st.baton = True, True
                    st.probed = tuple(self.cell(r).version for r in st.cells)
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
        st.dirty, st.probed = False, None
        if self.skip_unchanged:
            st.seen = self.values(st)
        if st.since is None or self.wake_rule == "latest":
            self.wait_seq += 1
            st.since = self.wait_seq
        for r in st.cells:
            c = self.cell(r)
            if t not in c.waiters:
                # In order of when each began waiting, so a waiter that waits
                # again after a probe readied it keeps its place.
                i = len(c.waiters)
                while i > 0 and c.waiters[i - 1].wait_on.since > st.since:
                    i -= 1
                c.waiters.insert(i, t)

    def unpark(self, t, st):
        for r in st.cells:
            c = self.cell(r)
            if t in c.waiters:
                c.waiters.remove(t)

    def release(self, st):
        for v in list(st.cells) + list(st.args):
            self.heap.drop(v)

    def poison(self, st, msg):
        """A step or deferred step crashed with its cells empty: they are
        poisoned, and their waiters are readied so that they crash too."""
        for r in st.cells:
            c = self.cell(r)
            if not c.poisoned:
                c.poisoned = msg
                self.stats["cells poisoned"] += 1
                if msg == POISONED_BY_UPDATE:
                    self.stats["cells poisoned by an update"] += 1
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
        r = self.run(st, cleanup=True)
        assert r[0] != "wait", "a deferred step waited"
        self.release(st)
        if r[0] == "ready":
            self.heap.drop(r[1])
            return None
        self.stats["deferred steps crashed"] += 1
        return (r[1], r[2])


# ---------------------------------------------------------------------------
# The update form (concurrency-shared-variables.md, "Lowering")
# ---------------------------------------------------------------------------


def _same(a, b):
    """The same value: the same heap object, or the same unboxed value."""
    if isinstance(a, Ref) or isinstance(b, Ref):
        return isinstance(a, Ref) and isinstance(b, Ref) and a.id == b.id
    return a == b


def update_form(f, n):
    """The name of f's update form for a step over n cells."""
    return f"{f}@update{n}"


def update_forms(procs):
    """Add the update form of every procedure a step or deferred step calls,
    before ARC runs. The update form is the procedure with each tail position
    of its body rewritten, followed through `if` and `match`: a tail that
    returns a `Keep(r)` or `Wait` it built in the same block returns
    `Keep(r, s1..sn)` or `Wait(s1..sn)`, the states it was given; a tail that
    returns a `Set` it built is unchanged; and any other tail, such as a call
    that returns the tag, becomes a `match` on the tag that does both. ARC
    then places the references, so a path that ends in `Set` holds the only
    reference to a state that nothing else holds, and a path that ends in
    `Keep` or `Wait` keeps it. Steps call the update form; probes call the
    procedure as written."""
    wanted = set()
    for p in procs.values():
        _step_sites(p.body, wanted)
    out = dict(procs)
    for f, n in sorted(wanted):
        p = procs[f]
        assert not p.eff and len(p.params) >= n, f"a step over {n} cells calls {f}"
        states = list(p.params[:n])
        assert not (_bound(p.body) & set(states)), f"{f} rebinds a state parameter"
        fresh = (f"upd%{k}" for k in itertools.count())
        out[update_form(f, n)] = Proc(update_form(f, n), list(p.params), _rewrite(p.body, states, fresh), False)
    return out


def _step_sites(b, acc):
    for st in b.stmts:
        e = st[2]
        if e[0] in STEP_OPS:
            acc.add((e[1], len(e[2])))
    t = b.term
    if t[0] == "if":
        _step_sites(t[2], acc)
        _step_sites(t[3], acc)
    elif t[0] == "match":
        for _, _, blk in t[2]:
            _step_sites(blk, acc)


def _bound(b):
    out = {st[1] for st in b.stmts}
    t = b.term
    if t[0] == "if":
        out |= _bound(t[2]) | _bound(t[3])
    elif t[0] == "match":
        for _, vs, blk in t[2]:
            out |= set(vs) | _bound(blk)
    return out


def _rewrite(b, states, fresh):
    t = b.term
    k = t[0]
    stmts = list(b.stmts)
    if k == "if":
        return Block(stmts, ("if", t[1], _rewrite(t[2], states, fresh), _rewrite(t[3], states, fresh)))
    if k == "match":
        return Block(stmts, ("match", t[1], [(tag, vs, _rewrite(blk, states, fresh)) for tag, vs, blk in t[2]]))
    if k == "crash":
        return Block(stmts, t)
    if k == "ret":
        x = t[1]
        at = [i for i, st in enumerate(stmts) if st[1] == x]
        if at:
            i = at[-1]
            e = stmts[i][2]
            used_again = any(x in expr_vars(st[2]) for st in stmts[i + 1:])
            if e[0] == "tag" and not used_again:
                if e[1] == "Set":
                    return Block(stmts, t)
                if e[1] in ("Keep", "Wait"):
                    stmts[i] = ("let", x, ("tag", e[1], list(e[2]) + states))
                    return Block(stmts, t)
        return Block(stmts, _tag_match(x, states, fresh))
    if k in ("tail", "tailc"):
        x = next(fresh)
        call = ("call", t[1], list(t[2])) if k == "tail" else ("callc", t[1], list(t[2]), False)
        return Block(stmts + [("let", x, call)], _tag_match(x, states, fresh))
    raise ValueError(t)


def _tag_match(x, states, fresh):
    """`match x` that returns Set as it is and puts the states into Keep and Wait."""
    n = len(states)
    vs = [next(fresh) for _ in range(n + 1)]
    r = next(fresh)
    y1, y2, y3 = next(fresh), next(fresh), next(fresh)
    return ("match", x, [
        ("Set", vs, Block([("let", y1, ("tag", "Set", vs))], ("ret", y1))),
        ("Keep", [r], Block([("let", y2, ("tag", "Keep", [r] + states))], ("ret", y2))),
        ("Wait", [], Block([("let", y3, ("tag", "Wait", list(states)))], ("ret", y3))),
    ])


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
