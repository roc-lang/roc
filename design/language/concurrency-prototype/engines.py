"""Three ways to run the same post-ARC program.

NoneEngine    a `None` build: no tasks; combinators run children one at a time.
StackEngine   stackful reference: every task is a chain of Python generators,
              which keep every frame while paused. It never consults the
              lowering, so it is the oracle the unwinding engine is checked
              against.
UnwindEngine  the RFC's lowering: tasks run on the native (here: Python) stack;
              a paused task is a chain of records, one per suspendable
              procedure, saved at suspension sites by unwinding, and resumed
              innermost first through per-site resume entries.

Every engine consumes the explicit dup/drop statements and the ledger ARC
published; none of them decides ownership itself.
"""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass

from heap import Handle, Ref
from ir import expr_vars


class RocCrash(Exception):
    def __init__(self, msg, leaked):
        super().__init__("crash")
        self.msg, self.leaked = msg, leaked


class ProgramCrash(Exception):
    """A crash in a None build: it stops the whole program."""

    def __init__(self, msg, supervised):
        super().__init__(msg)
        self.msg, self.supervised = msg, supervised


def eval_simple(heap, env, e):
    """Expressions with no control effect. Arguments are consumed."""
    k = e[0]
    if k == "int":
        return e[1]
    if k == "str":
        return heap.alloc("str", e[1], [])
    if k == "prim":
        a, b = env[e[2][0]], env[e[2][1]]
        op = e[1]
        if op == "add":
            return a + b
        if op == "sub":
            return a - b
        if op == "lt":
            return int(a < b)
        if op == "eq":
            return int(a == b)
        raise ValueError(op)
    if k == "tup":
        return heap.alloc("tup", None, [env[a] for a in e[1]])
    if k == "tag":
        return heap.alloc("tag", e[1], [env[a] for a in e[2]])
    if k == "list":
        return heap.alloc("list", None, [env[a] for a in e[1]])
    if k == "field":
        o = env[e[1]]
        v = heap.obj(o).fields[e[2]]
        heap.dup(v)
        heap.drop(o)
        return v
    if k == "len":
        o = env[e[1]]
        n = len(heap.obj(o).fields)
        heap.drop(o)
        return n
    if k == "get":
        o, i = env[e[1]], env[e[2]]
        v = heap.obj(o).fields[i]
        heap.dup(v)
        heap.drop(o)
        return v
    if k in ("set", "push"):
        o = env[e[1]]
        ob = heap.obj(o)
        if k == "set":
            i, v = env[e[2]], env[e[3]]
        else:
            i, v = len(ob.fields), env[e[2]]
        if heap.unique(o):
            heap.stats["inplace"] += 1
            if k == "set":
                old = ob.fields[i]
                ob.fields[i] = v
                heap.drop(old)
            else:
                ob.fields.append(v)
            return o
        items = list(ob.fields)
        for j, f in enumerate(items):
            if k == "push" or j != i:
                heap.dup(f)
        if k == "set":
            items[i] = v
        else:
            items.append(v)
        heap.drop(o)
        return heap.alloc("list", None, items)
    if k == "clo":
        return heap.alloc("clo", e[1], [env[a] for a in e[2]])
    raise ValueError(e)


def open_closure(heap, c):
    """Call through a closure value: its captures become the leading arguments."""
    o = heap.obj(c)
    caps = list(o.fields)
    for v in caps:
        heap.dup(v)
    heap.drop(c)
    return o.tag, caps


# ---------------------------------------------------------------------------
# None builds
# ---------------------------------------------------------------------------


class NoneEngine:
    def __init__(self, prog, heap, host):
        self.prog, self.heap, self.host = prog, heap, host
        self.children = {}  # handle id -> dict
        self.next_child = 0
        self.path = [()]
        self.spawned = Counter()  # path -> children spawned so far
        self.supervised = 0
        self.steps = 0

    def run(self):
        return self.call(self.prog.entry, [])

    def call(self, pname, args):
        heap, prog = self.heap, self.prog
        proc = prog.procs[pname]
        env = dict(zip(proc.params, args))
        block = proc.body
        while True:
            for st in block.stmts:
                self.steps += 1
                if st[0] == "dup":
                    heap.dup(env[st[1]])
                    continue
                if st[0] == "drop":
                    heap.drop(env[st[1]])
                    continue
                _, x, e, sid = st
                k = e[0]
                if k == "call":
                    v = self.call(e[1], [env[a] for a in e[2]])
                elif k == "callc":
                    f, caps = open_closure(heap, env[e[1]])
                    v = self.call(f, caps + [env[a] for a in e[2]])
                elif k == "host":
                    v = self.host.call_sync(self.path[-1], e[1], [env[a] for a in e[2]])
                elif k == "spawn":
                    me = self.path[-1]
                    idx = self.spawned[me]
                    self.spawned[me] += 1
                    self.next_child += 1
                    self.children[self.next_child] = {"clo": env[e[1]], "state": "unstarted",
                                                      "path": me + (idx,)}
                    v = Handle(self.next_child)
                elif k == "await":
                    v = self.await_any(env[e[1]], pname)
                elif k == "cancel":
                    c = self.children[env[e[1]].task]
                    if c["state"] == "unstarted":
                        heap.drop(c["clo"])
                        c["state"] = "cancelled"
                    v = 0
                else:
                    v = eval_simple(heap, env, e)
                env[x] = v
            t = block.term
            k = t[0]
            if k == "ret":
                return env[t[1]]
            if k == "tail":
                callee = prog.procs[t[1]]
                env = dict(zip(callee.params, [env[a] for a in t[2]]))
                block, pname = callee.body, t[1]
            elif k == "tailc":
                f, caps = open_closure(heap, env[t[1]])
                callee = prog.procs[f]
                env = dict(zip(callee.params, caps + [env[a] for a in t[2]]))
                block, pname = callee.body, f
            elif k == "if":
                block = t[2] if env[t[1]] else t[3]
            elif k == "match":
                block, env = take_match(heap, env, t)
            elif k == "crash":
                raise ProgramCrash(heap.data(env[t[1]]), self.supervised > 0)

    def await_any(self, kids, pname):
        """Run the earliest unreported child to completion (or report it cancelled)."""
        heap = self.heap
        hs = [h.task for h in heap.obj(kids).fields]
        heap.drop(kids)
        for i, cid in enumerate(hs):
            c = self.children[cid]
            if c["state"] == "reported":
                continue
            if c["state"] == "cancelled":
                c["state"] = "reported"
                return heap.alloc("tag", "Cancelled", [i])
            c["state"] = "reported"
            self.path.append(c["path"])
            sup = pname == "sup_loop"
            self.supervised += sup
            f, caps = open_closure(heap, c["clo"])
            v = self.call(f, caps)
            self.supervised -= sup
            self.path.pop()
            return heap.alloc("tag", "Done", [i, v])
        raise AssertionError("await with no unreported child")


def take_match(heap, env, t):
    o = env[t[1]]
    ob = heap.obj(o)
    for tag, vs, blk in t[2]:
        if tag == ob.tag:
            env = dict(env)
            for v, f in zip(vs, ob.fields):
                heap.dup(f)
                env[v] = f
            heap.drop(o)
            return blk, env
    raise AssertionError(f"no arm for {ob.tag}")


# ---------------------------------------------------------------------------
# Stackful reference engine
# ---------------------------------------------------------------------------


class Frame:
    __slots__ = ("env", "counts")

    def __init__(self, env):
        self.env = env
        self.counts = Counter({k: 1 for k in env})


class StackEngine:
    name = "stack"

    def __init__(self, prog, heap, rt):
        self.prog, self.heap, self.rt = prog, heap, rt

    def start(self, t):
        clo, t.closure = t.closure, None
        t.frames = []
        t.gen = self.run_closure(t, clo)
        return self.drive(t, None)

    def resume(self, t):
        return self.drive(t, self.rt.resume_value(t))

    def drive(self, t, v):
        try:
            t.gen.send(v)
            return ("pending",)
        except StopIteration as s:
            t.gen = None
            return ("done", s.value)
        except RocCrash as c:
            t.gen = None
            return ("crash", c.msg, c.leaked)

    def drop_paused(self, t):
        """Release what every paused frame owns, as a stackful runtime that kept
        exact ownership per frame could."""
        if getattr(t, "gen", None) is not None:
            for fr in t.frames:
                for var, n in fr.counts.items():
                    for _ in range(n):
                        self.heap.drop(fr.env[var])
            t.frames = []
            gen, t.gen = t.gen, None
            gen.close()
        elif t.closure is not None:
            self.heap.drop(t.closure)
            t.closure = None

    def owned(self, t):
        out = []
        for fr in t.frames:
            for var, n in fr.counts.items():
                out += [fr.env[var]] * n
        return out

    def run_closure(self, t, clo):
        f, caps = open_closure(self.heap, clo)
        return (yield from self.run_proc(t, f, caps))

    def run_proc(self, t, pname, args):
        heap, prog, rt = self.heap, self.prog, self.rt
        proc = prog.procs[pname]
        fr = Frame(dict(zip(proc.params, args)))
        t.frames.append(fr)
        block = proc.body
        try:
            while True:
                env, counts = fr.env, fr.counts
                for st in block.stmts:
                    rt.step(t)
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
                        if e[1] == "supervise!":
                            rt.sup_begin(t, env[e[2][0]])
                        v = yield from self.run_proc(t, e[1], [env[a] for a in e[2]])
                        if e[1] == "supervise!":
                            rt.sup_end(t, v)
                    elif k == "callc":
                        f, caps = open_closure(heap, env[e[1]])
                        v = yield from self.run_proc(t, f, caps + [env[a] for a in e[2]])
                    elif k == "host":
                        r = rt.hosted(t, e[1], [env[a] for a in e[2]])
                        v = (yield "park") if r[0] == "pending" else r[1]
                    elif k == "await":
                        r = rt.await_any(t, env[e[1]])
                        v = (yield "park") if r[0] == "pending" else r[1]
                    elif k == "spawn":
                        v = rt.spawn(t, env[e[1]])
                    elif k == "cancel":
                        rt.cancel(t, env[e[1]])
                        v = 0
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
                    fr.env = dict(zip(callee.params, vals))
                    fr.counts = Counter()
                    for p in callee.params:
                        fr.counts[p] += 1
                    block = callee.body
                elif k == "if":
                    counts[tm[1]] -= 1
                    block = tm[2] if env[tm[1]] else tm[3]
                elif k == "match":
                    counts[tm[1]] -= 1
                    block, fr.env = take_match(heap, env, tm)
                    for _, vs, blk in tm[2]:
                        if blk is block:
                            for v in vs:
                                fr.counts[v] = 1
                elif k == "crash":
                    counts[tm[1]] -= 1
                    msg = env[tm[1]]
                    raise RocCrash(msg, self.owned(t))
        finally:
            if t.frames and t.frames[-1] is fr:
                t.frames.pop()


# ---------------------------------------------------------------------------
# The lowering: capture by unwinding
# ---------------------------------------------------------------------------


@dataclass
class Record:
    proc: str
    sid: tuple
    saved: dict  # ledger variable -> value; every saved value is owned
    cell: object = None  # the pending cell this procedure writes a deferred call to
    dcell: object = None  # the pending cell a driving site reads


class PendingCall:
    """The value a procedure returns when it leaves a deferred erased call
    pending; the call itself is in a pending cell."""

    def __repr__(self):
        return "PENDING_CALL"


PENDING_CALL = PendingCall()


class Cell:
    """A pending cell (design.md, "Pending Cells"). C4 puts a suspendable
    procedure's cells in task storage; `owner` is "task" there, or the native
    activation that would hold it in a frame."""

    __slots__ = ("owner", "call", "dead")

    def __init__(self, owner):
        self.owner, self.call, self.dead = owner, None, False

    def put(self, clo, args):
        assert not self.dead, "C4: a deferred call written to a dead native frame"
        assert self.call is None, "a pending cell written twice"
        self.call = (clo, args)

    def take(self):
        c, self.call = self.call, None
        assert c is not None, "a driver found no pending call"
        return c


class UnwindEngine:
    name = "unwind"
    # Mutation switch for the regression tests: give each driving site a cell
    # in its own native frame instead of task storage, as C4 forbids.
    cells_in_frames = False

    def __init__(self, prog, low, heap, rt):
        self.prog, self.low, self.heap, self.rt = prog, low, heap, rt
        self.deferred = low.mode == "boxy"
        self.stats = Counter()

    def start(self, t):
        clo, t.closure = t.closure, None
        t.records = []
        t.native = []
        t.pending = False
        t.cell = Cell("task")
        # The runtime is the waiter of a task's entry closure: it drives.
        return self.run(t, lambda: self.drive(t, self.call_closure(t, clo, [], t.cell), t.cell))

    def run(self, t, thunk):
        t.pending = False
        try:
            v = thunk()
        except RocCrash as c:
            return ("crash", c.msg, c.leaked)
        if t.pending:
            assert t.cell.call is None, "a task parked with a call pending"
            self.stats["max_chain"] = max(self.stats["max_chain"], len(t.records))
            return ("pending",)
        assert v is not PENDING_CALL
        return ("done", v)

    def resume(self, t):
        value = self.rt.resume_value(t)
        while t.records:
            rec = t.records.pop(0)
            rest, t.records = t.records, []
            self.stats["resume_entries"] += 1
            out = self.run_raw(t, lambda: self.entry(t, rec, value))
            if out[0] == "crash":
                t.records = t.records + rest
                return out
            if out[0] == "pending":
                t.records = t.records + rest
                self.stats["max_chain"] = max(self.stats["max_chain"], len(t.records))
                return out
            t.records = rest
            value = out[1]
        # The outermost record may hand a deferred call up to the runtime.
        return self.run(t, lambda: self.drive(t, value, t.cell))

    def run_raw(self, t, thunk):
        """Like `run`, but a result may still be a pending call to hand outward."""
        t.pending = False
        try:
            v = thunk()
        except RocCrash as c:
            return ("crash", c.msg, c.leaked)
        if t.pending:
            return ("pending",)
        return ("done", v)

    def drop_paused(self, t):
        """Run each record's drop entry: release the saved values its owned mask selects."""
        if t.closure is not None:  # start record
            self.heap.drop(t.closure)
            t.closure = None
        for rec in getattr(t, "records", []):
            self.stats["drop_entries"] += 1
            for v in rec.saved.values():
                self.heap.drop(v)
        t.records = []

    def call_closure(self, t, clo, args, cell):
        f, caps = open_closure(self.heap, clo)
        p = self.prog.procs[f]
        return self.exec(t, f, dict(zip(p.params, caps + args)), p.body, 0, cell)

    def drive(self, t, v, cell):
        """Make pending calls until one returns a value or the task pauses.
        Each driven call returns before the next is made: constant stack."""
        while v is PENDING_CALL and not t.pending:
            clo, args = cell.take()
            self.stats["driven_calls"] += 1
            v = self.call_closure(t, clo, args, cell)
        return v

    def entry(self, t, rec, value):
        proc, b, i, x, ledger = self.low.sites[rec.sid]
        env = dict(rec.saved)
        if rec.sid in self.low.drive:
            t.native.append(list(env.values()))
            value = self.drive(t, value, rec.dcell)
            t.native.pop()
            if t.pending:
                self.save(t, proc, rec.sid, env, rec.cell, rec.dcell)
                return None
        e = b.stmts[i][2]
        if e[0] == "call" and e[1] == "supervise!":
            self.rt.sup_end(t, value)
        env[x] = value
        return self.exec(t, proc, env, b, i + 1, rec.cell)

    def save(self, t, pname, sid, env, cell, dcell):
        ledger = self.low.sites[sid][4]
        for c in (cell, dcell):
            assert c is None or c.owner == "task", \
                f"C4: record for {sid} holds the address of a native frame's pending cell"
        t.records.append(Record(pname, sid, {v: env[v] for v in ledger}, cell, dcell))
        self.stats["records"] += 1
        self.stats["saved_values"] += len(ledger)

    def driver_cell(self, t, sid):
        if self.cells_in_frames:
            return Cell(sid)
        return t.cell

    def exec(self, t, pname, env, block, i, cell):
        heap, prog, rt, low = self.heap, self.prog, self.rt, self.low
        sites = low.sites
        while True:
            stmts = block.stmts
            while i < len(stmts):
                st = stmts[i]
                i += 1
                rt.step(t)
                if st[0] == "dup":
                    heap.dup(env[st[1]])
                    continue
                if st[0] == "drop":
                    heap.drop(env[st[1]])
                    continue
                _, x, e, sid = st
                k = e[0]
                if k in ("call", "callc"):
                    ledger = low.calls[sid][4]
                    t.native.append([env[v] for v in ledger])
                    if sid in low.drive:
                        ccell = self.driver_cell(t, sid)
                    elif sid in low.handed:
                        ccell = cell
                    else:
                        ccell = None
                    if k == "call":
                        if e[1] == "supervise!":
                            rt.sup_begin(t, env[e[2][0]])
                        callee = prog.procs[e[1]]
                        v = self.exec(t, e[1], dict(zip(callee.params, [env[a] for a in e[2]])), callee.body, 0,
                                      ccell)
                    else:
                        v = self.call_closure(t, env[e[1]], [env[a] for a in e[2]], ccell)
                    if sid in low.drive:
                        v = self.drive(t, v, ccell)
                    t.native.pop()
                    if sid in sites:
                        self.stats["checks"] += 1
                        if t.pending:
                            dcell = ccell if sid in low.drive else None
                            self.save(t, pname, sid, env, cell, dcell)
                            if self.cells_in_frames and dcell is not None:
                                dcell.dead = True
                            return None
                    elif t.pending:
                        raise AssertionError(f"C2: pending flag after non-site call {sid}")
                    if k == "call" and e[1] == "supervise!":
                        rt.sup_end(t, v)
                    if v is PENDING_CALL:
                        assert sid in low.handed, f"a pending call reached {sid}, which neither drives nor hands up"
                elif k in ("host", "await"):
                    r = rt.hosted(t, e[1], [env[a] for a in e[2]]) if k == "host" else rt.await_any(t, env[e[1]])
                    if sid in sites:
                        self.stats["checks"] += 1
                    if r[0] == "pending":
                        if sid not in sites:
                            raise AssertionError(f"C2: {k} paused at non-site {sid}")
                        t.pending = True
                        self.save(t, pname, sid, env, cell, None)
                        return None
                    v = r[1]
                elif k == "spawn":
                    v = rt.spawn(t, env[e[1]])
                elif k == "cancel":
                    rt.cancel(t, env[e[1]])
                    v = 0
                else:
                    v = eval_simple(heap, env, e)
                env[x] = v
            tm = block.term
            k = tm[0]
            if k == "ret":
                return env[tm[1]]
            if k == "tail":
                # Frame-replacing: no status check and no record; a pending
                # flag set by the callee passes straight up.
                callee = prog.procs[tm[1]]
                env = dict(zip(callee.params, [env[a] for a in tm[2]]))
                pname, block, i = tm[1], callee.body, 0
                self.stats["tail_calls"] += 1
            elif k == "tailc":
                args = [env[a] for a in tm[2]]
                if self.deferred:
                    # A deferred erased call: record it and return; the waiter makes it.
                    assert cell is not None, f"{pname} defers a call but was given no pending cell"
                    cell.put(env[tm[1]], args)
                    self.stats["deferred_calls"] += 1
                    return PENDING_CALL
                # .lss: the lambda set names the target; a direct frame-replacing call.
                f, caps = open_closure(heap, env[tm[1]])
                callee = prog.procs[f]
                env = dict(zip(callee.params, caps + args))
                pname, block, i = f, callee.body, 0
                self.stats["tail_calls"] += 1
            elif k == "if":
                block, i = (tm[2] if env[tm[1]] else tm[3]), 0
            elif k == "match":
                block, env = take_match(heap, env, tm)
                i = 0
            elif k == "crash":
                leaked = [v for frame in t.native for v in frame]
                raise RocCrash(env[tm[1]], leaked)
