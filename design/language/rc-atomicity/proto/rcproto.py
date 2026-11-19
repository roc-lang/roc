"""Prototype of reference-count atomicity analyses for fork-join tasks.

The language is the Maude model's (design/language/rc-atomicity/maude) plus
procedure calls, recursion and a shape test:

    ('mk', x, ((label, y), ...))     x := C(label: y, ...)     (incs each y)
    ('cp', x, y)                     x := y                    (inc)
    ('proj', x, y, label)            x := y.label              (inc)
    ('drop', x)                      release x                 (dec, recursive free)
    ('setf', x, y, label, z)         x := y with label = z     (consumes y; in place if unique)
    ('host', x)                      a host keeps a reference to x
    ('fork', [Child, ...])           parallel children, then join
    ('call', x, f, (y, ...))         x := f(y, ...)            (incs each y)
    ('ifnode', y, then, else)        branch on whether y has fields

Every local holds one owned reference. The interpreter records every count
event with the (procedure, local, path) its RC statement names, the
series-parallel party it ran in, and the analysis context it ran under. A
race oracle then finds the events that can race, and each analysis is scored
on soundness (racy events must be atomic) and precision (atomic events that
cannot race).

Analyses:
    all_atomic   every count update atomic
    unify        the current analysis: union-find over all value flow, seeded
                 at every spawn boundary and hosted call
    forward      the proposal: spawn seeds only on captures that may alias a
                 sibling's, propagated forward over (local, path) facts;
                 host seeds keep equality classes
    forward_ctx  forward, with procedures specialized by their parameters'
                 modes
    lean         dynamic: objects marked shared at spawn, at child results
                 and at hosted calls; every update tests the mark; marked
                 objects never update in place
"""

from __future__ import annotations

import itertools
import random
import sys
from dataclasses import dataclass, field

LABELS = ("l", "r")
OTHER = {"l": "r", "r": "l"}
DEPTH = 4  # path facts deeper than this are truncated to a prefix (sound)


@dataclass
class Child:
    binds: list  # [(param, arg)]
    body: list
    ret: str
    bindto: str


@dataclass
class Proc:
    name: str
    params: list
    body: list
    ret: str


@dataclass
class Program:
    procs: dict
    main: list


# ---------------------------------------------------------------------------
# Static structure
# ---------------------------------------------------------------------------


def walk_stmts(body):
    """Yield every statement, descending into fork children and branches."""
    for st in body:
        yield st
        if st[0] == "fork":
            for ch in st[1]:
                yield from walk_stmts(ch.body)
        elif st[0] == "ifnode":
            yield from walk_stmts(st[2])
            yield from walk_stmts(st[3])


def proc_bodies(prog):
    yield "main", prog.main
    for p in prog.procs.values():
        yield p.name, p.body


def local_edges(proc, body):
    """Intraprocedural flow edges (a, pa, b, pb): fact (a, pa s) gives (b, pb s)."""
    q = lambda x: (proc, x)
    edges = []
    for st in walk_stmts(body):
        k = st[0]
        if k == "mk":
            for lab, y in st[2]:
                edges.append((q(y), (), q(st[1]), (lab,)))
        elif k == "cp":
            edges.append((q(st[2]), (), q(st[1]), ()))
        elif k == "proj":
            edges.append((q(st[2]), (st[3],), q(st[1]), ()))
        elif k == "setf":
            _, x, y, lab, z = st
            edges.append((q(y), (OTHER[lab],), q(x), (OTHER[lab],)))
            edges.append((q(z), (), q(x), (lab,)))
        elif k == "fork":
            for ch in st[1]:
                for p, a in ch.binds:
                    edges.append((q(a), (), q(p), ()))
                edges.append((q(ch.ret), (), q(ch.bindto), ()))
    return edges


def call_sites(proc, body):
    return [st for st in walk_stmts(body) if st[0] == "call"]


def call_edges(prog, proc, body):
    edges = []
    for st in call_sites(proc, body):
        _, x, f, args = st
        callee = prog.procs[f]
        for a, p in zip(args, callee.params):
            edges.append(((proc, a), (), (f, p), ()))
        edges.append(((f, callee.ret), (), (proc, x), ()))
    return edges


def all_edges(prog):
    edges = []
    for name, body in proc_bodies(prog):
        edges += local_edges(name, body)
        edges += call_edges(prog, name, body)
    return edges


def all_locals(prog):
    out = set()
    for a, _, b, _ in all_edges(prog):
        out.add(a)
        out.add(b)
    for name, body in proc_bodies(prog):
        for st in walk_stmts(body):
            for v in st[1:]:
                if isinstance(v, str):
                    out.add((name, v))
    for p in prog.procs.values():
        for x in p.params + [p.ret]:
            out.add((p.name, x))
    return out


class UnionFind:
    def __init__(self):
        self.parent = {}

    def find(self, x):
        self.parent.setdefault(x, x)
        while self.parent[x] != x:
            self.parent[x] = self.parent[self.parent[x]]
            x = self.parent[x]
        return x

    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra != rb:
            self.parent[ra] = rb


def alias_classes(prog):
    uf = UnionFind()
    for x in all_locals(prog):
        uf.find(x)
    for a, _, b, _ in all_edges(prog):
        uf.union(a, b)
    return uf


def host_args(prog):
    return [(name, st[1]) for name, body in proc_bodies(prog) for st in walk_stmts(body) if st[0] == "host"]


def fork_seeds(prog, uf, proc, body):
    seeds = set()
    for st in walk_stmts(body):
        if st[0] != "fork":
            continue
        children = st[1]
        for i, ch in enumerate(children):
            others = {uf.find((proc, a)) for j, c in enumerate(children) if j != i for _, a in c.binds}
            for p, a in ch.binds:
                if uf.find((proc, a)) in others:
                    seeds.add((proc, p))
    return seeds


# ---------------------------------------------------------------------------
# Prefix facts
# ---------------------------------------------------------------------------


class Facts:
    """Path-prefix facts per local, each exact or truncated.

    An exact fact (x, p) means every path from p down is atomic. A truncated
    fact stands for deeper exact facts that were cut to DEPTH steps: the
    positions under it are *mixed*, some shared and some not (note, 4.4).
    Analyses without edge modes treat both kinds as atomic.
    """

    def __init__(self):
        self.by_local = {}  # local -> {path: truncated}

    def mode(self, x, path):
        """'A' if an exact fact covers path, 'T' if only truncated ones do, else None."""
        ps = self.by_local.get(x)
        if not ps:
            return None
        mixed = False
        for p, trunc in ps.items():
            if path[: len(p)] == p:
                if not trunc:
                    return "A"
                mixed = True
        return "T" if mixed else None

    def covered(self, x, path):
        return self.mode(x, path) is not None

    def add(self, x, path, trunc=False):
        path = tuple(path)
        if len(path) > DEPTH:
            path, trunc = path[:DEPTH], True
        m = self.mode(x, path)
        if m == "A" or (m == "T" and trunc):
            return False
        ps = self.by_local.setdefault(x, {})
        for p, tr in list(ps.items()):
            if p[: len(path)] == path and (tr or not trunc):
                del ps[p]
        ps[path] = trunc
        return True

    def items(self):
        for x, ps in self.by_local.items():
            for p in ps:
                yield x, p

    def flagged(self):
        for x, ps in self.by_local.items():
            for p, tr in ps.items():
                yield x, p, tr


def close(edges, facts):
    out_edges = {}
    for a, pa, b, pb in edges:
        out_edges.setdefault(a, []).append((pa, b, pb))
    work = [x for x in facts.by_local]
    while work:
        a = work.pop()
        for f, tr in list(facts.by_local.get(a, {}).items()):
            for pa, b, pb in out_edges.get(a, ()):
                if f[: len(pa)] == pa:
                    new = pb + f[len(pa):]
                elif pa[: len(f)] == f:
                    new = pb
                else:
                    continue
                if facts.add(b, new, tr):
                    work.append(b)
    return facts


# ---------------------------------------------------------------------------
# Analyses
# ---------------------------------------------------------------------------


class Analysis:
    name = "?"

    def root_ctx(self):
        return None

    def callee_ctx(self, ctx, proc, stmt):
        return None

    def atomic(self, ctx, local, path):
        raise NotImplementedError


class AllAtomic(Analysis):
    name = "all_atomic"

    def __init__(self, prog):
        pass

    def atomic(self, ctx, local, path):
        return True


class Unify(Analysis):
    """The current analysis: one bit per alias class, seeded at every boundary."""

    name = "unify"

    def __init__(self, prog):
        uf = alias_classes(prog)
        seeded = set()
        for name, body in proc_bodies(prog):
            for st in walk_stmts(body):
                if st[0] == "host":
                    seeded.add(uf.find((name, st[1])))
                elif st[0] == "fork":
                    for ch in st[1]:
                        for p, a in ch.binds:
                            seeded.add(uf.find((name, p)))
                            seeded.add(uf.find((name, a)))
                        seeded.add(uf.find((name, ch.ret)))
                        seeded.add(uf.find((name, ch.bindto)))
        self.uf, self.seeded = uf, seeded

    def atomic(self, ctx, local, path):
        return self.uf.find(local) in self.seeded


def host_seed_locals(prog, uf):
    roots = {uf.find(h) for h in host_args(prog)}
    return {x for x in all_locals(prog) if uf.find(x) in roots}


class Forward(Analysis):
    name = "forward"

    def __init__(self, prog):
        uf = alias_classes(prog)
        facts = Facts()
        for name, body in proc_bodies(prog):
            for x in fork_seeds(prog, uf, name, body):
                facts.add(x, ())
        for x in host_seed_locals(prog, uf):
            facts.add(x, ())
        self.facts = close(all_edges(prog), facts)

    def atomic(self, ctx, local, path):
        return self.facts.covered(local, path)


class ForwardCtx(Analysis):
    """Forward analysis with procedures specialized by parameter modes.

    A context is (proc, key), where key is the frozenset of (param index,
    path prefix) facts that hold on entry.
    """

    name = "forward_ctx"

    def __init__(self, prog):
        self.prog = prog
        uf = alias_classes(prog)
        self.host_locals = host_seed_locals(prog, uf)
        self.local_edges = {n: local_edges(n, b) for n, b in proc_bodies(prog)}
        self.fseeds = self.spawn_seeds(prog, uf)
        self.sites = {n: call_sites(n, b) for n, b in proc_bodies(prog)}
        self.ctx_facts = {}
        self.ret_facts = {}
        self.callee = {}
        root = ("main", frozenset())
        known = {root}
        changed = True
        while changed:
            changed = False
            for ctx in list(known):
                facts = self.solve(ctx)
                proc = ctx[0]
                old = self.ctx_facts.get(ctx)
                snapshot = frozenset(facts.flagged())
                if old is None or old != snapshot:
                    self.ctx_facts[ctx] = snapshot
                    changed = True
                self._facts_obj = getattr(self, "_facts_obj", {})
                self._facts_obj[ctx] = facts
                for st in self.sites[proc]:
                    key = self.callee_key(facts, proc, st)
                    cctx = (st[2], key)
                    self.callee[(ctx, id(st))] = cctx
                    if cctx not in known:
                        known.add(cctx)
                        changed = True
                if proc != "main":
                    ret = (proc, self.prog.procs[proc].ret)
                    rp = frozenset((p, tr) for x, p, tr in facts.flagged() if x == ret)
                    if self.ret_facts.get(ctx) != rp:
                        self.ret_facts[ctx] = rp
                        changed = True
        self.contexts = known

    def callee_key(self, facts, proc, st):
        _, x, f, args = st
        key = set()
        for i, a in enumerate(args):
            for loc, p, tr in facts.flagged():
                if loc == (proc, a):
                    key.add((i, p, tr))
        return frozenset(key)

    def solve(self, ctx):
        proc, key = ctx
        facts = Facts()
        if proc != "main":
            params = self.prog.procs[proc].params
            for i, p, tr in key:
                facts.add((proc, params[i]), p, tr)
        for x in self.fseeds[proc]:
            facts.add(x, ())
        for x in self.host_locals:
            if x[0] == proc:
                facts.add(x, ())
        edges = list(self.local_edges[proc])
        # results of calls under the callee contexts already known
        prev = getattr(self, "_facts_obj", {}).get(ctx)
        for st in self.sites[proc]:
            if prev is not None:
                cctx = self.callee[(ctx, id(st))] if (ctx, id(st)) in self.callee else None
            else:
                cctx = None
            if cctx is not None:
                for p, tr in self.ret_facts.get(cctx, ()):
                    facts.add((proc, st[1]), p, tr)
        return close(edges, facts)

    def spawn_seeds(self, prog, uf):
        return {n: fork_seeds(prog, uf, n, b) for n, b in proc_bodies(prog)}

    def root_ctx(self):
        return ("main", frozenset())

    def callee_ctx(self, ctx, proc, stmt):
        return self.callee[(ctx, id(stmt))]

    def atomic(self, ctx, local, path):
        return self._facts_obj[ctx].covered(local, path)

    def mode(self, ctx, local, path):
        return "A" if self.atomic(ctx, local, path) else None



class PointsTo:
    """May-alias for spawn seeds that keeps call sites apart.

    Each local gets the set of abstract objects it may reach (field-insensitive):
    ('S', proc, k) for the k-th allocation or call site of proc, and ('P', i)
    for whatever parameter i reaches. A procedure's summary says what its result
    may reach: some parameters, and FRESH objects it allocated, which a caller
    names by its own call site. A site runs at most once per frame (the IR has
    no loops), so two locals of one frame reach a common object only if their
    sets intersect, or hold parameters that some caller passed overlapping
    arguments for.
    """

    def __init__(self, prog):
        self.prog = prog
        self.summary = {f: frozenset() for f in prog.procs}
        self.site = {}
        for name, body in proc_bodies(prog):
            for k, st in enumerate(walk_stmts(body)):
                self.site[(name, id(st))] = ("S", name, k)
        while True:
            self.reach = {name: self.solve(name, body) for name, body in proc_bodies(prog)}
            new = {}
            for f, p in prog.procs.items():
                r = self.reach[f].get(p.ret, set())
                new[f] = frozenset(n if n[0] == "P" else "FRESH" for n in r)
            if new == self.summary:
                break
            self.summary = new
        self.ovl = {f: set() for f in prog.procs}
        self.ovl["main"] = set()
        changed = True
        while changed:
            changed = False
            for name, body in proc_bodies(prog):
                r = self.reach[name]
                for st in walk_stmts(body):
                    if st[0] != "call":
                        continue
                    args = st[3]
                    for i, j in itertools.combinations(range(len(args)), 2):
                        pair = frozenset((i, j))
                        if pair not in self.ovl[st[2]] and self.overlap(name, r.get(args[i], set()),
                                                                         r.get(args[j], set())):
                            self.ovl[st[2]].add(pair)
                            changed = True

    def solve(self, name, body):
        reach = {}
        if name != "main":
            for i, p in enumerate(self.prog.procs[name].params):
                reach[p] = {("P", i)}
        get = lambda x: reach.get(x, set())
        changed = True
        while changed:
            changed = False

            def flow(x, objs):
                nonlocal changed
                cur = reach.setdefault(x, set())
                if not objs <= cur:
                    cur |= objs
                    changed = True

            for st in walk_stmts(body):
                k = st[0]
                me = self.site[(name, id(st))]
                if k == "mk":
                    flow(st[1], {me}.union(*[get(y) for _, y in st[2]]))
                elif k in ("cp", "proj"):
                    flow(st[1], get(st[2]))
                elif k == "setf":
                    flow(st[1], {me} | get(st[2]) | get(st[4]))
                elif k == "call":
                    summ = self.summary[st[2]]
                    objs = {me} if "FRESH" in summ else set()
                    for n in summ:
                        if n != "FRESH":
                            objs |= get(st[3][n[1]])
                    flow(st[1], objs)
                elif k == "fork":
                    for ch in st[1]:
                        for p, a in ch.binds:
                            flow(p, get(a))
                        flow(ch.bindto, get(ch.ret))
        return reach

    def overlap(self, proc, s1, s2):
        if s1 & s2:
            return True
        ps1 = {n[1] for n in s1 if n[0] == "P"}
        ps2 = {n[1] for n in s2 if n[0] == "P"}
        return any(frozenset((i, j)) in self.ovl[proc] for i in ps1 for j in ps2 if i != j)

    def fork_seeds(self, proc, body):
        r = self.reach[proc]
        seeds = set()
        for st in walk_stmts(body):
            if st[0] != "fork":
                continue
            children = st[1]
            for i, ch in enumerate(children):
                others = [a for j, c in enumerate(children) if j != i for _, a in c.binds]
                for p, a in ch.binds:
                    if any(self.overlap(proc, r.get(a, set()), r.get(b, set())) for b in others):
                        seeds.add((proc, p))
        return seeds


class ForwardPts(ForwardCtx):
    """forward_ctx with spawn seeds from the call-site-sensitive PointsTo."""

    name = "forward_pts"

    def spawn_seeds(self, prog, uf):
        pts = PointsTo(prog)
        return {n: pts.fork_seeds(n, b) for n, b in proc_bodies(prog)}


class ForwardEdge(ForwardPts):
    """forward_pts with edge modes (note, 4.4).

    Positions covered only by truncated facts are mixed: their mode is T, and
    RC statements there test a bit carried by the handle they use. The bit is
    written when a handle is created, from the static mode of the value it
    comes from, and is sticky downward: a handle read out of a possibly shared
    object is possibly shared. Run with Machine(edge=True).
    """

    name = "forward_edge"

    def mode(self, ctx, local, path):
        return self._facts_obj[ctx].mode(local, path)


class Borrow:
    """Borrow inference, with lending of captures to children (note, 4.6).

    A borrowed local holds a reference without a count of its own. Parameters
    and child captures are borrowed unless the body consumes them (as the
    target of an in-place update, or as a result); a local copied or projected
    from a borrowed one is borrowed under the same condition. The lender (the
    caller, or the parent across the fork) keeps its own reference alive and
    unchanged meanwhile.
    """

    def __init__(self, prog, lend=True):
        self.b = set()
        for name, body in proc_bodies(prog):
            consumed = {st[2] for st in walk_stmts(body) if st[0] == "setf"}
            if name != "main":
                consumed.add(prog.procs[name].ret)
            for st in walk_stmts(body):
                if st[0] == "fork":
                    consumed |= {ch.ret for ch in st[1]}
            if name != "main":
                for p in prog.procs[name].params:
                    if p not in consumed:
                        self.b.add((name, p))
            for st in walk_stmts(body):
                if st[0] == "fork" and lend:
                    for ch in st[1]:
                        for p, a in ch.binds:
                            if p not in consumed:
                                self.b.add((name, p))
                elif st[0] in ("cp", "proj") and (name, st[2]) in self.b and st[1] not in consumed:
                    self.b.add((name, st[1]))

    def __contains__(self, q):
        return q in self.b


# ---------------------------------------------------------------------------
# Interpreter
# ---------------------------------------------------------------------------


@dataclass
class Obj:
    rc: int
    fields: dict
    freed: bool = False
    marked: bool = False
    ebits: dict = field(default_factory=dict)  # edge-mode bit per field


class Env(dict):
    """A frame's locals, plus each handle's edge-mode bit."""

    def __init__(self, *a):
        super().__init__(*a)
        self.bits = {}


@dataclass
class Event:
    op: str  # inc, dec, test
    obj: int
    local: tuple
    path: tuple
    party: tuple
    ctx: object
    atomic: bool  # the mode the run used (lean and edge runs only)
    host_visible: bool


class Crash(Exception):
    pass


class Machine:
    """Runs a program, recording every count event.

    lean:   Lean-style marking decides atomicity at run time.
    edge:   the analysis's mode() decides, testing handle bits at T positions.
    borrow: a Borrow inference; borrowed locals skip their increments and
            decrements (lending, when captures are borrowed).
    """

    def __init__(self, prog, analysis, lean=False, edge=False, borrow=None):
        self.prog, self.an, self.lean, self.edge = prog, analysis, lean, edge
        self.borrow = borrow if borrow is not None else frozenset()
        self.heap = {}
        self.next_id = 0
        self.next_fork = 0
        self.events = []
        self.host_refs = []
        self.host_reach = set()
        self.walked = 0
        self.reuse_lost = 0
        self.inplace = 0
        self.tests = 0

    # -- heap --
    def alloc(self, fields):
        self.next_id += 1
        self.heap[self.next_id] = Obj(1, dict(fields))
        return self.next_id

    def obj(self, o):
        ob = self.heap[o]
        if ob.freed:
            raise Crash(f"use after free of {o}")
        return ob

    def reach(self, o, out):
        stack = [o]
        while stack:
            x = stack.pop()
            if x in out:
                continue
            out.add(x)
            stack.extend(self.heap[x].fields.values())
        return out

    def mark(self, o):
        stack = [o]
        while stack:
            x = stack.pop()
            ob = self.heap[x]
            if ob.marked or ob.freed:
                continue
            ob.marked = True
            self.walked += 1
            stack.extend(ob.fields.values())

    # -- edge-mode bits --
    def hbit(self, env, proc, x, ctx):
        """The bit a handle in local x carries: from x's static mode, or its own bit at T."""
        if not self.edge:
            return False
        m = self.an.mode(ctx, (proc, x), ())
        return m == "A" or (m == "T" and env.bits.get(x, False))

    def record(self, op, o, local, path, party, ctx, bit=False):
        ob = self.heap[o]
        if local == ("host", "host"):
            atomic = True  # the host's own updates are always atomic
        elif self.lean:
            atomic = ob.marked
        elif self.edge:
            m = self.an.mode(ctx, local, path)
            if m == "T":
                self.tests += 1
                atomic = bit
            else:
                atomic = m == "A"
        else:
            atomic = self.an.atomic(ctx, local, path)
        self.events.append(Event(op, o, local, path, party, ctx, atomic, o in self.host_reach))

    def inc(self, o, local, path, party, ctx, bit=False):
        self.obj(o)
        self.record("inc", o, local, path, party, ctx, bit)
        self.heap[o].rc += 1

    def dec(self, o, local, path, party, ctx, bit=False):
        ob = self.obj(o)
        self.record("dec", o, local, path, party, ctx, bit)
        ob.rc -= 1
        if ob.rc == 0:
            ob.freed = True
            for lab, c in ob.fields.items():
                self.dec(c, local, path + (lab,), party, ctx, bit or ob.ebits.get(lab, False))

    # -- execution --
    def run(self):
        env = Env()
        ctx = self.an.root_ctx() if not self.lean else None
        self.exec_body("main", self.prog.main, env, (), ctx)
        for k, v in env.items():
            raise Crash(f"main left {k} live")
        for o in self.host_refs:
            self.dec(o, ("host", "host"), (), ("host",), None)
        for o, ob in self.heap.items():
            if not ob.freed:
                raise Crash(f"leak of {o} (rc {ob.rc})")

    def exec_body(self, proc, body, env, party, ctx):
        for st in body:
            self.exec_stmt(proc, st, env, party, ctx)

    def exec_stmt(self, proc, st, env, party, ctx):
        q = lambda x: (proc, x)
        hb = lambda x: self.hbit(env, proc, x, ctx)
        borrowed = lambda x: (proc, x) in self.borrow
        k = st[0]
        if k == "mk":
            _, x, fs = st
            o = self.alloc({lab: env[y] for lab, y in fs})
            for lab, y in fs:
                b = hb(y)
                self.heap[o].ebits[lab] = b
                self.inc(env[y], q(y), (), party, ctx, b)
            env[x] = o
            env.bits[x] = False
        elif k == "cp":
            _, x, y = st
            env[x] = env[y]
            env.bits[x] = hb(y)
            if not borrowed(x):
                self.inc(env[y], q(y), (), party, ctx, hb(y))
        elif k == "proj":
            _, x, y, lab = st
            ob = self.obj(env[y])
            o = ob.fields[lab]
            env[x] = o
            env.bits[x] = hb(y) or ob.ebits.get(lab, False)
            if not borrowed(x):
                self.inc(o, q(x), (), party, ctx, hb(x))
        elif k == "drop":
            b = hb(st[1])
            o = env.pop(st[1])
            if not borrowed(st[1]):
                self.dec(o, q(st[1]), (), party, ctx, b)
        elif k == "setf":
            _, x, y, lab, z = st
            by = hb(y)
            o = env.pop(y)
            ob = self.obj(o)
            self.record("test", o, q(y), (), party, ctx, by)
            unique = ob.rc == 1 and not (self.lean and ob.marked)
            if self.lean and ob.rc == 1 and ob.marked:
                self.reuse_lost += 1
            if unique:
                self.inplace += 1
                old, oldbit = ob.fields[lab], by or ob.ebits.get(lab, False)
                ob.fields[lab] = env[z]
                ob.ebits[lab] = hb(z)
                self.inc(env[z], q(z), (), party, ctx, hb(z))
                self.dec(old, q(y), (lab,), party, ctx, oldbit)
                env[x] = o
            else:
                fields = dict(ob.fields)
                fields[lab] = env[z]
                n = self.alloc(fields)
                for l2, c in ob.fields.items():
                    if l2 != lab:
                        b = by or ob.ebits.get(l2, False)
                        self.heap[n].ebits[l2] = b
                        self.inc(c, q(y), (l2,), party, ctx, b)
                self.heap[n].ebits[lab] = hb(z)
                self.inc(env[z], q(z), (), party, ctx, hb(z))
                self.dec(o, q(y), (), party, ctx, by)
                env[x] = n
            env.bits[x] = False
        elif k == "host":
            o = env[st[1]]
            self.inc(o, q(st[1]), (), party, ctx, hb(st[1]))
            self.host_refs.append(o)
            self.reach(o, self.host_reach)
            if self.lean:
                self.mark(o)
        elif k == "call":
            _, x, f, args = st
            callee = self.prog.procs[f]
            cenv = Env()
            for a, p in zip(args, callee.params):
                if (f, p) not in self.borrow:
                    self.inc(env[a], q(a), (), party, ctx, hb(a))
                cenv[p] = env[a]
                cenv.bits[p] = hb(a)
            cctx = None if self.lean else self.an.callee_ctx(ctx, proc, st)
            self.exec_body(f, callee.body, cenv, party, cctx)
            env.bits[x] = self.hbit(cenv, f, callee.ret, cctx)
            env[x] = cenv.pop(callee.ret)
            if cenv:
                raise Crash(f"{f} left {sorted(cenv)} live")
        elif k == "ifnode":
            branch = st[2] if self.obj(env[st[1]]).fields else st[3]
            self.exec_body(proc, branch, env, party, ctx)
        elif k == "fork":
            self.next_fork += 1
            fid = self.next_fork
            children = st[1]
            for ch in children:
                for p, a in ch.binds:
                    if not borrowed(p):
                        self.inc(env[a], q(a), (), party, ctx, hb(a))
            if self.lean:
                for ch in children:
                    for p, a in ch.binds:
                        self.mark(env[a])
            results = []
            for i, ch in enumerate(children):
                cenv = Env({p: env[a] for p, a in ch.binds})
                for p, a in ch.binds:
                    cenv.bits[p] = hb(a)
                cparty = party + ((fid, i),)
                self.exec_body(proc, ch.body, cenv, cparty, ctx)
                rbit = self.hbit(cenv, proc, ch.ret, ctx)
                r = cenv.pop(ch.ret)
                if cenv:
                    raise Crash(f"child left {sorted(cenv)} live")
                if self.lean:
                    self.mark(r)
                results.append((ch.bindto, r, rbit))
            for b, r, rbit in results:
                env[b] = r
                env.bits[b] = rbit
        else:
            raise ValueError(st)


# ---------------------------------------------------------------------------
# Race oracle
# ---------------------------------------------------------------------------


def parallel(p1, p2):
    """Series-parallel criterion: the first divergence is between two children of one fork."""
    for a, b in zip(p1, p2):
        if a == b:
            continue
        return a[0] == b[0] and a[1] != b[1]
    return False


def racy_events(events):
    by_obj = {}
    for i, e in enumerate(events):
        by_obj.setdefault(e.obj, []).append(i)
    racy = set()
    for idxs in by_obj.values():
        groups = {}
        for i in idxs:
            groups.setdefault(events[i].party, []).append(i)
        parties = list(groups)
        for a, b in itertools.combinations(parties, 2):
            if a == ("host",) or b == ("host",) or not parallel(a, b):
                continue
            ga, gb = groups[a], groups[b]
            a_writes = any(events[i].op != "test" for i in ga)
            b_writes = any(events[i].op != "test" for i in gb)
            for i in ga:
                if b_writes or events[i].op != "test":
                    racy.add(i)
            for i in gb:
                if a_writes or events[i].op != "test":
                    racy.add(i)
    for i, e in enumerate(events):
        if e.host_visible and e.party != ("host",):
            racy.add(i)
    return racy


@dataclass
class Score:
    events: int = 0
    atomic: int = 0
    racy: int = 0
    false_atomic: int = 0
    unsound: int = 0
    tests: int = 0
    walked: int = 0
    reuse_lost: int = 0
    inplace: int = 0

    def add(self, o):
        for k in self.__dataclass_fields__:
            setattr(self, k, getattr(self, k) + getattr(o, k))


def score(prog, analysis_cls, lean=False, edge=False, borrow=None):
    an = None if lean else analysis_cls(prog)
    m = Machine(prog, an, lean=lean, edge=edge, borrow=borrow)
    m.run()
    racy = racy_events(m.events)
    s = Score(events=len(m.events), walked=m.walked, reuse_lost=m.reuse_lost, inplace=m.inplace,
              tests=m.tests)
    for i, e in enumerate(m.events):
        if e.local == ("host", "host"):
            continue
        atomic = e.atomic if (lean or edge) else an.atomic(e.ctx, e.local, e.path)
        s.atomic += atomic
        s.racy += i in racy
        s.false_atomic += atomic and i not in racy
        s.unsound += (not atomic) and i in racy
    if lean:
        s.tests = s.events
    return s, m


ANALYSES = [AllAtomic, Unify, Forward, ForwardCtx, ForwardPts]


def score_all(prog):
    out = {}
    for cls in ANALYSES:
        out[cls.name], _ = score(prog, cls)
    out["forward_edge"], _ = score(prog, ForwardEdge, edge=True)
    out["lean"], _ = score(prog, None, lean=True)
    return out
