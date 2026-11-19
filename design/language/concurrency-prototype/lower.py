"""Suspendability and the suspension lowering's site tables.

Suspendability is computed once, over the post-ARC program, and recorded per
procedure (`may` or `never`) and per call site; the engines read it and never
rediscover it (RFC, "Suspendability").

- Under `lss`, lambda sets give every closure call's possible targets: here a
  0-CFA over closure values stands in for them. A closure call is a
  suspension site only if some target may suspend.
- Under `boxy`, a generic body cannot know its callees, so every call through
  an effectful closure value is a suspension site, decided by its type alone.

A suspension site gets a status check after it and, on the pending path, a
record holding the ledger's values. `Lowering` also exposes, for each site,
where its resume entry continues: statement index+1 of the site's block.

A frame-replacing call through a closure (`tailc`) is a direct frame-replacing
call under `lss`, where the lambda set names its targets. Under `boxy` it is a
deferred erased call (design.md, "Deferred Erased Calls"): the procedure
records the call in a pending cell and returns, and the nearest waiter makes
it. `Lowering` publishes, as tail_drive would, which procedures may return with
a call pending (`leaves`), and for each call site whether it hands the pending
call up (`handed`: the site's value is returned unchanged) or makes it
(`drive`: every other use, and every erased call, whose runtime drives).
"""

from __future__ import annotations

from ir import blocks, expr_vars

SITE_KINDS = ("call", "callc", "host", "await")


class LowerError(Exception):
    pass


def lets(block):
    for b in blocks(block):
        for i, st in enumerate(b.stmts):
            if st[0] == "let":
                yield b, i, st


class Lowering:
    def __init__(self, arc, hosted, mode):
        assert mode in ("lss", "boxy")
        self.arc, self.prog, self.mode = arc, arc.prog, mode
        self.hosted = hosted  # name -> suspending?
        self.lambda_sets()
        self.suspendability()
        self.pending_drives()
        self.sites = {}  # sid -> (proc, block, index, result var, ledger)
        self.calls = {}  # sid -> (proc, block, index, result var, ledger) for every call-like statement
        for p in self.prog.procs.values():
            for b, i, st in lets(p.body):
                _, x, e, sid = st
                if e[0] in ("call", "callc", "host", "await", "spawn", "cancel"):
                    info = (p.name, b, i, x, arc.ledger[sid])
                    self.calls[sid] = info
                    if self.is_site(p, e):
                        self.sites[sid] = info

    # -- lambda sets (0-CFA) --
    def lambda_sets(self):
        prog = self.prog
        cl = {}
        heap = set()
        ret = {f: set() for f in prog.procs}
        get = lambda p, v: cl.get((p, v), set())

        def flow(key, objs):
            cur = cl.setdefault(key, set())
            if not objs <= cur:
                cur |= objs
                return True
            return False

        changed = True
        while changed:
            changed = False
            for p in prog.procs.values():
                for b in blocks(p.body):
                    for st in b.stmts:
                        if st[0] != "let":
                            continue
                        _, x, e, _ = st
                        k = e[0]
                        if k == "clo":
                            f = e[1]
                            changed |= flow((p.name, x), {f})
                            for a, q in zip(e[2], prog.procs[f].params):
                                changed |= flow((f, q), get(p.name, a))
                        elif k == "call":
                            f = e[1]
                            for a, q in zip(e[2], prog.procs[f].params):
                                changed |= flow((f, q), get(p.name, a))
                            changed |= flow((p.name, x), ret[f])
                        elif k == "callc":
                            for g in list(get(p.name, e[1])):
                                ps = prog.procs[g].params
                                for a, q in zip(e[2], ps[len(ps) - len(e[2]):]):
                                    changed |= flow((g, q), get(p.name, a))
                                changed |= flow((p.name, x), ret[g])
                        elif k in ("tup", "tag", "list", "set", "push"):
                            for a in expr_vars(e):
                                n = get(p.name, a)
                                if not n <= heap:
                                    heap |= n
                                    changed = True
                        elif k in ("field", "get", "await"):
                            changed |= flow((p.name, x), heap)
                        elif k == "spawn":
                            for g in list(get(p.name, e[1])):
                                if not ret[g] <= heap:
                                    heap |= ret[g]
                                    changed = True
                    t = b.term
                    if t[0] == "ret":
                        n = get(p.name, t[1])
                        if not n <= ret[p.name]:
                            ret[p.name] |= n
                            changed = True
                    elif t[0] == "tail":
                        f = t[1]
                        for a, q in zip(t[2], prog.procs[f].params):
                            changed |= flow((f, q), get(p.name, a))
                        if not ret[f] <= ret[p.name]:
                            ret[p.name] |= ret[f]
                            changed = True
                    elif t[0] == "tailc":
                        for g in list(get(p.name, t[1])):
                            ps = prog.procs[g].params
                            for a, q in zip(t[2], ps[len(ps) - len(t[2]):]):
                                changed |= flow((g, q), get(p.name, a))
                            if not ret[g] <= ret[p.name]:
                                ret[p.name] |= ret[g]
                                changed = True
                    elif t[0] == "match":
                        for _, vs, _ in t[2]:
                            for v in vs:
                                changed |= flow((p.name, v), heap)
        self.cl = cl

    def targets(self, proc, var):
        return self.cl.get((proc, var), set())

    # -- suspendability --
    def suspendability(self):
        prog = self.prog
        may = {f: False for f in prog.procs}
        changed = True
        while changed:
            changed = False
            for p in prog.procs.values():
                if may[p.name] or not p.eff:
                    continue
                if self.body_may(p, may):
                    may[p.name] = True
                    changed = True
        self.may = may

    def body_may(self, p, may):
        for b in blocks(p.body):
            for st in b.stmts:
                if st[0] == "let" and self.may_suspend_expr(p, st[2], may):
                    return True
            if b.term[0] == "tail" and may[b.term[1]]:
                return True
            if b.term[0] == "tailc":
                if self.mode == "boxy":
                    return True  # an erased effectful call
                if any(may[g] for g in self.targets(p.name, b.term[1])):
                    return True
        return False

    # -- pending calls (.boxy deferred erased calls) --
    def pending_drives(self):
        """Which procedures may return with a call pending, and which call sites
        hand it up or make it. Only `.boxy` defers erased calls."""
        prog = self.prog
        self.leaves = {f: False for f in prog.procs}
        self.drive, self.handed = set(), set()
        if self.mode != "boxy":
            return
        changed = True
        while changed:
            changed = False
            for p in prog.procs.values():
                if self.leaves[p.name]:
                    continue
                for b in blocks(p.body):
                    t = b.term
                    hit = t[0] == "tailc" or (t[0] == "tail" and self.leaves[t[1]])
                    for i, st in enumerate(b.stmts):
                        if st[0] == "let" and st[2][0] == "call" and self.leaves[st[2][1]] \
                                and returned_unchanged(b, i):
                            hit = True
                    if hit:
                        self.leaves[p.name] = True
                        changed = True
                        break
        for p in prog.procs.values():
            for b, i, st in lets(p.body):
                e, sid = st[2], st[3]
                if e[0] == "callc":
                    self.drive.add(sid)
                elif e[0] == "call" and self.leaves[e[1]]:
                    (self.handed if returned_unchanged(b, i) else self.drive).add(sid)

    def may_suspend_expr(self, p, e, may):
        k = e[0]
        if k == "host":
            return self.hosted[e[1]]
        if k == "await":
            return True
        if k == "call":
            return may[e[1]]
        if k == "callc":
            if not e[3]:
                return False
            if self.mode == "boxy":
                return True
            return any(may[g] for g in self.targets(p.name, e[1]))
        return False

    def is_site(self, p, e):
        return e[0] in SITE_KINDS and self.may_suspend_expr(p, e, self.may)

    # -- certification --
    def certify(self):
        """C2: a `never` procedure has no suspension site. C3: every site's
        ledger holds every variable its continuation uses, except the result."""
        for p in self.prog.procs.values():
            for b, i, st in lets(p.body):
                sid = st[3]
                if sid in self.sites and not self.may[p.name]:
                    raise LowerError(f"C2: never-suspending {p.name} has site {sid}")
                if not p.eff and st[2][0] in ("host", "await", "spawn", "cancel"):
                    raise LowerError(f"pure {p.name} performs {st[2][0]}")
        for sid, (proc, b, i, x, ledger) in self.sites.items():
            need = suffix_uses(b, i + 1) - {x}
            if not need <= set(ledger):
                raise LowerError(f"C3: site {sid} in {proc} misses {need - set(ledger)}")
        # A pending call is made only where it may arise; a site that makes one
        # whose call may suspend is a suspension site, so it has a record.
        for sid in self.drive | self.handed:
            proc, b, i, x, _ = self.calls[sid]
            if self.mode != "boxy":
                raise LowerError(f"{sid}: pending-call handling outside .boxy")
            if sid in self.handed and not returned_unchanged(b, i):
                raise LowerError(f"{sid}: handed up, but its value is used")
        for p in self.prog.procs.values():
            if self.leaves[p.name] and not self.may[p.name]:
                raise LowerError(f"C2: {p.name} leaves erased calls pending but is `never`")


def returned_unchanged(b, i):
    """The value of statement i is the block's result: only count statements
    on other variables separate it from `ret`."""
    x = b.stmts[i][1]
    return b.term == ("ret", x) and all(st[0] in ("dup", "drop") and st[1] != x for st in b.stmts[i + 1:])


def suffix_uses(b, start):
    """Variables the code from statement `start` of block b uses before defining."""
    live = term_uses(b.term)
    for st in reversed(b.stmts[start:]):
        if st[0] in ("dup", "drop"):
            live = live | {st[1]}
        else:
            live = (live - {st[1]}) | set(expr_vars(st[2]))
    return live


def term_uses(t):
    k = t[0]
    if k in ("ret", "crash"):
        return {t[1]}
    if k == "tail":
        return set(t[2])
    if k == "tailc":
        return {t[1]} | set(t[2])
    if k == "if":
        return {t[1]} | suffix_uses(t[2], 0) | suffix_uses(t[3], 0)
    if k == "match":
        out = {t[1]}
        for _, vs, blk in t[2]:
            out |= suffix_uses(blk, 0) - set(vs)
        return out
    raise ValueError(t)
