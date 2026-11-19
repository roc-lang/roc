"""Ownership: explicit dup/drop insertion and the suspension ledger.

Every variable holds exactly one reference from its definition until its last
use consumes it. A use that is not the last is preceded by `dup`; a variable
that is never used, or that is live into only one branch, gets `drop`. So at
every program point the owned variables are exactly the live ones.

ARC also publishes, for every call-like statement, the variables it owns
after that statement other than the result: the **suspension ledger** (RFC,
"What ARC must publish"). Lowering reads the ledger; it never recomputes it.
`certify` checks the ledger and the placement by an independent simulation of
ownership, as the debug certifier would.
"""

from __future__ import annotations

import itertools
from collections import Counter

from ir import Block, Proc, Program, expr_vars

CALL_LIKE = ("call", "callc", "host", "await", "spawn", "cancel")


class ArcError(Exception):
    pass


class Arc:
    def __init__(self, prog: Program):
        self.src = prog
        self.ledger = {}  # sid -> tuple of owned variables after the statement
        self.sid = itertools.count()
        self.memo = {}
        procs = {}
        for name, p in prog.procs.items():
            self.proc = name
            body = self.transform(p.body, set(p.params))
            procs[name] = Proc(name, p.params, body, p.eff)
        self.prog = Program(procs, prog.entry)

    # -- liveness --
    def live_in(self, b):
        key = id(b)
        if key in self.memo:
            return self.memo[key]
        live = self.term_live(b.term)
        for st in reversed(b.stmts):
            _, x, e = st
            live = (live - {x}) | set(expr_vars(e))
        self.memo[key] = live
        return live

    def term_live(self, t):
        k = t[0]
        if k in ("ret", "crash"):
            return {t[1]}
        if k == "tail":
            return set(t[2])
        if k == "tailc":
            return {t[1]} | set(t[2])
        if k == "if":
            return {t[1]} | self.live_in(t[2]) | self.live_in(t[3])
        if k == "match":
            return {t[1]} | self.match_shared(t)
        raise ValueError(t)

    def match_shared(self, t):
        out = set()
        for _, vs, blk in t[2]:
            out |= self.live_in(blk) - set(vs)
        return out

    # -- placement --
    def transform(self, b, entry_owned):
        live_start = self.live_in(b)
        extra = entry_owned - live_start
        pre = [("drop", v) for v in sorted(extra)]
        t = b.term
        k = t[0]
        tail_pre = []
        if k in ("ret", "crash"):
            new_term = t
            live = {t[1]}
        elif k in ("tail", "tailc"):
            args = t[2] if k == "tail" else [t[1]] + list(t[2])
            for a, n in Counter(args).items():
                tail_pre += [("dup", a)] * (n - 1)
            new_term = t
            live = set(args)
        elif k == "if":
            la, lb = self.live_in(t[2]), self.live_in(t[3])
            inner = la | lb
            if t[1] in inner:
                tail_pre.append(("dup", t[1]))
            new_term = ("if", t[1], self.transform(t[2], inner), self.transform(t[3], inner))
            live = inner | {t[1]}
        elif k == "match":
            shared = self.match_shared(t)
            if t[1] in shared:
                tail_pre.append(("dup", t[1]))
            arms = [(tag, vs, self.transform(blk, shared | set(vs))) for tag, vs, blk in t[2]]
            new_term = ("match", t[1], arms)
            live = shared | {t[1]}
        else:
            raise ValueError(t)
        out = tail_pre
        for st in reversed(b.stmts):
            _, x, e = st
            args = expr_vars(e)
            after = live - {x}
            post = [] if x in live else [("drop", x)]
            dups = []
            for a, n in Counter(args).items():
                dups += [("dup", a)] * (n if a in after else n - 1)
            sid = (self.proc, next(self.sid))
            if e[0] in CALL_LIKE:
                self.ledger[sid] = tuple(sorted(after))
            out = dups + [("let", x, e, sid)] + post + out
            live = after | set(args)
        return Block(pre + out, new_term)


# ---------------------------------------------------------------------------
# Certification
# ---------------------------------------------------------------------------


def certify(arc: Arc):
    """Simulate ownership over the placed code. Every use must consume an owned
    reference, nothing may be owned at a terminator except what it consumes,
    and each ledger entry must equal what is owned after its statement."""
    for p in arc.prog.procs.values():
        owned = Counter({x: 1 for x in p.params})
        _cert_block(arc, p.name, p.body, owned)


def _take(owned, v, where):
    if owned[v] < 1:
        raise ArcError(f"{where}: {v} used without an owned reference")
    owned[v] -= 1
    if owned[v] == 0:
        del owned[v]


def _cert_block(arc, proc, b, owned):
    owned = Counter(owned)
    for st in b.stmts:
        k = st[0]
        if k == "dup":
            if owned[st[1]] < 1:
                raise ArcError(f"{proc}: dup of unowned {st[1]}")
            owned[st[1]] += 1
        elif k == "drop":
            _take(owned, st[1], proc)
        else:
            _, x, e, sid = st
            for a in expr_vars(e):
                _take(owned, a, f"{proc} {sid}")
            if e[0] in CALL_LIKE:
                want = arc.ledger[sid]
                have = tuple(sorted(owned))
                if have != want or any(owned[v] != 1 for v in owned):
                    raise ArcError(f"{proc} {sid}: ledger {want} but owned {dict(owned)}")
            if owned[x]:
                raise ArcError(f"{proc}: {x} rebound while owned")
            owned[x] = 1
    t = b.term
    k = t[0]
    if k in ("ret", "crash"):
        _take(owned, t[1], proc)
        rest = []
    elif k in ("tail", "tailc"):
        for a in (t[2] if k == "tail" else [t[1]] + list(t[2])):
            _take(owned, a, proc)
        rest = []
    elif k == "if":
        _take(owned, t[1], proc)
        rest = [(t[2], Counter(owned)), (t[3], Counter(owned))]
    elif k == "match":
        _take(owned, t[1], proc)
        rest = []
        for _, vs, blk in t[2]:
            o = Counter(owned)
            for v in vs:
                if o[v]:
                    raise ArcError(f"{proc}: pattern rebinds owned {v}")
                o[v] = 1
            rest.append((blk, o))
    else:
        raise ValueError(t)
    if not rest and owned:
        raise ArcError(f"{proc}: {dict(owned)} still owned at {k}")
    for blk, o in rest:
        _cert_block(arc, proc, blk, o)
