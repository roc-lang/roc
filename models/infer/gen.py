"""Generator: small typed constructs composed at random into modules.

Each module has library functions (some annotated), then use-site functions
that call them.  The reference decides the expected verdict.
"""
import random
from lang import *
from ref import find, TypeError_, Printer

TAGS = ["A", "B", "C", "D"]


class Gen:
    def __init__(self, seed):
        self.r = random.Random(seed)
        self.n = 0

    def fresh(self, base):
        self.n += 1
        return "%s%d" % (base, self.n)

    # ---------------- expressions
    def expr(self, scope, depth, funs, allow_q=False):
        r = self.r
        choices = ["var", "lit", "tag", "tag", "rec", "if", "match", "call", "list", "block", "access", "pcall", "lam"]
        if depth <= 0:
            choices = ["var", "lit", "tag", "var"]
        c = r.choice(choices)
        vals = [v for v in scope]
        if c == "var" and vals:
            return Var(r.choice(vals))
        if c == "lit" or (c == "var" and not vals):
            # Str literals are `from_quote`-polymorphic in Roc; the fragment keeps U64 only
            return Lit("U64", str(r.randint(0, 9)))
        if c == "tag":
            name = r.choice(TAGS)
            nargs = r.choice([0, 0, 1, 1, 2]) if depth > 0 else r.choice([0, 1])
            return Tag(name, [self.expr(scope, depth - 1, funs) for _ in range(nargs)])
        if c == "rec":
            ks = r.sample(["a", "b", "c"], r.randint(1, 2))
            return Rec({k: self.expr(scope, depth - 1, funs) for k in ks})
        if c == "access" and vals:
            return Access(Var(r.choice(vals)), r.choice(["a", "b"]))
        if c == "if":
            cond = Var(r.choice(vals)) if vals and r.random() < 0.5 else BoolLit(r.random() < 0.5)
            return If(cond, self.expr(scope, depth - 1, funs), self.expr(scope, depth - 1, funs))
        if c == "match":
            scr = Var(r.choice(vals)) if vals and r.random() < 0.8 else self.expr(scope, depth - 1, funs)
            return Match(scr, self.branches(scope, depth, funs))
        if c == "call" and funs:
            f, arity = r.choice(funs)
            return Call(Var(f), [self.expr(scope, depth - 1, funs) for _ in range(arity)])
        if c == "pcall" and vals:
            # a parameter used as a function (higher-order, R-Polarity on callbacks)
            return Call(Var(r.choice(vals)), [self.expr(scope, depth - 1, funs)])
        if c == "lam":
            p = self.fresh("y")
            lam = Lam([PVar(p)], self.expr(scope + [p], depth - 1, funs))
            if funs and r.random() < 0.5:
                f, arity = r.choice(funs)
                return Call(Var(f), [lam] + [self.expr(scope, depth - 1, funs) for _ in range(arity - 1)])
            return lam
        if c == "list":
            return ListLit([self.expr(scope, depth - 1, funs) for _ in range(r.randint(1, 3))])
        if c == "block":
            nm = self.fresh("l")
            if r.random() < 0.5:
                p = self.fresh("z")
                rhs = Lam([PVar(p)], self.expr(scope + [p], depth - 1, funs))
                body = self.expr(scope + [nm], depth - 1, funs + [(nm, 1)])
                if r.random() < 0.7:
                    body = Rec({"a": Call(Var(nm), [self.expr(scope, 0, funs)]),
                                "b": Call(Var(nm), [self.expr(scope, 0, funs)])})
            else:
                rhs = self.expr(scope, depth - 1, funs)
                body = self.expr(scope + [nm], depth - 1, funs)
            return Block([(nm, None, rhs)], body)
        return Tag(r.choice(TAGS), [])

    def pattern(self, depth):
        r = self.r
        name = r.choice(TAGS)
        nargs = r.choice([0, 0, 1, 1])
        args = []
        for _ in range(nargs):
            if depth > 0 and r.random() < 0.25:
                args.append(PTag(r.choice(TAGS), []))
            elif r.random() < 0.7:
                args.append(PVar(self.fresh("p")))
            else:
                args.append(PWild())
        return PTag(name, args)

    def branches(self, scope, depth, funs):
        r = self.r
        k = r.randint(1, 3)
        pats = []
        used = {}
        for _ in range(k):
            p = self.pattern(depth)
            if p.name in used:
                # same tag, same arity, only nested-tag variation is allowed
                q = used[p.name]
                if len(q.args) != len(p.args) or len(p.args) != 1 or not isinstance(p.args[0], PTag) \
                        or not isinstance(q.args[0], PTag) or p.args[0].name == q.args[0].name:
                    continue
            used.setdefault(p.name, p)
            pats.append(p)
        if r.random() < 0.4:
            pats.append(PWild() if r.random() < 0.5 else PVar(self.fresh("w")))
        out = []
        for p in pats:
            binders = []

            def coll(q):
                if isinstance(q, PVar): binders.append(q.name)
                if isinstance(q, PTag):
                    for a in q.args: coll(a)
            coll(p)
            out.append((p, self.expr(scope + binders, depth - 1, funs)))
        return out

    # ---------------- `?` bodies
    def q_body(self, scope, funs):
        r = self.r
        p = r.choice(scope)
        v = self.fresh("v")
        tail = r.choice([
            Tag("Ok", [Var(v)]),
            Tag("Ok", [Tag(r.choice(TAGS), [Var(v)])]),
            If(BoolLit(r.random() < 0.5), Tag("Err", [Tag(r.choice(TAGS), [])]), Tag("Ok", [Var(v)])),
            If(BoolLit(r.random() < 0.5), Tag("Ok", [Var(v)]), Tag("Ok", [Lit("U64", "0")])),
        ])
        stmts = [(v, None, Q(Var(p)))]
        if len(scope) > 1 and r.random() < 0.4:
            w = self.fresh("v")
            stmts.append((w, None, Q(Var(r.choice([s for s in scope if s != p])))))
        return Block(stmts, tail)

    def library_fun(self, funs):
        r = self.r
        name = self.fresh("f")
        params = [self.fresh("x") for _ in range(r.choice([1, 1, 2]))]
        gl = list(getattr(self, "globals", []))
        if r.random() < 0.2:
            body = self.q_body(params, funs)
        else:
            body = self.expr(params + gl, r.randint(1, 3), funs)
        return Def(name, None, Lam([PVar(p) for p in params], body)), len(params)

    def use_site(self, funs, inf=None, types=None):
        r = self.r
        f, arity = r.choice([x for x in funs if x[0] != "e!"] or funs)
        name = self.fresh("u")
        ft = find(types[f]) if types and f in types else None
        if ft is not None and ft.kind == "fun":
            ft = inf.instantiate(ft)
            args = [self.value(inf, a, 2) for a in ft.args]
        else:
            args = [self.expr([], 1, []) for _ in range(arity)]
        call = Call(Var(f), args)
        if r.random() < 0.35:
            rt = find(ft.ret) if ft is not None and ft.kind == "fun" else None
            body = Match(call, self.consumer_branches(inf, rt))
        else:
            body = call
        return Def(name, None, Lam([PVar("_u")], body))

    def value(self, inf, t, depth):
        """A small value of reference type t; mutates with low probability."""
        r = self.r
        t = find(t)
        bad = r.random() < 0.08
        if t.kind == "var" or depth <= 0:
            return r.choice([Lit("U64", "1"), Tag(r.choice(TAGS), [])])
        if t.kind == "con":
            if t.con == "U64": return Tag("A", []) if bad else Lit("U64", str(r.randint(0, 9)))
            if t.con == "Bool": return BoolLit(r.random() < 0.5)
            if t.con == "List": return ListLit([self.value(inf, t.args[0], depth - 1) for _ in range(r.randint(0, 2))])
            if t.con == "Try":
                return Tag("Ok", [self.value(inf, t.args[0], depth - 1)]) if r.random() < 0.6 \
                    else Tag("Err", [self.value(inf, t.args[1], depth - 1)])
        if t.kind == "fun":
            ps = [PWild() for _ in t.args]
            return Lam(ps, self.value(inf, t.ret, depth - 1))
        if t.kind in ("tags", "etags"):
            tg, tail = inf.flat_tags(t)
            names = list(tg)
            if (tail.kind == "var" and r.random() < 0.3) or bad or not names:
                n = r.choice(TAGS)
                if n in tg:
                    return Tag(n, [self.value(inf, x, depth - 1) for x in tg[n]])
                return Tag(n, [])
            n = r.choice(names)
            return Tag(n, [self.value(inf, x, depth - 1) for x in tg[n]])
        if t.kind in ("rec", "erec"):
            fs, tail = inf.flat_rec(t)
            vals = {k: self.value(inf, v, depth - 1) for k, v in fs.items()}
            if (tail.kind == "var" and r.random() < 0.3) or bad:
                vals[r.choice(["a", "b", "c", "d"])] = Lit("U64", "3")
            if not vals:
                vals = {"a": Lit("U64", "3")}
            return Rec(vals)
        return Lit("U64", "1")

    def consumer_branches(self, inf, rt):
        r = self.r
        names = []
        tg = {}
        if rt is not None and rt.kind == "tags":
            tg, tail = inf.flat_tags(rt)
            names = list(tg)
        if rt is not None and rt.kind == "con" and rt.con == "Try":
            tg = {"Ok": [None], "Err": [None]}
            names = ["Ok", "Err"]
        pats = []
        for n in names:
            if r.random() < 0.8:
                pats.append(PTag(n, [PWild() for _ in tg[n]]))
        if r.random() < 0.3:
            extra = r.choice(TAGS)
            if extra not in names:
                pats.append(PTag(extra, []))
        if not pats or r.random() < 0.3:
            pats.append(PWild())
        res = r.choice([Lit("U64", "1"), Tag("R", [])])
        return [(p, res if not isinstance(res, Lit) else Lit("U64", str(i))) for i, p in enumerate(pats)]

    def module(self):
        r = self.r
        defs, funs = [], []
        self.globals = []
        if r.random() < 0.4:
            # R-EffectSlot fragment: one effectful primitive
            defs.append(Def("e!", AFun([ACon("U64", [])], ACon("U64", []), True), Lam([PVar("n")], Var("n"))))
            funs.append(("e!", 1))
        # top-level weak values (R-ValueRowBound, R-TopValueClose)
        for _ in range(r.choice([0, 0, 1, 2])):
            k = self.fresh("k")
            rhs = self.expr([], 2, [])
            ann = None
            if isinstance(rhs, Tag) and r.random() < 0.3:
                extra = {t: [] for t in r.sample(TAGS, 2)}
                if not rhs.args:
                    extra[rhs.name] = []
                    ann = ATags(extra, None)
            defs.append(Def(k, ann, rhs))
            self.globals.append(k)
        for _ in range(r.randint(1, 3)):
            d, ar = self.library_fun(funs)
            defs.append(d)
            funs.append((d.name, ar))
        c = Checker(); env = {}; types = {}
        try:
            for d in defs:
                types[d.name] = c.check_def(d.name, d.ann, d.expr, env, top=True)
        except TypeError_:
            types = None  # the library rejects: use sites get untyped arguments
        for _ in range(r.randint(1, 3)):
            defs.append(self.use_site(funs, c.inf, types))
        if r.random() < 0.3:
            # a late top-level value calling a library function (R-EffectfulValue, R-ValueRowBound)
            u = self.use_site(funs, c.inf, types)
            defs.append(Def(self.fresh("kv"), None, u.expr.body))
        return defs


# ---------------- reference type -> annotation (for metamorphic variants)
def type_to_ann(inf, t, names=None):
    """Print a reference type as an annotation that means the same thing
    under R-Polarity: hidden positive exts become extensionless unions.
    `names` is shared between an outer annotation and its locals' (R-ScopedTypeVars)."""
    pr = Printer(inf)
    pr.counts = {}
    pr.count(t, pr.counts, set())
    if names is None:
        names = {}

    def nm(v):
        if v.uid not in names:
            names[v.uid] = "abcdefghijklmnopqrstuvwxyz"[len(names) % 26] + ("" if len(names) < 26 else str(len(names)))
        return names[v.uid]

    def go(t, pos):
        t = find(t)
        if t.kind == "var":
            return AVar(nm(t))
        if t.kind == "con":
            return ACon(t.con, [go(a, pos) for a in t.args])
        if t.kind == "fun":
            return AFun([go(a, not pos) for a in t.args], go(t.ret, pos))
        if t.kind in ("tags", "etags"):
            tg, tail = inf.flat_tags(t)
            body = {k: [go(x, pos) for x in tg[k]] for k in sorted(tg)}
            if tail.kind == "var":
                if pos and pr.counts.get(tail.uid, 0) <= 1 and tg:
                    return ATags(body, None)
                return ATags(body, ".." + nm(tail) if pr.counts.get(tail.uid, 0) > 1 else "..")
            # a closed positive row spells as the extensionless union, which
            # R-Polarity opens: the annotation is then at least as general
            return ATags(body, None)
        if t.kind in ("rec", "erec"):
            fs, tail = inf.flat_rec(t)
            body = {k: go(fs[k], pos) for k in sorted(fs)}
            if tail.kind == "var":
                return ARec(body, ".." + nm(tail) if pr.counts.get(tail.uid, 0) > 1 else "..")
            return ARec(body, None)
        raise AssertionError(t.kind)

    return go(t, True)


def subterms(a, path=()):
    yield path, a
    if isinstance(a, ACon):
        for i, x in enumerate(a.args): yield from subterms(x, path + (i,))
    elif isinstance(a, AFun):
        for i, x in enumerate(a.args): yield from subterms(x, path + (i,))
        yield from subterms(a.ret, path + ("ret",))
    elif isinstance(a, ATags):
        for k, v in a.tags.items():
            for i, x in enumerate(v): yield from subterms(x, path + ((k, i),))
    elif isinstance(a, ARec):
        for k, v in a.fields.items(): yield from subterms(v, path + (k,))


def replace(a, path, new):
    if not path:
        return new
    h, rest = path[0], path[1:]
    if isinstance(a, ACon):
        args = list(a.args); args[h] = replace(args[h], rest, new); return ACon(a.name, args)
    if isinstance(a, AFun):
        if h == "ret": return AFun(a.args, replace(a.ret, rest, new))
        args = list(a.args); args[h] = replace(args[h], rest, new); return AFun(args, a.ret)
    if isinstance(a, ATags):
        k, i = h
        tg = {kk: list(v) for kk, v in a.tags.items()}
        tg[k][i] = replace(tg[k][i], rest, new)
        return ATags(tg, a.ext)
    if isinstance(a, ARec):
        fs = dict(a.fields); fs[h] = replace(fs[h], rest, new); return ARec(fs, a.ext)
    raise AssertionError(a)


def local_lambdas(e, out):
    """Names of unannotated local lambda definitions inside e."""
    if isinstance(e, Block):
        for n, a, r in e.stmts:
            if a is None and isinstance(r, Lam):
                out.append(n)
            local_lambdas(r, out)
        local_lambdas(e.body, out)
    elif isinstance(e, Lam): local_lambdas(e.body, out)
    elif isinstance(e, (Call,)):
        local_lambdas(e.f, out); [local_lambdas(a, out) for a in e.args]
    elif isinstance(e, If): [local_lambdas(x, out) for x in (e.c, e.t, e.e)]
    elif isinstance(e, Match):
        local_lambdas(e.e, out); [local_lambdas(b, out) for _, b in e.branches]
    elif isinstance(e, Tag): [local_lambdas(a, out) for a in e.args]
    elif isinstance(e, Rec): [local_lambdas(a, out) for a in e.fields.values()]
    elif isinstance(e, ListLit): [local_lambdas(a, out) for a in e.items]
    elif isinstance(e, (Access, Q)): local_lambdas(e.e, out)
    return out


def annotate_locals(e, anns):
    R = lambda x: annotate_locals(x, anns)
    if isinstance(e, Block):
        return Block([(n, anns.get(n, a), R(r)) for n, a, r in e.stmts], R(e.body))
    if isinstance(e, Lam): return Lam(e.params, R(e.body))
    if isinstance(e, Call): return Call(R(e.f), [R(a) for a in e.args])
    if isinstance(e, If): return If(R(e.c), R(e.t), R(e.e))
    if isinstance(e, Match): return Match(R(e.e), [(p, R(b)) for p, b in e.branches])
    if isinstance(e, Tag): return Tag(e.name, [R(a) for a in e.args])
    if isinstance(e, Rec): return Rec({k: R(v) for k, v in e.fields.items()})
    if isinstance(e, ListLit): return ListLit([R(a) for a in e.items])
    if isinstance(e, Access): return Access(R(e.e), e.f)
    if isinstance(e, Q): return Q(R(e.e))
    return e
