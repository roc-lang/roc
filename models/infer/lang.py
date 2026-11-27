"""Core-fragment AST, Roc source printer, and the inference judgment over it."""
from __future__ import annotations
from dataclasses import dataclass
from typing import Optional
from ref import Infer, TypeError_, find, T, GENERIC

# ---------------------------------------------------------------- AST
@dataclass
class Var: name: str
@dataclass
class Lit: ty: str; val: str            # ty is always U64 in the fragment
@dataclass
class BoolLit: val: bool
@dataclass
class Tag: name: str; args: list
@dataclass
class Rec: fields: dict
@dataclass
class Access: e: object; f: str
@dataclass
class Lam: params: list; body: object   # params: list of pattern
@dataclass
class Call: f: object; args: list
@dataclass
class If: c: object; t: object; e: object
@dataclass
class Match: e: object; branches: list  # [(pat, body)]
@dataclass
class Block: stmts: list; body: object  # [(name, ann|None, expr)]
@dataclass
class Q: e: object                      # e?
@dataclass
class ListLit: items: list

# patterns
@dataclass
class PVar: name: str
@dataclass
class PWild: pass
@dataclass
class PTag: name: str; args: list

# annotations
@dataclass
class AVar: name: str
@dataclass
class AHole: pass
@dataclass
class ACon: name: str; args: list
@dataclass
class AFun:
    args: list
    ret: object
    eff: bool = False
@dataclass
class ATags: tags: dict; ext: Optional[str]    # None | ".." | "..r"
@dataclass
class ARec: fields: dict; ext: Optional[str]

@dataclass
class Def:
    name: str
    ann: object
    expr: object

# ---------------------------------------------------------------- printers
def pp_pat(p):
    if isinstance(p, PVar): return p.name
    if isinstance(p, PWild): return "_"
    if isinstance(p, PTag):
        return p.name if not p.args else "%s(%s)" % (p.name, ", ".join(pp_pat(a) for a in p.args))
    raise AssertionError(p)


def pp(e, ind=0):
    I = "    " * ind
    if isinstance(e, Var): return e.name
    if isinstance(e, Lit): return "%s.%s" % (e.val, e.ty)
    if isinstance(e, BoolLit): return "Bool.True" if e.val else "Bool.False"
    if isinstance(e, Tag):
        return e.name if not e.args else "%s(%s)" % (e.name, ", ".join(pp(a, ind) for a in e.args))
    if isinstance(e, Rec): return "{ %s }" % ", ".join("%s: %s" % (k, pp(v, ind)) for k, v in e.fields.items())
    if isinstance(e, Access): return "%s.%s" % (pp_atom(e.e, ind), e.f)
    if isinstance(e, Lam): return "|%s| %s" % (", ".join(pp_pat(p) for p in e.params), pp(e.body, ind))
    if isinstance(e, Call): return "%s(%s)" % (pp_atom(e.f, ind), ", ".join(pp(a, ind) for a in e.args))
    if isinstance(e, If): return "if %s %s else %s" % (pp_atom(e.c, ind), pp_atom(e.t, ind), pp_atom(e.e, ind))
    if isinstance(e, Match):
        return "match %s { %s }" % (pp_atom(e.e, ind), ", ".join("%s => %s" % (pp_pat(p), pp_atom(b, ind)) for p, b in e.branches))
    if isinstance(e, Q): return "%s?" % pp_atom(e.e, ind)
    if isinstance(e, ListLit): return "[%s]" % ", ".join(pp(a, ind) for a in e.items)
    if isinstance(e, Block):
        J = "    " * (ind + 1)
        lines = ["{"]
        for name, ann, rhs in e.stmts:
            if ann is not None:
                lines.append(J + "%s : %s" % (name, pp_ann(ann)))
            lines.append(J + "%s = %s" % (name, pp(rhs, ind + 1)))
        lines.append(J + pp(e.body, ind + 1))
        lines.append(I + "}")
        return "\n".join(lines)
    raise AssertionError(e)


def pp_atom(e, ind):
    s = pp(e, ind)
    if isinstance(e, (Lam, If, Match)):
        return "(" + s + ")"
    return s


def pp_ann(a, fun_arg=False):
    if isinstance(a, AVar): return a.name
    if isinstance(a, AHole): return "_"
    if isinstance(a, ACon): return a.name if not a.args else "%s(%s)" % (a.name, ", ".join(pp_ann(x) for x in a.args))
    if isinstance(a, AFun):
        s = "%s %s %s" % (", ".join(pp_ann(x, True) for x in a.args), "=>" if a.eff else "->", pp_ann(a.ret, True))
        return "(" + s + ")" if fun_arg else s
    if isinstance(a, ATags):
        items = [k if not v else "%s(%s)" % (k, ", ".join(pp_ann(x) for x in v)) for k, v in a.tags.items()]
        if a.ext: items.append(a.ext)
        return "[" + ", ".join(items) + "]"
    if isinstance(a, ARec):
        items = ["%s: %s" % (k, pp_ann(v)) for k, v in a.fields.items()]
        if a.ext: items.append(a.ext)
        return "{ " + ", ".join(items) + " }" if items else "{}"
    raise AssertionError(a)


def pp_module(defs, header="H := [H].{}"):
    out = [header, ""]
    for d in defs:
        if d.ann is not None:
            out.append("%s : %s" % (d.name, pp_ann(d.ann)))
        out.append("%s = %s" % (d.name, pp(d.expr, 0)))
        out.append("")
    return "\n".join(out)

# ---------------------------------------------------------------- inference

def is_generalizing(e):
    """R-GenByExpr (design.md 'Value Bindings Generalize By Expression')."""
    return isinstance(e, (Lam, Var))


def ann_introduces_vars(a, outer=frozenset()):
    """R-ValueAnn: does the annotation introduce a type variable (named or `..`)?
    A name an enclosing annotation introduced is looked up, not introduced."""
    f = lambda x: ann_introduces_vars(x, outer)
    if isinstance(a, AVar): return a.name not in outer
    if isinstance(a, (AHole,)): return False
    if isinstance(a, ACon): return any(f(x) for x in a.args)
    if isinstance(a, AFun): return any(f(x) for x in a.args) or f(a.ret)
    if isinstance(a, (ATags, ARec)):
        if a.ext == "..": return True
        if a.ext and a.ext[2:] not in outer: return True
        vals = a.tags.values() if isinstance(a, ATags) else [[v] for v in a.fields.values()]
        return any(f(x) for v in vals for x in v)
    return False


def has_q(e):
    if isinstance(e, Q): return True
    if isinstance(e, Lam): return False
    for v in getattr(e, "__dict__", {}).values():
        for x in (v if isinstance(v, list) else v.values() if isinstance(v, dict) else [v]):
            for y in (x if isinstance(x, tuple) else [x]):
                if hasattr(y, "__dataclass_fields__") and has_q(y): return True
    return False


def pat_names(p):
    if isinstance(p, PVar): return [p.name]
    if isinstance(p, PTag): return [n for a in p.args for n in pat_names(a)]
    return []


def is_known(e, env):
    """Compile-time-known: no lambda-bound or runtime-pattern-bound name occurs free."""
    rt = env.get("__rt", frozenset())
    def go(e, bound):
        if isinstance(e, Var): return e.name in bound or e.name not in rt
        if isinstance(e, (Lit, BoolLit)): return True
        if isinstance(e, Tag): return all(go(a, bound) for a in e.args)
        if isinstance(e, Rec): return all(go(a, bound) for a in e.fields.values())
        if isinstance(e, Access): return go(e.e, bound)
        if isinstance(e, ListLit): return all(go(a, bound) for a in e.items)
        if isinstance(e, Lam): return True
        if isinstance(e, Call): return go(e.f, bound) and all(go(a, bound) for a in e.args)
        if isinstance(e, If): return go(e.c, bound) and go(e.t, bound) and go(e.e, bound)
        if isinstance(e, Match):
            return go(e.e, bound) and all(go(b, bound | set(pat_names(p))) for p, b in e.branches)
        if isinstance(e, Block):
            b2 = set(bound)
            for n, _, r in e.stmts:
                if not go(r, b2): return False
                b2.add(n)
            return go(e.body, b2)
        return False
    return go(e, set())


class Checker:
    def __init__(self):
        self.inf = Infer()
        self.errors = []
        self.ann_scope = {}     # R-ScopedTypeVars: names enclosing annotations introduced
        self.def_types = {}
        self.slots = [[]]       # R-EffectSlot: effect dependency lists, innermost last
        self.value_slots = []   # (name, deps) of top-level values
        self.lambda_kinds = []  # R-EffectFinalize: source lambdas' kinds
        self.escaped_bounds = []  # R-DeferredClose, one list per enclosing generalizing def

    # -------- annotations (R-Polarity)
    def ann_type(self, a, env_vars, pos, opened, mode):
        inf = self.inf
        if isinstance(a, AHole):
            return inf.fresh()
        if isinstance(a, AVar):
            if a.name not in env_vars:
                env_vars[a.name] = inf.fresh(rigid=(mode == "fun"), name=a.name)
            return env_vars[a.name]
        if isinstance(a, ACon):
            if a.name in ("U64", "Bool"):
                return inf.con(a.name)
            if a.name == "Try":
                return inf.try_(*[self.ann_type(x, env_vars, pos, opened, mode) for x in a.args])
            if a.name == "List":
                return inf.con("List", self.ann_type(a.args[0], env_vars, pos, opened, mode))
            raise AssertionError(a.name)
        if isinstance(a, AFun):
            return inf.fun([self.ann_type(x, env_vars, not pos, opened, mode) for x in a.args],
                           self.ann_type(a.ret, env_vars, pos, opened, mode),
                           "eff" if a.eff else "pure")
        if isinstance(a, ATags):
            tg = {k: [self.ann_type(x, env_vars, pos, opened, mode) for x in v] for k, v in a.tags.items()}
            if a.ext is None:
                if pos and tg and mode != "host":
                    ext = inf.fresh(bounded=True)  # R-Polarity: implicitly open, bounded
                    opened.append(ext)
                else:
                    ext = inf.etags
            elif a.ext == ".." and pos and tg and mode != "host":
                # R-Polarity: an explicit `..` in an output position means
                # exactly what its absence means
                ext = inf.fresh(bounded=True)
                opened.append(ext)
            elif a.ext == "..":
                ext = inf.fresh(rigid=(mode == "fun"))
            else:
                nm = a.ext[2:]
                if nm not in env_vars:
                    env_vars[nm] = inf.fresh(rigid=(mode == "fun"), name=nm)
                ext = env_vars[nm]
            return inf.tags(tg, ext)
        if isinstance(a, ARec):
            fs = {k: self.ann_type(v, env_vars, pos, opened, mode) for k, v in a.fields.items()}
            if a.ext is None:
                ext = inf.erec
            elif a.ext == "..":
                ext = inf.fresh(rigid=(mode == "fun"))
            else:
                nm = a.ext[2:]
                if nm not in env_vars:
                    env_vars[nm] = inf.fresh(rigid=(mode == "fun"), name=nm)
                ext = env_vars[nm]
            return inf.rec(fs, ext)
        raise AssertionError(a)

    # -------- patterns (R-MatchClose)
    def pattern_type(self, pats):
        """Type of a set of patterns at one position, jointly."""
        inf = self.inf
        catch = any(isinstance(p, (PVar, PWild)) for p in pats)
        tagps = [p for p in pats if isinstance(p, PTag)]
        if not tagps:
            return inf.fresh()
        by = {}
        for p in tagps:
            by.setdefault(p.name, []).append(p)
        tg = {}
        for name, ps in by.items():
            ar = len(ps[0].args)
            if any(len(q.args) != ar for q in ps):
                raise TypeError_("pattern arity")
            # children of catch-all branches cover every payload
            tg[name] = [self.pattern_type([q.args[i] for q in ps] + ([PWild()] if catch else []))
                        for i in range(ar)]
        return inf.tags(tg, inf.fresh() if catch else inf.etags)

    def bind_pattern(self, p, t, env):
        inf = self.inf
        if isinstance(p, PVar):
            env[p.name] = ("mono", t)
        elif isinstance(p, PWild):
            pass
        elif isinstance(p, PTag):
            tg, _ = inf.flat_tags(t)
            pay = tg[p.name]
            for q, pt in zip(p.args, pay):
                self.bind_pattern(q, pt, env)

    # -------- expressions
    def lookup(self, env, name):
        kind, t = env[name]
        if kind == "poly":
            return self.inf.instantiate(t)
        return t

    def infer(self, e, env):
        inf = self.inf
        if isinstance(e, Var):
            return self.lookup(env, e.name)
        if isinstance(e, Lit):
            return inf.con(e.ty)
        if isinstance(e, BoolLit):
            return inf.con("Bool")
        if isinstance(e, Tag):
            return inf.tags({e.name: [self.infer(a, env) for a in e.args]}, inf.fresh())
        if isinstance(e, Rec):
            return inf.rec({k: self.infer(v, env) for k, v in e.fields.items()}, inf.erec)
        if isinstance(e, Access):
            t = self.infer(e.e, env)
            r = inf.fresh()
            inf.unify(t, inf.rec({e.f: r}, inf.fresh()))
            return r
        if isinstance(e, ListLit):
            el = inf.fresh()
            for it in e.items:
                inf.unify(self.infer(it, env), el)
            return inf.con("List", el)
        if isinstance(e, Lam):
            return self.infer_lam(e, env, None)
        if isinstance(e, Call):
            ft = self.infer(e.f, env)
            ats = [self.infer(a, env) for a in e.args]
            r = inf.fresh()
            k = inf.fresh()
            inf.unify(ft, inf.fun(ats, r, k))
            # R-EffectSlot: a call adds the callee's kind to the active slot
            self.slots[-1].append(k)
            return r
        if isinstance(e, If):
            inf.unify(self.infer(e.c, env), inf.con("Bool"))
            a = self.infer(e.t, env)
            b = self.infer(e.e, env)
            inf.unify(a, b)
            return a
        if isinstance(e, Match):
            st = self.infer(e.e, env)
            pt = self.pattern_type([p for p, _ in e.branches])
            known = is_known(e.e, env)
            try:
                inf.unify(st, pt)
            except TypeError_ as ex:
                if known:
                    # R-KnownScrutinee: Roc checks a compile-time-known scrutinee
                    # against its value; outside the fragment
                    raise TypeError_(str(ex), "known-scrutinee")
                raise
            r = inf.fresh()
            for p, b in e.branches:
                env2 = dict(env)
                self.bind_pattern(p, pt, env2)
                if not known:
                    env2["__rt"] = env.get("__rt", frozenset()) | frozenset(pat_names(p))
                inf.unify(self.infer(b, env2), r)
            return r
        if isinstance(e, Q):
            if not inf.question:
                raise TypeError_("? outside lambda", "question")
            t = self.infer(e.e, env)
            ok, err = inf.fresh(), inf.fresh()
            inf.unify(t, inf.try_(ok, err))
            inf.question[-1].append(err)
            return ok
        if isinstance(e, Block):
            env2 = dict(env)
            for name, ann, rhs in e.stmts:
                self.check_def(name, ann, rhs, env2, top=False)
                if not is_known(rhs, env2):
                    env2["__rt"] = env2.get("__rt", frozenset()) | {name}
            return self.infer(e.body, env2)
        raise AssertionError(e)

    def infer_lam(self, e, env, ann_fun):
        """ann_fun: the annotation's function type when the lambda is the RHS
        of an annotated definition (R-SeedParams, R-Subsume)."""
        inf = self.inf
        env2 = dict(env)
        env2["__rt"] = env.get("__rt", frozenset()) | frozenset(n for p in e.params for n in pat_names(p))
        pts = []
        for i, p in enumerate(e.params):
            pt = self.pattern_type([p])
            if ann_fun is not None:
                inf.unify(pt, ann_fun.args[i])
            self.bind_pattern(p, pt, env2)
            pts.append(pt)
        inf.question.append([])
        kappa = inf.fresh()
        self.lambda_kinds.append(kappa)
        self.slots.append(kappa.args)
        try:
            if ann_fun is not None and not has_q(e.body):
                body = self.infer_result(e.body, env2, ann_fun.ret)
            else:
                body = self.infer(e.body, env2)
        finally:
            self.slots.pop()
        contribs = inf.question.pop()
        ann_ret = ann_fun.ret if ann_fun is not None else None
        if contribs:
            ret = self.compose_try(body, contribs, ann_ret)
        elif ann_ret is not None:
            # R-Subsume is applied per result-position leaf (see infer_result)
            ret = ann_ret
        else:
            ret = body
        if inf.is_eff(kappa):
            # R-EffectFinalize: positive effectfulness is final at once
            find(kappa).link = T("eeff")
        return inf.fun(pts, ret, kappa)

    def infer_result(self, e, env, target):
        """R-Subsume per leaf: a branch of an `if`/`match` or a block's final
        value in result position relates to the annotated result through its
        own widening copy (design.md Row Subsumption)."""
        inf = self.inf
        if isinstance(e, If):
            inf.unify(self.infer(e.c, env), inf.con("Bool"))
            self.infer_result(e.t, env, target)
            self.infer_result(e.e, env, target)
            return target
        if isinstance(e, Match):
            st = self.infer(e.e, env)
            pt = self.pattern_type([p for p, _ in e.branches])
            known = is_known(e.e, env)
            try:
                inf.unify(st, pt)
            except TypeError_ as ex:
                if known:
                    raise TypeError_(str(ex), "known-scrutinee")
                raise
            for p, b in e.branches:
                env2 = dict(env)
                self.bind_pattern(p, pt, env2)
                if not known:
                    env2["__rt"] = env.get("__rt", frozenset()) | frozenset(pat_names(p))
                self.infer_result(b, env2, target)
            return target
        if isinstance(e, Block):
            env2 = dict(env)
            for name, ann, rhs in e.stmts:
                self.check_def(name, ann, rhs, env2, top=False)
                if not is_known(rhs, env2):
                    env2["__rt"] = env2.get("__rt", frozenset()) | {name}
            return self.infer_result(e.body, env2, target)
        t = self.infer(e, env)
        inf.unify(inf.widen_copy(t), target)
        return target

    def compose_try(self, body, contribs, ann_ret):
        """R-TryCompose (design.md 'Try Return-Row Composition')."""
        inf = self.inf
        if ann_ret is not None:
            res = ann_ret
            ok_b, err_b = inf.fresh(), inf.fresh()
            inf.unify(body, inf.try_(ok_b, err_b))
            ok_r, err_r = inf.fresh(), inf.fresh()
            inf.unify(res, inf.try_(ok_r, err_r))
            inf.unify(ok_b, ok_r)
        else:
            ok_b, err_b = inf.fresh(), inf.fresh()
            inf.unify(body, inf.try_(ok_b, err_b))
            err_r = inf.fresh()
            res = inf.try_(ok_b, err_r)
        all_c = [err_b] + contribs
        tagless = []
        withtags = []
        for c in all_c:
            tg, tail = inf.flat_tags(c) if find(c).kind == "tags" else ({}, find(c))
            if tail.kind == "etags":
                # closed: heads only, fresh spine sharing payloads
                if tg:
                    inf.unify(inf.tags(tg, inf.fresh()), err_r)
                else:
                    pass
            elif tg:
                withtags.append(c)
            else:
                tagless.append(c)
        for c in withtags:
            inf.unify(c, err_r)
        for c in tagless:
            if ann_ret is not None:
                _, rtail = inf.flat_tags(err_r) if find(err_r).kind == "tags" else ({}, find(err_r))
                if find(c) is rtail:
                    continue  # explicitly shared residual tail: included as is
                inf.unify(c, err_r)
            else:
                _, tail = inf.flat_tags(err_r) if find(err_r).kind == "tags" else ({}, find(err_r))
                inf.unify(c, tail)
        return res

    def check_def(self, name, ann, rhs, env, top):
        if top and not is_generalizing(rhs):
            self.slots.append([])
            try:
                return self.check_def1(name, ann, rhs, env, top)
            finally:
                self.value_slots.append((name, self.slots.pop()))
        return self.check_def1(name, ann, rhs, env, top)

    def check_def1(self, name, ann, rhs, env, top):
        inf = self.inf
        gen = is_generalizing(rhs)
        if ann is not None and not gen and ann_introduces_vars(ann, frozenset(self.ann_scope)):
            raise TypeError_("polymorphic value annotation", "value-ann")
        if gen:
            inf.level += 1
            self.escaped_bounds.append([])
        opened = []
        if ann is not None:
            env_vars = dict(self.ann_scope)
            at = self.ann_type(ann, env_vars, True, opened, "fun" if gen else "value")
            saved = self.ann_scope
            self.ann_scope = env_vars
            try:
                if isinstance(rhs, Lam) and find(at).kind == "fun":
                    lt = self.infer_lam(rhs, env, find(at))
                    inf.unify(lt, at)
                elif isinstance(rhs, Lam):
                    raise TypeError_("lambda annotated with non-function", "ann")
                else:
                    t = self.infer(rhs, env)
                    inf.unify(t, at)
            finally:
                self.ann_scope = saved
            t = at
            # R-ValueRowBound: an annotated value's rows are the annotation's and
            # no use widens them (design.md Value Rows; the Polarity table's
            # LOCAL VALUE row says uses may widen -- the compiler follows Value Rows)
        else:
            t = self.infer(rhs, env)
        self.def_types[name] = t
        if gen:
            inf.level -= 1
            # R-DeferredClose: a bounded row an inner annotation shares with a
            # variable this definition owns takes the annotation's row, closed,
            # at this boundary (design.md Row Subsumption, deferred relations)
            for x in self.escaped_bounds.pop():
                x = find(x)
                if x.kind == "var" and not x.rigid and x.bounded and x.level != GENERIC and x.level > inf.level:
                    x.link = inf.etags
            inf.generalize(t)
            self.check_acyclic(t)
            env[name] = ("poly", t)
            if self.escaped_bounds:
                for x in opened:
                    x = find(x)
                    if x.kind == "var" and x.level != GENERIC:
                        self.escaped_bounds[-1].append(x)
        else:
            env[name] = ("mono", t)
            if top:
                inf.top_value_types.append(t)
        return t

    def check_acyclic(self, t):
        """R-NoAnonRecursion: a structural type may not contain itself."""
        inf = self.inf
        done = set()
        def go(t, path):
            t = find(t)
            if t.uid in path:
                raise TypeError_("anonymous recursion", "occurs")
            if t.uid in done:
                return
            path.add(t.uid)
            for c in inf.children(t):
                go(c, path)
            path.discard(t.uid)
            done.add(t.uid)
        go(t, set())

    def close_top_values(self):
        """R-TopValueClose: every still-open tag row of a top-level value closes.
        R-EffectfulValue: a top-level value's slot must settle pure."""
        inf = self.inf
        for name, deps in self.value_slots:
            if any(inf.is_eff(d) for d in deps):
                raise TypeError_("effectful top level value " + name, "effect")
        for k in self.lambda_kinds:
            k = find(k)
            if k.kind == "var" and inf.is_eff(k):
                k.link = T("eeff")
        seen = set()

        def walk(t):
            t = find(t)
            if t.uid in seen:
                return
            seen.add(t.uid)
            if t.kind == "tags":
                tg, tail = inf.flat_tags(t)
                if tail.kind == "var" and not tail.rigid and tail.level != GENERIC:
                    tail.link = inf.etags
                for v in tg.values():
                    for x in v:
                        walk(x)
                return
            for c in inf.children(t):
                walk(c)
        for t in inf.top_value_types:
            walk(t)

