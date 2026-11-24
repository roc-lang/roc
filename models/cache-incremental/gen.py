"""Program generator, reference evaluator and emitter for the cache-incremental model.

The expression language, generator and reference evaluator come from the backend-diff model
(`models/backend-diff/gen.py` on the backend-diff-model branch). This copy adds what edits need to reach:

- a path package `lib/` holding the lowest modules, imported by the app as `lib.M<i>`;
- the record and tag union as nominal types (`Rec`, `Tag`) declared in their own type modules, so an edit to their
  fields or tags changes no importer's source;
- top-level constants (evaluated at compile time), private definitions, unannotated definitions and bindings;
- top-level `expect`s for `roc test`;
- per-file comment and blank-line noise, unused imports, and an injected type error.

A program is a value; `edit.py` changes it, `Emit` writes it as files, `evaluate` computes its expected output.
Nothing here is a handwritten scenario: every definition, expression and edit is composed at random.
"""
import random
import re

I, U, S, B, L, R, T, F = "I64", "U64", "Str", "Bool", "List", "Rec", "Tag", "Fn"
VALUE_TYPES = (I, S, B, L, R, T, F)
ROC_TYPE = {
    I: "I64", U: "U64", S: "Str", B: "Bool", L: "List(I64)",
    R: "Rec", T: "Tag", F: "(I64 -> I64)", "gen": "g",
    "A": "a", "FA": "(a -> a)", "LA": "List(a)",
}
POLY_INST = (I, S, B, L, R, T)
EXPECT_TYPES = (I, S, B)
I64_MIN, I64_MAX = -(1 << 63), (1 << 63) - 1
WORDS = ["", "a", "b", "xy", "ok", "rc", "zz", "hello", "q r"]
# Fields and tags an edit can add to Rec and Tag. Nothing reads or builds them, so they change layouts, never values.
REC_EXTRA = [("c", I, "7.I64"), ("d", S, '"dd"'), ("e", B, "Bool.True"), ("aa", L, "[1.I64, 2.I64]")]
TAG_EXTRA = [("X", I), ("A0", None), ("Zq", S), ("Bb", L)]


class Overflow(Exception):
    """The reference left I64 range or ran too long; the program is discarded, never compared."""


# --------------------------------------------------------------------------------------------- program values

class Def:
    """A top-level definition. kind: plain, mk (returns a closure), hof (takes one), tail, nontail, group, poly,
    const (no parameters: evaluated at compile time)."""

    def __init__(self, name, kind, params, ret, module):
        self.name, self.kind, self.params, self.ret, self.module = name, kind, params, ret, module
        self.body = None
        self.group = None    # names of the recursive group's members
        self.private = False  # outside the type module's block, or left out of a header's exposing list
        self.ann = True       # written with a type annotation


class Expect:
    def __init__(self, module, ty, expr, want):
        self.module, self.ty, self.expr, self.want = module, ty, expr, want


class Program:
    def __init__(self, nmods, npkg):
        self.nmods = nmods
        self.npkg = npkg            # modules 0..npkg-1 live in the package lib/
        self.defs = []
        self.outputs = []           # (type, expr) printed by main!
        self.forms = []             # per library module: "type" or "header"
        self.expects = []
        self.rec_fields = [("a", I, None), ("b", S, None)]   # (name, type, value mk stores, None for a and b)
        self.tag_tags = [("P", I), ("Q", S), ("W", None)]
        self.noise = {}             # file name -> int; 0 means no comments or blank lines
        self.extra_imports = {}     # module -> set of lower modules imported though unused
        self.error = None           # name of a def whose body is wrapped in a type error

    def by_name(self):
        return {d.name: d for d in self.defs}

    def in_pkg(self, m):
        return m < self.npkg


# --------------------------------------------------------------------------------------------- generator

class Gen:
    def __init__(self, seed, max_mods=5, counter_base=0):
        self.r = random.Random(seed)
        self.n = counter_base
        self.max_mods = max_mods

    def fresh(self, p):
        self.n += 1
        return f"{p}{self.n}"

    # expressions ------------------------------------------------------------------------------------

    def lit(self, ty):
        r = self.r
        if ty == I:
            return ("lit", I, r.choice([0, 1, 2, 3, 5, 7, 10, 42, -1, -3, -8, 100]))
        if ty == S:
            return ("lit", S, r.choice(WORDS))
        if ty == B:
            return ("lit", B, r.random() < 0.5)
        if ty == L:
            return ("list", [self.lit(I) for _ in range(r.randint(0, 3))])
        if ty == R:
            return ("rec", self.lit(I), self.lit(S))
        if ty == T:
            k = r.choice("PQW")
            return ("tag", k, None if k == "W" else self.lit(I if k == "P" else S))
        if ty == F:
            p = self.fresh("p")
            return ("lam", p, ("bin", "+", ("var", p), self.lit(I)))
        raise ValueError(ty)

    def visible(self, ctx, d):
        """d can be referenced from ctx: an earlier def in a module at or below ctx's, and private only at home."""
        return d.module <= ctx["module"] and (not d.private or d.module == ctx["module"])

    def callable_defs(self, ctx, ty):
        inst = ty in POLY_INST or ty == "A"
        return [d for d in ctx["defs"] if self.visible(ctx, d) and (d.ret == ty or (d.ret == "A" and inst))]

    def counter(self, ctx):
        r = self.r
        k = r.randint(0, ctx.get("max_n", 6))
        base = ("ulit", k)
        if "zu" in ctx["uvars"] and r.random() < 0.8:
            return ("bin", "+", base, ("var", "zu"))
        if ctx["uvars"] and r.random() < 0.3:
            return ("var", r.choice(ctx["uvars"]))
        return base

    def call(self, d, ctx, depth, want=None):
        args = []
        sub = want if d.ret == "A" else None
        for pname, pty in d.params:
            if sub is not None and pty in ("A", "FA", "LA"):
                pty = {"A": sub, "FA": ("Fn", sub), "LA": ("List", sub)}[pty]
            if pty == U:
                args.append(self.counter(ctx))
            elif pty == "gen":
                args.append(self.expr(self.r.choice(VALUE_TYPES), ctx, depth - 1))
            else:
                args.append(self.expr(pty, ctx, depth - 1))
        return ("call", d.name, args)

    def expr(self, ty, ctx, depth):
        r = self.r
        if isinstance(ty, tuple) and ty[0] == "Fn":
            if ty[1] == "A":
                env = [v for v, t in ctx["env"] if t == "FA"]
                if env and (depth <= 0 or r.random() < 0.6):
                    return ("var", r.choice(env))
            elif ty[1] == I and r.random() < 0.3:
                return self.expr(F, ctx, depth)
            p = self.fresh("p")
            return ("lam", p, self.expr(ty[1], push(ctx, p, ty[1]), max(depth - 1, 0)))
        if isinstance(ty, tuple) and ty[0] == "List":
            if ty[1] == I:
                return self.expr(L, ctx, depth)
            ty = "LA" if ty[1] == "A" else ty
            if ty != "LA":
                return ("list", [self.expr(ty[1], ctx, depth - 1) for _ in range(r.randint(1, 3))])
        if ty in ("A", "LA"):
            return self.poly_expr(ty, ctx, depth)
        env = [v for v, t in ctx["env"] if t == ty]
        if depth <= 0:
            if env and r.random() < 0.7:
                return ("var", r.choice(env))
            return self.lit(ty)
        opts = ["lit", "var", "if", "let", "call", "call", "call", "match"]
        if ty == I:
            opts += ["bin", "bin", "rem", "field", "fold", "sum", "len", "slen", "apply"]
        elif ty == S:
            opts += ["concat", "tostr", "tostr", "join", "field"]
        elif ty == B:
            opts += ["cmp", "cmp", "streq", "logic"]
        elif ty == L:
            opts += ["listlit", "map", "keep", "append", "lconcat"]
        elif ty == R:
            opts += ["reclit"]
        elif ty == T:
            opts += ["taglit", "taglit"]
        elif ty == F:
            opts += ["lam", "lam", "fnref"]
        defs = self.callable_defs(ctx, ty)
        while True:
            k = r.choice(opts)
            if k == "lit":
                return self.lit(ty)
            if k == "var" and env:
                return ("var", r.choice(env))
            if k == "if":
                return ("if", self.expr(B, ctx, depth - 1), self.expr(ty, ctx, depth - 1), self.expr(ty, ctx, depth - 1))
            if k == "let":
                vt = r.choice(VALUE_TYPES)
                v = self.fresh("v")
                e1 = self.expr(vt, ctx, depth - 1)
                return ("let", v, e1, self.expr(ty, push(ctx, v, vt), depth - 1), vt, True)
            if k == "call" and defs:
                weights = [(3 if d.module != ctx["module"] else 1) * (3 if d.kind in ("tail", "nontail", "group") else 1)
                           * (2 if d.kind == "const" else 1) for d in defs]
                return self.call(r.choices(defs, weights)[0], ctx, depth, want=ty)
            if k == "match":
                p, q = self.fresh("n"), self.fresh("s")
                return ("match", self.expr(T, ctx, depth - 1), p, self.expr(ty, push(ctx, p, I), depth - 1),
                        q, self.expr(ty, push(ctx, q, S), depth - 1), self.expr(ty, ctx, depth - 1))
            if k == "bin":
                return ("bin", r.choice("+-*"), self.expr(I, ctx, depth - 1), self.expr(I, ctx, depth - 1))
            if k == "rem":
                return ("rem", self.expr(I, ctx, depth - 1), r.choice([2, 3, 7, 1000]))
            if k == "field":
                return ("field", self.expr(R, ctx, depth - 1), "a" if ty == I else "b")
            if k == "fold":
                s, x = self.fresh("s"), self.fresh("x")
                body = self.expr(I, push(push(ctx, s, I), x, I), depth - 1)
                return ("fold", self.expr(L, ctx, depth - 1), self.expr(I, ctx, depth - 1), s, x, body)
            if k == "sum":
                return ("sum", self.expr(L, ctx, depth - 1))
            if k == "len":
                return ("len", self.expr(L, ctx, depth - 1))
            if k == "slen":
                return ("slen", self.expr(S, ctx, depth - 1))
            if k == "apply":
                return ("apply", self.expr(F, ctx, depth - 1), self.expr(I, ctx, depth - 1))
            if k == "concat":
                return ("concat", self.expr(S, ctx, depth - 1), self.expr(S, ctx, depth - 1))
            if k == "tostr":
                return ("tostr", self.expr(I, ctx, depth - 1))
            if k == "join":
                return ("join", self.expr(L, ctx, depth - 1))
            if k == "cmp":
                return ("bin", r.choice(["<", "=="]), self.expr(I, ctx, depth - 1), self.expr(I, ctx, depth - 1))
            if k == "streq":
                return ("bin", "==", self.expr(S, ctx, depth - 1), self.expr(S, ctx, depth - 1))
            if k == "logic":
                return ("bin", r.choice(["and", "or"]), self.expr(B, ctx, depth - 1), self.expr(B, ctx, depth - 1))
            if k == "listlit":
                return ("list", [self.expr(I, ctx, depth - 1) for _ in range(r.randint(0, 3))])
            if k == "map":
                return ("map", self.expr(L, ctx, depth - 1), self.lam(ctx, depth - 1))
            if k == "keep":
                x = self.fresh("x")
                return ("keep", self.expr(L, ctx, depth - 1), x, self.expr(B, push(ctx, x, I), depth - 1))
            if k == "append":
                return ("append", self.expr(L, ctx, depth - 1), self.expr(I, ctx, depth - 1))
            if k == "lconcat":
                return ("lconcat", self.expr(L, ctx, depth - 1), self.expr(L, ctx, depth - 1))
            if k == "reclit":
                return ("rec", self.expr(I, ctx, depth - 1), self.expr(S, ctx, depth - 1))
            if k == "taglit":
                c = r.choice("PQW")
                return ("tag", c, None if c == "W" else self.expr(I if c == "P" else S, ctx, depth - 1))
            if k == "lam":
                return self.lam(ctx, depth - 1)
            if k == "fnref":
                fs = [d for d in ctx["defs"] if d.params and [t for _, t in d.params] == [I] and d.ret == I
                      and self.visible(ctx, d)]
                if fs:
                    return ("fnref", r.choice(fs).name)

    def poly_expr(self, ty, ctx, depth):
        r = self.r
        env = [v for v, t in ctx["env"] if t == ty]
        fs = [v for v, t in ctx["env"] if t == "FA"]
        if depth <= 0 or r.random() < 0.2:
            if env:
                return ("var", r.choice(env))
            if ty == "LA":
                return ("list", [self.poly_expr("A", ctx, 0)])
        while True:
            k = r.choice(["var", "apply", "if", "let", "call", "fold", "match"] if ty == "A"
                         else ["var", "list", "map", "append", "if", "let"])
            if k == "var" and env:
                return ("var", r.choice(env))
            if k == "apply" and fs:
                return ("apply", ("var", r.choice(fs)), self.poly_expr("A", ctx, depth - 1))
            if k == "if":
                return ("if", self.expr(B, ctx, depth - 1), self.poly_expr(ty, ctx, depth - 1),
                        self.poly_expr(ty, ctx, depth - 1))
            if k == "let":
                vt = r.choice(["A", "LA", I, S])
                v = self.fresh("v")
                e1 = self.expr(vt, ctx, depth - 1)
                return ("let", v, e1, self.poly_expr(ty, push(ctx, v, vt), depth - 1), vt, True)
            if k == "call":
                ds = [d for d in ctx["defs"] if d.ret == "A" and self.visible(ctx, d)]
                if ds:
                    return self.call(r.choice(ds), ctx, depth, want="A")
            if k == "fold":
                s_, x = self.fresh("s"), self.fresh("x")
                body = self.poly_expr("A", push(push(ctx, s_, "A"), x, "A"), depth - 1)
                return ("fold", self.poly_expr("LA", ctx, depth - 1), self.poly_expr("A", ctx, depth - 1), s_, x, body)
            if k == "match":
                p, q = self.fresh("n"), self.fresh("s")
                return ("match", self.expr(T, ctx, depth - 1), p, self.poly_expr(ty, push(ctx, p, I), depth - 1),
                        q, self.poly_expr(ty, push(ctx, q, S), depth - 1), self.poly_expr(ty, ctx, depth - 1))
            if k == "list":
                return ("list", [self.poly_expr("A", ctx, depth - 1) for _ in range(r.randint(1, 3))])
            if k == "map" and fs:
                return ("map", self.poly_expr("LA", ctx, depth - 1), ("var", r.choice(fs)))
            if k == "append":
                return ("append", self.poly_expr("LA", ctx, depth - 1), self.poly_expr("A", ctx, depth - 1))

    def lam(self, ctx, depth):
        p = self.fresh("p")
        return ("lam", p, self.expr(I, push(ctx, p, I), depth))

    # definitions ------------------------------------------------------------------------------------

    def value_params(self, n):
        return [(self.fresh("a"), self.r.choice(VALUE_TYPES)) for _ in range(n)]

    def body_ctx(self, defs, module, params):
        return {"defs": list(defs), "module": module, "env": [(p, t) for p, t in params if t not in (U, "gen")],
                "uvars": [p for p, t in params if t == U], "max_n": 4}

    def new_poly(self, defs, module):
        r = self.r
        params = [(self.fresh("x"), "A"), (self.fresh("f"), "FA")] + self.value_params(r.randint(0, 1))
        if r.random() < 0.3:
            params.append((self.fresh("xs"), "LA"))
        d = Def(self.fresh("pf"), "poly", params, "A", module)
        d.body = self.poly_expr("A", self.body_ctx(defs, module, params), r.randint(1, 3))
        return [d]

    def new_plain(self, defs, module):
        r = self.r
        if r.random() < 0.3:
            d = Def(self.fresh("f"), "plain", [(self.fresh("a"), I)], I, module)
        else:
            d = Def(self.fresh("f"), "plain", self.value_params(r.randint(1, 3)), r.choice(VALUE_TYPES), module)
        d.body = self.expr(d.ret, self.body_ctx(defs, module, d.params), r.randint(1, 3))
        return [d]

    def new_const(self, defs, module):
        """A top-level constant: the compiler evaluates it at compile time and stores its value."""
        d = Def(self.fresh("k"), "const", [], self.r.choice(VALUE_TYPES), module)
        ctx = self.body_ctx(defs, module, [])
        ctx["max_n"] = 6
        d.body = self.expr(d.ret, ctx, self.r.randint(1, 3))
        return [d]

    def new_mk(self, defs, module):
        d = Def(self.fresh("mk"), "mk", self.value_params(self.r.randint(1, 2)), F, module)
        d.body = self.lam(self.body_ctx(defs, module, d.params), 2)
        return [d]

    def new_recursive(self, defs, module, kind):
        r = self.r
        k = r.randint(2, 3) if kind == "group" else 1
        poly = r.random() < 0.3
        acc_ty = "A" if poly else r.choice(VALUE_TYPES)
        params = []
        if not poly and r.random() < 0.5:
            params.append((self.fresh("g"), "gen"))
        n = self.fresh("n")
        params.append((n, U))
        acc = self.fresh("acc")
        params.append((acc, acc_ty))
        if poly:
            params.append((self.fresh("f"), "FA"))
        params += self.value_params(r.randint(0, 2))
        ret = acc_ty if poly or r.random() < 0.6 else r.choice(VALUE_TYPES)
        members = [Def(self.fresh("t" if kind != "nontail" else "nt"), kind, params, ret, module) for _ in range(k)]
        names = [m.name for m in members]
        for m in members:
            m.group = names
        for i, m in enumerate(members):
            ctx = self.body_ctx(defs, module, params)
            ctx["uvars"] = []
            base = self.expr(ret, ctx, r.randint(0, 2))

            def rec_call(target, step_acc):
                args = []
                for p, t in params:
                    if t == U:
                        args.append(("bin", "-", ("var", n), ("ulit", 1)))
                    elif p == acc:
                        args.append(step_acc)
                    else:
                        args.append(("var", p))
                return ("call", target, args)

            if kind == "nontail":
                res = self.fresh("r")
                step = self.expr(acc_ty, ctx, r.randint(0, 2))
                combine = self.expr(ret, push(ctx, res, ret), r.randint(1, 2))
                other = ("let", res, rec_call(m.name, step), combine, ret, True)
            else:
                step = self.expr(acc_ty, ctx, r.randint(0, 2))
                nxt = names[(i + 1) % k]
                other = rec_call(nxt, step)
                if r.random() < 0.4:
                    alt = rec_call(r.choice(names), self.expr(acc_ty, ctx, r.randint(0, 1)))
                    other = ("if", self.expr(B, ctx, r.randint(0, 2)), other, alt)
            m.body = ("if", ("bin", "==", ("var", n), ("ulit", 0)), base, other)
        return members

    def new_hof(self, defs, module):
        r = self.r
        f = self.fresh("fn")
        d = Def(self.fresh("h"), "hof", [(f, F)] + self.value_params(r.randint(0, 2)), r.choice([I, L, S]), module)
        d.body = self.expr(d.ret, self.body_ctx(defs, module, d.params), 2)
        return [d]

    def new_defs(self, defs, module, kind=None):
        kind = kind or self.pick_kind()
        if kind in ("tail", "nontail", "group"):
            return self.new_recursive(defs, module, kind)
        return {"mk": self.new_mk, "hof": self.new_hof, "poly": self.new_poly, "const": self.new_const,
                "plain": self.new_plain}[kind](defs, module)

    def pick_kind(self):
        return self.r.choices(["plain", "mk", "hof", "tail", "nontail", "group", "poly", "const"],
                              [3, 1, 1, 2, 1, 2, 2, 3])[0]

    def app_ctx(self, prog):
        return {"defs": list(prog.defs), "module": prog.nmods - 1, "env": [("z", I)], "uvars": ["zu"], "max_n": 12}

    def output(self, prog, ty=None):
        ty = ty or self.r.choice(VALUE_TYPES)
        return (ty, self.expr(ty, self.app_ctx(prog), self.r.randint(1, 3)))

    def new_expect(self, prog, module):
        """An expect in module comparing an expression with the reference's value for it (or, sometimes, another
        value, so the expect fails)."""
        ty = self.r.choice(EXPECT_TYPES)
        ctx = self.body_ctx(prog.defs, module, [])
        e = self.expr(ty, ctx, self.r.randint(1, 3))
        want = evaluate_expr(prog, e)
        if self.r.random() < 0.25:
            want = perturb(want)
        return Expect(module, ty, e, want)

    def program(self):
        r = self.r
        nmods = r.randint(2, self.max_mods)
        npkg = r.randint(0, min(2, nmods - 1)) if r.random() < 0.6 else 0
        prog = Program(nmods, npkg)
        prog.forms = ["type" if m < npkg else r.choice(["type", "type", "header"]) for m in range(nmods - 1)]
        for _ in range(r.randint(3, 9)):
            kind = self.pick_kind()
            module = r.randrange(nmods - 1) if kind in ("tail", "group") and r.random() < 0.8 else r.randrange(nmods)
            prog.defs.extend(self.new_defs(prog.defs, module, kind))
        for _ in range(r.randint(2, 6)):
            prog.outputs.append(self.output(prog))
        called = calls_in(prog)
        ctx = self.app_ctx(prog)
        for d in prog.defs:
            if (d.group is None or d.group[0] == d.name) and d.name not in called:
                ty = r.choice(POLY_INST) if d.ret == "A" else d.ret
                prog.outputs.append((ty, self.call(d, ctx, 1, want=ty)))
        for _ in range(r.randint(0, 3)):
            try:
                prog.expects.append(self.new_expect(prog, r.randrange(nmods)))
            except (Overflow, RecursionError):
                pass
        return prog


def push(ctx, v, t):
    c = dict(ctx)
    c["env"] = ctx["env"] + [(v, t)]
    return c


def perturb(v):
    if isinstance(v, bool):
        return not v
    if isinstance(v, int):
        return v + 1
    return v + "x"


# --------------------------------------------------------------------------------------------- tree helpers

def children(e):
    out = []
    for i, c in enumerate(e):
        if isinstance(c, tuple):
            out.append((i, c))
        elif isinstance(c, list):
            for j, cc in enumerate(c):
                out.append(((i, j), cc))
    return out


def walk(e):
    yield e
    for _, c in children(e):
        yield from walk(c)


def refs(e):
    return {x[1] for x in walk(e) if x[0] in ("call", "fnref")}


def calls_in(prog):
    names = set()
    for d in prog.defs:
        names |= refs(d.body) - set(d.group or [d.name])
    for _, e in prog.outputs:
        names |= refs(e)
    return names


def reachable(prog):
    by = prog.by_name()
    seen = set()
    todo = [n for _, e in prog.outputs for n in refs(e)] + [n for x in prog.expects for n in refs(x.expr)]
    while todo:
        n = todo.pop()
        if n in seen:
            continue
        seen.add(n)
        todo += list(refs(by[n].body))
    return seen


def sites(prog):
    """(module, referencing def name or None, expr) for every expression of the program."""
    out = [(d.module, d.name, d.body) for d in prog.defs]
    out += [(prog.nmods - 1, None, e) for _, e in prog.outputs]
    out += [(x.module, None, x.expr) for x in prog.expects]
    return out


def referrers(prog, name):
    """The modules of every site referencing name (from outside its own recursive group)."""
    by = prog.by_name()
    grp = set(by[name].group or [name])
    return [m for m, owner, e in sites(prog) if owner not in grp and name in refs(e)]


# --------------------------------------------------------------------------------------------- reference

def chk(v):
    if not (I64_MIN <= v <= I64_MAX):
        raise Overflow()
    return v


class Eval:
    def __init__(self, prog, fuel=300000):
        self.defs = prog.by_name()
        self.fuel = fuel

    def tick(self):
        self.fuel -= 1
        if self.fuel < 0:
            raise Overflow()

    def ev(self, e, env):
        self.tick()
        k = e[0]
        if k == "lit":
            return e[2]
        if k == "ulit":
            return e[1]
        if k == "var":
            return env[e[1]]
        if k == "bin":
            op = e[1]
            if op == "and":
                return self.ev(e[2], env) and self.ev(e[3], env)
            if op == "or":
                return self.ev(e[2], env) or self.ev(e[3], env)
            a, b = self.ev(e[2], env), self.ev(e[3], env)
            if op == "+":
                return chk(a + b)
            if op == "-":
                if isinstance(a, int) and e[3][0] == "ulit" and a - b < 0:
                    raise Overflow()
                return chk(a - b)
            if op == "*":
                return chk(a * b)
            if op == "<":
                return a < b
            if op == "==":
                return a == b
        if k == "rem":
            a, c = self.ev(e[1], env), e[2]
            q = abs(a) // c
            return (abs(a) - q * c) * (1 if a >= 0 else -1)
        if k == "if":
            return self.ev(e[2] if self.ev(e[1], env) else e[3], env)
        if k == "let":
            env2 = dict(env)
            env2[e[1]] = self.ev(e[2], env)
            return self.ev(e[3], env2)
        if k == "call":
            d = self.defs[e[1]]
            args = [self.ev(a, env) for a in e[2]]
            return self.ev(d.body, dict(zip([p for p, _ in d.params], args)))
        if k == "match":
            t = self.ev(e[1], env)
            env2 = dict(env)
            if t[0] == "P":
                env2[e[2]] = t[1]
                return self.ev(e[3], env2)
            if t[0] == "Q":
                env2[e[4]] = t[1]
                return self.ev(e[5], env2)
            return self.ev(e[6], env)
        if k == "field":
            return self.ev(e[1], env)[e[2]]
        if k == "rec":
            return {"a": self.ev(e[1], env), "b": self.ev(e[2], env)}
        if k == "tag":
            return (e[1], None if e[2] is None else self.ev(e[2], env))
        if k == "list":
            return [self.ev(x, env) for x in e[1]]
        if k == "lam":
            return ("clo", e[1], e[2], env)
        if k == "fnref":
            d = self.defs[e[1]]
            return ("clo", d.params[0][0], d.body, {})
        if k == "apply":
            return self.apply(self.ev(e[1], env), self.ev(e[2], env))
        if k == "map":
            xs, f = self.ev(e[1], env), self.ev(e[2], env)
            return [self.apply(f, x) for x in xs]
        if k == "keep":
            xs = self.ev(e[1], env)
            return [x for x in xs if self.ev(e[3], {**env, e[2]: x})]
        if k == "append":
            return self.ev(e[1], env) + [self.ev(e[2], env)]
        if k == "lconcat":
            return self.ev(e[1], env) + self.ev(e[2], env)
        if k == "fold":
            xs, s = self.ev(e[1], env), self.ev(e[2], env)
            for x in xs:
                s = self.ev(e[5], {**env, e[3]: s, e[4]: x})
            return s
        if k == "sum":
            s = 0
            for x in self.ev(e[1], env):
                s = chk(s + x)
            return s
        if k == "len":
            return len(self.ev(e[1], env))
        if k == "slen":
            return len(self.ev(e[1], env).encode())
        if k == "concat":
            s = self.ev(e[1], env) + self.ev(e[2], env)
            if len(s) > 4000:
                raise Overflow()
            return s
        if k == "tostr":
            return str(self.ev(e[1], env))
        if k == "join":
            return ",".join(str(x) for x in self.ev(e[1], env))
        raise ValueError(k)

    def apply(self, clo, x):
        _, p, body, cenv = clo
        return self.ev(body, {**cenv, p: x})


def render_value(ty, v, ev):
    if ty == I:
        return str(v)
    if ty == S:
        return v
    if ty == B:
        return "T" if v else "F"
    if ty == L:
        return ",".join(map(str, v))
    if ty == R:
        return f"{v['a']}/{v['b']}"
    if ty == T:
        return {"P": lambda: f"P{v[1]}", "Q": lambda: f"Q{v[1]}", "W": lambda: "W"}[v[0]]()
    if ty == F:
        return str(ev.apply(v, 3))
    raise ValueError(ty)


def evaluate(prog):
    """The expected stdout lines. Raises Overflow for programs the model does not run. Every constant is evaluated
    too, since the compiler evaluates them all at compile time."""
    ev = Eval(prog)
    for d in prog.defs:
        if d.kind == "const":
            ev.ev(d.body, {})
    for x in prog.expects:
        ev.ev(x.expr, {})
    env = {"z": 0, "zu": 0}
    return [render_value(ty, ev.ev(e, env), ev) for ty, e in prog.outputs]


def evaluate_expr(prog, e):
    return Eval(prog).ev(e, {})


# --------------------------------------------------------------------------------------------- emitter

def mod_name(i):
    return f"M{i}"


class Emit:
    """The files of a program: {relative path: text}."""

    def __init__(self, prog):
        self.prog = prog
        self.by = prog.by_name()
        self.app = prog.nmods - 1

    def ref(self, name, from_mod):
        d = self.by[name]
        m = d.module
        if m == self.app or d.private:
            return name
        if m != from_mod or self.prog.forms[m] == "type":
            return f"{mod_name(m)}.{name}"
        return name

    def atom(self, e, m, ind):
        s = self.ex(e, m, ind)
        if e[0] in ("var", "call", "lit", "ulit", "list", "rec", "tostr", "concat", "join", "slen", "len") and not (
                e[0] == "lit" and e[1] == I and e[2] < 0):
            return s
        return f"({s})"

    def recv(self, e, m, ind):
        s = self.ex(e, m, ind)
        return s if e[0] in ("var", "call") else f"({s})"

    def ex(self, e, m, ind):
        k = e[0]
        nl = "\n" + "\t" * (ind + 1)
        end = "\n" + "\t" * ind
        if k == "lit":
            if e[1] == I:
                return f"{e[2]}.I64" if e[2] >= 0 else f"({e[2]}.I64)"
            if e[1] == S:
                return f'"{e[2]}"'
            if e[1] == B:
                return "Bool.True" if e[2] else "Bool.False"
        if k == "ulit":
            return f"{e[1]}.U64"
        if k == "var":
            return e[1]
        if k == "bin":
            return f"{self.atom(e[2], m, ind)} {e[1]} {self.atom(e[3], m, ind)}"
        if k == "rem":
            return f"I64.rem_by({self.ex(e[1], m, ind)}, {e[2]}.I64)"
        if k == "if":
            return f"if {self.atom(e[1], m, ind)} {self.atom(e[2], m, ind)} else {self.atom(e[3], m, ind)}"
        if k == "let":
            ann = f"{e[1]} : {ROC_TYPE[e[4]]}{nl}" if e[5] and e[4] in ROC_TYPE else ""
            return f"{{{nl}{ann}{e[1]} = {self.ex(e[2], m, ind + 1)}{nl}{self.ex(e[3], m, ind + 1)}{end}}}"
        if k == "call":
            d = self.by[e[1]]
            if d.kind == "const":
                return self.ref(e[1], m)
            return f"{self.ref(e[1], m)}({', '.join(self.ex(a, m, ind) for a in e[2])})"
        if k == "match":
            scrut = self.atom(e[1], m, ind)
            if e[1][0] not in ("var", "call"):
                nl2 = "\n" + "\t" * (ind + 2)
                self.tms = getattr(self, "tms", 0) + 1
                tm = f"tm{self.tms}"
                scrut = f"({{{nl2}{tm} : {ROC_TYPE[T]}{nl2}{tm} = {self.ex(e[1], m, ind + 2)}{nl2}{tm}{nl}}})"
            # The last branch is a wildcard, so tags an edit adds to Tag change no match.
            return (f"match {scrut} {{{nl}P({e[2]}) => {self.ex(e[3], m, ind + 1)}"
                    f"{nl}Q({e[4]}) => {self.ex(e[5], m, ind + 1)}{nl}_ => {self.ex(e[6], m, ind + 1)}{end}}}")
        if k == "field":
            return f"{self.recv(e[1], m, ind)}.{e[2]}"
        if k == "rec":
            return f"Rec.mk({self.ex(e[1], m, ind)}, {self.ex(e[2], m, ind)})"
        if k == "tag":
            return f"Tag.{e[1]}" if e[2] is None else f"Tag.{e[1]}({self.ex(e[2], m, ind)})"
        if k == "list":
            if not e[1]:
                return "List.repeat(0.I64, 0)"
            return "[" + ", ".join(self.ex(x, m, ind) for x in e[1]) + "]"
        if k == "lam":
            return f"|{e[1]}| {self.ex(e[2], m, ind)}"
        if k == "fnref":
            return self.ref(e[1], m)
        if k == "apply":
            return f"{self.atom(e[1], m, ind)}({self.ex(e[2], m, ind)})"
        if k == "map":
            return f"{self.recv(e[1], m, ind)}.map({self.ex(e[2], m, ind)})"
        if k == "keep":
            return f"{self.recv(e[1], m, ind)}.keep_if(|{e[2]}| {self.ex(e[3], m, ind)})"
        if k == "append":
            return f"{self.recv(e[1], m, ind)}.append({self.ex(e[2], m, ind)})"
        if k == "lconcat":
            return f"List.concat({self.ex(e[1], m, ind)}, {self.ex(e[2], m, ind)})"
        if k == "fold":
            return f"{self.recv(e[1], m, ind)}.fold({self.ex(e[2], m, ind)}, |{e[3]}, {e[4]}| {self.ex(e[5], m, ind)})"
        if k == "sum":
            return f"{self.recv(e[1], m, ind)}.sum()"
        if k == "len":
            return f"{self.recv(e[1], m, ind)}.len().to_i64_wrap()"
        if k == "slen":
            return f"Str.count_utf8_bytes({self.ex(e[1], m, ind)}).to_i64_wrap()"
        if k == "concat":
            return f"Str.concat({self.ex(e[1], m, ind)}, {self.ex(e[2], m, ind)})"
        if k == "tostr":
            return f"{self.recv(e[1], m, ind)}.to_str()"
        if k == "join":
            return f'Str.join_with({self.recv(e[1], m, ind)}.map(|q| q.to_str()), ",")'
        raise ValueError(k)

    def sig(self, d):
        if d.kind == "const":
            return ROC_TYPE[d.ret]
        ps = ", ".join(ROC_TYPE[t] for _, t in d.params)
        return f"{ps} -> {ROC_TYPE[d.ret]}"

    def def_text(self, d, m, ind):
        t = "\t" * ind
        ann = f"{t}{d.name} : {self.sig(d)}\n" if d.ann else ""
        body = self.ex(d.body, m, ind)
        if self.prog.error == d.name:
            nl = "\n" + "\t" * (ind + 1)
            body = f"{{{nl}oops : I64{nl}oops = \"not a number\"{nl}{body}\n{t}}}"
        if d.kind == "const":
            return f"{ann}{t}{d.name} = {body}\n"
        params = ", ".join(p for p, _ in d.params)
        return f"{ann}{t}{d.name} = |{params}| {body}\n"

    def expect_text(self, x, m):
        want = {I: lambda v: f"{v}.I64" if v >= 0 else f"({v}.I64)", S: lambda v: f'"{v}"',
                B: lambda v: "Bool.True" if v else "Bool.False"}[x.ty](x.want)
        return f"expect {self.atom(x.expr, m, 0)} == {want}\n"

    def noisy(self, fname, chunks):
        """Join a file's chunks, with comments and blank lines when its noise is nonzero."""
        n = self.prog.noise.get(fname, 0)
        if not n:
            return "\n".join(chunks)
        r = random.Random(n)
        out = [f"# {fname} revision {n}\n"]
        for c in chunks:
            if r.random() < 0.5:
                out.append("#" + r.choice([" note", " todo: tidy", "", " see below"]) + "\n")
            out.append(c)
            if r.random() < 0.4:
                out.append("\n")
        return "\n".join(out)

    def module_exists(self, m):
        return any(d.module == m for d in self.prog.defs) or any(x.module == m for x in self.prog.expects)

    def import_line(self, target, from_mod):
        name = mod_name(target) if isinstance(target, int) else target
        in_pkg = self.prog.in_pkg(target) if isinstance(target, int) else self.prog.npkg > 0
        if in_pkg and not self.prog.in_pkg(from_mod):
            return f"import lib.{name}\n"
        return f"import {name}\n"

    def imports(self, m, text):
        used = set()
        for d in self.prog.defs:
            if d.module == m:
                used |= refs(d.body)
        if m == self.app:
            for _, e in self.prog.outputs:
                used |= refs(e)
        for x in self.prog.expects:
            if x.module == m:
                used |= refs(x.expr)
        mods = {self.by[n].module for n in used} - {m}
        mods |= {x for x in self.prog.extra_imports.get(m, set()) if x < m and self.module_exists(x)}
        lines = [self.import_line(i, m) for i in sorted(mods)]
        for ty in ("Rec", "Tag"):
            if re.search(rf"\b{ty}\b", text):
                lines.append(self.import_line(ty, m))
        return "".join(lines)

    def type_files(self):
        p = self.prog
        fields = ", ".join(f"{n} : {ROC_TYPE[t]}" for n, t, _ in p.rec_fields)
        init = ", ".join(n if v is None else f"{n}: {v}" for n, t, v in p.rec_fields)
        rec = [f"Rec := {{ {fields} }}.{{\n\tmk : I64, Str -> Rec\n\tmk = |a, b| {{ {init} }}\n}}\n"]
        tags = ", ".join(n if t is None else f"{n}({ROC_TYPE[t]})" for n, t in p.tag_tags)
        tag = [f"Tag := [{tags}]\n"]
        return {"Rec.roc": self.noisy("Rec.roc", rec), "Tag.roc": self.noisy("Tag.roc", tag)}

    def files(self):
        p = self.prog
        out = {}
        pkg_dir = "lib/" if p.npkg > 0 else ""
        for name, text in self.type_files().items():
            out[pkg_dir + name] = text
        for m in range(self.app):
            ds = [d for d in p.defs if d.module == m]
            xs = [x for x in p.expects if x.module == m]
            if not ds and not xs:
                continue
            name = mod_name(m)
            exprs = [self.expect_text(x, m) for x in xs]
            if p.forms[m] == "type":
                public = [self.def_text(d, m, 1) for d in ds if not d.private]
                private = [self.def_text(d, m, 0) for d in ds if d.private]
                block = "\n".join(public)
                body = self.noisy(f"{name}.roc", [f"{name} :: [].{{\n{block}}}\n"] + private + exprs)
                header = ""
            else:
                exposed = ", ".join(d.name for d in ds if not d.private)
                header = f"module [{exposed}]\n\n"
                body = self.noisy(f"{name}.roc", [self.def_text(d, m, 0) for d in ds] + exprs)
            imp = self.imports(m, body)
            path = (pkg_dir if p.in_pkg(m) else "") + f"{name}.roc"
            out[path] = f"{header}{imp}{chr(10) if imp else ''}{body}"
        if p.npkg > 0:
            exposed = sorted(["Rec", "Tag"] + [mod_name(m) for m in range(p.npkg) if f"lib/{mod_name(m)}.roc" in out])
            out["lib/main.roc"] = f"package [{', '.join(exposed)}] {{}}\n"
        a = self.app
        chunks = [self.def_text(d, a, 0) for d in p.defs if d.module == a]
        rows = ["main! = |args| {", "\tzu = args.len() * 0", "\tz = zu.to_i64_wrap()"]
        for i, (ty, e) in enumerate(p.outputs):
            rows.append(f"\to{i} : {ROC_TYPE[ty]}")
            rows.append(f"\to{i} = {self.ex(e, a, 1)}")
        rendered = ", ".join(self.render_output(i, ty) for i, (ty, _) in enumerate(p.outputs))
        rows.append(f"\tlines = [{rendered}]")
        rows.append('\techo!(Str.concat(Str.join_with(lines, "\\n"), "\\n"))')
        rows.append("\tOk({})")
        rows.append("}")
        chunks.append("\n".join(rows) + "\n")
        chunks += [self.expect_text(x, a) for x in p.expects if x.module == a]
        body = self.noisy("app.roc", chunks)
        header = 'app [main!] { lib: "./lib/main.roc" }\n\n' if p.npkg > 0 else ""
        imp = self.imports(a, body)
        out["app.roc"] = f"{header}{imp}\n{body}"
        return out

    def render_output(self, i, ty):
        v = f"o{i}"
        if ty == I:
            return f"{v}.to_str()"
        if ty == S:
            return v
        if ty == B:
            return f'if {v} "T" else "F"'
        if ty == L:
            return f'Str.join_with({v}.map(|q| q.to_str()), ",")'
        if ty == R:
            return f'Str.concat({v}.a.to_str(), Str.concat("/", {v}.b))'
        if ty == T:
            return f'match {v} {{\n\t\tP(k) => Str.concat("P", k.to_str())\n\t\tQ(k) => Str.concat("Q", k)\n\t\t_ => "W"\n\t}}'
        if ty == F:
            return f"{v}(3.I64).to_str()"


def generate(seed, max_mods=5):
    """A program whose reference evaluates without overflow, and its expected lines."""
    for k in range(100):
        g = Gen(seed * 1000 + k, max_mods)
        prog = g.program()
        try:
            return prog, evaluate(prog)
        except (Overflow, RecursionError):
            continue
    raise RuntimeError("no program without overflow")
