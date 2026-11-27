"""Program generator, reference evaluator and emitter for the backend-diff model.

A program is a list of top-level definitions, each placed in one of 2 to 5 modules (the last module is the app), plus a
list of outputs that `main!` prints, one per line. Definitions and expressions are composed at random from small typed
constructs; nothing here is a handwritten scenario.

The same program value is emitted as Roc (for a native headerless app or for the wasm test platform, optionally with
every definition collapsed into the app module) and evaluated by `evaluate`, the Python reference.
"""
import random

I, U, S, B, L, R, T, F = "I64", "U64", "Str", "Bool", "List", "Rec", "Tag", "Fn"
VALUE_TYPES = (I, S, B, L, R, T, F)
ROC_TYPE = {
    I: "I64", U: "U64", S: "Str", B: "Bool", L: "List(I64)",
    R: "{ a : I64, b : Str }", T: "[P(I64), Q(Str), W]", F: "(I64 -> I64)", "gen": "g",
    "A": "a", "FA": "(a -> a)", "LA": "List(a)",
}
# Inside a polymorphic definition the type variable is "A"; "FA" is A -> A and "LA" is List(A).
POLY_INST = (I, S, B, L, R, T)
I64_MIN, I64_MAX = -(1 << 63), (1 << 63) - 1
WORDS = ["", "a", "b", "xy", "ok", "rc", "zz", "hello", "q r"]


class Overflow(Exception):
    """The reference left I64 range or ran too long; the program is discarded, never compared."""


# --------------------------------------------------------------------------------------------- program values

class Def:
    """A top-level function. kind: plain, mk (returns a closure), hof (takes one), tail, nontail, group."""

    def __init__(self, name, kind, params, ret, module):
        self.name, self.kind, self.params, self.ret, self.module = name, kind, params, ret, module
        self.body = None
        self.group = None  # names of the tail group's members, for kind == "group" and "tail"


class Program:
    def __init__(self, nmods):
        self.nmods = nmods
        self.defs = []          # in definition order; calls go only to earlier defs or the same group
        self.outputs = []       # (type, expr) printed by main!
        self.forms = []         # per library module: "type" (type module) or "header" (module [..])

    def by_name(self):
        return {d.name: d for d in self.defs}


# --------------------------------------------------------------------------------------------- generator

class Gen:
    def __init__(self, seed, max_mods=5):
        self.r = random.Random(seed)
        self.n = 0
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

    def callable_defs(self, ctx, ty):
        """Defs that can produce ty. A polymorphic def (ret "A") produces any type it can be instantiated at."""
        inst = ty in POLY_INST or ty == "A"
        return [d for d in ctx["defs"] if d.module <= ctx["module"] and (d.ret == ty or (d.ret == "A" and inst))]

    def counter(self, ctx):
        """A U64 recursion count: small, and through the runtime zero when one is in scope."""
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
                return ("let", v, e1, self.expr(ty, push(ctx, v, vt), depth - 1), vt)
            if k == "call" and defs:
                # Prefer definitions from other modules and recursive ones: those are the cross-module calls.
                weights = [(3 if d.module != ctx["module"] else 1) * (3 if d.kind in ("tail", "nontail", "group") else 1)
                           for d in defs]
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
                # A top-level function used as a value, so a closure value names another module's procedure.
                fs = [d for d in ctx["defs"] if d.params and [t for _, t in d.params] == [I] and d.ret == I
                      and d.module <= ctx["module"]]
                if fs:
                    return ("fnref", r.choice(fs).name)

    def poly_expr(self, ty, ctx, depth):
        """An expression of the type variable A (or List(A)) inside a polymorphic definition."""
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
                return ("let", v, e1, self.poly_expr(ty, push(ctx, v, vt), depth - 1), vt)
            if k == "call":
                ds = [d for d in ctx["defs"] if d.ret == "A" and d.module <= ctx["module"]]
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

    def add_poly(self, prog, module):
        """A polymorphic function: x : a, f : a -> a, a few concrete parameters, returning a."""
        r = self.r
        params = [(self.fresh("x"), "A"), (self.fresh("f"), "FA")] + self.value_params(r.randint(0, 1))
        if r.random() < 0.3:
            params.append((self.fresh("xs"), "LA"))
        d = Def(self.fresh("pf"), "poly", params, "A", module)
        ctx = self.body_ctx(prog, module, params)
        d.body = self.poly_expr("A", ctx, r.randint(1, 3))
        prog.defs.append(d)

    def lam(self, ctx, depth):
        p = self.fresh("p")
        return ("lam", p, self.expr(I, push(ctx, p, I), depth))

    # definitions ------------------------------------------------------------------------------------

    def value_params(self, n):
        return [(self.fresh("a"), self.r.choice(VALUE_TYPES)) for _ in range(n)]

    def body_ctx(self, prog, module, params):
        return {"defs": list(prog.defs), "module": module, "env": [(p, t) for p, t in params if t not in (U, "gen")],
                "uvars": [p for p, t in params if t == U], "max_n": 4}

    def add_plain(self, prog, module):
        r = self.r
        if r.random() < 0.3:  # an I64 -> I64 function, which can also be passed as a closure value
            d = Def(self.fresh("f"), "plain", [(self.fresh("a"), I)], I, module)
        else:
            d = Def(self.fresh("f"), "plain", self.value_params(r.randint(1, 3)), r.choice(VALUE_TYPES), module)
        d.body = self.expr(d.ret, self.body_ctx(prog, module, d.params), r.randint(1, 3))
        prog.defs.append(d)

    def add_mk(self, prog, module):
        """A function returning a closure that captures its parameters."""
        d = Def(self.fresh("mk"), "mk", self.value_params(self.r.randint(1, 2)), F, module)
        d.body = self.lam(self.body_ctx(prog, module, d.params), 2)
        prog.defs.append(d)

    def add_recursive(self, prog, module, kind):
        """A tail loop, a non-tail recursion, or a group of 2-3 functions in a cycle of tail calls."""
        r = self.r
        k = r.randint(2, 3) if kind == "group" else 1
        poly = r.random() < 0.3
        acc_ty = "A" if poly else r.choice(VALUE_TYPES)
        params = []
        if not poly and r.random() < 0.5:
            params.append((self.fresh("g"), "gen"))  # a generic parameter passed through every call (#13340)
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
            ctx = self.body_ctx(prog, module, params)
            ctx["uvars"] = []  # calls to other recursive defs from a body use literal counts only
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
                other = ("let", res, rec_call(m.name, step), combine)
            else:
                step = self.expr(acc_ty, ctx, r.randint(0, 2))
                nxt = names[(i + 1) % k]
                other = rec_call(nxt, step)
                if r.random() < 0.4:
                    alt = rec_call(r.choice(names), self.expr(acc_ty, ctx, r.randint(0, 1)))
                    other = ("if", self.expr(B, ctx, r.randint(0, 2)), other, alt)
            m.body = ("if", ("bin", "==", ("var", n), ("ulit", 0)), base, other)
        prog.defs.extend(members)

    def add_hof(self, prog, module):
        """A function taking a closure, applying it and passing it on."""
        r = self.r
        f = self.fresh("fn")
        d = Def(self.fresh("h"), "hof", [(f, F)] + self.value_params(r.randint(0, 2)), r.choice([I, L, S]), module)
        d.body = self.expr(d.ret, self.body_ctx(prog, module, d.params), 2)
        prog.defs.append(d)

    def program(self):
        r = self.r
        nmods = r.randint(2, self.max_mods)
        prog = Program(nmods)
        prog.forms = [r.choice(["type", "type", "header"]) for _ in range(nmods - 1)]
        for _ in range(r.randint(3, 9)):
            kind = r.choices(["plain", "mk", "hof", "tail", "nontail", "group", "poly"], [3, 1, 1, 2, 1, 3, 2])[0]
            module = r.randrange(nmods - 1) if kind in ("tail", "group") and r.random() < 0.8 else r.randrange(nmods)
            if kind in ("tail", "nontail", "group"):
                self.add_recursive(prog, module, kind)
            elif kind == "mk":
                self.add_mk(prog, module)
            elif kind == "hof":
                self.add_hof(prog, module)
            elif kind == "poly":
                self.add_poly(prog, module)
            else:
                self.add_plain(prog, module)
        ctx = {"defs": list(prog.defs), "module": nmods - 1, "env": [("z", I)], "uvars": ["zu"], "max_n": 12}
        for _ in range(r.randint(2, 6)):
            ty = r.choice(VALUE_TYPES)
            prog.outputs.append((ty, self.expr(ty, ctx, r.randint(1, 3))))
        # Every recursive group gets one call from the app, through its first member.
        called = calls_in(prog)
        for d in prog.defs:
            if (d.group is None or d.group[0] == d.name) and d.name not in called:
                ty = r.choice(POLY_INST) if d.ret == "A" else d.ret
                prog.outputs.append((ty, self.call(d, ctx, 1, want=ty)))
        return prog


def push(ctx, v, t):
    c = dict(ctx)
    c["env"] = ctx["env"] + [(v, t)]
    return c


# --------------------------------------------------------------------------------------------- tree helpers

def children(e):
    """(index, child) pairs for every sub-expression slot of e."""
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


def calls_in(prog):
    names = set()
    for d in prog.defs:
        names |= {e[1] for e in walk(d.body) if e[0] in ("call", "fnref") and e[1] not in (d.group or [d.name])}
    for _, e in prog.outputs:
        names |= {x[1] for x in walk(e) if x[0] in ("call", "fnref")}
    return names


def reachable(prog):
    """Names of defs reachable from the outputs."""
    by = prog.by_name()
    seen, todo = set(), [x[1] for _, e in prog.outputs for x in walk(e) if x[0] in ("call", "fnref")]
    while todo:
        n = todo.pop()
        if n in seen:
            continue
        seen.add(n)
        todo += [x[1] for x in walk(by[n].body) if x[0] in ("call", "fnref")]
    return seen


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
        if k in ("lit",):
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
    """The expected stdout lines. Raises Overflow for programs the model does not compare."""
    ev = Eval(prog)
    env = {"z": 0, "zu": 0}
    return [render_value(ty, ev.ev(e, env), ev) for ty, e in prog.outputs]


# --------------------------------------------------------------------------------------------- emitter

def mod_name(i):
    return f"M{i}"


class Emit:
    def __init__(self, prog, collapse=False):
        self.prog = prog
        self.by = prog.by_name()
        self.app = prog.nmods - 1
        self.collapse = collapse

    def module_of(self, d):
        return self.app if self.collapse else d.module

    def ref(self, name, from_mod):
        d = self.by[name]
        m = self.module_of(d)
        if m == self.app:
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
            # Annotated, so a tag literal's unused payloads still get concrete types.
            ann = f"{e[1]} : {ROC_TYPE[e[4]]}{nl}" if len(e) > 4 and e[4] in VALUE_TYPES else ""
            return f"{{{nl}{ann}{e[1]} = {self.ex(e[2], m, ind + 1)}{nl}{self.ex(e[3], m, ind + 1)}{end}}}"
        if k == "call":
            return f"{self.ref(e[1], m)}({', '.join(self.ex(a, m, ind) for a in e[2])})"
        if k == "match":
            scrut = self.atom(e[1], m, ind)
            if e[1][0] not in ("var", "call"):
                nl2 = "\n" + "\t" * (ind + 2)
                self.tms = getattr(self, "tms", 0) + 1
                tm = f"tm{self.tms}"
                scrut = f"({{{nl2}{tm} : {ROC_TYPE[T]}{nl2}{tm} = {self.ex(e[1], m, ind + 2)}{nl2}{tm}{nl}}})"
            return (f"match {scrut} {{{nl}P({e[2]}) => {self.ex(e[3], m, ind + 1)}"
                    f"{nl}Q({e[4]}) => {self.ex(e[5], m, ind + 1)}{nl}W => {self.ex(e[6], m, ind + 1)}{end}}}")
        if k == "field":
            return f"{self.recv(e[1], m, ind)}.{e[2]}"
        if k == "rec":
            return f"{{ a: {self.ex(e[1], m, ind)}, b: {self.ex(e[2], m, ind)} }}"
        if k == "tag":
            return e[1] if e[2] is None else f"{e[1]}({self.ex(e[2], m, ind)})"
        if k == "list":
            if not e[1]:
                return "List.repeat(0.I64, 0)"  # an empty literal's element type would stay unresolved
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
        ps = ", ".join(ROC_TYPE[t] for _, t in d.params)
        ret = "I64 -> I64" if d.ret == F else ROC_TYPE[d.ret]
        if d.ret == F:
            ret = "(I64 -> I64)"
        return f"{ps} -> {ret}"

    def def_text(self, d, m, ind):
        t = "\t" * ind
        params = ", ".join(p for p, _ in d.params)
        return f"{t}{d.name} : {self.sig(d)}\n{t}{d.name} = |{params}| {self.ex(d.body, m, ind)}\n"

    def imports(self, m, texts):
        used = set()
        for d in self.prog.defs:
            if self.module_of(d) == m:
                used |= {x[1] for x in walk(d.body) if x[0] in ("call", "fnref")}
        if m == self.app:
            for _, e in self.prog.outputs:
                used |= {x[1] for x in walk(e) if x[0] in ("call", "fnref")}
        mods = sorted({self.module_of(self.by[n]) for n in used} - {m})
        return "".join(f"import {mod_name(i)}\n" for i in mods)

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
            return f'match {v} {{\n\t\tP(k) => Str.concat("P", k.to_str())\n\t\tQ(k) => Str.concat("Q", k)\n\t\tW => "W"\n\t}}'
        if ty == F:
            return f"{v}(3.I64).to_str()"

    def files(self, target):
        """{filename: text}. target: native (headerless app with echo!) or wasm (test/wasm/platform app)."""
        out = {}
        for m in range(self.app):
            ds = [d for d in self.prog.defs if self.module_of(d) == m]
            if not ds:
                continue
            name = mod_name(m)
            imp = self.imports(m, out)
            if self.prog.forms[m] == "type":
                body = "\n".join(self.def_text(d, m, 1) for d in ds)
                out[f"{name}.roc"] = f"{imp}{chr(10) if imp else ''}{name} :: [].{{\n{body}}}\n"
            else:
                exposed = ", ".join(d.name for d in ds)
                body = "\n".join(self.def_text(d, m, 0) for d in ds)
                out[f"{name}.roc"] = f"module [{exposed}]\n\n{imp}{chr(10) if imp else ''}{body}"
        a = self.app
        lines = []
        if target == "wasm":
            lines.append('app [main!] { pf: platform "./platform/main.roc" }\n')
            lines.append("import pf.FallibleHost")
        lines.append(self.imports(a, out).rstrip("\n"))
        lines.append("")
        for d in self.prog.defs:
            if self.module_of(d) == a:
                lines.append(self.def_text(d, a, 0))
        rows = []
        if target == "wasm":
            rows.append("main! = || {")
            rows.append("\tzu = Str.count_utf8_bytes(FallibleHost.json_input!({})) * 0")
        else:
            rows.append("main! = |args| {")
            rows.append("\tzu = args.len() * 0")
        rows.append("\tz = zu.to_i64_wrap()")
        for i, (ty, e) in enumerate(self.prog.outputs):
            rows.append(f"\to{i} : {ROC_TYPE[ty]}")
            rows.append(f"\to{i} = {self.ex(e, a, 1)}")
        rendered = ", ".join(self.render_output(i, ty) for i, (ty, _) in enumerate(self.prog.outputs))
        rows.append(f"\tlines = [{rendered}]")
        if target == "wasm":
            rows.append('\tStr.join_with(lines, "\\n")')
        else:
            rows.append('\techo!(Str.concat(Str.join_with(lines, "\\n"), "\\n"))')
            rows.append("\tOk({})")
        rows.append("}")
        out["app.roc"] = "\n".join(lines) + "\n" + "\n".join(rows) + "\n"
        return out


# --------------------------------------------------------------------------------------------- features

def cross_module_group_calls(prog, collapse=False):
    """Calls from one module into a recursive group defined in another: the shape of #13340 and #13356."""
    if collapse:
        return []
    by = prog.by_name()
    out = []
    sites = [(d.module, d.body) for d in prog.defs] + [(prog.nmods - 1, e) for _, e in prog.outputs]
    for m, e in sites:
        for x in walk(e):
            if x[0] == "call":
                d = by[x[1]]
                if d.kind in ("tail", "nontail", "group") and d.module != m:
                    out.append(x[1])
    return out


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


def wrap_cross_group_calls(prog):
    """A copy of prog where every call from another module into a tail group goes through a non-recursive wrapper
    defined beside the group. The wrapper is an ordinary caller in the group's own module, so the importer no longer
    calls a group member. When this alone removes a wasm disagreement, the disagreement is #13356's."""
    import copy
    p = copy.deepcopy(prog)
    by = p.by_name()
    wrappers = {}

    def wrapper(name):
        if name not in wrappers:
            d = by[name]
            w = Def(f"w_{name}", "plain", list(d.params), d.ret, d.module)
            w.body = ("call", name, [("var", x) for x, _ in d.params])
            wrappers[name] = w
            p.defs.insert(p.defs.index(d) + 1, w)
        return wrappers[name]

    def rewrite(e, m):
        if not isinstance(e, tuple):
            return e
        if e[0] == "call" and by[e[1]].kind in ("tail", "nontail", "group") and by[e[1]].module != m:
            return ("call", wrapper(e[1]).name, [rewrite(a, m) for a in e[2]])
        out = []
        for c in e:
            if isinstance(c, tuple):
                out.append(rewrite(c, m))
            elif isinstance(c, list):
                out.append([rewrite(x, m) for x in c])
            else:
                out.append(c)
        return tuple(out)

    for d in list(p.defs):
        if d.body is not None and not d.name.startswith("w_"):
            d.body = rewrite(d.body, d.module)
    p.outputs = [(t, rewrite(e, p.nmods - 1)) for t, e in p.outputs]
    return p
