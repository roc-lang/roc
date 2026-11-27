"""Random generator of small typed Roc programs heavy in closures.

A program is a list of annotated top-level functions plus `expect`s that run its entry functions
(type `I64 -> I64`) on a few inputs. Every expression is built by choosing a construct for the type
it needs and filling the construct's parts with fresh random constructs of their types, so the
programs are compositions, not scenarios. The constructs lean on what lambda sets and SpecConstr act
on: lambdas that capture, functions selected by `if` or `match`, closures stored in records and tag
payloads, higher-order functions, functions returning closures, fuel-bounded recursion, structural
recursion over a recursive type that holds closures, generic combinators used at several types, loops.

No oracle is computed here: the interpreter run on the compiler's own after-lift program is the
reference (README.md).
"""
import random

I, B, F, L, OP, R, H, CH = "I64", "Bool", "I64 -> I64", "List(I64)", "Op", "Rc", "(I64 -> I64) -> I64", "Chain"
TR = "Try(I64, [Neg(I64)])"
VALUE_TYPES = [I, F, L, OP, R, CH]
PARAM_TYPES = [I, I, F, F, L, OP, R, CH]
RET_TYPES = [I, I, F, F, OP, R, L, CH, TR]

PRELUDE = """Op := [Inc(I64), Fn(I64 -> I64)]

Chain := [End, Link(I64 -> I64, Chain), Add(I64, Chain)]

Rc : { n : I64, f : I64 -> I64 }

apply : (a -> b), a -> b
apply = |f, x| f(x)

comp : (a -> b), (b -> c) -> (a -> c)
comp = |f, g| |x| g(f(x))

konst : a -> (b -> a)
konst = |v| |_| v

choose : Bool, a, a -> a
choose = |c, x, y| if c x else y

checked : I64 -> Try(I64, [Neg(I64)])
checked = |n| if n < 0 Err(Neg(n)) else Ok(n)
"""


def sty_of(res, acc):
    """The type of a while loop's state: an integer when the result reads it directly."""
    return I if res == acc else F


def ind(s, n=1):
    pad = "\t" * n
    return "\n".join(pad + line if line else line for line in s.split("\n"))


def paren(s):
    if "\n" not in s and (s.isidentifier() or s.lstrip("-").isdigit()):
        return s
    return "(" + s + ")"


def arm(s):
    if "\n" not in s or s.startswith("{"):
        return s
    return "{\n" + ind(s) + "\n}"


def callee(s):
    return s if s.isidentifier() else "(" + s + ")"


class Var:
    def __init__(self, name, ty, state=False):
        self.name, self.ty, self.state = name, ty, state


class Fn:
    def __init__(self, name, params, ret, rec):
        self.name, self.params, self.ret, self.rec = name, params, ret, rec


class Gen:
    def __init__(self, seed, nfuns=6, nentries=3, depth=4):
        self.r = random.Random(seed)
        self.n = 0
        self.fns = []
        self.nfuns, self.nentries, self.depth = nfuns, nentries, depth
        self.cur = None  # the function being generated, for recursive calls

    def fresh(self, base):
        self.n += 1
        return "%s%d" % (base, self.n)

    def pick(self, xs):
        return self.r.choice(xs)

    def chance(self, p):
        return self.r.random() < p

    def vars(self, env, ty, pure=False):
        return [v for v in env if v.ty == ty and not (pure and v.state)]

    def pure(self, env):
        return [v for v in env if not v.state]

    # ------------------------------------------------------------------ dispatch
    def gen(self, ty, env, d):
        return {I: self.g_int, B: self.g_bool, F: self.g_fn, L: self.g_list, OP: self.g_op, R: self.g_rec,
                CH: self.g_chain, TR: self.g_try}[ty](env, d)

    def g_try(self, env, d):
        """A `Try(I64, [Neg(I64)])`. Inside a function that returns one, `?` may propagate."""
        opts = ["ok", "checked", "err"]
        if d > 0:
            opts += ["call", "if", "map"]
            if self.cur is not None and self.cur.ret == TR:
                opts += ["question"] * 4
        c = self.pick(opts)
        if c == "ok":
            return "Ok(%s)" % self.g_int(env, max(d - 1, 0))
        if c == "err":
            return "Err(Neg(%s))" % self.g_int(env, 0)
        if c == "checked":
            return "checked(%s)" % self.g_int(env, max(d - 1, 0))
        if c == "call":
            return self.g_call(TR, env, d) or "checked(1)"
        if c == "if":
            return self.g_if(TR, env, d)
        if c == "map":
            vn = self.fresh("v")
            return "match %s {\n\tOk(%s) => checked(%s)\n\tErr(e) => Err(e)\n}" % (
                paren(self.g_try(env, d - 1)), vn, self.g_int(env + [Var(vn, I)], d - 1))
        if self.cur is None or self.cur.ret != TR:
            return "checked(%s)" % self.g_int(env, d - 1)
        vn = self.fresh("q")
        first = self.g_try(env, d - 1)
        rest = self.g_try(env + [Var(vn, I)], d - 1)
        return "{\n\t%s = %s?\n%s\n}" % (vn, paren(first), ind(rest))

    def g_call(self, ty, env, d):
        """A call of an earlier top-level function, or a recursive call with smaller fuel."""
        cands = [f for f in self.fns if f.ret == ty]
        me = self.cur
        if me is not None and me.rec and me.ret == ty and self.chance(0.5):
            # Fuel recursion passes `fuel - 1`; structural recursion passes the matched tail.
            first = "fuel - 1" if me.rec == "fuel" else "rest"
            if any(v.name == first.split(" ")[0] for v in env):
                args = [first] + [self.gen(t, env, max(d - 2, 0)) for t in me.params[1:]]
                return "%s(%s)" % (me.name, ", ".join(args))
        if not cands:
            return None
        f = self.pick(cands)
        args = []
        for i, t in enumerate(f.params):
            if f.rec == "fuel" and i == 0:
                args.append(str(self.r.randint(0, 4)))
            else:
                args.append(self.gen(t, env, max(d - 1, 0)))
        return "%s(%s)" % (f.name, ", ".join(args))

    def g_if(self, ty, env, d):
        c = self.g_bool(env, d - 1)
        a, b = self.gen(ty, env, d - 1), self.gen(ty, env, d - 1)
        return "if %s %s else %s" % (paren(c), paren(a) if "\n" not in a else "{\n" + ind(a) + "\n}",
                                      paren(b) if "\n" not in b else "{\n" + ind(b) + "\n}")

    def g_let(self, ty, env, d):
        bty = self.pick(VALUE_TYPES)
        e = self.gen(bty, env, d - 1)
        name = self.fresh("x")
        body = self.gen(ty, env + [Var(name, bty)], d - 1)
        return "{\n\t%s : %s\n%s\n%s\n}" % (name, bty, ind(name + " = " + paren(e)), ind(body))

    def g_opmatch(self, ty, env, d):
        scr = self.g_op(env, d - 1)
        kn, gn = self.fresh("k"), self.fresh("g")
        a1 = self.gen(ty, env + [Var(kn, I)], d - 1)
        a2 = self.gen(ty, env + [Var(gn, F)], d - 1)
        return "match %s {\n\tInc(%s) => %s\n\tFn(%s) => %s\n}" % (paren(scr), kn, arm(a1), gn, arm(a2))

    # ------------------------------------------------------------------ I64
    def g_int(self, env, d):
        vs = self.vars(env, I)
        opts = ["lit"] + ["var"] * 3 * bool(vs)
        if d > 0:
            opts += ["add", "sub", "dbl", "if", "call", "call", "let", "opmatch", "apply", "apply", "apply",
                     "field", "len", "fold", "hof", "get", "app", "app_fn", "choose", "chmatch", "trymatch"]
            if d > 1:
                opts += ["for", "while"]
        c = self.pick(opts)
        if c == "lit":
            return str(self.r.randint(0, 5))
        if c == "var":
            return self.pick(vs).name
        if c in ("add", "sub"):
            a, b = self.g_int(env, d - 1), self.g_int(env, d - 1)
            return "%s %s %s" % (paren(a), "+" if c == "add" else "-", paren(b))
        if c == "dbl":
            return "%s * 2" % paren(self.g_int(env, d - 1))
        if c == "if":
            return self.g_if(I, env, d)
        if c == "call":
            return self.g_call(I, env, d) or self.g_int(env, 0)
        if c == "let":
            return self.g_let(I, env, d)
        if c == "opmatch":
            return self.g_opmatch(I, env, d)
        if c == "apply":
            f = self.g_fn(env, d - 1)
            return "%s(%s)" % (callee(f), self.g_int(env, d - 1))
        if c == "field":
            return "%s.n" % paren(self.g_rec(env, d - 1))
        if c == "app":
            return "apply(%s, %s)" % (self.g_fn(env, d - 1), self.g_int(env, d - 1))
        if c == "app_fn":
            gn = self.fresh("h")
            body = self.g_int(self.pure(env) + [Var(gn, F)], min(d - 1, 2))
            return "apply(|%s| %s, %s)" % (gn, paren(body), self.g_fn(env, d - 1))
        if c == "choose":
            return "choose(%s, %s, %s)" % (self.g_bool(env, d - 1), self.g_int(env, d - 1), self.g_int(env, d - 1))
        if c == "trymatch":
            vn = self.fresh("v")
            ok = self.g_int(env + [Var(vn, I)], d - 1)
            return "match %s {\n\tOk(%s) => %s\n\tErr(Neg(%s)) => %s\n}" % (
                paren(self.g_try(env, d - 1)), vn, arm(ok), vn + "e", arm(self.g_int(env + [Var(vn + "e", I)], d - 1)))
        if c == "chmatch":
            scr = self.g_chain(env, d - 1)
            gn, kn, rn = self.fresh("g"), self.fresh("k"), self.fresh("r")
            a = self.g_int(env + [Var(gn, F), Var(rn, CH)], d - 1)
            b = self.g_int(env + [Var(kn, I), Var(rn, CH)], d - 1)
            return "match %s {\n\tLink(%s, %s) => %s\n\tAdd(%s, %s) => %s\n\tEnd => %s\n}" % (
                paren(scr), gn, rn, arm(a), kn, rn, arm(b), arm(self.g_int(env, 0)))
        if c == "len":
            return "List.len(%s).to_i64_wrap()" % self.g_list(env, d - 1)
        if c == "fold":
            xs = self.g_list(env, d - 1)
            init = self.g_int(env, d - 1)
            an, yn = self.fresh("acc"), self.fresh("y")
            body = self.g_int(self.pure(env) + [Var(an, I), Var(yn, I)], min(d - 1, 2))
            return "List.fold(%s, %s, |%s, %s| %s)" % (xs, init, an, yn, paren(body))
        if c == "hof":
            hs = [f for f in self.fns if f.params == [F, I] and f.ret == I]
            if not hs:
                return self.g_int(env, 0)
            h = self.pick(hs)
            return "%s(%s, %s)" % (h.name, self.g_fn(env, d - 1), self.g_int(env, d - 1))
        if c == "get":
            xs = self.g_list(env, d - 1)
            vn = self.fresh("e")
            ok = self.g_int(env + [Var(vn, I)], d - 1)
            dflt = self.g_int(env, d - 1)
            return "match List.get(%s, %d) {\n\tOk(%s) => %s\n\tErr(_) => %s\n}" % (
                xs, self.r.randint(0, 2), vn, arm(ok), arm(dflt))
        if c == "for":
            xs = self.g_list(env, d - 1)
            acc, yn = "$" + self.fresh("s"), self.fresh("y")
            init = self.g_int(env, d - 1)
            step = self.g_int(env + [Var(acc, I, True), Var(yn, I)], min(d - 1, 2))
            return "{\n\tvar %s = %s\n\tfor %s in %s {\n\t\t%s = %s\n\t}\n\t%s\n}" % (
                acc, paren(init), yn, xs, acc, paren(step), acc)
        # while: a counted loop whose state is an integer or a closure
        i, acc = "$" + self.fresh("i"), "$" + self.fresh("s")
        n = self.r.randint(0, 4)
        if self.chance(0.5):
            init = self.g_int(env, d - 1)
            step = self.g_int(env + [Var(acc, I, True), Var(i, I, True)], min(d - 1, 2))
            lines = ["%s = %s" % (acc, paren(step))]
            res = acc
        else:
            init = self.g_fn(env, d - 1)
            f = self.g_fn(env, d - 1)
            wn, zn = self.fresh("w"), self.fresh("z")
            lines = ["%s = %s" % (wn, acc), "%s = |%s| %s(%s(%s))" % (acc, zn, callee(f), wn, zn)]
            res = "%s(%s)" % (acc, self.g_int(env, 0))
        # A second loop-carried integer, whose final value the result may or may not read.
        extra = ""
        if self.chance(0.5):
            bn = "$" + self.fresh("b")
            bstep = self.g_int(env + [Var(acc, sty_of(res, acc), True), Var(i, I, True), Var(bn, I, True)], 1)
            lines.append("%s = %s" % (bn, paren(bstep)))
            extra = "\nvar %s = %s" % (bn, self.g_int(env, 0))
            if res == acc and self.chance(0.5):
                res = "%s + %s" % (acc, bn)
        body = "\n".join(lines + ["%s = %s + 1" % (i, i)])
        # The state's first value is bound under an annotation, so the loop never has to infer it.
        sn = self.fresh("init")
        sty = I if res.startswith(acc) and "(" not in res else F
        head = "%s : %s\n%s = %s\nvar %s = 0\nvar %s = %s%s" % (sn, sty, sn, paren(init), i, acc, sn, extra)
        return "{\n%s\n\twhile %s < %d {\n%s\n\t}\n\t%s\n}" % (ind(head), i, n, ind(body, 2), res)

    # ------------------------------------------------------------------ Bool
    def g_bool(self, env, d):
        d = max(d, 0)
        c = self.pick(["lt", "lt", "eq", "lit", "empty"])
        if c == "lit":
            return self.pick(["Bool.True", "Bool.False"])
        if c == "empty":
            return "List.is_empty(%s)" % self.g_list(env, min(d, 1))
        a, b = self.g_int(env, min(d, 2)), self.g_int(env, min(d, 2))
        return "%s %s %s" % (paren(a), "<" if c == "lt" else "==", paren(b))

    # ------------------------------------------------------------------ I64 -> I64
    def g_fn(self, env, d):
        vs = self.vars(env, F, pure=True)
        opts = ["lam", "lam"] + ["var"] * 2 * bool(vs)
        if d > 0:
            opts += ["call", "call", "if", "opmatch", "compose", "field", "curry", "let", "comp", "konst", "choose",
                     "app"]
        c = self.pick(opts)
        if c == "lam":
            yn = self.fresh("y")
            body = self.g_int(self.pure(env) + [Var(yn, I)], min(max(d - 1, 0), 2))
            return "|%s| %s" % (yn, paren(body))
        if c == "var":
            return self.pick(vs).name
        if c == "call":
            return self.g_call(F, env, d) or self.g_fn(env, 0)
        if c == "if":
            return self.g_if(F, env, d)
        if c == "opmatch":
            return self.g_opmatch(F, env, d)
        if c == "let":
            return self.g_let(F, env, d)
        if c == "field":
            return "%s.f" % paren(self.g_rec(env, d - 1))
        if c == "comp":
            return "comp(%s, %s)" % (self.g_fn(env, d - 1), self.g_fn(env, d - 1))
        if c == "konst":
            return "konst(%s)" % self.g_int(env, d - 1)
        if c == "choose":
            return "choose(%s, %s, %s)" % (self.g_bool(env, d - 1), self.g_fn(env, d - 1), self.g_fn(env, d - 1))
        if c == "app":
            an = self.fresh("a")
            body = self.g_fn(self.pure(env) + [Var(an, I)], d - 1)
            return "apply(|%s| %s, %s)" % (an, paren(body), self.g_int(env, d - 1))
        if c == "curry":
            an, yn = self.fresh("a"), self.fresh("y")
            body = self.g_int(self.pure(env) + [Var(an, I), Var(yn, I)], 1)
            return "(|%s| |%s| %s)(%s)" % (an, yn, paren(body), self.g_int(env, d - 1))
        f, g = self.g_fn(env, d - 1), self.g_fn(env, d - 1)
        fn_, gn_, yn = self.fresh("cf"), self.fresh("cg"), self.fresh("y")
        return "{\n\t%s : I64 -> I64\n%s\n\t%s : I64 -> I64\n%s\n\t|%s| %s(%s(%s))\n}" % (
            fn_, ind(fn_ + " = " + paren(f)), gn_, ind(gn_ + " = " + paren(g)), yn, gn_, fn_, yn)

    # ------------------------------------------------------------------ List(I64)
    def g_list(self, env, d):
        vs = self.vars(env, L)
        opts = ["lit", "lit"] + ["var"] * 2 * bool(vs)
        if d > 0:
            opts += ["map", "map", "append", "call", "if"]
        c = self.pick(opts)
        if c == "lit":
            return "[%s]" % ", ".join(self.g_int(env, 0) for _ in range(self.r.randint(0, 3)))
        if c == "var":
            return self.pick(vs).name
        if c == "map":
            return "List.map(%s, %s)" % (self.g_list(env, d - 1), self.g_fn(env, d - 1))
        if c == "append":
            return "List.append(%s, %s)" % (self.g_list(env, d - 1), self.g_int(env, d - 1))
        if c == "call":
            return self.g_call(L, env, d) or "[]"
        return self.g_if(L, env, d)

    # ------------------------------------------------------------------ Op
    def g_op(self, env, d):
        vs = self.vars(env, OP)
        opts = ["inc", "fn", "fn"] + ["var"] * 2 * bool(vs)
        if d > 0:
            opts += ["call", "if"]
        c = self.pick(opts)
        if c == "inc":
            return "Inc(%s)" % self.g_int(env, max(d - 1, 0))
        if c == "fn":
            return "Fn(%s)" % self.g_fn(env, max(d - 1, 0))
        if c == "var":
            return self.pick(vs).name
        if c == "call":
            return self.g_call(OP, env, d) or "Inc(1)"
        if self.chance(0.5):
            return "choose(%s, %s, %s)" % (self.g_bool(env, d - 1), self.g_op(env, d - 1), self.g_op(env, d - 1))
        return self.g_if(OP, env, d)

    # ------------------------------------------------------------------ { n : I64, f : I64 -> I64 }
    def g_rec(self, env, d):
        vs = self.vars(env, R)
        opts = ["lit", "lit"] + ["var"] * 2 * bool(vs)
        if d > 0:
            opts += ["update", "call", "if"]
        c = self.pick(opts)
        if c == "lit":
            return "{ n: %s, f: %s }" % (self.g_int(env, max(d - 1, 0)), self.g_fn(env, max(d - 1, 0)))
        if c == "var":
            return self.pick(vs).name
        if c == "update":
            base = self.g_rec(env, d - 1)
            if self.chance(0.5):
                return "{ ..%s, n: %s }" % (paren(base), self.g_int(env, d - 1))
            return "{ ..%s, f: %s }" % (paren(base), self.g_fn(env, d - 1))
        if c == "call":
            return self.g_call(R, env, d) or "{ n: 0, f: |q| q }"
        return self.g_if(R, env, d)

    # ------------------------------------------------------------------ Chain := [End, Link(I64 -> I64, Chain), Add(I64, Chain)]
    def g_chain(self, env, d):
        vs = self.vars(env, CH)
        opts = ["end", "link", "add"] + ["var"] * 2 * bool(vs)
        if d > 0:
            opts += ["call", "if", "link", "add"]
        c = self.pick(opts)
        if c == "end":
            return "End"
        if c == "var":
            return self.pick(vs).name
        if c == "link":
            return "Link(%s, %s)" % (self.g_fn(env, max(d - 1, 0)), self.g_chain(env, max(d - 1, 0)))
        if c == "add":
            return "Add(%s, %s)" % (self.g_int(env, max(d - 1, 0)), self.g_chain(env, max(d - 1, 0)))
        if c == "call":
            return self.g_call(CH, env, d) or "End"
        return self.g_if(CH, env, d)

    # ------------------------------------------------------------------ program
    def function(self, name, params, ret, rec):
        """A top-level function. `rec` is False, "fuel" (the first parameter counts down) or
        "chain" (the first parameter is a Chain matched on, recursing on its tail)."""
        f = Fn(name, params, ret, rec)
        first = {"fuel": "fuel", "chain": "chain"}.get(rec)
        pnames = [first if first and i == 0 else self.fresh("p") for i in range(len(params))]
        env = [Var(n, t) for n, t in zip(pnames, params)]
        self.cur = f
        if rec == "chain":
            gn, kn = self.fresh("g"), self.fresh("k")
            link = self.gen(ret, env + [Var(gn, F), Var("rest", CH)], self.depth - 1)
            add = self.gen(ret, env + [Var(kn, I), Var("rest", CH)], self.depth - 1)
            self.cur = None  # the base case never recurses
            base = self.gen(ret, env, 1)
            body = "match chain {\n\tEnd => %s\n\tLink(%s, rest) => %s\n\tAdd(%s, rest) => %s\n}" % (
                arm(base), gn, arm(link), kn, arm(add))
        else:
            body = self.gen(ret, env, self.depth)
        if rec == "fuel":
            self.cur = None  # the base case never recurses
            base = self.gen(ret, env, 1)
            body = "if fuel <= 0 %s else %s" % (
                paren(base) if "\n" not in base else "{\n" + ind(base) + "\n}",
                paren(body) if "\n" not in body else "{\n" + ind(body) + "\n}")
        self.cur = None
        def ann(t):
            return "(" + t + ")" if "->" in t else t
        sig = ", ".join(ann(t) for t in params)
        text = "%s : %s -> %s\n%s = |%s| %s\n" % (name, sig, ann(ret), name, ", ".join(pnames), arm(body) if "\n" in body else body)
        self.fns.append(f)
        return text

    def program(self):
        out = [PRELUDE]
        if self.chance(0.7):
            out.append(self.function("apply_twice", [F, I], I, False))
        for k in range(self.nfuns):
            rec = self.pick([False, False, False, "fuel", "chain", "chain"])
            params = [self.pick(PARAM_TYPES) for _ in range(self.r.randint(1, 3))]
            if rec:
                params = [I if rec == "fuel" else CH] + params[:2]
            out.append(self.function(self.fresh("fun"), params, self.pick(RET_TYPES), rec))
        entries = []
        for k in range(self.nentries):
            name = "entry%d" % k
            out.append(self.function(name, [I], I, False))
            entries.append(name)
        for name in entries:
            for x in (0, 3):
                out.append("expect {\n\tdbg %s(%d)\n\tBool.True\n}\n" % (name, x))
        return "\n".join(out)


def program(seed, **kw):
    return Gen(seed, **kw).program()


if __name__ == "__main__":
    import sys
    print(program(int(sys.argv[1]) if len(sys.argv) > 1 else 0))
