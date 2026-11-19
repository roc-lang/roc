"""Typed random program generator.

Programs are composed from small typed constructs, as AGENTS.md requires of
fuzzers: constants, arithmetic, tuples, tags, lists, hosted calls (suspending
and not), direct calls, self-recursion and tail loops bounded by fuel,
closures called directly (erased calls under .boxy), frame-replacing calls
through closures (deferred erased calls under .boxy), tail loops whose cycle
passes through a closure, every combinator, branches, matches and crashes.
Nothing is a handwritten scenario.

Types:
    I  Int            S  Str           T  Try(Int, Str)
    L  List(Int)      P  (Int, Str)    Q  (Int, Int)
    TL Try(List(Int), Str)             C  () => Int      CT () => Try(Int, Str)
"""

from __future__ import annotations

import random

from ir import Block, Proc

TYPES = ["I", "S", "T", "L", "P", "Q", "TL", "C", "CT"]


class Gen:
    def __init__(self, rng, crash_rate=0.04):
        self.rng = rng
        self.crash_rate = crash_rate
        self.procs = {}
        self.n = 0
        self.funcs = []  # (name, param types, return type)
        self.features = set()

    def fresh(self, base="v"):
        self.n += 1
        return f"{base}{self.n}"

    # ------------------------------------------------------------ values
    def let(self, out, env, ty, e):
        x = self.fresh()
        out.append(("let", x, e))
        env[x] = ty
        return x

    def pick(self, env, ty):
        vs = [v for v, t in env.items() if t == ty]
        return self.rng.choice(vs) if vs else None

    def need(self, out, env, ty, depth):
        """A variable of type ty: an existing one, or a freshly built one."""
        v = self.pick(env, ty)
        if v is not None and self.rng.random() < 0.8:
            return v
        return self.build(out, env, ty, depth)

    def build(self, out, env, ty, depth):
        rng = self.rng
        if ty == "I":
            return self.let(out, env, "I", ("int", rng.randint(0, 9)))
        if ty == "S":
            return self.let(out, env, "S", ("str", rng.choice(["a", "bb", "ccc"])))
        if ty == "T":
            if rng.random() < 0.7:
                return self.let(out, env, "T", ("tag", "Ok", [self.need(out, env, "I", depth)]))
            return self.let(out, env, "T", ("tag", "Err", [self.need(out, env, "S", depth)]))
        if ty == "L":
            return self.let(out, env, "L", ("list", [self.need(out, env, "I", depth) for _ in range(rng.randint(0, 2))]))
        if ty == "P":
            return self.let(out, env, "P", ("tup", [self.need(out, env, "I", depth), self.need(out, env, "S", depth)]))
        if ty == "Q":
            return self.let(out, env, "Q", ("tup", [self.need(out, env, "I", depth), self.need(out, env, "I", depth)]))
        if ty == "TL":
            return self.let(out, env, "TL", ("tag", "Ok", [self.need(out, env, "L", depth)]))
        if ty in ("C", "CT"):
            return self.closure(out, env, ty, depth)
        raise ValueError(ty)

    def closure(self, out, env, ty, depth):
        """A closure over a fresh thunk procedure capturing some of env."""
        rng = self.rng
        avail = [v for v, t in env.items() if t not in ("C", "CT") or depth < 2]
        caps = rng.sample(avail, min(len(avail), rng.randint(0, 3)))
        name = self.fresh("g")
        params = [self.fresh("p") for _ in caps]
        penv = {p: env[c] for p, c in zip(params, caps)}
        ret = "I" if ty == "C" else "T"
        body = self.block(penv, ret, depth + 1, name, budget=rng.randint(1, 4))
        self.procs[name] = Proc(name, params, body, True)
        return self.let(out, env, ty, ("clo", name, list(caps)))

    # ------------------------------------------------------------ blocks
    def block(self, env, ret, depth, me, budget, fuel=None):
        rng = self.rng
        env = dict(env)
        out = []
        for _ in range(budget):
            self.statement(out, env, depth, me)
        # terminator
        r = rng.random()
        if depth < 3 and r < 0.25 and self.pick(env, "T"):
            return self.match_term(out, env, ret, depth, me, fuel)
        if depth < 3 and r < 0.45 and self.pick(env, "I"):
            return self.if_term(out, env, ret, depth, me, fuel)
        if ret == "I" and r < 0.6 and self.pick(env, "C"):
            self.features.add("tailc")
            return Block(out, ("tailc", self.pick(env, "C"), []))
        return Block(out, ("ret", self.need(out, env, ret, depth)))

    def match_term(self, out, env, ret, depth, me, fuel):
        x = self.pick(env, "T")
        rng = self.rng
        a, b = self.fresh("ok"), self.fresh("err")
        e1 = dict(env)
        e1[a] = "I"
        e2 = dict(env)
        e2[b] = "S"
        arms = [("Ok", [a], self.block(e1, ret, depth + 1, me, rng.randint(0, 3), fuel)),
                ("Err", [b], self.maybe_crash(e2, ret, depth + 1, me, fuel))]
        rng.shuffle(arms)
        self.features.add("match")
        return Block(out, ("match", x, arms))

    def if_term(self, out, env, ret, depth, me, fuel):
        rng = self.rng
        a = self.pick(env, "I")
        k = self.let(out, env, "I", ("int", rng.randint(0, 9)))
        c = self.let(out, env, "I", ("prim", rng.choice(["lt", "eq"]), [a, k]))
        self.features.add("if")
        return Block(out, ("if", c, self.block(env, ret, depth + 1, me, rng.randint(0, 3), fuel),
                           self.maybe_crash(env, ret, depth + 1, me, fuel)))

    def maybe_crash(self, env, ret, depth, me, fuel):
        if self.rng.random() < self.crash_rate:
            out = []
            env = dict(env)
            s = self.need(out, env, "S", depth)
            self.features.add("crash")
            return Block(out, ("crash", s))
        return self.block(env, ret, depth, me, self.rng.randint(0, 3), fuel)

    def statement(self, out, env, depth, me):
        rng = self.rng
        kinds = ["const", "prim", "tup", "field", "tag", "list", "len", "push", "host", "host", "log",
                 "call", "callc", "all", "all_ok", "race", "both", "supervise"]
        weights = [2, 3, 1, 2, 1, 1, 1, 1, 4, 2, 1, 2, 1, 2, 1, 1, 1, 1]
        if depth >= 3:
            weights = [w if k not in ("all", "all_ok", "race", "both", "supervise", "callc", "call") else 0
                       for k, w in zip(kinds, weights)]
        k = rng.choices(kinds, weights)[0]
        if k == "const":
            self.build(out, env, rng.choice(["I", "S"]), depth)
        elif k == "prim":
            a, b = self.need(out, env, "I", depth), self.need(out, env, "I", depth)
            self.let(out, env, "I", ("prim", rng.choice(["add", "sub", "lt", "eq"]), [a, b]))
        elif k == "tup":
            self.build(out, env, rng.choice(["P", "Q"]), depth)
        elif k == "field":
            ty = rng.choice(["P", "Q"])
            x = self.pick(env, ty)
            if x:
                i = rng.randint(0, 1)
                self.let(out, env, "I" if i == 0 or ty == "Q" else "S", ("field", x, i))
        elif k == "tag":
            self.build(out, env, "T", depth)
        elif k == "list":
            self.build(out, env, "L", depth)
        elif k == "len":
            x = self.pick(env, "L")
            if x:
                self.let(out, env, "I", ("len", x))
        elif k == "push":
            x = self.pick(env, "L")
            if x:
                self.let(out, env, "L", ("push", x, self.need(out, env, "I", depth)))
        elif k == "host":
            name, arg, ty = rng.choice([("read", "I", "T"), ("slow", "I", "I"), ("fetch", "I", "S"),
                                        ("tick", "I", "I")])
            self.let(out, env, ty, ("host", name, [self.need(out, env, arg, depth)]))
            self.features.add("host")
        elif k == "log":
            self.let(out, env, "I", ("host", "log", [self.need(out, env, "S", depth)]))
        elif k == "call":
            if self.funcs:
                name, ptys, rty = rng.choice(self.funcs)
                args = [self.need(out, env, t, depth) for t in ptys]
                fuel = self.let(out, env, "I", ("int", rng.randint(0, 3)))
                self.let(out, env, rty, ("call", name, args + [fuel]))
                self.features.add("call")
        elif k == "callc":
            c = self.need(out, env, "C", depth)
            self.let(out, env, "I", ("callc", c, [], True))
            self.features.add("callc")
        elif k == "all":
            cs = [self.need(out, env, "C", depth) for _ in range(rng.randint(1, 3))]
            fs = self.let(out, env, "LC", ("list", cs))
            self.let(out, env, "L", ("call", "all!", [fs]))
            self.features.add("all!")
        elif k == "all_ok":
            cs = [self.need(out, env, "CT", depth) for _ in range(rng.randint(1, 3))]
            fs = self.let(out, env, "LC", ("list", cs))
            self.let(out, env, "TL", ("call", "all_ok!", [fs]))
            self.features.add("all_ok!")
        elif k == "race":
            f = self.need(out, env, "C", depth)
            cs = [self.need(out, env, "C", depth) for _ in range(rng.randint(0, 2))]
            fs = self.let(out, env, "LC", ("list", cs))
            self.let(out, env, "I", ("call", "race!", [f, fs]))
            self.features.add("race!")
        elif k == "both":
            f, g = self.need(out, env, "C", depth), self.need(out, env, "C", depth)
            self.let(out, env, "Q", ("call", "both!", [f, g]))
            self.features.add("both!")
        elif k == "supervise":
            f = self.need(out, env, "C", depth)
            m = self.let(out, env, "I", ("int", rng.randint(0, 2)))
            self.let(out, env, "I", ("call", "supervise!", [m, f]))
            self.features.add("supervise!")

    # ------------------------------------------------------------ procs
    def func(self, k):
        """f{k}(params..., fuel): may recurse on itself with fuel - 1, by a tail
        call or an ordinary call."""
        rng = self.rng
        name = f"f{k}"
        ptys = [rng.choice(["I", "S", "T", "L", "C"]) for _ in range(rng.randint(0, 2))]
        rty = rng.choice(["I", "S", "T", "L"])
        params = [self.fresh("a") for _ in ptys]
        env = dict(zip(params, ptys))
        fuel = self.fresh("fuel")
        env[fuel] = "I"
        out = []
        zero = self.let(out, env, "I", ("int", 0))
        more = self.let(out, env, "I", ("prim", "lt", [zero, fuel]))
        one = self.let(out, env, "I", ("int", 1))
        f2 = self.let(out, env, "I", ("prim", "sub", [fuel, one]))
        # recursive branch
        rec_env = dict(env)
        rout = []
        for _ in range(rng.randint(0, 3)):
            self.statement(rout, rec_env, 1, name)
        args = [self.need(rout, rec_env, t, 1) for t in ptys]
        r = rng.random()
        if r < 0.2:
            # A tail cycle through a closure: f -tailc-> step -tail-> f.
            self.features.add("tail via closure")
            step = self.fresh("step")
            sparams = [self.fresh("s") for _ in ptys] + [self.fresh("sf")]
            senv = dict(zip(sparams, ptys + ["I"]))
            sout = []
            for _ in range(rng.randint(0, 3)):
                self.statement(sout, senv, 2, step)
            sargs = [self.need(sout, senv, t, 2) for t in ptys]
            self.procs[step] = Proc(step, sparams, Block(sout, ("tail", name, sargs + [sparams[-1]])), True)
            c = self.fresh("k")
            rout.append(("let", c, ("clo", step, args + [f2])))
            rec = Block(rout, ("tailc", c, []))
        elif r < 0.6:
            self.features.add("tail")
            rec = Block(rout, ("tail", name, args + [f2]))
        else:
            r = self.fresh()
            rout.append(("let", r, ("call", name, args + [f2])))
            rec_env[r] = rty
            for _ in range(rng.randint(0, 2)):
                self.statement(rout, rec_env, 1, name)
            rec = Block(rout, ("ret", self.need(rout, rec_env, rty, 1)))
        base = self.block(env, rty, 1, name, rng.randint(1, 4))
        body = Block(out, ("if", more, rec, base))
        self.procs[name] = Proc(name, params + [fuel], body, True)
        self.funcs.append((name, ptys, rty))

    def program(self):
        rng = self.rng
        for k in range(rng.randint(0, 3)):
            self.func(k)
        body = self.block({}, rng.choice(["I", "S", "T", "L", "TL", "Q"]), 0, "main", rng.randint(2, 7))
        self.procs["main"] = Proc("main", [], body, True)
        return self.procs


def gen_program(seed, crash_rate=0.04):
    g = Gen(random.Random(seed), crash_rate)
    procs = g.program()
    return procs, g.features
