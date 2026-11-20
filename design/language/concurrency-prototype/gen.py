"""Typed random program generator.

Programs are composed from small typed constructs, as AGENTS.md requires of
fuzzers: constants, arithmetic, tuples, tags, lists, hosted calls (suspending
and not), direct calls, self-recursion and tail loops bounded by fuel,
closures called directly (erased calls under .boxy), frame-replacing calls
through closures (deferred erased calls under .boxy), tail loops whose cycle
passes through a closure, every combinator, concurrent loops over plans
(entries, concatenation, limits, shields) whose bodies exit early, return an
error, spawn more work or run nested loops, one-shot session channels that
carry a stream of numbers by continuation passing (producers and consumers
called directly or packed into closures, which then flow into loops and
combinators like any other closure; producers that end, drop their end or
crash; consumers that stop early), requests that carry the end their reply
goes to (a client sends a reply end, a server answers it with the Out end of
a stream back to itself, and the client produces into it: the shape of a
permit returned to its owner), branches, matches and crashes. Nothing is
a handwritten scenario.

Types:
    I  Int            S  Str           T  Try(Int, Str)
    L  List(Int)      P  (Int, Str)    Q  (Int, Int)
    TL Try(List(Int), Str)             C  () => Int      CT () => Try(Int, Str)
    SO Out(Stream)     SI In(Stream)   where Stream = [More(Int, In(Stream)), Done]
    QO Out(Req)        QI In(Req)      where Req = Out(Grant), Grant = Out(Stream)
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
        self.sessions = rng.random() < 0.5  # half the programs use no channels, so G4 still sees many

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
                 "call", "callc", "all", "all_ok", "race", "both", "supervise", "loop", "session"]
        weights = [2, 3, 1, 2, 1, 1, 1, 1, 4, 2, 1, 2, 1, 2, 1, 1, 1, 1, 3, 2 if self.sessions else 0]
        if depth >= 3:
            weights = [w if k not in ("all", "all_ok", "race", "both", "supervise", "callc", "call", "loop",
                                      "session") else 0
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
        elif k == "loop":
            self.loop(out, env, depth)
        elif k == "session":
            self.session(out, env, depth)
        elif k == "supervise":
            f = self.need(out, env, "C", depth)
            m = self.let(out, env, "I", ("int", rng.randint(0, 2)))
            self.let(out, env, "I", ("call", "supervise!", [m, f]))
            self.features.add("supervise!")

    # ------------------------------------------------------------ loops
    def plan(self, out, env, ty, depth, cs=None):
        """A plan over closures of type ty: entries, maybe limited, shielded or
        concatenated with another plan."""
        rng = self.rng
        cs = cs or [self.need(out, env, ty, depth) for _ in range(rng.randint(1, 3))]
        fs = self.let(out, env, "LC", ("list", cs))
        p = self.let(out, env, "PLAN", ("call", "concurrently", [fs]))
        r = rng.random()
        if r < 0.3:
            n = self.let(out, env, "I", ("int", rng.randint(1, 2)))
            p = self.let(out, env, "PLAN", ("call", "limit", [p, n]))
            self.features.add("limit")
        if rng.random() < 0.25:
            p = self.let(out, env, "PLAN", ("call", "shielded", [p]))
            self.features.add("shield")
        if rng.random() < 0.3:
            q = self.plan(out, env, ty, depth)
            p = self.let(out, env, "PLAN", ("call", "concat", [p, q]))
            self.features.add("concat")
        return p

    def loop(self, out, env, depth, cs=None):
        """for! v in plan { ... }, lowered as the compiler lowers it."""
        rng = self.rng
        trying = cs is None and rng.random() < 0.3  # arrivals are Try values; an Err exits, as `?` does
        ty = "CT" if trying else "C"
        p = self.plan(out, env, ty, depth, cs)
        g = self.let(out, env, "G", ("group", p))
        acc = self.let(out, env, "I", ("int", 0))
        budget = self.let(out, env, "I", ("int", rng.choice([0, 0, 1, 2])))
        sp = self.need(out, env, ty, depth)
        name = self.fresh("loop")
        exits = rng.random() < 0.4
        lg, lacc, lb, lsp = (self.fresh("g"), self.fresh("acc"), self.fresh("budget"), self.fresh("sp"))
        a, v, m = self.fresh("a"), self.fresh("v"), self.fresh("m")
        again = lambda o, acc2, b: Block(o, ("tail", name, [lg, acc2, b, lsp]))

        state = {"spawn": False}

        def cont(o, benv, acc2):
            """Back to the loop head, perhaps after spawning one more entry."""
            if rng.random() < 0.5:
                state["spawn"] = True
                z = self.let(o, benv, "I", ("int", 0))
                c = self.let(o, benv, "I", ("prim", "lt", [z, lb]))
                so, one = [], self.fresh()
                b2 = self.fresh()
                so += [("let", one, ("int", 1)), ("let", b2, ("prim", "sub", [lb, one])),
                       ("let", self.fresh("u"), ("spawn_in", lg, lsp))]
                return Block(o, ("if", c, again(so, acc2, b2), again([], acc2, lb)))
            return again(o, acc2, lb)

        def body(o, benv, acc2):
            # Statements of the body; they see only what the body builds.
            for _ in range(rng.randint(0, 2)):
                self.statement(o, benv, depth + 1, name)
            if not exits:
                return cont(o, benv, acc2)
            k = self.let(o, benv, "I", ("int", rng.randint(0, 12)))
            c = self.let(o, benv, "I", ("prim", "lt", [k, acc2]))
            xo = [("let", self.fresh("u"), ("call", "close_loop", [lg]))]
            res = acc2
            if trying:
                res = self.fresh("r")
                xo.append(("let", res, ("tag", "Ok", [acc2])))
            self.features.add("loop exit")
            return Block(o, ("if", c, Block(xo, ("ret", res)), cont([], dict(benv), acc2)))

        benv = {lsp: ty}
        bo = []
        if trying:
            ok, err = self.fresh("ok"), self.fresh("e")
            acc2 = self.fresh("acc")
            ok_block = body([("let", acc2, ("prim", "add", [lacc, ok]))], dict(benv), acc2)
            eo = [("let", self.fresh("u"), ("call", "close_loop", [lg]))]
            r = self.fresh("r")
            eo.append(("let", r, ("tag", "Err", [err])))
            arrived = Block([], ("match", v, [("Ok", [ok], ok_block), ("Err", [err], Block(eo, ("ret", r)))]))
            self.features.add("loop error exit")
            end_o = []
            fin = self.fresh("r")
            end_o.append(("let", fin, ("tag", "Ok", [lacc])))
            ended = Block(end_o, ("ret", fin))
        else:
            acc2 = self.fresh("acc")
            arrived = body([("let", acc2, ("prim", "add", [lacc, v]))], dict(benv), acc2)
            ended = Block([], ("ret", lacc))
        u = self.fresh("u")
        head = Block([("let", a, ("next", lg))], ("match", a, [
            ("Arrived", [v], arrived),
            ("Crashed", [m], Block([], ("crash", m))),
            ("Cancelled", [], Block([("let", u, ("str", "unreachable"))], ("crash", u))),
            ("Ended", [], ended),
        ]))
        self.procs[name] = Proc(name, [lg, lacc, lb, lsp], head, True)
        self.let(out, env, "T" if trying else "I", ("call", name, [g, acc, budget, sp]))
        self.features.add("loop")
        if state["spawn"]:
            self.features.add("loop spawn")
        if exits or trying or state["spawn"]:
            self.features.add("arrival order")

    # ------------------------------------------------------------ sessions
    def session(self, out, env, depth):
        """Make a channel and wire its ends: each end goes to a producer or a
        consumer called directly, or packed into a closure that later code may
        run in a loop, a combinator, or never (dropping its end), or is left
        for a later statement, or is dropped unused."""
        rng = self.rng
        self.features.add("session")
        sides = (("producer", "SO"), ("consumer", "SI"))
        if rng.random() < 0.4:
            sides = (("client", "QO"), ("server", "QI"))
            self.features.add("session request")
        pr = self.let(out, env, "SP", ("pair",))
        ends = [(side, self.let(out, env, ty, ("field", pr, k))) for k, (side, ty) in enumerate(sides)]
        # An end left by an earlier statement may be wired now instead.
        for side, ty in sides:
            old = [v for v, t in env.items() if t == ty and v != ends[0][1] and v != ends[1][1]]
            if old and rng.random() < 0.3:
                ends.append((side, rng.choice(old)))
        rng.shuffle(ends)
        made = []
        for side, end in ends:
            r = rng.random()
            if r < 0.15:
                continue  # left in scope for a later statement, or dropped
            n = self.let(out, env, "I", ("int", rng.randint(0, 3)))
            name = {"producer": self.producer, "consumer": self.consumer,
                    "client": self.client, "server": self.server}[side](depth)
            if r < 0.65:
                made.append(self.let(out, env, "C", ("clo", name, [end, n])))
                self.features.add(f"session {side} closure")
            else:
                self.let(out, env, "I", ("call", name, [end, n]))
                self.features.add(f"session {side} call")
            del env[end]  # used
        # Both ends' closures may run side by side at once, in either order.
        r = rng.random()
        if len(made) >= 2 and r < 0.6:
            rng.shuffle(made)
            if r < 0.25:
                fs = self.let(out, env, "LC", ("list", made))
                self.let(out, env, "L", ("call", "all!", [fs]))
                self.features.add("all!")
            elif r < 0.4:
                fs = self.let(out, env, "LC", ("list", made[1:]))
                self.let(out, env, "I", ("call", "race!", [made[0], fs]))
                self.features.add("race!")
            else:
                self.loop(out, env, depth, made)

    def producer(self, depth):
        """prod(o, n): send n numbers, each with the In end of the next step;
        then send Done, or end without sending (the consumer gets Cancelled),
        or crash."""
        rng = self.rng
        name = self.fresh("prod")
        o, n = self.fresh("o"), self.fresh("n")
        env = {n: "I"}
        out = []
        zero = self.let(out, env, "I", ("int", 0))
        z = self.let(out, env, "I", ("prim", "eq", [n, zero]))
        # the last step
        lo, lenv = [], dict(env)
        fin = rng.random()
        if fin < self.crash_rate * 3:
            s = self.need(lo, lenv, "S", depth + 1)
            last = Block(lo, ("crash", s))
            self.features.add("crash")
        else:
            if fin < 0.75:
                d = self.let(lo, lenv, "M", ("tag", "Done", []))
                lo.append(("let", self.fresh("u"), ("send", o, d)))
            else:
                self.features.add("session drop")
            last = Block(lo, ("ret", self.need(lo, lenv, "I", depth + 1)))
        # one more step
        so, senv = [], dict(env)
        for _ in range(rng.randint(0, 2)):
            self.statement(so, senv, depth + 1, name)
        a = self.need(so, senv, "I", depth + 1)
        pr = self.let(so, senv, "SP", ("pair",))
        o2 = self.let(so, senv, "SO", ("field", pr, 0))
        i2 = self.let(so, senv, "SI", ("field", pr, 1))
        m = self.let(so, senv, "M", ("tag", "More", [a, i2]))
        so.append(("let", self.fresh("u"), ("send", o, m)))
        one = self.let(so, senv, "I", ("int", 1))
        n2 = self.let(so, senv, "I", ("prim", "sub", [n, one]))
        step = Block(so, ("tail", name, [o2, n2]))
        self.procs[name] = Proc(name, [o, n], Block(out, ("if", z, last, step)), True)
        return name

    def consumer(self, depth):
        """cons(i, acc): receive until Done or Cancelled, adding up the
        numbers; perhaps stop early, dropping the rest of the stream."""
        rng = self.rng
        name = self.fresh("cons")
        i, acc = self.fresh("i"), self.fresh("acc")
        r, m, a, i2 = self.fresh("r"), self.fresh("m"), self.fresh("x"), self.fresh("i")
        acc2 = self.fresh("acc")
        mo = [("let", acc2, ("prim", "add", [acc, a]))]
        menv = {acc2: "I"}
        for _ in range(rng.randint(0, 2)):
            self.statement(mo, menv, depth + 1, name)
        again = ("tail", name, [i2, acc2])
        if rng.random() < 0.3:
            self.features.add("session early exit")
            k = self.let(mo, menv, "I", ("int", rng.randint(0, 12)))
            c = self.let(mo, menv, "I", ("prim", "lt", [k, acc2]))
            more = Block(mo, ("if", c, Block([], ("ret", acc2)), Block([], again)))
        else:
            more = Block(mo, again)
        got = Block([], ("match", m, [("More", [a, i2], more), ("Done", [], Block([], ("ret", acc)))]))
        head = Block([("let", r, ("recv", i))], ("match", r, [
            ("Got", [m], got),
            ("Cancelled", [], Block([], ("ret", acc))),
        ]))
        self.procs[name] = Proc(name, [i, acc], head, True)
        return name

    def client(self, depth):
        """client(q, n): send a request carrying the Out end of the reply
        channel, receive the grant (the Out end of a stream back to the
        server), and produce into it. Cancelled at the receive, the reply
        channel is abandoned and the grant sent to it must go back to the
        server as Cancelled."""
        name = self.fresh("client")
        q, n = self.fresh("q"), self.fresh("n")
        env = {n: "I"}
        out = []
        pr = self.let(out, env, "SP", ("pair",))
        go = self.let(out, env, "GO", ("field", pr, 0))
        gi = self.let(out, env, "GI", ("field", pr, 1))
        out.append(("let", self.fresh("u"), ("send", q, go)))
        r, so, res = self.fresh("r"), self.fresh("o"), self.fresh("x")
        prod = self.producer(depth)
        got = Block([("let", res, ("call", prod, [so, n]))], ("ret", res))
        out.append(("let", r, ("recv", gi)))
        self.procs[name] = Proc(name, [q, n], Block(out, ("match", r, [
            ("Got", [so], got),
            ("Cancelled", [], Block([], ("ret", n))),
        ])), True)
        return name

    def server(self, depth):
        """server(q, n): receive a request, answer it with the Out end of a
        stream back to itself, and consume that stream: the client's producer
        sends into it, or drops it, or the grant never arrives and dropping
        it delivers Cancelled."""
        name = self.fresh("server")
        q, n = self.fresh("q"), self.fresh("n")
        r, go, res = self.fresh("r"), self.fresh("g"), self.fresh("x")
        so_ = []
        senv = {n: "I"}
        pr = self.let(so_, senv, "SP", ("pair",))
        so = self.let(so_, senv, "SO", ("field", pr, 0))
        si = self.let(so_, senv, "SI", ("field", pr, 1))
        so_.append(("let", self.fresh("u"), ("send", go, so)))
        cons = self.consumer(depth)
        so_.append(("let", res, ("call", cons, [si, n])))
        head = Block([("let", r, ("recv", q))], ("match", r, [
            ("Got", [go], Block(so_, ("ret", res))),
            ("Cancelled", [], Block([], ("ret", n))),
        ]))
        self.procs[name] = Proc(name, [q, n], head, True)
        return name

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
