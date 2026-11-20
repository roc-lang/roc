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
permit returned to its owner), mailboxes (posters that post some numbers
through a shared Post, end early or crash holding it; readers that take
until Closed or stop early, dropping the inbox), semaphore cells (workers
that acquire a permit, work, and release it, crash holding it, or drop it
at once), shared variables (workers that capture one or two and run atomic
steps on them: increments, peeks, takes that wait for a nonzero value, moves
between two variables, reads; takes paired with a deferred or errdeferred
give, with work, crashes and cancellation in between; deferred steps that
crash and poison), cells whose states hold handles to cells, their own
included, and jobs that capture cells (workers and jobs that link, push
jobs, pop and run the newest item, clear, and make cells of their own: the
shapes that form reference cycles through states, which the runtime breaks
when no live task holds a cell), branches, matches and crashes. Nothing is a
handwritten scenario.

Types:
    I  Int            S  Str           T  Try(Int, Str)
    L  List(Int)      P  (Int, Str)    Q  (Int, Int)
    TL Try(List(Int), Str)             C  () => Int      CT () => Try(Int, Str)
    SO Out(Stream)     SI In(Stream)   where Stream = [More(Int, In(Stream)), Done]
    QO Out(Req)        QI In(Req)      where Req = Out(Grant), Grant = Out(Stream)
    PO Post(Int)       IB Inbox(Int)   SEM a semaphore cell
    SV Shared(Int)
    SN Shared(Node)    where Node = [Nil, Cons(Item, Node)], Item = [H(Shared(Node)), J(() => Int)]
"""

from __future__ import annotations

import random

from ir import Block, Proc, parse

# The pure procedures of generated steps.
STEP_PROCS = parse(r"""
(proc gs_take (n) pure
  (let z (eq n 0))
  (if z (block (ret (tag Wait))) (block (ret (tag Set 0 (sub n 1))))))
(proc gs_give (n) pure (ret (tag Set 0 (add n 1))))
(proc gs_inc (n k) pure (ret (tag Set n (add n k))))
(proc gs_peek (n) pure (ret (tag Keep n)))
(proc gs_move (a b) pure
  (let z (eq a 0))
  (if z (block (ret (tag Wait))) (block (ret (tag Set a (sub a 1) (add b 1))))))
(proc gs_bad (n) pure (crash "a deferred step crashed"))
(proc gs_cons (s x) pure (ret (tag Set 0 (tag Cons x s))))
(proc gs_clear (s) pure (ret (tag Set 0 (tag Nil))))
(proc gs_pop (s) pure
  (match s
    (Nil () (block (ret (tag Keep (tag Empty)))))
    (Cons (x rest) (block (ret (tag Set (tag Got x) rest))))))
""")

# Run a popped item: a job is called; a handle's cell has its newest item
# popped and dropped, so a cell released too early is used (V9).
NODE_PROCS = parse(r"""
(proc sn_run (p) eff
  (match p
    (Empty () (block (let z (int 0)) (ret z)))
    (Got (x) (block
      (match x
        (H (h) (block
          (let q (step gs_pop (h)))
          (match q
            (Empty () (block (let z (int 0)) (ret z)))
            (Got (y) (block (let o (int 1)) (ret o))))))
        (J (f) (block (let r (callc! f)) (ret r))))))))
""")

TYPES = ["I", "S", "T", "L", "P", "Q", "TL", "C", "CT"]


class Gen:
    def __init__(self, rng, crash_rate=0.04, links=False):
        self.rng = rng
        self.links = links  # cells that hold cells and jobs, which can form cycles
        self.crash_rate = crash_rate
        self.procs = {}
        self.n = 0
        self.funcs = []  # (name, param types, return type)
        self.features = set()
        self.sessions = rng.random() < 0.5  # half the programs use no channels, so G4 still sees many
        self.shared = rng.random() < 0.4 or links

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
                 "call", "callc", "all", "all_ok", "race", "both", "supervise", "loop", "session", "shared",
                 "plan_all", "plan_all_ok"]
        weights = [2, 3, 1, 2, 1, 1, 1, 1, 4, 2, 1, 2, 1, 2, 1, 1, 1, 1, 3, 2 if self.sessions else 0,
                   2 if self.shared else 0, 1, 1]
        if depth >= 3:
            weights = [w if k not in ("all", "all_ok", "race", "both", "supervise", "callc", "call", "loop",
                                      "session", "shared", "plan_all", "plan_all_ok") else 0
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
        elif k == "plan_all":
            p = self.plan(out, env, "C", depth)
            self.let(out, env, "L", ("call", "plan_all!", [p]))
            self.features.add("plan.all!")
        elif k == "plan_all_ok":
            p = self.plan(out, env, "CT", depth)
            self.let(out, env, "TL", ("call", "plan_all_ok!", [p]))
            self.features.add("plan.all_ok!")
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
        elif k == "shared":
            self.shared_var(out, env, depth)
        elif k == "supervise":
            f = self.need(out, env, "C", depth)
            m = self.let(out, env, "I", ("int", rng.randint(0, 2)))
            self.let(out, env, "I", ("call", "supervise!", [m, f]))
            self.features.add("supervise!")

    # ------------------------------------------------------------ loops
    def plan(self, out, env, ty, depth, cs=None):
        """A plan over closures of type ty: entries, maybe limited, shielded,
        marked background, concatenated with another plan, or limited as a
        whole. Sets self.root_limited: whether the plan's root is a limit."""
        rng = self.rng
        cs = cs or [self.need(out, env, ty, depth) for _ in range(rng.randint(1, 3))]
        fs = self.let(out, env, "LC", ("list", cs))
        p = self.let(out, env, "PLAN", ("call", "concurrently", [fs]))
        root_limited = False
        if rng.random() < 0.3:
            p, root_limited = self.limited(out, env, p), True
        if rng.random() < 0.25:
            p, root_limited = self.let(out, env, "PLAN", ("call", "shielded", [p])), False
            self.features.add("shield")
        if rng.random() < 0.15:
            p, root_limited = self.let(out, env, "PLAN", ("call", "background", [p])), False
            self.features.add("background")
            # Whether a background entry runs, and how far, depends on the
            # schedule.
            self.features.add("arrival order")
        if rng.random() < 0.3:
            q = self.plan(out, env, ty, depth)
            p, root_limited = self.let(out, env, "PLAN", ("call", "concat", [p, q])), False
            self.features.add("concat")
            if rng.random() < 0.3:
                p, root_limited = self.limited(out, env, p), True
        self.root_limited = root_limited
        return p

    def limited(self, out, env, p):
        n = self.let(out, env, "I", ("int", self.rng.randint(1, 2)))
        self.features.add("limit")
        return self.let(out, env, "PLAN", ("call", "limit", [p, n]))

    def loop(self, out, env, depth, cs=None):
        """for! v in plan { ... }, lowered as the compiler lowers it."""
        rng = self.rng
        trying = cs is None and rng.random() < 0.3  # arrivals are Try values; an Err exits, as `?` does
        ty = "CT" if trying else "C"
        p = self.plan(out, env, ty, depth, cs)
        root_limited = self.root_limited
        g = self.let(out, env, "G", ("group", p))
        acc = self.let(out, env, "I", ("int", 0))
        budget = self.let(out, env, "I", ("int", rng.choice([0, 0, 1, 2])))
        sp = self.need(out, env, ty, depth)
        name = self.fresh("loop")
        exits = rng.random() < 0.4
        lg, lacc, lb, lsp = (self.fresh("g"), self.fresh("acc"), self.fresh("budget"), self.fresh("sp"))
        a, v = self.fresh("a"), self.fresh("v")
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
            ("Cancelled", [], Block([("let", u, ("str", "unreachable"))], ("crash", u))),
            ("Ended", [], ended),
        ]))
        self.procs[name] = Proc(name, [lg, lacc, lb, lsp], head, True)
        self.let(out, env, "T" if trying else "I", ("call", name, [g, acc, budget, sp]))
        self.features.add("loop")
        if state["spawn"]:
            self.features.add("loop spawn")
            if root_limited:
                self.features.add("loop spawn under a root limit")
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
        r = rng.random()
        if r < 0.2:
            return self.mailbox(out, env, depth)
        if r < 0.35:
            return self.cell(out, env, depth)
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

    def run_closures(self, out, env, depth, made):
        """Run closures side by side, in either order, or one after another."""
        rng = self.rng
        rng.shuffle(made)
        r = rng.random()
        if r < 0.35:
            fs = self.let(out, env, "LC", ("list", made))
            self.let(out, env, "L", ("call", "all!", [fs]))
            self.features.add("all!")
        elif r < 0.5:
            fs = self.let(out, env, "LC", ("list", made[1:]))
            self.let(out, env, "I", ("call", "race!", [made[0], fs]))
            self.features.add("race!")
        elif r < 0.85:
            self.loop(out, env, depth, made)
        else:
            for c in made:
                self.let(out, env, "I", ("callc", c, [], True))

    def mailbox(self, out, env, depth):
        """A mailbox, some posters sharing its Post, and a reader."""
        rng = self.rng
        self.features.add("mailbox")
        mb = self.let(out, env, "MB", ("mailbox",))
        p = self.let(out, env, "PO", ("field", mb, 0))
        b = self.let(out, env, "IB", ("field", mb, 1))
        made = []
        for _ in range(rng.randint(1, 3)):
            n = self.let(out, env, "I", ("int", rng.randint(0, 3)))
            made.append(self.let(out, env, "C", ("clo", self.poster(depth), [p, n])))
        z = self.let(out, env, "I", ("int", 0))
        rd = self.reader(depth)
        if rng.random() < 0.85:
            made.append(self.let(out, env, "C", ("clo", rd, [b, z])))
            self.features.add("mailbox reader closure")
        else:
            self.let(out, env, "I", ("call", rd, [b, z]))  # reads before any poster runs
            self.features.add("mailbox reader call")
        del env[p], env[b]
        self.run_closures(out, env, depth, made)

    def poster(self, depth):
        """post(p, n): post n numbers, then end; or crash holding the Post."""
        rng = self.rng
        name = self.fresh("poster")
        p, n = self.fresh("p"), self.fresh("n")
        env = {n: "I"}
        out = []
        zero = self.let(out, env, "I", ("int", 0))
        z = self.let(out, env, "I", ("prim", "eq", [n, zero]))
        lo, lenv = [], dict(env)
        last = Block(lo, ("ret", self.need(lo, lenv, "I", depth + 1)))
        so, senv = [], dict(env)
        for _ in range(rng.randint(0, 2)):
            self.statement(so, senv, depth + 1, name)
        a = self.need(so, senv, "I", depth + 1)
        if rng.random() < self.crash_rate * 3:
            self.crash_call(so, senv, depth)  # with the Post live in this frame
        so.append(("let", self.fresh("u"), ("post", p, a)))
        one = self.let(so, senv, "I", ("int", 1))
        n2 = self.let(so, senv, "I", ("prim", "sub", [n, one]))
        step = Block(so, ("tail", name, [p, n2]))
        self.procs[name] = Proc(name, [p, n], Block(out, ("if", z, last, step)), True)
        return name

    def reader(self, depth):
        """read(b, acc): take until Closed, adding up; perhaps stop early,
        dropping the inbox."""
        rng = self.rng
        name = self.fresh("reader")
        b, acc = self.fresh("b"), self.fresh("acc")
        r, v, rest, acc2 = self.fresh("r"), self.fresh("x"), self.fresh("b"), self.fresh("acc")
        mo = [("let", acc2, ("prim", "add", [acc, v]))]
        menv = {acc2: "I"}
        for _ in range(rng.randint(0, 2)):
            self.statement(mo, menv, depth + 1, name)
        again = ("tail", name, [rest, acc2])
        if rng.random() < 0.3:
            self.features.add("mailbox early exit")
            k = self.let(mo, menv, "I", ("int", rng.randint(0, 12)))
            c = self.let(mo, menv, "I", ("prim", "lt", [k, acc2]))
            got = Block(mo, ("if", c, Block([], ("ret", acc2)), Block([], again)))
        else:
            got = Block(mo, again)
        head = Block([("let", r, ("take", b))], ("match", r, [
            ("Got", [v, rest], got),
            ("Closed", [], Block([], ("ret", acc))),
        ]))
        self.procs[name] = Proc(name, [b, acc], head, True)
        return name

    def cell(self, out, env, depth):
        """A semaphore cell and some workers sharing it."""
        rng = self.rng
        self.features.add("semaphore cell")
        k = self.let(out, env, "I", ("int", rng.randint(1, 2)))
        sm = self.let(out, env, "SEM", ("sem", k))
        made = []
        for _ in range(rng.randint(1, 3)):
            made.append(self.let(out, env, "C", ("clo", self.worker(depth), [sm])))
        del env[sm]
        self.run_closures(out, env, depth, made)

    def worker(self, depth):
        """work(s): acquire a permit, work while holding it, release it. Or
        crash holding it, or let it go at once (its last use is the acquire)."""
        rng = self.rng
        name = self.fresh("worker")
        s = self.fresh("s")
        env = {}
        out = []
        pm = self.let(out, env, "PM", ("acquire", s))
        for _ in range(rng.randint(1, 3)):
            self.statement(out, env, depth + 1, name)
        r = rng.random()
        if r < 0.85:
            if r < self.crash_rate * 3:
                self.crash_call(out, env, depth)  # with the permit live in this frame
            out.append(("let", self.fresh("u"), ("call", "release_permit", [pm])))
        else:
            self.features.add("permit let go")
        body = Block(out, ("ret", self.need(out, env, "I", depth + 1)))
        del env[pm]
        self.procs[name] = Proc(name, [s], body, True)
        if "release_permit" not in self.procs:
            x, z = self.fresh("pm"), self.fresh("z")
            self.procs["release_permit"] = Proc("release_permit", [x], Block([("let", z, ("int", 0))], ("ret", z)), True)
        return name

    # ------------------------------------------------------------ shared variables
    def shared_var(self, out, env, depth):
        """A shared variable (or one made by an earlier statement), and some
        workers capturing it, and maybe a second one, run side by side; then
        maybe a read."""
        rng = self.rng
        self.features.add("shared")
        for name, pr in STEP_PROCS.items():
            self.procs.setdefault(name, pr)
        if self.links and rng.random() < 0.6:
            self.node_cells(out, env, depth)
            return
        c = self.pick(env, "SV")
        if c is None or rng.random() < 0.5:
            k = self.let(out, env, "I", ("int", rng.randint(0, 2)))
            c = self.let(out, env, "SV", ("shared", k))
        other = [v for v, t in env.items() if t == "SV" and v != c]
        if not other and rng.random() < 0.3:
            k = self.let(out, env, "I", ("int", rng.randint(0, 2)))
            other = [self.let(out, env, "SV", ("shared", k))]
        c2 = rng.choice(other) if other and rng.random() < 0.5 else None
        made = []
        for _ in range(rng.randint(1, 3)):
            caps = [c] if c2 is None else [c, c2]
            made.append(self.let(out, env, "C", ("clo", self.sv_worker(depth, len(caps)), caps)))
        self.run_closures(out, env, depth, made)
        if rng.random() < 0.5:
            self.let(out, env, "I", ("read", c))
            self.features.add("shared read")

    def sv_worker(self, depth, ncells):
        """worker(c[, c2]): one use of the shared variables, as a small Roc
        function would make it."""
        rng = self.rng
        name = self.fresh("svw")
        cs = [self.fresh("c") for _ in range(ncells)]
        env = {}
        out = []
        r = rng.random()
        if r < 0.45:
            # A permit: take, defer the give, work (maybe crash), end the block.
            self.features.add("shared permit")
            out.append(("let", self.fresh("u"), ("step", "gs_take", [cs[0]], [])))
            kind = "errdefer" if rng.random() < 0.2 else "defer"
            give = "gs_bad" if rng.random() < self.crash_rate * 2 else "gs_give"
            if give == "gs_bad":
                self.features.add("shared deferred crash")
            if kind == "errdefer":
                self.features.add("shared errdefer")
            out.append(("let", self.fresh("u"), (kind, give, [cs[0]], [])))
            for _ in range(rng.randint(1, 3)):
                self.statement(out, env, depth + 1, name)
            if rng.random() < self.crash_rate * 3:
                self.crash_call(out, env, depth)  # with the give pending
            out.append(("let", self.fresh("u"), ("undefer",)))
        elif r < 0.65:
            k = self.let(out, env, "I", ("int", rng.randint(1, 3)))
            self.let(out, env, "I", ("step", "gs_inc", [cs[0]], [k]))
            self.features.add("shared increment")
        elif r < 0.75:
            self.let(out, env, "I", ("step", "gs_peek", [cs[0]], []))
            self.features.add("shared peek")
        elif r < 0.85 and ncells == 2:
            self.let(out, env, "I", ("step", "gs_move", [cs[0], cs[1]], []))
            self.features.add("shared move")
        else:
            self.let(out, env, "I", ("read", cs[0]))
            self.features.add("shared read")
        for _ in range(rng.randint(0, 2)):
            self.statement(out, env, depth + 1, name)
        body = Block(out, ("ret", self.need(out, env, "I", depth + 1)))
        self.procs[name] = Proc(name, cs, body, True)
        return name

    def node_cells(self, out, env, depth):
        """Cells whose states hold items: handles to cells, their own included,
        and jobs that capture cells. Workers capturing some of them run side by
        side; then maybe one more use here."""
        rng = self.rng
        self.features.add("cell links")
        for name, pr in NODE_PROCS.items():
            self.procs.setdefault(name, pr)
        cells = [v for v, t in env.items() if t == "SN"]
        while not cells or (len(cells) < 3 and rng.random() < 0.4):
            cells.append(self.new_node_cell(out, env))
        made = []
        for _ in range(rng.randint(1, 3)):
            caps = rng.sample(cells, rng.randint(1, len(cells)))
            made.append(self.let(out, env, "C", ("clo", self.node_proc(depth, len(caps), "snw"), caps)))
        self.run_closures(out, env, depth, made)
        if rng.random() < 0.4:
            self.node_op(out, env, list(cells), depth)

    def new_node_cell(self, out, env):
        nil = self.let(out, env, "NL", ("tag", "Nil", []))
        return self.let(out, env, "SN", ("shared", nil))

    def node_proc(self, depth, ncells, base):
        """A worker or a job over some cells: a few uses of them, and maybe
        other work."""
        rng = self.rng
        name = self.fresh(base)
        cs = [self.fresh("c") for _ in range(ncells)]
        params = list(cs)
        env, out = {}, []
        for _ in range(rng.randint(1, 3)):
            self.node_op(out, env, cs, depth)
        for _ in range(rng.randint(0, 1)):
            self.statement(out, env, depth + 1, name)
        body = Block(out, ("ret", self.need(out, env, "I", depth + 1)))
        self.procs[name] = Proc(name, params, body, True)
        return name

    def node_op(self, out, env, cs, depth):
        """One use of the cells in cs, which a new cell made here joins."""
        rng = self.rng
        a = rng.choice(cs)
        r = rng.random()
        if r < 0.12:
            cs.append(self.new_node_cell(out, env))
            self.features.add("cell made in a task")
        elif r < 0.42:
            b = rng.choice(cs)
            i = self.let(out, env, "NI", ("tag", "H", [b]))
            self.let(out, env, "I", ("step", "gs_cons", [a], [i]))
            self.features.add("cell holds its own handle" if b == a else "cell holds a handle")
        elif r < 0.64 and depth < 4:
            caps = rng.sample(cs, rng.randint(1, min(2, len(cs))))
            f = self.let(out, env, "C", ("clo", self.node_proc(depth + 1, len(caps), "snj"), caps))
            i = self.let(out, env, "NI", ("tag", "J", [f]))
            self.let(out, env, "I", ("step", "gs_cons", [a], [i]))
            self.features.add("job captures its own cell" if a in caps else "job captures a cell")
        elif r < 0.86:
            p = self.let(out, env, "NP", ("step", "gs_pop", [a], []))
            self.let(out, env, "I", ("call", "sn_run", [p]))
            self.features.add("item popped and run")
        else:
            self.let(out, env, "I", ("step", "gs_clear", [a], []))
            self.features.add("cell cleared")

    def crash_call(self, out, env, depth):
        """Crash inside a call, so the caller's live values are in a running frame."""
        m = self.need(out, env, "S", depth + 1)
        if "crash_now" not in self.procs:
            x = self.fresh("m")
            self.procs["crash_now"] = Proc("crash_now", [x], Block([], ("crash", x)), True)
        self.let(out, env, "I", ("call", "crash_now", [m]))
        self.features.add("crash")

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


def gen_program(seed, crash_rate=0.04, links=False):
    g = Gen(random.Random(seed), crash_rate, links)
    procs = g.program()
    return procs, g.features
