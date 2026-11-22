"""Well-typed programs composed from small typed constructs.

A program is a headerless app for the default platform: a few type aliases, top-level functions and constants, `expect`s,
and a `main!` that prints every definition. Every expression is an `E` node that records its type and the scope it was built
in, so `mutate.py` can replace any node with another typed construct.
"""
import copy
import random

# Types are tuples: ("I64",) ("Str",) ("Bool",) ("List", t) ("Tup", a, b) ("Rec", name) ("Union", name) ("Try", t)
# ("Fn", (args...), ret). Records and unions name an alias declared in the program.
I64, STR, BOOL = ("I64",), ("Str",), ("Bool",)
BASE = [I64, STR, BOOL]


def show_type(t, prog, arg=False):
    k = t[0]
    if k in ("I64", "Str", "Bool"):
        return k
    if k == "List":
        return "List(%s)" % show_type(t[1], prog)
    if k == "Tup":
        return "(%s, %s)" % (show_type(t[1], prog), show_type(t[2], prog))
    if k in ("Rec", "Union", "Nom"):
        return t[1]
    if k == "Try":
        return "Try(%s, [Oops])" % show_type(t[1], prog)
    if k == "Fn":
        s = "%s -> %s" % (", ".join(show_type(a, prog, True) for a in t[1]), show_type(t[2], prog))
        return "(%s)" % s if arg else s
    if k == "Oops":
        return "[Oops]"
    if k == "Named":  # an annotation mutation: a raw type name
        return t[1]
    raise ValueError(t)


def is_data(t, prog):
    """Whether Str.inspect and == apply: no functions inside."""
    k = t[0]
    if k == "Fn":
        return False
    if k in ("List", "Try"):
        return is_data(t[1], prog)
    if k == "Tup":
        return is_data(t[1], prog) and is_data(t[2], prog)
    return True


class E:
    """One expression. `kind` selects the printer; `kids` are subexpressions; `env` is the scope it was built in."""

    def __init__(self, kind, ty, kids=(), env=None, **data):
        self.kind, self.ty, self.kids, self.env, self.data = kind, ty, list(kids), env or [], data
        self.mutated = False

    def walk(self, parent=None, slot=None):
        yield self, parent, slot
        for i, k in enumerate(self.kids):
            yield from k.walk(self, i)


class Def:
    def __init__(self, name, ty, params, body, annotated):
        self.name, self.ty, self.params, self.body, self.annotated = name, ty, params, body, annotated
        self.anno = None  # a mutated annotation string replaces the printed one


class Prog:
    def __init__(self):
        self.records = {}  # name -> [(field, type)]
        self.unions = {}  # name -> [(tag, [payload types])]
        self.nominals = {}  # name -> [payload types]; the backing is the single tag `Name(payload...)`
        self.methods = []  # Def with .nominal: a method of a nominal type, its first parameter the receiver
        self.defs = []
        self.expects = []  # [E] of type Bool
        self.extra = []  # extra top-level source lines a mutation adds
        self.mutations = []  # descriptions


class Gen:
    def __init__(self, rng, size=1.0):
        self.rng, self.size, self.n = rng, size, 0
        self.prog = Prog()

    def fresh(self, p):
        self.n += 1
        return "%s%d" % (p, self.n)

    # ---- types ------------------------------------------------------------------------------------------------
    def rand_type(self, depth=2, fn=True):
        r = self.rng
        opts = ["base"] * 6 + ["list", "tup", "rec", "union", "try"] + (["nom"] if self.prog.nominals else [])
        if fn and depth > 0:
            opts.append("fn")
        k = r.choice(opts) if depth > 0 else "base"
        if k == "base":
            return r.choice(BASE)
        if k == "list":
            return ("List", self.rand_type(depth - 1, False))
        if k == "tup":
            return ("Tup", self.rand_type(depth - 1, False), self.rand_type(depth - 1, False))
        if k == "rec":
            return ("Rec", r.choice(list(self.prog.records)))
        if k == "nom":
            return ("Nom", r.choice(list(self.prog.nominals)))
        if k == "union":
            return ("Union", r.choice(list(self.prog.unions)))
        if k == "try":
            return ("Try", self.rand_type(depth - 1, False))
        return ("Fn", tuple(self.rand_type(0) for _ in range(r.randint(1, 2))), self.rand_type(depth - 1, False))

    def data_type(self, depth=2):
        while True:
            t = self.rand_type(depth, False)
            if is_data(t, self.prog):
                return t

    # ---- expressions ------------------------------------------------------------------------------------------
    def leaf(self, t, env):
        r = self.rng
        k = t[0]
        vs = [n for n, vt in env if vt == t]
        if vs and r.random() < 0.6:
            return E("var", t, env=env, name=r.choice(vs))
        if k == "I64":
            return E("lit", t, env=env, text=str(r.randint(0, 9)))
        if k == "Str":
            return E("lit", t, env=env, text='"%s"' % r.choice(["a", "bc", "", "xyz"]))
        if k == "Bool":
            return E("lit", t, env=env, text=r.choice(["True", "False"]))
        if k == "List":
            return E("list", t, [self.leaf(t[1], env) for _ in range(r.randint(0, 2))], env)
        if k == "Tup":
            return E("tup", t, [self.leaf(t[1], env), self.leaf(t[2], env)], env)
        if k == "Rec":
            fs = self.prog.records[t[1]]
            return E("rec", t, [self.leaf(ft, env) for _, ft in fs], env, fields=[f for f, _ in fs])
        if k == "Union":
            tag, args = r.choice(self.prog.unions[t[1]])
            return E("tag", t, [self.leaf(a, env) for a in args], env, tag=tag)
        if k == "Try":
            if r.random() < 0.7:
                return E("tag", t, [self.leaf(t[1], env)], env, tag="Ok")
            return E("tag", t, [E("lit", ("Oops",), env=env, text="Oops")], env, tag="Err")
        if k == "Fn":
            return self.lam(t, env, 0)
        if k == "Nom":
            return E("tag", t, [self.leaf(a, env) for a in self.prog.nominals[t[1]]], env, tag="%s.%s" % (t[1], t[1]))
        raise ValueError(t)

    def lam(self, t, env, depth):
        ps = [(self.fresh("p"), a) for a in t[1]]
        inner = env + ps
        return E("lam", t, [self.expr(t[2], inner, depth)], env, params=[p for p, _ in ps])

    def callables(self, t, env):
        """Functions in scope that return `t`: top-level functions and function-typed parameters."""
        return [(n, vt) for n, vt in env if vt[0] == "Fn" and vt[2] == t]

    def expr(self, t, env, depth, ctx=None):
        r = self.rng
        ctx = ctx or {}
        if depth <= 0 or r.random() < 0.25 / self.size:
            return self.leaf(t, env)
        d = depth - 1
        k = t[0]
        cands = ["leaf", "if", "let", "match_union", "match_list", "field", "tupidx", "destructure"]
        if self.callables(t, env):
            cands += ["call"] * 3
        if [m for m in self.prog.methods if m.ty[2] == t]:
            cands += ["method"] * 2
        if ctx.get("try") and r.random() < 0.5:
            cands.append("question")
        if k == "I64":
            cands += ["arith", "arith", "len", "strlen", "fold", "match_int"]
        elif k == "Str":
            cands += ["concat", "interp", "inspect"]
        elif k == "Bool":
            cands += ["eq", "lt", "andor", "not", "isempty"]
        elif k == "List":
            cands += ["listlit", "map", "append", "lconcat"]
        elif k == "Rec":
            cands += ["update"]
        elif k == "Try":
            cands += ["ok"]
        elif k == "Fn":
            cands += ["lam", "lam"]
        elif k == "Nom":
            cands += ["nomctor"]
        c = r.choice(cands)
        sub = lambda tt, e=env: self.expr(tt, e, d, ctx)
        if c == "leaf":
            return self.leaf(t, env)
        if c == "call":
            n, ft = r.choice(self.callables(t, env))
            return E("call", t, [sub(a) for a in ft[1]], env, fn=n)
        if c == "if":
            return E("if", t, [sub(BOOL), sub(t), sub(t)], env)
        if c == "let":
            rt = self.rand_type(1)
            name = self.fresh("v")
            return E("let", t, [sub(rt), self.expr(t, env + [(name, rt)], d, ctx)], env, name=name)
        if c == "destructure":
            if r.random() < 0.5 and self.prog.records:
                rn = r.choice(list(self.prog.records))
                fs = self.prog.records[rn]
                bs = [(self.fresh("p"), ft) for _, ft in fs]
                return E("drec", t, [sub(("Rec", rn)), self.expr(t, env + bs, d, ctx)], env,
                         fields=[f for f, _ in fs], binders=[b for b, _ in bs])
            a, b = self.rand_type(1, False), self.rand_type(1, False)
            bs = [(self.fresh("p"), a), (self.fresh("p"), b)]
            return E("dtup", t, [sub(("Tup", a, b)), self.expr(t, env + bs, d, ctx)], env, binders=[x for x, _ in bs])
        if c == "match_union":
            un = r.choice(list(self.prog.unions))
            tags = self.prog.unions[un]
            # an annotated local fixes the scrutinee's union: a tag literal alone would close it
            sv = self.fresh("v")
            branches, kids = [], [E("annot", ("Union", un), [sub(("Union", un))], env, name=sv)]
            wild = r.random() < 0.3 and len(tags) > 1
            for i, (tag, args) in enumerate(tags):
                if wild and i == len(tags) - 1:
                    branches.append(("_", []))
                    kids.append(sub(t))
                    break
                bs = [(self.fresh("p"), a) for a in args]
                branches.append((tag, [b for b, _ in bs]))
                kids.append(self.expr(t, env + bs, d, ctx))
            return E("match", t, kids, env, branches=branches)
        if c == "match_list":
            et = self.rand_type(1, False)
            h = self.fresh("p")
            return E("match", t, [sub(("List", et)), sub(t), self.expr(t, env + [(h, et)], d, ctx)], env,
                     branches=[("[]", []), ("[%s, ..]", [h])])
        if c == "match_int":
            return E("match", t, [sub(I64), sub(t), sub(t)], env, branches=[(str(r.randint(0, 3)), []), ("_", [])])
        if c == "field":
            opts = [(rn, f) for rn, fs in self.prog.records.items() for f, ft in fs if ft == t]
            if opts:
                rn, f = r.choice(opts)
                return E("field", t, [sub(("Rec", rn))], env, field=f)
            return self.leaf(t, env)
        if c == "tupidx":
            o = self.rand_type(1, False)
            i = r.randint(0, 1)
            tt = ("Tup", t, o) if i == 0 else ("Tup", o, t)
            return E("tupidx", t, [sub(tt)], env, idx=i)
        if c == "question":
            return E("question", t, [sub(("Try", t))], env)
        if c == "arith":
            return E("binop", t, [sub(I64), sub(I64)], env, op=r.choice(["+", "-", "*"]))
        if c == "len":
            return E("tmpl", t, [sub(("List", self.rand_type(1, False)))], env, fmt="List.len({0}).to_i64_wrap()")
        if c == "strlen":
            return E("tmpl", t, [sub(STR)], env, fmt="Str.count_utf8_bytes({0}).to_i64_wrap()")
        if c == "fold":
            et = self.rand_type(1, False)
            acc, x = self.fresh("$s"), self.fresh("p")
            return E("fold", t, [sub(t), sub(("List", et)), self.expr(t, env + [(acc, t), (x, et)], d, ctx)], env,
                     acc=acc, x=x)
        if c == "concat":
            return E("tmpl", t, [sub(STR), sub(STR)], env, fmt="Str.concat({0}, {1})")
        if c == "interp":
            return E("interp", t, [sub(STR), sub(STR)], env)
        if c == "inspect":
            return E("tmpl", t, [sub(self.data_type(1))], env, fmt="Str.inspect({0})")
        if c == "eq":
            et = r.choice([I64, STR, BOOL])
            return E("binop", t, [sub(et), sub(et)], env, op=r.choice(["==", "!="]))
        if c == "lt":
            return E("binop", t, [sub(I64), sub(I64)], env, op=r.choice(["<", ">", "<="]))
        if c == "andor":
            return E("binop", t, [sub(BOOL), sub(BOOL)], env, op=r.choice(["and", "or"]))
        if c == "not":
            return E("tmpl", t, [sub(BOOL)], env, fmt="!({0})")
        if c == "isempty":
            return E("tmpl", t, [sub(("List", self.rand_type(1, False)))], env, fmt="List.is_empty({0})")
        if c == "listlit":
            return E("list", t, [sub(t[1]) for _ in range(r.randint(1, 3))], env)
        if c == "map":
            et = self.rand_type(1, False)
            return E("tmpl", t, [sub(("List", et)), self.lam(("Fn", (et,), t[1]), env, d)], env, fmt="List.map({0}, {1})")
        if c == "append":
            return E("tmpl", t, [sub(t), sub(t[1])], env, fmt="List.append({0}, {1})")
        if c == "lconcat":
            return E("tmpl", t, [sub(t), sub(t)], env, fmt="List.concat({0}, {1})")
        if c == "update":
            fs = self.prog.records[t[1]]
            f, ft = r.choice(fs)
            return E("update", t, [sub(t), sub(ft)], env, field=f)
        if c == "ok":
            return E("tag", t, [sub(t[1])], env, tag="Ok")
        if c == "lam":
            return self.lam(t, env, d)
        if c == "nomctor":
            return E("tag", t, [sub(a) for a in self.prog.nominals[t[1]]], env, tag="%s.%s" % (t[1], t[1]))
        if c == "method":
            m = r.choice([m for m in self.prog.methods if m.ty[2] == t])
            return E("mcall", t, [sub(a) for a in m.ty[1]], env, method=m.name, nominal=m.nominal,
                     qualified=r.random() < 0.3)
        return self.leaf(t, env)

    # ---- program ----------------------------------------------------------------------------------------------
    def program(self):
        r, p = self.rng, self.prog
        for i in range(r.randint(1, 2)):
            fs = [(self.fresh("fx"), r.choice(BASE + [("List", I64)])) for _ in range(r.randint(1, 3))]
            p.records[self.fresh("Pt")] = fs
        for i in range(r.randint(1, 2)):
            tags = []
            for _ in range(r.randint(2, 3)):
                tags.append((self.fresh("T"), [r.choice(BASE) for _ in range(r.randint(0, 2))]))
            p.unions[self.fresh("Sh")] = tags
        for i in range(r.randint(0, 2)):
            name = self.fresh("Nm")
            p.nominals[name] = [r.choice(BASE) for _ in range(r.randint(1, 2))]
            for _ in range(r.randint(1, 2)):
                args = (("Nom", name),) + tuple(self.rand_type(1) for _ in range(r.randint(0, 2)))
                ret = self.rand_type(1, False)
                recv = [(self.fresh("p"), a) for a in p.nominals[name]]
                ps = [(self.fresh("a"), a) for a in args[1:]]
                body = self.expr(ret, recv + ps, int(2 * self.size))
                m = Def(self.fresh("m"), ("Fn", args, ret), [x for x, _ in ps], body, True)
                m.nominal, m.recv = name, [x for x, _ in recv]
                p.methods.append(m)
        env = []
        for i in range(r.randint(2, 4)):
            if r.random() < 0.7:
                args = tuple(self.rand_type(1) for _ in range(r.randint(1, 3)))
                ret = self.rand_type(1, False)
                if r.random() < 0.25:
                    ret = ("Try", ret)
                ft = ("Fn", args, ret)
                name = self.fresh("f")
                ps = [(self.fresh("a"), a) for a in args]
                body = self.expr(ret, env + ps, int(3 * self.size), {"try": ret[0] == "Try"})
                p.defs.append(Def(name, ft, [x for x, _ in ps], body, r.random() < 0.85))
                env = env + [(name, ft)]
            else:
                t = self.data_type(1)
                name = self.fresh("c")
                p.defs.append(Def(name, t, None, self.expr(t, env, int(2 * self.size)), r.random() < 0.85))
                env = env + [(name, t)]
        for _ in range(r.randint(1, 2)):
            p.expects.append(self.expr(BOOL, env, 2))
        main_args = []
        for d in p.defs:
            if d.params is None:
                if is_data(d.ty, p):
                    main_args.append(E("var", d.ty, env=env, name=d.name))
            elif is_data(d.ty[2], p):
                main_args.append(E("call", d.ty[2], [self.leaf(a, env) for a in d.ty[1]], env, fn=d.name))
        p.main = main_args
        p.env = env
        return p


# ---- printing ---------------------------------------------------------------------------------------------------
IND = "    "
ATOMS = ("lit", "var", "call", "mcall", "rec", "tup", "field", "tupidx", "tag", "list", "binop", "question")


def pr(e, prog, ind):
    """Print `e`; a multi-line result's continuation lines start at indentation `ind`."""
    k, x = e.kind, e.data
    P = lambda c: pr(c, prog, ind)
    A = lambda c: P(c) if c.kind in ATOMS else "(%s)" % P(c)
    if k == "lit":
        return x["text"]
    if k == "raw":
        return x["text"].format(*[P(c) for c in e.kids])
    if k == "var":
        return x["name"]
    if k == "call":
        return "%s(%s)" % (x.get("callee") or x["fn"], ", ".join(P(c) for c in e.kids))
    if k == "tmpl":
        return x["fmt"].format(*[pr(c, prog, ind) for c in e.kids])
    if k == "binop":
        return "(%s %s %s)" % (A(e.kids[0]), x["op"], A(e.kids[1]))
    if k == "interp":
        a, b = P(e.kids[0]), P(e.kids[1])
        if "\n" in a + b or '"' in a + b:  # a block cannot sit inside a string literal
            return "Str.concat(%s, %s)" % (a, b)
        return '"${%s}-${%s}"' % (a, b)
    if k == "list":
        return "[%s]" % ", ".join(P(c) for c in e.kids)
    if k == "tup":
        return "(%s)" % ", ".join(P(c) for c in e.kids)
    if k == "rec":
        return "{ %s }" % ", ".join("%s: %s" % (f, P(c)) for f, c in zip(x["fields"], e.kids))
    if k == "update":
        return "{ ..%s, %s: %s }" % (P(e.kids[0]), x["field"], P(e.kids[1]))
    if k == "field":
        return "%s.%s" % (A(e.kids[0]), x["field"])
    if k == "tupidx":
        return "%s.%d" % (A(e.kids[0]), x["idx"])
    if k == "tag":
        return x["tag"] + ("(%s)" % ", ".join(P(c) for c in e.kids) if e.kids else "")
    if k == "question":
        return "(%s)?" % P(e.kids[0])
    if k == "lam":
        return "|%s| %s" % (", ".join(x["params"]), P(e.kids[0]))
    if k == "annot":
        i1 = ind + IND
        return "{\n%s%s : %s\n%s%s = %s\n%s%s\n%s}" % (i1, x["name"], show_type(e.ty, prog), i1, x["name"],
                                                     pr(e.kids[0], prog, i1), i1, x["name"], ind)
    if k == "mcall":
        if x["qualified"]:
            return "%s.%s(%s)" % (x["nominal"], x["method"], ", ".join(P(c) for c in e.kids))
        return "%s.%s(%s)" % (A(e.kids[0]), x["method"], ", ".join(P(c) for c in e.kids[1:]))
    if k == "if":
        return "if %s { %s } else { %s }" % tuple(P(c) for c in e.kids)
    i1 = ind + IND
    if k in ("let", "drec", "dtup", "fold", "stmt"):
        lines = []
        cur = e
        while cur.kind in ("let", "drec", "dtup", "fold", "stmt"):
            d = cur.data
            if cur.kind == "let":
                lines.append("%s = %s" % (d["name"], pr(cur.kids[0], prog, i1)))
            elif cur.kind == "drec":
                pat = d.get("pattern") or "{ %s }" % ", ".join("%s: %s" % fb for fb in zip(d["fields"], d["binders"]))
                lines.append("%s = %s" % (pat, pr(cur.kids[0], prog, i1)))
            elif cur.kind == "dtup":
                pat = d.get("pattern") or "(%s)" % ", ".join(d["binders"])
                lines.append("%s = %s" % (pat, pr(cur.kids[0], prog, i1)))
            elif cur.kind == "stmt":
                lines.append(d["text"])
            else:
                lines.append("var %s = %s" % (d["acc"], pr(cur.kids[0], prog, i1)))
                lines.append("for %s in %s {" % (d["x"], pr(cur.kids[1], prog, i1)))
                lines.append("%s%s = %s" % (IND, d["acc"], pr(cur.kids[2], prog, i1 + IND)))
                lines.append("}")
                lines.append(d["acc"])
                break
            cur = cur.kids[-1]
        else:
            lines.append(pr(cur, prog, i1))
        return "{\n" + "".join(i1 + l + "\n" for l in lines) + ind + "}"
    if k == "match":
        scrut = e.kids[0]
        st = pr(scrut, prog, i1)
        out = ["match %s {" % (st if scrut.kind in ATOMS else "(%s)" % st)]
        for (pat, bs), body in zip(x["branches"], e.kids[1:]):
            if "%s" in pat:
                ptxt = pat % ", ".join(bs)
            else:
                ptxt = pat + ("(%s)" % ", ".join(bs) if bs else "")
            out.append("%s%s => %s" % (i1, ptxt, pr(body, prog, i1)))
        return "\n".join(out) + "\n" + ind + "}"
    raise ValueError(k)


def render(prog):
    lines = []
    for n, fs in prog.records.items():
        lines.append("%s : { %s }" % (n, ", ".join("%s : %s" % (f, show_type(t, prog)) for f, t in fs)))
        lines.append("")
    for n, tags in prog.unions.items():
        ts = [tg + ("(%s)" % ", ".join(show_type(a, prog) for a in args) if args else "") for tg, args in tags]
        lines.append("%s : [%s]" % (n, ", ".join(ts)))
        lines.append("")
    for n, args in prog.nominals.items():
        lines.append("%s := [%s(%s)].{" % (n, n, ", ".join(show_type(a, prog) for a in args)))
        for m in prog.methods:
            if m.nominal != n:
                continue
            lines.append("%s%s : %s" % (IND, m.name, m.anno or show_type(m.ty, prog)))
            ps = ["%s(%s)" % (n, ", ".join(m.recv))] + m.params
            lines.append("%s%s = |%s| %s" % (IND, m.name, ", ".join(ps), pr(m.body, prog, IND)))
            lines.append("")
        lines[-1:] = ["}", ""] if lines[-1] == "" else lines[-1:] + ["}", ""]
    lines += prog.extra
    for d in prog.defs:
        if d.anno is not None:
            lines.append("%s : %s" % (d.name, d.anno))
        elif d.annotated:
            lines.append("%s : %s" % (d.name, show_type(d.ty, prog)))
        if d.params is None:
            lines.append("%s = %s" % (d.name, pr(d.body, prog, "")))
        else:
            lines.append("%s = |%s| %s" % (d.name, ", ".join(d.params), pr(d.body, prog, "")))
        lines.append("")
    for e in prog.expects:
        lines.append("expect %s" % pr(e, prog, ""))
        lines.append("")
    lines.append("main! = |_args| {")
    for e in prog.main:
        lines.append("%secho!(Str.inspect(%s))" % (IND, pr(e, prog, IND)))
    lines.append("%sOk({})" % IND)
    lines.append("}")
    return "\n".join(lines) + "\n"


def generate(seed, size=1.0):
    g = Gen(random.Random(seed), size)
    return g, g.program()


def clone(prog):
    return copy.deepcopy(prog)
