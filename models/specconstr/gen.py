"""Random generators of small typed Roc programs for the SpecConstr model.

Both families pick a construct for the type they need (let, match, call, loop, ...), fill its
parts recursively with fresh random constructs of the right types, and render the Roc text and
the oracle from that one choice, so the two cannot drift apart.

  model: programs inside the Maude IR fragment (maude/ir.maude): I64, N := [Z, S(N)],
         Sh := [A, B(I64), C(N, I64)], tuples, let, match with constructor/literal/tuple patterns,
         direct calls, fuel-bounded recursion, while loops with carried state and break, nested
         loops whose results are partly read. Each expression is also rendered as a Maude term;
         the Maude evaluator gives the expected results, and the Maude SpecConstr pass is checked
         on the same term.
  wide:  constructs the Maude model leaves out: lambdas and captures, a closure in a tag payload
         (Op := [Inc(I64), Fn(I64 -> I64)]), functions returning closures, closures threaded or
         wrapped through fuel-bounded recursion, Str, List(I64) with map/keep_if/fold/get, list
         patterns, `for` loops, and function bodies that open with a run of lets over non-tag
         parameters (the shape SpecConstr's prefix split copies, `analyzePrefixRuns`). Each
         expression carries a Python closure that evaluates it; that evaluator is the oracle.
"""
import random

I64, NAT, SH, OP, STR, LST, FN = "I64", "N", "Sh", "Op", "Str", "List(I64)", "I64 -> I64"


def ind(s, n=1):
    pad = "\t" * n
    return "\n".join(pad + line if line else line for line in s.split("\n"))


def paren(s):
    return s if "\n" not in s else "(" + s + ")"


def armtext(s):
    if "\n" not in s or s.startswith("{"):
        return s
    return "{\n" + ind(s) + "\n}"


def block(s):
    return s if "\n" not in s or s.startswith("{") else "{\n" + ind(s) + "\n}"


class Base:
    def __init__(self, seed, prefix):
        self.r = random.Random(seed)
        self.prefix = prefix
        self.n = 0
        self.fns = []  # (name, [param types], ret type) of the functions defined so far

    def fresh(self, base):
        self.n += 1
        return f"{base}{self.n}"

    def pick(self, xs):
        return self.r.choice(xs)

    def chance(self, p):
        return self.r.random() < p


# ====================================================================== model family

T2 = ("tup", I64, I64)
TN = ("tup", I64, NAT)


def mty(t):
    if t == I64:
        return "tInt"
    if t == NAT:
        return "tN('Nat)"
    if t == SH:
        return "tN('Sh)"
    return "tT(" + " ; ".join(mty(x) for x in t[1:]) + ")"


def rty(t):
    if t in (I64, NAT, SH):
        return t
    return "(" + ", ".join(rty(x) for x in t[1:]) + ")"


def mlist(xs):
    if not xs:
        return "enil"
    if len(xs) == 1:
        return xs[0]
    return "(" + ", ".join(xs) + ")"


class MEx:
    """An expression: Roc text (possibly a multi-line block) and its Maude term."""

    def __init__(self, roc, mau, ty):
        self.roc, self.mau, self.ty = roc, mau, ty


class MVar:
    def __init__(self, ty, roc, mau):
        self.ty, self.roc, self.mau = ty, roc, mau


class ModelGen(Base):
    """Programs inside the Maude IR fragment: every expression is rendered as Roc and as a Maude term."""

    def vars_of(self, env, ty):
        return [v for v in env if v.ty == ty]

    # ---------------- leaves ----------------
    def lit(self):
        k = self.r.randint(0, 4)
        return MEx(str(k), f"i({k})", I64)

    def var(self, v):
        return MEx(v.roc, v.mau, v.ty)

    # ---------------- by type ----------------
    def gen(self, ty, env, d):
        if ty == I64:
            return self.gen_int(env, d)
        if ty == NAT:
            return self.gen_nat(env, d)
        if ty == SH:
            return self.gen_sh(env, d)
        return self.gen_tup(ty, env, d)

    def gen_tup(self, ty, env, d):
        vs = self.vars_of(env, ty)
        if vs and self.r.random() < 0.3:
            return self.var(self.pick(vs))
        parts = [self.typed(self.gen(t, env, d - 1)) for t in ty[1:]]
        return MEx("(" + ", ".join(p.roc for p in parts) + ")",
                  "tp(" + mlist([p.mau for p in parts]) + ")", ty)

    def gen_nat(self, env, d):
        vs = self.vars_of(env, NAT)
        opts = ["z", "s"]
        if vs:
            opts += ["var", "var", "svar"]
        if d > 0:
            opts += ["s", "call", "if", "match"]
        c = self.pick(opts)
        if c == "z":
            return MEx("Z", "tg('Z, enil)", NAT)
        if c == "var":
            return self.var(self.pick(vs))
        if c == "svar":
            v = self.pick(vs)
            return MEx(f"S({v.roc})", f"tg('S, {v.mau})", NAT)
        if c == "s":
            a = self.gen_nat(env, d - 1)
            return MEx(f"S({paren(a.roc)})", f"tg('S, {a.mau})", NAT)
        if c == "call":
            e = self.gen_call(NAT, env, d)
            if e:
                return e
            return MEx("Z", "tg('Z, enil)", NAT)
        if c == "if":
            return self.gen_if(NAT, env, d)
        return self.gen_match(NAT, env, d)

    def gen_sh(self, env, d):
        vs = self.vars_of(env, SH)
        opts = ["a", "b", "c"] + (["var", "var"] if vs else [])
        if d > 0:
            opts += ["call", "if"]
        c = self.pick(opts)
        if c == "a":
            return MEx("A", "tg('A, enil)", SH)
        if c == "b":
            x = self.gen_int(env, d - 1)
            return MEx(f"B({x.roc})", f"tg('B, {x.mau})", SH)
        if c == "c":
            n = self.gen_nat(env, d - 1)
            x = self.gen_int(env, d - 1)
            return MEx(f"C({n.roc}, {x.roc})", f"tg('C, ({n.mau}, {x.mau}))", SH)
        if c == "var":
            return self.var(self.pick(vs))
        if c == "call":
            e = self.gen_call(SH, env, d)
            if e:
                return e
            return MEx("A", "tg('A, enil)", SH)
        return self.gen_if(SH, env, d)

    def gen_int(self, env, d):
        vs = self.vars_of(env, I64)
        opts = ["lit"] + (["var", "var", "var"] if vs else [])
        if d > 0:
            opts += ["add", "add", "sub", "if", "call", "call", "match", "match", "let", "let",
                     "proj", "tmatch", "depth"]
            if d > 1:
                opts += ["loop", "loop"]
        c = self.pick(opts)
        if c == "lit":
            return self.lit()
        if c == "var":
            return self.var(self.pick(vs))
        if c in ("add", "sub"):
            a, b = self.gen_int(env, d - 1), self.gen_int(env, d - 1)
            op = "+" if c == "add" else "-"
            return MEx(f"({paren(a.roc)} {op} {paren(b.roc)})", f"pr('{c}, ({a.mau}, {b.mau}))", I64)
        if c == "if":
            return self.gen_if(I64, env, d)
        if c == "call":
            e = self.gen_call(I64, env, d)
            return e if e else self.lit()
        if c == "match":
            return self.gen_match(I64, env, d)
        if c == "let":
            return self.gen_let(I64, env, d)
        if c == "proj":
            ty = self.pick([T2, TN])
            t = self.gen_tup(ty, env, d - 1)
            name = self.fresh("t")
            v = MVar(ty, name, f"v('{name})")
            k = self.r.randint(0, 1)
            body = self.gen_int(env + [v], d - 1) if self.r.random() < 0.5 else None
            if ty[k + 1] == I64:
                proj = MEx(f"{name}.{k}", f"pj(v('{name}), {k})", I64)
            else:
                proj = MEx(f"depth({name}.{k})", f"cl('depth, pj(v('{name}), {k}))", I64)
            if body:
                return MEx(f"{{\n{ind(name + ' = ' + t.roc)}\n\t{proj.roc} + {paren(body.roc)}\n}}",
                          f"lt('{name}, {t.mau}, pr('add, ({proj.mau}, {body.mau})))", I64)
            return MEx(f"{{\n{ind(name + ' = ' + t.roc)}\n\t{proj.roc}\n}}",
                      f"lt('{name}, {t.mau}, {proj.mau})", I64)
        if c == "tmatch":
            return self.gen_tmatch(env, d)
        if c == "depth":
            n = self.gen_nat(env, d - 1)
            return MEx(f"depth({n.roc})", f"cl('depth, {n.mau})", I64)
        return self.gen_loop(env, d)

    # ---------------- control ----------------
    def gen_cond(self, env, d):
        a, b = self.gen_int(env, max(d - 2, 0)), self.gen_int(env, max(d - 2, 0))
        if self.r.random() < 0.8:
            return f"{paren(a.roc)} < {paren(b.roc)}", f"pr('lt, ({a.mau}, {b.mau}))"
        return f"{paren(a.roc)} == {paren(b.roc)}", f"pr('eq, ({a.mau}, {b.mau}))"

    def gen_if(self, ty, env, d):
        cr, cm = self.gen_cond(env, d)
        x, y = self.gen(ty, env, d - 1), self.gen(ty, env, d - 1)
        return MEx(f"if {cr} {{\n{ind(x.roc)}\n}} else {{\n{ind(y.roc)}\n}}",
                  f"mt({cm}, (pc('True, pnil) => {x.mau}) | (pc('False, pnil) => {y.mau}))", ty)

    def gen_let(self, ty, env, d):
        bty = self.pick([I64, NAT, NAT, SH, T2])
        e = self.gen(bty, env, d - 1)
        name = self.fresh("x")
        v = MVar(bty, name, f"v('{name})")
        body = self.gen(ty, env + [v], d - 1)
        ann = f"{name} : {rty(bty)}\n" if bty in (NAT, SH) else ""
        return MEx(f"{{\n{ind(ann + name + ' = ' + e.roc)}\n{ind(body.roc)}\n}}",
                  f"lt('{name}, {e.mau}, {body.mau})", ty)

    def gen_match(self, ty, env, d):
        if self.r.random() < 0.6:
            scr = self.typed(self.gen_nat(env, d - 1))
            branches = []
            shapes = self.pick([
                ["Z", "S(m)"], ["S(Z)", "S(S(m))", "_"], ["Z", "S(Z)", "S(m)"],
                ["S(S(_))", "m"], ["Z", "_"]])
            for p in shapes:
                benv = list(env)
                if "m" in p.replace("(", " ").replace(")", " ").split():
                    mn = self.fresh("m")
                    benv.append(MVar(NAT, mn, f"v('{mn})"))
                    p = p.replace("m", mn)
                else:
                    mn = None
                body = self.gen(ty, benv, d - 1)
                branches.append((p, self.npat(p), body))
        else:
            scr = self.typed(self.gen_sh(env, d - 1))
            branches = []
            shapes = self.pick([["A", "B(b)", "C(c, b)"], ["C(Z, b)", "C(S(c), b)", "_"],
                                ["B(b)", "_"], ["A", "B(0)", "B(b)", "C(c, _)"]])
            for p in shapes:
                benv = list(env)
                rp, mp = p, p
                toks = p.replace("(", " ").replace(")", " ").replace(",", " ").split()
                if "b" in toks:
                    bn = self.fresh("b")
                    benv.append(MVar(I64, bn, f"v('{bn})"))
                    rp = rp.replace("b", bn)
                if "c" in toks:
                    cn = self.fresh("c")
                    benv.append(MVar(NAT, cn, f"v('{cn})"))
                    rp = rp.replace("c", cn)
                body = self.gen(ty, benv, d - 1)
                branches.append((rp, self.npat(rp), body))
        roc = "match " + paren(scr.roc) + " {\n" + "\n".join(
            ind(f"{rp} => " + armtext(b.roc)) for rp, _, b in branches) + "\n}"
        mau = f"mt({scr.mau}, " + " | ".join(f"({mp} => {b.mau})" for _, mp, b in branches) + ")"
        return MEx(roc, mau, ty)

    def gen_tmatch(self, env, d):
        a, b = self.gen_int(env, d - 1), self.gen_int(env, d - 1)
        x1, x2 = self.fresh("z"), self.fresh("z")
        e1 = self.gen_int(env, d - 1)
        e2 = self.gen_int(env + [MVar(I64, x1, f"v('{x1})")], d - 1)
        e3 = self.gen_int(env + [MVar(I64, x2, f"v('{x2})")], d - 1)
        k1, k2 = self.r.randint(0, 2), self.r.randint(0, 2)
        roc = (f"match ({paren(a.roc)}, {paren(b.roc)}) {{\n\t({k1}, _) => {armtext(e1.roc)}\n"
               f"\t({x1}, {k2}) => {armtext(e2.roc)}\n\t(_, {x2}) => {armtext(e3.roc)}\n}}")
        mau = (f"mt(tp(({a.mau}, {b.mau})), (pt((pl({k1}), pw)) => {e1.mau}) | "
               f"(pt((pv('{x1}), pl({k2}))) => {e2.mau}) | (pt((pw, pv('{x2}))) => {e3.mau}))")
        return MEx(roc, mau, I64)

    def typed(self, e):
        """Pin a tag value to its nominal type (an identity call), so it never infers a structural type."""
        if e.ty == NAT and not e.roc.startswith(("asn(", "$")) and e.mau[:3] != "v('":
            return MEx(f"asn({paren(e.roc)})", f"cl('asn, {e.mau})", NAT)
        if e.ty == SH and not e.roc.startswith(("ash(", "$")) and e.mau[:3] != "v('":
            return MEx(f"ash({paren(e.roc)})", f"cl('ash, {e.mau})", SH)
        return e

    def npat(self, p):
        """Roc pattern text -> Maude pattern."""
        p = p.strip()
        if p == "_":
            return "pw"
        if p.isdigit():
            return f"pl({p})"
        if "(" not in p:
            if p[0].isupper():
                return f"pc('{p}, pnil)"
            return f"pv('{p})"
        name, rest = p.split("(", 1)
        rest = rest[:-1]
        parts, depth, cur = [], 0, ""
        for ch in rest:
            if ch == "," and depth == 0:
                parts.append(cur)
                cur = ""
                continue
            depth += ch == "("
            depth -= ch == ")"
            cur += ch
        parts.append(cur)
        return f"pc('{name}, " + mlist([self.npat(x) for x in parts]).replace("enil", "pnil") + ")"

    def gen_call(self, ty, env, d):
        cands = [f for f in self.fns if f[2] == ty]
        if not cands:
            return None
        name, ps, _ = self.pick(cands)
        args = [self.gen(p, env, d - 1) for p in ps]
        return MEx(f"{name}(" + ", ".join(paren(a.roc) for a in args) + ")",
                  f"cl('{name}, " + mlist([a.mau for a in args]) + ")", ty)

    # ---------------- loops ----------------
    def gen_loop(self, env, d):
        """A while loop as a block producing I64. Carried state: counter $i plus 1-2 vars."""
        lid = self.fresh("")
        tys = [I64] + [self.pick([I64, I64, NAT]) for _ in range(self.r.randint(1, 2))]
        names = [f"i{lid}"] + [f"s{lid}_{k}" for k in range(1, len(tys))]
        inits = [MEx("0", "i(0)", I64)] + [self.typed(self.gen(t, env, 1)) for t in tys[1:]]
        bound = self.pick([MEx(str(self.r.randint(0, 3)), None, I64)] +
                          [self.var(v) for v in env if v.ty == I64 and v.roc == "n"])
        if bound.mau is None:
            bound.mau = f"i({bound.roc})"
        rnm = f"r{lid}"
        state_in = [MVar(t, "$" + nm, f"v('{nm})") for t, nm in zip(tys, names)]
        benv = env + state_in
        # body: optional break check, optional inner loop, then updates
        brk_m = "bk(tp(" + mlist([f"v('{nm})" for nm in names]) + "))"
        use_break = self.r.random() < 0.35
        if use_break:
            cr, cm = self.gen_cond(benv, 2)
        locb = None
        if self.r.random() < 0.3:
            xn = self.fresh("w")
            sc = self.gen_int(benv, 1)
            k = self.r.randint(0, 3)
            val = self.typed(self.gen_nat(benv, 1))
            locb = (xn, sc, k, val)
            benv = benv + [MVar(NAT, xn, f"v('{xn})")]
        inner = None
        if d > 2 and self.r.random() < 0.4:
            inner = self.gen_inner(benv, d - 1)
            benv = benv + inner["vars"]
        news = []
        for t, nm in zip(tys[1:], names[1:]):
            e = self.gen(t, benv, min(d - 1, 2))
            news.append((nm, t, e))
        # Roc text
        lines = [f"var ${nm} = {paren(e.roc)}" for nm, e in zip(names, inits)]
        body = []
        if use_break:
            body.append(f"if {cr} {{\n\tbreak\n}}")
        if locb:
            xn, sc, k, val = locb
            body.append(f"{xn} = match {paren(sc.roc)} {{\n\t{k} => {{\n\t\tbreak\n\t}}\n\t_ => {armtext(val.roc)}\n}}")
        if inner:
            body.append(inner["roc"])
        for nm, t, e in news:
            body.append(f"n_{nm} = {paren(e.roc)}")
        body.append(f"n_{names[0]} = ${names[0]} + 1")
        for nm in names:
            body.append(f"${nm} = n_{nm}")
        lines.append(f"while ${names[0]} < {bound.roc} {{\n" + ind("\n".join(body)) + "\n}")
        # continuation after the loop: uses some state vars
        after_env = env + [MVar(t, "$" + nm, f"pj(v('{rnm}), {k})") for k, (t, nm) in enumerate(zip(tys, names))]
        rest = self.gen_int(after_env, max(d - 2, 1))
        lines.append(rest.roc)
        roc = "{\n" + ind("\n".join(lines)) + "\n}"
        # Maude term
        upd = f"ct(" + mlist([f"pr('add, (v('{names[0]}), i(1)))"] + [f"v('n_{nm})" for nm, _, _ in news]) + ")"
        for nm, t, e in reversed(news):
            upd = f"lt('n_{nm}, {e.mau}, {upd})"
        if inner:
            upd = inner["wrap"](upd)
        if locb:
            xn, sc, k, val = locb
            upd = f"lt('{xn}, mt({sc.mau}, (pl({k}) => {brk_m}) | (pw => {val.mau})), {upd})"
        if use_break:
            upd = f"mt({cm}, (pc('True, pnil) => {brk_m}) | (pc('False, pnil) => {upd}))"
        loop_body = (f"mt(pr('lt, (v('{names[0]}), {bound.mau})), (pc('True, pnil) => {upd}) | "
                     f"(pc('False, pnil) => {brk_m}))")
        params = mlist([f"'{nm} : {mty(t)}" for nm, t in zip(names, tys)]).replace("enil", "qnil")
        lp = (f"lp(tT({' ; '.join(mty(t) for t in tys)}), {params}, " +
              mlist([e.mau for e in inits]) + f", {loop_body})")
        mau = f"lt('{rnm}, {lp}, {rest.mau})"
        return MEx(roc, mau, I64)

    def gen_inner(self, env, d):
        """An inner while loop as a statement of the outer body; its vars are read afterwards."""
        lid = self.fresh("")
        tys = [I64, I64]
        names = [f"j{lid}", f"u{lid}"]
        init_u = self.gen_int(env, 1)
        bound = self.r.randint(0, 2)
        rnm = f"q{lid}"
        state_in = [MVar(I64, "$" + nm, f"v('{nm})") for nm in names]
        benv = env + state_in
        new_u = self.gen_int(benv, 2)
        use_break = self.r.random() < 0.3
        if use_break:
            cr, cm = self.gen_cond(benv, 2)
        body = []
        if use_break:
            body.append(f"if {cr} {{\n\tbreak\n}}")
        body += [f"n_{names[1]} = {paren(new_u.roc)}", f"n_{names[0]} = ${names[0]} + 1",
                 f"${names[0]} = n_{names[0]}", f"${names[1]} = n_{names[1]}"]
        roc = (f"var ${names[0]} = 0\nvar ${names[1]} = {paren(init_u.roc)}\n"
               f"while ${names[0]} < {bound} {{\n" + ind("\n".join(body)) + "\n}")
        brk_m = f"bk(tp((v('{names[0]}), v('{names[1]}))))"
        upd = f"lt('n_{names[1]}, {new_u.mau}, ct((pr('add, (v('{names[0]}), i(1))), v('n_{names[1]}))))"
        if use_break:
            upd = f"mt({cm}, (pc('True, pnil) => {brk_m}) | (pc('False, pnil) => {upd}))"
        body_m = (f"mt(pr('lt, (v('{names[0]}), i({bound}))), (pc('True, pnil) => {upd}) | "
                  f"(pc('False, pnil) => {brk_m}))")
        lp = f"lp(tT(tInt ; tInt), ('{names[0]} : tInt, '{names[1]} : tInt), (i(0), {init_u.mau}), {body_m})"
        # after the inner loop, the outer body reads only the second component (exit selection)
        vars_after = [MVar(I64, "$" + names[1], f"pj(v('{rnm}), 1)")]
        if self.r.random() < 0.3:
            vars_after.append(MVar(I64, "$" + names[0], f"pj(v('{rnm}), 0)"))
        return {"roc": roc, "vars": vars_after, "wrap": lambda k: f"lt('{rnm}, {lp}, {k})"}

    # ---------------- functions ----------------
    def gen_fn(self, idx):
        name = f"{self.prefix}f{idx}"
        kind = self.pick(["plain", "rec", "rec", "rec"])
        ret = self.pick([I64, I64, NAT, SH])
        if kind == "plain":
            ps = [self.pick([I64, NAT, SH, NAT]) for _ in range(self.r.randint(1, 2))]
            pnames = [f"a{k}" for k in range(len(ps))]
            env = [MVar(t, n, f"v('{n})") for t, n in zip(ps, pnames)]
            body = self.gen(ret, env, 3)
        else:
            extra = self.pick([[], [I64], [SH]])
            ps = [NAT, I64] + extra
            pnames = ["x", "fuel"] + [f"a{k}" for k in range(len(extra))]
            env = [MVar(t, n, f"v('{n})") for t, n in zip(ps, pnames)]
            self.fns.append((name, ps, ret))  # visible to its own body for recursion
            base = self.gen(ret, [env[0]] + env[2:], 1)
            # recursive arms call name(newx, fuel - 1, ...)
            mn = self.fresh("m")
            armenv = env + [MVar(NAT, mn, f"v('{mn})")]
            arms = []
            for p, mp in [("Z", "pc('Z, pnil)"), (f"S({mn})", f"pc('S, pv('{mn}))")]:
                e = self.gen_rec_arm(name, ps, ret, armenv if "S" in p else env, 2)
                arms.append((p, mp, e))
            self.fns.pop()
            body_roc = (f"if fuel < 1 {{\n{ind(base.roc)}\n}} else {{\n\tmatch x {{\n" +
                        "\n".join(ind(f"{p} => {armtext(e.roc)}", 2) for p, _, e in arms) + "\n\t}\n}")
            body_mau = (f"mt(pr('lt, (v('fuel), i(1))), (pc('True, pnil) => {base.mau}) | (pc('False, pnil) => mt(v('x), " +
                        " | ".join(f"({mp} => {e.mau})" for _, mp, e in arms) + ")))")
            body = MEx(body_roc, body_mau, ret)
        sig = f"{name} : " + ", ".join(rty(t) for t in ps) + f" -> {rty(ret)}"
        roc = f"{sig}\n{name} = |{', '.join(pnames)}| {block(body.roc)}\n"
        params = mlist([f"'{n} : {mty(t)}" for n, t in zip(pnames, ps)]).replace("enil", "qnil")
        mau = f"fn('{name}, {params}, {mty(ret)}, {body.mau})"
        self.fns.append((name, ps, ret))
        return roc, mau

    def gen_rec_arm(self, name, ps, ret, env, d):
        """An arm that makes a recursive call on fuel - 1 with a (possibly rebuilt) Nat."""
        nats = [v for v in env if v.ty == NAT]
        x = self.pick(nats)
        newx = self.pick([MEx(x.roc, x.mau, NAT), MEx(f"S({x.roc})", f"tg('S, {x.mau})", NAT),
                          MEx(f"S(S({x.roc}))", f"tg('S, tg('S, {x.mau}))", NAT), MEx("Z", "tg('Z, enil)", NAT)])
        args = [newx, MEx("fuel - 1", "pr('sub, (v('fuel), i(1)))", I64)]
        for t in ps[2:]:
            args.append(self.gen(t, env, 1))
        call = MEx(f"{name}(" + ", ".join(paren(a.roc) for a in args) + ")",
                  f"cl('{name}, " + mlist([a.mau for a in args]) + ")", ret)
        if ret == I64 and self.r.random() < 0.6:
            other = self.gen_int(env, 1)
            return MEx(f"{paren(other.roc)} + {call.roc}", f"pr('add, ({other.mau}, {call.mau}))", I64)
        if ret == NAT and self.r.random() < 0.4:
            return MEx(f"S({call.roc})", f"tg('S, {call.mau})", NAT)
        return call

    def gen_main(self):
        name = f"{self.prefix}main"
        env = [MVar(I64, "n", "v('n)")]
        # top level: a let of a constructed value, then an int computation
        body = self.gen_int(env, 4)
        roc = f"{name} : I64 -> I64\n{name} = |n| {block(body.roc)}\n"
        mau = f"fn('{name}, 'n : tInt, tInt, {body.mau})"
        return name, roc, mau

    def program(self):
        rocs, maus = [], []
        for k in range(self.r.randint(1, 3)):
            r, m = self.gen_fn(k)
            rocs.append(r)
            maus.append(m)
        name, r, m = self.gen_main()
        rocs.append(r)
        maus.append(m)
        return name, "\n".join(rocs), " ".join(maus)




# ====================================================================== wide family

ALL = [I64, NAT, OP, STR, LST, FN]
TAGGED = (NAT, OP, FN)
LIMIT = 1 << 40
STRS = ["", "a", "bc", "xyz", "q"]


class Discard(Exception):
    """The oracle left its budget (steps, integer range, string or list size): no expectation."""


class Budget:
    steps = 0


def tick():
    Budget.steps += 1
    if Budget.steps > 200000:
        raise Discard("step budget")


def chk(v):
    if isinstance(v, int) and abs(v) > LIMIT:
        raise Discard("int range")
    if isinstance(v, (str, list)) and len(v) > 2000:
        raise Discard("size")
    return v


class Scope:
    def __init__(self, parent=None):
        self.parent, self.v = parent, {}

    def get(self, n):
        s = self
        while s is not None:
            if n in s.v:
                return s.v[n]
            s = s.parent
        raise KeyError(n)

    def put(self, n, x):
        self.v[n] = x

    def assign(self, n, x):
        s = self
        while s is not None:
            if n in s.v:
                s.v[n] = x
                return
            s = s.parent
        raise KeyError(n)


def rstr(s):
    return '"' + s + '"'


class WEx:
    """An expression: Roc text and a function from a Scope to its value."""

    def __init__(self, roc, ev, ty):
        self.roc, self.ev, self.ty = roc, ev, ty


class WVar:
    def __init__(self, ty, name, state=False):
        self.ty, self.name, self.state = ty, name, state

    def ex(self):
        n = self.name
        return WEx(n, lambda sc: sc.get(n), self.ty)


class WideGen(Base):
    """Programs outside the Maude fragment; a Python evaluator built alongside the text is the oracle."""

    def __init__(self, seed, prefix):
        super().__init__(seed, prefix)
        self.funs = {}  # name -> python callable(list of args)

    def vs(self, env, ty, pure=False):
        return [v for v in env if v.ty == ty and not (pure and v.state)]

    # ------------------------------------------------------------------ dispatch
    def gen(self, ty, env, d):
        return {I64: self.g_int, NAT: self.g_nat, OP: self.g_op, STR: self.g_str,
                LST: self.g_lst, FN: self.g_fn}[ty](env, d)

    def typed(self, e):
        """Pin a tag value to its nominal type so it never infers a structural type."""
        if e.ty == NAT:
            return WEx(f"asn({paren(e.roc)})", e.ev, NAT)
        if e.ty == OP:
            return WEx(f"aop({paren(e.roc)})", e.ev, OP)
        return e

    # ------------------------------------------------------------------ I64
    def g_int(self, env, d):
        vs = self.vs(env, I64)
        opts = ["lit"] + ["var"] * 3 * bool(vs)
        if d > 0:
            opts += ["add", "add", "sub", "if", "call", "call", "let", "let", "nmatch", "omatch",
                     "smatch", "lmatch", "lget", "slen", "llen", "fold", "apply", "apply", "depth"]
            if d > 1:
                opts += ["while", "for", "for"]
        c = self.pick(opts)
        if c == "lit":
            k = self.r.randint(0, 4)
            return WEx(str(k), lambda sc: k, I64)
        if c == "var":
            return self.pick(vs).ex()
        if c in ("add", "sub"):
            a, b = self.g_int(env, d - 1), self.g_int(env, d - 1)
            if c == "add":
                return WEx(f"({paren(a.roc)} + {paren(b.roc)})", lambda sc: chk(a.ev(sc) + b.ev(sc)), I64)
            return WEx(f"({paren(a.roc)} - {paren(b.roc)})", lambda sc: chk(a.ev(sc) - b.ev(sc)), I64)
        if c == "if":
            return self.g_if(I64, env, d)
        if c == "call":
            return self.g_call(I64, env, d) or self.g_int(env, 0)
        if c == "let":
            return self.g_let(I64, env, d)
        if c == "nmatch":
            return self.g_nmatch(I64, env, d)
        if c == "omatch":
            return self.g_omatch(I64, env, d)
        if c == "smatch":
            return self.g_smatch(I64, env, d)
        if c == "lmatch":
            return self.g_lmatch(I64, env, d)
        if c == "lget":
            xs = self.g_lst(env, d - 1)
            k = self.r.randint(0, 3)
            dflt = self.g_int(env, d - 1)
            vn = self.fresh("g")
            ok = self.g_int(env + [WVar(I64, vn)], d - 1)

            def ev(sc, xs=xs, k=k, dflt=dflt, ok=ok, vn=vn):
                l = xs.ev(sc)
                if k < len(l):
                    s2 = Scope(sc)
                    s2.put(vn, l[k])
                    return ok.ev(s2)
                return dflt.ev(sc)
            roc = (f"match List.get({paren(xs.roc)}, {k}) {{\n\tOk({vn}) => {armtext(ok.roc)}\n"
                   f"\tErr(_) => {armtext(dflt.roc)}\n}}")
            return WEx(roc, ev, I64)
        if c == "slen":
            s = self.g_str(env, d - 1)
            return WEx(f"Str.count_utf8_bytes({paren(s.roc)}).to_i64_wrap()",
                      lambda sc: len(s.ev(sc).encode()), I64)
        if c == "llen":
            xs = self.g_lst(env, d - 1)
            return WEx(f"List.len({paren(xs.roc)}).to_i64_wrap()", lambda sc: len(xs.ev(sc)), I64)
        if c == "fold":
            xs = self.g_lst(env, d - 1)
            init = self.g_int(env, d - 1)
            an, yn = self.fresh("acc"), self.fresh("y")
            body = self.g_int(self.pure(env) + [WVar(I64, an), WVar(I64, yn)], min(d - 1, 2))

            def ev(sc, xs=xs, init=init, body=body, an=an, yn=yn):
                acc = init.ev(sc)
                for y in xs.ev(sc):
                    tick()
                    s2 = Scope(sc)
                    s2.put(an, acc)
                    s2.put(yn, y)
                    acc = chk(body.ev(s2))
                return acc
            return WEx(f"List.fold({paren(xs.roc)}, {paren(init.roc)}, |{an}, {yn}| {paren(body.roc)})", ev, I64)
        if c == "apply":
            f = self.g_fn(env, d - 1)
            a = self.g_int(env, d - 1)
            fr = f.roc if f.roc.isidentifier() else f"({f.roc})"
            return WEx(f"{fr}({paren(a.roc)})", lambda sc: chk(f.ev(sc)(a.ev(sc))), I64)
        if c == "depth":
            x = self.g_nat(env, d - 1)
            return WEx(f"depth({paren(x.roc)})", x.ev, I64)
        if c == "while":
            return self.g_while(env, d)
        return self.g_for(env, d)

    def pure(self, env):
        """Variables a lambda may capture: never a `var` (state) binding."""
        return [v for v in env if not v.state]

    # ------------------------------------------------------------------ conditions
    def g_cond(self, env, d):
        d = max(d - 1, 0)
        c = self.pick(["lt", "lt", "lt", "eq", "seq", "sempty", "lempty", "lcontains", "starts"])
        if c in ("lt", "eq"):
            a, b = self.g_int(env, d), self.g_int(env, d)
            op = "<" if c == "lt" else "=="
            f = (lambda x, y: x < y) if c == "lt" else (lambda x, y: x == y)
            return f"{paren(a.roc)} {op} {paren(b.roc)}", lambda sc: f(a.ev(sc), b.ev(sc))
        if c == "seq":
            a, b = self.g_str(env, d), self.g_str(env, d)
            return f"{paren(a.roc)} == {paren(b.roc)}", lambda sc: a.ev(sc) == b.ev(sc)
        if c == "sempty":
            a = self.g_str(env, d)
            return f"Str.is_empty({paren(a.roc)})", lambda sc: a.ev(sc) == ""
        if c == "starts":
            a, b = self.g_str(env, d), self.g_str(env, d)
            return f"Str.starts_with({paren(a.roc)}, {paren(b.roc)})", lambda sc: a.ev(sc).startswith(b.ev(sc))
        if c == "lempty":
            a = self.g_lst(env, d)
            return f"List.is_empty({paren(a.roc)})", lambda sc: len(a.ev(sc)) == 0
        a, x = self.g_lst(env, d), self.g_int(env, d)
        return f"List.contains({paren(a.roc)}, {paren(x.roc)})", lambda sc: x.ev(sc) in a.ev(sc)

    def g_if(self, ty, env, d):
        cr, cev = self.g_cond(env, d)
        x, y = self.gen(ty, env, d - 1), self.gen(ty, env, d - 1)
        return WEx(f"if {cr} {{\n{ind(x.roc)}\n}} else {{\n{ind(y.roc)}\n}}",
                  lambda sc: x.ev(sc) if cev(sc) else y.ev(sc), ty)

    # ------------------------------------------------------------------ let / blocks
    def g_let(self, ty, env, d):
        bty = self.pick(ALL)
        e = self.gen(bty, env, d - 1)
        name = self.fresh("x")
        body = self.gen(ty, env + [WVar(bty, name)], d - 1)

        def ev(sc, e=e, name=name, body=body):
            s2 = Scope(sc)
            s2.put(name, e.ev(sc))
            return body.ev(s2)
        return WEx(f"{{\n\t{name} : {bty}\n{ind(name + ' = ' + paren(e.roc))}\n{ind(body.roc)}\n}}", ev, ty)

    def prefix_block(self, ty, env, d, k):
        """k lets of non-tag values computed from non-tag locals, then the rest (a prefix-split run)."""
        plain = [v for v in env if v.ty not in TAGGED]
        lets, cur = [], list(plain)
        for _ in range(k):
            bty = self.pick([I64, I64, STR, LST])
            e = self.gen(bty, self.pure(cur), 2)
            name = self.fresh("p")
            lets.append((name, bty, e))
            cur.append(WVar(bty, name))
        env2 = env + [WVar(t, n) for n, t, _ in lets]
        rest = self.gen(ty, env2, d)

        def ev(sc, lets=lets, rest=rest):
            s2 = Scope(sc)
            for name, _, e in lets:
                s2.put(name, e.ev(s2))
            return rest.ev(s2)
        lines = []
        for name, bty, e in lets:
            lines += [f"{name} : {bty}", f"{name} = {paren(e.roc)}"]
        lines.append(rest.roc)
        return WEx("{\n" + ind("\n".join(lines)) + "\n}", ev, ty)

    # ------------------------------------------------------------------ N
    def g_nat(self, env, d):
        vs = self.vs(env, NAT)
        opts = ["z", "s"] + (["var", "var", "svar"] if vs else [])
        if d > 0:
            opts += ["s", "call", "if", "match"]
        c = self.pick(opts)
        if c == "z":
            return WEx("Z", lambda sc: 0, NAT)
        if c == "var":
            return self.pick(vs).ex()
        if c == "svar":
            v = self.pick(vs).ex()
            return WEx(f"S({v.roc})", lambda sc: v.ev(sc) + 1, NAT)
        if c == "s":
            a = self.g_nat(env, d - 1)
            return WEx(f"S({paren(a.roc)})", lambda sc: chk(a.ev(sc) + 1), NAT)
        if c == "call":
            return self.g_call(NAT, env, d) or WEx("Z", lambda sc: 0, NAT)
        if c == "if":
            return self.g_if(NAT, env, d)
        return self.g_nmatch(NAT, env, d)

    def g_nmatch(self, ty, env, d):
        scr = self.typed(self.g_nat(env, d - 1))
        shape = self.pick([["Z", "S(m)"], ["S(Z)", "S(S(m))", "_"], ["S(S(_))", "m"], ["Z", "_"]])
        arms = []
        for p in shape:
            benv = list(env)
            mn = None
            if "m" in p:
                mn = self.fresh("m")
                benv.append(WVar(NAT, mn))
                p = p.replace("m", mn)
            arms.append((p, mn, self.gen(ty, benv, d - 1)))

        def matches(p, v):
            # returns (ok, bound value)
            if p == "_":
                return True, None
            if p == "Z":
                return v == 0, None
            if p.isidentifier():
                return True, v
            inner = p[2:-1]
            if v == 0:
                return False, None
            return matches(inner, v - 1)

        def ev(sc, scr=scr, arms=arms):
            v = scr.ev(sc)
            for p, mn, body in arms:
                ok, b = matches(p, v)
                if ok:
                    s2 = Scope(sc)
                    if mn:
                        s2.put(mn, b)
                    return body.ev(s2)
            raise AssertionError("non-exhaustive N match")
        roc = f"match {paren(scr.roc)} {{\n" + "\n".join(ind(f"{p} => {armtext(b.roc)}") for p, _, b in arms) + "\n}"
        return WEx(roc, ev, ty)

    # ------------------------------------------------------------------ Op (closure in a tag)
    def g_op(self, env, d):
        vs = self.vs(env, OP)
        opts = ["inc", "fn", "fn"] + (["var", "var"] if vs else [])
        if d > 0:
            opts += ["call", "if"]
        c = self.pick(opts)
        if c == "inc":
            x = self.g_int(env, max(d - 1, 0))
            return WEx(f"Inc({paren(x.roc)})", lambda sc: ("Inc", x.ev(sc)), OP)
        if c == "fn":
            f = self.g_fn(env, max(d - 1, 0))
            return WEx(f"Fn({paren(f.roc)})", lambda sc: ("Fn", f.ev(sc)), OP)
        if c == "var":
            return self.pick(vs).ex()
        if c == "call":
            return self.g_call(OP, env, d) or WEx("Inc(1)", lambda sc: ("Inc", 1), OP)
        return self.g_if(OP, env, d)

    def g_omatch(self, ty, env, d):
        scr = self.typed(self.g_op(env, d - 1))
        kn, gn = self.fresh("k"), self.fresh("h")
        a1 = self.gen(ty, env + [WVar(I64, kn)], d - 1)
        a2 = self.gen(ty, env + [WVar(FN, gn)], d - 1)

        def ev(sc):
            tag, payload = scr.ev(sc)
            s2 = Scope(sc)
            if tag == "Inc":
                s2.put(kn, payload)
                return a1.ev(s2)
            s2.put(gn, payload)
            return a2.ev(s2)
        roc = f"match {paren(scr.roc)} {{\n\tInc({kn}) => {armtext(a1.roc)}\n\tFn({gn}) => {armtext(a2.roc)}\n}}"
        return WEx(roc, ev, ty)

    # ------------------------------------------------------------------ I64 -> I64
    def g_fn(self, env, d):
        vs = self.vs(env, FN, pure=True)
        opts = ["lam", "lam"] + (["var", "var"] if vs else [])
        if d > 0:
            opts += ["call", "if", "compose"]
        c = self.pick(opts)
        if c == "lam":
            yn = self.fresh("y")
            body = self.g_int(self.pure(env) + [WVar(I64, yn)], min(max(d - 1, 0), 2))

            def ev(sc, yn=yn, body=body):
                def f(v):
                    tick()
                    s2 = Scope(sc)
                    s2.put(yn, v)
                    return chk(body.ev(s2))
                return f
            return WEx(f"|{yn}| {paren(body.roc)}", ev, FN)
        if c == "var":
            return self.pick(vs).ex()
        if c == "call":
            return self.g_call(FN, env, d) or self.g_fn(env, 0)
        if c == "compose":
            f = self.g_fn(env, d - 1)
            g = self.g_fn(env, d - 1)
            fn_, gn_ = self.fresh("cf"), self.fresh("cg")
            yn = self.fresh("y")

            def ev(sc):
                ff, gg = f.ev(sc), g.ev(sc)
                return lambda v: chk(gg(ff(v)))
            roc = (f"{{\n\t{fn_} : I64 -> I64\n\t{fn_} = {paren(f.roc)}\n\t{gn_} : I64 -> I64\n\t{gn_} = {paren(g.roc)}\n"
                   f"\t|{yn}| {gn_}({fn_}({yn}))\n}}")
            return WEx(roc, ev, FN)
        return self.g_if(FN, env, d)

    # ------------------------------------------------------------------ Str
    def g_str(self, env, d):
        vs = self.vs(env, STR)
        opts = ["lit", "lit"] + (["var", "var", "var"] if vs else [])
        if d > 0:
            opts += ["concat", "interp", "repeat", "if", "call", "smatch"]
        c = self.pick(opts)
        if c == "lit":
            s = self.pick(STRS)
            return WEx(rstr(s), lambda sc: s, STR)
        if c == "var":
            return self.pick(vs).ex()
        if c == "concat":
            a, b = self.g_str(env, d - 1), self.g_str(env, d - 1)
            return WEx(f"Str.concat({paren(a.roc)}, {paren(b.roc)})", lambda sc: chk(a.ev(sc) + b.ev(sc)), STR)
        if c == "interp":
            a, b = self.g_str(env, d - 1), self.g_str(env, d - 1)
            if "\n" in a.roc or "\n" in b.roc or '"' in a.roc or '"' in b.roc:
                return WEx(f"Str.concat({paren(a.roc)}, {paren(b.roc)})", lambda sc: chk(a.ev(sc) + b.ev(sc)), STR)
            return WEx(f'"<${{{a.roc}}}-${{{b.roc}}}>"', lambda sc: chk("<" + a.ev(sc) + "-" + b.ev(sc) + ">"), STR)
        if c == "repeat":
            a = self.g_str(env, d - 1)
            k = self.r.randint(0, 3)
            return WEx(f"Str.repeat({paren(a.roc)}, {k})", lambda sc: chk(a.ev(sc) * k), STR)
        if c == "if":
            return self.g_if(STR, env, d)
        if c == "call":
            return self.g_call(STR, env, d) or WEx('"a"', lambda sc: "a", STR)
        return self.g_smatch(STR, env, d)

    def g_smatch(self, ty, env, d):
        scr = self.g_str(env, d - 1)
        picks = self.r.sample(STRS, self.r.randint(1, 3))
        arms = [(l, self.gen(ty, env, d - 1)) for l in picks]
        sn = self.fresh("s")
        dflt = self.gen(ty, env + [WVar(STR, sn)], d - 1)

        def ev(sc):
            v = scr.ev(sc)
            for l, body in arms:
                if v == l:
                    return body.ev(sc)
            s2 = Scope(sc)
            s2.put(sn, v)
            return dflt.ev(s2)
        roc = (f"match {paren(scr.roc)} {{\n" + "\n".join(ind(f"{rstr(l)} => {armtext(b.roc)}") for l, b in arms)
               + f"\n\t{sn} => {armtext(dflt.roc)}\n}}")
        return WEx(roc, ev, ty)

    # ------------------------------------------------------------------ List(I64)
    def g_lst(self, env, d):
        vs = self.vs(env, LST)
        opts = ["lit", "lit"] + (["var", "var", "var"] if vs else [])
        if d > 0:
            opts += ["append", "concat", "map", "keep", "if", "call", "lmatch"]
        c = self.pick(opts)
        if c == "lit":
            k = self.r.randint(0, 3)
            xs = [self.g_int(env, max(d - 1, 0)) for _ in range(k)]
            return WEx("[" + ", ".join(paren(x.roc) for x in xs) + "]", lambda sc: [x.ev(sc) for x in xs], LST)
        if c == "var":
            return self.pick(vs).ex()
        if c == "append":
            a, x = self.g_lst(env, d - 1), self.g_int(env, d - 1)
            return WEx(f"List.append({paren(a.roc)}, {paren(x.roc)})", lambda sc: chk(a.ev(sc) + [x.ev(sc)]), LST)
        if c == "concat":
            a, b = self.g_lst(env, d - 1), self.g_lst(env, d - 1)
            return WEx(f"List.concat({paren(a.roc)}, {paren(b.roc)})", lambda sc: chk(a.ev(sc) + b.ev(sc)), LST)
        if c == "map":
            a, f = self.g_lst(env, d - 1), self.g_fn(env, d - 1)
            return WEx(f"List.map({paren(a.roc)}, {paren(f.roc)})",
                      lambda sc: [f.ev(sc)(v) for v in a.ev(sc)], LST)
        if c == "keep":
            a = self.g_lst(env, d - 1)
            yn = self.fresh("y")
            k = self.g_int(self.pure(env), 1)

            def ev(sc):
                bound = k.ev(sc)
                return [v for v in a.ev(sc) if v < bound]
            return WEx(f"List.keep_if({paren(a.roc)}, |{yn}| {yn} < {paren(k.roc)})", ev, LST)
        if c == "if":
            return self.g_if(LST, env, d)
        if c == "call":
            return self.g_call(LST, env, d) or WEx("[]", lambda sc: [], LST)
        return self.g_lmatch(LST, env, d)

    def g_lmatch(self, ty, env, d):
        scr = self.g_lst(env, d - 1)
        hn, rn = self.fresh("hd"), self.fresh("tl")
        e0 = self.gen(ty, env, d - 1)
        e1 = self.gen(ty, env + [WVar(I64, hn), WVar(LST, rn)], d - 1)

        def ev(sc):
            v = scr.ev(sc)
            if not v:
                return e0.ev(sc)
            s2 = Scope(sc)
            s2.put(hn, v[0])
            s2.put(rn, v[1:])
            return e1.ev(s2)
        roc = f"match {paren(scr.roc)} {{\n\t[] => {armtext(e0.roc)}\n\t[{hn}, .. as {rn}] => {armtext(e1.roc)}\n}}"
        return WEx(roc, ev, ty)

    # ------------------------------------------------------------------ calls
    def g_call(self, ty, env, d):
        cands = [f for f in self.fns if f[2] == ty]
        if not cands:
            return None
        name, ps, _ = self.pick(cands)
        args = [self.typed(self.gen(p, env, d - 1)) for p in ps]
        funs = self.funs

        def ev(sc):
            tick()
            return funs[name]([a.ev(sc) for a in args])
        return WEx(f"{name}(" + ", ".join(paren(a.roc) for a in args) + ")", ev, ty)

    # ------------------------------------------------------------------ loops
    def g_while(self, env, d):
        lid = self.fresh("")
        tys = [I64] + [self.pick([I64, I64, STR, LST, NAT]) for _ in range(self.r.randint(1, 2))]
        names = [f"$i{lid}"] + [f"$s{lid}_{k}" for k in range(1, len(tys))]
        inits = [WEx("0", lambda sc: 0, I64)] + [self.typed(self.gen(t, env, 1)) for t in tys[1:]]
        bound = self.r.randint(0, 3)
        state = [WVar(t, n, state=True) for t, n in zip(tys, names)]
        benv = env + state
        brk = self.g_cond(benv, 2) if self.chance(0.35) else None
        news = [(n, t, self.gen(t, benv, min(d - 1, 2))) for t, n in zip(tys[1:], names[1:])]
        rest = self.g_int(env + state, max(d - 2, 1))

        def ev(sc):
            s2 = Scope(sc)
            for n, e in zip(names, inits):
                s2.put(n, e.ev(sc))
            while s2.get(names[0]) < bound:
                tick()
                s3 = Scope(s2)
                if brk and brk[1](s3):
                    break
                vals = [(n, e.ev(s3)) for n, _, e in news]
                s2.assign(names[0], s2.get(names[0]) + 1)
                for n, v in vals:
                    s2.assign(n, v)
            return rest.ev(s2)
        lines = []
        for n, t, e in zip(names, tys, inits):
            lines.append(f"var {n} = {paren(e.roc)}")
        body = []
        if brk:
            body.append(f"if {brk[0]} {{\n\tbreak\n}}")
        for n, _, e in news:
            body.append(f"n_{n[1:]} = {paren(e.roc)}")
        body.append(f"{names[0]} = {names[0]} + 1")
        for n, _, _ in news:
            body.append(f"{n} = n_{n[1:]}")
        lines.append(f"while {names[0]} < {bound} {{\n" + ind("\n".join(body)) + "\n}")
        lines.append(rest.roc)
        return WEx("{\n" + ind("\n".join(lines)) + "\n}", ev, I64)

    def g_for(self, env, d):
        lid = self.fresh("")
        xs = self.g_lst(env, d - 1)
        acc = f"$acc{lid}"
        yn = self.fresh("e")
        init = self.g_int(env, 1)
        benv = env + [WVar(I64, acc, state=True), WVar(I64, yn)]
        brk = self.g_cond(benv, 2) if self.chance(0.3) else None
        upd = self.g_int(benv, min(d - 1, 2))
        rest = self.g_int(env + [WVar(I64, acc, state=True)], max(d - 2, 1))

        def ev(sc):
            s2 = Scope(sc)
            s2.put(acc, init.ev(sc))
            for v in xs.ev(sc):
                tick()
                s3 = Scope(s2)
                s3.put(yn, v)
                if brk and brk[1](s3):
                    break
                s2.assign(acc, chk(upd.ev(s3)))
            return rest.ev(s2)
        body = []
        if brk:
            body.append(f"if {brk[0]} {{\n\tbreak\n}}")
        body.append(f"{acc} = {paren(upd.roc)}")
        roc = ("{\n" + ind(f"var {acc} = {paren(init.roc)}\nfor {yn} in {paren(xs.roc)} {{\n" + ind("\n".join(body))
                           + "\n}\n" + rest.roc) + "\n}")
        return WEx(roc, ev, I64)

    # ------------------------------------------------------------------ functions
    def define(self, name, pnames, body):
        def run(args, pnames=pnames, body=body):
            tick()
            sc = Scope()
            for n, a in zip(pnames, args):
                sc.put(n, a)
            return body.ev(sc)
        self.funs[name] = run

    def g_function(self, idx):
        name = f"{self.prefix}f{idx}"
        kind = self.pick(["plain", "prefix", "prefix", "rec", "rec", "hof", "mk"])
        if kind == "mk":
            # a function returning a closure that captures its arguments
            ps = [I64] + ([self.pick([I64, STR, LST])] if self.chance(0.5) else [])
            pnames = [f"a{k}" for k in range(len(ps))]
            env = [WVar(t, n) for t, n in zip(ps, pnames)]
            ret = FN
            body = self.g_fn(env, 2)
            if not body.roc.startswith("|"):
                yn = self.fresh("y")
                inner = self.g_int(env + [WVar(I64, yn)], 1)

                def ev(sc, yn=yn, inner=inner):
                    def f(v):
                        s2 = Scope(sc)
                        s2.put(yn, v)
                        return chk(inner.ev(s2))
                    return f
                body = WEx(f"|{yn}| {paren(inner.roc)}", ev, FN)
        elif kind in ("plain", "prefix"):
            ret = self.pick([I64, I64, NAT, STR, LST, OP])
            ps = [self.pick(ALL) for _ in range(self.r.randint(1, 3))]
            if kind == "prefix":
                ps = [self.pick([NAT, OP, FN])] + [self.pick([I64, STR, LST]) for _ in range(self.r.randint(1, 2))]
            pnames = [f"a{k}" for k in range(len(ps))]
            env = [WVar(t, n) for t, n in zip(ps, pnames)]
            if kind == "prefix":
                body = self.prefix_block(ret, env, 2, self.r.randint(2, 4))
            else:
                body = self.gen(ret, env, 3)
        else:
            # fuel-bounded recursion on N, optionally threading a closure (hof) and prefix lets
            ret = self.pick([I64, I64, NAT, STR, LST])
            extra = self.pick([[], [I64], [STR], [LST]])
            ps = [NAT, I64] + ([FN] if kind == "hof" or self.chance(0.4) else []) + extra
            pnames = ["x", "fuel"] + [f"a{k}" for k in range(len(ps) - 2)]
            env = [WVar(t, n) for t, n in zip(ps, pnames)]
            # optional prefix lets in front of the recursion: independent of x and of the closure
            lets = []
            if self.chance(0.5):
                plain = [v for v in env if v.ty not in TAGGED]
                for _ in range(self.r.randint(2, 3)):
                    t = self.pick([I64, STR, LST])
                    e = self.gen(t, plain, 2)
                    pname = self.fresh("p")
                    lets.append((pname, t, e))
                    plain = plain + [WVar(t, pname)]
                env = env + [WVar(t, pname) for pname, t, _ in lets]
            base = self.gen(ret, [v for v in env if v.name not in ("x", "fuel")] + [env[0]], 1)
            mn = self.fresh("m")
            arms = []
            for pat in ("Z", f"S({mn})"):
                aenv = env + ([WVar(NAT, mn)] if pat != "Z" else [])
                arms.append((pat, self.g_rec_arm(name, ps, pnames, ret, aenv)))

            def ev(sc, arms=arms, base=base, mn=mn):
                if sc.get("fuel") < 1:
                    return base.ev(sc)
                x = sc.get("x")
                s2 = Scope(sc)
                if x == 0:
                    return arms[0][1].ev(s2)
                s2.put(mn, x - 1)
                return arms[1][1].ev(s2)
            roc = (f"if fuel < 1 {{\n{ind(base.roc)}\n}} else {{\n\tmatch x {{\n" +
                   "\n".join(ind(f"{p} => {armtext(e.roc)}", 2) for p, e in arms) + "\n\t}\n}")
            body = WEx(roc, ev, ret)
            if lets:
                terms = []
                for pname, t, _ in lets:
                    if t == I64:
                        terms.append((pname, lambda sc, pname=pname: sc.get(pname)))
                    elif t == STR:
                        terms.append((f"Str.count_utf8_bytes({pname}).to_i64_wrap()",
                                      lambda sc, pname=pname: len(sc.get(pname).encode())))
                    else:
                        terms.append((f"List.len({pname}).to_i64_wrap()", lambda sc, pname=pname: len(sc.get(pname))))
                rn = self.fresh("r")
                inner = body

                def ev2(sc, lets=lets, inner=inner, terms=terms, rn=rn):
                    s2 = Scope(sc)
                    for pname, _, e in lets:
                        s2.put(pname, e.ev(s2))
                    r = inner.ev(s2)
                    if ret != I64:
                        return r
                    return chk(r + sum(f(s2) for _, f in terms))
                lines = []
                for pname, t, e in lets:
                    lines += [f"{pname} : {t}", f"{pname} = {paren(e.roc)}"]
                if ret == I64:
                    lines += [f"{rn} : I64", f"{rn} = {paren(inner.roc)}", " + ".join([rn] + [t for t, _ in terms])]
                else:
                    lines.append(inner.roc)
                body = WEx("{\n" + ind("\n".join(lines)) + "\n}", ev2, ret)
        sig = f"{name} : " + ", ".join(self.sigty(t) for t in ps) + f" -> {self.sigty(ret)}"
        roc = f"{sig}\n{name} = |{', '.join(pnames)}| {block(body.roc)}\n"
        self.define(name, pnames, body)
        self.fns.append((name, ps, ret))
        return roc

    def g_rec_arm(self, name, ps, pnames, ret, env):
        nats = [v for v in env if v.ty == NAT]
        x = self.pick(nats).ex()
        newx = self.pick([x, WEx(f"S({x.roc})", lambda sc: x.ev(sc) + 1, NAT),
                          WEx(f"S(S({x.roc}))", lambda sc: x.ev(sc) + 2, NAT), WEx("Z", lambda sc: 0, NAT)])
        args = [newx, WEx("fuel - 1", lambda sc: sc.get("fuel") - 1, I64)]
        for t, pname in zip(ps[2:], pnames[2:]):
            if t == FN:
                g = WVar(FN, pname).ex()
                c = self.pick(["same", "same", "wrap", "fresh"])
                if c == "same":
                    args.append(g)
                elif c == "wrap":
                    yn = self.fresh("y")
                    k = self.g_int(self.pure(env), 1)

                    def ev(sc, g=g, k=k):
                        gg, kk = g.ev(sc), k.ev(sc)
                        return lambda v: chk(gg(v) + kk)
                    args.append(WEx(f"|{yn}| {pname}({yn}) + {paren(k.roc)}", ev, FN))
                else:
                    args.append(self.g_fn(env, 1))
            else:
                args.append(self.gen(t, env, 1))
        funs = self.funs
        call = WEx(f"{name}(" + ", ".join(paren(a.roc) for a in args) + ")",
                  lambda sc: funs[name]([a.ev(sc) for a in args]), ret)
        if ret == I64 and self.chance(0.6):
            other = self.g_int(env, 1)
            return WEx(f"{paren(other.roc)} + {call.roc}", lambda sc: chk(other.ev(sc) + call.ev(sc)), I64)
        if ret == NAT and self.chance(0.4):
            return WEx(f"S({call.roc})", lambda sc: chk(call.ev(sc) + 1), NAT)
        if ret == STR and self.chance(0.4):
            return WEx(f"Str.concat(\"a\", {call.roc})", lambda sc: chk("a" + call.ev(sc)), STR)
        return call

    def sigty(self, t):
        return f"({t})" if t == FN else t

    def gen_main(self):
        name = f"{self.prefix}main"
        env = [WVar(I64, "n")]
        body = self.g_int(env, 4)
        self.define(name, ["n"], body)
        return name, f"{name} : I64 -> I64\n{name} = |n| {block(body.roc)}\n"

    def program(self):
        rocs = [self.g_function(k) for k in range(self.r.randint(1, 4))]
        name, r = self.gen_main()
        rocs.append(r)
        return name, "\n".join(rocs)

    def oracle(self, name, k):
        Budget.steps = 0
        try:
            return self.funs[name]([k])
        except (Discard, RecursionError):
            return None



# ====================================================================== shared

ROC_PRELUDE = """N := [Z, S(N)]
Sh := [A, B(I64), C(N, I64)]
Op := [Inc(I64), Fn(I64 -> I64)]

asn : N -> N
asn = |v| v

ash : Sh -> Sh
ash = |v| v

aop : Op -> Op
aop = |v| v

depth : N -> I64
depth = |d| match d {
\tZ => 0
\tS(e) => 1 + depth(e)
}
"""

MAUDE_TYPES = ("ty('Bool, cd('True, tnil) ; cd('False, tnil)) "
               "ty('Nat, cd('Z, tnil) ; cd('S, tN('Nat))) "
               "ty('Sh, cd('A, tnil) ; cd('B, tInt) ; cd('C, (tN('Nat) ; tInt))) "
               "fn('asn, 'v : tN('Nat), tN('Nat), v('v)) fn('ash, 'v : tN('Sh), tN('Sh), v('v)) "
               "fn('depth, 'd : tN('Nat), tInt, mt(v('d), (pc('Z, pnil) => i(0)) | "
               "(pc('S, pv('e)) => pr('add, (i(1), cl('depth, v('e)))))))")
