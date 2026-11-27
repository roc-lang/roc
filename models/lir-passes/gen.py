"""Random small Roc programs aimed at the LIR optimization passes.

usage: python3 -I gen.py --seed S [--groups K] OUT.roc

Two levels of small typed constructs, composed at random:
  - expressions of type I64, Bool, Str, List(I64) and Sh (`e_int`, `e_bool`, ...), each a random
    choice among arithmetic, if, match, closures, List/Str builtins and calls to functions already
    defined at a matching type;
  - function definitions of type I64 -> T (`fn_*`), each one construct (a loop appending to a list or
    string, self recursion in tail or constructor position, a Try chain with `?`, ...) whose holes are
    filled with random expressions. Every definition joins the pool of callees for its result type.
The constructs are chosen to exercise tag-case fusion and known-tag jumps (matches on call results),
scalarization and join-parameter pruning (tuples through joins), TRMC/TCE, loop append promotion and
str_append. Each group ends with a `main` and an `expect` per function that `dbg`s its result; those
expects are the roots `check.py` runs and the lines `opt_diff.py` compares.
"""
import argparse
import random

PRELUDE = """Lst := [Nil, Cons(I64, Lst)]
Tree := [Leaf, Node(Tree, I64, Tree)]
Sh := [Circle(I64), Rect(I64, I64), Dot]

lst_sum : Lst -> I64
lst_sum = |l| match l {
	Nil => 0
	Cons(h, t) => h + lst_sum(t)
}

lst_len : Lst, I64 -> I64
lst_len = |l, acc| match l {
	Nil => acc
	Cons(_, t) => lst_len(t, acc + 1)
}

tree_sum : Tree -> I64
tree_sum = |t| match t {
	Leaf => 0
	Node(l, v, r) => tree_sum(l) + v + tree_sum(r)
}

area : Sh -> I64
area = |s| match s {
	Circle(r) => 3 * r * r
	Rect(w, h) => w * h
	Dot => 0
}
"""


class G:
    def __init__(self, rng, prefix):
        self.r = rng
        self.p = prefix
        self.n = 0
        self.fns = []  # (name, sig, src)
        self.int_fns = []  # names of I64 -> I64 helpers
        self.sh_fns = []  # names of I64 -> Sh
        self.try_fns = []  # names of I64 -> Try(I64, Str)
        self.str_fns = []  # names of I64 -> Str
        self.lst_fns = []  # names of I64 -> Lst
        self.list_fns = []  # names of I64 -> List(I64)

    def fresh(self, base="v"):
        self.n += 1
        return "%s%d" % (base, self.n)

    def lit(self):
        return str(self.r.choice([0, 1, 2, 3, 5, 7, 10]))

    # ---------------------------------------------------------- expressions by type
    def e_int(self, env, d):
        r = self.r
        ints = [v for v, t in env if t == "I64"]
        if d <= 0 or r.random() < 0.25:
            return r.choice(ints) if ints and r.random() < 0.7 else self.lit()
        c = r.randrange(14)
        if c == 0:
            return "(%s + %s)" % (self.e_int(env, d - 1), self.e_int(env, d - 1))
        if c == 1:
            return "(%s - %s)" % (self.e_int(env, d - 1), self.lit())
        if c == 2:
            return "(%s * %s)" % (self.e_int(env, d - 1), self.lit())
        if c == 3:
            return "(if %s { %s } else { %s })" % (self.e_bool(env, d - 1), self.e_int(env, d - 1), self.e_int(env, d - 1))
        if c == 4:
            return self.match_sh(env, d - 1)
        if c == 5:
            return "List.len(%s).to_i64_wrap()" % self.e_list(env, d - 1)
        if c == 6:
            return "Str.count_utf8_bytes(%s).to_i64_wrap()" % self.e_str(env, d - 1)
        if c == 7:
            a, y = self.fresh("acc"), self.fresh("y")
            return "List.fold(%s, %s, |%s, %s| %s)" % (self.e_list(env, d - 1), self.e_int(env, d - 1), a, y,
                                                      self.e_int(env + [(a, "I64"), (y, "I64")], d - 1))
        if c == 8 and self.int_fns:
            return "%s(%s)" % (r.choice(self.int_fns), self.e_int(env, d - 1))
        if c == 9 and self.try_fns:
            v, e = self.fresh("ok"), self.fresh("err")
            return "(match %s(%s) {\n\tOk(%s) => %s\n\tErr(%s) => Str.count_utf8_bytes(%s).to_i64_wrap()\n})" % (
                r.choice(self.try_fns), self.e_int(env, d - 1), v, self.e_int(env + [(v, "I64")], d - 1), e, e)
        if c == 10 and self.lst_fns:
            return "lst_sum(%s(%s))" % (r.choice(self.lst_fns), self.e_int(env, d - 1))
        if c == 11:
            return "area(%s)" % self.e_sh(env, d - 1)
        if c == 12:
            # a block with a record join
            a, b = self.fresh("pa"), self.fresh("pb")
            return "({\n\t(%s, %s) = if %s { (%s, %s) } else { (%s, %s) }\n\t%s\n})" % (
                a, b, self.e_bool(env, d - 1), self.e_int(env, d - 1), self.e_int(env, d - 1),
                self.e_int(env, d - 1), self.e_int(env, d - 1), self.e_int(env + [(a, "I64"), (b, "I64")], d - 1))
        if c == 13:
            f = self.e_closure(env, d - 1)
            return "(%s)(%s)" % (f, self.e_int(env, d - 1))
        return self.lit()

    def e_bool(self, env, d):
        r = self.r
        c = r.randrange(5)
        if c == 0:
            return "%s < %s" % (self.e_int(env, d), self.e_int(env, d))
        if c == 1:
            return "%s == %s" % (self.e_int(env, d), self.lit())
        if c == 2:
            return "Str.is_empty(%s)" % self.e_str(env, d)
        if c == 3:
            return "List.is_empty(%s)" % self.e_list(env, d)
        return "(%s > %s)" % (self.e_int(env, d), self.lit())

    def e_str(self, env, d):
        r = self.r
        strs = [v for v, t in env if t == "Str"]
        if d <= 0 or r.random() < 0.3:
            if strs and r.random() < 0.6:
                return r.choice(strs)
            return r.choice(['""', '"a"', '"bc"', '"x-y"'])
        c = r.randrange(5)
        if c == 0:
            return '"${%s}-${%s.to_str()}"' % (self.e_str(env, d - 1), self.e_int(env, d - 1))
        if c == 1:
            return "Str.concat(%s, %s)" % (self.e_str(env, d - 1), self.e_str(env, d - 1))
        if c == 2:
            return "(if %s { %s } else { %s })" % (self.e_bool(env, d - 1), self.e_str(env, d - 1), self.e_str(env, d - 1))
        if c == 3 and self.str_fns:
            return "%s(%s)" % (r.choice(self.str_fns), self.e_int(env, d - 1))
        return "%s.to_str()" % self.e_int(env, d - 1)

    def e_list(self, env, d):
        r = self.r
        lists = [v for v, t in env if t == "List"]
        if d <= 0 or r.random() < 0.3:
            if lists and r.random() < 0.6:
                return r.choice(lists)
            return "[%s]" % ", ".join(self.lit() for _ in range(r.randrange(4)))
        c = r.randrange(5)
        if c == 0:
            return "List.append(%s, %s)" % (self.e_list(env, d - 1), self.e_int(env, d - 1))
        if c == 1:
            y = self.fresh("y")
            return "List.map(%s, |%s| %s)" % (self.e_list(env, d - 1), y, self.e_int(env + [(y, "I64")], d - 1))
        if c == 2:
            y = self.fresh("y")
            return "List.keep_if(%s, |%s| %s)" % (self.e_list(env, d - 1), y, self.e_bool(env + [(y, "I64")], d - 1))
        if c == 3 and self.list_fns:
            return "%s(%s)" % (r.choice(self.list_fns), self.e_int(env, d - 1))
        return "List.concat(%s, %s)" % (self.e_list(env, d - 1), self.e_list(env, d - 1))

    def e_sh(self, env, d):
        r = self.r
        c = r.randrange(5)
        if c == 0:
            return "Circle(%s)" % self.e_int(env, d - 1)
        if c == 1:
            return "Rect(%s, %s)" % (self.e_int(env, d - 1), self.e_int(env, d - 1))
        if c == 2 and self.sh_fns:
            return "%s(%s)" % (r.choice(self.sh_fns), self.e_int(env, d - 1))
        if c == 3:
            return "(if %s { %s } else { %s })" % (self.e_bool(env, d - 1), self.e_sh(env, d - 1), self.e_sh(env, d - 1))
        return "Dot"

    def match_sh(self, env, d):
        a, b, c = self.fresh("r"), self.fresh("w"), self.fresh("h")
        return "(match %s {\n\tCircle(%s) => %s\n\tRect(%s, %s) => %s\n\tDot => %s\n})" % (
            self.e_sh(env, d), a, self.e_int(env + [(a, "I64")], d - 1), b, c,
            self.e_int(env + [(b, "I64"), (c, "I64")], d - 1), self.e_int(env, d - 1))

    def e_closure(self, env, d):
        y = self.fresh("y")
        if self.r.random() < 0.5:
            return "|%s| %s" % (y, self.e_int(env + [(y, "I64")], d))
        return "if %s { |%s| %s } else { |%s| %s }" % (self.e_bool(env, d), y, self.e_int(env + [(y, "I64")], d),
                                                    y, self.e_int(env + [(y, "I64")], d))

    # ---------------------------------------------------------- functions
    def fn_int(self):
        name = self.fresh(self.p + "fi")
        env = [("x", "I64")]
        src = "%s : I64 -> I64\n%s = |x| %s\n" % (name, name, self.e_int(env, 3))
        self.fns.append((name, src, True))
        self.int_fns.append(name)

    def fn_sh(self):
        name = self.fresh(self.p + "fs")
        env = [("x", "I64")]
        src = "%s : I64 -> Sh\n%s = |x| if %s {\n\tCircle(%s)\n} else if %s {\n\tRect(x, %s)\n} else {\n\tDot\n}\n" % (
            name, name, self.e_bool(env, 1), self.e_int(env, 2), self.e_bool(env, 1), self.e_int(env, 2))
        self.fns.append((name, src, True))
        self.sh_fns.append(name)

    def fn_try(self):
        name = self.fresh(self.p + "ft")
        env = [("x", "I64")]
        if self.r.random() < 0.5 and self.try_fns:
            other = self.r.choice(self.try_fns)
            a = self.fresh("a")
            body = "{\n\t%s = %s(x)?\n\tb = %s(%s - 1)?\n\tOk(%s + b)\n}" % (
                a, other, other, a, self.e_int(env + [(a, "I64")], 2))
        else:
            body = "if %s {\n\tErr(%s)\n} else {\n\tOk(%s)\n}" % (self.e_bool(env, 1), self.e_str(env, 1), self.e_int(env, 2))
        src = "%s : I64 -> Try(I64, Str)\n%s = |x| %s\n" % (name, name, body)
        self.fns.append((name, src, True))
        self.try_fns.append(name)

    def fn_lst(self):
        """Constructor-position recursion: TRMC."""
        name = self.fresh(self.p + "fl")
        env = [("n", "I64")]
        r = self.r
        if r.random() < 0.5:
            body = "if n <= 0 {\n\tNil\n} else {\n\tCons(%s, %s(n - 1))\n}" % (self.e_int(env, 2), name)
        else:
            body = ("if n <= 0 {\n\tNil\n} else if %s {\n\t%s(n - 1)\n} else {\n\tCons(%s, %s(n - %s))\n}"
                    % (self.e_bool(env, 1), name, self.e_int(env, 2), name, r.choice(["1", "2"])))
        src = "%s : I64 -> Lst\n%s = |n| %s\n" % (name, name, body)
        self.fns.append((name, src, True))
        self.lst_fns.append(name)

    def fn_tree(self):
        name = self.fresh(self.p + "ftr")
        env = [("n", "I64")]
        body = "if n <= 0 {\n\tLeaf\n} else {\n\tNode(%s(n - 2), %s, %s(n - 1))\n}" % (name, self.e_int(env, 1), name)
        src = "%s : I64 -> Tree\n%s = |n| %s\n" % (name, name, body)
        self.fns.append((name, src, True))
        return name

    def fn_tail(self):
        """Tail recursion with several tail sites, possibly returning a tag."""
        name = self.fresh(self.p + "fa")
        env = [("n", "I64"), ("acc", "I64")]
        r = self.r
        if r.random() < 0.5:
            base = "Ok({ value: acc, n })"
            ret = "Try({ value : I64, n : I64 }, Str)"
            use = "(match %s(x, %s) {\n\tOk(rec) => rec.value + rec.n\n\tErr(_) => 0\n})"
        else:
            base = "acc"
            ret = "I64"
            use = "%s(x, %s)"
        body = ("if n <= 0 {\n\t%s\n} else if %s {\n\t%s(n - 1, %s)\n} else {\n\t%s(n - 1, %s)\n}"
                % (base, self.e_bool(env, 1), name, self.e_int(env, 2), name, self.e_int(env, 2)))
        src = "%s : I64, I64 -> %s\n%s = |n, acc| %s\n" % (name, ret, name, body)
        self.fns.append((name, src, False))
        wrapper = self.fresh(self.p + "fw")
        wsrc = "%s : I64 -> I64\n%s = |x| %s\n" % (wrapper, wrapper, use % (name, self.lit()))
        self.fns.append((wrapper, wsrc, True))
        self.int_fns.append(wrapper)

    def fn_loop_list(self):
        """for/while loops appending to a list var: loop append promotion."""
        name = self.fresh(self.p + "fll")
        r = self.r
        env = [("n", "I64"), ("i", "I64")]
        if r.random() < 0.5:
            body = ("{\n\tvar $out = %s\n\tvar $i = 0\n\twhile $i < n {\n\t\ti = $i\n\t\tif %s {\n\t\t\t$out = List.append($out, %s)\n\t\t}\n\t\t$i = $i + 1\n\t}\n\t$out\n}"
                    % (r.choice(["[]", "[1]", "List.with_capacity(2)"]), self.e_bool(env, 1), self.e_int(env, 2)))
        else:
            ys = self.fresh("y")
            body = ("{\n\tvar $out = []\n\tfor %s in %s {\n\t\t$out = List.append($out, %s)\n\t\tif %s {\n\t\t\t$out = List.append($out, %s)\n\t\t}\n\t}\n\t$out\n}"
                    % (ys, self.e_list([("n", "I64")], 1) if r.random() < 0.5 else "0..<n",
                       self.e_int([("n", "I64"), (ys, "I64")], 2), self.e_bool([("n", "I64"), (ys, "I64")], 1),
                       self.e_int([("n", "I64"), (ys, "I64")], 1)))
        src = "%s : I64 -> List(I64)\n%s = |n| %s\n" % (name, name, body)
        self.fns.append((name, src, True))
        self.list_fns.append(name)

    def fn_loop_str(self):
        name = self.fresh(self.p + "fls")
        r = self.r
        env = [("n", "I64"), ("i", "I64")]
        step = r.choice(["Str.concat($s, %s)", '"${$s}${%s}"', '"${$s},${%s}"'])
        body = ("{\n\tvar $s = %s\n\tvar $i = 0\n\twhile $i < n {\n\t\ti = $i\n\t\t$s = %s\n\t\t$i = $i + 1\n\t}\n\t$s\n}"
                % (r.choice(['""', '"<"']), step % self.e_str(env, 2)))
        src = "%s : I64 -> Str\n%s = |n| %s\n" % (name, name, body)
        self.fns.append((name, src, True))
        self.str_fns.append(name)

    def fn_str(self):
        name = self.fresh(self.p + "fst")
        env = [("x", "I64")]
        r = self.r
        c = r.randrange(3)
        if c == 0:
            body = "Str.concat(%s, %s)" % (self.e_str(env, 2), self.e_str(env, 2))
        elif c == 1:
            # recursive, so it stays a call that str_append can fuse
            body = 'if x <= 0 {\n\t%s\n} else {\n\tStr.concat(%s, %s(x - 1))\n}' % (
                self.e_str(env, 1), self.e_str(env, 1), name)
        else:
            body = '"${%s}:${%s}"' % (self.e_str(env, 2), self.e_str(env, 2))
        src = "%s : I64 -> Str\n%s = |x| %s\n" % (name, name, body)
        self.fns.append((name, src, True))
        self.str_fns.append(name)
        # and a loop that appends the helper's result (str_append fusion)
        lname = self.fresh(self.p + "fsl")
        lbody = ("{\n\tvar $s = %s\n\tfor i in 0..<n {\n\t\t$s = %s\n\t}\n\t$s\n}"
                 % (r.choice(['""', '"["']), r.choice(["Str.concat($s, %s(i))" % name, '"${$s}${%s(i)}"' % name])))
        self.fns.append((lname, "%s : I64 -> Str\n%s = |n| %s\n" % (lname, lname, lbody), True))
        self.str_fns.append(lname)

    def fn_flag_loop(self):
        """A loop ended through a flag (known-tag jump threading)."""
        name = self.fresh(self.p + "ffl")
        r = self.r
        env = [("n", "I64"), ("$i", "I64")]
        body = ("{\n\tvar $done = False\n\tvar $i = 0\n\tvar $acc = %s\n\twhile !$done {\n\t\t$acc = List.append($acc, %s)\n\t\t$i = $i + 1\n\t\tif $i >= n or %s {\n\t\t\t$done = True\n\t\t}\n\t}\n\t$acc\n}"
                % (r.choice(["[]", "[0]"]), self.e_int(env, 2), self.e_bool(env, 1)))
        src = "%s : I64 -> List(I64)\n%s = |n| %s\n" % (name, name, body)
        self.fns.append((name, src, True))
        self.list_fns.append(name)

    def fn_early(self):
        """Early return out of a loop."""
        name = self.fresh(self.p + "fe")
        y = self.fresh("y")
        env = [("n", "I64"), (y, "I64")]
        body = ("{\n\tfor %s in %s {\n\t\tif %s {\n\t\t\treturn %s\n\t\t}\n\t}\n\t%s\n}"
                % (y, self.e_list([("n", "I64")], 2), self.e_bool(env, 1), self.e_sh(env, 2), self.e_sh([("n", "I64")], 1)))
        src = "%s : I64 -> Sh\n%s = |n| %s\n" % (name, name, body)
        self.fns.append((name, src, True))
        self.sh_fns.append(name)

    def program(self, nfns):
        r = self.r
        makers = [self.fn_int, self.fn_sh, self.fn_try, self.fn_lst, self.fn_tail, self.fn_loop_list,
                  self.fn_loop_str, self.fn_tree, self.fn_str, self.fn_flag_loop, self.fn_early]
        trees = []
        for _ in range(nfns):
            m = r.choice(makers)
            res = m()
            if m == self.fn_tree:
                trees.append(res)
        main = self.p + "main"
        env = [("x", "I64")]
        parts = [self.e_int(env, 3) for _ in range(2)]
        for t in trees:
            parts.append("tree_sum(%s(x))" % t)
        body = " + ".join("(%s)" % p for p in parts)
        out = [src for _, src, _ in self.fns]
        out.append("%s : I64 -> I64\n%s = |x| %s\n" % (main, main, body))
        exps = []
        for k in range(5):
            exps.append('expect {\n\tdbg ("%s", %d, %s(%d))\n\tTrue\n}' % (main, k, main, k))
        # roots for every helper, so each is run on its own as well
        for name, _, unary in self.fns:
            if unary:
                exps.append('expect {\n\tdbg ("%s", %s(3))\n\tTrue\n}' % (name, name))
        return "\n".join(out) + "\n" + "\n".join(exps) + "\n"


def main():
    ap = argparse.ArgumentParser(description="Generate a random Roc program for the LIR pass model.")
    ap.add_argument("out")
    ap.add_argument("--seed", type=int, required=True)
    ap.add_argument("--groups", type=int, default=4, help="independent function groups (default 4)")
    a = ap.parse_args()
    srcs = [PRELUDE]
    for i in range(a.groups):
        g = G(random.Random(a.seed * 1000 + i), "g%d_" % i)
        srcs.append(g.program(random.Random(a.seed * 1000 + i + 7).randrange(3, 8)))
    with open(a.out, "w") as f:
        f.write("\n".join(srcs))


if __name__ == "__main__":
    main()
