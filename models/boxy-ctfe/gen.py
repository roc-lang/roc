"""Program generator and emitter for the boxy-ctfe model.

A program is a list of probes plus the top-level helpers, nominal types and recursive functions they call. Each probe
is one closed expression, emitted twice from the same tree:

  c<i> : T                     the compile-time side: a top-level constant, which the compiler evaluates while checking
  c<i> = E

  r<i> : U64 -> T              the runtime side: the same expression inside the else branch of a condition on the
  r<i> = |z| if z != 0 { crash "rt" } else { E' }      argument, so nothing in E' is a compile-time root

E' differs from E only where E reads an earlier probe: the constant reads `c<j>`, the runtime side calls `r<j>(z)`.
`main!` prints, per data probe, the constant inspected at compile time, the constant inspected at runtime, and the
runtime side inspected at runtime (and `==` of the two sides at runtime when the type has equality). A function-typed
probe is printed by applying both sides to fixed arguments. Every line of a probe must agree, and every configuration
must print the same thing; there is no reference evaluator, the program is its own reference.

Expressions are composed at random from small typed constructs, directed by the type they must produce. Arithmetic
wraps (`plus_wrap` and friends), `Dec` only adds and subtracts small values, division is `rem_by` a constant of at
least 2, so every compared expression is total and has one value.
"""
import random

# Shapes of filed bugs the generator leaves out, so they do not hide other failures (driver `--with-known` keeps them):
# #13423, a record or tuple whose fields are all zero-sized structs, or a nominal record whose fields are all
# zero-sized.
AVOID_KNOWN = True

INTS = ["U8", "U16", "U32", "U64", "U128", "I8", "I16", "I32", "I64", "I128"]
BITS = {"U8": 8, "U16": 16, "U32": 32, "U64": 64, "U128": 128, "I8": 8, "I16": 16, "I32": 32, "I64": 64, "I128": 128}
FLOATS = ["F32", "F64"]
WORDS = ["", "a", "bc", "xyz", "hello", "q r", "é", "日本", "a long string that is not small at all!"]
TAGS = ["Alpha", "Beta", "Gamma", "Delta", "Eps"]
FIELDS = ["a", "b", "c", "d"]

# Types are tuples:
#   ("int", W) ("float", W) ("dec",) ("bool",) ("str",) ("unit",)
#   ("rec", ((field, T), ...))   ("tup", (T, T, ...))   ("tags", ((Tag, (T, ...)), ...))   ("list", T)
#   ("try", T, E)  with E a "tags" type     ("nom", name)     ("fn", A, B)
U64 = ("int", "U64")
BOOL = ("bool",)
STR = ("str",)
UNIT = ("unit",)
ERR_OOB = ("tags", (("OutOfBounds", ()),))
ERR_EMPTY = ("tags", (("ListWasEmpty", ()),))


def int_conv(src, dst):
    """The method converting int width src to dst: lossless when every value fits, else wrapping."""
    sb, db = BITS[src], BITS[dst]
    if src == dst:
        return f"to_{dst.lower()}_wrap"
    if signed(src) == signed(dst):
        lossless = db > sb
    else:
        lossless = not signed(src) and db > sb
    return f"to_{dst.lower()}" + ("" if lossless else "_wrap")


ERR_RANGE = ("tags", (("OutOfRange", ()),))


def zst_struct(t):
    """A zero-sized record or tuple (or `{}`)."""
    if t[0] == "unit":
        return True
    if t[0] == "rec":
        return all(zst_struct(x) for _, x in t[1])
    if t[0] == "tup":
        return all(zst_struct(x) for x in t[1])
    return False


def zero_sized(t):
    """A zero-sized type without nominals: `{}`, a single tag whose payloads are zero-sized, records and tuples of
    them."""
    if t[0] == "unit":
        return True
    if t[0] == "rec":
        return all(zero_sized(x) for _, x in t[1])
    if t[0] == "tup":
        return all(zero_sized(x) for x in t[1])
    if t[0] == "tags":
        return len(t[1]) == 1 and all(zero_sized(x) for x in t[1][0][1])
    return False


def is_int(t):
    return t[0] == "int"


def signed(w):
    return w[0] == "I"


def int_range(w):
    b = BITS[w]
    return (-(1 << (b - 1)), (1 << (b - 1)) - 1) if signed(w) else (0, (1 << b) - 1)


def fn_of(t):
    """The function type a probe of type t is applied at: t itself, or the f field of a function-holding record."""
    if t[0] == "fn":
        return t
    if t[0] == "rec" and t[1] and t[1][0][0] == "f" and t[1][0][1][0] == "fn":
        return t[1][0][1]
    return None


class Program:
    def __init__(self):
        self.noms = {}       # name -> backing type ("rec" or "tags")
        self.helpers = []    # Helper, in creation order
        self.probes = []     # Probe, in order; a probe may read only earlier probes
        self.split = False   # constants, helpers and nominal types in a type module `Lib` the app imports

    def copy(self):
        import copy
        return copy.deepcopy(self)


class Helper:
    """A top-level function. kind: plain (params -> ret), mk (params -> (A -> B)), tail or nontail (U64, T -> T)."""

    def __init__(self, name, kind, params, ret):
        self.name, self.kind, self.params, self.ret = name, kind, params, ret
        self.body = None


class Probe:
    def __init__(self, idx, ty, expr, args=None, mode="taint"):
        self.idx, self.ty, self.expr, self.mode = idx, ty, expr, mode
        self.args = args or []   # for a function probe: argument expressions (closed, no probe reads)
        self.observe = None      # for a value holding functions elsewhere: a Str expression over `v` showing it


# --------------------------------------------------------------------------------------------- types

class Gen:
    def __init__(self, seed, nprobes=None):
        self.r = random.Random(seed)
        self.p = Program()
        self.n = 0
        self.nprobes = nprobes or self.r.randint(3, 7)

    def fresh(self, pre):
        self.n += 1
        return f"{pre}{self.n}"

    def scalar_type(self):
        r = self.r
        k = r.random()
        if k < 0.45:
            return ("int", r.choice(INTS))
        if k < 0.55:
            return ("float", r.choice(FLOATS))
        if k < 0.62:
            return ("dec",)
        if k < 0.72:
            return BOOL
        if k < 0.92:
            return STR
        # zero-sized: {}, a single tag without payload, a record of them
        zs = [UNIT, ("tags", (("Only", ()),)), ("tags", (("One", (UNIT,)),))]
        return r.choice(zs if AVOID_KNOWN else zs + [("rec", (("a", UNIT),))])

    def avoid_zst(self, t):
        """t, or (under AVOID_KNOWN) t with its first part made a U8 when every part is a zero-sized struct."""
        parts = [x for _, x in t[1]] if t[0] == "rec" else list(t[1])
        if not AVOID_KNOWN or not all(zst_struct(x) for x in parts):
            return t
        if t[0] == "rec":
            return ("rec", ((t[1][0][0], ("int", "U8")),) + t[1][1:])
        return ("tup", (("int", "U8"),) + t[1][1:])

    def data_type(self, depth=2):
        r = self.r
        if depth <= 0 or r.random() < 0.35:
            return self.scalar_type()
        k = r.random()
        if k < 0.2:
            fs = r.sample(FIELDS, r.randint(1, 3))
            return self.avoid_zst(("rec", tuple(sorted((f, self.data_type(depth - 1)) for f in fs))))
        if k < 0.3:
            return self.avoid_zst(("tup", tuple(self.data_type(depth - 1) for _ in range(r.randint(2, 3)))))
        if k < 0.5:
            return self.tags_type(depth - 1)
        if k < 0.7:
            return ("list", self.data_type(depth - 1))
        if k < 0.82:
            err = r.choice([ERR_OOB, ERR_EMPTY, ERR_RANGE, self.tags_type(0)])
            return ("try", self.data_type(depth - 1), err)
        if k < 0.88:
            return ("box", self.data_type(depth - 1))
        return self.nominal(depth - 1)

    def tags_type(self, depth):
        r = self.r
        names = sorted(r.sample(TAGS, r.randint(1, 3)))
        out = []
        for t in names:
            k = r.random()
            if k < 0.35:
                pl = ()
            elif k < 0.5:
                pl = (UNIT,)  # zero-sized payload
            elif k < 0.85:
                pl = (self.data_type(depth),)
            else:
                pl = (self.data_type(depth), self.data_type(0))
            out.append((t, pl))
        return ("tags", tuple(out))

    def nominal(self, depth):
        r = self.r
        if self.p.noms and r.random() < 0.4:
            return ("nom", r.choice(sorted(self.p.noms)))
        if r.random() < 0.3:
            # a recursive nominal: a chain of payloads, `Rk := [Stop, More(T, Rk)]`
            name = f"R{len(self.p.noms) + 1}"
            self.p.noms[name] = ("tags", (("More", (self.data_type(0), ("nom", name))), ("Stop", ())))
            return ("nom", name)
        if r.random() < 0.5:
            fs = r.sample(FIELDS, r.randint(1, 3))
            backing = ("rec", tuple(sorted((f, self.data_type(depth)) for f in fs)))
            if AVOID_KNOWN and all(zero_sized(x) for _, x in backing[1]):
                # #13423: a nominal record of zero-sized fields
                backing = ("rec", ((backing[1][0][0], ("int", "U8")),) + backing[1][1:])
        else:
            backing = self.tags_type(depth)
        # named after the backing is built, which may itself add a nominal
        name = f"N{len(self.p.noms) + 1}"
        self.p.noms[name] = backing
        return ("nom", name)

    def has_eq(self, t):
        k = t[0]
        if k in ("float", "fn", "box"):
            return False
        if k == "nom":
            return False  # a nominal type has == only through its own is_eq
        if k == "rec":
            return all(self.has_eq(x) for _, x in t[1])
        if k == "tup":
            return all(self.has_eq(x) for x in t[1])
        if k == "tags":
            return all(self.has_eq(x) for _, pl in t[1] for x in pl)
        if k == "list":
            return self.has_eq(t[1])
        if k == "try":
            return self.has_eq(t[1]) and self.has_eq(t[2])
        return True

    # ----------------------------------------------------------------------------------------- literals

    def int_value(self, w):
        lo, hi = int_range(w)
        r = self.r
        k = r.random()
        if k < 0.15:
            return r.choice([lo, hi, lo + 1, hi - 1])
        if k < 0.3:
            return r.randint(lo, hi)
        v = r.choice([0, 1, 2, 3, 5, 7, 10, 42, 100, 127, 200, 255, 1000, 65535])
        if signed(w) and r.random() < 0.3:
            v = -v
        return max(lo, min(hi, v))

    def lit(self, t):
        """A literal or constructor of type t, built only from literals."""
        r = self.r
        k = t[0]
        if k == "int":
            return ("lit", t, self.int_value(t[1]))
        if k == "float":
            return ("lit", t, r.choice(["0.0", "1.5", "0.1", "2.25", "1000.125", "3.0e10", "1.0e-7"]))
        if k == "dec":
            return ("lit", t, r.choice(["0.0", "1.5", "0.1", "0.2", "12.75", "999.001"]))
        if k == "bool":
            return ("lit", t, r.random() < 0.5)
        if k == "str":
            return ("lit", t, r.choice(WORDS))
        if k == "unit":
            return ("unit", t)
        if k == "rec":
            return ("rec", t, [self.lit(ft) for _, ft in t[1]])
        if k == "tup":
            return ("tup", t, [self.lit(x) for x in t[1]])
        if k == "tags":
            tag, pl = r.choice(t[1])
            return ("tag", t, tag, [self.lit(x) for x in pl])
        if k == "list":
            return ("list", t, [self.lit(t[1]) for _ in range(r.choice([0, 1, 2, 3, 3, 5]))])
        if k == "box":
            return ("call", t, "Box.box", [self.lit(t[1])])
        if k == "try":
            if r.random() < 0.6:
                return ("tag", t, "Ok", [self.lit(t[1])])
            tag, pl = r.choice(t[2][1])
            return ("tag", t, "Err", [("tag", t[2], tag, [self.lit(x) for x in pl])])
        if k == "nom":
            back = self.p.noms[t[1]]
            if self.recursive(t[1]):
                return self.chain(t, [self.lit(back[1][0][1][0]) for _ in range(r.randint(0, 3))])
            return ("nomwrap", t, self.lit(back))
        if k == "fn":
            x = self.fresh("x")
            return ("lam", t, x, self.lit(t[2]))
        raise ValueError(t)

    def recursive(self, name):
        back = self.p.noms[name]
        return back[0] == "tags" and any(("nom", name) in pl for _, pl in back[1])

    def chain(self, t, items, tail=None):
        """More(i0, More(i1, ... Stop)) of recursive nominal t."""
        back = self.p.noms[t[1]]
        e = tail or ("nomwrap", t, ("tag", back, "Stop", []))
        for it in reversed(items):
            e = ("nomwrap", t, ("tag", back, "More", [it, e]))
        return e

    # ----------------------------------------------------------------------------------------- expressions

    def expr(self, t, env, depth, probes):
        """An expression of type t. env: [(name, type)] in scope; probes: earlier probe indices it may read."""
        r = self.r
        k = t[0]
        same = [v for v, vt in env if vt == t]
        if depth <= 0:
            if same and r.random() < 0.6:
                return ("var", t, r.choice(same))
            return self.lit(t)
        opts = ["lit", "let", "if", "match", "field", "call", "closure", "unbox"]
        if same:
            opts += ["var", "var"]
        readable = [p for p in probes if self.p.probes[p].ty == t]
        if readable:
            opts += ["probe", "probe"]
        if k == "int":
            opts += ["arith"] * 5 + ["conv", "conv", "len", "fold", "rec_call"]
        elif k == "float":
            opts += ["farith"] * 4 + ["fconv"]
        elif k == "dec":
            opts += ["darith"] * 3 + ["dconv"]
        elif k == "bool":
            opts += ["cmp"] * 4 + ["logic", "logic", "not", "empty"]
        elif k == "str":
            opts += ["concat", "concat", "inspect", "inspect", "to_str", "join", "interp", "rec_call"]
        elif k == "list":
            opts += ["lcons", "map", "map", "keep", "append", "lconcat", "repeat", "take", "rec_call", "withcap"]
        elif k == "try":
            opts += ["ok", "err", "lget"]
        elif k in ("rec", "tup", "tags", "nom", "unit"):
            opts += ["cons", "cons", "cons"]
            if k == "nom" and self.recursive(t[1]):
                opts += ["rec_call", "rec_call"]
        elif k == "box":
            opts += ["box", "box"]
        elif k == "fn":
            opts = ["lam", "lam", "mk", "if", "let"] + (["var"] * 2 if same else [])
        choice = r.choice(opts)
        d = depth - 1
        if choice == "lit":
            return self.lit(t)
        if choice == "var":
            return ("var", t, r.choice(same))
        if choice == "probe":
            return ("probe", t, r.choice(readable))
        if choice == "let":
            vt = self.data_type(1) if r.random() < 0.8 else ("fn", self.data_type(0), self.data_type(1))
            v = self.fresh("v")
            return ("let", t, v, self.expr(vt, env, d, probes), self.expr(t, env + [(v, vt)], d, probes))
        if choice == "if":
            return ("if", t, self.expr(BOOL, env, d, probes), self.expr(t, env, d, probes), self.expr(t, env, d, probes))
        if choice == "match":
            st = self.r.choice([self.tags_type(1), ("try", self.data_type(1), self.tags_type(0)), "nom"])
            if st == "nom":
                st = self.nominal(1)
            return self.match(t, st, env, d, probes)
        if choice == "field":
            return self.field(t, env, d, probes)
        if choice == "unbox":
            return ("call", t, "Box.unbox", [self.expr(("box", t), env, d, probes)])
        if choice == "box":
            return ("call", t, "Box.box", [self.expr(t[1], env, d, probes)])
        if choice == "call":
            return self.call_plain(t, env, d, probes)
        if choice == "closure":
            at = self.data_type(1)
            f = self.fresh("f")
            ft = ("fn", at, t)
            lam = self.expr(ft, env, d, probes)
            return ("let", t, f, lam, ("app", t, ("var", ft, f), self.expr(at, env, d, probes)))
        if choice == "rec_call":
            return self.rec_call(t, env, d, probes)
        if choice == "cons":
            return self.cons(t, env, d, probes)
        if choice == "arith":
            w = t[1]
            op = r.choice(["plus_wrap", "minus_wrap", "times_wrap", "rem_by", "bitwise_and", "bitwise_xor", "shl_wrap"])
            a = self.expr(t, env, d, probes)
            if op == "rem_by":
                return ("method", t, op, a, [("lit", t, r.choice([2, 3, 7, 10, 100]))])
            if op == "shl_wrap":
                return ("method", t, op, a, [("lit", ("int", "U8"), r.randint(0, BITS[w] + 2))])
            return ("method", t, op, a, [self.expr(t, env, d, probes)])
        if choice == "conv":
            if r.random() < 0.15:
                src = ("float", r.choice(FLOATS))
                return ("method", t, f"to_{t[1].lower()}_wrap", self.expr(src, env, d, probes), [])
            src = ("int", r.choice(INTS))
            return ("method", t, int_conv(src[1], t[1]), self.expr(src, env, d, probes), [])
        if choice == "len":
            src = r.choice([("list", self.data_type(1)), STR])
            inner = ("method", U64, "len" if src[0] == "list" else "count_utf8_bytes", self.expr(src, env, d, probes), [])
            if t == U64:
                return inner
            return ("method", t, int_conv("U64", t[1]), inner, [])
        if choice == "fold":
            et = self.data_type(1)
            acc, x = self.fresh("acc"), self.fresh("x")
            body = ("method", t, "plus_wrap", ("var", t, acc),
                    [self.expr(t, env + [(acc, t), (x, et)], d, probes)])
            return ("fold", t, self.expr(("list", et), env, d, probes), self.expr(t, env, d, probes), acc, x, et, body)
        if choice == "farith":
            return ("binop", t, r.choice(["+", "-", "*"]), self.expr(t, env, d, probes), self.expr(t, env, d, probes))
        if choice == "fconv":
            src = ("int", r.choice(INTS))
            return ("method", t, f"to_{t[1].lower()}", self.expr(src, env, d, probes), [])
        if choice == "darith":
            return ("binop", t, r.choice(["+", "-"]), self.expr(t, env, d, probes), self.lit(t))
        if choice == "dconv":
            src = ("int", r.choice(["U8", "I8", "U16", "I16", "U32", "I32"]))
            return ("method", t, "to_dec", self.expr(src, env, d, probes), [])
        if choice == "cmp":
            if r.random() < 0.5:
                at = ("int", r.choice(INTS))
                op = r.choice(["<", "<=", ">", "==", "!="])
            else:
                at = self.data_type(1)
                if not self.has_eq(at):
                    at = STR
                op = r.choice(["==", "!="])
            return ("binop", t, op, self.expr(at, env, d, probes), self.expr(at, env, d, probes))
        if choice == "logic":
            return ("binop", t, r.choice(["and", "or"]), self.expr(t, env, d, probes), self.expr(t, env, d, probes))
        if choice == "not":
            return ("not", t, self.expr(t, env, d, probes))
        if choice == "empty":
            src = r.choice([("list", self.data_type(1)), STR])
            return ("method", t, "is_empty", self.expr(src, env, d, probes), [])
        if choice == "concat":
            return ("call", t, "Str.concat", [self.expr(t, env, d, probes), self.expr(t, env, d, probes)])
        if choice == "inspect":
            return ("call", t, "Str.inspect", [self.expr(self.data_type(2), env, d, probes)])
        if choice == "to_str":
            src = r.choice([("int", r.choice(INTS)), ("dec",), ("float", r.choice(FLOATS))])
            return ("method", t, "to_str", self.expr(src, env, d, probes), [])
        if choice == "join":
            return ("call", t, "Str.join_with", [self.expr(("list", STR), env, d, probes), self.lit(STR)])
        if choice == "interp":
            return ("interp", t, [self.expr(STR, env, d, probes) for _ in range(r.randint(1, 2))], r.choice(WORDS[:5]))
        if choice == "lcons":
            return ("list", t, [self.expr(t[1], env, d, probes) for _ in range(r.randint(1, 3))])
        if choice == "map":
            at = self.data_type(1)
            return ("method", t, "map", self.expr(("list", at), env, d, probes), [self.expr(("fn", at, t[1]), env, d, probes)])
        if choice == "keep":
            return ("method", t, "keep_if", self.expr(t, env, d, probes), [self.expr(("fn", t[1], BOOL), env, d, probes)])
        if choice == "append":
            return ("method", t, "append", self.expr(t, env, d, probes), [self.expr(t[1], env, d, probes)])
        if choice == "lconcat":
            return ("method", t, "concat", self.expr(t, env, d, probes), [self.expr(t, env, d, probes)])
        if choice == "repeat":
            return ("call", t, "List.repeat", [self.expr(t[1], env, d, probes), ("lit", U64, r.randint(0, 4))])
        if choice == "withcap":
            # an empty list with capacity: compile-time evaluation stores no empty collection
            return ("call", t, "List.with_capacity", [("lit", U64, r.randint(0, 9))])
        if choice == "take":
            return ("method", t, r.choice(["take_first", "drop_first"]), self.expr(t, env, d, probes),
                    [("lit", U64, r.randint(0, 3))])
        if choice == "ok":
            return ("tag", t, "Ok", [self.expr(t[1], env, d, probes)])
        if choice == "err":
            tag, pl = r.choice(t[2][1])
            return ("tag", t, "Err", [("tag", t[2], tag, [self.expr(x, env, d, probes) for x in pl])])
        if choice == "lget":
            if t[2] == ERR_OOB:
                return ("method", t, "get", self.expr(("list", t[1]), env, d, probes), [("lit", U64, r.randint(0, 3))])
            if t[2] == ERR_RANGE and t[1][0] == "int":
                src = ("int", r.choice([w for w in INTS if w != t[1][1]]))
                return ("method", t, f"to_{t[1][1].lower()}_try", self.expr(src, env, d, probes), [])
            if t[2] == ERR_EMPTY:
                return ("method", t, r.choice(["first", "last"]), self.expr(("list", t[1]), env, d, probes), [])
            return ("tag", t, "Ok", [self.expr(t[1], env, d, probes)])
        if choice == "lam":
            x = self.fresh("x")
            return ("lam", t, x, self.expr(t[2], env + [(x, t[1])], d, probes))
        if choice == "mk":
            return self.call_mk(t, env, d, probes)
        raise ValueError(choice)

    def cons(self, t, env, d, probes):
        r = self.r
        k = t[0]
        if k == "unit":
            return ("unit", t)
        if k == "rec":
            return ("rec", t, [self.expr(ft, env, d, probes) for _, ft in t[1]])
        if k == "tup":
            return ("tup", t, [self.expr(x, env, d, probes) for x in t[1]])
        if k == "tags":
            tag, pl = r.choice(t[1])
            return ("tag", t, tag, [self.expr(x, env, d, probes) for x in pl])
        if k == "nom":
            if self.recursive(t[1]):
                et = self.p.noms[t[1]][1][0][1][0]
                tail = self.expr(t, env, d - 1, probes) if d > 0 and r.random() < 0.5 else None
                return self.chain(t, [self.expr(et, env, d, probes) for _ in range(r.randint(0, 2))], tail)
            return ("nomwrap", t, self.cons(self.p.noms[t[1]], env, d, probes))
        raise ValueError(t)

    def match(self, t, st, env, d, probes):
        """match on a value of tag type st (structural, Try or nominal), each branch producing t."""
        scrut = self.expr(st, env, d, probes)
        if self.r.random() < 0.5:
            # bind the scrutinee under its annotation, so its type keeps every tag
            v = self.fresh("s")
            scrut = ("let", st, v, scrut, ("var", st, v))
        if st[0] == "nom":
            back = self.p.noms[st[1]]
            if back[0] == "rec":
                binds = [(f, self.fresh("m"), ft) for f, ft in back[1]]
                body = self.expr(t, env + [(v, ft) for _, v, ft in binds], d, probes)
                return ("matchrec", t, scrut, st[1], binds, body)
            cases = back[1]
        elif st[0] == "try":
            cases = (("Ok", (st[1],)), ("Err", (st[2],)))
        else:
            cases = st[1]
        branches = []
        for tag, pl in cases:
            vs = [self.fresh("m") for _ in pl]
            branches.append((tag, vs, self.expr(t, env + list(zip(vs, pl)), d, probes)))
        return ("match", t, scrut, branches)

    def field(self, t, env, d, probes):
        r = self.r
        if r.random() < 0.5:
            others = [(f, self.data_type(0)) for f in r.sample(FIELDS, r.randint(0, 2))]
            fname = r.choice(FIELDS)
            fs = dict(others)
            fs[fname] = t
            rt = ("rec", tuple(sorted(fs.items())))
            if AVOID_KNOWN and all(zst_struct(x) for x in fs.values()):
                return self.lit(t)
            return ("field", t, self.expr(rt, env, d, probes), fname)
        n = r.randint(2, 3)
        i = r.randrange(n)
        items = tuple(t if j == i else self.data_type(0) for j in range(n))
        if AVOID_KNOWN and all(zst_struct(x) for x in items):
            return self.lit(t)
        return ("tupidx", t, self.expr(("tup", items), env, d, probes), i)

    def call_plain(self, t, env, d, probes):
        r = self.r
        cands = [h for h in self.p.helpers if h.kind == "plain" and h.ret == t and h.body is not None]
        if cands and r.random() < 0.5:
            h = r.choice(cands)
        else:
            params = [(self.fresh("p"), self.data_type(1)) for _ in range(r.randint(1, 3))]
            h = Helper(self.fresh("h"), "plain", params, t)
            self.p.helpers.append(h)
            h.body = self.expr(t, list(params), max(d, 1), [])
        return ("hcall", t, h.name, [self.expr(pt, env, d, probes) for _, pt in h.params])

    def call_mk(self, t, env, d, probes):
        """A helper returning a closure that captures its parameters."""
        params = [(self.fresh("p"), self.data_type(1)) for _ in range(self.r.randint(1, 2))]
        h = Helper(self.fresh("mk"), "mk", params, t)
        self.p.helpers.append(h)
        x = self.fresh("x")
        h.body = ("lam", t, x, self.expr(t[2], list(params) + [(x, t[1])], max(d, 1), []))
        return ("hcall", t, h.name, [self.expr(pt, env, d, probes) for _, pt in h.params])

    def rec_call(self, t, env, d, probes):
        """A self-recursive helper over a U64 count: tail (`if n == 0 acc else f(n - 1, step)`) or not."""
        r = self.r
        kind = r.choice(["tail", "nontail"])
        h = Helper(self.fresh("rec"), kind, [("n", U64), ("acc", t)], t)
        self.p.helpers.append(h)
        # a long recursion builds a large stored value; its step then reads the accumulator once, so it stays linear
        big = r.random() < 0.15
        benv = [("n", U64)] if big else [("n", U64), ("acc", t)]
        if kind == "tail":
            h.body = ("if", t, ("binop", BOOL, "==", ("var", U64, "n"), ("lit", U64, 0)), ("var", t, "acc"),
                      ("hcall", t, h.name, [("binop", U64, "-", ("var", U64, "n"), ("lit", U64, 1)),
                                            self.step(t, benv)]))
        else:
            rv = self.fresh("rv")
            rec = ("hcall", t, h.name, [("binop", U64, "-", ("var", U64, "n"), ("lit", U64, 1)), ("var", t, "acc")])
            h.body = ("if", t, ("binop", BOOL, "==", ("var", U64, "n"), ("lit", U64, 0)), ("var", t, "acc"),
                      ("let", t, rv, rec, self.step(t, benv if big else benv + [(rv, t)], prefer=rv)))
        count = r.randint(20, 300) if big else r.randint(0, 4)
        return ("hcall", t, h.name, [("lit", U64, count), self.expr(t, env, d, probes)])

    def step(self, t, env, prefer="acc"):
        """One step of a recursion: a small expression of t that uses the accumulator, so values stay small."""
        r = self.r
        a = ("var", t, prefer)
        k = t[0]
        if k == "int":
            return ("method", t, r.choice(["plus_wrap", "times_wrap", "bitwise_xor"]), a,
                    [self.expr(t, env, 1, [])])
        if k == "str":
            return ("call", t, "Str.concat", [a, self.expr(t, env, 1, [])] if r.random() < 0.5
                    else [self.expr(t, env, 1, []), a])
        if k == "list":
            return ("method", t, "append", a, [self.expr(t[1], env, 1, [])])
        if k == "nom":
            et = self.p.noms[t[1]][1][0][1][0]
            return self.chain(t, [self.expr(et, env, 1, [])], a)
        raise ValueError(t)

    # ----------------------------------------------------------------------------------------- programs

    def probe_type(self):
        r = self.r
        k = r.random()
        if k < 0.15:
            return ("fn", self.data_type(1), self.data_type(1))
        if k < 0.25:
            # a record holding a function: a compile-time root that is not a data constant
            return ("rec", (("f", ("fn", self.data_type(1), self.data_type(1))), ("k", self.data_type(1))))
        if k < 0.37:
            return self.fn_holding_type()
        return self.data_type(2)

    def fn_holding_type(self):
        """A data shape with a function inside: a list, tag payload, Try, tuple, Box or curried result."""
        r = self.r
        f = ("fn", self.data_type(1), self.data_type(1))
        k = r.randrange(6)
        if k == 0:
            return ("list", f)
        if k == 1:
            return ("tags", (("Alpha", (f,)), ("Beta", (self.data_type(1),)), ("Gamma", ())))
        if k == 2:
            return ("try", f, self.tags_type(0))
        if k == 3:
            return ("tup", (self.data_type(1), f))
        if k == 4:
            return ("box", f)
        return ("fn", self.data_type(1), f)

    def holds_fn(self, t):
        k = t[0]
        if k == "fn":
            return True
        if k == "rec":
            return any(self.holds_fn(x) for _, x in t[1])
        if k == "tup":
            return any(self.holds_fn(x) for x in t[1])
        if k == "tags":
            return any(self.holds_fn(x) for _, pl in t[1] for x in pl)
        if k in ("list", "box"):
            return self.holds_fn(t[1])
        if k == "try":
            return self.holds_fn(t[1]) or self.holds_fn(t[2])
        return False

    def observe(self, t, e):
        """A Str expression that shows value e of type t, applying every function it holds to a literal."""
        insp = lambda x: ("call", STR, "Str.inspect", [x])
        cat = lambda a, b: ("call", STR, "Str.concat", [a, b])
        lit = lambda s: ("lit", STR, s)
        if not self.holds_fn(t):
            return insp(e)
        k = t[0]
        if k == "fn":
            return self.observe(t[2], ("app", t[2], e, self.lit(t[1])))
        if k == "rec":
            out = lit("{")
            for f, ft in t[1]:
                out = cat(cat(out, lit(f + "=")), self.observe(ft, ("field", ft, e, f)))
            return cat(out, lit("}"))
        if k == "tup":
            out = lit("(")
            for i, it in enumerate(t[1]):
                out = cat(cat(out, lit(",")), self.observe(it, ("tupidx", it, e, i)))
            return cat(out, lit(")"))
        if k == "list":
            x = self.fresh("o")
            return ("call", STR, "Str.join_with", [("method", ("list", STR), "map", e,
                                                    [("lam", ("fn", t[1], STR), x, self.observe(t[1], ("var", t[1], x)))]),
                                                   lit(";")])
        if k == "box":
            return self.observe(t[1], ("call", t[1], "Box.unbox", [e]))
        cases = t[1] if k == "tags" else (("Ok", (t[1],)), ("Err", (t[2],)))
        branches = []
        for tag, pl in cases:
            vs = [self.fresh("o") for _ in pl]
            body = lit(tag)
            for v, pt in zip(vs, pl):
                body = cat(body, self.observe(pt, ("var", pt, v)))
            branches.append((tag, vs, body))
        return ("match", STR, e, branches)

    def program(self):
        r = self.r
        self.p.split = r.random() < 0.35
        for i in range(self.nprobes):
            t = self.probe_type()
            e = self.expr(t, [], r.randint(2, 4), list(range(i)))
            ft = fn_of(t)
            args = [self.lit(ft[1]) for _ in range(2)] if ft else []
            pr = Probe(i, t, e, args, "guard" if r.random() < 0.25 else "taint")
            if self.holds_fn(t) and not (ft and not self.holds_fn(ft[2])):
                pr.observe = self.observe(t, ("var", t, "v"))
            self.p.probes.append(pr)
        return self.p


def generate(seed, nprobes=None):
    g = Gen(seed, nprobes)
    for _ in range(50):
        try:
            return g.program()
        except RecursionError:
            g = Gen(g.r.random(), nprobes)
    raise RuntimeError("generator kept recursing")


# --------------------------------------------------------------------------------------------- emitter

def ty(t, q=""):
    """The Roc spelling of type t; q qualifies nominal names (`Lib.`) in a program split across two modules."""
    k = t[0]
    if k in ("int", "float"):
        return t[1]
    if k == "dec":
        return "Dec"
    if k == "bool":
        return "Bool"
    if k == "str":
        return "Str"
    if k == "unit":
        return "{}"
    if k == "rec":
        return "{ " + ", ".join(f"{f} : {ty(x, q)}" for f, x in t[1]) + " }"
    if k == "tup":
        return "(" + ", ".join(ty(x, q) for x in t[1]) + ")"
    if k == "tags":
        return "[" + ", ".join(tag + (f"({', '.join(ty(x, q) for x in pl)})" if pl else "") for tag, pl in t[1]) + "]"
    if k == "list":
        return f"List({ty(t[1], q)})"
    if k == "try":
        return f"Try({ty(t[1], q)}, {ty(t[2], q)})"
    if k == "nom":
        return q + t[1]
    if k == "fn":
        return f"({ty(t[1], q)} -> {ty(t[2], q)})"
    if k == "box":
        return f"Box({ty(t[1], q)})"
    raise ValueError(t)


def roc_str(s):
    out = []
    for ch in s:
        if ch in '"\\$':
            out.append("\\" + ch)
        else:
            out.append(ch)
    return '"' + "".join(out) + '"'


def uses(name, node):
    """True if node reads variable name."""
    if isinstance(node, tuple):
        if len(node) >= 3 and node[0] == "var" and node[2] == name:
            return True
        return any(uses(name, c) for c in node[1:] if isinstance(c, (tuple, list)))
    if isinstance(node, list):
        return any(uses(name, c) for c in node)
    return False


def bind(name, body):
    return name if uses(name, body) else "_" + name


class Emit:
    """Emits a program as Roc. side "c" reads probes as constants, side "r" calls their runtime functions."""

    def __init__(self, prog):
        self.p = prog
        self.q = "Lib." if getattr(prog, "split", False) else ""

    def ty(self, t):
        return ty(t, self.q)

    def e(self, x, side, ind):
        k = x[0]
        t = x[1]
        nl = "\n" + "\t" * (ind + 1)
        end = "\n" + "\t" * ind
        if side == "r" and self.leaf(x):
            # every leaf of the runtime side reads the argument, so no part of it is a compile-time root
            return f"rt(z, {self.e(x, 'c', ind)})"
        if k == "lit":
            v = x[2]
            if t[0] == "int":
                return self.neg(v, t[1])
            if t[0] in ("float", "dec"):
                return f"{v}.{self.ty(t)}"
            if t[0] == "bool":
                return "Bool.True" if v else "Bool.False"
            if t[0] == "str":
                return roc_str(v)
        if k == "unit":
            return "{}"
        if k == "var":
            return x[2]
        if k == "probe":
            return f"{self.q}c{x[2]}" if side == "c" else f"r{x[2]}(z)"  # side "g" reads probes like "r"
        if k == "rec":
            return "{ " + ", ".join(f"{f}: {self.e(v, side, ind)}" for (f, _), v in zip(t[1], x[2])) + " }"
        if k == "tup":
            return "(" + ", ".join(self.e(v, side, ind) for v in x[2]) + ")"
        if k == "tag":
            tag = x[2]
            if not x[3]:
                return tag
            return f"{tag}(" + ", ".join(self.e(v, side, ind) for v in x[3]) + ")"
        if k == "nomwrap":
            return f"{self.q}{t[1]}." + self.e(x[2], side, ind)
        if k == "list":
            return "[" + ", ".join(self.e(v, side, ind) for v in x[2]) + "]"
        if k == "let":
            v, rhs, body = x[2], x[3], x[4]
            v = bind(v, body)
            ann = f"{v} : {self.ty(rhs[1])}{nl}"
            return f"{{{nl}{ann}{v} = {self.e(rhs, side, ind + 1)}{nl}{self.e(body, side, ind + 1)}{end}}}"
        if k == "if":
            return (f"if {self.e(x[2], side, ind)} {{{nl}{self.e(x[3], side, ind + 1)}{end}}} else {{{nl}"
                    f"{self.e(x[4], side, ind + 1)}{end}}}")
        if k == "match":
            nl2 = "\n" + "\t" * (ind + 2)
            arms = []
            for tag, vs, body in x[3]:
                pat = tag + (f"({', '.join(bind(v, body) for v in vs)})" if vs else "")
                arms.append(f"{pat} => {{{nl2}{self.e(body, side, ind + 2)}{nl}}}")
            return f"match {self.e(x[2], side, ind)} {{{nl}" + nl.join(arms) + f"{end}}}"
        if k == "matchrec":
            nl2 = "\n" + "\t" * (ind + 2)
            pat = f"{self.q}{x[3]}.{{ " + ", ".join(f"{f}: {bind(v, x[5])}" for f, v, _ in x[4]) + " }"
            return f"match {self.e(x[2], side, ind)} {{{nl}{pat} => {{{nl2}{self.e(x[5], side, ind + 2)}{nl}}}{end}}}"
        if k == "field":
            return f"{self.atom(x[2], side, ind)}.{x[3]}"
        if k == "tupidx":
            return f"{self.atom(x[2], side, ind)}.{x[3]}"
        if k == "hcall":
            if side == "r":
                return f"{x[2]}_r(z, " + ", ".join(self.e(a, side, ind) for a in x[3]) + ")"
            return f"{self.q}{x[2]}(" + ", ".join(self.e(a, side, ind) for a in x[3]) + ")"
        if k == "call":
            return f"{x[2]}(" + ", ".join(self.e(a, side, ind) for a in x[3]) + ")"
        if k == "method":
            return f"{self.atom(x[3], side, ind)}.{x[2]}(" + ", ".join(self.e(a, side, ind) for a in x[4]) + ")"
        if k == "app":
            return f"{self.atom(x[2], side, ind)}({self.e(x[3], side, ind)})"
        if k == "binop":
            return f"({self.opnd(x[3], side, ind)} {x[2]} {self.opnd(x[4], side, ind)})"
        if k == "not":
            return f"!{self.atom(x[2], side, ind)}"
        if k == "fold":
            _, _, lst, init, acc, xv, et, body = x
            return (f"{self.atom(lst, side, ind)}.fold({self.e(init, side, ind)}, |{bind(acc, body)}, {bind(xv, body)}| {{{nl}"
                    f"{self.e(body, side, ind + 1)}{end}}})")
        if k == "interp":
            parts, sep = x[2], x[3]
            sepq = roc_str(sep)[1:-1]
            return '"' + sepq + sepq.join("${" + self.e(p, side, ind) + "}" for p in parts) + '"'
        if k == "lam":
            return f"|{bind(x[2], x[3])}| {{{nl}{self.e(x[3], side, ind + 1)}{end}}}"
        raise ValueError(k)

    def leaf(self, x):
        """A part with no parts of its own: a literal, `{}`, a tag without payload, an empty list, or a nominal
        around a tag without payload."""
        k = x[0]
        if k in ("lit", "unit"):
            return True
        if k == "tag":
            return not x[3]
        if k == "list":
            return not x[2]
        if k == "nomwrap":
            return x[2][0] == "tag" and not x[2][3]
        return False

    def neg(self, v, w):
        return f"{v}.{w}" if v >= 0 else f"-{-v}.{w}"

    def opnd(self, x, side, ind):
        """An operand of a binary operator: an `if`, `match`, block or lambda goes in parentheses."""
        s = self.e(x, side, ind)
        return f"({s})" if x[0] in ("if", "match", "matchrec", "let", "lam") else s

    def atom(self, x, side, ind):
        s = self.e(x, side, ind)
        if x[0] in ("var", "probe", "rec", "tup", "list", "hcall", "call", "method", "unit"):
            return s
        return f"({s})"

    def helper(self, h):
        """The helper for the constant side, and `<name>_r` for the runtime side, which takes the argument z first."""
        params = ", ".join(bind(n, h.body) for n, _ in h.params)
        sig = ", ".join(self.ty(pt) for _, pt in h.params)
        c = f"{h.name} : {sig} -> {self.ty(h.ret)}\n{h.name} = |{params}| {self.e(h.body, 'c', 0)}\n"
        r = f"{h.name}_r : U64, {sig} -> {self.ty(h.ret)}\n{h.name}_r = |z, {params}| {self.e(h.body, 'r', 0)}\n"
        return c, r

    def const_def(self, pr):
        i = pr.idx
        return f"c{i} : {self.ty(pr.ty)}\nc{i} = {self.e(pr.expr, 'c', 0)}\n"

    def runtime_def(self, pr):
        i = pr.idx
        t = self.ty(pr.ty)
        if pr.mode == "guard":
            # the expression in a branch on the argument: a guarded compile-time root, restored at runtime
            body = self.e(pr.expr, "g", 1)
            return f"r{i} : U64 -> {t}\nr{i} = |z| if z != 0 {{\n\tcrash \"rt\"\n}} else {{\n\t{body}\n}}\n"
        return f"r{i} : U64 -> {t}\nr{i} = |z| {self.e(pr.expr, 'r', 0)}\n"

    def output_lines(self, pr):
        """(label, roc expression of type Str) per line main! prints for this probe."""
        i = pr.idx
        c = f"{self.q}c{i}"
        if getattr(pr, "observe", None) is not None:
            return [(f"p{i} ct", f"ob_ct{i}({c})"), (f"p{i} rt-const", f"ob{i}(z, {c})"),
                    (f"p{i} rt", f"ob{i}(z, r{i}(z))")]
        if pr.ty[0] == "fn":
            out = []
            for j, a in enumerate(pr.args):
                arg = self.e(a, "c", 1)
                out.append((f"p{i}.{j} ct", f"Str.inspect({c}({arg}))"))
                out.append((f"p{i}.{j} rt-const", f"apply_rt(z, {c}, {arg})"))
                out.append((f"p{i}.{j} rt", f"apply_rt(z, r{i}(z), {arg})"))
            return out
        if fn_of(pr.ty):
            out = [(f"p{i}.k ct", f"Str.inspect({c}.k)"), (f"p{i}.k rt-const", f"show_k_rt(z, {c})"),
                   (f"p{i}.k rt", f"show_k_rt(z, r{i}(z))")]
            for j, a in enumerate(pr.args):
                arg = self.e(a, "c", 1)
                out.append((f"p{i}.{j} ct", f"Str.inspect(({c}.f)({arg}))"))
                out.append((f"p{i}.{j} rt-const", f"apply_f_rt(z, {c}, {arg})"))
                out.append((f"p{i}.{j} rt", f"apply_f_rt(z, r{i}(z), {arg})"))
            return out
        out = [(f"p{i} ct", f"Str.inspect({c})"), (f"p{i} rt-const", f"show_rt(z, {c})"),
               (f"p{i} rt", f"show_rt(z, r{i}(z))")]
        if pr.ty[0] == "list":
            # update the list at runtime: a stored constant is never unique, so the update copies it first
            out += [(f"p{i}.g rt-const", f"grow_rt(z, {c})"), (f"p{i}.g rt", f"grow_rt(z, r{i}(z))")]
        return out

    def has_eq(self, t):
        g = Gen(0)
        g.p = self.p
        return g.has_eq(t)

    def eq_line(self, pr):
        i = pr.idx
        if self.has_eq(pr.ty):
            return (f"p{i} eq", f"eq_rt(z, {self.q}c{i}, r{i}(z))")
        return None

    def files(self, with_expects=True):
        """{file name: source}. The constants, their helpers and the nominal types go in a type module `Lib`
        when the program is split, else everything is in the app."""
        p = self.p
        lib = [f"{name} := {self.ty(p.noms[name])}\n" for name in sorted(p.noms)]
        app = []
        app.append("rt = |z, v| if z != 0 {\n\tcrash \"rt\"\n} else {\n\tv\n}\n")
        app.append("show_rt = |z, v| if z != 0 {\n\tcrash \"rt\"\n} else {\n\tStr.inspect(v)\n}\n")
        app.append("grow_rt = |z, v| if z != 0 {\n\tcrash \"rt\"\n} else {\n\tStr.inspect(v.concat(v).drop_first(1))\n}\n")
        app.append("apply_rt = |z, f, a| if z != 0 {\n\tcrash \"rt\"\n} else {\n\tStr.inspect(f(a))\n}\n")
        app.append("apply_f_rt = |z, v, a| if z != 0 {\n\tcrash \"rt\"\n} else {\n\tStr.inspect((v.f)(a))\n}\n")
        app.append("show_k_rt = |z, v| if z != 0 {\n\tcrash \"rt\"\n} else {\n\tStr.inspect(v.k)\n}\n")
        app.append("eq_rt = |z, a, b| if z != 0 {\n\tcrash \"rt\"\n} else if a == b {\n\t\"eq\"\n} else {\n\t\"NE\"\n}\n")
        for h in p.helpers:
            c, r = self.helper(h)
            lib.append(c)
            app.append(r)
        lines = []
        for pr in p.probes:
            lib.append(self.const_def(pr))
            app.append(self.runtime_def(pr))
            if getattr(pr, "observe", None) is not None:
                t = self.ty(pr.ty)
                body = self.e(pr.observe, "c", 1)
                app.append(f"ob_ct{pr.idx} : {t} -> Str\nob_ct{pr.idx} = |v| {body}\n")
                app.append(f"ob{pr.idx} : U64, {t} -> Str\nob{pr.idx} = |z, v| if z != 0 {{\n\tcrash \"rt\"\n}} else {{\n\t{body}\n}}\n")
            lines += self.output_lines(pr)
            el = self.eq_line(pr)
            if el:
                lines.append(el)
            if with_expects and self.has_eq(pr.ty):
                app.append(f"expect {self.q}c{pr.idx} == r{pr.idx}(0)\n")
        body = "\n".join(f"\techo!(Str.concat(Str.concat({roc_str(label + ': ')}, {expr}), \"\\n\"))"
                         for label, expr in lines)
        app.append(f"main! = |args| {{\n\tz = args.len() * 0\n{body}\n\tOk({{}})\n}}\n")
        if not self.q:
            return {"app.roc": "\n".join(lib + app)}
        inner = "\n".join(lib)
        inner = "\n".join(("\t" + l) if l else l for l in inner.split("\n"))
        return {"Lib.roc": "Lib :: [].{\n" + inner.rstrip() + "\n}\n", "app.roc": "import Lib\n\n" + "\n".join(app)}

    def source(self):
        return "\n".join(f"# ---- {k}\n{v}" for k, v in sorted(self.files().items()))

    def labels(self):
        out = []
        for pr in self.p.probes:
            out += [label for label, _ in self.output_lines(pr)]
            el = self.eq_line(pr)
            if el:
                out.append(el[0])
        return out
