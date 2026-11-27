"""Generator, reference encoder and structural equality for the codec-roundtrip model. See README.md.

A program is a list of cases over a shared set of nominal declarations. Each case is a random type built from small
typed constructs, a value `v` of it, and a second value `w` that is either a copy of `v` or a nearby value. For every
case the model computes:

  - the JSON text the builtin Json codec's documented format gives `v` (`encode`),
  - whether `v == w` under structural equality (`equal`).

The emitter writes the same cases as Roc, either as a type module of `expect`s (for `roc test`) or as an app whose
`main!` prints one line per case (for built and interpreted programs on every backend).

Types are tuples:
  ("str",) ("bool",) ("int", "U8"|...|"I128") ("dec",) ("unit",)            unit is the empty record `{}`
  ("list", T) ("set", K) ("dict", K, T) ("tuple", [T..]) ("record", [(name, T, optional)..])
  ("tags", [(Tag, [T..])..])            a structural tag union, tags sorted by name
  ("try", ok, err)                      builtin Try; err is a ("tags", ..) type
  ("nom", name, [args])                 a nominal declared in Prog.noms
  ("param",)                            the type parameter inside a parametric nominal's backing
  ("self",)                             the recursive occurrence inside a recursive nominal's backing

Values are:
  str, bool, int (Dec as its atto count), ("unit",), list, ("set", [..]), ("dict", [(k, v)..]), tuple,
  ("rec", {name: value}), ("tag", Tag, [payloads..]); nominals are their backing value (custom codecs included).
  An optional record field holds a ("tag", "Ok", [x]) or ("tag", "Err", [("tag", "Missing", [])]).
"""
import random

INTS = {
    "U8": (0, 2**8 - 1), "I8": (-2**7, 2**7 - 1), "U16": (0, 2**16 - 1), "I16": (-2**15, 2**15 - 1),
    "U32": (0, 2**32 - 1), "I32": (-2**31, 2**31 - 1), "U64": (0, 2**64 - 1), "I64": (-2**63, 2**63 - 1),
    "U128": (0, 2**128 - 1), "I128": (-2**127, 2**127 - 1),
}
DEC_RANGE = (-2**127, 2**127 - 1)  # atto counts
DEC_ONE = 10**18

FIELD_NAMES = ["a", "b", "count", "id", "name", "x", "y", "zeta", "item_list", "v2"]
TAG_NAMES = ["A", "B", "Cat", "Dog", "Leaf", "Node", "Pair", "Q", "Red", "Zed"]

# Strings: code points chosen to stress JSON escaping and UTF-8.
AWKWARD = ['"', "\\", "\n", "\r", "\t", "\b", "\f", "\x00", "\x01", "\x1f", "\x7f", "/", "$", "{", "}", "[",
           "]", ":", ",", " ", "é", "́", " ", " ", "\U0001F600", "﻿", "中", "'", "u", "0"]
PLAIN = "abcdefghijklmnopqrstuvwxyzABCXYZ0123456789 _-"


# --------------------------------------------------------------------------------------------- nominals

class Nom:
    """A nominal declaration.

    kind: "derived" (all four methods derived), "custom" (backing { v : scalar }, Json codec hand-written as the
    bare scalar, is_eq/to_hash derived). opaque: declared with `::` instead of `:=`. param: has one type parameter
    `a`, used as ("param",) in the backing. recursive: the backing mentions ("self",).
    """

    def __init__(self, name, kind, backing, opaque=False, param=False, recursive=False):
        self.name = name
        self.kind = kind
        self.backing = backing
        self.opaque = opaque
        self.param = param
        self.recursive = recursive


class Case:
    def __init__(self, ty, v, w):
        self.ty = ty
        self.v = v
        self.w = w


class Prog:
    def __init__(self):
        self.noms = []
        self.cases = []

    def nom(self, name):
        for n in self.noms:
            if n.name == name:
                return n
        raise KeyError(name)


# --------------------------------------------------------------------------------------------- type helpers

def subst(t, arg, self_ty):
    """The backing of a nominal application: ("param",) becomes arg, ("self",) becomes self_ty."""
    k = t[0]
    if k == "param":
        return arg
    if k == "self":
        return self_ty
    if k in ("list", "set"):
        return (k, subst(t[1], arg, self_ty))
    if k == "dict":
        return (k, subst(t[1], arg, self_ty), subst(t[2], arg, self_ty))
    if k == "tuple":
        return (k, [subst(x, arg, self_ty) for x in t[1]])
    if k == "record":
        return (k, [(n, subst(x, arg, self_ty), o) for n, x, o in t[1]])
    if k == "tags":
        return (k, [(n, [subst(x, arg, self_ty) for x in ps]) for n, ps in t[1]])
    if k == "try":
        return (k, subst(t[1], arg, self_ty), subst(t[2], arg, self_ty))
    return t


def backing_of(prog, t):
    n = prog.nom(t[1])
    return subst(n.backing, t[2][0] if t[2] else None, t)


def is_null_try(t):
    return t[0] == "try" and t[2] == ("tags", [("Null", [])])


def is_missing_try(t):
    return t[0] == "try" and t[2] == ("tags", [("Missing", [])])


def contains(prog, t, pred, seen=None):
    """Whether pred holds for t or any type inside it (through nominal backings, once each)."""
    seen = set() if seen is None else seen
    if pred(t):
        return True
    k = t[0]
    if k == "nom":
        key = render_type(t)
        if key in seen:
            return False
        seen.add(key)
        return contains(prog, backing_of(prog, t), pred, seen)
    return any(contains(prog, c, pred, seen) for c in child_types(t))


def child_types(t):
    k = t[0]
    if k in ("list", "set"):
        return [t[1]]
    if k == "dict":
        return [t[1], t[2]]
    if k == "tuple":
        return list(t[1])
    if k == "record":
        return [x for _, x, _ in t[1]]
    if k == "tags":
        return [x for _, ps in t[1] for x in ps]
    if k == "try":
        return [t[1], t[2]]
    if k == "nom":
        return list(t[2])
    return []


def hashable(prog, t):
    """Set elements need to_hash. Every type the generator writes has one (Dict and Set hash independently of
    order), so every case gets the hashing judgment; this is the one place to narrow it."""
    return True


# --------------------------------------------------------------------------------------------- type generation

class Gen:
    def __init__(self, rnd):
        self.r = rnd
        self.prog = Prog()
        self.next_nom = 0

    def scalar(self):
        r = self.r.random()
        if r < 0.25:
            return ("str",)
        if r < 0.32:
            return ("bool",)
        if r < 0.40:
            return ("dec",)
        return ("int", self.r.choice(list(INTS)))

    def key(self):
        return self.r.choice([("str",), ("int", "U8"), ("int", "I64"), ("int", "U64")])

    def zst(self):
        """A zero-sized type."""
        return self.r.choice([("unit",), ("tags", [(self.r.choice(TAG_NAMES), [])])])

    def tags(self, d, allow_self=False, allow_param=False):
        names = sorted(self.r.sample(TAG_NAMES, self.r.randint(1, 4)))
        out = []
        for n in names:
            np = self.r.choice([0, 0, 1, 1, 2, 3])
            out.append((n, [self.ty(d - 1, allow_self, allow_param) for _ in range(np)]))
        return ("tags", out)

    def record(self, d, allow_self=False, allow_param=False):
        names = sorted(self.r.sample(FIELD_NAMES, self.r.randint(1, 4)))
        fields = []
        for n in names:
            t = self.ty(d - 1, allow_self, allow_param)
            optional = self.r.random() < 0.2
            fields.append((n, t, optional))
        return ("record", fields)

    def ty(self, d, allow_self=False, allow_param=False):
        r = self.r.random()
        if allow_param and r < 0.15:
            return ("param",)
        if allow_self and r < 0.25:
            return ("self",)
        if d <= 0 or r < 0.3:
            return self.scalar() if self.r.random() < 0.9 else self.zst()
        c = self.r.choice(["list", "tuple", "record", "record", "tags", "tags", "try", "null", "nom", "nom", "zst",
                           "set", "dict"])
        if c == "list":
            return ("list", self.ty(d - 1, allow_self, allow_param))
        if c == "set":
            return ("set", self.key())
        if c == "dict":
            return ("dict", self.key(), self.ty(d - 1, allow_self, allow_param))
        if c == "tuple":
            return ("tuple", [self.ty(d - 1, allow_self, allow_param) for _ in range(self.r.randint(2, 3))])
        if c == "record":
            return self.record(d, allow_self, allow_param)
        if c == "tags":
            return self.tags(d, allow_self, allow_param)
        if c == "try":
            # `Try(ok, [Missing])` is the optional-field form; anywhere else the codec rejects it at compile
            # time, so the generator writes it only as an optional record field.
            return ("try", self.ty(d - 1, allow_self, allow_param), self.tags(1))
        if c == "null":
            ok = self.ty(d - 1, allow_self, allow_param)
            if ok[0] in ("try", "param", "self", "nom"):
                # A nullable whose Ok is itself nullable cannot round-trip (both encode Err(Null) as null).
                ok = self.scalar()
            return ("try", ok, ("tags", [("Null", [])]))
        if c == "zst":
            return self.zst()
        if c == "nom":
            if allow_self or allow_param:
                return self.scalar()
            return self.new_nom(d)
        raise AssertionError(c)

    def new_nom(self, d):
        name = f"N{self.next_nom}"
        self.next_nom += 1
        r = self.r.random()
        if r < 0.15:
            nom = Nom(name, "custom", ("record", [("v", self.r.choice([("str",), ("int", "U64"), ("int", "I32"),
                                                                     ("bool",)]), False)]))
        elif r < 0.4:
            # Recursive: a tag union with a base tag and a tag (or a list) holding the nominal itself.
            base = (self.r.choice(["Leaf", "Nil"]), self.r.choice([[], [self.scalar()]]))
            rec_payload = self.r.choice([[("self",)], [("self",), self.scalar()], [("list", ("self",))],
                                         [self.scalar(), ("self",), ("self",)]])
            tags = sorted([base, ("Node", rec_payload)])
            nom = Nom(name, "derived", ("tags", tags), recursive=True)
        elif r < 0.55:
            # One type parameter, used in a tag payload or a record field.
            if self.r.random() < 0.5:
                backing = ("tags", sorted([("Has", [("param",)]), ("Nope", [])]))
            else:
                backing = ("record", [("inner", ("param",), False), ("tag", self.scalar(), False)])
            nom = Nom(name, "derived", backing, param=True)
        else:
            backing = self.r.choice([self.record(d), self.tags(d), ("tuple", [self.scalar(), self.ty(d - 1)]),
                                     self.scalar()])
            nom = Nom(name, "derived", backing)
        nom.opaque = self.r.random() < 0.3
        self.prog.noms.append(nom)
        args = [self.ty(max(d - 2, 0))] if nom.param else []
        if nom.param:
            # Keep argument types free of nominals declared later, so declarations print in order.
            args = [self.scalar()] if self.r.random() < 0.5 else args
        return ("nom", name, args)

    # ----------------------------------------------------------------------------------------- values

    def string(self):
        n = self.r.choice([0, 1, 2, 3, 5, 8, 16, 30])
        out = []
        for _ in range(n):
            out.append(self.r.choice(AWKWARD) if self.r.random() < 0.4 else self.r.choice(PLAIN))
        return "".join(out)

    def int_val(self, name):
        lo, hi = INTS[name]
        r = self.r.random()
        if r < 0.15:
            return lo
        if r < 0.3:
            return hi
        if r < 0.4:
            return 0
        if r < 0.7:
            return self.r.randint(max(lo, -100), min(hi, 100))
        return self.r.randint(lo, hi)

    def dec_val(self):
        lo, hi = DEC_RANGE
        r = self.r.random()
        if r < 0.1:
            return lo
        if r < 0.2:
            return hi
        if r < 0.3:
            return 0
        if r < 0.6:
            return self.r.randint(-1000, 1000) * DEC_ONE // self.r.choice([1, 2, 4, 8, 10, 100])
        if r < 0.8:
            return self.r.randint(-10**6, 10**6)
        return self.r.randint(lo, hi)

    def value(self, t, depth=4):
        k = t[0]
        if k == "str":
            return self.string()
        if k == "bool":
            return self.r.random() < 0.5
        if k == "int":
            return self.int_val(t[1])
        if k == "dec":
            return self.dec_val()
        if k == "unit":
            return ("unit",)
        if k == "list":
            n = 0 if depth <= 0 else self.r.choice([0, 1, 2, 3])
            return [self.value(t[1], depth - 1) for _ in range(n)]
        if k == "set":
            items = []
            for _ in range(self.r.choice([0, 1, 2, 3])):
                x = self.value(t[1], depth - 1)
                if x not in items:
                    items.append(x)
            return ("set", items)
        if k == "dict":
            entries = []
            for _ in range(0 if depth <= 0 else self.r.choice([0, 1, 2, 3])):
                key = self.value(t[1], depth - 1)
                if all(key != e[0] for e in entries):
                    entries.append((key, self.value(t[2], depth - 1)))
            return ("dict", entries)
        if k == "tuple":
            return tuple(self.value(x, depth - 1) for x in t[1])
        if k == "record":
            fields = {}
            for n, x, opt in t[1]:
                if opt:
                    fields[n] = (("tag", "Ok", [self.value(x, depth - 1)]) if self.r.random() < 0.6
                                 else ("tag", "Err", [("tag", "Missing", [])]))
                else:
                    fields[n] = self.value(x, depth - 1)
            return ("rec", fields)
        if k == "tags":
            choices = t[1]
            if depth <= 0:
                # Prefer a tag whose payloads cannot recurse.
                flat = [c for c in choices if not any(contains(self.prog, p, lambda y: y[0] == "nom"
                                                               and self.prog.nom(y[1]).recursive) for p in c[1])]
                choices = flat or choices
            n, ps = self.r.choice(choices)
            return ("tag", n, [self.value(p, depth - 1) for p in ps])
        if k == "try":
            if self.r.random() < 0.6:
                return ("tag", "Ok", [self.value(t[1], depth - 1)])
            return ("tag", "Err", [self.value(t[2], depth - 1)])
        if k == "nom":
            return self.value(backing_of(self.prog, t), depth - 1)
        raise AssertionError(t)

    def mutate(self, t, v, depth=4):
        """A value of t that is usually close to v: one leaf changed, or v itself."""
        if self.r.random() < 0.3:
            return self.value(t, depth)
        k = t[0]
        if k == "nom":
            return self.mutate(backing_of(self.prog, t), v, depth)
        if k == "tuple" and v:
            i = self.r.randrange(len(v))
            return tuple(self.mutate(t[1][i], x, depth - 1) if j == i else x for j, x in enumerate(v))
        if k == "record" and v[1]:
            n, x, opt = self.r.choice(t[1])
            fields = dict(v[1])
            if opt:
                fields[n] = self.value(t, depth)[1][n]
            else:
                fields[n] = self.mutate(x, v[1][n], depth - 1)
            return ("rec", fields)
        if k == "list" and v:
            i = self.r.randrange(len(v))
            return [self.mutate(t[1], x, depth - 1) if j == i else x for j, x in enumerate(v)]
        if k == "set" and v[1]:
            # Same elements in another order: still equal.
            items = list(v[1])
            self.r.shuffle(items)
            return ("set", items)
        if k == "dict" and v[1]:
            entries = list(v[1])
            self.r.shuffle(entries)
            return ("dict", entries)
        return self.value(t, depth)


def generate(seed, ncases=5):
    g = Gen(random.Random(seed))
    for _ in range(ncases):
        t = g.ty(g.r.choice([1, 2, 2, 3, 3]))
        v = g.value(t)
        w = v if g.r.random() < 0.4 else g.mutate(t, v)
        g.prog.cases.append(Case(t, v, w))
    return g.prog


# --------------------------------------------------------------------------------------------- equality

def equal(prog, t, a, b):
    """Structural equality, as the derived is_eq computes it."""
    k = t[0]
    if k in ("str", "bool", "int", "dec", "unit"):
        return a == b
    if k == "list":
        return len(a) == len(b) and all(equal(prog, t[1], x, y) for x, y in zip(a, b))
    if k == "set":
        return len(a[1]) == len(b[1]) and all(any(equal(prog, t[1], x, y) for y in b[1]) for x in a[1])
    if k == "dict":
        if len(a[1]) != len(b[1]):
            return False
        for ka, va in a[1]:
            match = [vb for kb, vb in b[1] if equal(prog, t[1], ka, kb)]
            if not match or not equal(prog, t[2], va, match[0]):
                return False
        return True
    if k == "tuple":
        return all(equal(prog, x, p, q) for x, p, q in zip(t[1], a, b))
    if k == "record":
        for n, x, opt in t[1]:
            ft = ("try", x, ("tags", [("Missing", [])])) if opt else x
            if not equal(prog, ft, a[1][n], b[1][n]):
                return False
        return True
    if k == "tags":
        if a[1] != b[1]:
            return False
        ps = dict(t[1])[a[1]]
        return all(equal(prog, p, x, y) for p, x, y in zip(ps, a[2], b[2]))
    if k == "try":
        return equal(prog, ("tags", [("Err", [t[2]]), ("Ok", [t[1]])]), a, b)
    if k == "nom":
        return equal(prog, backing_of(prog, t), a, b)
    raise AssertionError(t)


# --------------------------------------------------------------------------------------------- reference encoder

def json_string(s):
    """Json's append_json_quoted_string: byte by byte over the UTF-8 encoding."""
    out = bytearray(b'"')
    for byte in s.encode("utf-8"):
        if byte == 34:
            out += b'\\"'
        elif byte == 92:
            out += b"\\\\"
        elif byte == 8:
            out += b"\\b"
        elif byte == 9:
            out += b"\\t"
        elif byte == 10:
            out += b"\\n"
        elif byte == 12:
            out += b"\\f"
        elif byte == 13:
            out += b"\\r"
        elif byte < 32:
            out += b"\\u00" + b"0123456789abcdef"[byte // 16:byte // 16 + 1] + b"0123456789abcdef"[byte % 16:byte % 16 + 1]
        else:
            out.append(byte)
    out += b'"'
    return out.decode("utf-8")


def dec_str(attos):
    """Dec.to_str: the integer part, a point, and the fraction without trailing zeros (at least one digit)."""
    neg = attos < 0
    a = -attos if neg else attos
    whole, frac = divmod(a, DEC_ONE)
    fs = str(frac).rjust(18, "0").rstrip("0") or "0"
    return ("-" if neg else "") + f"{whole}.{fs}"


def key_str(t, v):
    if t[0] == "str":
        return json_string(v)
    if t[0] == "int":
        return json_string(str(v))
    if t[0] == "bool":
        return json_string("true" if v else "false")
    raise AssertionError(t)


def encode(prog, t, v):
    """The JSON text Json.to_str writes for v of type t."""
    k = t[0]
    if k == "str":
        return json_string(v)
    if k == "bool":
        return "true" if v else "false"
    if k == "int":
        return str(v)
    if k == "dec":
        return dec_str(v)
    if k == "unit":
        return "{}"
    if k == "list":
        return "[" + ",".join(encode(prog, t[1], x) for x in v) + "]"
    if k == "set":
        return "[" + ",".join(encode(prog, t[1], x) for x in v[1]) + "]"
    if k == "dict":
        return "{" + ",".join(key_str(t[1], kk) + ":" + encode(prog, t[2], vv) for kk, vv in v[1]) + "}"
    if k == "tuple":
        return "[" + ",".join(encode(prog, x, y) for x, y in zip(t[1], v)) + "]"
    if k == "record":
        parts = []
        for n, x, opt in sorted(t[1]):
            fv = v[1][n]
            if opt:
                if fv[1] == "Err":
                    continue
                fv = fv[2][0]
            parts.append(json_string(n) + ":" + encode(prog, x, fv))
        return "{" + ",".join(parts) + "}"
    if k == "tags":
        ps = dict(t[1])[v[1]]
        if not ps:
            return json_string(v[1])
        if len(ps) == 1:
            return "{" + json_string(v[1]) + ":" + encode(prog, ps[0], v[2][0]) + "}"
        return "{" + json_string(v[1]) + ":[" + ",".join(encode(prog, p, x) for p, x in zip(ps, v[2])) + "]}"
    if k == "try":
        if is_null_try(t):
            return encode(prog, t[1], v[2][0]) if v[1] == "Ok" else "null"
        return encode(prog, ("tags", [("Err", [t[2]]), ("Ok", [t[1]])]), v)
    if k == "nom":
        n = prog.nom(t[1])
        if n.kind == "custom":
            inner = n.backing[1][0][1]
            return encode(prog, inner, v[1]["v"])
        return encode(prog, backing_of(prog, t), v)
    raise AssertionError(t)


# --------------------------------------------------------------------------------------------- Roc emission

def render_type(t):
    k = t[0]
    if k == "str":
        return "Str"
    if k == "bool":
        return "Bool"
    if k == "int":
        return t[1]
    if k == "dec":
        return "Dec"
    if k == "unit":
        return "{}"
    if k == "list":
        return f"List({render_type(t[1])})"
    if k == "set":
        return f"Set({render_type(t[1])})"
    if k == "dict":
        return f"Dict({render_type(t[1])}, {render_type(t[2])})"
    if k == "tuple":
        return "(" + ", ".join(render_type(x) for x in t[1]) + ")"
    if k == "record":
        fs = []
        for n, x, opt in t[1]:
            ft = f"Try({render_type(x)}, [Missing])" if opt else render_type(x)
            fs.append(f"{n} : {ft}")
        return "{ " + ", ".join(fs) + " }"
    if k == "tags":
        return "[" + ", ".join(n if not ps else f"{n}({', '.join(render_type(p) for p in ps)})"
                               for n, ps in t[1]) + "]"
    if k == "try":
        return f"Try({render_type(t[1])}, {render_type(t[2])})"
    if k == "nom":
        return t[1] + (f"({', '.join(render_type(a) for a in t[2])})" if t[2] else "")
    if k == "param":
        return "a"
    if k == "self":
        return "SELF"
    raise AssertionError(t)


def roc_string(s):
    """A Roc string literal for s: printable ASCII as is, everything else as \\u(..)."""
    out = ['"']
    for ch in s:
        c = ord(ch)
        if ch in '"\\$' or c < 0x20 or c >= 0x7f:
            out.append(f"\\u({c:x})")
        else:
            out.append(ch)
    out.append('"')
    return "".join(out)


def roc_int(name, v):
    lo, hi = INTS[name]
    if v == lo and lo < 0:
        return f"{name}.lowest"
    if v < 0:
        return f"-{-v}.{name}"
    return f"{v}.{name}"


def roc_value(prog, t, v, nom_ctx=None):
    """A Roc expression of type t for v. nom_ctx qualifies a nominal backing's constructor."""
    k = t[0]
    if k == "str":
        return roc_string(v)
    if k == "bool":
        return "Bool.True" if v else "Bool.False"
    if k == "int":
        return roc_int(t[1], v)
    if k == "dec":
        if v == DEC_RANGE[0]:
            return "Dec.from_attos(I128.lowest)"
        return f"Dec.from_attos({roc_int('I128', v)})"
    if k == "unit":
        return "{}"
    if k == "list":
        return "[" + ", ".join(roc_value(prog, t[1], x) for x in v) + "]"
    if k == "set":
        return "Set.from_list([" + ", ".join(roc_value(prog, t[1], x) for x in v[1]) + "])"
    if k == "dict":
        return ("Dict.from_list([" + ", ".join(f"({roc_value(prog, t[1], kk)}, {roc_value(prog, t[2], vv)})"
                                               for kk, vv in v[1]) + "])")
    if k == "tuple":
        body = "(" + ", ".join(roc_value(prog, x, y) for x, y in zip(t[1], v)) + ")"
        return f"{nom_ctx}.{body}" if nom_ctx else body
    if k == "record":
        fs = []
        for n, x, opt in t[1]:
            fv = v[1][n]
            if opt:
                ft = ("try", x, ("tags", [("Missing", [])]))
                fs.append(f"{n}: {roc_value(prog, ft, fv)}")
            else:
                fs.append(f"{n}: {roc_value(prog, x, fv)}")
        body = "{ " + ", ".join(fs) + " }"
        return f"{nom_ctx}.{body}" if nom_ctx else body
    if k == "tags":
        ps = dict(t[1])[v[1]]
        q = f"{nom_ctx}." if nom_ctx else ""
        if not ps:
            return q + v[1]
        return q + v[1] + "(" + ", ".join(roc_value(prog, p, x) for p, x in zip(ps, v[2])) + ")"
    if k == "try":
        inner = t[1] if v[1] == "Ok" else t[2]
        return f"{v[1]}({roc_value(prog, inner, v[2][0])})"
    if k == "nom":
        b = backing_of(prog, t)
        if b[0] in ("record", "tags", "tuple"):
            return roc_value(prog, b, v, nom_ctx=t[1])
        return f"{t[1]}.({roc_value(prog, b, v)})"
    raise AssertionError(t)


def nom_decl(n):
    head = n.name + ("(a)" if n.param else "")
    backing = render_type(n.backing).replace("SELF", head)
    op = "::" if n.opaque else ":="
    if n.kind == "custom":
        inner = n.backing[1][0][1]
        m = {"str": "str", "bool": "bool"}.get(inner[0]) or inner[1].lower()
        return "\n".join([
            f"{n.name} {op} {backing}.{{",
            "\tis_eq : _",
            "\tto_hash : _",
            "\tparser_for = |encoding| |state| {",
            f"\t\tparsed = Json.parse_{m}(encoding, state)?",
            f"\t\tOk({{ value: {n.name}.{{ v: parsed.value }}, rest: parsed.rest }})",
            "\t}",
            f"\tencoder_for = |encoding| |x, state| Json.encode_{m}(encoding, x.v, state)",
            "}",
        ])
    methods = ["is_eq : _", "to_hash : _", "parser_for : _", "encoder_for : _"]
    return f"{head} {op} {backing}.{{\n" + "".join(f"\t{m}\n" for m in methods) + "}"


def case_lines(prog, i, c):
    """Roc definitions of case i: the type alias, v and w as functions of an unused argument."""
    T = f"T{i}"
    return [
        f"{T} : {render_type(c.ty)}",
        "",
        f"v{i} : U64 -> {T}",
        f"v{i} = |_z| {roc_value(prog, c.ty, c.v)}",
        "",
        f"w{i} : U64 -> {T}",
        f"w{i} = |_z| {roc_value(prog, c.ty, c.w)}",
        "",
    ]


def case_expect_eq(prog, c):
    return equal(prog, c.ty, c.v, c.w)


def emit_test_module(prog, name="Prog"):
    """A type module whose expects state each judgment. Returns (text, {line: (case, judgment)})."""
    lines = [f"{name} :: [].{{}}", ""]
    for n in prog.noms:
        lines += [nom_decl(n), ""]
    where = {}
    text_lines = "\n".join(lines).split("\n")

    def add(stmt, ci, judgment):
        where[len(text_lines) + 1] = (ci, judgment)
        text_lines.extend(stmt.split("\n"))
        text_lines.append("")

    for i, c in enumerate(prog.cases):
        text_lines.extend(case_lines(prog, i, c))
        T = f"T{i}"
        add(f"expect Json.to_str(v{i}(0)) == {roc_string(encode(prog, c.ty, c.v))}", i, "encode")
        add("\n".join([
            "expect {",
            f"\tr : Try({T}, [InvalidJson(Str)])",
            f"\tr = Json.parse(Json.to_str(v{i}(0)))",
            f"\tr == Ok(v{i}(0))",
            "}",
        ]), i, "roundtrip")
        add("\n".join([
            "expect {",
            f"\tr : Try({T}, [InvalidJson(Str)])",
            f"\tr = Json.parse({roc_string(encode(prog, c.ty, c.v))})",
            f"\tr == Ok(v{i}(0))",
            "}",
        ]), i, "decode")
        eq = case_expect_eq(prog, c)
        add(f"expect (v{i}(0) == w{i}(0)) == {'Bool.True' if eq else 'Bool.False'}", i, "eq")
        add(f"expect (v{i}(0) != w{i}(0)) == {'Bool.False' if eq else 'Bool.True'}", i, "neq")
        if hashable(prog, c.ty):
            add(f"expect Set.from_list([v{i}(0), w{i}(0)]).len() == {1 if eq else 2}", i, "hash")
    return "\n".join(text_lines), where


def emit_app(prog, target="native"):
    """An app printing, per case: `c<i> <roundtrip> <decode> <eq> <neq> <setlen> <json>`."""
    lines = []
    if target == "wasm":
        lines += ['app [main!] { pf: platform "./platform/main.roc" }', "", "import pf.FallibleHost", ""]
    for n in prog.noms:
        lines += [nom_decl(n), ""]
    for i, c in enumerate(prog.cases):
        lines += case_lines(prog, i, c)
        T = f"T{i}"
        set_len = f"Set.from_list([v, w]).len().to_str()" if hashable(prog, c.ty) else '"-"'
        lines += [
            f"run{i} : U64 -> Str",
            f"run{i} = |z| {{",
            # z is a runtime value the compiler cannot fold, so neither the values nor the decoded text are constants.
            f"\tv = if z < 1000000 v{i}(z) else w{i}(z)",
            f"\tw = if z < 1000000 w{i}(z) else v{i}(z)",
            "\tjs = Json.to_str(v)",
            f"\tr : Try({T}, [InvalidJson(Str)])",
            "\tr = Json.parse(js)",
            "\trt = match r {",
            '\t\tOk(x) => if x == v "ok" else "neq"',
            '\t\tErr(_) => "err"',
            "\t}",
            f"\td : Try({T}, [InvalidJson(Str)])",
            f"\td = Json.parse(if z < 1000000 {roc_string(encode(prog, c.ty, c.v))} else \"\")",
            "\tdc = match d {",
            '\t\tOk(x) => if x == v "ok" else "neq"',
            '\t\tErr(_) => "err"',
            "\t}",
            '\teq = if v == w "1" else "0"',
            '\tne = if v != w "1" else "0"',
            f'\tStr.join_with(["c{i}", rt, dc, eq, ne, {set_len}, js], " ")',
            "}",
            "",
        ]
    runs = ", ".join(f"run{i}(z)" for i in range(len(prog.cases)))
    if target == "wasm":
        lines += ["main! = || {", "\tz = Str.count_utf8_bytes(FallibleHost.json_input!({}))",
                  f'\tStr.join_with([{runs}], "\\n")', "}"]
    else:
        lines += ["main! = |args| {", "\tz = args.len()", f'\techo!(Str.join_with([{runs}], "\\n"))',
                  "\tOk({})", "}"]
    return "\n".join(lines) + "\n"


def expected_app_lines(prog):
    out = []
    for i, c in enumerate(prog.cases):
        eq = equal(prog, c.ty, c.v, c.w)
        set_len = ("1" if eq else "2") if hashable(prog, c.ty) else "-"
        out.append(f"c{i} ok ok {'1' if eq else '0'} {'0' if eq else '1'} {set_len} {encode(prog, c.ty, c.v)}")
    return out


def fixture(noms, cases):
    """A Prog from explicit nominals and (type, v, w) triples, for tests/*/case.py."""
    p = Prog()
    p.noms = list(noms)
    p.cases = [Case(t, v, w) for t, v, w in cases]
    return p
