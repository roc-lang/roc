"""Reference model of pre-ARC LIR: a parser for ROC_LIR_PASS_DUMP files and a small interpreter.

The interpreter is meant to be obviously correct, not fast. Values:
  int / Dec      -> Python int (Dec as attos, i.e. value * 10**18)
  F32/F64        -> Python float (F32 rounded through struct)
  Str            -> bytes
  List           -> LList(items, cap)   (functional: ops return new LList)
  struct         -> tuple, fields in semantic (original) index order
  tag            -> Tag(disc, payload)  (payload: value, or None)
  box / ptr      -> Cell (mutable; ptr_store / store_struct / store_tag write it)
  zero-sized     -> ()
  proc_ref       -> FnRef(proc)
See FORMAT.md and RULES.md.
"""
import struct as _struct
import sys

sys.setrecursionlimit(1_000_000)


# ---------------------------------------------------------------- parsing

class Proc:
    __slots__ = ("id", "name", "args", "ret", "body", "abi", "hosted", "static_init", "transform",
                 "erased_capture", "dyn_result", "stmts", "locals", "dyn", "text")

    def __init__(self):
        self.stmts = {}
        self.locals = {}
        self.dyn = set()
        self.text = []


def _ints(field):
    return [] if field == "-" else [int(x) for x in field.split(",")]


def _hexbytes(field):
    return b"" if field == "-" else bytes.fromhex(field)


def _arm(prefix, end, steps):
    out = []
    if steps != "-":
        for st in steps.split(","):
            cap, delim = st.split(":")
            out.append((None if cap == "_" else int(cap), _hexbytes(delim)))
    return (_hexbytes(prefix), end, out)


def parse_stmt(toks):
    """toks: tokens after 'S <id>'. Returns a tuple (kind, ...)."""
    k = toks[0]
    if k == "ref":
        t, kind, src, idx, var, disc, nxt = toks[1:8]
        return ("ref", int(t), kind, int(src), int(idx), int(var), int(disc), int(nxt))
    if k == "lit":
        t = int(toks[1])
        ty = toks[2]
        if ty in ("int", "dec"):
            return ("lit", t, ty, int(toks[3]), int(toks[4]))
        if ty == "f64":
            return ("lit", t, ty, _struct.unpack("<d", int(toks[3], 16).to_bytes(8, "little"))[0], int(toks[4]))
        if ty == "f32":
            return ("lit", t, ty, _struct.unpack("<f", int(toks[3], 16).to_bytes(4, "little"))[0], int(toks[4]))
        if ty == "str":
            return ("lit", t, ty, _hexbytes(toks[3]), int(toks[4]))
        if ty == "bytes":
            return ("lit", t, ty, (int(toks[3]), _hexbytes(toks[4])), int(toks[5]))
        if ty in ("static", "proc_ref"):
            return ("lit", t, ty, int(toks[3]), int(toks[4]))
        raise ValueError(toks)
    if k == "uninit":
        return ("uninit", int(toks[1]), int(toks[2]))
    if k == "call":
        return ("call", int(toks[1]), int(toks[2]), _ints(toks[3]), int(toks[4]), toks[5] == "tail=1")
    if k == "call_erased":
        return ("call_erased", int(toks[1]), int(toks[2]), _ints(toks[3]), int(toks[4]))
    if k == "packed_fn":
        return ("packed_fn", int(toks[1]), int(toks[2]), None if toks[3] == "-" else int(toks[3]), int(toks[4]))
    if k == "ll":
        return ("ll", int(toks[1]), toks[2], _ints(toks[3]), int(toks[4]), "dyn_elem_desc" in toks[5:])
    if k in ("list", "struct"):
        return (k, int(toks[1]), _ints(toks[2]), int(toks[3]))
    if k == "tag":
        return ("tag", int(toks[1]), int(toks[2]), int(toks[3]), None if toks[4] == "-" else int(toks[4]), int(toks[5]))
    if k == "store_struct":
        return ("store_struct", int(toks[1]), int(toks[2]), _ints(toks[3]), int(toks[4]))
    if k == "store_tag":
        return ("store_tag", int(toks[1]), int(toks[2]), int(toks[3]), int(toks[4]),
                None if toks[5] == "-" else int(toks[5]), int(toks[6]))
    if k == "set":
        return ("set", int(toks[1]), int(toks[2]), toks[3], int(toks[4]))
    if k in ("debug", "expect"):
        return (k, int(toks[1]), int(toks[2]))
    if k == "expect_err":
        return ("expect_err", int(toks[1]))
    if k == "dyn":
        return ("dyn", toks[1], toks[2], int(toks[3]))
    if k in ("runtime_error", "comptime_exhaustiveness_failed"):
        return (k,)
    if k == "rc":
        return ("rc", toks[1], int(toks[2]), int(toks[3]))
    if k == "switch":
        cond, dflt, cont = int(toks[1]), int(toks[2]), toks[3]
        branches = []
        for b in toks[4:]:
            v, s = b.split(":")
            branches.append((int(v), int(s)))
        return ("switch", cond, dflt, None if cont == "-" else int(cont), branches)
    if k == "sip":
        return ("sip", int(toks[1]), int(toks[2]), int(toks[3]), int(toks[4]), int(toks[5]))
    if k == "str_match":
        src, on_match, on_miss = int(toks[1]), int(toks[2]), int(toks[3])
        assert toks[4] == "arm"
        return ("str_match_set", src, on_miss, [(on_match,) + _arm(toks[5], toks[6], toks[7])])
    if k == "str_match_set":
        src, on_miss = int(toks[1]), int(toks[2])
        arms = []
        i = 3
        while i < len(toks):
            assert toks[i] == "arm"
            arms.append((int(toks[i + 1]),) + _arm(toks[i + 2], toks[i + 3], toks[i + 4]))
            i += 5
        return ("str_match_set", src, on_miss, arms)
    if k == "join":
        return ("join", int(toks[1]), _ints(toks[2]), int(toks[3]), int(toks[4]))
    if k == "jump":
        return ("jump", int(toks[1]))
    if k == "ret":
        return ("ret", int(toks[1]))
    if k == "crash":
        if toks[1] == "lit":
            return ("crash", "lit", _hexbytes(toks[2]))
        return ("crash", "local", int(toks[2]))
    raise ValueError("unknown stmt " + " ".join(toks))


def parse_layout(toks):
    """toks after 'L <id>'."""
    k = toks[0]
    if k in ("int", "frac", "vector"):
        return (k, toks[1])
    if k in ("box", "list", "ptr", "closure"):
        return (k, int(toks[1]))
    if k == "struct":
        fields = []
        for f in toks[1:]:
            i, l = f.split(":")
            fields.append((int(i), int(l)))
        fields.sort()
        return ("struct", tuple(l for _, l in fields), tuple(i for i, _ in fields))
    if k == "tag_union":
        return ("tag_union", tuple(int(x) for x in toks[2:]))
    return (k,)


def parse_procs(lines, layouts, statics=None):
    procs = {}
    cur = None
    for line in lines:
        toks = line.split()
        if not toks:
            continue
        if toks[0] == "D":
            if statics is not None:
                slot, lay, kind = int(toks[1]), int(toks[2]), toks[3]
                if kind == "parts":
                    statics[slot] = ("parts", lay, toks[4], int(toks[5]), [int(x) for x in toks[6:]])
                elif kind in ("init", "callable"):
                    statics[slot] = (kind, lay, int(toks[4]))
                else:
                    statics[slot] = ("opaque", lay)
            continue
        if toks[0] == "P":
            cur = Proc()
            cur.text.append(line)
            cur.id = int(toks[1])
            kv = dict(t.split("=", 1) for t in toks[2:])
            cur.name = kv["name"]
            cur.args = _ints(kv["args"])
            cur.ret = int(kv["ret"])
            cur.body = None if kv["body"] == "-" else int(kv["body"])
            cur.abi = kv["abi"]
            cur.hosted = kv["hosted"] == "1"
            cur.static_init = kv["static_init"] == "1"
            cur.transform = kv.get("transform", "none")
            cur.erased_capture = kv.get("erased_capture") == "1"
            cur.dyn_result = kv.get("dyn_result") == "1"
            procs[cur.id] = cur
        elif toks[0] == "S":
            cur.text.append(line)
            cur.stmts[int(toks[1])] = parse_stmt(toks[2:])
        elif toks[0] == "V":
            cur.text.append(line)
            cur.locals[int(toks[1])] = int(toks[2])
            if len(toks) > 3 and toks[3] == "dyn":
                cur.dyn.add(int(toks[1]))
        elif toks[0] == "L":
            layouts[int(toks[1])] = parse_layout(toks[2:])
    return procs


class Block:
    def __init__(self, kind, run, stage, label, proc):
        self.kind, self.run, self.stage, self.label, self.proc = kind, run, stage, label, proc
        self.procs = None  # program blocks
        self.statics = {}
        self.pre = self.post = None  # phase blocks
        self.layouts = {}


def parse_dump(text):
    """The blocks of a dump file, in file order."""
    blocks = []
    lines = text.split("\n")
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        if not line.startswith("@@LPD begin"):
            i += 1
            continue
        hdr = dict(t.split("=", 1) for t in line.split()[3:])
        b = Block(line.split()[2], int(hdr["run"]), int(hdr["stage"]), hdr["label"], hdr["proc"])
        i += 1
        body = []
        while i < n and lines[i] != "@@LPD end":
            body.append(lines[i])
            i += 1
        i += 1
        if b.kind == "program":
            b.procs = parse_procs(body, b.layouts, b.statics)
        else:
            k = body.index("@@LPD post")
            assert body[0] == "@@LPD pre"
            pre = parse_procs(body[1:k], b.layouts)
            post = parse_procs(body[k + 1:], b.layouts)
            b.pre = next(iter(pre.values()))
            b.post = next(iter(post.values()))
        blocks.append(b)
    return blocks


# ---------------------------------------------------------------- values

class LList:
    """`shared` models a list some other owner may also hold (an argument or static data in the
    not-unique run); every list an operation builds is unique."""
    __slots__ = ("items", "cap", "shared")

    def __init__(self, items, cap=None, shared=False):
        self.items = list(items)
        self.cap = len(self.items) if cap is None else max(cap, len(self.items))
        self.shared = shared

    def __repr__(self):
        return "LList(%r)" % (self.items,)


class Tag:
    __slots__ = ("disc", "payload")

    def __init__(self, disc, payload):
        self.disc, self.payload = disc, payload

    def __repr__(self):
        return "Tag(%d,%r)" % (self.disc, self.payload)


class Cell:
    __slots__ = ("val",)

    def __init__(self, val):
        self.val = val

    def __repr__(self):
        return "Cell(%r)" % (self.val,)


class FnRef:
    __slots__ = ("proc",)

    def __init__(self, proc):
        self.proc = proc


class StaticAtom:
    """A static data value the model cannot see (compile-time result / ConstStore node)."""
    __slots__ = ("slot",)

    def __init__(self, slot):
        self.slot = slot


ZERO = object()  # contents of a zero-filled box cell / fresh alloca: reading it is undefined
POISON = object()  # value of init_uninitialized


class Undefined(Exception):
    """The run left the defined semantics (bad input, or an unsafe op's precondition failed)."""


class Unsupported(Exception):
    """The model does not cover something this run needs."""


class Crash(Exception):
    def __init__(self, msg):
        super().__init__(msg)
        self.msg = msg


class OutOfFuel(Exception):
    pass


class ExpectErr(Exception):
    def __init__(self, msg):
        super().__init__(msg)
        self.msg = msg


def canon(v, depth=0):
    """Deep, hashable, comparable form of a value (cells dereferenced, capacity dropped)."""
    if depth > 10000:
        raise Unsupported("value too deep")
    if v is None or isinstance(v, (int, bytes)):
        return v
    if isinstance(v, float):
        return ("f", "nan" if v != v else v)
    if isinstance(v, tuple):
        t = tuple(canon(x, depth + 1) for x in v)
        return () if all(x == () for x in t) else t
    if isinstance(v, LList):
        return ("list", tuple(canon(x, depth + 1) for x in v.items))
    if isinstance(v, Tag):
        return ("tag", v.disc, () if v.payload is None else canon(v.payload, depth + 1))
    if isinstance(v, Cell):
        return ("box", canon(v.val, depth + 1))
    if isinstance(v, FnRef):
        return ("fn", v.proc)
    if isinstance(v, StaticAtom):
        return ("static", v.slot)
    if v is ZERO:
        return ("zero",)
    if v is POISON:
        return ("poison",)
    raise Unsupported("canon " + type(v).__name__)


def clone(v, memo=None, shared=False):
    """Deep copy, preserving cell sharing; `shared` marks every copied list as shared."""
    if memo is None:
        memo = {}
    if v is None or isinstance(v, (int, bytes, float, FnRef, StaticAtom)) or v is ZERO or v is POISON:
        return v
    if isinstance(v, tuple):
        return tuple(clone(x, memo, shared) for x in v)
    if isinstance(v, LList):
        return LList([clone(x, memo, shared) for x in v.items], v.cap, shared or v.shared)
    if isinstance(v, Tag):
        return Tag(v.disc, clone(v.payload, memo, shared))
    if isinstance(v, Cell):
        if id(v) in memo:
            return memo[id(v)]
        c = Cell(None)
        memo[id(v)] = c
        c.val = clone(v.val, memo, shared)
        return c
    raise Unsupported("clone " + type(v).__name__)


# ---------------------------------------------------------------- layouts

# Layout ids below 17 are the layout store's fixed scalar layouts (`layout.Idx` in src/layout/layout.zig).
# The export prints an L line for every layout a procedure mentions, but not for a D line's layout.
SENTINEL = ["bool", "str", "u8", "i8", "u16", "i16", "u32", "i32", "u64", "i64", "u128", "i128",
            "f32", "f64", "dec", "opaque_ptr", "zst"]
INT_BITS = {"u8": (8, False), "i8": (8, True), "u16": (16, False), "i16": (16, True), "u32": (32, False),
            "i32": (32, True), "u64": (64, False), "i64": (64, True), "u128": (128, False), "i128": (128, True)}


def wrap(n, kind):
    bits, signed = INT_BITS[kind]
    n &= (1 << bits) - 1
    if signed and n >= 1 << (bits - 1):
        n -= 1 << bits
    return n


def in_range(n, kind):
    return wrap(n, kind) == n


def f32(x):
    try:
        return _struct.unpack("<f", _struct.pack("<f", x))[0]
    except OverflowError:
        return float("inf") if x > 0 else float("-inf")


class Layouts:
    def __init__(self, table):
        self.t = table

    def get(self, idx):
        if idx in self.t:
            return self.t[idx]
        if idx < len(SENTINEL):
            name = SENTINEL[idx]
            if name in INT_BITS:
                return ("int", name)
            if name in ("f32", "f64", "dec"):
                return ("frac", name)
            return (name,)
        raise Unsupported("layout %d missing" % idx)

    def num_kind(self, idx):
        """'u8'..'i128', 'f32', 'f64', 'dec', 'bool', or None."""
        l = self.get(idx)
        if l[0] in ("int", "frac"):
            return l[1]
        if idx == 0:
            return "bool"
        return None

    def is_box(self, idx):
        return self.get(idx)[0] in ("box", "box_of_zst")


DEC_ONE = 10 ** 18
I128 = "i128"
OPAQUE_PTR = 15
ZST = 16


# ---------------------------------------------------------------- interpreter

class UnboundLocal(Exception):
    """A statement read a local no statement on this path wrote (an LIR well-formedness failure)."""


def _bool(b):
    return 1 if b else 0


def _truthy(v):
    if isinstance(v, StaticAtom):
        raise Unsupported("opaque static data")
    if isinstance(v, Tag):
        return v.disc
    if isinstance(v, int):
        return v
    raise Undefined("non-bool condition %r" % (v,))


def _items(v):
    if isinstance(v, StaticAtom):
        raise Unsupported("opaque static data")
    if not isinstance(v, LList):
        raise Undefined("expected list, got %r" % (type(v).__name__,))
    return v.items


def _int(v):
    if isinstance(v, StaticAtom):
        raise Unsupported("opaque static data")
    if isinstance(v, bool) or not isinstance(v, int):
        raise Undefined("expected int, got %r" % (v,))
    return v


class Machine:
    """Runs procedures of one program snapshot.

    procs: dict id -> Proc (callees are resolved here).
    unique: what list/str uniqueness queries answer (the pre-ARC model has no refcounts, so the
            answer is an external choice; a correct pass must give the same result for either).
    hosted: callable(name, args, ret_layout, index) -> value, the effect oracle.
    """

    def __init__(self, procs, layouts, fuel=200_000, unique=True, hosted=None, max_depth=4000, statics=None):
        self.procs = procs
        self.statics = statics or {}
        self.static_cache = {}
        self.L = layouts
        self.fuel = fuel
        self.steps = 0
        self.unique = unique
        self.hosted = hosted
        self.effects = []
        self.depth = 0
        self.max_depth_seen = 0
        self.max_depth = max_depth
        self.call_log = None  # optional: list of (proc_id, cloned args) for input harvesting
        self.call_log_limit = 0

    # -- helpers
    def coerce(self, v, src_layout, dst_layout):
        if src_layout == dst_layout:
            return v
        sb, db = self.L.is_box(src_layout), self.L.is_box(dst_layout)
        if db and not sb:
            if self.L.get(src_layout)[0] not in ("int", "frac") and src_layout != OPAQUE_PTR:
                return Cell(v)
        if sb and not db and dst_layout != OPAQUE_PTR:
            if not isinstance(v, Cell):
                raise Undefined("unbox of non-box")
            return v.val
        sk, dk = self.L.num_kind(src_layout), self.L.num_kind(dst_layout)
        if sk in INT_BITS and dk in INT_BITS and isinstance(v, int):
            return wrap(v, dk)
        return v

    def static(self, slot):
        if slot in self.static_cache:
            return self.static_cache[slot]
        d = self.statics.get(slot)
        if d is None or d[0] == "opaque" or d[0] == "callable":
            v = StaticAtom(slot)
        elif d[0] == "init":
            v = self.call(d[2], [])
        else:
            _, lay, kind, arg, children = d
            ch = [self.static(c) for c in children]
            if kind == "word":
                v = arg
                if self.L.num_kind(lay) not in INT_BITS:
                    v = StaticAtom(slot)
            elif kind == "product":
                v = tuple(ch)
            elif kind == "list":
                v = LList(ch, arg)
            elif kind == "box":
                v = Cell(ch[0] if ch else ())
            elif kind == "tag":
                v = arg if lay == 0 else Tag(arg, ch[0] if ch else None)
            else:
                v = StaticAtom(slot)
        self.static_cache[slot] = v
        return v

    # -- calls
    def call(self, pid, args):
        proc = self.procs.get(pid)
        if proc is None:
            raise Unsupported("call to unknown proc %d" % pid)
        if proc.hosted:
            if self.hosted is None:
                raise Unsupported("hosted call")
            idx = len(self.effects)
            canon_args = tuple(canon(a) for a in args)
            v = self.hosted(proc.name, canon_args, proc.ret, idx)
            self.effects.append(("hosted", proc.name, canon_args))
            return v
        if proc.body is None:
            raise Unsupported("call to bodiless proc %s" % proc.name)
        if proc.abi != "roc" or proc.erased_capture or proc.dyn_result or proc.dyn:
            raise Unsupported("proc abi %s" % proc.abi)
        if len(args) != len(proc.args):
            raise Unsupported("arity")
        if self.call_log is not None and len(self.call_log) < self.call_log_limit:
            try:
                self.call_log.append((pid, tuple(clone(a) for a in args)))
            except Unsupported:
                pass
        self.depth += 1
        if self.depth > self.max_depth_seen:
            self.max_depth_seen = self.depth
        if self.depth > self.max_depth:
            raise OutOfFuel("depth")
        try:
            return self.run(proc, args)
        finally:
            self.depth -= 1

    def run(self, proc, args):
        env = dict(zip(proc.args, args))
        joins = {}
        stmts = proc.stmts
        cur = proc.body
        L = self.L
        loc = proc.locals

        def get(l):
            try:
                v = env[l]
            except KeyError:
                raise UnboundLocal("p%d l%d" % (proc.id, l))
            if v is POISON:
                raise Undefined("read of uninitialized l%d" % l)
            return v

        while True:
            self.steps += 1
            if self.steps > self.fuel:
                raise OutOfFuel("steps")
            s = stmts.get(cur)
            if s is None:
                raise Unsupported("missing stmt %d" % cur)
            k = s[0]
            if k == "ref":
                _, t, kind, src, idx, var, disc, nxt = s
                v = get(src)
                sl = loc[src]
                if kind == "local" or kind == "nominal" or kind == "list_reinterpret":
                    env[t] = self.coerce(v, sl, loc[t])
                elif kind == "field":
                    if L.is_box(sl):
                        if not isinstance(v, Cell):
                            raise Undefined("field of non-box")
                        v = v.val
                    if not isinstance(v, tuple) or idx >= len(v):
                        raise Undefined("field %d of %r" % (idx, type(v).__name__))
                    env[t] = v[idx]
                elif kind in ("tag_payload", "tag_payload_struct", "discriminant"):
                    if L.is_box(sl):
                        if not isinstance(v, Cell):
                            raise Undefined("payload of non-box")
                        v = v.val
                    if kind == "discriminant":
                        env[t] = v.disc if isinstance(v, Tag) else _int(v)
                    else:
                        if not isinstance(v, Tag):
                            raise Undefined("payload of non-tag %r" % (v,))
                        if v.disc != disc:
                            raise Undefined("payload of wrong variant (have %d want %d)" % (v.disc, disc))
                        p = v.payload
                        if kind == "tag_payload_struct":
                            env[t] = p
                        else:
                            ul = L.get(sl)
                            if ul[0] == "box":
                                ul = L.get(ul[1])
                            pl = L.get(ul[1][var]) if ul[0] == "tag_union" and var < len(ul[1]) else ("zst",)
                            if pl[0] == "struct":
                                if not isinstance(p, tuple) or idx >= len(p):
                                    raise Undefined("payload field")
                                env[t] = p[idx]
                            else:
                                env[t] = p
                else:
                    raise Unsupported("ref " + kind)
                cur = nxt
            elif k == "lit":
                _, t, ty, val, nxt = s
                if ty == "int":
                    kd = L.num_kind(loc[t])
                    if kd == "dec":
                        val = val * DEC_ONE
                    env[t] = val
                elif ty in ("dec", "str"):
                    env[t] = val
                elif ty == "f64":
                    env[t] = val
                elif ty == "f32":
                    env[t] = f32(val)
                elif ty == "bytes":
                    n, data = val
                    env[t] = LList(list(data[:n]))
                elif ty == "proc_ref":
                    env[t] = FnRef(val)
                elif ty == "static":
                    env[t] = clone(self.static(val), shared=not self.unique)
                else:
                    raise Unsupported("literal " + ty)
                cur = nxt
            elif k == "uninit":
                env[s[1]] = POISON
                cur = s[2]
            elif k == "call":
                _, t, p, a, nxt, _tail = s
                env[t] = self.call(p, [get(x) for x in a])
                cur = nxt
            elif k == "ll":
                _, t, op, a, nxt, dyn = s
                if dyn:
                    raise Unsupported("dyn_elem_desc")
                env[t] = self.lowlevel(op, a, t, proc, get)
                cur = nxt
            elif k == "list":
                env[s[1]] = LList([get(x) for x in s[2]])
                cur = s[3]
            elif k == "struct":
                v = tuple(get(x) for x in s[2])
                env[s[1]] = Cell(v) if L.is_box(loc[s[1]]) else v
                cur = s[3]
            elif k == "tag":
                _, t, var, disc, pay, nxt = s
                if loc[t] == 0:
                    env[t] = disc
                else:
                    v = Tag(disc, None if pay is None else get(pay))
                    env[t] = Cell(v) if L.is_box(loc[t]) else v
                cur = nxt
            elif k == "store_struct":
                _, dest, lay, fields, nxt = s
                c = get(dest)
                if not isinstance(c, Cell):
                    raise Undefined("store through non-pointer")
                c.val = tuple(get(x) for x in fields)
                cur = nxt
            elif k == "store_tag":
                _, dest, lay, var, disc, pay, nxt = s
                c = get(dest)
                if not isinstance(c, Cell):
                    raise Undefined("store through non-pointer")
                c.val = disc if lay == 0 else Tag(disc, None if pay is None else get(pay))
                cur = nxt
            elif k == "set":
                _, t, v, mode, nxt = s
                env[t] = self.coerce(get(v), loc[v], loc[t])
                cur = nxt
            elif k == "debug":
                self.effects.append(("dbg", canon(get(s[1]))))
                cur = s[2]
            elif k == "expect":
                if not _truthy(get(s[1])):
                    self.effects.append(("expect_failed",))
                cur = s[2]
            elif k == "expect_err":
                raise ExpectErr(canon(get(s[1])))
            elif k == "switch":
                _, cond, dflt, cont, branches = s
                c = get(cond)
                c = c.disc if isinstance(c, Tag) else _int(c)
                c &= (1 << 64) - 1
                nxt = dflt
                for bv, body in branches:
                    if bv == c:
                        nxt = body
                        break
                cur = nxt
            elif k == "sip":
                raise Unsupported("switch_initialized_payload")
            elif k == "str_match_set":
                _, src, on_miss, arms = s
                text = get(src)
                nxt = on_miss
                for on_match, prefix, end, steps in arms:
                    caps = str_match(text, prefix, end, steps)
                    if caps is not None:
                        for l, b in caps:
                            env[l] = b
                        nxt = on_match
                        break
                cur = nxt
            elif k == "join":
                _, jid, params, body, rem = s
                joins[jid] = body
                cur = rem
            elif k == "jump":
                if s[1] not in joins:
                    raise UnboundLocal("jump to unbound join %d" % s[1])
                cur = joins[s[1]]
            elif k == "ret":
                return get(s[1])
            elif k == "crash":
                if s[1] == "lit":
                    raise Crash(s[2])
                raise Crash(canon(get(s[2])))
            elif k == "runtime_error":
                raise Undefined("runtime_error reached")
            elif k == "comptime_exhaustiveness_failed":
                raise Crash(b"non-exhaustive pattern match")
            else:
                raise Unsupported("stmt " + k)

    # -- low-level operations
    def lowlevel(self, op, a, t, proc, get):
        L = self.L
        loc = proc.locals
        args = [get(x) for x in a]
        rk = L.num_kind(loc[t])
        ak = L.num_kind(loc[a[0]]) if a else None

        def ret_int(n, kind=None):
            kind = kind or rk
            if kind in INT_BITS:
                return wrap(n, kind)
            return n

        if op.startswith("num_int_") and op.split("_")[2] in ("add", "sub", "mul"):
            o, mode = op.split("_")[2], "_".join(op.split("_")[3:])
            x, y = _int(args[0]), _int(args[1])
            if ak == "dec":
                if o == "mul":
                    raise Unsupported("dec int mul")
                r = x + y if o == "add" else x - y
                kind = I128
            else:
                r = {"add": x + y, "sub": x - y, "mul": x * y}[o]
                kind = ak
            if kind not in INT_BITS:
                raise Unsupported("int family on %s" % kind)
            ovf = not in_range(r, kind)
            if mode == "overflows":
                return _bool(ovf)
            if mode == "wrap":
                return wrap(r, kind)
            if mode == "crash_on_overflow":
                if ovf:
                    raise Crash(b"overflow:" + op.encode())
                return r
            if mode == "proven_cannot_overflow":
                if ovf:
                    raise Undefined("proven_cannot_overflow overflowed")
                return r
            raise Unsupported(op)
        if op in ("num_is_eq", "num_is_lt", "num_is_lte", "num_is_gt", "num_is_gte"):
            x, y = args
            if isinstance(x, Tag) or isinstance(y, Tag):
                x = x.disc if isinstance(x, Tag) else x
                y = y.disc if isinstance(y, Tag) else y
            if not isinstance(x, (int, float)) or not isinstance(y, (int, float)):
                raise Unsupported("%s on %s" % (op, type(x).__name__))
            return _bool({"num_is_eq": x == y, "num_is_lt": x < y, "num_is_lte": x <= y,
                          "num_is_gt": x > y, "num_is_gte": x >= y}[op])
        if op == "compare":
            x, y = args
            return 1 if x < y else (2 if x == y else 0)
        if op == "bool_not":
            return 1 - _truthy(args[0])
        if op == "bool_likely":
            return _truthy(args[0])
        if op in ("num_float_add", "num_float_sub", "num_float_mul"):
            x, y = args
            r = {"num_float_add": x + y, "num_float_sub": x - y, "num_float_mul": x * y}[op] \
                if ak in ("f32", "f64") else None
            if r is None:
                if ak == "dec":
                    r = {"num_float_add": x + y, "num_float_sub": x - y, "num_float_mul": None}[op]
                    if r is None:
                        raise Unsupported("dec mul")
                    if not in_range(r, I128):
                        raise Crash(b"dec overflow")
                    return r
                raise Unsupported("%s on %s" % (op, ak))
            return f32(r) if ak == "f32" else r
        if op in ("num_div_by", "num_div_trunc_by", "num_rem_by", "num_mod_by",
                  "num_div_trunc_by_checked", "num_rem_by_checked", "num_mod_by_checked", "num_div_by_checked"):
            x, y = args
            if ak in INT_BITS:
                if y == 0:
                    if op.endswith("_checked"):
                        raise Crash(b"division by zero")
                    raise Undefined("int division by zero")
                q = abs(x) // abs(y)
                if (x < 0) != (y < 0):
                    q = -q
                base = op.replace("_checked", "")
                if base in ("num_div_by", "num_div_trunc_by"):
                    r = q
                elif base == "num_rem_by":
                    r = x - q * y
                else:
                    r = x % y if y > 0 else -((-x) % (-y))
                if not in_range(r, ak):
                    if op.endswith("_checked"):
                        raise Crash(b"division overflow")
                    raise Undefined("division overflow")
                return r
            raise Unsupported("%s on %s" % (op, ak))
        if op in ("num_negate", "num_negate_checked", "num_abs", "num_abs_checked"):
            x = args[0]
            if ak in INT_BITS:
                r = -x if op.startswith("num_negate") else abs(x)
                if not in_range(r, ak):
                    if op.endswith("_checked"):
                        raise Crash(b"negate overflow")
                    return wrap(r, ak)
                return r
            if ak in ("f32", "f64"):
                return -x if op.startswith("num_negate") else abs(x)
            raise Unsupported(op)
        if op == "num_abs_diff":
            x, y = args
            if ak in INT_BITS:
                return wrap(abs(x - y), rk if rk in INT_BITS else ak)
            return abs(x - y)
        if op in ("num_bitwise_and", "num_bitwise_or", "num_bitwise_xor"):
            x, y = args
            r = {"num_bitwise_and": x & y, "num_bitwise_or": x | y, "num_bitwise_xor": x ^ y}[op]
            return ret_int(r, ak)
        if op == "num_bitwise_not":
            return ret_int(~args[0], ak)
        if op in ("num_shift_left_by", "num_shift_right_by", "num_shift_right_zf_by"):
            x, n = args
            bits, signed = INT_BITS[ak]
            if n >= bits:
                if op == "num_shift_right_by" and signed and x < 0:
                    return -1
                return 0
            if op == "num_shift_left_by":
                return wrap(x << n, ak)
            if op == "num_shift_right_by":
                return x >> n
            return wrap((x & ((1 << bits) - 1)) >> n, ak)
        if op.endswith("_to_str") and op.split("_")[0] in INT_BITS:
            return str(args[0]).encode()
        if op == "dec_to_str":
            x = _int(args[0])
            sign = "-" if x < 0 else ""
            q, r = divmod(abs(x), DEC_ONE)
            frac = ("%018d" % r).rstrip("0") or "0"
            return ("%s%d.%s" % (sign, q, frac)).encode()
        if op in ("f64_to_str", "f32_to_str"):
            return repr(args[0]).encode()
        if op in ("dec_mul", "dec_mul_saturated"):
            x, y = _int(args[0]), _int(args[1])
            p = x * y
            r = abs(p) // DEC_ONE
            r = -r if p < 0 else r
            if not in_range(r, I128):
                if op == "dec_mul_saturated":
                    return (1 << 127) - 1 if r > 0 else -(1 << 127)
                raise Crash(b"Dec multiplication overflowed")
            return r
        conv = _conversion(op)
        if conv is not None:
            src, dst, mode = conv
            x = args[0]
            if src in INT_BITS and dst in INT_BITS:
                if mode == "wrap" or mode == "":
                    return wrap(x, dst)
                raise Unsupported(op)
            if src in INT_BITS and dst in ("f32", "f64"):
                return f32(float(x)) if dst == "f32" else float(x)
            if src in INT_BITS and dst == "dec" and mode == "":
                return x * DEC_ONE
            raise Unsupported(op)
        # strings
        if op in ("str_concat", "str_append"):
            return _bytes(args[0]) + _bytes(args[1])
        if op == "str_is_eq":
            return _bool(_bytes(args[0]) == _bytes(args[1]))
        if op == "str_count_utf8_bytes":
            return len(_bytes(args[0]))
        if op == "str_to_utf8":
            return LList(list(_bytes(args[0])))
        if op == "str_starts_with":
            return _bool(_bytes(args[0]).startswith(_bytes(args[1])))
        if op == "str_ends_with":
            return _bool(_bytes(args[0]).endswith(_bytes(args[1])))
        if op == "str_contains":
            return _bool(_bytes(args[1]) in _bytes(args[0]))
        if op == "str_repeat":
            return _bytes(args[0]) * max(0, args[1])
        if op == "str_with_capacity":
            return b""
        if op in ("str_reserve", "str_release_excess_capacity"):
            return _bytes(args[0])
        if op == "str_trim":
            return _bytes(args[0]).strip(b" \t\n\r")
        if op == "str_trim_start":
            return _bytes(args[0]).lstrip(b" \t\n\r")
        if op == "str_trim_end":
            return _bytes(args[0]).rstrip(b" \t\n\r")
        if op == "str_join_with":
            return _bytes(args[1]).join(_bytes(x) for x in _items(args[0]))
        if op == "str_split_on":
            sep = _bytes(args[1])
            if sep == b"":
                return LList([_bytes(args[0])])
            return LList(_bytes(args[0]).split(sep))
        if op == "str_inspect":
            b = _bytes(args[0])
            return b'"' + b.replace(b"\\", b"\\\\").replace(b'"', b'\\"') + b'"'
        if op == "str_get_utf8_byte_unsafe":
            b = _bytes(args[0])
            if not 0 <= args[1] < len(b):
                raise Undefined("str byte index")
            return b[args[1]]
        if op == "str_substring_unsafe":
            b = _bytes(args[0])
            st, ln = args[1], args[2]
            if st + ln > len(b):
                raise Undefined("substring range")
            return b[st:st + ln]
        if op == "str_from_parts":
            return b"".join(_bytes(x) for x in args)
        if op == "str_is_eq_static_small":
            raise Unsupported(op)
        # lists
        if op == "list_len":
            return len(_items(args[0]))
        if op == "list_capacity":
            return args[0].cap
        if op == "list_prefetch":
            return ()
        if op == "list_get_unsafe":
            it = _items(args[0])
            i = args[1]
            if not 0 <= i < len(it):
                raise Undefined("list_get_unsafe out of range")
            return it[i]
        if op == "list_with_capacity":
            return LList([], args[0])
        if op in ("list_reserve", "list_reserve_for_append"):
            l = args[0]
            return LList(_items(l), max(l.cap, len(l.items) + args[1]))
        if op == "list_release_excess_capacity":
            return LList(_items(args[0]))
        if op == "list_append_unsafe":
            l = args[0]
            it = _items(l)
            if l.cap <= len(it):
                raise Undefined("list_append_unsafe without capacity")
            return LList(it + [args[1]], l.cap)
        if op == "list_concat":
            x, y = args
            return LList(_items(x) + _items(y), x.cap)
        if op == "list_prepend":
            return LList([args[1]] + _items(args[0]))
        if op == "list_reverse":
            return LList(list(reversed(_items(args[0]))), args[0].cap)
        if op == "list_clear":
            return LList([], args[0].cap)
        if op in ("list_set", "list_set_in_place_unsafe"):
            l, i, v = args
            it = list(_items(l))
            if not 0 <= i < len(it):
                if op == "list_set_in_place_unsafe":
                    raise Undefined("set_in_place out of range")
                return LList(it, l.cap)
            it[i] = v
            return LList(it, l.cap)
        if op == "list_replace_unsafe":
            if not 0 <= args[1] < len(_items(args[0])):
                raise Undefined("replace_unsafe out of range")
            raise Unsupported("list_replace_unsafe result shape")
        if op == "list_swap":
            l, i, j = args
            it = list(_items(l))
            if 0 <= i < len(it) and 0 <= j < len(it):
                it[i], it[j] = it[j], it[i]
            return LList(it, l.cap)
        if op == "list_drop_at":
            l, i = args
            it = list(_items(l))
            if 0 <= i < len(it):
                del it[i]
            return LList(it)
        if op in ("list_sublist", "list_sublist_borrowed"):
            l, rec = args  # rec = { len (field 0), start (field 1) }
            ln, st = rec
            it = _items(l)
            return LList(it[st:st + ln])
        if op == "list_slack_unique":
            l = args[0]
            return (l.cap - len(_items(l))) if not l.shared else 0
        if op == "list_owned_unique":
            _items(args[0])
            return _bool(not args[0].shared)
        if op in ("list_append_range_within", "list_append_range_within_unsafe"):
            l, st, cnt = args
            it = _items(l)
            if st + cnt > len(it):
                raise Undefined("append_range_within range")
            if op.endswith("_unsafe") and l.cap < len(it) + cnt:
                raise Undefined("append_range_within_unsafe without capacity")
            return LList(it + it[st:st + cnt], l.cap)
        if op == "list_copy_range_within":
            l, src, dst, cnt = args
            it = list(_items(l))
            if src + cnt > len(it) or dst + cnt > len(it):
                raise Undefined("copy_range_within range")
            seg = it[src:src + cnt]
            it[dst:dst + cnt] = seg
            return LList(it, l.cap)
        if op == "list_append_sublist":
            l, srcl, st, cnt = args
            src = _items(srcl)
            if st + cnt > len(src):
                raise Undefined("append_sublist range")
            return LList(_items(l) + src[st:st + cnt], l.cap)
        if op in ("list_append_le_bytes", "list_append_le_bytes_unsafe"):
            l, val, n = args
            it = _items(l)
            if op.endswith("_unsafe") and l.cap < len(it) + n:
                raise Undefined("append_le_bytes_unsafe without capacity")
            return LList(it + list((val & ((1 << (8 * n)) - 1)).to_bytes(n, "little")), l.cap)
        if op in ("list_map_prepare_reuse", "list_map_cast_unsafe"):
            _items(args[0])
            return args[0]
        if op == "list_map_can_reuse":
            _items(args[0])
            return _bool(not args[0].shared)
        if op == "list_map_write_unsafe":
            l, i, v = args[0], args[1], args[2]
            it = list(_items(l))
            if not 0 <= i < len(it):
                raise Undefined("map_write range")
            it[i] = v
            return LList(it, l.cap)
        if op in ("list_move_out_unsafe", "list_map_extract_unsafe"):
            l, i = args[0], args[1]
            it = _items(l)
            if not 0 <= i < len(it):
                raise Undefined("move_out range")
            return it[i]
        # boxes and pointers
        if op == "box_box":
            return Cell(args[0])
        if op in ("box_unbox", "box_unbox_borrowed"):
            c = args[0]
            if not isinstance(c, Cell):
                raise Undefined("unbox non-box")
            if c.val is ZERO:
                raise Undefined("unbox zero cell")
            return c.val
        if op in ("box_alloc_zeroed", "ptr_alloca"):
            return Cell(ZERO)
        if op == "ptr_store":
            c, v = args
            if not isinstance(c, Cell):
                raise Undefined("ptr_store non-pointer")
            c.val = v
            return ()
        if op == "ptr_load":
            c = args[0]
            if not isinstance(c, Cell) or c.val is ZERO:
                raise Undefined("ptr_load")
            return c.val
        if op == "ptr_cast":
            if not isinstance(args[0], Cell):
                raise Undefined("ptr_cast non-pointer")
            return args[0]
        if op == "crash":
            raise Crash(b"Roc crashed")
        raise Unsupported("low-level " + op)


def _bytes(v):
    if isinstance(v, StaticAtom):
        raise Unsupported("opaque static data")
    if not isinstance(v, bytes):
        raise Undefined("expected str, got %r" % (type(v).__name__,))
    return v


_CONV = {}


def _conversion(op):
    if op in _CONV:
        return _CONV[op]
    parts = op.split("_to_")
    res = None
    if len(parts) == 2:
        src = parts[0]
        rest = parts[1].split("_")
        dst = rest[0]
        mode = "_".join(rest[1:])
        if src in list(INT_BITS) + ["f32", "f64", "dec"] and dst in list(INT_BITS) + ["f32", "f64", "dec"]:
            res = (src, dst, mode)
    _CONV[op] = res
    return res


def str_match(text, prefix, end, steps):
    """Ordered string pattern per docs/langref/pattern-matching.md: a capture stops at the first
    place the following text appears. Returns [(local, bytes)] or None."""
    text = _bytes(text)
    if not text.startswith(prefix):
        return None
    cur = len(prefix)
    caps = []
    for i, (cap, delim) in enumerate(steps):
        final_tail = end == "tail" and i + 1 == len(steps) and delim == b""
        if final_tail:
            stop, nxt = len(text), len(text)
        else:
            f = text.find(delim, cur)
            if f < 0:
                return None
            stop, nxt = f, f + len(delim)
        if cap is not None:
            caps.append((cap, text[cur:stop]))
        cur = nxt
    if end == "exact" and cur != len(text):
        return None
    if cur > len(text):
        return None
    return caps
