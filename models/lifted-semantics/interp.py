"""A reference interpreter for Monotype Lifted IR (RULES.md, "Semantics").

`Machine(program).run_root(fn_id, args)` runs one function and returns an `Outcome`: how it ended
(a value, a crash, or a model limit) and the effect trace it produced (hosted calls, `dbg`, failed
`expect`s). Values are plain Python data, rendered canonically by `render` so two programs can be
compared without regard to function ids or capture ids.

The interpreter also records, for every `call_value` it executes, the expression id and the
function the callee closure named (`Machine.sites`), and for every closure it builds or calls,
whether every capture slot of the function was supplied (`Machine.violations`).
"""
import struct
import sys

sys.setrecursionlimit(1_000_000)

DEC_ONE = 10 ** 18

INT_RANGES = {
    "u8": (0, 2 ** 8 - 1), "i8": (-2 ** 7, 2 ** 7 - 1),
    "u16": (0, 2 ** 16 - 1), "i16": (-2 ** 15, 2 ** 15 - 1),
    "u32": (0, 2 ** 32 - 1), "i32": (-2 ** 31, 2 ** 31 - 1),
    "u64": (0, 2 ** 64 - 1), "i64": (-2 ** 63, 2 ** 63 - 1),
    "u128": (0, 2 ** 128 - 1), "i128": (-2 ** 127, 2 ** 127 - 1),
    "dec": (-2 ** 127, 2 ** 127 - 1),
}


# ---------------------------------------------------------------------------- values

class Unit:
    def __repr__(self):
        return "{}"


UNIT = Unit()


class Uninit:
    def __repr__(self):
        return "<uninit>"


UNINIT = Uninit()


class Pending:
    """The value of a recursive binding while it is being built."""

    def __repr__(self):
        return "<pending>"


class Tag:
    __slots__ = ("name", "args")

    def __init__(self, name, args):
        self.name = name
        self.args = tuple(args)


class Rec:
    __slots__ = ("fields",)

    def __init__(self, fields):
        self.fields = dict(fields)


class Tup:
    __slots__ = ("items",)

    def __init__(self, items):
        self.items = tuple(items)


class Lst:
    __slots__ = ("items",)

    def __init__(self, items):
        self.items = tuple(items)


class Box:
    __slots__ = ("value",)

    def __init__(self, value):
        self.value = value


class Clo:
    __slots__ = ("fn", "caps")

    def __init__(self, fn, caps):
        self.fn = fn
        self.caps = caps


class Flt:
    """A float, kept apart from integers so equal-looking values of different types never compare equal."""
    __slots__ = ("v",)

    def __init__(self, v):
        self.v = v


def render(v):
    if v is True:
        return "True"
    if v is False:
        return "False"
    if isinstance(v, int):
        return str(v)
    if isinstance(v, Flt):
        return repr(v.v)
    if isinstance(v, bytes):
        return repr(v)
    if isinstance(v, Unit):
        return "{}"
    if isinstance(v, Uninit):
        return "<uninit>"
    if isinstance(v, Tag):
        if not v.args:
            return v.name
        return v.name + "(" + ", ".join(render(a) for a in v.args) + ")"
    if isinstance(v, Rec):
        return "{" + ", ".join(k + ": " + render(v.fields[k]) for k in sorted(v.fields)) + "}"
    if isinstance(v, Tup):
        return "(" + ", ".join(render(a) for a in v.items) + ",)"
    if isinstance(v, Lst):
        return "[" + ", ".join(render(a) for a in v.items) + "]"
    if isinstance(v, Box):
        return "Box(" + render(v.value) + ")"
    if isinstance(v, Clo):
        return "<fn>"
    return "<?%r>" % (v,)


def dbg_text(v):
    """What `dbg` shows: Monotype renders the value to a `Str` first, so this is usually that string."""
    if isinstance(v, bytes):
        return v.decode("utf-8", "replace")
    return render(v)


def equal(a, b):
    if isinstance(a, bool) or isinstance(b, bool):
        return truth(a) == truth(b)
    if isinstance(a, Tag) and isinstance(b, Tag):
        return a.name == b.name and len(a.args) == len(b.args) and all(equal(x, y) for x, y in zip(a.args, b.args))
    if isinstance(a, Rec) and isinstance(b, Rec):
        return a.fields.keys() == b.fields.keys() and all(equal(a.fields[k], b.fields[k]) for k in a.fields)
    if isinstance(a, (Tup, Lst)) and isinstance(b, (Tup, Lst)):
        return len(a.items) == len(b.items) and all(equal(x, y) for x, y in zip(a.items, b.items))
    if isinstance(a, Box) and isinstance(b, Box):
        return equal(a.value, b.value)
    if isinstance(a, Flt) and isinstance(b, Flt):
        return a.v == b.v
    if isinstance(a, Unit) and isinstance(b, Unit):
        return True
    if isinstance(a, Clo) or isinstance(b, Clo):
        raise ModelLimit("structural equality on a function value")
    return a == b


def truth(v):
    if v is True or v is False:
        return v
    if isinstance(v, Tag) and v.name in ("True", "False") and not v.args:
        return v.name == "True"
    if isinstance(v, int):
        return v != 0
    raise ModelLimit("not a Bool: " + render(v))


# ---------------------------------------------------------------------------- control

class Crash(Exception):
    """The program crashed: a `crash`, a failed pattern, an overflow, an out-of-range read."""


class Violation(Exception):
    """The program broke a rule of Lifted IR itself (RULES.md): reached `unreachable`, called a
    closure whose captures are not bound, read an unbound local, jumped to a join point not in scope."""


class ModelLimit(Exception):
    """The program uses something this interpreter does not model, or ran out of fuel."""


class BreakEx(Exception):
    def __init__(self, value):
        self.value = value


class ContinueEx(Exception):
    def __init__(self, values):
        self.values = values


class JumpEx(Exception):
    def __init__(self, jid, args):
        self.jid = jid
        self.args = args


class ReturnEx(Exception):
    def __init__(self, value):
        self.value = value


class Outcome:
    def __init__(self, kind, detail, trace):
        self.kind = kind  # value | crash | violation | limit
        self.detail = detail
        self.trace = trace

    def key(self):
        return (self.kind, self.detail, tuple(self.trace))

    def __str__(self):
        t = "; ".join(self.trace[:12]) + (" ..." if len(self.trace) > 12 else "")
        return "%s %s [%s]" % (self.kind, self.detail, t)


# ---------------------------------------------------------------------------- machine

class Machine:
    def __init__(self, prog, fuel=2_000_000):
        self.p = prog
        self.fuel = fuel
        self.trace = []
        self.sites = {}  # call_value expr id -> set of fn ids seen
        self.violations = []
        self.depth = 0

    # -- types
    def resolve(self, ty, seen=None):
        t = self.p.types[ty]
        n = 0
        while t[0] == "named":
            if t[5] is None:
                return t
            t = self.p.types[t[5]]
            n += 1
            if n > 1000:
                raise ModelLimit("named type with no structural backing")
        return t

    def prim(self, ty):
        t = self.resolve(ty)
        if t[0] == "prim":
            return t[1]
        return None

    def is_bool(self, ty):
        t = self.p.types[ty]
        if t[0] == "named" and t[3] == "Bool":
            return True
        t = self.resolve(ty)
        if t[0] == "prim" and t[1] == "bool":
            return True
        if t[0] == "tags" and [n for n, _ in t[1]] == ["False", "True"] and all(not p for _, p in t[1]):
            return True
        return False

    def default(self, ty, depth=0):
        """A deterministic value of a type, for a hosted function's result."""
        if depth > 20:
            raise ModelLimit("default value of a deep type")
        t = self.resolve(ty)
        k = t[0]
        if k == "prim":
            if t[1] == "bool":
                return False
            if t[1] == "str":
                return b""
            if t[1] in ("f32", "f64"):
                return Flt(0.0)
            if t[1] in INT_RANGES:
                return 0
            raise ModelLimit("default of " + t[1])
        if k == "zst":
            return UNIT
        if k == "list":
            return Lst(())
        if k == "box":
            return Box(self.default(t[1], depth + 1))
        if k == "tuple":
            return Tup(self.default(x, depth + 1) for x in t[1])
        if k == "record":
            return Rec((n, self.default(x, depth + 1)) for n, x in t[1])
        if k == "tags":
            if self.is_bool(ty):
                return False
            if not t[1]:
                raise ModelLimit("default of an empty tag union")
            names = [n for n, _ in t[1]]
            n, ps = t[1][names.index("Ok")] if "Ok" in names else t[1][0]
            return Tag(n, [self.default(x, depth + 1) for x in ps])
        raise ModelLimit("default of " + k)

    # -- entry
    def run_root(self, fn, args):
        self.trace = []
        try:
            v = self.call(fn, args, {})
            return Outcome("value", render(v), self.trace)
        except Crash as e:
            return Outcome("crash", str(e), self.trace)
        except Violation as e:
            return Outcome("violation", str(e), self.trace)
        except ModelLimit as e:
            return Outcome("limit", str(e), self.trace)
        except RecursionError:
            return Outcome("limit", "recursion depth", self.trace)
        except (BreakEx, ContinueEx, JumpEx, ReturnEx) as e:
            return Outcome("violation", "control left its function: " + type(e).__name__, self.trace)

    def tick(self):
        self.fuel -= 1
        if self.fuel <= 0:
            raise ModelLimit("out of fuel")

    def call(self, fid, args, caps):
        self.tick()
        p = self.p
        if fid is None or fid >= len(p.fns):
            raise Violation("call of unknown function %r" % (fid,))
        fargs, fcaps, ret, body = p.fns[fid]
        if len(args) != len(fargs):
            raise Violation("function %d takes %d arguments, given %d" % (fid, len(fargs), len(args)))
        if body is None:
            self.trace.append("hosted %d(%s)" % (fid, ", ".join(render(a) for a in args)))
            return self.default(ret)
        frame = {}
        for (loc, _), v in zip(fargs, args):
            frame[loc] = v
        for loc, _ in fcaps:
            cid = p.locals[loc][1]
            if cid not in caps:
                self.violations.append("function %d: capture slot %r (local %d) not supplied" % (fid, cid, loc))
                raise Violation("capture %r of function %d is not bound" % (cid, fid))
            frame[loc] = caps[cid]
        self.depth += 1
        if self.depth > 20000:
            raise ModelLimit("call depth")
        try:
            return self.ev(body, frame)
        except ReturnEx as r:
            return r.value
        finally:
            self.depth -= 1

    def caps(self, spans, frame):
        return {cid: self.ev(e, frame) for cid, e in spans}

    # -- expressions
    def ev(self, eid, frame):
        self.tick()
        ty, k, o = self.p.exprs[eid]
        m = getattr(self, "e_" + k, None)
        if m is None:
            raise ModelLimit("expression kind " + k)
        return m(eid, ty, o, frame)

    def e_local(self, eid, ty, o, frame):
        if o[0] not in frame:
            raise Violation("read of unbound local %d" % o[0])
        return frame[o[0]]

    def e_unit(self, eid, ty, o, frame):
        return UNIT

    def e_unreachable(self, eid, ty, o, frame):
        raise Violation("reached an unreachable marker (expr %d)" % eid)

    def e_uninitialized(self, eid, ty, o, frame):
        return UNINIT

    def e_uninitialized_payload(self, eid, ty, o, frame):
        return UNINIT

    def e_crash(self, eid, ty, o, frame):
        raise Crash(o[0].decode("utf-8", "replace"))

    def e_checked_error(self, eid, ty, o, frame):
        raise Crash("checked error: " + o[0].decode("utf-8", "replace"))

    def e_comptime_exhaustiveness_failed(self, eid, ty, o, frame):
        raise Crash("non-exhaustive at compile time")

    def e_int_lit(self, eid, ty, o, frame):
        if self.is_bool(ty):
            return o[0] != 0
        r = self.resolve(ty)
        if r[0] == "tags":
            # A payload-free tag written as its discriminant: the index of its label.
            if all(not ps for _, ps in r[1]) and 0 <= o[0] < len(r[1]):
                return Tag(r[1][o[0]][0], [])
            raise ModelLimit("integer literal at a tag union with payloads")
        pr = self.prim(ty)
        if pr in ("f32", "f64"):
            return Flt(float(o[0]))
        if pr == "dec":
            return o[0] * DEC_ONE
        return o[0]

    def e_frac_f32_lit(self, eid, ty, o, frame):
        return Flt(struct.unpack("<f", struct.pack("<I", o[0]))[0])

    def e_frac_f64_lit(self, eid, ty, o, frame):
        return Flt(struct.unpack("<d", struct.pack("<Q", o[0]))[0])

    def e_dec_lit(self, eid, ty, o, frame):
        return o[0]

    def e_str_lit(self, eid, ty, o, frame):
        return o[0]

    def e_bytes_lit(self, eid, ty, o, frame):
        data, n, elem, width = o
        if elem is None:
            return self.packed_product(ty, data, n, width)
        if elem == "u8" and len(data) == n:
            return Lst(data)
        sizes = {"i8": 1, "u8": 1, "i16": 2, "u16": 2, "i32": 4, "u32": 4, "i64": 8, "u64": 8, "i128": 16, "u128": 16,
                 "dec": 16}
        if elem in sizes and width == 0 and len(data) == n * sizes[elem]:
            w = sizes[elem]
            signed = elem[0] in "id"
            return Lst(int.from_bytes(data[i * w:(i + 1) * w], "little", signed=signed) for i in range(n))
        if elem in ("f32", "f64") and width == 0:
            w, fmt = (4, "<f") if elem == "f32" else (8, "<d")
            if len(data) == n * w:
                return Lst(Flt(struct.unpack(fmt, data[i * w:(i + 1) * w])[0]) for i in range(n))
        raise ModelLimit("packed list literal of %s" % elem)

    def packed_product(self, ty, data, n, width):
        """A packed list whose items are not one scalar kind: decoded by the item type. The model
        reads only items that are one unsigned byte or discriminant (a `U8`, a `Bool`, a payload-free
        tag as the index of its label) and zero-sized items."""
        lt = self.resolve(ty)
        if lt[0] != "list":
            raise ModelLimit("packed list literal at a non-list type")
        et = lt[1]
        r = self.resolve(et)
        if width == 0 and not data:
            if r[0] == "tags" and len(r[1]) == 1 and not r[1][0][1]:
                return Lst(Tag(r[1][0][0], []) for _ in range(n))
            if r[0] == "zst" or (r[0] in ("record", "tuple") and not r[1]):
                return Lst(UNIT for _ in range(n))
        if width in (1, 2) and len(data) == n * width:
            items = [int.from_bytes(data[i * width:(i + 1) * width], "little") for i in range(n)]
            if self.is_bool(et):
                return Lst(x != 0 for x in items)
            if r[0] == "tags" and all(not ps for _, ps in r[1]) and all(j < len(r[1]) for j in items):
                return Lst(Tag(r[1][j][0], []) for j in items)
            if r[0] == "prim" and r[1] in ("u8", "u16"):
                return Lst(items)
        raise ModelLimit("packed product list literal (width %d)" % width)

    def e_static_data_candidate(self, eid, ty, o, frame):
        if o[0] is None:
            raise ModelLimit("stored static list (value not exported)")
        return self.ev(o[0], frame)

    def e_comptime_value(self, eid, ty, o, frame):
        return self.ev(o[0], frame)

    def e_typed_boundary(self, eid, ty, o, frame):
        return self.ev(o[0], frame)

    e_row_widen = e_typed_boundary
    e_nominal = e_typed_boundary

    def e_dbg(self, eid, ty, o, frame):
        v = self.ev(o[0], frame)
        self.trace.append("dbg " + dbg_text(v))
        return v

    def e_expect(self, eid, ty, o, frame):
        if not truth(self.ev(o[0], frame)):
            self.trace.append("expect failed")
        return UNIT

    def e_expect_err(self, eid, ty, o, frame):
        self.ev(o[0], frame)
        raise Crash("expect_err")

    def e_literal_rejected(self, eid, ty, o, frame):
        self.ev(o[0], frame)
        raise Crash("literal rejected")

    def e_list(self, eid, ty, o, frame):
        return Lst(self.ev(e, frame) for e in o[0])

    def e_tuple(self, eid, ty, o, frame):
        return Tup(self.ev(e, frame) for e in o[0])

    def e_record(self, eid, ty, o, frame):
        return Rec((n, self.ev(e, frame)) for n, e in o[0])

    def e_record_update(self, eid, ty, o, frame):
        base = self.ev(o[0], frame)
        upd = [(n, self.ev(e, frame)) for n, e in o[1]]
        if not isinstance(base, Rec):
            raise ModelLimit("record update of " + render(base))
        f = dict(base.fields)
        f.update(upd)
        return Rec(f)

    def e_tag(self, eid, ty, o, frame):
        args = [self.ev(e, frame) for e in o[1]]
        if not args and o[0] in ("True", "False") and self.is_bool(ty):
            return o[0] == "True"
        return Tag(o[0], args)

    def e_fn_ref(self, eid, ty, o, frame):
        return Clo(o[0], self.caps(o[1], frame))

    def e_call_proc(self, eid, ty, o, frame):
        fid, shaped, args, caps = o
        if fid is None:
            raise Violation("call of a pre-lift template")
        av = [self.ev(e, frame) for e in args]
        cv = self.caps(caps, frame)
        if shaped:
            av = [self.shape(v, t) for v, (_, t) in zip(av, self.p.fns[fid][0])]
            r = self.call(fid, av, cv)
            return self.unshape(r, ty)
        return self.call(fid, av, cv)

    def e_call_value(self, eid, ty, o, frame):
        callee = self.ev(o[0], frame)
        av = [self.ev(e, frame) for e in o[1]]
        if not isinstance(callee, Clo):
            raise Violation("call of a non-function " + render(callee))
        self.sites.setdefault(eid, set()).add(callee.fn)
        return self.call(callee.fn, av, callee.caps)

    # Shaped calls pass records as tuples in row order (FORMAT.md). The interpreter's values are
    # structural, so only records change.
    def shape(self, v, ty):
        t = self.resolve(ty)
        if isinstance(v, Rec) and t[0] == "tuple":
            names = sorted(v.fields)
            return Tup(self.shape(v.fields[n], x) for n, x in zip(names, t[1]))
        return v

    def unshape(self, v, ty):
        t = self.resolve(ty)
        if isinstance(v, Tup) and t[0] == "record":
            return Rec((n, self.unshape(x, nt)) for (n, nt), x in zip(t[1], v.items))
        return v

    def e_low_level(self, eid, ty, o, frame):
        args = [self.ev(e, frame) for e in o[1]]
        return self.lowlevel(o[0], args, ty, o[1])

    def e_field_access(self, eid, ty, o, frame):
        v = self.ev(o[0], frame)
        for n in o[1]:
            if isinstance(v, Rec) and n in v.fields:
                v = v.fields[n]
            else:
                raise Violation("field %s of %s" % (n, render(v)))
        return v

    def e_tuple_access(self, eid, ty, o, frame):
        v = self.ev(o[0], frame)
        if isinstance(v, Tup) and o[1] < len(v.items):
            return v.items[o[1]]
        if isinstance(v, Rec):
            names = sorted(v.fields)
            if o[1] < len(names):
                return v.fields[names[o[1]]]
        raise Violation("item %d of %s" % (o[1], render(v)))

    def e_structural_eq(self, eid, ty, o, frame):
        a = self.ev(o[0], frame)
        b = self.ev(o[1], frame)
        r = equal(a, b)
        return (not r) if o[2] else r

    def e_structural_hash(self, eid, ty, o, frame):
        v = self.ev(o[0], frame)
        h = self.ev(o[1], frame)
        return self.hash_into(h, render(v).encode())

    def e_match_(self, eid, ty, o, frame):
        v = self.ev(o[0], frame)
        for pat, stmts, guard, body in o[1]:
            saved = dict(frame)
            if self.match(pat, v, frame):
                for s in stmts:
                    self.stmt(s, frame)
                if guard is None or truth(self.ev(guard, frame)):
                    return self.ev(body, frame)
            frame.clear()
            frame.update(saved)
        raise Crash("no branch matched " + render(v))

    def e_if_(self, eid, ty, o, frame):
        for c, b in o[0]:
            if truth(self.ev(c, frame)):
                return self.ev(b, frame)
        return self.ev(o[1], frame)

    def e_if_initialized_payload(self, eid, ty, o, frame):
        cond, mask, payload, init, uninit = o
        self.ev(cond, frame)
        if payload in frame and frame[payload] is not UNINIT:
            return self.ev(init, frame)
        return self.ev(uninit, frame)

    def try_split(self, v):
        if isinstance(v, Tag) and v.name in ("Ok", "Err") and len(v.args) == 1:
            return v.name == "Ok", v.args[0]
        raise Violation("try sequence over " + render(v))

    def e_try_sequence(self, eid, ty, o, frame):
        inp, ok_local, err, body = o
        ok, payload = self.try_split(self.ev(inp, frame))
        if ok:
            frame[ok_local] = payload
            return self.ev(body, frame)
        if err is not None:
            raise JumpEx(err, [payload])
        return Tag("Err", [payload])

    def e_try_record_sequence(self, eid, ty, o, frame):
        inp, vlocal, vfield, rlocal, rfield, err, body = o
        ok, payload = self.try_split(self.ev(inp, frame))
        if ok:
            if not isinstance(payload, Rec):
                raise Violation("try record sequence over " + render(payload))
            frame[vlocal] = payload.fields[vfield]
            frame[rlocal] = payload.fields[rfield]
            return self.ev(body, frame)
        if err is not None:
            raise JumpEx(err, [payload])
        return Tag("Err", [payload])

    def e_block(self, eid, ty, o, frame):
        for s in o[0]:
            self.stmt(s, frame)
        return self.ev(o[1], frame)

    def e_loop_(self, eid, ty, o, frame):
        params, init, body = o
        vals = [self.ev(e, frame) for e in init]
        for (loc, _), v in zip(params, vals):
            frame[loc] = v
        while True:
            self.tick()
            try:
                # The body is typed at the loop's result (Lambda Solved `loop_`), so a body that
                # ends without `break` or `continue` ends the loop with its value.
                return self.ev(body, frame)
            except BreakEx as b:
                return b.value
            except ContinueEx as c:
                if len(c.values) != len(params):
                    raise Violation("continue with %d values for %d parameters" % (len(c.values), len(params)))
                for (loc, _), v in zip(params, c.values):
                    frame[loc] = v

    def e_break_(self, eid, ty, o, frame):
        raise BreakEx(UNIT if o[0] is None else self.ev(o[0], frame))

    def e_continue_(self, eid, ty, o, frame):
        raise ContinueEx([self.ev(e, frame) for e in o[0]])

    def e_join_point(self, eid, ty, o, frame):
        jid, params, body, rem = o
        target = rem
        while True:
            try:
                return self.ev(target, frame)
            except JumpEx as j:
                if j.jid != jid:
                    raise
                if len(j.args) != len(params):
                    raise Violation("jump with %d arguments to a join with %d parameters" % (len(j.args), len(params)))
                for (loc, _), v in zip(params, j.args):
                    frame[loc] = v
                target = body

    def e_jump(self, eid, ty, o, frame):
        jid, args, lps, lvs = o
        av = [self.ev(e, frame) for e in args]
        lv = [self.ev(e, frame) for e in lvs]
        for (loc, _), v in zip(lps, lv):
            frame[loc] = v
        raise JumpEx(jid, av)

    def e_let_(self, eid, ty, o, frame):
        v = self.ev(o[1], frame)
        if not self.match(o[0], v, frame):
            raise Crash("let pattern did not match " + render(v))
        return self.ev(o[2], frame)

    def e_return_(self, eid, ty, o, frame):
        raise ReturnEx(self.ev(o[0], frame))

    # -- statements
    def stmt(self, sid, frame):
        self.tick()
        k, o = self.p.stmts[sid]
        if k == "let_" and o[2]:
            self.recursive_let(o[0], o[1], frame)
        elif k == "let_":
            v = self.ev(o[1], frame)
            if not self.match(o[0], v, frame):
                raise Crash("let pattern did not match " + render(v))
        elif k == "uninitialized":
            self.bind_uninit(o[0], frame)
        elif k == "expr":
            self.ev(o[0], frame)
        elif k == "expect":
            if not truth(self.ev(o[0], frame)):
                self.trace.append("expect failed")
        elif k == "dbg":
            self.trace.append("dbg " + dbg_text(self.ev(o[0], frame)))
        elif k == "return_":
            raise ReturnEx(self.ev(o[0], frame))
        elif k == "crash":
            raise Crash(o[0].decode("utf-8", "replace"))
        elif k == "checked_error":
            raise Crash("checked error: " + o[0].decode("utf-8", "replace"))
        else:
            raise ModelLimit("statement kind " + k)

    def recursive_let(self, pid, eid, frame):
        """A recursive binding: closures in its value capture the value itself. The bound local
        holds a placeholder while the value is built, and every capture of the placeholder is then
        pointed at the finished value."""
        ty, k, o = self.p.pats[pid]
        if k != "bind":
            raise ModelLimit("recursive let of a %s pattern" % k)
        hole = Pending()
        frame[o[0]] = hole
        v = self.ev(eid, frame)
        seen = set()

        def tie(x):
            if id(x) in seen:
                return
            seen.add(id(x))
            if isinstance(x, Clo):
                for cid, c in list(x.caps.items()):
                    if c is hole:
                        x.caps[cid] = v
                    else:
                        tie(c)
            elif isinstance(x, (Tup, Lst)):
                for c in x.items:
                    tie(c)
            elif isinstance(x, Tag):
                for c in x.args:
                    tie(c)
            elif isinstance(x, Rec):
                for c in x.fields.values():
                    tie(c)
            elif isinstance(x, Box):
                tie(x.value)
        tie(v)
        frame[o[0]] = v

    def bind_uninit(self, pid, frame):
        ty, k, o = self.p.pats[pid]
        if k == "bind":
            frame[o[0]] = UNINIT
        elif k == "as":
            frame[o[1]] = UNINIT
            self.bind_uninit(o[0], frame)
        elif k == "wildcard":
            pass
        else:
            raise ModelLimit("uninitialized pattern " + k)

    # -- patterns
    def match(self, pid, v, frame):
        ty, k, o = self.p.pats[pid]
        if k == "bind":
            frame[o[0]] = v
            return True
        if k == "wildcard":
            return True
        if k == "as":
            if not self.match(o[0], v, frame):
                return False
            frame[o[1]] = v
            return True
        if k == "nominal":
            return self.match(o[0], v, frame)
        if k == "int_lit":
            if isinstance(v, bool):
                return int(v) == o[0]
            pr = self.prim(ty)
            if pr == "dec":
                return v == o[0] * DEC_ONE
            if isinstance(v, Flt):
                return v.v == o[0]
            return v == o[0]
        if k == "dec_lit":
            return v == o[0]
        if k in ("frac_f32_lit", "frac_f64_lit"):
            fmt = ("<f", "<I") if k == "frac_f32_lit" else ("<d", "<Q")
            return isinstance(v, Flt) and v.v == struct.unpack(fmt[0], struct.pack(fmt[1], o[0]))[0]
        if k == "str_lit":
            return v == o[0]
        if k == "tag":
            name, subs = o
            if isinstance(v, bool):
                return (name == "True") == v and not subs
            if not isinstance(v, Tag):
                raise Violation("tag pattern %s against %s" % (name, render(v)))
            if v.name != name:
                return False
            if len(v.args) != len(subs):
                raise Violation("tag %s with %d payloads against a pattern of %d" % (name, len(v.args), len(subs)))
            return all(self.match(sp, a, frame) for sp, a in zip(subs, v.args))
        if k == "tuple":
            items = v.items if isinstance(v, Tup) else None
            if items is None or len(items) != len(o[0]):
                raise Violation("tuple pattern against " + render(v))
            return all(self.match(sp, a, frame) for sp, a in zip(o[0], items))
        if k == "record":
            if not isinstance(v, Rec):
                if isinstance(v, Tup):
                    names = sorted(n for n, _ in o[0])
                    vals = dict(zip(names, v.items))
                    return all(self.match(sp, vals[n], frame) for n, sp in o[0])
                raise Violation("record pattern against " + render(v))
            return all(self.match(sp, v.fields[n], frame) for n, sp in o[0])
        if k == "list":
            if not isinstance(v, Lst):
                raise Violation("list pattern against " + render(v))
            subs, rest_index, rest_pat = o
            items = v.items
            if rest_index is None:
                if len(items) != len(subs):
                    return False
                return all(self.match(sp, a, frame) for sp, a in zip(subs, items))
            if len(items) < len(subs):
                return False
            front = subs[:rest_index]
            back = subs[rest_index:]
            for sp, a in zip(front, items):
                if not self.match(sp, a, frame):
                    return False
            for sp, a in zip(back, items[len(items) - len(back):]):
                if not self.match(sp, a, frame):
                    return False
            if rest_pat is not None:
                return self.match(rest_pat, Lst(items[len(front):len(items) - len(back)]), frame)
            return True
        if k == "str_pattern":
            prefix, steps, end = o
            if not isinstance(v, bytes) or not v.startswith(prefix):
                return False
            pos = len(prefix)
            for cap, delim in steps:
                if delim:
                    i = v.find(delim, pos)
                    if i < 0:
                        return False
                    piece, pos = v[pos:i], i + len(delim)
                else:
                    piece, pos = v[pos:], len(v)
                if cap is not None and not self.match(cap, piece, frame):
                    return False
            return end != "exact" or pos == len(v)
        raise ModelLimit("pattern kind " + k)

    # -- low-level operations
    def hash_into(self, h, data):
        if not isinstance(h, int):
            raise ModelLimit("hasher of shape " + render(h))
        x = h
        for b in data:
            x = ((x ^ b) * 0x100000001B3) % (2 ** 64)
        return x

    def lowlevel(self, op, a, ty, arg_exprs):
        p = self.prim(ty)
        aty = [self.p.exprs[e][0] for e in arg_exprs]
        ap = [self.prim(t) for t in aty]
        if op in ("num_plus", "num_minus", "num_times", "num_int_add_crash_on_overflow", "num_int_sub_crash_on_overflow",
                  "num_int_mul_crash_on_overflow", "num_int_add_proven_cannot_overflow",
                  "num_int_sub_proven_cannot_overflow", "num_int_mul_proven_cannot_overflow"):
            kind = "add" if ("plus" in op or "add" in op) else "sub" if ("minus" in op or "sub" in op) else "mul"
            return self.arith(kind, a[0], a[1], p, crash=True)
        if op in ("num_int_add_wrap", "num_int_sub_wrap", "num_int_mul_wrap"):
            kind = op.split("_")[2]
            return self.arith(kind, a[0], a[1], p, crash=False)
        if op in ("num_float_add", "num_float_sub", "num_float_mul"):
            kind = op.split("_")[2]
            return self.arith(kind, a[0], a[1], p, crash=True)
        if op in ("num_is_eq",):
            return equal(a[0], a[1])
        if op in ("num_is_gt", "num_is_gte", "num_is_lt", "num_is_lte"):
            x, y = self.num(a[0]), self.num(a[1])
            return {"num_is_gt": x > y, "num_is_gte": x >= y, "num_is_lt": x < y, "num_is_lte": x <= y}[op]
        if op == "compare":
            x, y = self.num(a[0]), self.num(a[1])
            return Tag("EQ" if x == y else "GT" if x > y else "LT", [])
        if op == "num_negate":
            if isinstance(a[0], Flt):
                return Flt(-a[0].v)
            return self.fit(-a[0], p, crash=True)
        if op == "num_abs":
            if isinstance(a[0], Flt):
                return Flt(abs(a[0].v))
            return self.fit(abs(a[0]), p, crash=True)
        if op in ("num_div_trunc_by", "num_div_by", "num_rem_by", "num_mod_by"):
            x, y = a
            if isinstance(x, Flt):
                if op == "num_div_by":
                    return Flt(x.v / y.v) if y.v != 0 else Flt(float("inf") if x.v > 0 else float("-inf") if x.v < 0 else float("nan"))
                raise ModelLimit(op + " on floats")
            if y == 0:
                raise Crash("integer division by zero")
            if p == "dec":
                if op in ("num_div_by", "num_div_trunc_by"):
                    q = abs(x) * DEC_ONE // abs(y)
                    q = q if (x >= 0) == (y >= 0) else -q
                    if op == "num_div_trunc_by":
                        q = (abs(q) // DEC_ONE) * DEC_ONE * (1 if q >= 0 else -1)
                    return self.fit(q, p, crash=True)
                raise ModelLimit(op + " on Dec")
            q = abs(x) // abs(y)
            q = q if (x >= 0) == (y >= 0) else -q
            if op in ("num_div_trunc_by", "num_div_by"):
                return self.fit(q, p, crash=True)
            r = x - q * y
            if op == "num_rem_by":
                return r
            return r + y if r != 0 and (r < 0) != (y < 0) else r
        if op == "bool_not":
            return not truth(a[0])
        if op == "bool_likely":
            return a[0]
        if op == "crash":
            raise Crash(a[0].decode("utf-8", "replace") if isinstance(a[0], bytes) else render(a[0]))
        if op in ("str_is_eq", "str_is_eq_static_small"):
            return a[0] == a[1]
        if op == "str_from_parts":
            return b"".join(a)
        if op in ("str_concat", "str_append"):
            return a[0] + a[1]
        if op == "str_count_utf8_bytes":
            return len(a[0])
        if op == "str_starts_with":
            return a[0].startswith(a[1])
        if op == "str_ends_with":
            return a[0].endswith(a[1])
        if op == "str_contains":
            return a[1] in a[0]
        if op == "str_repeat":
            return a[0] * a[1]
        if op in ("str_with_capacity",):
            return b""
        if op in ("str_reserve", "str_release_excess_capacity"):
            return a[0]
        if op == "str_to_utf8":
            return Lst(a[0])
        if op == "str_get_utf8_byte_unsafe":
            return a[0][a[1]]
        if op == "str_substring_unsafe":
            return a[0][a[1]:a[1] + a[2]]
        if op == "str_trim":
            return a[0].strip(b" \t\n\r")
        if op == "str_trim_start":
            return a[0].lstrip(b" \t\n\r")
        if op == "str_trim_end":
            return a[0].rstrip(b" \t\n\r")
        if op == "str_with_ascii_lowercased":
            return a[0].lower()
        if op == "str_with_ascii_uppercased":
            return a[0].upper()
        if op == "str_inspect":
            if isinstance(a[0], bytes):
                esc = {0x22: b'\\"', 0x5C: b"\\\\", 0x0A: b"\\n", 0x0D: b"\\r", 0x09: b"\\t"}
                return b'"' + b"".join(esc.get(c, bytes([c])) for c in a[0]) + b'"'
            raise ModelLimit("str_inspect of " + render(a[0]))
        if op.endswith("_to_str") and op.split("_")[0] in INT_RANGES:
            if op.startswith("dec"):
                return self.dec_str(a[0]).encode()
            return str(a[0]).encode()
        if op in ("f32_to_str", "f64_to_str"):
            return repr(a[0].v).encode()
        if op == "list_len":
            return len(a[0].items)
        if op == "list_capacity":
            return len(a[0].items)
        if op == "list_get_unsafe":
            i = a[1]
            if not 0 <= i < len(a[0].items):
                raise Crash("list index out of range")
            return a[0].items[i]
        if op == "list_prefetch":
            return UNIT
        if op in ("list_append_unsafe",):
            return Lst(a[0].items + (a[1],))
        if op == "list_prepend":
            return Lst((a[1],) + a[0].items)
        if op == "list_concat":
            return Lst(a[0].items + a[1].items)
        if op == "list_with_capacity":
            return Lst(())
        if op in ("list_reserve", "list_reserve_for_append", "list_release_excess_capacity"):
            return a[0]
        if op == "list_clear":
            return Lst(())
        if op == "list_reverse":
            return Lst(reversed(a[0].items))
        if op in ("list_sublist", "list_sublist_borrowed"):
            items = a[0].items
            if isinstance(a[1], Rec):
                start, n = a[1].fields.get("start"), a[1].fields.get("len")
            else:
                start, n = a[1], a[2]
            return Lst(items[start:start + n])
        if op == "list_drop_at":
            items = a[0].items
            i = a[1]
            return Lst(items[:i] + items[i + 1:]) if 0 <= i < len(items) else a[0]
        if op in ("list_set", "list_replace_unsafe", "list_set_in_place_unsafe"):
            items = list(a[0].items)
            i = a[1]
            if not 0 <= i < len(items):
                if op == "list_set":
                    return a[0]
                raise Crash("list index out of range")
            old = items[i]
            items[i] = a[2]
            if op == "list_replace_unsafe":
                return Rec([("list", Lst(items)), ("prev", old)])
            return Lst(items)
        if op == "list_swap":
            items = list(a[0].items)
            i, j = a[1], a[2]
            if 0 <= i < len(items) and 0 <= j < len(items):
                items[i], items[j] = items[j], items[i]
            return Lst(items)
        if op in ("list_slack_unique", "list_owned_unique", "list_map_can_reuse", "list_item_size_at_most"):
            # Ownership and layout questions: the model always answers no, which every caller
            # handles by copying; both programs of a pair get the same answer.
            return 0
        if op in ("list_map_prepare_reuse", "list_map_cast_unsafe", "list_prefetched"):
            return a[0]
        if op in ("list_map_extract_unsafe", "list_move_out_unsafe"):
            return a[0].items[a[1]]
        if op == "list_map_write_unsafe":
            items = list(a[0].items)
            items[a[1]] = a[2]
            return Lst(items)
        if op == "box_box":
            return Box(a[0])
        if op in ("box_unbox", "box_unbox_borrowed"):
            return a[0].value
        conv = self.conversion(op, a, p)
        if conv is not None:
            return conv
        if op in ("num_shift_left_by", "num_shift_right_by", "num_shift_right_zf_by", "num_bitwise_and",
                  "num_bitwise_or", "num_bitwise_xor"):
            x, y = a
            lo, hi = INT_RANGES[p]
            bits = hi.bit_length() + (1 if lo < 0 else 0)
            if op == "num_bitwise_and":
                return x & y
            if op == "num_bitwise_or":
                return x | y
            if op == "num_bitwise_xor":
                return x ^ y
            if op == "num_shift_left_by":
                return self.fit(x << y if y < bits else 0, p, crash=False)
            if op == "num_shift_right_by":
                return x >> min(y, bits)
            return (x % (2 ** bits)) >> y if y < bits else 0
        raise ModelLimit("low-level " + op)

    def conversion(self, op, a, p):
        parts = op.split("_")
        if len(parts) < 3 or parts[1] != "to" or parts[0] not in INT_RANGES or parts[2] not in INT_RANGES:
            return None
        src, dst = parts[0], parts[2]
        mode = parts[3] if len(parts) > 3 else ""
        if src == "dec" or dst == "dec" or src.startswith("f") or dst.startswith("f"):
            if dst == "dec" and src != "dec":
                return a[0] * DEC_ONE
            return None
        x = a[0]
        lo, hi = INT_RANGES[dst]
        if mode == "wrap" or mode == "":
            return self.fit(x, dst, crash=False)
        if mode == "try":
            if lo <= x <= hi:
                return Tag("Ok", [x])
            return Tag("Err", [Tag("OutOfRange", [])])
        return None

    def num(self, v):
        if isinstance(v, Flt):
            return v.v
        if isinstance(v, bool):
            return int(v)
        return v

    def dec_str(self, v):
        sign = "-" if v < 0 else ""
        v = abs(v)
        whole, frac = divmod(v, DEC_ONE)
        if frac == 0:
            return sign + str(whole) + ".0"
        return sign + str(whole) + "." + ("%018d" % frac).rstrip("0")

    def fit(self, x, p, crash):
        if p not in INT_RANGES:
            raise ModelLimit("integer op at type %s" % p)
        lo, hi = INT_RANGES[p]
        if lo <= x <= hi:
            return x
        if crash:
            raise Crash("integer overflow")
        span = hi - lo + 1
        return (x - lo) % span + lo

    def arith(self, kind, x, y, p, crash):
        if isinstance(x, Flt):
            r = {"add": x.v + y.v, "sub": x.v - y.v, "mul": x.v * y.v}[kind]
            if p == "f32":
                r = struct.unpack("<f", struct.pack("<f", r))[0] if abs(r) < 3.5e38 else r
            return Flt(r)
        if p == "dec":
            if kind == "mul":
                r = abs(x) * abs(y) // DEC_ONE
                r = r if (x >= 0) == (y >= 0) else -r
            else:
                r = x + y if kind == "add" else x - y
            return self.fit(r, p, crash)
        r = x + y if kind == "add" else x - y if kind == "sub" else x * y
        return self.fit(r, p, crash)
