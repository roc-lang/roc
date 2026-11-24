"""Reader for the Lifted IR export, format version 3 (FORMAT.md).

`parse(text)` returns a `Program` whose tables are indexed by id. Every record keeps its operands as
plain Python values: ids are ints, absent optionals are None, names are decoded `str`s (string literal
bytes are kept as `bytes`).
"""

VERSION = 3


class FormatError(Exception):
    pass


def unescape(tok):
    if tok == "%":
        return b""
    out = bytearray()
    i = 0
    raw = tok.encode("ascii")
    while i < len(raw):
        c = raw[i]
        if c == 0x25:
            out.append(int(raw[i + 1:i + 3], 16))
            i += 3
        else:
            out.append(c)
            i += 1
    return bytes(out)


class Cursor:
    def __init__(self, toks, pos=0):
        self.t = toks
        self.i = pos

    def tok(self):
        if self.i >= len(self.t):
            raise FormatError("record ended early: " + " ".join(self.t))
        x = self.t[self.i]
        self.i += 1
        return x

    def int(self):
        return int(self.tok())

    def opt(self):
        x = self.tok()
        return None if x == "-" else int(x)

    def name(self):
        return unescape(self.tok()).decode("utf-8", "replace")

    def bytes(self):
        return unescape(self.tok())

    def span(self):
        return [self.int() for _ in range(self.int())]

    def typed_locals(self):
        n = self.int()
        return [(self.int(), self.int()) for _ in range(n)]

    def captures(self):
        n = self.int()
        return [(self.int(), self.int()) for _ in range(n)]

    def intlit(self):
        x = self.tok()
        return int(x[1:])

    def done(self):
        if self.i != len(self.t):
            raise FormatError("trailing operands: " + " ".join(self.t))


class Program:
    def __init__(self):
        self.stage = None
        self.types = {}
        self.locals = {}
        self.fns = []
        self.exprs = {}
        self.pats = {}
        self.stmts = {}
        self.roots = []


def parse_type(c):
    kind = c.tok()
    if kind == "prim":
        return ("prim", c.tok())
    if kind in ("zst", "hole", "erased"):
        return (kind,)
    if kind in ("list", "box"):
        return (kind, c.int())
    if kind == "tuple":
        return ("tuple", c.span())
    if kind == "func":
        args = c.span()
        return ("func", args, c.int())
    if kind == "record":
        n = c.int()
        return ("record", [(c.name(), c.int()) for _ in range(n)])
    if kind == "tags":
        n = c.int()
        return ("tags", [(c.name(), c.span()) for _ in range(n)])
    if kind == "named":
        nk = c.tok()
        key = c.tok()
        nm = c.name()
        args = c.span()
        return ("named", nk, key, nm, args, c.opt())
    raise FormatError("unknown type kind " + kind)


NO_OPERANDS = {"unit", "unreachable", "uninitialized", "comptime_exhaustiveness_failed"}
ONE_CHILD = {"typed_boundary", "row_widen", "nominal", "dbg", "expect", "expect_err", "literal_rejected", "comptime_value"}


def parse_expr(k, c):
    if k in NO_OPERANDS:
        return ()
    if k in ("crash", "checked_error", "str_lit"):
        return (c.bytes(),)
    if k == "int_lit":
        return (c.intlit(),)
    if k in ("frac_f32_lit", "frac_f64_lit"):
        return (int(c.tok(), 16),)
    if k == "dec_lit":
        return (c.int(),)
    if k == "bytes_lit":
        data = c.bytes()
        n = c.int()
        elem = c.tok()
        width = c.int()
        return (data, n, None if elem == "-" else elem, width)
    if k == "local":
        return (c.int(),)
    if k == "uninitialized_payload":
        return (c.int(), c.int())
    if k == "static_data_candidate":
        return (c.opt(),)
    if k in ONE_CHILD:
        return (c.int(),)
    if k in ("list", "tuple"):
        return (c.span(),)
    if k == "record":
        n = c.int()
        return ([(c.name(), c.int()) for _ in range(n)],)
    if k == "record_update":
        base = c.int()
        n = c.int()
        return (base, [(c.name(), c.int()) for _ in range(n)])
    if k == "tag":
        return (c.name(), c.span())
    if k == "fn_ref":
        return (c.int(), c.captures())
    if k == "call_value":
        return (c.int(), c.span())
    if k == "call_proc":
        f = c.opt()
        shaped = c.int()
        args = c.span()
        return (f, shaped == 1, args, c.captures())
    if k == "low_level":
        return (c.tok(), c.span())
    if k == "field_access":
        recv = c.int()
        n = c.int()
        return (recv, [c.name() for _ in range(n)])
    if k == "tuple_access":
        return (c.int(), c.int())
    if k == "structural_eq":
        return (c.int(), c.int(), c.int() == 1)
    if k == "structural_hash":
        return (c.int(), c.int())
    if k == "match_":
        scrut = c.int()
        n = c.int()
        branches = []
        for _ in range(n):
            pat = c.int()
            stmts = c.span()
            guard = c.opt()
            branches.append((pat, stmts, guard, c.int()))
        return (scrut, branches)
    if k == "if_":
        n = c.int()
        branches = [(c.int(), c.int()) for _ in range(n)]
        return (branches, c.int())
    if k == "if_initialized_payload":
        return (c.int(), c.int(), c.int(), c.int(), c.int())
    if k == "try_sequence":
        return (c.int(), c.int(), c.opt(), c.int())
    if k == "try_record_sequence":
        return (c.int(), c.int(), c.name(), c.int(), c.name(), c.opt(), c.int())
    if k == "block":
        stmts = c.span()
        return (stmts, c.int())
    if k == "loop_":
        params = c.typed_locals()
        init = c.span()
        return (params, init, c.int())
    if k == "break_":
        return (c.opt(),)
    if k == "continue_":
        return (c.span(),)
    if k == "join_point":
        jid = c.int()
        params = c.typed_locals()
        body = c.int()
        return (jid, params, body, c.int())
    if k == "jump":
        jid = c.int()
        args = c.span()
        lp = c.typed_locals()
        return (jid, args, lp, c.span())
    if k == "return_":
        return (c.int(), c.int())
    if k == "let_":
        return (c.int(), c.int(), c.int())
    if k in ("lambda", "def_ref", "fn_def"):
        raise FormatError("pre-lift form " + k)
    raise FormatError("unknown expression kind " + k)


def parse_pat(k, c):
    if k == "wildcard":
        return ()
    if k == "int_lit":
        return (c.intlit(),)
    if k == "dec_lit":
        return (c.int(),)
    if k in ("frac_f32_lit", "frac_f64_lit"):
        return (int(c.tok(), 16),)
    if k == "str_lit":
        return (c.bytes(),)
    if k == "bind":
        return (c.int(),)
    if k == "as":
        return (c.int(), c.int())
    if k == "record":
        n = c.int()
        return ([(c.name(), c.int()) for _ in range(n)],)
    if k == "tuple":
        return (c.span(),)
    if k == "list":
        items = c.span()
        return (items, c.opt(), c.opt())
    if k == "tag":
        return (c.name(), c.span())
    if k == "nominal":
        return (c.int(),)
    if k == "str_pattern":
        prefix = c.bytes()
        n = c.int()
        steps = [(c.opt(), c.bytes()) for _ in range(n)]
        return (prefix, steps, c.tok())
    raise FormatError("unknown pattern kind " + k)


def parse_stmt(k, c):
    if k == "uninitialized":
        return (c.int(),)
    if k == "let_":
        return (c.int(), c.int(), c.int() == 1)
    if k in ("expr", "expect", "dbg"):
        return (c.int(),)
    if k == "return_":
        return (c.int(), c.int())
    if k in ("crash", "checked_error"):
        return (c.bytes(),)
    raise FormatError("unknown statement kind " + k)


def parse(text):
    p = Program()
    lines = text.split("\n")
    if not lines or lines[0] != "roc-lifted-ir %d" % VERSION:
        raise FormatError("not a version %d export: %r" % (VERSION, lines[0] if lines else ""))
    saw_end = False
    for line in lines[1:]:
        if not line:
            continue
        toks = line.split(" ")
        tag = toks[0]
        c = Cursor(toks, 1)
        if tag == "stage":
            p.stage = toks[1]
            continue
        if tag in ("types", "locals", "fns", "roots"):
            continue
        if tag == "end":
            saw_end = True
            continue
        if tag == "t":
            i = c.int()
            p.types[i] = parse_type(c)
        elif tag == "l":
            i = c.int()
            p.locals[i] = (c.int(), c.opt())
        elif tag == "f":
            i = c.int()
            if i != len(p.fns):
                raise FormatError("function ids out of order")
            args = c.typed_locals()
            caps = c.typed_locals()
            ret = c.int()
            b = c.tok()
            p.fns.append((args, caps, ret, None if b == "hosted" else int(b)))
        elif tag == "e":
            i = c.int()
            ty = c.int()
            k = c.tok()
            p.exprs[i] = (ty, k, parse_expr(k, c))
        elif tag == "p":
            i = c.int()
            ty = c.int()
            k = c.tok()
            p.pats[i] = (ty, k, parse_pat(k, c))
        elif tag == "s":
            i = c.int()
            k = c.tok()
            p.stmts[i] = (k, parse_stmt(k, c))
        elif tag == "r":
            p.roots.append((c.int(), c.tok(), c.int()))
        else:
            raise FormatError("unknown record " + tag)
        c.done()
    if not saw_end:
        raise FormatError("missing end")
    return p


def load(path):
    with open(path, "r", encoding="ascii") as f:
        return parse(f.read())
