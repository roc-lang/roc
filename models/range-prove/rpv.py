"""rpv: translation validator for the LIR range prover.

Reads ROC_RANGE_PROVE_DUMP output (FORMAT.md), symbolically executes each
round's pre-rewrite procedure with Z3 bit-vectors (bounded loop unrolling),
and for every rewrite asks whether some execution reaching the rewritten
statement contradicts the rewrite. A satisfiable query is a countermodel.

Usage: python3 rpv.py DUMP... [--k K] [--havoc-k K] [--paths N] [--deadline S] [--quiet]
Exits 1 if any rewrite has a countermodel.
"""
import sys, re, time, itertools
from dataclasses import dataclass, field
import z3

sys.setrecursionlimit(100000)

INT_W = {'u8': 8, 'u16': 16, 'u32': 32, 'u64': 64, 'u128': 128,
         'i8': 8, 'i16': 16, 'i32': 32, 'i64': 64, 'i128': 128}


def is_int(c):
    return c in INT_W


def signed(c):
    return c.startswith('i') and c in INT_W


# ---------------------------------------------------------------- parsing

@dataclass
class Stmt:
    sid: int
    kind: str
    toks: list


@dataclass
class Block:
    src: str
    proc: int
    rnd: int
    args: list = field(default_factory=list)  # [(local, cls)]
    body: int = -1
    stmts: dict = field(default_factory=dict)
    events: list = field(default_factory=list)  # (kind, sid, a, b)
    cls: dict = field(default_factory=dict)


def parse_local(tok):
    m = re.fullmatch(r'l(\d+):(\w+)', tok)
    assert m, tok
    return int(m.group(1)), m.group(2)


# One block, cut out by its markers: the compiler prints each block with one
# call, but other stderr output (warnings) can leave a partial line before it.
DUMP_BLOCK = re.compile(r'RPD P \d+ \d+\n.*?\nRPD END', re.S)


def parse_dump(path):
    """The blocks of a dump file, or of raw stderr containing them."""
    blocks = []
    with open(path, errors='replace') as f:
        text = f.read()
    for chunk in DUMP_BLOCK.findall(text):
        lines = chunk.split('\n')
        _, _, p, r = lines[0].split()
        cur = Block(path, int(p), int(r))
        for line in lines[1:-1]:
            t = line.split()
            if not t:
                continue
            if t[0] == 'A':
                cur.args = [parse_local(x) for x in t[1:]]
                for l, c in cur.args:
                    cur.cls[l] = c
            elif t[0] == 'B':
                cur.body = int(t[1])
            elif t[0] == 'S':
                st = Stmt(int(t[1]), t[2], t[3:])
                cur.stmts[st.sid] = st
                for x in t[3:]:
                    if re.fullmatch(r'l\d+:\w+', x):
                        l, c = parse_local(x)
                        cur.cls[l] = c
            elif t[0] == 'E':
                cur.events.append((t[1], int(t[2]), int(t[3]), int(t[4])))
        blocks.append(cur)
    return blocks


# ---------------------------------------------------------------- values

_fresh = itertools.count()


def fresh_name(p):
    return f'{p}{next(_fresh)}'


class Val:
    pass


class IntV(Val):
    __slots__ = ('t', 'cls')

    def __init__(self, t, cls):
        self.t, self.cls = t, cls


class BoolV(Val):  # bool layout: a 1-bit vector
    __slots__ = ('t',)

    def __init__(self, t):
        self.t = t


class ListV(Val):
    __slots__ = ('len', 'arr', 'name')

    def __init__(self, ln, arr=None, name=None):
        self.len = ln
        self.arr = arr if arr is not None else {}
        self.name = name


class StructV(Val):
    __slots__ = ('fields',)

    def __init__(self, fields):
        self.fields = fields


class TagV(Val):
    __slots__ = ('disc', 'payload')

    def __init__(self, disc, payload):
        self.disc, self.payload = disc, payload


class Opaque(Val):
    __slots__ = ('memo', 'name')

    def __init__(self, name):
        self.memo = {}
        self.name = name


def bv(v, w):
    return z3.BitVecVal(v % (1 << w), w)


def fresh(cls, name=None):
    name = name or fresh_name('v')
    if is_int(cls):
        return IntV(z3.BitVec(name, INT_W[cls]), cls)
    if cls == 'bool':
        return BoolV(z3.BitVec(name, 1))
    if cls == 'list':
        # A list's length is below 2^63 (isize max) by construction.
        return ListV(z3.ZeroExt(1, z3.BitVec(name + '_len', 63)), None, name)
    return Opaque(name)


def memo(o, key, cls):
    if key not in o.memo:
        o.memo[key] = fresh(cls, f'{o.name}_{key}')
    return coerce(o.memo[key], cls)


def coerce(v, cls):
    """View `v` as a value of layout class `cls`."""
    if is_int(cls):
        if isinstance(v, IntV):
            if v.t.size() == INT_W[cls]:
                return IntV(v.t, cls)
            return fresh(cls)
        if isinstance(v, Opaque):
            return memo(v, 'as_' + cls, cls)
        if isinstance(v, StructV) and len(v.fields) == 1:
            return coerce(v.fields[0], cls)
        return fresh(cls)
    if cls == 'bool':
        if isinstance(v, BoolV):
            return v
        if isinstance(v, IntV) and v.t.size() == 1:
            return BoolV(v.t)
        if isinstance(v, TagV):
            return BoolV(z3.Extract(0, 0, v.disc))
        if isinstance(v, Opaque):
            return memo(v, 'as_bool', 'bool')
        return fresh('bool')
    if cls == 'list':
        if isinstance(v, ListV):
            return v
        if isinstance(v, Opaque):
            return memo(v, 'as_list', 'list')
        if isinstance(v, StructV) and len(v.fields) == 1:
            return coerce(v.fields[0], cls)
        return fresh('list')
    return v


def as64(v):
    if isinstance(v, (IntV, BoolV)):
        t = v.t
        if t.size() < 64:
            return z3.ZeroExt(64 - t.size(), t)
        return z3.Extract(63, 0, t)
    if isinstance(v, TagV):
        return z3.ZeroExt(32, v.disc)
    if isinstance(v, Opaque):
        return z3.ZeroExt(32, memo(v, 'disc', 'u32').t)
    return z3.BitVec(fresh_name('sw'), 64)


def disc_of(v):
    if isinstance(v, TagV):
        return v.disc
    if isinstance(v, BoolV):
        return z3.ZeroExt(31, v.t)
    if isinstance(v, Opaque):
        return memo(v, 'disc', 'u32').t
    return z3.BitVec(fresh_name('d'), 32)


# ---------------------------------------------------------------- arithmetic

def ext(t, w, sgn):
    n = w - t.size()
    return z3.SignExt(n, t) if sgn else z3.ZeroExt(n, t)


def overflow(op, a, b, sgn):
    """The condition under which `a op b` overflows its width."""
    if not sgn:
        if op == 'add':
            return z3.ULT(a + b, a)
        if op == 'sub':
            return z3.ULT(a, b)
        return z3.Not(z3.BVMulNoOverflow(a, b, False))
    if op == 'add':
        return z3.Not(z3.And(z3.BVAddNoOverflow(a, b, True), z3.BVAddNoUnderflow(a, b)))
    if op == 'sub':
        return z3.Not(z3.And(z3.BVSubNoOverflow(a, b), z3.BVSubNoUnderflow(a, b, True)))
    return z3.Not(z3.And(z3.BVMulNoOverflow(a, b, True), z3.BVMulNoUnderflow(a, b)))


def wrap(op, a, b):
    return {'add': a + b, 'sub': a - b, 'mul': a * b}[op]


FAMILY = re.compile(r'num_int_(add|sub|mul)_(wrap|crash_on_overflow|overflows|proven_cannot_overflow)$')
CONV = re.compile(r'([ui]\d+)_to_([ui]\d+)(_wrap|_try)?$')


# ---------------------------------------------------------------- executor

class Found(Exception):
    pass


@dataclass
class Result:
    kind: str
    sid: int
    status: str = 'unreached'  # unreached | ok | cex | skip
    detail: str = ''
    reached: int = 0


class Exec:
    def __init__(self, blk, k=3, max_paths=20000, timeout_ms=5000, deadline=60.0, havoc=False):
        self.b = blk
        self.k = k
        self.havoc = havoc
        self.checks = 0
        self.max_paths = max_paths
        self.paths = 0
        self.solver = z3.Solver()
        self.solver.set('timeout', timeout_ms)
        self.deadline = time.time() + deadline
        self.timed_out = False
        self.targets = {}
        self.results = []
        self.chain_inner = {}
        for (kind, sid, a, b) in blk.events:
            r = Result(kind, sid)
            r.a, r.b = a, b
            self.results.append(r)
            self.targets.setdefault(sid, []).append(r)
            if kind == 'chain':
                self.chain_inner.setdefault(a, []).append(r)
        self.join_body = {}
        for st in blk.stmts.values():
            if st.kind == 'join':
                self.join_body[st.toks[0]] = (int(st.toks[1]), [parse_local(x) for x in st.toks[3:]])
        self.compute_relevance()
        self.cycle_assigned = {}

    def loop_assigned(self, j):
        """Locals assigned inside join `j`'s body: its lexical region,
        following every edge except jumps (a jump to a join declared in the
        region reaches a statement of the region anyway; a jump to an
        enclosing join leaves it). Locals defined in the body before they
        are read are rewritten each iteration, so havocking them too is
        harmless; the ones read before written are the loop-carried ones."""
        if j in self.cycle_assigned:
            return self.cycle_assigned[j]
        body = self.join_body[j][0]
        region, work = set(), [body]
        while work:
            x = work.pop()
            if x in region:
                continue
            region.add(x)
            st = self.b.stmts[x]
            if st.kind == 'jump':
                continue
            if st.kind == 'join':
                work.extend([int(st.toks[1]), int(st.toks[2])])
                continue
            work.extend(self.succs(st))
        # Keep the statements on the cycle: those that reach `jump j` along
        # region edges, including jumps to joins declared in the region.
        declared = {self.b.stmts[x].toks[0] for x in region if self.b.stmts[x].kind == 'join'}
        preds = {}
        for x in region:
            st = self.b.stmts[x]
            if st.kind == 'jump':
                if st.toks[0] in declared:
                    preds.setdefault(self.join_body[st.toks[0]][0], []).append(x)
                continue
            nxt = [int(st.toks[1]), int(st.toks[2])] if st.kind == 'join' else self.succs(st)
            for y in nxt:
                preds.setdefault(y, []).append(x)
        cycle, work = set(), [x for x in region if self.b.stmts[x].kind == 'jump' and self.b.stmts[x].toks[0] == j]
        while work:
            x = work.pop()
            if x in cycle or x not in region:
                continue
            cycle.add(x)
            work.extend(preds.get(x, []))
        out = set(l for (l, _) in self.join_body[j][1])
        for x in cycle:
            st = self.b.stmts[x]
            if st.kind in ('lit', 'ref', 'll', 'struct', 'list', 'tag', 'set', 'havoc') and len(st.toks) > 1:
                out.add(parse_local(st.toks[1])[0])
        self.cycle_assigned[j] = out
        return out

    # Statements from which some rewrite target is reachable.
    def succs(self, st):
        t = st.toks
        k = st.kind
        if k in ('lit', 'ref', 'll', 'struct', 'list', 'tag', 'set', 'havoc', 'nop'):
            return [int(t[0])]
        if k == 'switch':
            return [int(t[1])] + [int(x.split(':')[1]) for x in t[2:]]
        if k == 'branch':
            return [int(x) for x in t]
        if k == 'join':
            return [int(t[2])]
        if k == 'jump':
            return [self.join_body[t[0]][0]]
        return []

    def compute_relevance(self):
        preds = {}
        for st in self.b.stmts.values():
            for s in self.succs(st):
                preds.setdefault(s, []).append(st.sid)
        rel = set()
        work = list(self.targets.keys()) + list(self.chain_inner.keys())
        while work:
            x = work.pop()
            if x in rel:
                continue
            rel.add(x)
            work.extend(preds.get(x, []))
        self.relevant = rel

    def pending(self):
        return any(r.status != 'cex' for r in self.results)

    def sat(self, extra):
        self.solver.push()
        self.solver.add(extra)
        r = self.solver.check()
        m = self.solver.model() if r == z3.sat else None
        self.solver.pop()
        return r, m

    def get(self, env, l):
        cls = self.b.cls.get(l, 'other')
        if l not in env:
            # Read before any write on this path (an uninitialized join
            # parameter on an edge the pass never reads it on, say).
            env[l] = fresh(cls, f'u{l}_')
        return coerce(env[l], cls)

    def run(self):
        env = {}
        for (l, c) in self.b.args:
            env[l] = fresh(c, f'arg{l}_')
        self.argvals = dict(env)
        try:
            self.walk(self.b.body, env, {})
        except Found:
            pass
        return self.results

    def check_targets(self, sid, env):
        for r in self.targets.get(sid, []):
            if r.status == 'cex':
                continue
            r.reached += 1
            cond = self.violation(r, env)
            if cond is None:
                r.status = 'skip'
                continue
            res, m = self.sat(cond)
            if res == z3.sat:
                r.status = 'cex'
                r.detail = self.describe(m, env, r)
            elif res == z3.unknown:
                r.detail = 'unknown'
            elif r.status != 'skip':
                r.status = 'ok'
        for r in self.chain_inner.get(sid, []):
            # The inner op of a folded chain now wraps: an execution whose
            # inner op overflowed (and crashed) must still crash at the outer.
            if r.status == 'cex':
                continue
            cond = self.chain_inner_violation(r, env)
            if cond is None:
                continue
            res, m = self.sat(cond)
            if res == z3.sat:
                r.status = 'cex'
                r.detail = 'inner-overflow-not-crashing ' + self.describe(m, env, r)

    def stmt_args(self, st):
        return [parse_local(x) for x in st.toks[4:]]

    def violation(self, r, env):
        st = self.b.stmts[r.sid]
        if r.kind in ('cmp', 'ovf', 'proven', 'crash', 'div', 'simd', 'chain'):
            if st.kind != 'll':
                return None
            op = st.toks[2]
            args = [self.get(env, l) for (l, _) in self.stmt_args(st)]
            if st.toks[3] == '1':
                args = args[:-1]
            if r.kind == 'cmp':
                v = self.compare(op, args)
                return v != bv(r.a, 1)
            m = FAMILY.match(op)
            if r.kind == 'ovf':
                if not m or len(args) != 2:
                    return None
                a, b = args
                o = overflow(m.group(1), a.t, b.t, signed(a.cls))
                return o if r.a == 0 else z3.Not(o)
            if r.kind == 'proven':
                if not m or len(args) != 2:
                    return None
                a, b = args
                return overflow(m.group(1), a.t, b.t, signed(a.cls))
            if r.kind == 'crash':
                if not m or len(args) != 2:
                    return None
                a, b = args
                return z3.Not(overflow(m.group(1), a.t, b.t, signed(a.cls)))
            if r.kind == 'div':
                d = args[1]
                return d.t == 0
            if r.kind == 'simd':
                c = args[2]
                return as64(c) != bv(r.a, 64)
            if r.kind == 'chain':
                # outer: y = t op c2, t = x op c1 (inner, already executed and
                # checked). New: y = x op (c1+c2), crash iff that overflows.
                return self.chain_outer_violation(r, env, st, args)
        if r.kind == 'switch':
            if st.kind != 'switch':
                return None
            c = self.get(env, int(st.toks[0][1:]))
            return as64(c) != bv(r.a, 64)
        return None

    def chain_parts(self, r):
        inner = self.b.stmts[r.a]
        lit = self.b.stmts[r.b]
        m = FAMILY.match(inner.toks[2])
        iargs = self.stmt_args(inner)
        return inner, lit, m.group(1), iargs

    def chain_outer_violation(self, r, env, st, args):
        inner, lit, op, iargs = self.chain_parts(r)
        t_local = parse_local(inner.toks[1])[0]
        c2 = int(lit.toks[2])
        # The base x and the constant c1 were captured when the walk passed
        # the inner statement.
        cap = getattr(r, 'cap', None)
        if cap is None:
            return None
        x, c1 = cap
        w = x.size()
        sgn = False
        orig_ovf = overflow(op, self.get(env, t_local).t, bv(c2, w), sgn)
        comb = c1 + c2
        if comb > (1 << w) - 1:
            new_ovf = z3.BoolVal(True)
        else:
            new_ovf = overflow(op, x, bv(comb, w), sgn)
        return orig_ovf != new_ovf

    def chain_inner_violation(self, r, env):
        inner, lit, op, iargs = self.chain_parts(r)
        vals = [self.get(env, l) for (l, _) in iargs]
        if len(vals) != 2:
            return None
        # The constant operand is the one whose value simplifies to a literal
        # on this path; the other is the base.
        a, b = vals[0].t, vals[1].t
        if z3.is_bv_value(z3.simplify(b)):
            x, c1 = a, z3.simplify(b).as_long()
        elif z3.is_bv_value(z3.simplify(a)) and op == 'add':
            x, c1 = b, z3.simplify(a).as_long()
        else:
            return None
        r.cap = (x, c1)
        c2 = int(self.b.stmts[r.b].toks[2])
        w = x.size()
        comb = c1 + c2
        inner_ovf = overflow(op, x, bv(c1, w), False)
        if comb > (1 << w) - 1:
            return None
        return z3.And(inner_ovf, z3.Not(overflow(op, x, bv(comb, w), False)))

    def describe(self, m, env, r):
        parts = []
        for l, v in self.argvals.items():
            if isinstance(v, IntV):
                parts.append(f'arg l{l}={m.eval(v.t, model_completion=True)}')
            elif isinstance(v, ListV):
                parts.append(f'arg l{l}.len={m.eval(v.len, model_completion=True)}')
        st = self.b.stmts[r.sid]
        if st.kind == 'll':
            for (l, _) in self.stmt_args(st):
                v = env.get(l)
                if isinstance(v, (IntV, BoolV)):
                    parts.append(f'l{l}={m.eval(v.t, model_completion=True)}')
        elif st.kind == 'switch':
            l = int(st.toks[0][1:])
            v = env.get(l)
            if v is not None:
                parts.append(f'l{l}={m.eval(as64(v), model_completion=True)}')
        decls = sorted((d.name(), m[d]) for d in m.decls())
        parts.append('model{' + ', '.join(f'{n}={v}' for n, v in decls[:40]) + '}')
        return ' '.join(parts)

    def compare(self, op, args):
        a, b = args
        if isinstance(a, BoolV) or isinstance(b, BoolV):
            at, bt = as64(a), as64(b)
            sg = False
        elif isinstance(a, IntV) and isinstance(b, IntV):
            at, bt = a.t, b.t
            sg = signed(a.cls)
        else:
            return z3.BitVec(fresh_name('cmp'), 1)
        if op == 'num_is_eq':
            c = at == bt
        elif op == 'num_is_lt':
            c = (at < bt) if sg else z3.ULT(at, bt)
        elif op == 'num_is_lte':
            c = (at <= bt) if sg else z3.ULE(at, bt)
        elif op == 'num_is_gt':
            c = (at > bt) if sg else z3.UGT(at, bt)
        elif op == 'num_is_gte':
            c = (at >= bt) if sg else z3.UGE(at, bt)
        else:
            return z3.BitVec(fresh_name('cmp'), 1)
        return z3.If(c, bv(1, 1), bv(0, 1))

    # Low-level semantics. Returns (value, path_constraint or None).
    def lowlevel(self, op, tcls, args, env):
        def A(i):
            return args[i]
        m = FAMILY.match(op)
        if m:
            if len(args) != 2 or not isinstance(A(0), IntV) or not isinstance(A(1), IntV):
                return fresh(tcls), None
            a, b = A(0).t, A(1).t
            if a.size() != b.size():
                return fresh(tcls), None
            o = overflow(m.group(1), a, b, signed(A(0).cls))
            mode = m.group(2)
            if mode == 'overflows':
                return BoolV(z3.If(o, bv(1, 1), bv(0, 1))), None
            r = IntV(wrap(m.group(1), a, b), A(0).cls)
            if mode == 'crash_on_overflow':
                return r, z3.Not(o)
            return r, None
        if op in ('num_is_eq', 'num_is_lt', 'num_is_lte', 'num_is_gt', 'num_is_gte'):
            if len(args) == 2:
                return BoolV(self.compare(op, args)), None
            return fresh('bool'), None
        if op == 'bool_not':
            return BoolV(~coerce(A(0), 'bool').t), None
        if op == 'bool_likely':
            return coerce(A(0), 'bool'), None
        if op == 'list_len':
            l = coerce(A(0), 'list')
            return coerce(IntV(l.len, 'u64'), tcls), None
        if op in ('num_bitwise_and', 'num_bitwise_or', 'num_bitwise_xor') and len(args) == 2 \
                and isinstance(A(0), IntV) and isinstance(A(1), IntV):
            a, b = A(0).t, A(1).t
            f = {'num_bitwise_and': a & b, 'num_bitwise_or': a | b, 'num_bitwise_xor': a ^ b}[op]
            return IntV(f, A(0).cls), None
        if op == 'num_bitwise_not' and isinstance(A(0), IntV):
            return IntV(~A(0).t, A(0).cls), None
        if op in ('num_div_by', 'num_div_by_checked', 'num_div_trunc_by', 'num_div_trunc_by_checked',
                  'num_rem_by', 'num_rem_by_checked', 'num_mod_by', 'num_mod_by_checked') \
                and len(args) == 2 and isinstance(A(0), IntV) and isinstance(A(1), IntV):
            a, b = A(0).t, A(1).t
            sg = signed(A(0).cls)
            if 'div' in op:
                r = (a / b) if sg else z3.UDiv(a, b)
            elif 'rem' in op:
                r = z3.SRem(a, b) if sg else z3.URem(a, b)
            else:
                r = z3.SMod(a, b) if sg else z3.URem(a, b)
            pc = (b != 0) if op.endswith('_checked') else None
            return IntV(r, A(0).cls), pc
        if op in ('num_shift_left_by', 'num_shift_right_by', 'num_shift_right_zf_by') and len(args) == 2 \
                and isinstance(A(0), IntV) and isinstance(A(1), IntV):
            a, n = A(0).t, A(1).t
            w = a.size()
            # The count is taken modulo the width (Builtin.roc shl_wrap/shr_wrap).
            nw = max(n.size(), w)
            n = z3.URem(ext(n, nw, False), bv(w, nw))
            n = z3.Extract(w - 1, 0, n) if nw > w else n
            if op == 'num_shift_left_by':
                r = a << n
            elif op == 'num_shift_right_zf_by' or not signed(A(0).cls):
                r = z3.LShR(a, n)
            else:
                r = a >> n
            return IntV(r, A(0).cls), None
        if op == 'num_count_one_bits' and isinstance(A(0), IntV):
            a = A(0).t
            w = a.size()
            s = sum(z3.ZeroExt(w - 1, z3.Extract(i, i, a)) for i in range(w))
            if not is_int(tcls):
                return fresh(tcls), None
            tw = INT_W[tcls]
            r = z3.ZeroExt(tw - w, s) if tw >= w else z3.Extract(tw - 1, 0, s)
            return IntV(r, tcls), None
        if op in ('num_count_leading_zero_bits', 'num_count_trailing_zero_bits') and isinstance(A(0), IntV) and is_int(tcls):
            a = A(0).t
            w = a.size()
            tw = INT_W[tcls]
            r = bv(w, tw)
            rng = range(w) if op == 'num_count_leading_zero_bits' else range(w - 1, -1, -1)
            # leading: highest set bit i gives w-1-i; build from low to high
            for i in rng:
                cnt = (w - 1 - i) if op == 'num_count_leading_zero_bits' else i
                r = z3.If(z3.Extract(i, i, a) == 1, bv(cnt, tw), r)
            return IntV(r, tcls), None
        if op == 'num_abs_diff' and len(args) == 2 and isinstance(A(0), IntV) and isinstance(A(1), IntV):
            a, b = A(0).t, A(1).t
            if signed(A(0).cls):
                return fresh(tcls), None
            return IntV(z3.If(z3.UGE(a, b), a - b, b - a), A(0).cls), None
        mc = CONV.match(op)
        if mc and isinstance(A(0), IntV):
            src, dst, suf = mc.group(1), mc.group(2), mc.group(3)
            a = A(0).t
            sw, dw = INT_W[src], INT_W[dst]
            if suf == '_try':
                return fresh(tcls), None
            if dw <= sw:
                r = z3.Extract(dw - 1, 0, a)
            else:
                r = ext(a, dw, signed(src))
            return IntV(r, dst), None
        # Lists
        if op == 'list_clear' or op == 'list_with_capacity':
            return ListV(bv(0, 64)), None
        if op == 'list_capacity':
            l = coerce(A(0), 'list')
            c = z3.BitVec(fresh_name('cap'), 64)
            return IntV(z3.If(z3.UGE(c, l.len), c, l.len), 'u64'), None
        if op == 'list_append_unsafe':
            l = coerce(A(0), 'list')
            return ListV(l.len + 1, {}), None
        if op in ('list_reserve', 'list_reserve_for_append', 'list_release_excess_capacity',
                  'list_map_prepare_reuse', 'list_map_cast_unsafe', 'list_reverse', 'list_swap',
                  'list_set', 'list_set_in_place_unsafe', 'list_map_write_unsafe', 'list_replace_unsafe'):
            if not isinstance(A(0), (ListV, Opaque)):
                return fresh(tcls), None
            l = coerce(A(0), 'list')
            if op in ('list_reserve', 'list_reserve_for_append', 'list_release_excess_capacity', 'list_map_prepare_reuse'):
                nl = ListV(l.len, l.arr, l.name)
            else:
                nl = ListV(l.len, {})
            if tcls == 'list':
                return nl, None
            if op == 'list_replace_unsafe':
                # returns a struct { list, value } in some layout; not modeled
                return fresh(tcls), None
            return nl, None
        if op == 'list_prepend':
            l = coerce(A(0), 'list')
            return ListV(l.len + 1, {}), None
        if op == 'list_concat':
            a, b = coerce(A(0), 'list'), coerce(A(1), 'list')
            return ListV(a.len + b.len, {}), z3.ULT(a.len + b.len, bv(1 << 63, 64))
        if op == 'list_drop_at':
            l = coerce(A(0), 'list')
            i = A(1)
            if isinstance(i, IntV) and i.t.size() == 64:
                return ListV(z3.If(z3.ULT(i.t, l.len), l.len - 1, l.len), {}), None
            return fresh('list'), None
        if op in ('list_sublist', 'list_sublist_borrowed') and len(args) >= 3:
            l = coerce(A(0), 'list')
            s_, n_ = A(1), A(2)
            if isinstance(s_, IntV) and isinstance(n_, IntV) and s_.t.size() == 64 and n_.t.size() == 64:
                size = l.len
                rest = size - s_.t
                ln = z3.If(z3.Or(size == 0, n_.t == 0, z3.UGE(s_.t, size)), bv(0, 64),
                           z3.If(z3.ULT(n_.t, rest), n_.t, rest))
                return ListV(ln, {}), None
            return fresh('list'), None
        if op == 'list_get_unsafe' and len(args) >= 2:
            l = coerce(A(0), 'list')
            i = A(1)
            if is_int(tcls) and isinstance(i, IntV):
                key = ('elem', tcls)
                if key not in l.arr:
                    l.arr[key] = z3.Array(fresh_name('arr'), z3.BitVecSort(64), z3.BitVecSort(INT_W[tcls]))
                return IntV(z3.Select(l.arr[key], as64(i)), tcls), None
            return fresh(tcls), None
        return fresh(tcls), None

    def walk(self, sid, env, visits):
        while True:
            if time.time() > self.deadline:
                self.timed_out = True
                raise Found()
            if not self.pending():
                raise Found()
            if sid not in self.relevant:
                self.paths += 1
                return
            self.check_targets(sid, env)
            st = self.b.stmts[sid]
            k, t = st.kind, st.toks
            if k == 'lit':
                l, c = parse_local(t[1])
                v = t[2]
                if re.fullmatch(r'-?\d+', v):
                    if is_int(c):
                        env[l] = IntV(bv(int(v), INT_W[c]), c)
                    elif c == 'bool':
                        env[l] = BoolV(bv(int(v), 1))
                    else:
                        env[l] = fresh(c)
                elif v.startswith('len='):
                    env[l] = ListV(bv(int(v[4:]), 64))
                else:
                    env[l] = fresh(c)
                sid = int(t[0])
            elif k == 'ref':
                l, c = parse_local(t[1])
                kind, src, idx, var = t[2], int(t[3][1:]), int(t[4]), int(t[5])
                sv = env.get(src)
                if sv is None:
                    sv = self.get(env, src)
                if kind in ('local', 'nominal', 'list_reinterpret'):
                    env[l] = coerce(sv, c)
                elif kind == 'field':
                    if isinstance(sv, StructV) and idx < len(sv.fields):
                        env[l] = coerce(sv.fields[idx], c)
                    elif isinstance(sv, Opaque):
                        env[l] = memo(sv, f'f{idx}', c)
                    else:
                        env[l] = fresh(c)
                elif kind == 'discriminant':
                    d = disc_of(sv)
                    if is_int(c):
                        w = INT_W[c]
                        env[l] = IntV(z3.Extract(w - 1, 0, d) if w <= 32 else z3.ZeroExt(w - 32, d), c)
                    else:
                        env[l] = fresh(c)
                elif kind == 'tag_payload_struct':
                    if isinstance(sv, TagV) and var in sv.payload:
                        env[l] = coerce(sv.payload[var], c)
                    elif isinstance(sv, Opaque):
                        env[l] = memo(sv, f'p{var}', c)
                    else:
                        env[l] = fresh(c)
                elif kind == 'tag_payload':
                    p = None
                    if isinstance(sv, TagV) and var in sv.payload:
                        p = sv.payload[var]
                    elif isinstance(sv, Opaque):
                        p = memo(sv, f'p{var}', 'other')
                    if isinstance(p, StructV) and idx < len(p.fields):
                        env[l] = coerce(p.fields[idx], c)
                    elif isinstance(p, Opaque):
                        env[l] = memo(p, f'f{idx}', c)
                    elif p is not None and idx == 0:
                        env[l] = coerce(p, c)
                    else:
                        env[l] = fresh(c)
                else:
                    env[l] = fresh(c)
                sid = int(t[0])
            elif k == 'll':
                l, c = parse_local(t[1])
                op = t[2]
                args = [self.get(env, a) for (a, _) in self.stmt_args(st)]
                if t[3] == '1':
                    args = args[:-1]
                v, pc = self.lowlevel(op, c, args, env)
                env[l] = v
                if pc is not None:
                    # Control continues only where the check passed.
                    self.solver.add(pc)  # within the enclosing push scope
                sid = int(t[0])
            elif k == 'struct':
                l, c = parse_local(t[1])
                env[l] = StructV([self.get(env, a) for (a, _) in (parse_local(x) for x in t[2:])])
                sid = int(t[0])
            elif k == 'list':
                l, c = parse_local(t[1])
                env[l] = ListV(bv(len(t) - 2, 64))
                sid = int(t[0])
            elif k == 'tag':
                l, c = parse_local(t[1])
                var, d = int(t[2]), int(t[3])
                if c == 'bool':
                    env[l] = BoolV(bv(d, 1))
                else:
                    payload = {}
                    if len(t) > 4:
                        payload[var] = self.get(env, parse_local(t[4])[0])
                    env[l] = TagV(bv(d, 32), payload)
                sid = int(t[0])
            elif k == 'set':
                l, c = parse_local(t[1])
                env[l] = coerce(self.get(env, parse_local(t[2])[0]), c)
                sid = int(t[0])
            elif k == 'havoc':
                if len(t) > 1:
                    l, c = parse_local(t[1])
                    env[l] = fresh(c)
                sid = int(t[0])
            elif k == 'nop':
                sid = int(t[0])
            elif k == 'join':
                # Entering a join's scope afresh (an inner loop on each outer
                # iteration) restarts its unrolling count.
                if t[0] in visits:
                    visits = dict(visits)
                    del visits[t[0]]
                sid = int(t[2])
            elif k == 'jump':
                j = t[0]
                n = visits.get(j, 0)
                if n == 'H':
                    # A back edge after the havocked iteration: the havoc
                    # state already covers every later iteration.
                    self.paths += 1
                    return
                visits = dict(visits)
                if n >= self.k:
                    if not self.havoc:
                        self.paths += 1
                        return
                    for l in self.loop_assigned(j):
                        env[l] = fresh(self.b.cls.get(l, 'other'), f'h{l}_')
                    visits[j] = 'H'
                else:
                    visits[j] = n + 1
                sid = self.join_body[j][0]
            elif k == 'switch':
                cond = as64(self.get(env, int(t[0][1:])))
                default = int(t[1])
                cases = [(int(x.split(':')[0]), int(x.split(':')[1])) for x in t[2:]]
                arms = [(cond == bv(v, 64), b) for (v, b) in cases]
                arms.append((z3.And([cond != bv(v, 64) for (v, _) in cases]) if cases else z3.BoolVal(True), default))
                self.fork(arms, env, visits)
                return
            elif k == 'branch':
                self.fork([(z3.BoolVal(True), int(x)) for x in t], env, visits)
                return
            else:  # ret, crash, stop
                self.paths += 1
                return

    def fork(self, arms, env, visits):
        live = [(c, b) for (c, b) in arms if b in self.relevant]
        if not live:
            self.paths += 1
            return
        for (c, b) in live:
            if self.paths > self.max_paths:
                self.timed_out = True
                raise Found()
            c = z3.simplify(c)
            if z3.is_false(c):
                continue
            self.solver.push()
            self.solver.add(c)
            r = self.solver.check() if (len(live) > 1 and not z3.is_true(c)) else z3.sat
            self.checks += 1
            if r != z3.unsat:
                self.walk(b, dict(env), visits)
            self.solver.pop()


def validate(path, k=3, havoc_k=2, deadline=60.0, max_paths=20000):
    """Check every rewrite in the dump at `path`. Bounded execution first;
    then, for the targets it left unreached, a second pass that havocs loop
    state at the bound (havoc_k > 0). Havoc mode forgets loop invariants such
    as `i <= len`, so it is only consulted where bounded execution found no
    path; on reached targets it would only add spurious countermodels.

    Returns (counts, lines): counts maps a status to how many rewrites ended
    in it (`budget` counts blocks that ran out of time or paths), and lines
    describes every non-ok result."""
    counts, lines = {}, []

    def run(blk, mode, kk, hv):
        ex = Exec(blk, k=kk, max_paths=max_paths, deadline=deadline, havoc=hv)
        ex.solver.push()
        results = ex.run()
        if ex.timed_out:
            counts['budget'] = counts.get('budget', 0) + 1
            lines.append(f'{mode} p{blk.proc} r{blk.rnd}: budget exhausted (paths={ex.paths})')
        return results

    for blk in parse_dump(path):
        final = run(blk, 'bmc', k, False)
        left = {(r.kind, r.sid) for r in final if r.status == 'unreached'}
        if left and havoc_k > 0:
            sub = Block(blk.src, blk.proc, blk.rnd, blk.args, blk.body, blk.stmts,
                        [e for e in blk.events if (e[0], e[1]) in left], blk.cls)
            final = [r for r in final if r.status != 'unreached'] + run(sub, 'havoc', havoc_k, True)
        for r in final:
            counts[r.status] = counts.get(r.status, 0) + 1
            if r.status != 'ok':
                lines.append(f'p{blk.proc} r{blk.rnd} {r.kind} s{r.sid} a={r.a} b={r.b}: {r.status} {r.detail}'.rstrip())
    return counts, lines


def main():
    import argparse
    ap = argparse.ArgumentParser(description='Re-prove every range_prove rewrite in ROC_RANGE_PROVE_DUMP output.')
    ap.add_argument('dumps', nargs='+')
    ap.add_argument('--k', type=int, default=3, help='loop unrolling bound (default 3)')
    ap.add_argument('--havoc-k', type=int, default=2,
                    help='unrolling bound of the havoc pass over unreached targets; 0 disables it (default 2)')
    ap.add_argument('--paths', type=int, default=20000, help='path budget per round (default 20000)')
    ap.add_argument('--deadline', type=float, default=60.0, help='seconds per round and pass (default 60)')
    ap.add_argument('--quiet', action='store_true', help='print only countermodels and the totals')
    a = ap.parse_args()
    tot = {}
    for d in a.dumps:
        counts, lines = validate(d, k=a.k, havoc_k=a.havoc_k, deadline=a.deadline, max_paths=a.paths)
        for s_, n in counts.items():
            tot[s_] = tot.get(s_, 0) + n
        for l in lines:
            if not a.quiet or ': cex' in l:
                print(f'{d} {l}')
    print('TOTAL ' + ' '.join(f'{s_}={n}' for s_, n in sorted(tot.items())))
    sys.exit(1 if tot.get('cex') else 0)


if __name__ == '__main__':
    main()
