"""Random generator: a type, a set of patterns over it, and a Roc program that matches them in one context.

Each step composes small typed pieces at random: gen_type builds a type from type constructors,
gen_pat builds a pattern of a given type, gen_branches builds an ordered branch list, and a context
places the branches (or one pattern) in a match, a destructure, a function argument, a lambda or a loop.
"""

import random
import re

from model import Env, analyze, missing_witness, W

U64 = ('N', 'U64')
I64 = ('N', 'I64')
STR = ('S',)
BOOL = ('U', (('False', ()), ('True', ())), False, 'Bool')
EMPTY = ('U', (), False, None)

TAGS = ['A', 'B', 'C', 'D']
FIELDS = ['a', 'b', 'c']


def TRY(a, b):
    return ('U', (('Ok', (a,)), ('Err', (b,))), False, 'Try')


class Gen:
    def __init__(self, rng, allow_open=True, allow_var=True):
        self.rng = rng
        self.nominals = {}  # name -> ('U', tags, False, name)
        self.allow_open = allow_open
        self.allow_var = allow_var
        self.counter = 0

    # ---------------- types ----------------

    def gen_type(self, depth):
        r = self.rng
        if depth <= 0:
            return r.choice([U64, STR, BOOL, I64, ('U', (('A', ()), ('B', ())), False, None)])
        kinds = ['union', 'open', 'try', 'bool', 'nom', 'rec_nom', 'void_nom', 'record', 'tuple', 'list', 'prim',
                 'empty', 'var']
        weights = [14, 4 if self.allow_open else 0, 8, 3, 5, 2, 2, 5, 6, 5, 4, 2, 1 if self.allow_var else 0]
        k = r.choices(kinds, weights)[0]
        if k in ('union', 'open'):
            n = r.randint(1, 3)
            tags = tuple((TAGS[i], tuple(self.gen_type(depth - 1) for _ in range(r.choice([0, 0, 1, 1, 2]))))
                         for i in range(n))
            return ('U', tags, k == 'open', None)
        if k == 'try':
            err = EMPTY if r.random() < 0.3 else self.gen_type(depth - 1)
            return TRY(self.gen_type(depth - 1), err)
        if k == 'bool':
            return BOOL
        if k == 'nom':
            i = len(self.nominals) + 1
            tags = tuple(('%s%d' % (TAGS[j], i), tuple(self.gen_type(depth - 1) for _ in range(r.choice([0, 1, 1, 2]))))
                         for j in range(r.randint(1, 3)))
            return self._add_nom('N%d' % i, tags)
        if k == 'rec_nom':
            i = len(self.nominals) + 1
            name = 'Rec%d' % i
            leaf = self.gen_type(0)
            choice = r.random()
            if choice < 0.6:
                tags = (('Leaf%d' % i, ()), ('Node%d' % i, (('NR', name), leaf)))
            elif choice < 0.8:
                tags = (('Loop%d' % i, (('NR', name),)),)  # inhabited: INHAB is coinductive
            else:
                tags = (('Bad%d' % i, (('NR', name), EMPTY)), ('Ok%d' % i, (leaf,)))
            return self._add_nom(name, tags)
        if k == 'void_nom':
            return self._add_nom('V%d' % (len(self.nominals) + 1), ())
        if k == 'record':
            return ('R', tuple((FIELDS[i], self.gen_type(depth - 1)) for i in range(r.randint(1, 3))))
        if k == 'tuple':
            return ('T', tuple(self.gen_type(depth - 1) for _ in range(r.randint(2, 3))))
        if k == 'list':
            e = self.gen_type(depth - 1)
            if not Env(self.nominals).inhabited(e):
                e = U64
            return ('L', e)
        if k == 'empty':
            return EMPTY
        if k == 'var':
            return ('V', 'a')
        return r.choice([U64, STR, I64])

    def _add_nom(self, name, tags):
        # A nominal declaration may not contain `..` or a free type variable.
        tags = tuple((n, tuple(strip_open_var(a) for a in args)) for n, args in tags)
        self.nominals[name] = ('U', tags, False, name)
        return ('NR', name)

    # ---------------- printing types ----------------

    def ty(self, t, open_ok=True):
        k = t[0]
        if k == 'NR':
            return t[1]
        if k == 'U':
            if t[3] == 'Try':
                return 'Try(%s, %s)' % (self.ty(t[1][0][1][0], open_ok), self.ty(t[1][1][1][0], open_ok))
            if t[3] is not None:
                return t[3]
            parts = [self._tag_decl(n, a, open_ok) for n, a in t[1]]
            if t[2] and open_ok:
                parts.append('..')
            return '[' + ', '.join(parts) + ']'
        if k == 'R':
            return '{ ' + ', '.join('%s : %s' % (f, self.ty(x, open_ok)) for f, x in t[1]) + ' }'
        if k == 'T':
            return '(' + ', '.join(self.ty(x, open_ok) for x in t[1]) + ')'
        if k == 'L':
            return 'List(%s)' % self.ty(t[1], open_ok)
        if k == 'S':
            return 'Str'
        if k == 'N':
            return t[1]
        if k == 'V':
            return t[1]
        raise ValueError(t)

    def _tag_decl(self, name, args, open_ok=True):
        return name if not args else '%s(%s)' % (name, ', '.join(self.ty(x, open_ok) for x in args))

    def nominal_decls(self):
        return ['%s := [%s]' % (name, ', '.join(self._tag_decl(n, a) for n, a in t[1]))
                for name, t in self.nominals.items()]

    # ---------------- patterns ----------------

    def gen_pat(self, t, depth):
        r = self.rng
        t = Env(self.nominals).resolve(t)
        if depth <= 0 or r.random() < 0.25:
            return W
        k = t[0]
        if k == 'U':
            if not t[1] or (t[2] and r.random() < 0.2):
                return W
            name, args = r.choice(t[1])
            return ('C', name, tuple(self.gen_pat(a, depth - 1) for a in args))
        if k == 'R':
            fields = []
            rest = False
            for f, ft in t[1]:
                if r.random() < 0.3:
                    rest = True
                    continue
                fields.append((f, self.gen_pat(ft, depth - 1)))
            return ('Rec', tuple(fields), rest or not fields)
        if k == 'T':
            return ('Tup', tuple(self.gen_pat(e, depth - 1) for e in t[1]))
        if k == 'L':
            if r.random() < 0.5:
                return ('Lst', tuple(self.gen_pat(t[1], depth - 1) for _ in range(r.randint(0, 2))), None)
            pre = r.randint(0, 2)
            suf = r.randint(0, 1)
            return ('Lst', tuple(self.gen_pat(t[1], depth - 1) for _ in range(pre)),
                    tuple(self.gen_pat(t[1], depth - 1) for _ in range(suf)))
        if k == 'N':
            return ('Lit', r.randint(-1 if t[1] == 'I64' else 0, 2))
        if k == 'S':
            return ('Lit', r.choice(['"x"', '"y"', '""']))
        return W

    def _fresh(self, prefix):
        self.counter += 1
        return '%s%d' % (prefix, self.counter)

    def pat(self, p, t, top=False):
        """Print pattern p of type t. `top` allows an `as` binder around the whole pattern."""
        r = self.rng
        rt = Env(self.nominals).resolve(t)
        k = p[0]
        if k == 'W':
            return r.choice(['_', '_', self._fresh('v')])
        if k == 'C':
            name = p[1]
            args = dict(rt[1])[name]
            if rt[3] not in (None, 'Try') and r.random() < 0.4:
                name = '%s.%s' % (rt[3], name)
            s = name if not p[2] else '%s(%s)' % (name, ', '.join(self.pat(a, at) for a, at in zip(p[2], args)))
        elif k == 'Rec':
            ft = dict(rt[1])
            parts = ['%s: %s' % (f, self.pat(fp, ft[f])) for f, fp in p[1]]
            if p[2] or len(p[1]) < len(rt[1]):
                parts.append('..')
            s = '{ ' + ', '.join(parts) + ' }'
        elif k == 'Tup':
            s = '(' + ', '.join(self.pat(e, et) for e, et in zip(p[1], rt[1])) + ')'
        elif k == 'Lit':
            s = str(p[1])
        elif k == 'Lst':
            parts = [self.pat(e, rt[1]) for e in p[1]]
            if p[2] is not None:
                parts.append(r.choice(['..', '..', '.. as ' + self._fresh('rest')]))
                parts += [self.pat(e, rt[1]) for e in p[2]]
            s = '[' + ', '.join(parts) + ']'
        else:
            raise ValueError(p)
        if k != 'Lit' and top and r.random() < 0.08:
            s = '%s as %s' % (s, self._fresh('w'))
        return s

    # ---------------- branch lists ----------------

    def gen_branches(self, t, guards=True):
        r = self.rng
        env = Env(self.nominals)
        branches = []
        for _ in range(r.randint(1, 4)):
            alts = [self.gen_pat(t, 3) for _ in range(1 if r.random() < 0.8 else 2)]
            branches.append((alts, guards and r.random() < 0.15))
        mode = r.random()
        if mode < 0.55:
            # Complete the set with model witnesses, so many cases sit at the exhaustive boundary.
            for _ in range(6):
                w = missing_witness(env, t, [p for alts, g in branches if not g for p in alts])
                if w is None:
                    break
                branches.append(([w], False))
        if mode < 0.25 and len(branches) > 1:
            del branches[r.randrange(len(branches))]  # a near miss
        if r.random() < 0.1:
            branches.insert(r.randrange(len(branches) + 1), ([W], False))
        if r.random() < 0.1:
            i = r.randrange(len(branches))
            branches.insert(i + 1, branches[i])
        return branches[:8]


# ---------------- contexts ----------------
#
# A context is the position the patterns sit in. Each emitter appends lines through `add(line, marks)`,
# where marks are (branch, alt, column) triples recording where each alternative starts, so a
# diagnostic's line:column can be mapped back to the row it is about.

MULTI_CTX = ('match_pure', 'match_eff', 'nested_if', 'main_call_match')
STMT_CTX = ('destr_pure', 'destr_eff', 'main_call_destr', 'pure_call_destr')
SINGLE_CTX = STMT_CTX + ('arg', 'for_loop', 'lambda_arg')
CALL_CTX = ('main_call_match', 'main_call_destr', 'pure_call_destr')
CONTEXTS = MULTI_CTX + SINGLE_CTX


def strip_binders(s):
    """Alternatives in one branch must bind the same names; print them binder-free."""
    s = re.sub(r'\bv\d+\b', '_', s)
    return re.sub(r' as (w|rest)\d+', '', s)


class Emitter:
    def __init__(self, g, t, branches, ctx):
        self.g, self.t, self.branches, self.ctx = g, t, branches, ctx
        self.lines = []
        self.locs = {}
        self.T = g.ty(t, open_ok=ctx not in CALL_CTX)

    def add(self, line, marks=()):
        self.lines.append(line)
        for bi, ai, c in marks:
            self.locs[(len(self.lines), c)] = (bi, ai)

    def match_arms(self, indent):
        for bi, (alts, guarded) in enumerate(self.branches):
            ps, marks = [], []
            col = len(indent) + 1
            for ai, a in enumerate(alts):
                s = self.g.pat(a, self.t, top=len(alts) == 1)
                if len(alts) > 1:
                    s = strip_binders(s)
                marks.append((bi, ai, col))
                col += len(s) + 3
                ps.append(s)
            self.add(indent + ' | '.join(ps) + (' if n > %d' % bi if guarded else '') + ' => %d' % bi, marks)

    def single(self):
        return self.g.pat(self.branches[0][0][0], self.t, top=self.ctx not in STMT_CTX)

    def sig(self, name, args, eff):
        self.add('%s%s : %s %s U64' % (name, '!' if eff else '', args, '=>' if eff else '->'))

    def ctx_match(self, eff):
        self.sig('f', '%s, U64' % self.T, eff)
        self.add('f%s = |x, n| {' % ('!' if eff else ''))
        if eff:
            self.add('    echo!("e")')
        self.add('    match x {')
        self.match_arms('        ')
        self.add('    }')
        self.add('}')

    def ctx_destr(self, eff):
        self.sig('f', self.T, eff)
        self.add('f%s = |x| {' % ('!' if eff else ''))
        if eff:
            self.add('    echo!("e")')
        self.add('    ' + self.single() + ' = x', [(0, 0, 5)])
        self.add('    0')
        self.add('}')

    def ctx_arg(self):
        self.add('f : %s -> U64' % self.T)
        self.add('f = |' + self.single() + '| 0', [(0, 0, 6)])

    def ctx_lambda_arg(self):
        self.add('f : List(%s) -> List(U64)' % self.T)
        self.add('f = |xs| xs.map(|' + self.single() + '| 0)', [(0, 0, 18)])

    def ctx_for_loop(self):
        self.add('f : List(%s) -> U64' % self.T)
        self.add('f = |xs| {')
        self.add('    var $t = 0')
        self.add('    for ' + self.single() + ' in xs {', [(0, 0, 9)])
        self.add('        $t = $t + 1')
        self.add('    }')
        self.add('    $t')
        self.add('}')

    def ctx_nested_if(self):
        self.add('f : U64, %s -> U64' % self.T)
        self.add('f = |n, x| if n > 100 0 else {')
        self.add('    g = |y| y + 1')
        self.add('    g(match x {')
        self.match_arms('        ')
        self.add('    })')
        self.add('}')

    def ctx_call(self):
        # The scrutinee is a call result, so its structural unions are in output position (POL).
        self.add('mk : U64 -> %s' % self.T)
        self.add('mk = |_| crash "no"')
        self.add('')
        if self.ctx == 'pure_call_destr':
            self.add('f : U64 -> U64')
            self.add('f = |n| {')
            self.add('    ' + self.single() + ' = mk(n)', [(0, 0, 5)])
            self.add('    0')
            self.add('}')
            self.add('')
            self.add('main! = |args| {')
            self.add('    _ = f(List.len(args))')
            self.add('    Ok({})')
            self.add('}')
            return
        self.add('main! = |args| {')
        self.add('    n = List.len(args)')
        if self.ctx == 'main_call_destr':
            self.add('    ' + self.single() + ' = mk(n)', [(0, 0, 5)])
        else:
            self.add('    r = match mk(n) {')
            self.match_arms('        ')
            self.add('    }')
            self.add('    echo!(r.to_str())')
        self.add('    echo!("done")')
        self.add('    Ok({})')
        self.add('}')

    def emit(self):
        for d in self.g.nominal_decls():
            self.add(d)
        if self.g.nominals:
            self.add('')
        c = self.ctx
        if c in ('match_pure', 'match_eff'):
            self.ctx_match(c == 'match_eff')
        elif c in ('destr_pure', 'destr_eff'):
            self.ctx_destr(c == 'destr_eff')
        elif c in CALL_CTX:
            self.ctx_call()
        else:
            getattr(self, 'ctx_' + c)()
        if c not in CALL_CTX:
            self.add('')
            self.add('main! = |_args| Ok({})')
        return '\n'.join(self.lines) + '\n', self.locs


def emit(g, t, branches, ctx):
    """Return (source, locs) where locs maps (line, column) of each alternative to (branch, alt)."""
    return Emitter(g, t, branches, ctx).emit()


def render(case, seed=0):
    """Emit a case's program again from its (ctx, type, nominals, branches)."""
    g = Gen(random.Random(seed))
    g.nominals = dict(case['nominals'])
    return emit(g, case['type'], case['branches'], case['ctx'])


# ---------------- whole cases ----------------

def make_case(seed, ctx=None):
    rng = random.Random(seed)
    g = Gen(rng)
    ctx = ctx or rng.choice(CONTEXTS)
    if ctx in CALL_CTX or ctx in ('lambda_arg', 'for_loop', 'nested_if'):
        g.allow_var = False
    if ctx == 'lambda_arg':
        g.allow_open = False
    t = g.gen_type(rng.choice([1, 2, 2, 3]))
    if ctx in CALL_CTX:
        t = close_all(t)
        g.nominals = {k: close_all(v, False) for k, v in g.nominals.items()}
    if ctx in SINGLE_CTX:
        p = g.gen_pat(t, 3)
        if rng.random() < 0.5:
            w = missing_witness(Env(g.nominals), t, [])  # often an irrefutable pattern
            p = w if w is not None else p
        if ctx in STMT_CTX:
            p = stmt_safe(p)
        branches = [([p], False)]
    else:
        branches = g.gen_branches(t, guards=ctx != 'main_call_match')
    src, locs = emit(g, t, branches, ctx)
    return dict(seed=seed, ctx=ctx, type=t, nominals=g.nominals, branches=branches,
                expected=analyze(Env(g.nominals), t, branches), src=src, locs=locs)


def strip_open_var(t):
    k = t[0]
    if k == 'U':
        return ('U', tuple((n, tuple(strip_open_var(a) for a in args)) for n, args in t[1]),
                False if t[2] is True else t[2], t[3])
    if k == 'R':
        return ('R', tuple((f, strip_open_var(x)) for f, x in t[1]))
    if k == 'T':
        return ('T', tuple(strip_open_var(x) for x in t[1]))
    if k == 'L':
        return ('L', strip_open_var(t[1]))
    if k == 'V':
        return U64
    return t


def stmt_safe(p):
    """A destructuring statement cannot start with a list pattern, a bare tag or a literal: `[..] = xs`,
    `Unit = u` and `1 = x` do not parse. Use a wildcard there instead."""
    if p[0] in ('Lst', 'Lit') or (p[0] == 'C' and not p[2]):
        return W
    return p


def close_all(t, reach=True):
    """Mark output-position unions (design.md Polarity). A structural union with tags reached through
    structural types or `Try` is implicitly open with a flex tail ('flex', closed for the reference).
    A user nominal's backing is fixed by its declaration, so `reach` stops there."""
    k = t[0]
    if k == 'U':
        structural = t[3] is None
        mark = 'flex' if (structural and reach and t[1]) else ('flex' if t[2] == 'flex' else False)
        inner = reach and (structural or t[3] == 'Try')
        return ('U', tuple((n, tuple(close_all(a, inner) for a in args)) for n, args in t[1]), mark, t[3])
    if k == 'R':
        return ('R', tuple((f, close_all(x, reach)) for f, x in t[1]))
    if k == 'T':
        return ('T', tuple(close_all(x, reach) for x in t[1]))
    if k == 'L':
        return ('L', close_all(t[1], reach))
    return t
