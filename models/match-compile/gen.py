"""Random generator: types, values, patterns, guards, branch lists, and the Roc program that runs them.

A program holds a few units. Each unit is one scrutinee type, one `match` over it (with guards,
alternatives, `as` and variables), and a list of small values of the type, each paired with an
argument `k` for the guards and the arm and bindings the model expects. Everything is composed from
small typed pieces at random: gen_type builds a type, gen_pat a pattern of a type, sample_value and
value_for values of a type, gen_branches an ordered branch list.
"""

import random

import model
import usefulness as U
from model import Env

W = ('W',)
BOOL = ('U', (('False', ()), ('True', ())), 'Bool')
INTS = ('U8', 'I64', 'U64')
TAGS = ['A', 'B', 'C', 'D']
FIELDS = ['a', 'b', 'c']
STR_ATOMS = ['a', 'b', 'ab', '/', 'é']
INT_EDGES = {'U8': [0, 1, 2, 255], 'I64': [-1, 0, 1, 2, -9223372036854775808, 9223372036854775807],
             'U64': [0, 1, 2, 18446744073709551615]}


def TRY(a, b):
    return ('U', (('Ok', (a,)), ('Err', (b,))), 'Try')


class Gen:
    def __init__(self, rng, prefix=''):
        self.rng = rng
        self.decls = {}  # name -> (decl, opaque)
        self.prefix = prefix
        self.names = 0

    def env(self):
        return Env(self.decls)

    # ---------------------------------------------------------------- types

    def gen_type(self, depth):
        r = self.rng
        if depth <= 0:
            return r.choice([('N', r.choice(INTS)), ('S',), BOOL, ('U', (('A', ()), ('B', ())), None)])
        kinds = ['union', 'try', 'bool', 'nom', 'rec_nom', 'wrap', 'record', 'tuple', 'list', 'int', 'str']
        weights = [12, 5, 2, 5, 4, 4, 6, 6, 7, 5, 4]
        k = r.choices(kinds, weights)[0]
        if k == 'union':
            return ('U', self._tags(depth, '', ''), None)
        if k == 'try':
            return TRY(self.gen_type(depth - 1), self.gen_type(depth - 1))
        if k == 'bool':
            return BOOL
        if k == 'nom':
            name = self._nom_name('N')
            self.decls[name] = (('union', self._tags(depth, '', str(len(self.decls)))), r.random() < 0.3)
            return ('NR', name)
        if k == 'rec_nom':
            name = self._nom_name('Rec')
            i = str(len(self.decls))
            leaf = self.gen_type(0)
            if r.random() < 0.5:
                tags = (('Leaf' + i, ()), ('Node' + i, (('NR', name), leaf)))
            else:
                tags = (('Nil' + i, ()), ('One' + i, (leaf,)), ('Two' + i, (('NR', name), ('NR', name))))
            self.decls[name] = (('union', tags), r.random() < 0.3)
            return ('NR', name)
        if k == 'wrap':
            name = self._nom_name('W')
            backing = r.choice([  # gen_type(0) adds no nominals, so the order of evaluation is safe
('N', r.choice(INTS)), ('S',),
                                ('T', (self.gen_type(0), self.gen_type(0))),
                                ('R', tuple((FIELDS[j], self.gen_type(0)) for j in range(r.randint(1, 2))))])
            self.decls[name] = (('wrap', backing), r.random() < 0.3)
            return ('NR', name)
        if k == 'record':
            return ('R', tuple((FIELDS[j], self.gen_type(depth - 1)) for j in range(r.randint(1, 3))))
        if k == 'tuple':
            return ('T', tuple(self.gen_type(depth - 1) for _ in range(r.randint(2, 3))))
        if k == 'list':
            return ('L', self.gen_type(depth - 1))
        if k == 'int':
            return ('N', r.choice(INTS))
        return ('S',)

    def _tags(self, depth, pre, suf):
        r = self.rng
        return tuple((pre + TAGS[j] + suf, tuple(self.gen_type(depth - 1) for _ in range(r.choice([0, 0, 1, 1, 2]))))
                     for j in range(r.randint(1, 4)))

    def _nom_name(self, base):
        """A fresh name, reserved at once so nominals built inside this one's declaration get others."""
        name = '%s%s%d' % (base, self.prefix, len(self.decls) + 1)
        self.decls[name] = None
        return name

    # ---------------------------------------------------------------- printing types

    def ty(self, t):
        k = t[0]
        if k == 'NR':
            return t[1]
        if k == 'U':
            if t[2] == 'Bool':
                return 'Bool'
            if t[2] == 'Try':
                return 'Try(%s, %s)' % (self.ty(t[1][0][1][0]), self.ty(t[1][1][1][0]))
            if t[2] is not None:
                return t[2]
            return '[' + ', '.join(self._tag_decl(n, a) for n, a in t[1]) + ']'
        if k == 'R':
            if not t[1]:
                return '{}'
            return '{ ' + ', '.join('%s : %s' % (f, self.ty(x)) for f, x in t[1]) + ' }'
        if k == 'T':
            return '(' + ', '.join(self.ty(x) for x in t[1]) + ')'
        if k == 'L':
            return 'List(%s)' % self.ty(t[1])
        if k == 'S':
            return 'Str'
        if k == 'N':
            return t[1]
        raise ValueError(t)

    def _tag_decl(self, name, args):
        return name if not args else '%s(%s)' % (name, ', '.join(self.ty(x) for x in args))

    def decl_lines(self):
        out = []
        for name, ((kind, body), opaque) in self.decls.items():
            op = '::' if opaque else ':='
            if kind == 'union':
                out.append('%s %s [%s].{ is_eq : _ }' % (name, op, ', '.join(self._tag_decl(n, a) for n, a in body)))
            else:
                out.append('%s %s %s.{ is_eq : _ }' % (name, op, self.ty(body)))
        return out

    # ---------------------------------------------------------------- values

    def sample_value(self, t, depth, pool):
        """A small random value of t. pool: {'int': [...], 'str': [...]} literals the patterns mention."""
        r = self.rng
        env = self.env()
        rt = env.resolve(t)
        k = rt[0]
        if k == 'U':
            tags = rt[1]
            if depth <= 0:
                # Prefer a tag that does not recurse, so the value stays finite.
                flat = [tg for tg in tags if not any(self._recursive(a) for a in tg[1])]
                tags = flat or tags
            name, args = r.choice(tags)
            return ('tag', name, tuple(self.sample_value(a, depth - 1, pool) for a in args))
        if k == 'Wrap':
            return ('wrap', rt[1], self.sample_value(rt[2], depth, pool))
        if k == 'R':
            return ('rec', tuple((f, self.sample_value(x, depth - 1, pool)) for f, x in rt[1]))
        if k == 'T':
            return ('tup', tuple(self.sample_value(x, depth - 1, pool) for x in rt[1]))
        if k == 'L':
            n = r.choice([0, 1, 1, 2, 2, 3, 4]) if depth > 0 else r.choice([0, 1])
            return ('list', tuple(self.sample_value(rt[1], depth - 1, pool) for _ in range(n)))
        if k == 'N':
            cands = INT_EDGES[rt[1]] + [v for v in pool.get(rt[1], [])]
            return ('int', r.choice(cands))
        if k == 'S':
            cands = ['', 'a', 'ab', 'é/b'] + pool.get('str', [])
            return ('str', r.choice(cands))
        raise ValueError(t)

    def _recursive(self, t):
        return t[0] == 'NR' and self.decls[t[1]][0][0] == 'union' and any(
            a == t for _, args in self.decls[t[1]][0][1] for a in args)

    def value_for(self, p, t, pool):
        """A value of t that pattern p matches (wildcards and variables are filled at random)."""
        r = self.rng
        env = self.env()
        rt = env.resolve(t)
        k = p[0]
        if k in ('W', 'B'):
            return self.sample_value(t, 2, pool)
        if k == 'As':
            return self.value_for(p[1], t, pool)
        if k == 'C':
            args = dict(rt[1])[p[1]]
            return ('tag', p[1], tuple(self.value_for(sp, at, pool) for sp, at in zip(p[2], args)))
        if k == 'Rec':
            given = dict(p[1])
            return ('rec', tuple((f, self.value_for(given[f], x, pool) if f in given else self.sample_value(x, 1, pool))
                                 for f, x in rt[1]))
        if k == 'Tup':
            return ('tup', tuple(self.value_for(sp, x, pool) for sp, x in zip(p[1], rt[1])))
        if k == 'Lst':
            pre = [self.value_for(sp, rt[1], pool) for sp in p[1]]
            if p[2] is None:
                return ('list', tuple(pre))
            mid = [self.sample_value(rt[1], 1, pool) for _ in range(r.choice([0, 0, 1, 2]))]
            suf = [self.value_for(sp, rt[1], pool) for sp in p[2][1]]
            return ('list', tuple(pre + mid + suf))
        if k == 'Lit':
            return p[1]
        if k == 'SP':
            parts = p[1]
            s = parts[0]
            for i in range(1, len(parts), 2):
                s += ''.join(r.choice(STR_ATOMS + [parts[i + 1] or 'x']) for _ in range(r.choice([0, 1, 2])))
                s += parts[i + 1]
            return ('str', s)
        if k == 'Wrap':
            return ('wrap', p[1], self.value_for(p[2], rt[2], pool))
        raise ValueError(p)

    # ---------------------------------------------------------------- patterns

    def fresh(self, base='x'):
        self.names += 1
        return '%s%d' % (base, self.names)

    def gen_pat(self, t, depth, pool, top=False):
        r = self.rng
        env = self.env()
        rt = env.resolve(t)
        if depth <= 0 or r.random() < (0.05 if top else 0.25):
            return ('B', self.fresh()) if r.random() < 0.5 else W
        k = rt[0]
        p = None
        if k == 'U':
            name, args = r.choice(rt[1])
            p = ('C', name, tuple(self.gen_pat(a, depth - 1, pool) for a in args))
        elif k == 'Wrap':
            inner = self.gen_pat(rt[2], depth, pool)
            if inner[0] in ('W', 'B'):
                return inner  # a variable matches the nominal itself
            p = ('Wrap', rt[1], inner)
        elif k == 'R':
            fields = []
            dropped = False
            for f, ft in rt[1]:
                if r.random() < 0.3:
                    dropped = True
                    continue
                fields.append((f, self.gen_pat(ft, depth - 1, pool)))
            rest = None
            if dropped or not fields or r.random() < 0.2:
                rest = self.fresh('rest') if r.random() < 0.3 else ''
            p = ('Rec', tuple(fields), rest)
        elif k == 'T':
            p = ('Tup', tuple(self.gen_pat(e, depth - 1, pool) for e in rt[1]))
        elif k == 'L':
            pre = tuple(self.gen_pat(rt[1], depth - 1, pool) for _ in range(r.randint(0, 2)))
            if r.random() < 0.45:
                p = ('Lst', pre, None)
            else:
                suf = tuple(self.gen_pat(rt[1], depth - 1, pool) for _ in range(r.randint(0, 1)))
                p = ('Lst', pre, (self.fresh('rest') if r.random() < 0.3 else None, suf))
        elif k == 'N':
            v = r.choice(INT_EDGES[rt[1]][:3] + [3, 7])
            if rt[1] == 'I64' and r.random() < 0.3:
                v = -v
            pool.setdefault(rt[1], []).append(v)
            p = ('Lit', ('int', v))
        elif k == 'S':
            if r.random() < 0.5:
                s = r.choice(['', 'a', 'ab', 'b/', 'é'])
                pool.setdefault('str', []).append(s)
                p = ('Lit', ('str', s))
            else:
                p = ('SP', self.str_pattern())
        if p is not None and p[0] not in ('Lit',) and r.random() < 0.08:
            p = ('As', p, self.fresh('w'))
        return p

    def str_pattern(self):
        """Literal pieces around one or two captures. Two captures always have text between them."""
        r = self.rng
        n = r.choice([1, 1, 2])
        parts = [r.choice(['', '', 'a', 'ab', '/'])]
        for i in range(n):
            parts.append(self.fresh('s') if r.random() < 0.7 else None)
            last = i == n - 1
            parts.append(r.choice(['', '/', '.b', 'a']) if last else r.choice(['/', 'a', '.b']))
        return tuple(parts)

    # ---------------------------------------------------------------- branches

    def gen_branches(self, t, pool):
        """An ordered list of (alternatives, guard). Completed with model witnesses or a final variable
        often enough that most matches are exhaustive without a catch-all."""
        r = self.rng
        env = self.env()
        branches = []
        for _ in range(r.randint(1, 5)):
            n_alts = 1 if r.random() < 0.75 else r.randint(2, 3)
            alts = [self.gen_pat(t, 3, pool, top=True) for _ in range(n_alts)]
            if n_alts > 1:
                alts = self.share_binders(alts, t)
            guard = self.gen_guard(env, alts[0], t, pool) if r.random() < 0.3 else None
            branches.append((alts, guard))
        uenv, ut = to_usefulness(env, t)
        rows = [to_upat(p) for alts, g in branches if g is None for p in alts]
        for _ in range(8):
            w = U.missing_witness(uenv, ut, rows)
            if w is None:
                break
            if r.random() < 0.3:
                branches.append(([('B', self.fresh())], None))
                break
            p = fix_wraps(env, from_upat(w), t)
            branches.append(([p], None))
            rows.append(to_upat(p))
        else:
            branches.append(([W], None))
        if U.missing_witness(uenv, ut, [to_upat(p) for alts, g in branches if g is None for p in alts]) is not None:
            branches.append(([W], None))
        return branches

    def gen_guard(self, env, p, t, pool):
        r = self.rng
        bt = model.binder_types(env, p, t)
        opts = [('k', r.choice([0, 1]))]
        for name, x in bt.items():
            rx = env.resolve(x)
            if rx[0] == 'N':
                opts.append(('gt', name, r.choice([0, 1, 2]), rx[1]))
            elif rx[0] == 'S':
                opts.append(('seq', name, r.choice(['', 'a', 'ab'] + pool.get('str', [])[:2])))
            elif rx[0] == 'L':
                opts.append(('len', name, r.choice([0, 1, 2])))
        g = r.choice(opts)
        if g[0] == 'gt':
            g = g[:3]
        return g

    def share_binders(self, alts, t):
        """Alternatives must bind the same names at the same types (langref "Branch Alternatives").
        Remove every variable, then name one or two slots of a type that every alternative has."""
        env = self.env()
        alts = [strip_binders(p) for p in alts]
        slot_lists = [list(slots(env, p, t, ())) for p in alts]
        shared = []
        used = [set() for _ in alts]
        for path0, ty0 in slot_lists[0]:
            if len(shared) == 2 or self.rng.random() < 0.4:
                continue
            picks = []
            for i, sl in enumerate(slot_lists[1:], 1):
                cands = [path for path, ty in sl if ty == ty0 and path not in used[i]]
                if not cands:
                    break
                picks.append(self.rng.choice(cands))
            else:
                used[0].add(path0)
                for i, path in enumerate(picks, 1):
                    used[i].add(path)
                shared.append((path0, picks))
        out = list(alts)
        for path0, picks in shared:
            name = self.fresh('y')
            out[0] = set_at(out[0], path0, ('B', name))
            for i, path in enumerate(picks):
                out[i + 1] = set_at(out[i + 1], path, ('B', name))
        return out


# ---------------------------------------------------------------- pattern paths

def strip_binders(p):
    k = p[0]
    if k == 'B':
        return W
    if k == 'As':
        return strip_binders(p[1])
    if k == 'C':
        return ('C', p[1], tuple(strip_binders(x) for x in p[2]))
    if k == 'Rec':
        return ('Rec', tuple((f, strip_binders(x)) for f, x in p[1]), '' if p[2] is not None else None)
    if k == 'Tup':
        return ('Tup', tuple(strip_binders(x) for x in p[1]))
    if k == 'Lst':
        rest = None if p[2] is None else (None, tuple(strip_binders(x) for x in p[2][1]))
        return ('Lst', tuple(strip_binders(x) for x in p[1]), rest)
    if k == 'SP':
        return ('SP', tuple(x if i % 2 == 0 else None for i, x in enumerate(p[1])))
    if k == 'Wrap':
        return ('Wrap', p[1], strip_binders(p[2]))
    return p


def children(p):
    """(index, child) pairs of pattern p."""
    k = p[0]
    if k == 'C':
        return [((2, i), x) for i, x in enumerate(p[2])]
    if k == 'Rec':
        return [((1, i, 1), x) for i, (_, x) in enumerate(p[1])]
    if k == 'Tup':
        return [((1, i), x) for i, x in enumerate(p[1])]
    if k == 'Lst':
        out = [((1, i), x) for i, x in enumerate(p[1])]
        if p[2] is not None:
            out += [((2, 1, i), x) for i, x in enumerate(p[2][1])]
        return out
    if k == 'Wrap':
        return [((2,), p[2])]
    if k == 'As':
        return [((1,), p[1])]
    return []


def slots(env, p, t, path):
    """(path, type) of every wildcard position in p."""
    if p[0] == 'W':
        yield path, t
        return
    rt = env.resolve(t)
    k = p[0]
    for idx, c in children(p):
        if k == 'C':
            ct = dict(rt[1])[p[1]][idx[1]]
        elif k == 'Rec':
            ct = dict(rt[1])[p[1][idx[1]][0]]
        elif k == 'Tup':
            ct = rt[1][idx[1]]
        elif k == 'Lst':
            ct = rt[1]
        elif k == 'Wrap':
            ct = rt[2]
        else:
            ct = t
        yield from slots(env, c, ct, path + (idx,))


def is_prefix(a, b):
    return len(a) <= len(b) and b[:len(a)] == a


def set_in(obj, idx, value):
    if not idx:
        return value
    lst = list(obj)
    lst[idx[0]] = set_in(obj[idx[0]], idx[1:], value)
    return tuple(lst)


def get_in(obj, idx):
    for j in idx:
        obj = obj[j]
    return obj


def set_at(p, path, new):
    """Replace the subpattern at path (a sequence of index tuples from children())."""
    if not path:
        return new
    return set_in(p, path[0], set_at(get_in(p, path[0]), path[1:], new))


def get_at(p, path):
    for idx in path:
        for j in idx:
            p = p[j]
    return p


# ---------------------------------------------------------------- projection to the usefulness model

def to_usefulness(env, t):
    """The exhaustive model's (Env, type) for t. Wrap nominals become their backing; Str and number
    literal families stay infinite."""
    noms = {}

    def conv(x):
        k = x[0]
        if k == 'NR':
            d, _ = env.decls[x[1]]
            if d[0] == 'wrap':
                return conv(d[1])
            if x[1] not in noms:
                noms[x[1]] = None
                noms[x[1]] = ('U', tuple((n, tuple(conv(a) for a in args)) for n, args in d[1]), False, x[1])
            return ('NR', x[1])
        if k == 'U':
            return ('U', tuple((n, tuple(conv(a) for a in args)) for n, args in x[1]), False, x[2])
        if k == 'R':
            return ('R', tuple((f, conv(y)) for f, y in x[1]))
        if k == 'T':
            return ('T', tuple(conv(y) for y in x[1]))
        if k == 'L':
            return ('L', conv(x[1]))
        if k == 'N':
            return ('N', 'U64' if x[1] != 'I64' else 'I64')
        return x
    ut = conv(t)
    return U.Env(noms), ut


def to_upat(p):
    """A string pattern with captures covers less than a wildcard, so it becomes a literal no value equals."""
    k = p[0]
    if k in ('W', 'B'):
        return U.W
    if k == 'As':
        return to_upat(p[1])
    if k == 'C':
        return ('C', p[1], tuple(to_upat(x) for x in p[2]))
    if k == 'Rec':
        return ('Rec', tuple((f, to_upat(x)) for f, x in p[1]), p[2] is not None)
    if k == 'Tup':
        return ('Tup', tuple(to_upat(x) for x in p[1]))
    if k == 'Lst':
        return ('Lst', tuple(to_upat(x) for x in p[1]), None if p[2] is None else tuple(to_upat(x) for x in p[2][1]))
    if k == 'Lit':
        return ('Lit', p[1])
    if k == 'SP':
        return ('Lit', ('sp', p[1]))
    if k == 'Wrap':
        return to_upat(p[2])
    raise ValueError(p)


def from_upat(w, env=None, t=None):
    """A model witness back as a pattern of ours (wraps are restored by fix_wraps)."""
    k = w[0]
    if k == 'W':
        return W
    if k == 'C':
        return ('C', w[1], tuple(from_upat(x) for x in w[2]))
    if k == 'Rec':
        return ('Rec', tuple((f, from_upat(x)) for f, x in w[1]), '' if w[2] else None)
    if k == 'Tup':
        return ('Tup', tuple(from_upat(x) for x in w[1]))
    if k == 'Lst':
        return ('Lst', tuple(from_upat(x) for x in w[1]), None if w[2] is None else (None, tuple(from_upat(x) for x in w[2])))
    if k == 'Lit':
        return ('Lit', w[1])
    raise ValueError(w)


def fix_wraps(env, p, t):
    """Put back the `N.(...)` around patterns of wrap nominals, which the usefulness model erases."""
    rt = env.resolve(t)
    k = p[0]
    if rt[0] == 'Wrap' and k not in ('W', 'B', 'Wrap', 'As'):
        return ('Wrap', rt[1], fix_wraps(env, p, rt[2]))
    if k == 'As':
        return ('As', fix_wraps(env, p[1], t), p[2])
    if k == 'Wrap':
        return ('Wrap', p[1], fix_wraps(env, p[2], rt[2]))
    if k == 'C':
        return ('C', p[1], tuple(fix_wraps(env, x, a) for x, a in zip(p[2], dict(rt[1])[p[1]])))
    if k == 'Rec':
        ft = dict(rt[1])
        return ('Rec', tuple((f, fix_wraps(env, x, ft[f])) for f, x in p[1]), p[2])
    if k == 'Tup':
        return ('Tup', tuple(fix_wraps(env, x, a) for x, a in zip(p[1], rt[1])))
    if k == 'Lst':
        rest = None if p[2] is None else (p[2][0], tuple(fix_wraps(env, x, rt[1]) for x in p[2][1]))
        return ('Lst', tuple(fix_wraps(env, x, rt[1]) for x in p[1]), rest)
    return p
