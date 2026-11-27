"""Reference exhaustiveness and redundancy checker for Roc patterns (Maranget 2007 usefulness).

Types are nested tuples:
  ('U', tags, open, nom)   tag union. tags: tuple of (Name, tuple[type]).
                           open: False (closed), True (rigid `..`), or 'flex' (implicitly open; closed for the
                           reference, used only by the 'flex' emulation). nom: None, 'Try', 'Bool' or a user nominal name.
  ('NR', name)             reference to a user nominal (resolved through Env, so it can be recursive)
  ('R', ((field, type), ...))
  ('T', (type, ...))
  ('L', elem)
  ('S',)                   Str
  ('N', 'U64' | 'I64')
  ('V', name)              rigid type variable

Patterns:
  ('W',)                         wildcard or binder (`as` is transparent)
  ('C', tag, (pats))             tag
  ('Rec', ((field, pat), ...), rest)  missing fields are wildcards; rest records a printed `..`
  ('Tup', (pats))
  ('Lit', value)                 int or quoted string
  ('Lst', (prefix), suffix)      suffix None: exact list; else a tuple, `[prefix.., .., suffix..]`

Rule names (INHAB, CTOR-U, CTOR-List, U, EXH, UNM, RED, RED-G) are defined in RULES.md.
"""

W = ('W',)

# Known compiler bugs the model can emulate. They are used only to sort fuzzer disagreements into
# buckets; the reference answer is always computed with no flags.
KNOWN_BUGS = {
    'uninh_sibling': 13289,
    'empty_base': 13290,
    'flex': '13291, 13294',
    'guard_first': 13292,
}


class Env:
    def __init__(self, nominals):
        # nominals: name -> ('U', tags, False, name)
        self.nominals = dict(nominals)
        self._known = None

    def resolve(self, t):
        while t[0] == 'NR':
            t = self.nominals[t[1]]
        return t

    def nominal_inhabited(self):
        """Rule INHAB: greatest fixed point over the nominals (start from 'all inhabited', remove until stable)."""
        if self._known is None:
            known = {n: True for n in self.nominals}
            changed = True
            while changed:
                changed = False
                for n, t in self.nominals.items():
                    if known[n] and not self._inhabited_with(t, known):
                        known[n] = False
                        changed = True
            self._known = known
        return self._known

    def _inhabited_with(self, t, known):
        k = t[0]
        if k == 'NR':
            return known[t[1]]
        if k == 'U':
            if t[2] is True:
                return True  # a rigid open tail may hold any tag
            return any(all(self._inhabited_with(a, known) for a in args) for _, args in t[1])
        if k == 'R':
            return all(self._inhabited_with(ft, known) for _, ft in t[1])
        if k == 'T':
            return all(self._inhabited_with(e, known) for e in t[1])
        return True  # List, Str, numbers, type variables

    def inhabited(self, t):
        return self._inhabited_with(t, self.nominal_inhabited())


# ---------- constructor families ----------

def list_family(col_pats):
    """Rule CTOR-List: exact lengths 0..L-1 plus one variable-length class."""
    max_exact = -1
    max_pre = 0
    max_suf = 0
    for p in col_pats:
        if p[0] != 'Lst':
            continue
        if p[2] is None:
            max_exact = max(max_exact, len(p[1]))
        else:
            max_pre = max(max_pre, len(p[1]))
            max_suf = max(max_suf, len(p[2]))
    lvar = max(max_exact + 1, max_pre + max_suf)
    return [('len', k) for k in range(lvar)] + [('var', max_pre, max_suf)]


def ctor_arity_types(env, t, c):
    t = env.resolve(t)
    if c[0] == 'tag':
        return list(dict(t[1])[c[1]])
    if c[0] == 'rec':
        return [ft for _, ft in t[1]]
    if c[0] == 'tup':
        return list(t[1])
    if c[0] == 'lit':
        return []
    if c[0] == 'len':
        return [t[1]] * c[1]
    if c[0] == 'var':
        return [t[1]] * (c[1] + c[2])
    raise ValueError(c)


def head_ctor(p):
    """The constructor a non-wildcard, non-list pattern covers."""
    if p[0] == 'C':
        return ('tag', p[1])
    if p[0] == 'Rec':
        return ('rec',)
    if p[0] == 'Tup':
        return ('tup',)
    if p[0] == 'Lit':
        return ('lit', p[1])
    raise ValueError(p)


def specialize_pat(env, t, p, c):
    """Argument patterns of p under constructor c, or None if p does not match c."""
    t = env.resolve(t)
    if p[0] == 'W':
        return [W] * len(ctor_arity_types(env, t, c))
    if c[0] == 'tag':
        return list(p[2]) if p[0] == 'C' and p[1] == c[1] else None
    if c[0] == 'rec':
        d = dict(p[1])
        return [d.get(f, W) for f, _ in t[1]]
    if c[0] == 'tup':
        return list(p[1])
    if c[0] == 'lit':
        return [] if p[1] == c[1] else None
    pre, suf = p[1], p[2]
    if c[0] == 'len':
        n = c[1]
        if suf is None:
            return list(pre) if len(pre) == n else None
        if len(pre) + len(suf) <= n:
            return list(pre) + [W] * (n - len(pre) - len(suf)) + list(suf)
        return None
    if c[0] == 'var':
        _, mp, ms = c
        if suf is None:
            return None
        return list(pre) + [W] * (mp - len(pre)) + [W] * (ms - len(suf)) + list(suf)
    raise ValueError(c)


def full_family(env, t, col_pats):
    """Rule CTOR-U: the inhabited constructors of t if the family is finite, else None."""
    t = env.resolve(t)
    k = t[0]
    if k == 'U':
        if t[2] is True:
            return None
        return [('tag', name) for name, args in t[1] if all(env.inhabited(a) for a in args)]
    if k in ('R', 'T'):
        return [(('rec',) if k == 'R' else ('tup',))] if env.inhabited(t) else []
    if k == 'L':
        if not env.inhabited(t[1]):
            return [('len', 0)]  # only the empty list exists
        return list_family(col_pats)
    return None  # Str, numbers, type variables: infinite


def useful(env, P, q, ts, compat=frozenset(), mode='red'):
    """Rule U: is row q useful with respect to matrix P (every row has len(ts) columns)?

    compat: known-bug emulations (KNOWN_BUGS); empty for the reference.
    mode: 'red' for a redundancy query, 'exh' for an exhaustiveness query (q is all wildcards)."""
    if 'empty_base' in compat and mode == 'exh' and not P:
        return True
    if not ts:
        return len(P) == 0
    t = env.resolve(ts[0])
    h = q[0]
    if h[0] == 'Lst':
        fam = list_family([r[0] for r in P] + [h])
        if not env.inhabited(t[1]):
            fam = [('len', 0)]
        cs = [c for c in fam if specialize_pat(env, t, h, c) is not None]
        return any(_useful_ctor(env, P, q, ts, t, c, compat, mode) for c in cs)
    if h[0] != 'W':
        return _useful_ctor(env, P, q, ts, t, head_ctor(h), compat, mode)
    col = [r[0] for r in P]
    fam = full_family(env, t, col + [h])
    D = [r[1:] for r in P if r[0][0] == 'W']
    if fam is not None:
        present = {head_ctor(p) for p in col if p[0] not in ('W', 'Lst')}
        if t[0] == 'U' and t[2] == 'flex' and 'flex' in compat and present:
            # 13291 / 13294: on a flex row a wildcard is always useful, and when only the open tail is
            # missing the found constructors' payloads are never checked.
            if mode == 'red':
                return True
            missing = [c for c in fam if c not in present]
            if any(p[0] == 'W' for p in col) or missing:
                return bool(missing) and useful(env, D, q[1:], ts[1:], compat, mode)
        if (t[0] == 'U' and 'uninh_sibling' in compat and all(c in present for c in fam)
                and len(fam) < len(t[1]) and len(present) < len(t[1])):
            # 13289: a missing uninhabited sibling makes the family look incomplete, so the found
            # constructors' payloads are never checked.
            return False
        if t[0] == 'L' or all(c in present for c in fam):
            return any(_useful_ctor(env, P, q, ts, t, c, compat, mode) for c in fam)
    if not env.inhabited(t):
        return False
    return useful(env, D, q[1:], ts[1:], compat, mode)


def _useful_ctor(env, P, q, ts, t, c, compat, mode):
    SP = []
    for r in P:
        s = specialize_pat(env, t, r[0], c)
        if s is not None:
            SP.append(s + list(r[1:]))
    sq = specialize_pat(env, t, q[0], c)
    return useful(env, SP, sq + list(q[1:]), ctor_arity_types(env, t, c) + list(ts[1:]), compat, mode)


def analyze(env, t, branches, compat=frozenset()):
    """branches: list of (alternatives: list[pattern], guarded: bool).

    Returns dict(exhaustive, redundant, unmatchable, redundant_guarded), each list holding (branch, alt).
    EXH: exhaustive iff [_] is not useful against the unguarded rows.
    UNM: a row is unmatchable iff it is not useful against the empty matrix.
    RED: an unguarded matchable row is redundant iff it is not useful against earlier unguarded rows.
    RED-G: a guarded row covered by earlier unguarded rows; kept apart because the compiler treats
           guarded rows as always useful, and not compared.
    """
    prev = []
    red, unm, red_g = [], [], []
    for bi, (alts, guarded) in enumerate(branches):
        for ai, p in enumerate(alts):
            row = [p]
            if not useful(env, [], row, [t]):
                unm.append((bi, ai))
                continue
            if not useful(env, prev, row, [t], compat):
                (red_g if guarded else red).append((bi, ai))
                continue
            if not guarded:
                prev.append(row)
    exh = not useful(env, prev, [W], [t], compat, 'exh')
    if 'guard_first' in compat and any(g for _a, g in branches):
        # 13292: the guard wrapper reads the payload of the scrutinee's first tag (in name order);
        # if that payload is uninhabited the match is accepted as exhaustive.
        rt = env.resolve(t)
        if rt[0] == 'U' and rt[1]:
            first = sorted(rt[1])[0]
            if not all(env.inhabited(a) for a in first[1]):
                exh = True
    return dict(exhaustive=exh, redundant=red, unmatchable=unm, redundant_guarded=red_g)


def missing_witness(env, t, rows):
    """A pattern of type t that no row covers, or None. The generator uses it to build near-exhaustive sets."""
    P = [[p] for p in rows]
    if not useful(env, P, [W], [t]):
        return None
    return _witness(env, P, [t])[0]


def _witness(env, P, ts):
    """One pattern per column not covered by P; assumes [_]*len(ts) is useful against P."""
    if not ts:
        return []
    t = env.resolve(ts[0])
    col = [r[0] for r in P]
    fam = full_family(env, t, col)
    present = {head_ctor(p) for p in col if p[0] not in ('W', 'Lst')}
    if fam is not None and (t[0] == 'L' or all(c in present for c in fam)):
        for c in fam:
            args_t = ctor_arity_types(env, t, c)
            SP = []
            for r in P:
                s = specialize_pat(env, t, r[0], c)
                if s is not None:
                    SP.append(s + list(r[1:]))
            n = len(args_t)
            if useful(env, SP, [W] * (n + len(ts) - 1), args_t + list(ts[1:])):
                w = _witness(env, SP, args_t + list(ts[1:]))
                return [rebuild(t, c, w[:n])] + w[n:]
        raise AssertionError('no witness')
    D = [r[1:] for r in P if r[0][0] == 'W']
    rest = _witness(env, D, list(ts[1:]))
    head = W
    if fam is not None:
        for c in fam:
            if c not in present:
                head = rebuild(t, c, [W] * len(ctor_arity_types(env, t, c)))
                break
    return [head] + rest


def rebuild(t, c, args):
    if c[0] == 'tag':
        return ('C', c[1], tuple(args))
    if c[0] == 'rec':
        return ('Rec', tuple((f, a) for (f, _), a in zip(t[1], args)), False)
    if c[0] == 'tup':
        return ('Tup', tuple(args))
    if c[0] == 'lit':
        return ('Lit', c[1])
    if c[0] == 'len':
        return ('Lst', tuple(args), None)
    if c[0] == 'var':
        mp = c[1]
        return ('Lst', tuple(args[:mp]), tuple(args[mp:]))
    raise ValueError(c)
