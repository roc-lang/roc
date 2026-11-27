"""Reference semantics of `match`: which arm runs for a value, and what each pattern variable is bound to.

Types are nested tuples:
  ('U', tags, nom)          tag union. tags: tuple of (Name, tuple[type]). nom: None, 'Bool', 'Try', or a
                            user nominal name (then the union is that nominal's backing).
  ('NR', name)              a user nominal, resolved through Env (so it can be recursive)
  ('R', ((field, type), ...))
  ('T', (type, ...))
  ('L', elem)
  ('S',)                    Str
  ('N', 'U8' | 'I64' | 'U64')

A nominal declaration is ('union', tags) or ('wrap', backing) with a non-union backing (a number, `Str`,
a tuple or a record); Env.decls maps its name to (decl, opaque).

Values:
  ('tag', name, (values))   also the value of a union nominal
  ('rec', ((field, value), ...))
  ('tup', (values))
  ('list', (values))
  ('int', n)
  ('str', s)
  ('wrap', name, value)     the value of a wrap nominal

Patterns:
  ('W',)                    `_`
  ('B', name)               a variable
  ('As', pat, name)         `pat as name`
  ('C', tag, (pats))        a tag
  ('Rec', ((field, pat), ...), rest)  rest: None (every field listed), '' (`..`), or a name (`..name`)
  ('Tup', (pats))
  ('Lst', (prefix), rest)   rest: None for an exact-length pattern, else (name or None, (suffix))
  ('Lit', value)            an int or a str value tuple
  ('SP', parts)             a string pattern with captures: literal strings at even indexes, capture
                            names (None for `${_}`) at odd indexes
  ('Wrap', name, pat)       a wrap nominal's backing: `N.(p)`, `N.(a, b)`, `N.{ .. }`

Rules (ARM, PAT-*, GUARD, ALT, BIND) are defined in RULES.md.
"""


class Env:
    def __init__(self, decls):
        self.decls = dict(decls)  # name -> (decl, opaque)

    def resolve(self, t):
        """The structural type behind a nominal reference: a union nominal's union, or a wrap nominal."""
        if t[0] == 'NR':
            d, _ = self.decls[t[1]]
            if d[0] == 'union':
                return ('U', d[1], t[1])
            return ('Wrap', t[1], d[1])
        return t


# ---------------------------------------------------------------- matching (PAT-*)

def match(env, p, t, v):
    """The bindings {name: (type, value)} when pattern p of type t matches value v, else None."""
    out = {}
    return out if _match(env, p, t, v, out) else None


def _match(env, p, t, v, out):
    k = p[0]
    rt = env.resolve(t)
    if k == 'W':
        return True
    if k == 'B':
        out[p[1]] = (t, v)
        return True
    if k == 'As':
        if not _match(env, p[1], t, v, out):
            return False
        out[p[2]] = (t, v)
        return True
    if k == 'C':
        if v[1] != p[1]:
            return False
        args = dict(rt[1])[p[1]]
        return all(_match(env, sp, at, sv, out) for sp, at, sv in zip(p[2], args, v[2]))
    if k == 'Rec':
        fv = dict(v[1])
        ft = dict(rt[1])
        for f, sp in p[1]:
            if not _match(env, sp, ft[f], fv[f], out):
                return False
        if p[2]:
            listed = {f for f, _ in p[1]}
            rest_t = ('R', tuple((f, x) for f, x in rt[1] if f not in listed))
            rest_v = ('rec', tuple((f, x) for f, x in v[1] if f not in listed))
            out[p[2]] = (rest_t, rest_v)
        return True
    if k == 'Tup':
        return all(_match(env, sp, et, sv, out) for sp, et, sv in zip(p[1], rt[1], v[1]))
    if k == 'Lst':
        items = v[1]
        pre = p[1]
        et = rt[1]
        if p[2] is None:
            if len(items) != len(pre):
                return False
            return all(_match(env, sp, et, sv, out) for sp, sv in zip(pre, items))
        name, suf = p[2]
        if len(items) < len(pre) + len(suf):
            return False
        mid = items[len(pre):len(items) - len(suf)]
        tail = items[len(items) - len(suf):] if suf else ()
        if not all(_match(env, sp, et, sv, out) for sp, sv in zip(pre, items)):
            return False
        if not all(_match(env, sp, et, sv, out) for sp, sv in zip(suf, tail)):
            return False
        if name:
            out[name] = (t if t[0] == 'L' else rt, ('list', tuple(mid)))
        return True
    if k == 'Lit':
        return v == p[1]
    if k == 'SP':
        caps = str_match(p[1], v[1])
        if caps is None:
            return False
        for name, s in caps:
            if name:
                out[name] = (('S',), ('str', s))
        return True
    if k == 'Wrap':
        return _match(env, p[2], rt[2], v[2], out)
    raise ValueError(p)


def str_match(parts, s):
    """Rule PAT-StrCapture (langref "String Patterns with Captures"): the text must start with the first
    literal; each capture then stops at the first occurrence of the literal after it, and the text must end
    where the last literal ends. A capture with no literal after it takes the rest of the text. Returns
    [(name, captured)] or None."""
    lead = parts[0]
    if not s.startswith(lead):
        return None
    pos = len(lead)
    caps = []
    for i in range(1, len(parts), 2):
        name, delim = parts[i], parts[i + 1]
        if delim == '':
            assert i + 1 == len(parts) - 1, 'only the last capture may have no literal after it'
            caps.append((name, s[pos:]))
            pos = len(s)
            continue
        j = s.find(delim, pos)
        if j < 0:
            return None
        caps.append((name, s[pos:j]))
        pos = j + len(delim)
    return caps if pos == len(s) else None


# ---------------------------------------------------------------- guards (GUARD)

def eval_guard(g, binds, k):
    """g: ('k', c) is `k == c` on the match's U64 argument; ('gt', name, c) is `name > c` on an int
    variable; ('seq', name, s) is `name == "s"` on a Str variable; ('len', name, c) is
    `name.len() > c` on a List variable."""
    if g is None:
        return True
    if g[0] == 'k':
        return k == g[1]
    v = binds[g[1]][1]
    if g[0] == 'gt':
        return v[1] > g[2]
    if g[0] == 'seq':
        return v[1] == g[2]
    if g[0] == 'len':
        return len(v[1]) > g[2]
    raise ValueError(g)


# ---------------------------------------------------------------- the judgment (ARM, ALT, BIND)

def choose(env, t, branches, v, k):
    """Rule ARM: the first branch, in source order, with an alternative that matches v and whose guard is
    true. Rule ALT: the alternatives of one branch are tried left to right; the first that matches binds.
    Returns (branch index, bindings) or None when no branch matches (the program would crash)."""
    for bi, (alts, guard) in enumerate(branches):
        for p in alts:
            b = match(env, p, t, v)
            if b is None:
                continue
            if eval_guard(guard, b, k):
                return bi, b
            # Roc tries a branch's alternatives as if each were its own branch with the same guard.
    return None


def binders(p):
    """The variable names pattern p introduces, in source order."""
    k = p[0]
    if k == 'B':
        return [p[1]]
    if k == 'As':
        return binders(p[1]) + [p[2]]
    if k == 'C':
        return [n for sp in p[2] for n in binders(sp)]
    if k == 'Rec':
        return [n for _, sp in p[1] for n in binders(sp)] + ([p[2]] if p[2] else [])
    if k == 'Tup':
        return [n for sp in p[1] for n in binders(sp)]
    if k == 'Lst':
        out = [n for sp in p[1] for n in binders(sp)]
        if p[2] is not None:
            if p[2][0]:
                out.append(p[2][0])
            out += [n for sp in p[2][1] for n in binders(sp)]
        return out
    if k == 'SP':
        return [n for n in p[1][1::2] if n]
    if k == 'Wrap':
        return binders(p[2])
    return []


def binder_types(env, p, t):
    """{name: type} for the variables of pattern p of type t."""
    out = {}
    _binder_types(env, p, t, out)
    return out


def _binder_types(env, p, t, out):
    k = p[0]
    rt = env.resolve(t)
    if k == 'B':
        out[p[1]] = t
    elif k == 'As':
        _binder_types(env, p[1], t, out)
        out[p[2]] = t
    elif k == 'C':
        for sp, at in zip(p[2], dict(rt[1])[p[1]]):
            _binder_types(env, sp, at, out)
    elif k == 'Rec':
        ft = dict(rt[1])
        for f, sp in p[1]:
            _binder_types(env, sp, ft[f], out)
        if p[2]:
            listed = {f for f, _ in p[1]}
            out[p[2]] = ('R', tuple((f, x) for f, x in rt[1] if f not in listed))
    elif k == 'Tup':
        for sp, et in zip(p[1], rt[1]):
            _binder_types(env, sp, et, out)
    elif k == 'Lst':
        for sp in p[1]:
            _binder_types(env, sp, rt[1], out)
        if p[2] is not None:
            if p[2][0]:
                out[p[2][0]] = t if t[0] == 'L' else rt
            for sp in p[2][1]:
                _binder_types(env, sp, rt[1], out)
    elif k == 'SP':
        for n in p[1][1::2]:
            if n:
                out[n] = ('S',)
    elif k == 'Wrap':
        _binder_types(env, p[2], rt[2], out)
