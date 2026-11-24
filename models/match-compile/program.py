"""Programs: a few units, each a type, a match over it, and the values to run it on; and their Roc text.

A unit is a dict:
  type      the scrutinee type
  branches  [(alternatives, guard)]
  cases     [(value, k, expected)] where expected is (branch index, [(name, type, value)] sorted by name)
  arms      [[(name, type)]] the variables each branch reports, sorted by name

The program value also holds `decls` (the nominal declarations every unit shares). The emitted Roc
program runs each unit's match on every case and prints one line per case whose result differs
from the expected arm and bindings, then `done`.
"""

import random

import model
from gen import Gen
from model import Env


def make_unit(g, pool_seed=None):
    r = g.rng
    t = g.gen_type(r.choice([1, 2, 2, 3]))
    pool = {}
    branches = g.gen_branches(t, pool)
    env = g.env()
    arms = []
    for alts, _ in branches:
        bt = model.binder_types(env, alts[0], t)
        arms.append(sorted(bt.items()))
    values = []
    seen = set()
    for alts, _ in branches:
        for p in alts:
            for _ in range(2):
                values.append(g.value_for(p, t, pool))
    for _ in range(12):
        values.append(g.sample_value(t, 3, pool))
    cases = []
    for v in values:
        if v in seen:
            continue
        seen.add(v)
        ks = [r.choice([0, 1])] if r.random() < 0.6 else [0, 1]
        for k in ks:
            got = model.choose(env, t, branches, v, k)
            if got is None:
                continue  # not covered; the match is not exhaustive for this value (cannot happen: _ is added)
            bi, b = got
            cases.append((v, k, (bi, [(n, b[n][0], b[n][1]) for n, _ in arms[bi]])))
    return dict(type=t, branches=branches, cases=cases[:60], arms=arms)


def make_program(seed, n_units=None):
    rng = random.Random(seed)
    g = Gen(rng)
    units = [make_unit(g) for _ in range(n_units or rng.randint(1, 3))]
    return dict(seed=seed, decls=dict(g.decls), units=units)


# ---------------------------------------------------------------- printing

class Printer:
    def __init__(self, decls, seed=0, qualify=True):
        self.qualify = qualify
        self.g = Gen(random.Random(seed))
        self.g.decls = dict(decls)
        self.env = Env(decls)
        self.rng = random.Random(seed)

    def ty(self, t):
        return self.g.ty(t)

    def value(self, v, t):
        rt = self.env.resolve(t)
        k = v[0]
        if k == 'tag':
            args = dict(rt[1])[v[1]]
            name = v[1]
            if t[0] == 'NR':
                name = '%s.%s' % (t[1], name)
            if not v[2]:
                return name
            return '%s(%s)' % (name, ', '.join(self.value(x, a) for x, a in zip(v[2], args)))
        if k == 'wrap':
            inner = self.value(v[2], rt[2])
            if v[2][0] == 'rec':
                return '%s.%s' % (v[1], inner)
            if v[2][0] == 'tup':
                return '%s.%s' % (v[1], inner)
            return '%s.(%s)' % (v[1], inner)
        if k == 'rec':
            if not v[1]:
                return '{}'
            ft = dict(rt[1])
            return '{ ' + ', '.join('%s: %s' % (f, self.value(x, ft[f])) for f, x in v[1]) + ' }'
        if k == 'tup':
            return '(' + ', '.join(self.value(x, a) for x, a in zip(v[1], rt[1])) + ')'
        if k == 'list':
            return '[' + ', '.join(self.value(x, rt[1]) for x in v[1]) + ']'
        if k == 'int':
            return int_lit(v[1], rt[1])
        if k == 'str':
            return str_lit(v[1])
        raise ValueError(v)

    def pat(self, p, t):
        rt = self.env.resolve(t)
        k = p[0]
        if k == 'W':
            return '_'
        if k == 'B':
            return p[1]
        if k == 'As':
            return '%s as %s' % (self.pat(p[1], t), p[2])
        if k == 'C':
            args = dict(rt[1])[p[1]]
            name = p[1]
            if t[0] == 'NR' and self.rng.random() < 0.4 and self.qualify:
                name = '%s.%s' % (t[1], name)
            if not p[2]:
                return name
            return '%s(%s)' % (name, ', '.join(self.pat(x, a) for x, a in zip(p[2], args)))
        if k == 'Rec':
            ft = dict(rt[1])
            parts = ['%s: %s' % (f, self.pat(x, ft[f])) for f, x in p[1]]
            if p[2] is not None:
                parts.append('..' + p[2])
            if not parts:
                return '{ .. }'
            return '{ ' + ', '.join(parts) + ' }'
        if k == 'Tup':
            return '(' + ', '.join(self.pat(x, a) for x, a in zip(p[1], rt[1])) + ')'
        if k == 'Lst':
            parts = [self.pat(x, rt[1]) for x in p[1]]
            if p[2] is not None:
                parts.append('..' if not p[2][0] else '.. as ' + p[2][0])
                parts += [self.pat(x, rt[1]) for x in p[2][1]]
            return '[' + ', '.join(parts) + ']'
        if k == 'Lit':
            if p[1][0] == 'int':
                return str(p[1][1])
            return str_lit(p[1][1])
        if k == 'SP':
            s = ''
            for i, x in enumerate(p[1]):
                s += escape(x) if i % 2 == 0 else '${%s}' % (x or '_')
            return '"' + s + '"'
        if k == 'Wrap':
            inner = self.pat(p[2], rt[2])
            if p[2][0] in ('Rec', 'Tup'):
                return '%s.%s' % (p[1], inner)
            return '%s.(%s)' % (p[1], inner)
        raise ValueError(p)

    def guard(self, g):
        if g is None:
            return ''
        if g[0] == 'k':
            return ' if k == %d' % g[1]
        if g[0] == 'gt':
            return ' if %s > %d' % (g[1], g[2])
        if g[0] == 'seq':
            return ' if %s == %s' % (g[1], str_lit(g[2]))
        if g[0] == 'len':
            return ' if %s.len() > %d' % (g[1], g[2])
        raise ValueError(g)


def escape(s):
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('$', '\\$')


def str_lit(s):
    return '"' + escape(s) + '"'


def int_lit(n, ty):
    if ty == 'I64' and n == -9223372036854775808:
        return '(-9223372036854775807 - 1)'
    return str(n)


def unit_lines(pr, u, ui):
    """The Roc definitions for unit ui: its result type, its match, its cases and its checker."""
    t = u['type']
    T = pr.ty(t)
    R = 'R%d' % ui
    tags = []
    for bi, arm in enumerate(u['arms']):
        tags.append('Arm%d' % bi if not arm else 'Arm%d(%s)' % (bi, ', '.join(pr.ty(x) for _, x in arm)))
    uses_k = any(g is not None and g[0] == 'k' for _, g in u['branches'])
    out = ['%s : [%s]' % (R, ', '.join(tags)), '']
    out.append('f%d : %s, U64 -> %s' % (ui, T, R))
    out.append('f%d = |x, %s| match x {' % (ui, 'k' if uses_k else '_k'))
    for bi, (alts, g) in enumerate(u['branches']):
        arm = u['arms'][bi]
        body = 'Arm%d' % bi if not arm else 'Arm%d(%s)' % (bi, ', '.join(n for n, _ in arm))
        out.append('    %s%s => %s' % (' | '.join(pr.pat(p, t) for p in alts), pr.guard(g), body))
    out.append('}')
    out.append('')
    out.append('arm%d : %s -> U64' % (ui, R))
    out.append('arm%d = |r| match r {' % ui)
    for bi, arm in enumerate(u['arms']):
        out.append('    Arm%d%s => %d' % (bi, '' if not arm else '(%s)' % ', '.join('_' for _ in arm), bi))
    out.append('}')
    out.append('')
    out.append('diff%d : %s, %s -> Str' % (ui, R, R))
    out.append('diff%d = |g, w| match (g, w) {' % ui)
    for bi, arm in enumerate(u['arms']):
        if not arm:
            continue
        gs = ', '.join('g%d' % j for j in range(len(arm)))
        ws = ', '.join('w%d' % j for j in range(len(arm)))
        parts = ['(if g%d == w%d "" else " %s")' % (j, j, n) for j, (n, _) in enumerate(arm)]
        body = parts[0]
        for p in parts[1:]:
            body = 'Str.concat(%s, %s)' % (body, p)
        out.append('    (Arm%d(%s), Arm%d(%s)) => %s' % (bi, gs, bi, ws, body))
    out.append('    _ => ""')
    out.append('}')
    out.append('')
    out.append('cases%d : List((%s, U64, %s))' % (ui, T, R))
    out.append('cases%d = [' % ui)
    for v, k, (bi, binds) in u['cases']:
        want = 'Arm%d' % bi if not binds else 'Arm%d(%s)' % (bi, ', '.join(pr.value(bv, bt) for _, bt, bv in binds))
        out.append('    (%s, %d, %s),' % (pr.value(v, t), k, want))
    out.append(']')
    out.append('')
    out.append('check%d : U64 -> Str' % ui)
    out.append('check%d = |z| {' % ui)
    out.append('    var $out = ""')
    out.append('    var $i = 0.U64')
    out.append('    for (v, k, want) in cases%d {' % ui)
    out.append('        got = f%d(v, k + z)' % ui)
    out.append('        $out = if got == want $out else Str.concat($out, "u%d case ${$i.to_str()} arm ${arm%d(got).to_str()}${diff%d(got, want)}\\n")'
               % (ui, ui, ui))
    out.append('        $i = $i + 1')
    out.append('    }')
    out.append('    $out')
    out.append('}')
    out.append('')
    return out


def render(prog, target):
    """{filename: text}. target: 'native' (a headerless app printing with echo!), 'wasm' (an app on
    test/wasm/platform), or 'test' (the native app plus one top-level expect per case, for roc test)."""
    pr = Printer(prog['decls'], prog['seed'])
    lines = []
    if target == 'wasm':
        lines += ['app [main!] { pf: platform "./platform/main.roc" }', '', 'import pf.FallibleHost', '']
    lines += pr.g.decl_lines()
    lines.append('')
    for ui, u in enumerate(prog['units']):
        lines += unit_lines(Printer(prog['decls'], prog['seed'] * 31 + ui, prog.get('qualify', True)), u, ui)
    report = '"done"'
    for ui in reversed(range(len(prog['units']))):
        report = 'Str.concat(check%d(z), %s)' % (ui, report)
    if target == 'wasm':
        lines += ['main! = || {', '    z = Str.count_utf8_bytes(FallibleHost.json_input!({})) * 0',
                  '    %s' % report, '}']
    else:
        lines += ['main! = |args| {', '    z = args.len() * 0', '    echo!(%s)' % report, '    Ok({})', '}']
    if target == 'test':
        for ui, u in enumerate(prog['units']):
            pu = Printer(prog['decls'], prog['seed'] * 31 + ui)
            lines.append('')
            for v, k, (bi, binds) in u['cases']:
                want = 'Arm%d' % bi if not binds else 'Arm%d(%s)' % (bi, ', '.join(pu.value(bv, bt) for _, bt, bv in binds))
                lines.append('expect f%d(%s, %d) == %s' % (ui, pu.value(v, u['type']), k, want))
    return {'app.roc': '\n'.join(lines) + '\n'}


def expected_output(prog):
    return 'done'
