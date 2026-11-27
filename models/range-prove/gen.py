"""Random Roc programs around the range prover's fact sources.

Small typed constructs composed at random. Two domains:

- `general`: unsigned arithmetic at U8/U32/U64 (checked, wrapping,
  saturating, try, overflow predicates), signed wrapping/division/masks
  converted back, masks and shifts, comparisons feeding if/else joins, list
  updates with known length effects, counted loops over lists, List.get reads
  at derived indices. Each program reads one input line `a b c x0 x1 ...` and
  prints each function's result; the runner feeds it several lines.
- `u8` / `u16`: every function takes three U8 (or U16) values and uses only
  operations that cannot crash, so `main` can loop over (a, b) pairs for each
  c on the input line and print one checksum per function. For U8 that is
  every pair; for U16 it is a stride-7 grid over 0..4095.

Either way `--opt=dev` and `--opt=speed` builds must print the same thing.

Usage: python3 gen.py SEED OUT.roc [--domain general|u8|u16] [--platform PATH]
PATH is the platform's main.roc relative to OUT.roc.
"""
import argparse
import random

EDGES64 = [0, 1, 2, 3, 7, 8, 15, 16, 255, 256, 1000, 2**32 - 1, 2**63, 2**64 - 1, 2**64 - 2, 2**64 - 8]
EDGES8 = [0, 1, 2, 7, 8, 15, 16, 100, 127, 128, 200, 254, 255]
EDGES32 = [0, 1, 7, 8, 255, 65535, 2**31, 2**32 - 1, 2**32 - 2]
WIDTHS = {'U8': EDGES8, 'U16': [0, 1, 255, 256, 65534, 65535], 'U32': EDGES32, 'U64': EDGES64}
TABLE = 37  # length of the lookup table the exhaustive domain reads
DOMAINS = ('general', 'u8', 'u16')


class Base:
    def cond(self, d=0):
        """A comparison, sometimes combined with another by and/or."""
        x, y = self.cond_operands(d)
        c = f'{x} {self.r.choice(["<", "<=", ">", ">=", "==", "!="])} {y}'
        if d < 2 and self.r.random() < self.and_or:
            c = f'({c}) {self.r.choice(["and", "or"])} ({self.cond(d + 1)})'
        return c


class General(Base):
    def __init__(self, rng):
        self.r = rng
        self.vars = []  # (name, ty)
        self.loopdepth = 0
        self.nid = 0
        self.noinline = rng.random() < 0.75
        self.and_or = 0.25

    def fresh(self, p):
        self.nid += 1
        return f'{p}{self.nid}'

    def lit(self, ty):
        v = self.r.choice(WIDTHS[ty])
        return f'{v}.{ty}'

    def nzlit(self, ty):
        v = self.r.choice([x for x in WIDTHS[ty] if x != 0])
        return f'{v}.{ty}'

    def conv(self, e, src, dst):
        if src == dst:
            return e
        order = ['U8', 'U16', 'U32', 'U64']
        if order.index(src) < order.index(dst):
            return f'({e}).to_{dst.lower()}()'
        return f'({e}).to_{dst.lower()}_wrap()'

    def atom(self, ty):
        c = self.r.random()
        cands = [n for (n, t) in self.vars if t == ty]
        if c < 0.35 and cands:
            return self.r.choice(cands)
        if c < 0.5:
            src = self.r.choice(['a', 'b', 'c'])
            return self.conv(src, 'U64', ty)
        if c < 0.6:
            return self.conv('List.len(xs)', 'U64', ty)
        if c < 0.65:
            return self.conv('List.len(bs)', 'U64', ty)
        lists = [n for (n, t) in self.vars if t == 'L']
        if c < 0.72 and lists:
            return self.conv(f'List.len({self.r.choice(lists)})', 'U64', ty)
        return self.lit(ty)

    def sexpr(self, st, d=0):
        edges = {'I64': [-1, 0, 1, 127, -128, 2**63 - 1, -2**63, 255], 'I8': [-1, 0, 1, 127, -128, 15], 'I32': [-1, 0, 7, 2**31 - 1, -2**31]}[st]
        if d > 3 or self.r.random() < 0.3:
            c = self.r.random()
            if c < 0.5:
                src = self.r.choice(['a', 'b', 'c'])
                return f'({src}).to_{st.lower()}_wrap()'
            return f'{self.r.choice(edges)}.{st}'
        k = self.r.choice(['and', 'pw', 'mw', 'shr', 'div', 'rem', 'max', 'neg', 'cmpif'])
        a = lambda: self.sexpr(st, d + 1)
        if k == 'and':
            return f'({a()}).bitwise_and({self.r.choice([x for x in edges if x >= 0])}.{st})'
        if k == 'pw':
            return f'({a()}).plus_wrap({a()})'
        if k == 'mw':
            return f'({a()}).minus_wrap({a()})'
        if k == 'shr':
            return f'({a()}).shr_wrap({self.r.randint(0, 9)}.U8)'
        if k == 'div':
            return f'({a()} // {self.r.choice([x for x in edges if x > 0])}.{st})'
        if k == 'rem':
            return f'({a()} % {self.r.choice([x for x in edges if x > 0])}.{st})'
        if k == 'max':
            return f'{st}.max({a()}, {a()})'
        if k == 'neg':
            return f'({a()}).bitwise_and({a()})'
        return f'(if {a()} < {a()} {{ {a()} }} else {{ {a()} }})'

    def lexpr(self, d=0):
        lists = [n for (n, t) in self.vars if t == 'L'] + ['xs']
        l = self.r.choice(lists)
        e = lambda: self.expr('U64', d + 1)
        k = self.r.choice(['src', 'append', 'droplast', 'dropfirst', 'sublist', 'set', 'concat', 'repeat', 'takefirst', 'dropat', 'prepend'])
        if k == 'src':
            return l
        if k == 'append':
            return f'{l}.append({e()})'
        if k == 'prepend':
            return f'{l}.prepend({e()})'
        if k == 'droplast':
            return f'List.drop_last({l}, {self.r.choice(["1", e()])})'
        if k == 'dropfirst':
            return f'List.drop_first({l}, {self.r.choice(["1", e()])})'
        if k == 'takefirst':
            return f'List.take_first({l}, {e()})'
        if k == 'dropat':
            return f'List.drop_at({l}, {e()})'
        if k == 'sublist':
            return f'{l}.sublist({{ start: {e()}, len: {e()} }})'
        if k == 'set':
            return f'(match List.set({l}, {e()}, {e()}) {{ Ok(v) => v, Err(_) => {l} }})'
        if k == 'concat':
            return f'{l}.concat(xs)'
        return f'List.repeat({e()}, ({e()}).bitwise_and(15))'

    def expr(self, ty, d=0):
        if ty == 'L':
            return self.lexpr(d)
        if d > 2 or self.r.random() < 0.3:
            return self.atom(ty)
        k = self.r.choice(['add', 'sub', 'mul', 'satsub', 'satadd', 'wrapadd', 'wrapsub', 'and', 'or', 'shr', 'shl',
                           'div', 'rem', 'get', 'getb', 'if', 'min', 'max', 'absdiff', 'try', 'xor', 'not', 'ovf', 'sconv'])
        a = lambda: self.expr(ty, d + 1)
        if k == 'add':
            return f'({a()} + {a()})'
        if k == 'sub':
            return f'({a()} - {a()})'
        if k == 'mul':
            return f'({a()} * {self.lit(ty)})'
        if k == 'satsub':
            return f'({a()}).minus_saturated({a()})'
        if k == 'satadd':
            return f'({a()}).plus_saturated({a()})'
        if k == 'wrapadd':
            return f'({a()}).plus_wrap({a()})'
        if k == 'wrapsub':
            return f'({a()}).minus_wrap({a()})'
        if k == 'and':
            return f'({a()}).bitwise_and({self.r.choice([a(), self.lit(ty)])})'
        if k == 'or':
            return f'({a()}).bitwise_or({a()})'
        if k == 'xor':
            return f'({a()}).bitwise_xor({a()})'
        if k == 'not':
            return f'({a()}).bitwise_not()'
        if k == 'shr':
            return f'({a()}).shr_wrap({self.r.choice([str(self.r.randint(0, 70)) + ".U8", self.conv(self.atom("U64"), "U64", "U8")])})'
        if k == 'shl':
            return f'({a()}).shl_wrap({self.r.randint(0, 10)}.U8)'
        if k == 'div':
            return f'({a()} // {self.r.choice([self.nzlit(ty), a()])})'
        if k == 'rem':
            return f'({a()} % {self.r.choice([self.nzlit(ty), a()])})'
        if k == 'min':
            return f'{ty}.min({a()}, {a()})'
        if k == 'max':
            return f'{ty}.max({a()}, {a()})'
        if k == 'absdiff':
            return f'({a()}).abs_diff({a()})'
        if k == 'sconv':
            st = self.r.choice(['I64', 'I8', 'I32'])
            return f'({self.sexpr(st, d + 1)}).to_{ty.lower()}_wrap()'
        if k == 'ovf':
            op = self.r.choice(['plus', 'minus', 'times'])
            return f'(if ({a()}).{op}_overflows({a()}) {{ {self.lit(ty)} }} else {{ {a()} }})'
        if k == 'try':
            op = self.r.choice(['plus', 'minus', 'times'])
            return f'(match ({a()}).{op}_try({a()}) {{ Ok(v) => v, Err(_) => {self.lit(ty)} }})'
        if k == 'get':
            idx = self.expr('U64', d + 1)
            src = self.r.choice(['xs'] + [n for (n, t) in self.vars if t == 'L'])
            return self.conv(f'(match List.get({src}, {idx}) {{ Ok(v) => v, Err(_) => {self.r.choice(EDGES64)}.U64 }})', 'U64', ty)
        if k == 'getb':
            idx = self.expr('U64', d + 1)
            return self.conv(f'(match List.get(bs, {idx}) {{ Ok(v) => v, Err(_) => {self.r.choice(EDGES8)}.U8 }})', 'U8', ty)
        if k == 'if':
            return f'(if {self.cond(d + 1)} {{ {a()} }} else {{ {a()} }})'
        return self.atom(ty)

    def cond_operands(self, d):
        ty = self.r.choice(['U64', 'U64', 'U8', 'U32'])
        return self.expr(ty, d + 1), self.expr(ty, d + 1)

    def stmts(self, n, ind):
        out = []
        for _ in range(n):
            out.extend(self.stmt(ind))
        return out

    def stmt(self, ind):
        p = '\t' * ind
        k = self.r.random()
        mut = [v for (v, t) in self.vars if v.startswith('$') and not v.endswith('_ro')]
        if k < 0.35 or not mut:
            ty = self.r.choice(['U64', 'U64', 'U8', 'U32', 'L'])
            name = self.fresh('$v')
            line = f'{p}var {name} = {self.expr(ty)}'
            self.vars.append((name, ty))
            return [line]
        if k < 0.55:
            name = self.r.choice(mut)
            ty = dict(self.vars)[name]
            return [f'{p}{name} = {self.expr(ty)}']
        if k < 0.75:
            saved = list(self.vars)
            lines = [f'{p}if {self.cond()} {{']
            lines += self.stmts(self.r.randint(1, 2), ind + 1)
            self.vars = list(saved)
            lines.append(f'{p}}} else {{')
            lines += self.stmts(self.r.randint(1, 2), ind + 1)
            self.vars = saved
            lines.append(f'{p}}}')
            return lines
        if self.loopdepth >= 2:
            name = self.r.choice(mut)
            return [f'{p}{name} = {self.expr(dict(self.vars)[name])}']
        # a counted loop; the counter is never assigned in the body
        i = self.fresh('$i')
        ty = self.r.choice(['U64', 'U64', 'U8'])
        shape = self.r.choice(['up', 'up', 'down', 'margin'])
        saved = list(self.vars)
        lines = []
        if shape == 'up':
            lists = [n for (n, t) in self.vars if t == 'L']
            bound = self.r.choice(['List.len(xs)', 'List.len(bs)'] + [f'List.len({n})' for n in lists] + [ self.lit(ty) if ty == 'U8' else str(self.r.randint(0, 20)),
                                   'a.bitwise_and(15)', 'List.len(xs).minus_saturated(1)', 'List.len(bs).minus_saturated(4)'])
            bound = self.conv(bound, 'U64', ty) if not bound[0].isdigit() and not bound.endswith('U8') else bound
            lines.append(f'{p}var {i} = {self.r.choice(["0.U64", "1.U64", "c.bitwise_and(3)"]) if ty == "U64" else "0.U8"}')
            lines.append(f'{p}while {i} < {bound} {{')
            self.vars.append((i + '_ro', ty))
            self.loopdepth += 1
            body = self.stmts(self.r.randint(1, 3), ind + 1)
            self.loopdepth -= 1
            body = [b.replace(i + '_ro', i) for b in body]
            lines += body
            lines.append(f'{p}\t{i} = {i} + {self.r.choice(["1", "1", "2"])}')
            lines.append(f'{p}}}')
        elif shape == 'down':
            lines.append(f'{p}var {i} = {self.r.choice(["List.len(xs)", "List.len(bs)", "b.bitwise_and(31)"])}')
            lines.append(f'{p}while {i} > 0 {{')
            lines.append(f'{p}\t{i} = {i} - 1')
            self.vars.append((i + '_ro', 'U64'))
            self.loopdepth += 1
            body = self.stmts(self.r.randint(1, 3), ind + 1)
            self.loopdepth -= 1
            lines += [b.replace(i + '_ro', i) for b in body]
            lines.append(f'{p}}}')
        else:
            w = self.r.choice([1, 2, 4, 8])
            lines.append(f'{p}var {i} = 0.U64')
            lines.append(f'{p}while {i} + {w} <= List.len(bs) {{')
            self.vars.append((i + '_ro', 'U64'))
            self.loopdepth += 1
            body = self.stmts(self.r.randint(0, 2), ind + 1)
            self.loopdepth -= 1
            lines += [b.replace(i + '_ro', i) for b in body]
            idx = self.r.choice([f'{i} + {w - 1}', f'{i}', f'{i} + {self.r.randint(0, w)}'])
            tgt = self.r.choice([m for m in mut if dict(saved).get(m) != 'L'] or [None])
            if tgt is None:
                tgt = self.fresh('$v')
                lines.insert(0, f'{p}var {tgt} = 0.U64')
                saved.append((tgt, 'U64'))
            tty = dict(saved).get(tgt, 'U64')
            lines.append(f'{p}\t{tgt} = {self.conv(f"(match List.get(bs, {idx}) {{ Ok(v) => v, Err(_) => 99.U8 }})", "U8", tty)}')
            lines.append(f'{p}\t{i} = {i} + {w}')
            lines.append(f'{p}}}')
        self.vars = saved
        return lines

    def func(self, name):
        self.vars = []
        self.nid = 0
        body = self.stmts(self.r.randint(2, 6), 1)
        res = [(n if t != 'L' else f'List.len({n})', t if t != 'L' else 'U64') for (n, t) in self.vars]
        ret = ''.join(f'.plus_wrap({self.conv(n, t, "U64")})' for (n, t) in res[:4])
        lines = [f'{name} : List(U64), List(U8), U64, U64, U64 -> U64', f'{name} = |xs, bs, a, b, c| {{']
        if self.noinline:
            # A recursive call on a path no input takes keeps the function a
            # procedure of its own, so its arguments stay unknown to the prover.
            lines.append(f'\tif c == 987654321.U64 and b == 123456789.U64 {{ {name}(xs, bs, a, b, 0.U64) }} else {{')
            lines += ['\t' + l for l in body]
            lines.append(f'\t\t0.U64{ret}')
            lines.append('\t}')
        else:
            lines += body
            lines.append(f'\t0.U64{ret}')
        lines.append('}')
        return '\n'.join(lines)


class Exhaustive(Base):
    def __init__(self, r, ty):
        self.r = r
        self.ty = ty
        self.max = 255 if ty == 'U8' else 65535
        self.vars = []
        self.and_or = 0.3

    def lit(self):
        return f'{self.r.choice([0, 1, 2, 3, 7, 8, 15, 16, 100, 127, 128, self.max - 1, self.max, TABLE, TABLE - 1, TABLE + 1])}.{self.ty}'

    def atom(self):
        c = self.r.random()
        if c < 0.55:
            return self.r.choice(['a', 'b', 'c'] + self.vars)
        return self.lit()

    def e(self, d=0):
        if d > 2 or self.r.random() < 0.3:
            return self.atom()
        a = lambda: self.e(d + 1)
        k = self.r.choice(['pw', 'mw', 'tw', 'ps', 'ms', 'ptry', 'mtry', 'ttry', 'povf', 'movf', 'tovf', 'and', 'or', 'xor',
                           'shr', 'shl', 'div', 'rem', 'min', 'max', 'absd', 'if', 'if', 'get', 'not', 'cnt'])
        if k == 'pw':
            return f'({a()}).plus_wrap({a()})'
        if k == 'mw':
            return f'({a()}).minus_wrap({a()})'
        if k == 'tw':
            return f'({a()}).times_wrap({a()})'
        if k == 'ps':
            return f'({a()}).plus_saturated({a()})'
        if k == 'ms':
            return f'({a()}).minus_saturated({a()})'
        if k in ('ptry', 'mtry', 'ttry'):
            op = {'ptry': 'plus', 'mtry': 'minus', 'ttry': 'times'}[k]
            return f'(match ({a()}).{op}_try({a()}) {{ Ok(v) => v, Err(_) => {a()} }})'
        if k in ('povf', 'movf', 'tovf'):
            op = {'povf': 'plus', 'movf': 'minus', 'tovf': 'times'}[k]
            return f'(if ({a()}).{op}_overflows({a()}) {{ {a()} }} else {{ {a()} }})'
        if k == 'and':
            return f'({a()}).bitwise_and({a()})'
        if k == 'or':
            return f'({a()}).bitwise_or({a()})'
        if k == 'xor':
            return f'({a()}).bitwise_xor({a()})'
        if k == 'not':
            return f'({a()}).bitwise_not()'
        if k == 'cnt':
            return f'({a()}).count_one_bits().to_{self.ty.lower()}()' if self.ty == 'U16' else f'({a()}).count_one_bits()'
        if k == 'shr':
            return f'({a()}).shr_wrap(({a()}).to_u8_wrap())'
        if k == 'shl':
            return f'({a()}).shl_wrap(({a()}).to_u8_wrap())'
        if k == 'div':
            return f'({a()} // ({a()}).bitwise_or(1.{self.ty}))'
        if k == 'rem':
            return f'({a()} % ({a()}).bitwise_or(1.{self.ty}))'
        if k == 'min':
            return f'{self.ty}.min({a()}, {a()})'
        if k == 'max':
            return f'{self.ty}.max({a()}, {a()})'
        if k == 'absd':
            return f'({a()}).abs_diff({a()})'
        if k == 'get':
            return f'(match List.get(table, ({a()}).to_u64()) {{ Ok(v) => v, Err(_) => {self.lit()} }})'
        return f'(if {self.cond(d + 1)} {{ {a()} }} else {{ {a()} }})'

    def cond_operands(self, d):
        return self.e(d + 1), self.e(d + 1)

    def func(self, name):
        self.vars = []
        # As in General.func: an unreachable recursive call keeps the function
        # out of line, so the prover sees its arguments as unknown.
        lines = [f'{name} : List({self.ty}), {self.ty}, {self.ty}, {self.ty} -> U64', f'{name} = |table, a, b, c| {{',
                 f'\tif a == 7.{self.ty} and b == 9.{self.ty} and c == 201.{self.ty} {{ {name}(table, a, b, 0.{self.ty}) }} else {{']
        for _ in range(self.r.randint(2, 5)):
            v = f'v{len(self.vars)}'
            if self.r.random() < 0.3:
                lines.append(f'\t\t{v} = if {self.cond()} {{ {self.e()} }} else {{ {self.e()} }}')
            else:
                lines.append(f'\t\t{v} = {self.e()}')
            self.vars.append(v)
        lines.append('\t\t' + '.plus_wrap('.join(['0.U64'] + [f'({v}).to_u64().shl_wrap({4 * i}.U8)' for i, v in enumerate(self.vars)]) + ')' * len(self.vars))
        lines.append('\t}')
        lines.append('}')
        return '\n'.join(lines)


HEADER = '''app [main!] { pf: platform "PLATFORM" }

import pf.Stdin
import pf.Stdout

'''

MAIN = '''
main! = || {
\tline = Stdin.line!()
\tnums = line.trim().split_on(" ").keep_oks(|s| U64.from_str(s))
\ta = match List.get(nums, 0) { Ok(v) => v, Err(_) => 0 }
\tb = match List.get(nums, 1) { Ok(v) => v, Err(_) => 0 }
\tc = match List.get(nums, 2) { Ok(v) => v, Err(_) => 0 }
\txs = List.drop_first(nums, 3)
\tbs = xs.map(|v| v.to_u8_wrap())
CALLS}
'''


def general_program(seed, platform, nfuncs=4):
    r = random.Random(seed)
    g = General(r)
    funcs = [g.func(f'f{i}') for i in range(nfuncs)]
    calls = ''.join(f'\tStdout.line!("f{i} ${{f{i}(xs, bs, a, b, c).to_str()}}")\n' for i in range(nfuncs))
    return HEADER.replace('PLATFORM', platform) + '\n\n'.join(funcs) + '\n' + MAIN.replace('CALLS', calls)


def general_inputs(seed, n=8):
    r = random.Random(seed * 7919 + 1)
    out = []
    for _ in range(n):
        vals = [r.choice(EDGES64 + [r.randint(0, 40) for _ in range(6)]) for _ in range(3)]
        xs = [r.choice([0, 1, 2, 3, 7, 8, 9, 255, 256, 2**64 - 1, r.randint(0, 300)]) for _ in range(r.choice([0, 1, 2, 3, 4, 5, 8, 9, 12, 17]))]
        out.append(' '.join(str(v) for v in vals + xs))
    return out


def exhaustive_program(seed, ty, platform, nf=6):
    r = random.Random(seed)
    g = Exhaustive(r, ty)
    fs = [g.func(f'f{i}') for i in range(nf)]
    hi = 255 if ty == 'U8' else 4095
    step = 1 if ty == 'U8' else 7
    body = HEADER.replace('PLATFORM', platform) + '\n\n'.join(fs) + f'''

main! = || {{
\tline = Stdin.line!()
\tcs = line.trim().split_on(" ").keep_oks(|s| {ty}.from_str(s))
\tvar $t = List.with_capacity({TABLE})
\tvar $k = 0.{ty}
\twhile $k < {TABLE}.{ty} {{
\t\t$t = $t.append($k.times_wrap(3.{ty}))
\t\t$k = $k + 1
\t}}
\ttable = $t
'''
    for i in range(nf):
        body += f'''\tvar $h{i} = 0.U64
\tfor c in cs {{
\t\tvar $a = 0.U64
\t\twhile $a <= {hi} {{
\t\t\tvar $b = 0.U64
\t\t\twhile $b <= {hi} {{
\t\t\t\t$h{i} = ($h{i}.times_wrap(1099511628211)).bitwise_xor(f{i}(table, $a.to_{ty.lower()}_wrap(), $b.to_{ty.lower()}_wrap(), c))
\t\t\t\t$b = $b + {step}
\t\t\t}}
\t\t\t$a = $a + {step}
\t\t}}
\t}}
\tStdout.line!("f{i} ${{$h{i}.to_str()}}")
'''
    body += '}\n'
    return body


def program(seed, domain, platform):
    if domain == 'general':
        return general_program(seed, platform)
    return exhaustive_program(seed, domain.upper(), platform)


def inputs(seed, domain):
    """The stdin lines to run a program on."""
    if domain == 'general':
        return general_inputs(seed)
    return ['0 1 2 7 37 128 255' if domain == 'u8' else '0 1 255 256 4095 65535']


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('seed', type=int)
    ap.add_argument('out')
    ap.add_argument('--domain', choices=DOMAINS, default='general')
    ap.add_argument('--platform', default='../../../test/fx/platform/main.roc')
    a = ap.parse_args()
    with open(a.out, 'w') as f:
        f.write(program(a.seed, a.domain, a.platform))


if __name__ == '__main__':
    main()
