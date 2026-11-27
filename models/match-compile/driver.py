"""Driver for the match-compile model. Called by run.sh; see README.md.

  driver.py selftest                       model unit tests, then the fixtures in tests/ on every config
  driver.py fuzz [N] [--seed S] [--configs a,b] [--jobs J] [--no-reduce]
  driver.py check CASE.txt... [--configs a,b]
  driver.py reduce CASE.txt [--configs a,b]
  driver.py gen SEED [--target native|wasm|test]

Environment: ROC (the compiler), OUT (output directory), NODE (default `node`).
"""

import argparse
import ast
import os
import pprint
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import model  # noqa: E402
import program  # noqa: E402

# name: (target, mode, opt, specialize). mode "run" is `roc app.roc`, "test" is `roc test app.roc`,
# "build" builds and runs the output.
CONFIGS = {
    'interp': ('native', 'run', 'interpreter', 'no'),
    'test': ('test', 'test', None, None),
    'dev': ('native', 'build', 'dev', 'no'),
    'dev-spec': ('native', 'build', 'dev', 'yes'),
    'speed': ('native', 'build', 'speed', 'no'),
    'speed-spec': ('native', 'build', 'speed', 'yes'),
    'wasm-dev': ('wasm', 'build', 'dev', 'no'),
    'wasm-dev-spec': ('wasm', 'build', 'dev', 'yes'),
    'wasm-speed': ('wasm', 'build', 'speed', 'no'),
    'wasm-speed-spec': ('wasm', 'build', 'speed', 'yes'),
}
TIMEOUT = 300


def roc():
    return os.environ['ROC']


def out_dir():
    return os.environ.get('OUT', os.path.join(HERE, 'out'))


def tail(s, n=8):
    """The last n lines of s, after the panic message if there is one."""
    m = re.search(r'^.*panic: .*$', s, re.M)
    head = m.group(0).strip() + '\n' if m else ''
    lines = [l for l in s.strip().splitlines() if l.strip() and not l.lstrip().startswith('at ')]
    return head + '\n'.join(lines[-n:])


def write_prog(prog, base):
    base = os.path.abspath(base)
    dirs = {}
    for target in ('native', 'wasm', 'test'):
        d = os.path.join(base, target)
        if os.path.isdir(d):
            shutil.rmtree(d)
        os.makedirs(d)
        for name, text in program.render(prog, target).items():
            with open(os.path.join(d, name), 'w') as f:
                f.write(text)
        if target == 'wasm':
            os.symlink(os.path.join(HERE, 'platform'), os.path.join(d, 'platform'))
        dirs[target] = d
    with open(os.path.join(base, 'case.txt'), 'w') as f:
        f.write(pprint.pformat(prog, width=118, sort_dicts=False) + '\n')
    return dirs


def roc_check(d):
    """None if `roc check` reports no error, else its output."""
    p = subprocess.run([roc(), 'check', '--no-cache', 'app.roc'], cwd=d, capture_output=True, text=True,
                       timeout=TIMEOUT, stdin=subprocess.DEVNULL)
    text = p.stdout + p.stderr
    m = re.search(r'(\d+) errors? and', text)
    if (m and int(m.group(1)) > 0) or p.returncode not in (0, 2):
        return text
    return None


def run_config(d, name):
    """('ok', stdout) | ('build', message) | ('crash', message)."""
    target, mode, opt, spec = CONFIGS[name]
    try:
        if mode == 'test':
            p = subprocess.run([roc(), 'test', '--no-cache', 'app.roc'], cwd=d, capture_output=True, text=True,
                               timeout=TIMEOUT, stdin=subprocess.DEVNULL)
            text = p.stdout + p.stderr
            if re.search(r'All \(\d+\) tests passed', text):
                return ('ok', 'done')
            if re.search(r'\d+ (tests? )?failed', text):
                return ('ok', 'expect failures:\n' + tail(text, 30))
            return ('crash', 'rc=%d %s' % (p.returncode, tail(text)))
        flags = ['--opt=%s' % opt, '--specialize=%s' % spec, '--no-cache']
        if mode == 'run':
            p = subprocess.run([roc()] + flags + ['app.roc'], cwd=d, capture_output=True, text=True,
                               timeout=TIMEOUT, stdin=subprocess.DEVNULL)
            if p.returncode not in (0, 2) or not p.stdout.strip():
                return ('crash', 'rc=%d %s' % (p.returncode, tail(p.stderr + p.stdout)))
            return ('ok', p.stdout)
        out = os.path.join(d, 'bin-%s%s' % (name, '.wasm' if target == 'wasm' else ''))
        if os.path.exists(out):
            os.remove(out)
        cmd = [roc(), 'build'] + flags + ['--output=%s' % out, 'app.roc']
        if target == 'wasm':
            cmd.insert(2, '--target=wasm32')
        p = subprocess.run(cmd, cwd=d, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL)
        if not os.path.exists(out):
            return ('build', 'rc=%d %s' % (p.returncode, tail(p.stdout + p.stderr)))
        run = [os.environ.get('NODE', 'node'), os.path.join(HERE, 'run_wasm.js'), out] if target == 'wasm' else [out]
        q = subprocess.run(run, cwd=d, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL)
        os.remove(out)
        if q.returncode != 0:
            return ('crash', 'rc=%d %s' % (q.returncode, tail(q.stderr + q.stdout)))
        return ('ok', q.stdout)
    except subprocess.TimeoutExpired:
        return ('crash', 'timeout')


def judge(prog, base, configs, jobs):
    """('invalid', text) if roc check rejects the program, else ('ran', {config: (verdict, detail)})
    with verdict ok, wrong, crash or build."""
    dirs = write_prog(prog, base)
    err = roc_check(dirs['native'])
    if err is not None:
        return 'invalid', err
    with ThreadPoolExecutor(max_workers=jobs) as pool:
        done = dict(zip(configs, pool.map(lambda c: run_config(dirs[CONFIGS[c][0]], c), configs)))
    res = {}
    for c in configs:
        kind, text = done[c]
        if kind == 'ok':
            text = text.strip()
            res[c] = ('ok', '') if text == program.expected_output(prog) else ('wrong', text)
        else:
            res[c] = (kind, text)
    return 'ran', res


def bad(res):
    return sorted(c for c, (v, _) in res.items() if v != 'ok')


def explain(prog, detail):
    """Turn `uN case I arm A` lines into the case, the expected arm and bindings, and the arm taken."""
    out = []
    for m in re.finditer(r'u(\d+) case (\d+) arm (\d+)([^\n]*)', detail):
        ui, ci, arm = int(m.group(1)), int(m.group(2)), int(m.group(3))
        differ = m.group(4).split()
        u = prog['units'][ui]
        v, k, (bi, binds) = u['cases'][ci]
        pr = program.Printer(prog['decls'])
        what = 'wrong bindings (%s)' % ', '.join(differ) if arm == bi else 'wrong arm'
        out.append('  u%d: %s for %s (k=%d): expected arm %d %s, got arm %d' % (
            ui, what, pr.value(v, u['type']), k, bi,
            '{' + ', '.join('%s=%s' % (n, pr.value(bv, bt)) for n, bt, bv in binds) + '}', arm))
    return '\n'.join(out)


# ---------------------------------------------------------------- reduction

def signature(res):
    """What a reduction must keep: each config's verdict, and for a crash its message without numbers."""
    out = {}
    for c, (v, detail) in res.items():
        if v == 'crash':
            m = re.search(r'panic: ([^\n]*)', detail)
            detail = re.sub(r'\d+', 'N', m.group(1)) if m else ''
            out[c] = (v, detail)
        else:
            out[c] = (v, '')
    return out


def reduce(prog, base, configs, jobs, target_bad):
    """Greedy: drop units, then cases, then branches (recomputing expectations), while the same
    configs stay non-ok in the same way. It runs only two failing configs and one agreeing one."""
    _, res0 = judge(prog, base, configs, jobs)
    ok = [c for c in configs if c not in target_bad]
    sub = target_bad[:2] + ok[:1]
    want = {c: signature(res0)[c] for c in sub}

    def still(p):
        status, res = judge(p, base, sub, jobs)
        return status == 'ran' and signature(res) == want

    def recompute(u):
        env = model.Env(prog['decls'])
        cases = []
        for v, k, _ in u['cases']:
            got = model.choose(env, u['type'], u['branches'], v, k)
            if got is None:
                return None
            bi, b = got
            cases.append((v, k, (bi, [(n, b[n][0], b[n][1]) for n, _ in u['arms'][bi]])))
        return dict(u, cases=cases)

    cur = prog
    changed = True
    while changed:
        changed = False
        for i in range(len(cur['units'])):
            if len(cur['units']) == 1:
                break
            cand = dict(cur, units=cur['units'][:i] + cur['units'][i + 1:])
            if still(cand):
                cur, changed = cand, True
                break
        for ui in range(len(cur['units'])):
            u = cur['units'][ui]
            # cases: halves, then one at a time
            n = len(u['cases'])
            chunk = max(1, n // 2)
            while chunk >= 1:
                i = 0
                while i < len(cur['units'][ui]['cases']):
                    u = cur['units'][ui]
                    cs = u['cases'][:i] + u['cases'][i + chunk:]
                    if cs:
                        cand = dict(cur, units=cur['units'][:ui] + [dict(u, cases=cs)] + cur['units'][ui + 1:])
                        if still(cand):
                            cur, changed = cand, True
                            continue
                    i += chunk
                chunk //= 2
            bi = 0
            while bi < len(cur['units'][ui]['branches']):
                u = cur['units'][ui]
                if len(u['branches']) == 1:
                    break
                nu = dict(u, branches=u['branches'][:bi] + u['branches'][bi + 1:], arms=u['arms'][:bi] + u['arms'][bi + 1:])
                nu = recompute(nu)
                if nu is not None:
                    cand = dict(cur, units=cur['units'][:ui] + [nu] + cur['units'][ui + 1:])
                    if still(cand):
                        cur, changed = cand, True
                        continue
                bi += 1
    judge(cur, base, configs, jobs)
    return cur


# ---------------------------------------------------------------- commands

def parse_configs(s):
    cs = s.split(',') if s else list(CONFIGS)
    for c in cs:
        if c not in CONFIGS:
            sys.exit('unknown config %s (have %s)' % (c, ', '.join(CONFIGS)))
    return cs


def report_case(prog, res, f=sys.stdout):
    for c, (v, detail) in sorted(res.items()):
        if v == 'ok':
            continue
        print('  %s: %s' % (c, v), file=f)
        if v == 'wrong':
            ex = explain(prog, detail)
            print(ex if ex else '    ' + detail.replace('\n', '\n    '), file=f)
        else:
            print('    ' + detail.replace('\n', '\n    '), file=f)


MATCH1_PANICS = ('payload read of discriminant', 'published outcome discriminant')


def known_match1(prog, res, base, jobs):
    """#13427: under `.boxy`, a qualified tag pattern (`N.A(..)`) next to bare ones tests the tag twice and
    the Debug ARC certifier panics. It is this bug if each disagreeing config crashes that way and printing
    every tag pattern bare makes them agree."""
    b = bad(res)
    if not b or any(res[c][0] not in ('crash', 'build') or not any(s in res[c][1] for s in MATCH1_PANICS) for c in b):
        return False
    status, res2 = judge(dict(prog, qualify=False), base, b, jobs)
    return status == 'ran' and not bad(res2)


def known_match2(prog, res):
    """#13426: on wasm32 with --opt=dev, a string pattern with two or more delimiter steps writes its
    captures before it knows the arm matches, clobbering a value a later branch binds. It is this bug if
    only wasm-dev configs disagree, with wrong results, and some unit has such a pattern."""
    b = bad(res)
    if not b or any(c not in ('wasm-dev', 'wasm-dev-spec') or res[c][0] != 'wrong' for c in b):
        return False

    def multi(p):
        """Whether some string pattern inside p has a capture followed by text and then another step."""
        if not isinstance(p, tuple):
            return False
        if p and p[0] == 'SP':
            return len(p[1]) >= 5
        return any(multi(c) for c in p)
    return any(multi(p) for u in prog['units'] for alts, _ in u['branches'] for p in alts)


def walk(p):
    """Every node of a nested tuple."""
    if isinstance(p, tuple):
        yield p
        for c in p:
            yield from walk(c)


def has_str_test(p):
    return any(n and n[0] == 'SP' or (n and n[0] == 'Lit' and isinstance(n[1], tuple) and n[1][0] == 'str')
               for n in walk(p))


def known_match3(prog, res):
    """#13425: a row whose string pattern matched but which then fails (its guard, or another column)
    skips later rows with another string shape. It is this bug if every config that ran gives a wrong arm
    (never wrong bindings alone), the rest crash with a known panic or agree, and each unit with a wrong
    case has at least two rows that test a string."""
    units = set()
    for c, (v, detail) in res.items():
        if v == 'wrong':
            if 'expect failures' in detail:
                continue
            for m in re.finditer(r'u(\d+) case (\d+) arm (\d+)', detail):
                u = prog['units'][int(m.group(1))]
                if u['cases'][int(m.group(2))][2][0] == int(m.group(3)):
                    return False
                units.add(int(m.group(1)))
        elif v != 'ok' and not any(s in detail for s in KNOWN_PANICS):
            return False
    if not units:
        return False
    return all(sum(1 for alts, _ in prog['units'][ui]['branches'] for p in alts if has_str_test(p)) >= 2
               for ui in units)


KNOWN_PANICS = ('payload read of discriminant', 'did not preserve the exact entry unit')


def known_match4(prog, res):
    """#13428: a string-pattern capture returned inside a tag makes the ARC certifier panic about outcome
    restitution under `.boxy`, and so does a variable of a guarded branch with alternatives. It is this bug
    if every disagreeing config panics that way and some branch binds a capture or is guarded with `|`."""
    b = bad(res)
    if not b or any(res[c][0] not in ('crash', 'build') or KNOWN_PANICS[1] not in res[c][1] for c in b):
        return False
    capture = any(n[0] == 'SP' and any(n[1][1::2]) for u in prog['units'] for alts, _ in u['branches']
                  for p in alts for n in walk(p) if n)
    guarded_alts = any(g is not None and len(alts) > 1 for u in prog['units'] for alts, g in u['branches'])
    return capture or guarded_alts


def cmd_fuzz(a):
    configs = parse_configs(a.configs)
    root = os.path.join(out_dir(), 'fuzz')
    os.makedirs(root, exist_ok=True)
    tsv = open(os.path.join(root, 'results.tsv'), 'a')
    counts = {}
    new = 0
    for i in range(a.n):
        seed = a.seed + i
        prog = program.make_program(seed)
        base = os.path.join(root, str(seed))
        status, res = judge(prog, base, configs, a.jobs)
        if status == 'invalid':
            verdict = 'invalid'
            detail = res.strip().splitlines()[:6]
        else:
            b = bad(res)
            verdict = 'ok' if not b else 'bad:' + ','.join(b)
        counts[verdict.split(':')[0]] = counts.get(verdict.split(':')[0], 0) + 1
        ncases = sum(len(u['cases']) for u in prog['units'])
        tsv.write('%d\t%s\t%d\t%d\n' % (seed, verdict, len(prog['units']), ncases))
        tsv.flush()
        print('seed %d: %s (%d units, %d cases)' % (seed, verdict, len(prog['units']), ncases),
              flush=True)
        if status == 'invalid':
            print('  ' + '\n  '.join(detail))
            continue
        if verdict == 'ok':
            shutil.rmtree(base, ignore_errors=True)
            continue
        if known_match4(prog, res):
            print('  known-#13428: ARC outcome-restitution panic, and a branch binds a string capture')
            counts['known-#13428'] = counts.get('known-#13428', 0) + 1
            continue
        if known_match3(prog, res):
            print('  known-#13425: wrong arm after a guarded string row')
            counts['known-#13425'] = counts.get('known-#13425', 0) + 1
            continue
        if known_match2(prog, res):
            print('  known-#13426: wasm-dev only, and a string pattern has two delimiter steps')
            counts['known-#13426'] = counts.get('known-#13426', 0) + 1
            continue
        if known_match1(prog, res, os.path.join(base, 'unqualified'), a.jobs):
            print('  known-#13427: bare tag patterns instead of qualified ones make every config agree')
            counts['known-#13427'] = counts.get('known-#13427', 0) + 1
            continue
        new += 1
        report_case(prog, res)
        if not a.no_reduce:
            red = reduce(prog, os.path.join(base, 'min'), configs, a.jobs, bad(res))
            print('  reduced: out/fuzz/%d/min/native/app.roc' % seed)
            _, rres = judge(red, os.path.join(base, 'min'), configs, a.jobs)
            report_case(red, rres)
    print('summary: ' + ', '.join('%s=%d' % kv for kv in sorted(counts.items())))
    return 1 if new or counts.get('invalid') else 0



def load(path):
    with open(path) as f:
        return ast.literal_eval(f.read())


def cmd_check(a):
    configs = parse_configs(a.configs)
    rc = 0
    for path in a.cases:
        prog = load(path)
        base = os.path.join(out_dir(), 'check', os.path.basename(os.path.dirname(os.path.abspath(path))))
        status, res = judge(prog, base, configs, a.jobs)
        if status == 'invalid':
            print('%s: invalid\n%s' % (path, tail(res)))
            rc = 1
            continue
        b = bad(res)
        known = known_bad(path)
        print('%s: %s%s' % (path, 'ok' if not b else 'bad:' + ','.join(b), ' (expected %s)' % ','.join(known) if known else ''))
        report_case(prog, res)
        if b != sorted(c for c in known if c in configs):
            rc = 1
    return rc


def known_bad(path):
    """tests/<name>/known.txt lists the configs an open issue makes disagree (one per line)."""
    k = os.path.join(os.path.dirname(os.path.abspath(path)), 'known.txt')
    if not os.path.exists(k):
        return []
    return sorted(l.split('#')[0].strip() for l in open(k) if l.split('#')[0].strip())


def cmd_reduce(a):
    configs = parse_configs(a.configs)
    prog = load(a.case)
    base = os.path.join(out_dir(), 'reduce')
    status, res = judge(prog, base, configs, a.jobs)
    if status != 'ran' or not bad(res):
        print('nothing to reduce: %s' % (status if status != 'ran' else 'every config agrees'))
        return 1
    red = reduce(prog, base, configs, a.jobs, bad(res))
    _, rres = judge(red, base, configs, a.jobs)
    report_case(red, rres)
    print('reduced program: %s' % os.path.join(base, 'native', 'app.roc'))
    return 0


def cmd_gen(a):
    prog = program.make_program(a.seed)
    sys.stdout.write(program.render(prog, a.target)['app.roc'])
    return 0


def selftest(a):
    # Model unit tests: the semantics of each pattern form, on hand-picked values.
    env = model.Env({})
    S = ('S',)
    sp = ('', 'd', '/', 'f', '.txt')
    assert model.str_match(sp, 'a/b/c.txt') == [('d', 'a'), ('f', 'b/c')]
    assert model.str_match(('users/', 'id', ''), 'users/42') == [('id', '42')]
    assert model.str_match(('users/', 'id', ''), 'posts/42') is None
    assert model.str_match(('', 'f', '.txt'), 'a.txt.txt') is None  # the capture stops at the first `.txt`
    L = ('L', ('N', 'U64'))
    lst = ('list', (('int', 1), ('int', 2), ('int', 3)))
    assert model.match(env, ('Lst', (('B', 'a'),), ('r', (('B', 'z'),))), L, lst) == {
        'a': (('N', 'U64'), ('int', 1)), 'z': (('N', 'U64'), ('int', 3)), 'r': (L, ('list', (('int', 2),)))}
    assert model.match(env, ('Lst', (W := ('W',), W), ('r', (W, W))), L, lst) is None
    R = ('R', (('a', ('N', 'U64')), ('b', S)))
    rv = ('rec', (('a', ('int', 1)), ('b', ('str', 'x'))))
    assert model.match(env, ('Rec', (('a', ('Lit', ('int', 1))),), 'rest'), R, rv) == {
        'rest': (('R', (('b', S),)), ('rec', (('b', ('str', 'x')),)))}
    T = ('U', (('A', (('N', 'U64'),)), ('B', ())), None)
    br = [([('C', 'A', (('B', 'n'),))], ('gt', 'n', 1)), ([('C', 'A', (('B', 'n'),)), ('C', 'B', ())], None)]
    assert model.choose(env, T, br, ('tag', 'A', (('int', 5),)), 0)[0] == 0
    assert model.choose(env, T, br, ('tag', 'A', (('int', 1),)), 0) == (1, {'n': (('N', 'U64'), ('int', 1))})
    # Generator: every case's expectation is what the model says, and every program renders.
    for seed in range(200):
        prog = program.make_program(seed)
        for u in prog['units']:
            e = model.Env(prog['decls'])
            for v, k, (bi, binds) in u['cases']:
                assert model.choose(e, u['type'], u['branches'], v, k)[0] == bi
        for t in ('native', 'wasm', 'test'):
            program.render(prog, t)
    print('model and generator checks passed')
    # Fixtures.
    cases = sorted(os.path.join(HERE, 'tests', d, 'case.txt') for d in os.listdir(os.path.join(HERE, 'tests'))
                   if os.path.exists(os.path.join(HERE, 'tests', d, 'case.txt')))
    a.cases = cases
    rc = cmd_check(a)
    # The comparison must see a wrong answer: expect the wrong arm for one case of the first fixture.
    prog = load(os.path.join(HERE, 'tests', 'rich', 'case.txt'))
    for u in prog['units']:
        for ci, (v, k, (bi, _)) in enumerate(u['cases']):
            other = [j for j, arm in enumerate(u['arms']) if j != bi and not arm]
            if other:
                u['cases'][ci] = (v, k, (other[0], []))
                break
        else:
            continue
        break
    else:
        sys.exit('selftest: no case to falsify in tests/rich')
    sub = [c for c in ('interp', 'dev') if c in parse_configs(a.configs)]
    _, res = judge(prog, os.path.join(out_dir(), 'selftest-negative'), sub, a.jobs)
    if any(v != 'wrong' for v, _ in res.values()):
        print('selftest: a falsified expectation was not reported as wrong: %s' % res)
        rc = 1
    else:
        print('falsified expectation reported as wrong on %s' % ', '.join(sub))
    return rc


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest='cmd', required=True)
    f = sub.add_parser('fuzz')
    f.add_argument('n', nargs='?', type=int, default=20)
    f.add_argument('--seed', type=int, default=1)
    f.add_argument('--configs', default='')
    f.add_argument('--jobs', type=int, default=int(os.environ.get('JOBS', '3')))
    f.add_argument('--no-reduce', action='store_true')
    c = sub.add_parser('check')
    c.add_argument('cases', nargs='+')
    c.add_argument('--configs', default='')
    c.add_argument('--jobs', type=int, default=int(os.environ.get('JOBS', '3')))
    r = sub.add_parser('reduce')
    r.add_argument('case')
    r.add_argument('--configs', default='')
    r.add_argument('--jobs', type=int, default=int(os.environ.get('JOBS', '3')))
    g = sub.add_parser('gen')
    g.add_argument('seed', type=int)
    g.add_argument('--target', default='native')
    s = sub.add_parser('selftest')
    s.add_argument('--configs', default='')
    s.add_argument('--jobs', type=int, default=int(os.environ.get('JOBS', '3')))
    a = ap.parse_args()
    fn = {'fuzz': cmd_fuzz, 'check': cmd_check, 'reduce': cmd_reduce, 'gen': cmd_gen, 'selftest': selftest}[a.cmd]
    sys.exit(fn(a))


if __name__ == '__main__':
    main()
