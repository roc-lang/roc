"""Command line for the exhaustiveness model. Run through run.sh."""

import argparse
import os
import random
import sys
from concurrent.futures import ThreadPoolExecutor

import compare
from compare import OUT, case_text, check_case, read_case
from gen import CONTEXTS, make_case
from model import Env, analyze, missing_witness, W
from reduce import reduce

HERE = os.path.dirname(os.path.abspath(__file__))


def show(status, exp, act):
    print('  model:    exhaustive=%s redundant=%s unmatchable=%s' % (exp['exhaustive'], exp['redundant'],
                                                                     exp['unmatchable']))
    print('  compiler: exhaustive=%s redundant=%s unmatchable=%s%s' % (
        act['exhaustive'], act['redundant'], act['unmatchable'],
        (' other=%s' % act['other']) if act['other'] else ''))


def cmd_check(args):
    bad = 0
    for path in args.files:
        case = read_case(path)
        status, exp, act, _out, _rc = check_case(case, 'check')
        print('%s: %s' % (path, status))
        if status != 'ok':
            show(status, exp, act)
            bad += 1
    return 1 if bad else 0


def cmd_fuzz(args):
    seed = args.seed if args.seed is not None else random.randrange(1 << 30)
    d = os.path.join(OUT, 'fuzz')
    os.makedirs(d, exist_ok=True)
    print('seeds %d..%d, ctx=%s' % (seed, seed + args.n - 1, args.ctx or 'random'))

    def one(s):
        case = make_case(s, args.ctx)
        status, exp, act, out, rc = check_case(case, 'c%d' % s)
        if status != 'ok':
            sub = os.path.join(d, status)
            os.makedirs(sub, exist_ok=True)
            with open(os.path.join(sub, 'c%d.roc' % s), 'w') as f:
                f.write(case_text(case))
            with open(os.path.join(sub, 'c%d.txt' % s), 'w') as f:
                f.write('expected=%r\nactual=%r\nrc=%d\n\n%s' % (exp, act, rc, out))
        return s, case['ctx'], status

    stats = {}
    with ThreadPoolExecutor(args.jobs) as ex, open(os.path.join(d, 'results.tsv'), 'a') as res:
        for s, ctx, status in ex.map(one, range(seed, seed + args.n)):
            stats[status] = stats.get(status, 0) + 1
            res.write('%d\t%s\t%s\n' % (s, ctx, status))
    for k in sorted(stats):
        print('%-28s %d' % (k, stats[k]))
    print('non-ok cases are in %s/<status>/' % os.path.relpath(d))
    return 1 if stats.get('disagree') or stats.get('crash') else 0


def cmd_reduce(args):
    case = read_case(args.file)
    final, sig, out = reduce(case)
    print(final['src'])
    print('# status: %s, differing: %s' % sig)
    path = os.path.join(OUT, 'reduced.roc')
    with open(path, 'w') as f:
        f.write(case_text(final))
    print('# written to %s' % os.path.relpath(path))
    print(out)
    return 0


# ---------------- selftest ----------------

U64 = ('N', 'U64')
EMPTY = ('U', (), False, None)
TRY_VOID = ('U', (('Ok', (U64,)), ('Err', (EMPTY,))), False, 'Try')


def C(tag, *args):
    return ('C', tag, tuple(args))


def model_tests():
    """Pure model checks, one per rule. Each is (name, got, want)."""
    loop = {'Loop': ('U', (('Loop', (('NR', 'Loop'),)),), False, 'Loop')}
    bad = {'Bad': ('U', (('B', (('NR', 'Bad'), EMPTY)),), False, 'Bad')}
    lst = ('L', U64)
    ab = ('U', (('A', ()), ('B', ())), False, None)

    def a(t, branches, nominals=()):
        return analyze(Env(dict(nominals)), t, branches)
    yield 'INHAB: Loop := [Loop(Loop)] is inhabited', Env(loop).inhabited(('NR', 'Loop')), True
    yield 'INHAB: Bad := [B(Bad, [])] is not', Env(bad).inhabited(('NR', 'Bad')), False
    yield 'INHAB: an open union is inhabited', Env({}).inhabited(('U', (), True, None)), True
    yield 'CTOR-U: Ok(_) covers Try(U64, [])', a(TRY_VOID, [([C('Ok', W)], False)])['exhaustive'], True
    yield 'UNM: Err(_) on Try(U64, []) is unmatchable', \
        a(TRY_VOID, [([C('Ok', W)], False), ([C('Err', W)], False)])['unmatchable'], [(1, 0)]
    yield 'EXH: Ok(0) alone on Try(U64, []) is not exhaustive', \
        a(TRY_VOID, [([C('Ok', ('Lit', 0))], False)])['exhaustive'], False
    yield 'EXH: _ on [] is exhaustive', a(EMPTY, [([W], False)])['exhaustive'], True
    yield 'CTOR-List: [] and [_, ..] cover List(U64)', \
        a(lst, [([('Lst', (), None)], False), ([('Lst', (W,), ())], False)])['exhaustive'], True
    yield 'CTOR-List: [] and [_] do not', \
        a(lst, [([('Lst', (), None)], False), ([('Lst', (W,), None)], False)])['exhaustive'], False
    yield 'RED: [.., _] after [_, ..] is redundant', \
        a(lst, [([('Lst', (W,), ())], False), ([('Lst', (), (W,))], False)])['redundant'], [(1, 0)]
    yield 'RED: a second A is redundant', \
        a(ab, [([C('A')], False), ([C('A')], False), ([C('B')], False)])['redundant'], [(1, 0)]
    yield 'RED-G: a guard does not cover', a(ab, [([C('A')], True), ([C('B')], False)])['exhaustive'], False
    yield 'RED-G: a covered guarded row is set apart', \
        a(ab, [([W], False), ([C('A')], True)])['redundant_guarded'], [(1, 0)]
    yield 'witness: [A, B] minus A is B', missing_witness(Env({}), ab, [C('A')]), C('B')
    yield 'emulation 13289 accepts Ok(0) alone', analyze(
        Env({}), TRY_VOID, [([C('Ok', ('Lit', 0))], False)], frozenset({'uninh_sibling'}))['exhaustive'], True


def generator_tests():
    """Every context emits, generation is deterministic, and every recorded mark is in the source."""
    for ctx in CONTEXTS:
        for s in range(30):
            c1, c2 = make_case(s, ctx), make_case(s, ctx)
            if c1['src'] != c2['src']:
                yield 'gen %s seed %d is deterministic' % (ctx, s), False, True
            lines = c1['src'].split('\n')
            for (ln, col) in c1['locs']:
                if not (0 < ln <= len(lines) and len(lines[ln - 1]) >= col):
                    yield 'gen %s seed %d mark %d:%d' % (ctx, s, ln, col), False, True
    yield 'generator: %d contexts x 30 seeds' % len(CONTEXTS), True, True


def cmd_selftest(_args):
    failed = 0
    for name, got, want in list(model_tests()) + list(generator_tests()):
        ok = got == want
        failed += not ok
        print('%s  %s%s' % ('pass' if ok else 'FAIL', name, '' if ok else ' (got %r, want %r)' % (got, want)))
    tests = os.path.join(HERE, 'tests')
    for fn in sorted(os.listdir(tests)):
        if not fn.endswith('.roc'):
            continue
        case = read_case(os.path.join(tests, fn))
        status, exp, act, _out, _rc = check_case(case, 'selftest')
        ok = status == case['expect']
        failed += not ok
        print('%s  tests/%s: %s%s' % ('pass' if ok else 'FAIL', fn, status,
                                     '' if ok else ' (want %s)' % case['expect']))
        if not ok:
            show(status, exp, act)
    print('selftest: %s' % ('FAILED (%d)' % failed if failed else 'ok'))
    return 1 if failed else 0


def main(argv):
    p = argparse.ArgumentParser(prog='run.sh')
    sub = p.add_subparsers(dest='cmd', required=True)
    sub.add_parser('selftest')
    c = sub.add_parser('check')
    c.add_argument('files', nargs='+')
    f = sub.add_parser('fuzz')
    f.add_argument('n', nargs='?', type=int, default=200)
    f.add_argument('--seed', type=int)
    f.add_argument('--ctx', choices=CONTEXTS)
    f.add_argument('--jobs', type=int, default=os.cpu_count() or 4)
    r = sub.add_parser('reduce')
    r.add_argument('file')
    args = p.parse_args(argv)
    if not os.path.isfile(compare.roc_binary()):
        raise SystemExit('roc not found at %s; set ROC' % compare.roc_binary())
    return {'selftest': cmd_selftest, 'check': cmd_check, 'fuzz': cmd_fuzz, 'reduce': cmd_reduce}[args.cmd](args)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
