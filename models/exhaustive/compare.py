"""Run `roc check` on a case, read its verdict from the diagnostics, and compare it with the model.

The abstraction function is the verdict parse: each `redundant pattern` / `unmatchable pattern` header
is mapped back to a (branch, alternative) through the line:column the emitter recorded, and any
`non exhaustive` header makes the verdict non-exhaustive. Everything else in the report is dropped.
"""

import ast
import os
import re
import subprocess
from itertools import combinations

from gen import SINGLE_CTX
from model import KNOWN_BUGS, Env, analyze

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, 'out')
HDR = re.compile(r'^── ([✗●]) (.+?) ─+ ?(?:\S+?:(\d+):(\d+))?\s*$')
IGNORED = {'unused variable', 'unconditional condition', 'redundant open tag union'}
CASE_KEYS = ('ctx', 'type', 'nominals', 'branches', 'locs')
TRAILER = '# exhaustive-model case'


def roc_binary():
    return os.environ['ROC']


# ---------------- compiler side ----------------

def run_roc(src, name):
    d = os.path.join(OUT, 'work')
    os.makedirs(d, exist_ok=True)
    path = os.path.join(d, name + '.roc')
    with open(path, 'w') as f:
        f.write(src)
    try:
        p = subprocess.run([roc_binary(), 'check', path], capture_output=True, text=True, timeout=60)
        out, rc = p.stdout + p.stderr, p.returncode
    except subprocess.TimeoutExpired:
        out, rc = 'TIMEOUT', -999
    os.remove(path)
    return out, rc


def parse(out):
    diags = []
    for line in out.splitlines():
        m = HDR.match(line)
        if m:
            title = m.group(2).strip()
            if re.match(r'\d+ errors? and \d+ warnings?', title):
                continue
            diags.append((title, int(m.group(3)) if m.group(3) else None, int(m.group(4)) if m.group(4) else None))
    return diags


def actual_verdict(diags, locs):
    nonex = any(t.startswith('non exhaustive') for t, _, _ in diags)
    red, unm, other = [], [], []
    for t, l, c in diags:
        if t == 'redundant pattern':
            red.append(locs.get((l, c), ('?', l, c)))
        elif t == 'unmatchable pattern':
            unm.append(locs.get((l, c), ('?', l, c)))
        elif not (t.startswith('non exhaustive') or t in IGNORED):
            other.append(t)
    return dict(exhaustive=not nonex, redundant=sorted(red, key=str), unmatchable=sorted(unm, key=str), other=other)


# ---------------- model side ----------------

def normalize_expected(case, exp):
    """Reporting policy (RULES.md REP-*)."""
    branches = case['branches']

    def collapse(lst):  # REP-ALT
        out = []
        for bi, (alts, _g) in enumerate(branches):
            mine = [x for x in lst if x[0] == bi]
            out += [(bi, 0)] if mine and len(mine) == len(alts) else mine
        return sorted(out, key=str)
    red = collapse(exp['redundant'])
    unm = [] if case['ctx'] in SINGLE_CTX else collapse(exp['unmatchable'])  # REP-DESTR
    unm = [x for x in unm if not branches[x[0]][1]]  # REP-GUARD
    return dict(exhaustive=exp['exhaustive'], redundant=red, unmatchable=unm)


def expected(case, compat=frozenset()):
    return normalize_expected(case, analyze(Env(case['nominals']), case['type'], case['branches'], compat))


def same_useless(a, b):
    return sorted(a['redundant'] + a['unmatchable'], key=str) == sorted(b['redundant'] + b['unmatchable'], key=str)


def explain(case, act):
    """The smallest set of known-bug emulations under which the model agrees with the compiler."""
    for n in range(1, len(KNOWN_BUGS) + 1):
        for combo in combinations(sorted(KNOWN_BUGS), n):
            exp = expected(case, frozenset(combo))
            if act['exhaustive'] == exp['exhaustive'] and same_useless(act, exp):
                return '+'.join(combo)
    return None


def classify(case, out, rc):
    """Returns (status, expected verdict, actual verdict). Statuses are listed in README.md."""
    act = actual_verdict(parse(out), case['locs'])
    exp = expected(case)
    if rc not in (0, 1, 2) or 'panic' in out.lower() or out == 'TIMEOUT':
        return 'crash', exp, act
    if act['other']:
        if case['ctx'] == 'lambda_arg' and set(act['other']) <= {'type mismatch', 'invalid nominal tag'}:
            # A refutable lambda argument is rejected as a type mismatch. That is right when the pattern is
            # non-exhaustive; when it is exhaustive the rejection is 13293.
            return ('lambda-nonexh' if not exp['exhaustive'] else 'known-lambda_reject'), exp, act
        return 'invalid', exp, act
    if (act['exhaustive'], act['redundant'], act['unmatchable']) == (exp['exhaustive'], exp['redundant'],
                                                                      exp['unmatchable']):
        return 'ok', exp, act
    if act['exhaustive'] == exp['exhaustive'] and same_useless(act, exp):
        return 'label', exp, act
    known = explain(case, act)
    return ('known-' + known if known else 'disagree'), exp, act


def check_case(case, name):
    out, rc = run_roc(case['src'], name)
    status, exp, act = classify(case, out, rc)
    return status, exp, act, out, rc


# ---------------- case files ----------------
#
# A case file is the generated program followed by a trailer of `# key: <Python literal>` comment lines
# holding the model's input. The trailer comes last so the recorded line numbers stay valid.

def case_text(case, extra=()):
    lines = [case['src'].rstrip('\n'), '', TRAILER]
    lines += ['# %s: %r' % (k, case[k]) for k in CASE_KEYS]
    lines += ['# %s: %r' % kv for kv in extra]
    return '\n'.join(lines) + '\n'


def read_case(path):
    with open(path) as f:
        text = f.read()
    if TRAILER not in text:
        raise SystemExit('%s: no "%s" trailer; check only reads programs written by fuzz' % (path, TRAILER))
    src, trailer = text.split('\n' + TRAILER + '\n', 1)
    case = {'src': src.rstrip('\n') + '\n'}
    for line in trailer.splitlines():
        k, v = line[2:].split(': ', 1)
        case[k] = ast.literal_eval(v)
    missing = [k for k in CASE_KEYS if k not in case]
    if missing:
        raise SystemExit('%s: trailer is missing %s' % (path, ', '.join(missing)))
    return case
