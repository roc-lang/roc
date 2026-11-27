"""Greedy structural reducer for a case the compiler and the model disagree on.

It drops branches, alternatives and guards and replaces subpatterns with wildcards, keeping a step
when the compiler still gives the same status and the same verdict keys still differ.
"""

from compare import check_case
from gen import render
from model import W


def build(case, branches):
    c = dict(case)
    c['branches'] = branches
    c['src'], c['locs'] = render(c)
    return c


def signature(case):
    status, exp, act, out, _rc = check_case(case, 'reduce')
    return (status, tuple(k for k in ('exhaustive', 'redundant', 'unmatchable') if exp[k] != act[k])), out


def sub_shrinks(p):
    """Smaller variants of pattern p."""
    if p[0] == 'W':
        return
    yield W
    if p[0] == 'C':
        for i, a in enumerate(p[2]):
            for s in sub_shrinks(a):
                yield ('C', p[1], p[2][:i] + (s,) + p[2][i + 1:])
    elif p[0] == 'Tup':
        for i, a in enumerate(p[1]):
            for s in sub_shrinks(a):
                yield ('Tup', p[1][:i] + (s,) + p[1][i + 1:])
    elif p[0] == 'Rec':
        for i, (f, a) in enumerate(p[1]):
            yield ('Rec', p[1][:i] + p[1][i + 1:], True)
            for s in sub_shrinks(a):
                yield ('Rec', p[1][:i] + ((f, s),) + p[1][i + 1:], p[2])
    elif p[0] == 'Lst':
        for i, a in enumerate(p[1]):
            for s in sub_shrinks(a):
                yield ('Lst', p[1][:i] + (s,) + p[1][i + 1:], p[2])
        for i, a in enumerate(p[2] or ()):
            for s in sub_shrinks(a):
                yield ('Lst', p[1], p[2][:i] + (s,) + p[2][i + 1:])


def candidates(branches):
    for i in range(len(branches)):
        if len(branches) > 1:
            yield branches[:i] + branches[i + 1:]
    for i, (alts, g) in enumerate(branches):
        if g:
            yield branches[:i] + [(alts, False)] + branches[i + 1:]
        if len(alts) > 1:
            for j in range(len(alts)):
                yield branches[:i] + [(alts[:j] + alts[j + 1:], g)] + branches[i + 1:]
        for j, a in enumerate(alts):
            for s in sub_shrinks(a):
                yield branches[:i] + [(alts[:j] + [s] + alts[j + 1:], g)] + branches[i + 1:]


def reduce(case):
    """Returns (reduced case, signature, compiler output)."""
    sig, _ = signature(case)
    if sig[0] == 'ok':
        raise SystemExit('the compiler and the model agree on this case; nothing to reduce')
    branches = case['branches']
    progress = True
    while progress:
        progress = False
        for cand in candidates(branches):
            if signature(build(case, cand))[0] == sig:
                branches = cand
                progress = True
                break
    final = build(case, branches)
    _, out = signature(final)
    return final, sig, out
