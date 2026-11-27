"""Run every exported program before and after SpecConstr and compare (RULES.md S1 to S3).

Usage: python3 check.py DIR [--sites FILE]

DIR holds `<n>-after_lift.lifted` and `<n>-after_spec_constr.lifted` pairs written by one compiler
run. For every pair and every root, both programs run on the same inputs; a root whose outcomes
differ is a divergence. A rule violation in either program is reported on its own. A root either
side cannot run (a model limit) is counted as skipped, with the reason.

Prints one line per finding and a summary line; exits 1 when there is a finding.
"""
import os
import re
import sys
import threading

import interp
import lifted

FILE = re.compile(r"^(\d+)-(after_lift|after_spec_constr)\.lifted$")


def pairs(d):
    found = {}
    for name in os.listdir(d):
        m = FILE.match(name)
        if m:
            found.setdefault(int(m.group(1)), {})[m.group(2)] = os.path.join(d, name)
    return [(n, s["after_lift"], s["after_spec_constr"]) for n, s in sorted(found.items())
            if "after_lift" in s and "after_spec_constr" in s]


INT_INPUTS = [0, 1, 3, -2]


def inputs(m, types):
    """A few deterministic argument tuples for a root's parameter types, or None when one has no
    input values the model can make (a function, for example)."""
    per = []
    for t in types:
        r = m.resolve(t)
        if r[0] == "prim" and r[1] in interp.INT_RANGES:
            lo, _ = interp.INT_RANGES[r[1]]
            per.append([x for x in INT_INPUTS if x >= lo])
        elif r[0] == "prim" and r[1] == "str":
            per.append([b"", b"ab"])
        elif r[0] == "list":
            e = m.resolve(r[1])
            if e[0] == "prim" and e[1] == "str":
                per.append([interp.Lst(()), interp.Lst((b"x", b"yz"))])
            elif e[0] == "prim" and e[1] in interp.INT_RANGES:
                per.append([interp.Lst(()), interp.Lst((1, 2, 3))])
            else:
                return None
        elif r[0] == "zst" or (r[0] == "record" and not r[1]):
            per.append([interp.UNIT])
        else:
            return None
    n = max([len(p) for p in per] + [1])
    return [tuple(p[i % len(p)] for p in per) for i in range(n)]


class Result:
    def __init__(self):
        self.programs = 0
        self.roots = 0
        self.runs = 0
        self.compared = 0
        self.skipped = {}
        self.findings = []
        # What the test program's roots do on the interpreter, after SpecConstr, to compare with
        # what `roc test` printed (E1): dbg texts, number of failed tests, whether any run was skipped.
        self.test_dbg = []
        self.test_failed = 0
        self.test_programs = 0
        self.test_complete = True


def check_pair(n, lpath, spath, res):
    try:
        a = lifted.load(lpath)
        b = lifted.load(spath)
    except lifted.FormatError as e:
        res.findings.append("%d: export does not parse: %s" % (n, e))
        return
    res.programs += 1
    if [(f, k) for f, k, _ in a.roots] != [(f, k) for f, k, _ in b.roots]:
        res.findings.append("%d: SpecConstr changed the roots: %r -> %r" % (n, a.roots, b.roots))
        return
    ma, mb = interp.Machine(a), interp.Machine(b)
    is_test = bool(a.roots) and all(k == "test_expect" for _, k, _ in a.roots)
    res.test_programs += is_test
    for fid, kind, order in a.roots:
        res.roots += 1
        ins = inputs(ma, [t for _, t in a.fns[fid][0]])
        if ins is None:
            res.skipped["root parameter type"] = res.skipped.get("root parameter type", 0) + 1
            continue
        for args in ins:
            res.runs += 1
            ma.fuel = mb.fuel = 2_000_000
            oa = ma.run_root(fid, list(args))
            ob = mb.run_root(fid, list(args))
            where = "%d root fn %d (%s) args (%s)" % (n, fid, kind, ", ".join(interp.render(x) for x in args))
            if oa.kind == "violation":
                res.findings.append("S3 after_lift %s: %s" % (where, oa))
            if ob.kind == "violation" and oa.kind != "violation":
                res.findings.append("S3 after_spec_constr %s: %s" % (where, ob))
            if is_test:
                res.test_dbg += [t[4:] for t in ob.trace if t.startswith("dbg ")]
                if ob.kind != "value" or ob.detail != "True" or "expect failed" in ob.trace:
                    res.test_failed += 1
                if ob.kind in ("limit", "violation"):
                    res.test_complete = False
            if oa.kind == "limit" or ob.kind == "limit":
                why = (oa if oa.kind == "limit" else ob).detail
                res.skipped[why] = res.skipped.get(why, 0) + 1
                continue
            res.compared += 1
            if oa.key() != ob.key() and "violation" not in (oa.kind, ob.kind):
                res.findings.append("S1 %s:\n    after_lift:        %s\n    after_spec_constr: %s" % (where, oa, ob))


def check_dir(d):
    res = Result()
    for n, lp, sp in pairs(d):
        check_pair(n, lp, sp, res)
    return res


def summary(res):
    sk = sum(res.skipped.values())
    top = sorted(res.skipped.items(), key=lambda kv: -kv[1])[:4]
    return "programs %d, roots %d, runs %d, compared %d, skipped %d%s, findings %d" % (
        res.programs, res.roots, res.runs, res.compared, sk,
        (" (" + "; ".join("%s: %d" % kv for kv in top) + ")") if top else "", len(res.findings))


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    res = check_dir(argv[1])
    for f in res.findings:
        print(f)
    print(summary(res))
    return 1 if res.findings else 0


def big_stack(fn, *args):
    threading.stack_size(512 * 1024 * 1024)
    box = []
    t = threading.Thread(target=lambda: box.append(fn(*args)))
    t.start()
    t.join()
    return box[0] if box else 3


if __name__ == "__main__":
    sys.exit(big_stack(main, sys.argv))
