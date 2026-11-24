"""Check lambda sets against the closures that really reach each call (RULES.md L1 to L3).

Usage: python3 check.py is the SpecConstr check; this is: python3 lsets.py DIR

DIR holds `<n>-lambda_solved.lifted` files written under `ROC_LAMBDA_SET_EXPORT`: the program
Lambda Solved consumed (after it gave every closure capture slots of its own) plus one `c` record per
value call direct LIR lowering lowered, naming the lifted functions of the callee's callable
variants, or `erased`. Every root runs on the interpreter; each executed `call_value` must call a
function its callee's recorded lambda set names (L1), every closure called must have all its capture
slots bound (L2), and an executed value call must have been lowered at all (L3, reported as a count
only, since a call in a body no root reaches at run time of the compiled program may be inlined away).
"""
import os
import re
import sys

import check
import interp
import lifted

FILE = re.compile(r"^(\d+)-lambda_solved\.lifted$")


def parse_sites(text):
    """The `c` records of a lambda-set export: callee expr -> set of fn ids, or None for erased."""
    sites = {}
    for line in text.split("\n"):
        if not line.startswith("c "):
            continue
        t = line.split(" ")
        callee = int(t[1])
        if t[2] == "erased":
            sites[callee] = None
            continue
        n = int(t[3])
        members = {int(x) for x in t[4:4 + n]}
        if callee in sites and sites[callee] is None:
            continue
        sites.setdefault(callee, set()).update(members)
    return sites


class Result:
    def __init__(self):
        self.programs = 0
        self.runs = 0
        self.calls_checked = 0
        self.unlowered = 0
        self.erased = 0
        self.skipped = {}
        self.findings = []


def check_file(path, res):
    with open(path, encoding="ascii") as f:
        text = f.read()
    body = "\n".join(l for l in text.split("\n") if not l.startswith("c "))
    try:
        prog = lifted.parse(body)
    except lifted.FormatError as e:
        res.findings.append("%s: export does not parse: %s" % (path, e))
        return
    sites = parse_sites(text)
    res.programs += 1
    m = interp.Machine(prog)
    for fid, kind, order in prog.roots:
        ins = check.inputs(m, [t for _, t in prog.fns[fid][0]])
        if ins is None:
            res.skipped["root parameter type"] = res.skipped.get("root parameter type", 0) + 1
            continue
        for args in ins:
            res.runs += 1
            m.fuel = 2_000_000
            m.sites = {}
            m.violations = []
            o = m.run_root(fid, list(args))
            where = "%s root fn %d (%s)" % (os.path.basename(path), fid, kind)
            for v in m.violations:
                res.findings.append("L2 %s: %s" % (where, v))
            if o.kind == "violation" and not m.violations:
                res.findings.append("S3 %s: %s" % (where, o.detail))
            if o.kind == "limit":
                res.skipped[o.detail] = res.skipped.get(o.detail, 0) + 1
            for call, fns in m.sites.items():
                callee = prog.exprs[call][2][0]
                if callee not in sites:
                    res.unlowered += 1
                    continue
                allowed = sites[callee]
                if allowed is None:
                    res.erased += 1
                    continue
                res.calls_checked += 1
                missing = sorted(fns - allowed)
                if missing:
                    res.findings.append("L1 %s: value call e%d (callee e%d) reached fn %s, lowered lambda set %s" % (
                        where, call, callee, ", ".join(map(str, missing)), sorted(allowed)))


def check_dir(d):
    res = Result()
    for name in sorted(os.listdir(d)):
        if FILE.match(name):
            check_file(os.path.join(d, name), res)
    return res


def summary(res):
    top = sorted(res.skipped.items(), key=lambda kv: -kv[1])[:3]
    return "lambda-set programs %d, runs %d, value calls checked %d, erased %d, not lowered %d, skipped %d%s, findings %d" % (
        res.programs, res.runs, res.calls_checked, res.erased, res.unlowered, sum(res.skipped.values()),
        (" (" + "; ".join("%s: %d" % kv for kv in top) + ")") if top else "", len(res.findings))


def main(argv):
    res = check_dir(argv[1])
    for f in dict.fromkeys(res.findings):
        print(f)
    print(summary(res))
    return 1 if res.findings else 0


if __name__ == "__main__":
    sys.exit(check.big_stack(main, sys.argv))
