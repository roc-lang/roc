"""Unit checks of the reader, the interpreter and both judgments on small hand-written exports.

Each fixture is a complete version 3 export. The checks state what the model must say about it:
the value it computes, and that a changed literal, a dropped capture or a missing lambda-set member
is reported.
"""
import os
import sys
import tempfile

import check
import interp
import lifted
import lsets

# fn 0 (the root): x = 40; (fn 1 capturing x)(2). fn 1: |y| y + x.
BASE = """roc-lifted-ir 3
stage {stage}
types 2
t 0 prim i64
t 1 func 1 0 0
locals 3
l 0 0 -
l 1 0 5
l 2 0 -
fns 2
f 0 0 0 0 0
e 0 0 block 1 0 3
s 0 let_ 0 1 0
p 0 0 bind 2
e 1 0 int_lit i{lit}
e 3 0 call_value 4 1 5
e 4 1 fn_ref 1 {caps}
e 6 0 local 2
e 5 0 int_lit i2
f 1 1 0 0 1 1 0 0 10
e 10 0 low_level num_plus 2 11 12
e 11 0 local 0
e 12 0 local 1
roots 1
r 0 test_expect 0
{extra}end
"""


def text(stage="after_lift", lit=40, caps="1 5 6", extra=""):
    return BASE.format(stage=stage, lit=lit, caps=caps, extra=extra)


def run(t):
    p = lifted.parse(t)
    return interp.Machine(p).run_root(0, [])


def pair_findings(a, b):
    with tempfile.TemporaryDirectory() as d:
        with open(os.path.join(d, "0-after_lift.lifted"), "w") as f:
            f.write(a)
        with open(os.path.join(d, "0-after_spec_constr.lifted"), "w") as f:
            f.write(b)
        return check.check_dir(d).findings


def lset_findings(t):
    with tempfile.TemporaryDirectory() as d:
        with open(os.path.join(d, "0-lambda_solved.lifted"), "w") as f:
            f.write(t)
        return lsets.check_dir(d)


def main():
    failures = []

    def expect(cond, what):
        print(("ok   " if cond else "FAIL ") + what)
        if not cond:
            failures.append(what)

    o = run(text())
    expect(o.kind == "value" and o.detail == "42", "a capturing closure computes 40 + 2 (got %s)" % o)
    expect(lifted.unescape("a%20b%25") == b"a b%", "names unescape")
    try:
        lifted.parse(text().replace("roc-lifted-ir 3", "roc-lifted-ir 2"))
        expect(False, "a version 2 export is rejected")
    except lifted.FormatError:
        expect(True, "a version 2 export is rejected")

    expect(pair_findings(text(), text("after_spec_constr")) == [], "S1: equal programs agree")
    f = pair_findings(text(), text("after_spec_constr", lit=41))
    expect(len(f) == 1 and f[0].startswith("S1"), "S1: a changed literal is a divergence")
    f = pair_findings(text(), text("after_spec_constr", caps="0"))
    expect(len(f) == 1 and f[0].startswith("S3 after_spec_constr"), "S3/L2: a dropped capture is reported")

    r = lset_findings(text("lambda_solved", extra="c 4 finite 1 1\n"))
    expect(r.findings == [] and r.calls_checked == 1, "L1: a call inside its lambda set passes")
    r = lset_findings(text("lambda_solved", extra="c 4 finite 1 0\n"))
    expect(len(r.findings) == 1 and r.findings[0].startswith("L1"), "L1: a member missing from the set is reported")
    r = lset_findings(text("lambda_solved", extra="c 4 erased\n"))
    expect(r.findings == [] and r.erased == 1, "L1: an erased call is not checked")
    r = lset_findings(text("lambda_solved"))
    expect(r.findings == [] and r.unlowered == 1, "L3: an unlowered call is counted")

    m = interp.Machine(lifted.parse(text()))
    expect(m.fit(2 ** 63, "i64", crash=False) == -2 ** 63, "wrapping arithmetic wraps")
    try:
        m.fit(2 ** 63, "i64", crash=True)
        expect(False, "overflow crashes")
    except interp.Crash:
        expect(True, "overflow crashes")

    print("selftest: %d failure(s)" % len(failures))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(check.big_stack(main))
