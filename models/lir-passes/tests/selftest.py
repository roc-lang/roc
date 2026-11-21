"""Offline self-test of the model: no compiler needed.

usage: python3 -I tests/selftest.py

1. `str_match` follows the language reference: a capture stops at the first place the delimiter
   appears, multi-byte delimiters included (the shape of roc-lang/roc#13302).
2. `tce.dump` (a real dump of tce.roc: the lowered program, the trmc phase of both procedures, and
   the snapshot after the trmc segment) checks clean.
3. Two broken copies of that dump are rejected: one where TCE's rewritten loop subtracts instead
   of adding (a divergence), and one where TCE left its tail call in place (a structural finding).
"""
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
import check  # noqa: E402
import lir  # noqa: E402

failures = []


def expect(ok, what):
    print("%s %s" % ("ok  " if ok else "FAIL", what))
    if not ok:
        failures.append(what)


def str_match_cases():
    # "${x}--${y}": one arm, exact end, steps (capture x, delim "--"), (capture y, delim "")
    steps = [(1, b"--"), (2, b"")]
    expect(lir.str_match(b"a-b--c", b"", "tail", steps) == [(1, b"a-b"), (2, b"c")],
           "str_match: delimiter's first byte earlier in the text")
    expect(lir.str_match(b"ab--c", b"", "tail", steps) == [(1, b"ab"), (2, b"c")],
           "str_match: plain multi-byte delimiter")
    expect(lir.str_match(b"abc", b"", "tail", steps) is None, "str_match: missing delimiter misses")
    expect(lir.str_match(b"k=v", b"k=", "exact", []) is None, "str_match: exact end rejects a tail")


def phase_post(text, proc):
    """(start, end) offsets of the post section of the trmc phase block for `proc`."""
    head = text.index("@@LPD begin phase run=1 stage=1 label=trmc proc=%d\n" % proc)
    start = text.index("@@LPD post\n", head)
    end = text.index("@@LPD end\n", start)
    return start, end


def mutate(text, proc, old, new):
    start, end = phase_post(text, proc)
    post = text[start:end]
    assert post.count(old) == 1, old
    return text[:start] + post.replace(old, new) + text[end:]


def kinds(rep):
    return sorted({k for k, _, _ in rep.findings})


def fixture_cases():
    with open(os.path.join(HERE, "tce.dump")) as f:
        text = f.read()
    rep = check.check_text(text, nrand=6)
    expect(not rep.findings, "tce.dump checks clean (findings: %s)" % kinds(rep))
    expect(rep.stats.get("phase:trmc") == 2 and rep.stats.get("compared", 0) > 0,
           "tce.dump: both trmc phases compared (%s)" % rep.stats)

    # `sum` (proc 1): the loop TCE built adds h to acc; make it subtract.
    bad = mutate(text, 1, "num_int_add_crash_on_overflow", "num_int_sub_crash_on_overflow")
    rep = check.check_text(bad, nrand=6)
    expect("divergence" in kinds(rep), "wrong TCE loop body is a divergence (got %s)" % kinds(rep))

    # `sum` again: put the tail call back where TCE wrote the join-parameter update.
    bad = mutate(text, 1, "S 41 set 1 36 initialize_join_param 75\n",
                 "S 41 call 29 1 35,36 40 tail=1\nS 40 jump 2\n")
    rep = check.check_text(bad, nrand=6)
    expect("tail-site-left" in kinds(rep), "tail call left by trmc is reported (got %s)" % kinds(rep))


def main():
    str_match_cases()
    fixture_cases()
    if failures:
        print("selftest: %d failed" % len(failures))
        return 1
    print("selftest: model ok")
    return 0


if __name__ == "__main__":
    sys.exit(main())
