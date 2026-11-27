"""Command line for the infer model. Use run.sh, which sets ROC and OUT."""
import argparse, glob, os, random, re, sys
from lang import pp_module
from gen import Gen
from parse import parse_defs, split_file, HEADER, ParseError
from diff import compare, classify, model_module
from fuzz import fuzz, variants
from roc import OUT

HERE = os.path.dirname(os.path.abspath(__file__))
EXPECT = re.compile(r"^# expect: (agree|disagree (\S+))\s*$", re.M)


def check_sections(path):
    """Yields (label, rows, expectation) for each section of a .roc file."""
    text = open(path).read()
    header, sections = split_file(text)
    name = HEADER.match(header).group(1)
    stem = os.path.splitext(os.path.basename(path))[0]
    for label, src, first_line in sections:
        m = EXPECT.search(src)
        expect = None if not m else (m.group(2) or "agree")
        defs = parse_defs(src, first_line)
        lines = [l for l in src.splitlines() if not l.lstrip().startswith("#")]
        d = os.path.join(OUT, "check", stem + ("-" + re.sub(r"\W+", "_", label) if label else ""))
        rows, _ = compare(defs, header + "\n" + src, lines, d, name + ".roc")
        yield label, rows, expect


def show(rows):
    for kind, m, r, note in rows:
        label = classify((kind, m, r, note))
        mark = "  " if label is None else "!!"
        if kind == "verdict":
            print("  %s verdict  model=%s compiler=%s %s" % (mark, m, r, ("(" + note + ")") if note else ""))
        else:
            print("  %s %s\n       model:    %s\n       compiler: %s" % (mark, kind, m, r))
        if label:
            print("       -> %s" % label)


def cmd_check(files):
    bad = outside = 0
    for path in files:
        try:
            for label, rows, _ in check_sections(path):
                print("%s%s" % (path, (" [" + label + "]") if label else ""))
                show(rows)
                bad += sum(classify(r) is not None for r in rows)
        except ParseError as ex:
            print("%s: outside the fragment: %s" % (path, ex))
            outside += 1
    print("disagreements: %d, files outside the fragment: %d" % (bad, outside))
    return 1 if bad or outside else 0


def cmd_selftest():
    failures = 0
    # 1. the parser reads back what the printer writes, and the model never
    #    raises anything but a type error on generated modules
    #    or their annotated variants
    n = 0
    for seed in range(300):
        defs = Gen(seed).module()
        mv, c, types, _ = model_module(defs)
        mods = [("base", defs)]
        if mv == "accept":
            mods += variants(defs, c, types, random.Random(seed))
        for tag, m in mods:
            n += 1
            if tag != "base":
                model_module(m)
            if parse_defs(pp_module(m, header="")) != m:
                print("FAIL parse(pp(module)) differs for seed %d, %s" % (seed, tag))
                failures += 1
    print("ok   parser round trip and model on %d generated modules" % n)
    # 2. fixtures against the compiler
    for path in sorted(glob.glob(os.path.join(HERE, "tests", "*.roc"))):
        for label, rows, expect in check_sections(path):
            name = os.path.basename(path) + (" [%s]" % label if label else "")
            labels = [classify(r) for r in rows]
            got = [l for l in labels if l]
            expect = expect or "agree"
            if expect == "agree":
                ok = not got
            else:
                ok = expect in got and all(l == expect for l in got)
            print("%s %s: expect %s" % ("ok  " if ok else "FAIL", name, expect))
            if not ok:
                show(rows)
                if expect != "agree" and not got:
                    print("       the compiler now agrees with the model; if the issue is fixed, change this fixture to `# expect: agree`")
                failures += 1
    print("selftest: %d failure(s)" % failures)
    return 1 if failures else 0


def main():
    ap = argparse.ArgumentParser(prog="run.sh")
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("selftest")
    c = sub.add_parser("check")
    c.add_argument("files", nargs="+")
    f = sub.add_parser("fuzz")
    f.add_argument("n", nargs="?", type=int, default=100)
    f.add_argument("--seed", type=int, default=0)
    f.add_argument("-j", "--jobs", type=int, default=4)
    a = ap.parse_args()
    if a.cmd == "selftest":
        return cmd_selftest()
    if a.cmd == "check":
        return cmd_check(a.files)
    return 1 if fuzz(a.n, a.seed, a.jobs) else 0


if __name__ == "__main__":
    sys.exit(main())
