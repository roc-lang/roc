"""Driver for the SpecConstr model. Called by run.sh; see README.md.

  driver.py maude-tests FILE...                     run Maude suites; every reduce must give true,
                                                    every search must find no solution
  driver.py check FILE.roc...                       roc test at --opt=dev and --opt=speed, compare
  driver.py fuzz N [--seed S] [--family F] [--no-roc]
                                                    generate N programs (F: model, wide or both)

Environment: ROC (the compiler), MAUDE (default `maude`), OUT (output directory).
"""
import argparse
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from gen import MAUDE_TYPES, ROC_PRELUDE, ModelGen, WideGen  # noqa: E402

PASS = os.path.join(HERE, "maude", "pass.maude")
INPUTS = range(5)
FUEL = 60
OPTS = ("dev", "speed")


def roc_bin():
    return os.environ["ROC"]


def out_dir():
    return os.environ.get("OUT", os.path.join(HERE, "out"))


# ---------------------------------------------------------------------- Maude

def maude(path, timeout=3600):
    p = subprocess.run([os.environ.get("MAUDE", "maude"), "-no-banner", "-no-advise", "-batch", path],
                       capture_output=True, text=True, timeout=timeout, stdin=subprocess.DEVNULL)
    return p.stdout + p.stderr


def maude_tests(files):
    """Each command in a suite is a reduce that must be `true` or a search that must find nothing."""
    failed = 0
    for path in files:
        text = maude(path, timeout=600)
        warnings = [l for l in text.splitlines() if l.startswith("Warning")]
        blocks = re.split(r"\n(?=reduce in |search (?:\[\d+\] )?in )", text)
        cmds = [b for b in blocks if b.startswith(("reduce in ", "search "))]
        bad = []
        for b in cmds:
            if b.startswith("reduce"):
                ok = re.search(r"^result Bool: true$", b, re.M) is not None
            else:
                ok = re.search(r"^No solution\.$", b, re.M) is not None
            if not ok:
                bad.append(" ".join(b.split())[:300])
        status = "ok" if not bad and not warnings and cmds else "FAIL"
        print(f"{status:4} {os.path.basename(path)}: {len(cmds) - len(bad)}/{len(cmds)} hold")
        for w in warnings[:5]:
            print("     ", w)
        for b in bad:
            print("      failed:", b)
        failed += status != "ok"
    return failed


def maude_batch(progs, path):
    """progs: (name, maude term). Returns, per program, the oracle per input and the check report."""
    lines = [f"load {PASS}", "fmod BATCH is protecting SC-DRIVER ."]
    for i, (_, mau) in enumerate(progs):
        lines += [f"op P{i} : -> Prog .", f"eq P{i} = {MAUDE_TYPES} {mau} ."]
    lines.append("endfm")
    for i, (name, _) in enumerate(progs):
        lines += [f"red run(P{i}, '{name}, vi({k}), {FUEL}) ." for k in INPUTS]
        lines.append(f"red check(P{i}, noF, '{name}, ints({len(INPUTS)}), {FUEL}) .")
    with open(path, "w") as f:
        f.write("\n".join(lines) + "\n")
    text = maude(path)
    warnings = [l for l in text.splitlines() if l.startswith("Warning")]
    if warnings:
        sys.exit(f"maude: {len(warnings)} warnings in {path}, first: {warnings[0]}")
    results = []
    for chunk in text.split("reduce in BATCH :")[1:]:
        m = re.search(r"result [^:]+:\s*(.*?)(?:\nBye\.|\n=====|\Z)", chunk, re.S)
        results.append(" ".join(m.group(1).split()) if m else "?")
    per = len(INPUTS) + 1
    out = []
    for i in range(len(progs)):
        rs = results[i * per:(i + 1) * per]
        vals = []
        for r in rs[:-1]:
            m = re.fullmatch(r"ok\(vi\((-?\d+)\)\)", r)
            vals.append(m.group(1) if m else None)
        out.append((vals, rs[-1] if rs else "?"))
    return out


def report_passes(rep):
    return re.fullmatch(r"report\(true, tsnil, agree, \d+, \d+\)", rep) is not None


# ---------------------------------------------------------------------- roc

def roc_test(path, opt, timeout=600):
    try:
        p = subprocess.run([roc_bin(), "test", f"--opt={opt}", path], capture_output=True, text=True,
                           timeout=timeout, stdin=subprocess.DEVNULL)
        return p.returncode, p.stdout + p.stderr
    except subprocess.TimeoutExpired:
        return "timeout", ""


def summary(text):
    """(passed, failed, compiler errors), or None when roc printed no summary."""
    m = re.search(r"(\d+) passed\s+(\d+) failed\s+(\d+) compiler errors", text)
    if m:
        return tuple(int(x) for x in m.groups())
    m = re.search(r"All \((\d+)\) tests passed", text)
    if m:
        return (int(m.group(1)), 0, 0)
    return None


def failing_lines(text):
    return sorted({int(m.group(1)) for m in re.finditer(r"fail[^\n]*?\.roc:(\d+):\d+", text)})


def verdict(rc, text):
    """'ok', 'expect failed [lines]', or 'unhealthy <why>'."""
    s = summary(text)
    if rc == "timeout":
        return "unhealthy timeout"
    if "panic" in text:
        return "unhealthy panic"
    if s is None or s[2] != 0:
        return f"unhealthy rc={rc} summary={s}"
    if s[1]:
        return f"expect failed {failing_lines(text)}"
    return "ok"


def where(per):
    bad = [o for o in OPTS if per[o] != "ok"]
    if not bad:
        return "ok"
    if len(bad) == 2:
        return "both"
    return f"{bad[0]}-only"


def check(files):
    """Run each file's expects at dev and at speed. SpecConstr runs only at speed."""
    os.makedirs(out_dir(), exist_ok=True)
    failed = 0
    for path in files:
        per = {}
        for opt in OPTS:
            rc, text = roc_test(path, opt)
            per[opt] = verdict(rc, text)
            log = os.path.join(out_dir(), f"{os.path.basename(path)}.{opt}.log")
            with open(log, "w") as f:
                f.write(text)
        w = where(per)
        print(f"{'ok' if w == 'ok' else 'FAIL':4} {path}: dev: {per['dev']}; speed: {per['speed']}"
              + ("" if w == "ok" else f" ({w})"))
        failed += w != "ok"
    return failed


# ---------------------------------------------------------------------- fuzz

def write_roc(progs, path, only=None):
    """progs: (name, roc text, oracle per input). Returns {line: (program index, input)}."""
    out = [ROC_PRELUDE]
    for i, (name, src, oracle) in enumerate(progs):
        if only is not None and i != only:
            continue
        out.append(src)
        out += [f"expect {name}({k}) == {v}" for k, v in zip(INPUTS, oracle) if v is not None]
    text = "\n".join(out) + "\n"
    with open(path, "w") as f:
        f.write(text)
    index = {name: i for i, (name, _, _) in enumerate(progs)}
    at = {}
    for ln, line in enumerate(text.splitlines(), 1):
        m = re.match(r"expect (\w+_main)\((\d+)\) ==", line)
        if m:
            at[ln] = (index[m.group(1)], int(m.group(2)))
    return at


def fuzz(count, seed0, family, use_roc):
    outdir = os.path.join(out_dir(), f"fuzz-{family}-{seed0}")
    os.makedirs(outdir, exist_ok=True)
    model, wide = [], []
    for i in range(count):
        seed = seed0 + i
        fam = family if family != "both" else ("model" if i % 2 == 0 else "wide")
        if fam == "model":
            name, roc, mau = ModelGen(seed, f"m{seed}_").program()
            model.append((name, roc, mau))
        else:
            g = WideGen(seed, f"w{seed}_")
            name, roc = g.program()
            oracle = [g.oracle(name, k) for k in INPUTS]
            if any(v is not None for v in oracle):
                wide.append((name, roc, oracle))

    findings = []
    progs = []
    if model:
        results = maude_batch([(n, m) for n, _, m in model], os.path.join(outdir, "batch.maude"))
        for (name, roc, _), (vals, rep) in zip(model, results):
            if not report_passes(rep):
                findings.append(f"MODEL {name}: SpecConstr model check failed: {rep[:400]}")
            if any(v is not None for v in vals):
                progs.append((name, roc, vals))
    progs += wide
    print(f"generated {len(model)} model and {len(wide)} wide programs (seeds {seed0}..{seed0 + count - 1}); "
          f"{len(findings)} model-check failures")

    if use_roc and progs:
        path = os.path.join(outdir, "batch.roc")
        write_roc(progs, path)
        per = {}
        for opt in OPTS:
            rc, text = roc_test(path, opt)
            per[opt] = verdict(rc, text)
            with open(os.path.join(outdir, f"batch.{opt}.log"), "w") as f:
                f.write(text)
        if where(per) != "ok":
            # attribute: rerun every program alone
            for i, (name, _, _) in enumerate(progs):
                p1 = os.path.join(outdir, f"{name}.roc")
                write_roc(progs, p1, only=i)
                one = {}
                for opt in OPTS:
                    rc, text = roc_test(p1, opt)
                    one[opt] = verdict(rc, text)
                    if one[opt] != "ok":
                        with open(os.path.join(outdir, f"{name}.{opt}.log"), "w") as f:
                            f.write(text)
                w = where(one)
                if w != "ok":
                    findings.append(f"ROC {name} ({w}): dev: {one['dev']}; speed: {one['speed']}; {p1}")
                else:
                    os.remove(p1)
        print(f"roc test: dev: {per['dev']}; speed: {per['speed']}")
    for f in findings:
        print("  " + f)
    print(f"{len(findings)} findings; output in {outdir}")
    return len(findings)


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    a = sub.add_parser("maude-tests")
    a.add_argument("files", nargs="+")
    a = sub.add_parser("check")
    a.add_argument("files", nargs="+")
    a = sub.add_parser("fuzz")
    a.add_argument("n", type=int, nargs="?", default=40)
    a.add_argument("--seed", type=int, default=1)
    a.add_argument("--family", choices=["model", "wide", "both"], default="both")
    a.add_argument("--no-roc", action="store_true", help="only generate and run the Maude model check")
    args = ap.parse_args()
    if args.cmd == "maude-tests":
        bad = maude_tests(args.files)
    elif args.cmd == "check":
        bad = check(args.files)
    else:
        bad = fuzz(args.n, args.seed, args.family, not args.no_roc)
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
