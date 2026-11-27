"""Compile Roc programs with the exports on and run both checks on what the compiler wrote.

  python3 drive.py check FILE.roc...        compile each file and check it
  python3 drive.py fuzz N [--seed S]        generate N programs and check them
  python3 drive.py repo [GLOB]              check the repository's own test programs

Each file is compiled once with `roc test --opt=speed --specialize=yes` (SpecConstr runs only at
size and speed; `OPT=dev` picks another level), and for an app (a file with an `app` header) with
`roc build` instead.
`ROC_LIFTED_IR_EXPORT` and `ROC_LAMBDA_SET_EXPORT` point at a fresh directory under `out/`.
A file the compiler rejects or crashes on is reported apart from model findings. Exits 1 when
anything is found.
"""
import glob
import re
import os
import shutil
import subprocess
import sys

import check
import gen
import lsets

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
ROC = os.environ.get("ROC", os.path.join(ROOT, "zig-out", "bin", "roc"))
OUT = os.path.join(HERE, "out")
TIMEOUT = int(os.environ.get("ROC_TIMEOUT", "300"))


OPT = os.environ.get("OPT", "speed")


def compile_file(path, work, opt=OPT):
    """Compile `path` with both exports on. Returns (status, compiler output tail)."""
    if os.path.isdir(work):
        shutil.rmtree(work)
    os.makedirs(work)
    env = dict(os.environ)
    env["ROC_LIFTED_IR_EXPORT"] = work
    env["ROC_LAMBDA_SET_EXPORT"] = work
    with open(path, encoding="utf-8", errors="replace") as f:
        head = f.read(4000)
    is_app = "app [" in head or head.lstrip().startswith("app")
    if is_app:
        cmd = [ROC, "build", "--no-cache", "--opt=" + opt, "--specialize=yes", "--output", os.path.join(work, "app.bin"), path]
    else:
        cmd = [ROC, "test", "--no-cache", "--opt=" + opt, "--specialize=yes", path]
    try:
        p = subprocess.run(cmd, env=env, capture_output=True, text=True, timeout=TIMEOUT, cwd=os.path.dirname(os.path.abspath(path)))
    except subprocess.TimeoutExpired:
        return "timeout", ""
    text = (p.stdout + p.stderr)
    with open(os.path.join(work, "compile.log"), "w") as f:
        f.write(" ".join(cmd) + "\n" + text)
    if p.returncode < 0 or "panic" in text or "invariant violated" in text:
        return "compiler-crash", text
    return "ok" if p.returncode == 0 else "exit %d" % p.returncode, text


def check_compiled(work):
    a = check.check_dir(work)
    b = lsets.check_dir(work)
    return a, b


E2E = [0]  # files whose compiled test output E1 compared


def compiled_tests(text):
    """The `[dbg]` texts `roc test` printed, and how many tests it says failed, or None if it
    printed no summary."""
    dbg = [l[len("[dbg] "):] for l in text.splitlines() if l.startswith("[dbg] ")]
    m = re.search(r"All \((\d+)\) tests passed", text)
    if m:
        return dbg, 0
    m = re.search(r"(\d+) failed and (\d+) passed", text) or re.search(r"(\d+) of \d+ tests? failed", text)
    if m:
        return dbg, int(m.group(1))
    return None


def end_to_end(path, text, a):
    """E1: what the compiled test program printed agrees with the interpreter's run of the
    after-SpecConstr test program."""
    got = compiled_tests(text)
    if got is None or a.test_programs != 1 or not a.test_complete:
        return []
    dbg, failed = got
    E2E[0] += 1
    out = []
    if sorted(dbg) != sorted(a.test_dbg):
        out.append("E1 compiled dbg output %r, interpreter %r" % (sorted(dbg), sorted(a.test_dbg)))
    if failed != a.test_failed:
        out.append("E1 compiled run failed %d tests, interpreter %d" % (failed, a.test_failed))
    return out


def run_file(path, work, report):
    status, text = compile_file(path, work)
    tail = text[-1500:]
    a, b = check_compiled(work)
    found = list(a.findings) + list(dict.fromkeys(b.findings))
    if status != "compiler-crash":
        found += end_to_end(path, text, a)
    if status == "compiler-crash":
        report.append("COMPILER %s: %s" % (path, tail.strip().splitlines()[-1] if tail.strip() else ""))
    for f in found:
        report.append("FINDING %s: %s" % (path, f))
    return status, a, b, found


def merge(total, a, b):
    for k in ("programs", "roots", "runs", "compared"):
        total[k] = total.get(k, 0) + getattr(a, k)
    total["lsets_calls"] = total.get("lsets_calls", 0) + b.calls_checked
    total["lsets_programs"] = total.get("lsets_programs", 0) + b.programs
    total["unlowered"] = total.get("unlowered", 0) + b.unlowered
    for k, v in list(a.skipped.items()) + list(b.skipped.items()):
        sk = total.setdefault("skipped", {})
        sk[k] = sk.get(k, 0) + v


def fmt_total(total, files, statuses):
    sk = total.get("skipped", {})
    top = sorted(sk.items(), key=lambda kv: -kv[1])[:5]
    st = ", ".join("%s %d" % kv for kv in sorted(statuses.items()))
    return ("files %d (%s); SpecConstr: programs %d, roots %d, runs %d, compared %d; lambda sets: programs %d, "
            "value calls checked %d, not lowered %d; end-to-end compared %d; skipped %d (%s)") % (
        files, st, total.get("programs", 0), total.get("roots", 0), total.get("runs", 0), total.get("compared", 0),
        total.get("lsets_programs", 0), total.get("lsets_calls", 0), total.get("unlowered", 0), E2E[0], sum(sk.values()),
        "; ".join("%s: %d" % kv for kv in top))


def run_many(paths, tag):
    report, total, statuses = [], {}, {}
    for i, path in enumerate(paths):
        work = os.path.join(OUT, tag, "%04d" % i)
        status, a, b, found = run_file(path, work, report)
        statuses[status] = statuses.get(status, 0) + 1
        merge(total, a, b)
        mark = "!" if found else "."
        print("%s %s %s (%d compared, %d calls)%s" % (mark, status, os.path.relpath(path, ROOT), a.compared,
                                                      b.calls_checked, "" if not found else " " + found[0].splitlines()[0][:160]),
              flush=True)
    print()
    for r in report:
        print(r)
    print(fmt_total(total, len(paths), statuses))
    return 1 if any(r.startswith("FINDING") for r in report) else 0


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    cmd = argv[1]
    os.makedirs(OUT, exist_ok=True)
    if cmd == "check":
        return run_many([os.path.abspath(p) for p in argv[2:]], "check")
    if cmd == "fuzz":
        n = int(argv[2]) if len(argv) > 2 else 20
        seed = int(argv[argv.index("--seed") + 1]) if "--seed" in argv else 1
        gdir = os.path.join(OUT, "fuzz-src")
        os.makedirs(gdir, exist_ok=True)
        paths = []
        for i in range(n):
            p = os.path.join(gdir, "g%d.roc" % (seed + i))
            with open(p, "w") as f:
                f.write(gen.program(seed + i))
            paths.append(p)
        return run_many(paths, "fuzz")
    if cmd == "repo":
        pat = argv[2] if len(argv) > 2 else "test/**/*.roc"
        paths = sorted(glob.glob(os.path.join(ROOT, pat), recursive=True))
        return run_many(paths, "repo")
    print(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(check.big_stack(main, sys.argv))
