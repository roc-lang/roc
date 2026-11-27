"""Run the compiler on one program in every mode and judge the results.

Every command must finish normally (no panic, signal or hang) and every command must agree on whether the program has
errors. A built executable must not die of a signal when run: an erroneous program crashes with a Roc runtime error
at the rejected code, never a segfault.
"""
import os, re, shutil, subprocess

ROC = os.environ.get("ROC", "roc")

# name, argv after `roc`; the file name is appended
COMMANDS = [
    ("check", ["check"]),
    ("test-dev", ["test", "--opt=dev"]),
    ("test-dev-spec", ["test", "--opt=dev", "--specialize=yes"]),
    ("test-speed", ["test", "--opt=speed"]),
    ("build-dev", ["build", "--opt=dev"]),
    ("build-dev-spec", ["build", "--opt=dev", "--specialize=yes"]),
    ("build-speed", ["build", "--opt=speed"]),
]
COMMAND_NAMES = [c for c, _ in COMMANDS]

PANIC = re.compile(r"panic|Segmentation fault|reached unreachable|Illegal instruction|Bus error|invariant violated|"
                   r"internal compiler error|Abort trap|stack overflow in the Roc compiler", re.I)
SUMMARY = re.compile(r"(\d+) errors? and (\d+) warnings?")
TEST_FAILED = re.compile(r"Compilation failed with (\d+) errors?")
HEADER = re.compile(r"── ✗ ([^─\n]+?) ─")
FIRST_PANIC_LINE = re.compile(r"^.*(panic|invariant|Segmentation|unreachable).*$", re.M | re.I)


# Panic messages of open issues, so a fuzz run can tell new failures from known ones. Remove an entry when its issue
# is fixed.
KNOWN = [
    ("a hosted procedure has no host binding", "issue:13359"),
]


def known(results):
    tags = set()
    for r in results:
        for text, tag in KNOWN:
            if text in r.out:
                tags.add(tag)
    return sorted(tags)


class Result:
    def __init__(self, cmd, status, errors, out, rc):
        self.cmd, self.status, self.errors, self.out, self.rc = cmd, status, errors, out, rc

    def short(self):
        if self.status == "ok":
            return "%d" % self.errors
        return self.status

    def panic_line(self):
        m = FIRST_PANIC_LINE.search(self.out)
        return m.group(0).strip()[:200] if m else ""


def classify(cmd, rc, out):
    """status is ok, panic, signal(N), timeout or unknown; errors is the reported error count."""
    if rc is None:
        return "timeout", -1
    if PANIC.search(out):
        return "panic", -1
    if rc < 0 or rc >= 128:
        return "signal(%d)" % (-rc if rc < 0 else rc - 128), -1
    if cmd.startswith("test"):
        m = TEST_FAILED.search(out)
        if m:
            return "ok", int(m.group(1))
        if re.search(r"tests? passed|passed|failed", out):
            return "ok", 0
        return "unknown", -1
    if cmd == "check" and "No errors found" in out:
        return "ok", 0
    m = SUMMARY.search(out)
    if m:
        return "ok", int(m.group(1))
    return "unknown", -1


def run_cmd(d, filename, cmd, argv, timeout):
    args = [ROC] + argv + ["--no-cache"]
    if argv[0] == "build":
        args.append("--output=%s" % os.path.join(d, cmd + ".bin"))
    args.append(filename)
    try:
        p = subprocess.run(args, cwd=d, capture_output=True, text=True, timeout=timeout, errors="replace")
        rc, out = p.returncode, p.stdout + p.stderr
    except subprocess.TimeoutExpired as e:
        rc, out = None, ""
    status, errors = classify(cmd, rc, out)
    return Result(cmd, status, errors, out, rc)


def run_binary(d, cmd, timeout=20):
    exe = os.path.join(d, cmd + ".bin")
    if not os.path.exists(exe):
        return None
    try:
        p = subprocess.run([exe], cwd=d, capture_output=True, text=True, timeout=timeout, errors="replace")
    except subprocess.TimeoutExpired:
        return Result("run-" + cmd, "timeout", -1, "", None)
    out = p.stdout + p.stderr
    if p.returncode < 0 or p.returncode >= 128:
        return Result("run-" + cmd, "signal(%d)" % (-p.returncode if p.returncode < 0 else p.returncode - 128), -1,
                      out, p.returncode)
    return Result("run-" + cmd, "ok", 0, out, p.returncode)


def judge(src, d, commands=None, timeout=180, run=True, filename="H.roc"):
    """Run `src` in every mode. Returns (verdict, results); verdict is a list of failure labels, empty when all is
    well. Labels: panic:<cmd>, signal:<cmd>, timeout:<cmd>, unknown:<cmd>, disagree, run-signal:<cmd>."""
    os.makedirs(d, exist_ok=True)
    with open(os.path.join(d, filename), "w") as f:
        f.write(src)
    results = []
    for cmd, argv in COMMANDS:
        if commands and cmd not in commands:
            continue
        results.append(run_cmd(d, filename, cmd, argv, timeout))
    labels = []
    for r in results:
        if r.status != "ok":
            labels.append("%s:%s" % (r.status.split("(")[0], r.cmd))
    verdicts = {r.cmd: r.errors > 0 for r in results if r.status == "ok"}
    if len(set(verdicts.values())) > 1:
        labels.append("disagree")
    if run:
        for r in results:
            if r.cmd.startswith("build") and r.status == "ok":
                rr = run_binary(d, r.cmd)
                if rr is not None:
                    results.append(rr)
                    if rr.status != "ok":
                        labels.append("run-%s:%s" % (rr.status.split("(")[0], r.cmd))
    return labels, results


def summary(results):
    return " ".join("%s=%s" % (r.cmd, r.short()) for r in results if not r.cmd.startswith("run-"))


def clean_binaries(d):
    for f in os.listdir(d):
        if f.endswith(".bin") or f.endswith(".bin.dSYM"):
            p = os.path.join(d, f)
            shutil.rmtree(p) if os.path.isdir(p) else os.remove(p)
