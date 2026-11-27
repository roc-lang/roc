"""Driver for the cache-incremental model. Called by run.sh; see README.md.

  driver.py fuzz N [--seed S] [--steps K] [--configs a,b] [--jobs J] [--keep] [--no-verify]
  driver.py replay DIR [--configs a,b] [--states i,j,..]
  driver.py reduce DIR [--configs a,b]
  driver.py check DIR...
  driver.py selftest DIR...
  driver.py gen SEED [--steps K]

Environment: ROC (the compiler), OUT (output directory).

The judgment: after any sequence of edits, every command run with the warm cache behaves exactly like the same
command run on the same sources with an empty cache directory.
"""
import argparse
import os
import pickle
import random
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import edit  # noqa: E402
import gen  # noqa: E402

# name: the roc arguments before app.roc, and whether the command builds an executable that is then run.
CONFIGS = {
    "check": (["check"], False),
    "test": (["test"], False),
    "test-spec": (["test", "--opt=dev", "--specialize=yes"], False),
    "run": (["run"], False),
    "run-spec": (["run", "--specialize=yes"], False),
    "interp": (["run", "--opt=interpreter"], False),
    "dev": (["build", "--opt=dev"], True),
    "dev-spec": (["build", "--opt=dev", "--specialize=yes"], True),
    "speed": (["build", "--opt=speed"], True),
    "size": (["build", "--opt=size"], True),
    "size-boxy": (["build", "--opt=size", "--specialize=no"], True),
}
CHEAP = ["check", "test", "run", "dev", "dev-spec", "run-spec", "interp", "test-spec"]
LLVM = ["speed", "size", "size-boxy"]
TIMEOUT = 600


def roc():
    return os.environ["ROC"]


def out_dir():
    return os.environ.get("OUT", os.path.join(HERE, "out"))


# --------------------------------------------------------------------------------------------- running

def write_tree(d, files):
    """Make d hold exactly files (and nothing else but the build products the driver puts there)."""
    if os.path.isdir(d):
        shutil.rmtree(d)
    os.makedirs(d)
    for name, text in files.items():
        path = os.path.join(d, name)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(text)


def read_tree(d):
    out = {}
    for root, _, names in os.walk(d):
        for n in names:
            if n.endswith(".roc"):
                path = os.path.join(root, n)
                with open(path) as f:
                    out[os.path.relpath(path, d)] = f.read()
    return out


TIMING = re.compile(r"\b\d+(\.\d+)?\s*(ms|µs|us|s|ns|m|h)\b(\s+\d+(\.\d+)?\s*(ms|µs|us|s|ns))*")


def normalize(text, paths):
    """Drop what may differ between two runs of the same command on the same sources: timings and the private
    directories each run uses."""
    for p in paths:
        text = text.replace(p, "<dir>")
    text = TIMING.sub("<t>", text)
    return text.rstrip()


def env_for(cache, verify):
    env = dict(os.environ)
    env["ROC_CACHE_DIR"] = cache
    env["XDG_CACHE_HOME"] = os.path.join(cache, "xdg")
    env.pop("ROC_VERIFY_BOXY_OBJECT_REUSE", None)
    if verify:
        env["ROC_VERIFY_BOXY_OBJECT_REUSE"] = "1"
    return env


def run_config(ws, cache, name, verify=False):
    """What one command observably did: (command output, exit status, program stdout, program exit status)."""
    args, builds = CONFIGS[name]
    env = env_for(cache, verify)
    tmp = os.path.join(cache, "out")
    os.makedirs(tmp, exist_ok=True)
    exe = os.path.join(tmp, "app")
    cmd = [roc()] + args + ["--no-color"] if args[0] not in ("check",) else [roc()] + args
    if builds:
        if os.path.exists(exe):
            os.remove(exe)
        cmd += [f"--output={exe}"]
    cmd += ["app.roc"]
    try:
        p = subprocess.run(cmd, cwd=ws, env=env, capture_output=True, text=True, timeout=TIMEOUT,
                           stdin=subprocess.DEVNULL)
    except subprocess.TimeoutExpired:
        return ("timeout", "", "", "")
    paths = [exe, os.path.relpath(exe, ws), ws, cache, os.path.relpath(cache, ws)]
    res = [normalize(p.stdout + "\n--stderr--\n" + p.stderr, paths), str(p.returncode), "", ""]
    if builds and os.path.exists(exe):
        try:
            q = subprocess.run([exe], cwd=ws, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL)
            res[2] = normalize(q.stdout + "\n--stderr--\n" + q.stderr, paths)
            res[3] = str(q.returncode)
        except subprocess.TimeoutExpired:
            res[2], res[3] = "timeout", "timeout"
        os.remove(exe)
    return tuple(res)


def program_stdout(name, res):
    """The program's stdout, for comparison with the reference (None when the command did not run a program)."""
    args, builds = CONFIGS[name]
    if builds:
        return drop_newline(res[2].split("\n--stderr--")[0]) if res[3] == "0" else None
    if args[0] == "run":
        return drop_newline(res[0].split("\n--stderr--")[0]) if res[1] in ("0", "2") else None
    return None


def drop_newline(s):
    return s[:-1] if s.endswith("\n") else s


def describe(res):
    return f"command exit {res[1]}, program exit {res[3] or '-'}\n--- command output\n{res[0]}\n--- program output\n{res[2]}"


def first_diff(a, b):
    for k, (x, y) in enumerate(zip(a, b)):
        if x != y:
            xl, yl = x.splitlines(), y.splitlines()
            for i, (u, v) in enumerate(zip(xl, yl)):
                if u != v:
                    return f"{['command output', 'command exit', 'program output', 'program exit'][k]} line {i + 1}: warm {u!r} clean {v!r}"
            return f"{['command output', 'command exit', 'program output', 'program exit'][k]}: warm has {len(xl)} lines, clean {len(yl)}"
    return "no difference"


class Session:
    """One warm cache directory that a sequence of states is built in, beside an empty one per state."""

    def __init__(self, base, configs, verify):
        self.base = base
        self.ws = os.path.join(base, "ws")
        self.warm = os.path.join(base, "cache-warm")
        self.clean_root = os.path.join(base, "cache-clean")
        self.configs = configs
        self.verify = verify
        shutil.rmtree(self.warm, ignore_errors=True)
        os.makedirs(self.warm)

    def step(self, files, idx):
        """Write files, run every config warm and clean. {config: (warm, clean)}."""
        write_tree(self.ws, files)
        clean = os.path.join(self.clean_root, str(idx))
        shutil.rmtree(clean, ignore_errors=True)
        os.makedirs(clean)

        def warm_chain():
            return {c: run_config(self.ws, self.warm, c, self.verify) for c in self.configs}

        def clean_chain():
            return {c: run_config(self.ws, clean, c) for c in self.configs}

        with ThreadPoolExecutor(max_workers=2) as pool:
            w, c = pool.submit(warm_chain), pool.submit(clean_chain)
            warm, cl = w.result(), c.result()
        shutil.rmtree(clean, ignore_errors=True)
        return {k: (warm[k], cl[k]) for k in self.configs}

    def recheck_clean(self, files, idx, config):
        """Run config again on files with another empty cache, to tell a cache bug from nondeterminism."""
        clean = os.path.join(self.clean_root, f"{idx}-again")
        shutil.rmtree(clean, ignore_errors=True)
        os.makedirs(clean)
        r = run_config(self.ws, clean, config)
        shutil.rmtree(clean, ignore_errors=True)
        return r


# --------------------------------------------------------------------------------------------- fuzzing

def pick_configs(r, llvm=True):
    cs = ["check"] + r.sample([c for c in CHEAP if c != "check"], r.randint(2, 4))
    if llvm and r.random() < 0.5:
        cs.append(r.choice(LLVM))
    return cs


def history(seed, steps):
    """The states of one fuzz sequence and the edit that produced each: [(prog, description)]."""
    prog, _ = gen.generate(seed)
    out = [(prog, "initial program")]
    r = random.Random(seed * 7919 + 1)
    for k in range(steps):
        g = gen.Gen(seed * 100003 + k, counter_base=100000 * (k + 1))
        if len(out) > 2 and r.random() < 0.1:
            j = r.randrange(len(out) - 1)
            out.append((out[j][0], f"revert to state {j}"))
            continue
        if out[-1][0].error and r.random() < 0.4:
            res = edit.e_error(g, out[-1][0])  # fix the injected error, so most states run
        else:
            res = edit.random_edit(g, out[-1][0])
        if res is None:
            break
        out.append(res)
    return out


def save_sequence(base, states):
    os.makedirs(base, exist_ok=True)
    with open(os.path.join(base, "edits.txt"), "w") as f:
        for i, (_, desc) in enumerate(states):
            f.write(f"{i}\t{desc}\n")
    for i, (prog, _) in enumerate(states):
        d = os.path.join(base, "states", str(i))
        write_tree(d, gen.Emit(prog).files())
        with open(os.path.join(d, "prog.pickle"), "wb") as f:
            pickle.dump(prog, f)


def run_sequence(base, state_files, configs, verify, log, expected=None):
    """Build each state in turn with one warm cache. Returns None, or the first disagreement as
    (state index, config, warm, clean, kind) with kind 'cache' or 'flaky'. Also returns reference mismatches."""
    sess = Session(base, configs, verify)
    refs = []
    for i, files in enumerate(state_files):
        res = sess.step(files, i)
        for c in configs:
            warm, clean = res[c]
            if warm != clean:
                again = sess.recheck_clean(files, i, c)
                kind = "cache" if again == clean else "flaky"
                return (i, c, warm, clean, kind), refs
            if expected is not None and expected[i] is not None:
                out = program_stdout(c, clean)
                if out is not None and out != "\n".join(expected[i]):
                    refs.append((i, c))
        log(f"  state {i}: agree on {', '.join(configs)}")
    return None, refs


def fuzz_one(seed, args, root):
    r = random.Random(seed)
    configs = args.configs.split(",") if args.configs else pick_configs(r, not args.no_llvm)
    verify = not args.no_verify and r.random() < 0.5
    states = history(seed, args.steps)
    base = os.path.join(root, str(seed))
    shutil.rmtree(base, ignore_errors=True)
    save_sequence(base, states)
    expected = []
    for prog, _ in states:
        try:
            expected.append(None if prog.error else gen.evaluate(prog))
        except (gen.Overflow, RecursionError):
            expected.append(None)
    files = [gen.Emit(p).files() for p, _ in states]
    lines = []
    found, refs = run_sequence(base, files, configs, verify, lines.append, expected)
    shutil.rmtree(os.path.join(base, "ws"), ignore_errors=True)
    shutil.rmtree(os.path.join(base, "cache-warm"), ignore_errors=True)
    shutil.rmtree(os.path.join(base, "cache-clean"), ignore_errors=True)
    head = f"seed {seed} ({len(states)} states; {','.join(configs)}{'; verify' if verify else ''})"
    if found is None:
        if not args.keep and not refs:
            shutil.rmtree(base)
        msg = f"{head}: agree"
        if refs:
            msg += "; clean build differs from the reference at " + ", ".join(f"state {i} {c}" for i, c in refs)
        return "ok", msg
    i, c, warm, clean, kind = found
    with open(os.path.join(base, "verdict.txt"), "w") as f:
        f.write(f"{kind}: state {i} ({states[i][1]}), config {c}, verify={verify}\n{first_diff(warm, clean)}\n\n")
        f.write(f"=== warm\n{describe(warm)}\n\n=== clean\n{describe(clean)}\n")
    with open(os.path.join(base, "configs.txt"), "w") as f:
        f.write(",".join(configs) + ("\nverify\n" if verify else "\n"))
    return kind, f"{head}: {kind.upper()} at state {i} ({states[i][1]}) on {c}: {first_diff(warm, clean)}"


def fuzz(args):
    root = os.path.join(out_dir(), "fuzz")
    os.makedirs(root, exist_ok=True)
    counts = {}
    findings = []
    seeds = list(range(args.seed, args.seed + args.n))
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        for seed, (kind, msg) in zip(seeds, pool.map(lambda s: fuzz_one(s, args, root), seeds)):
            counts[kind] = counts.get(kind, 0) + 1
            print(msg, flush=True)
            if kind == "cache":
                findings.append(seed)
    print("summary: " + ", ".join(f"{k} {v}" for k, v in sorted(counts.items())))
    if findings:
        print("warm/clean disagreements at seeds: " + " ".join(map(str, findings)) + f" (in {root})")
    return 1 if findings else 0


# --------------------------------------------------------------------------------------------- replay and reduce

def load_states(d):
    sd = os.path.join(d, "states")
    idx = sorted(int(x) for x in os.listdir(sd))
    return [read_tree(os.path.join(sd, str(i))) for i in idx]


def dir_configs(d, args):
    if args.configs:
        return args.configs.split(","), False
    with open(os.path.join(d, "configs.txt")) as f:
        lines = f.read().split("\n")
    return lines[0].split(","), "verify" in lines[1:]


def replay(args):
    d = os.path.abspath(args.dir)
    configs, verify = dir_configs(d, args)
    states = load_states(d)
    if args.states:
        states = [states[int(i)] for i in args.states.split(",")]
    base = os.path.join(out_dir(), "replay")
    found, _ = run_sequence(base, states, configs, verify, print)
    if found is None:
        print("warm and clean agree at every state")
        return 0
    i, c, warm, clean, kind = found
    print(f"{kind}: state {i} on {c}: {first_diff(warm, clean)}")
    print(f"=== warm\n{describe(warm)}\n=== clean\n{describe(clean)}")
    return 1


def reduce(args):
    """Shrink a failing sequence: keep the failing state last, drop earlier states, then drop configs."""
    d = os.path.abspath(args.dir)
    configs, verify = dir_configs(d, args)
    states = load_states(d)
    base = os.path.join(out_dir(), "reduce")

    def fails(seq, cs):
        found, _ = run_sequence(base, [states[i] for i in seq], cs, verify, lambda _: None)
        return found is not None and found[4] == "cache" and found[0] == len(seq) - 1

    with open(os.path.join(d, "verdict.txt")) as f:
        last = int(re.match(r"\w+: state (\d+)", f.read()).group(1))
    seq = list(range(last + 1))
    if not fails(seq, configs):
        print("the sequence no longer fails")
        return 0
    # Configs first: a single config usually suffices, and each check is then cheaper.
    for c in list(configs):
        if len(configs) > 1 and fails(seq, [x for x in configs if x != c]):
            configs = [x for x in configs if x != c]
            print(f"  configs: {','.join(configs)}")
    # A pair (state i, then the failing state) is the common shape of a stale reuse.
    for i in reversed(seq[:-1]):
        if fails([i, last], configs):
            seq = [i, last]
            print(f"  states: {seq}")
            break
    changed = True
    while changed and len(seq) > 1:
        changed = False
        for k in range(len(seq) - 1):
            cand = seq[:k] + seq[k + 1:]
            if fails(cand, configs):
                seq = cand
                changed = True
                print(f"  states: {seq}")
                break
    out = os.path.join(d, "min")
    shutil.rmtree(out, ignore_errors=True)
    for k, i in enumerate(seq):
        write_tree(os.path.join(out, "states", str(k)), states[i])
    with open(os.path.join(out, "configs.txt"), "w") as f:
        f.write(",".join(configs) + ("\nverify\n" if verify else "\n"))
    with open(os.path.join(out, "edits.txt"), "w") as f:
        f.write("".join(f"{k}\toriginal state {i}\n" for k, i in enumerate(seq)))
    print(f"reduced to states {seq} on {','.join(configs)}: {out} (replay it with ./run.sh replay {out})")
    return 1


def check(args):
    """Replay each fixture directory (states/0, states/1, ... and configs.txt); each must agree."""
    bad = []
    for d in args.dirs:
        d = os.path.abspath(d)
        configs, verify = dir_configs(d, argparse.Namespace(configs=args.configs))
        found, _ = run_sequence(os.path.join(out_dir(), "check"), load_states(d), configs, verify, lambda _: None)
        name = os.path.basename(d.rstrip("/"))
        if found is None:
            print(f"{name}: agree")
        else:
            i, c, warm, clean, kind = found
            print(f"{name}: {kind.upper()} at state {i} on {c}: {first_diff(warm, clean)}")
            bad.append(d)
    return 1 if bad else 0


def selftest(args):
    """1. The generator, edits and reference alone: 200 sequences of 12 edits, every state emits and evaluates, and
       every kind of edit applies somewhere.
    2. The fixtures in tests/ (each a sequence of states that must agree, or, with known.txt, must disagree).
    3. A two-sequence fuzz batch on cheap configs, whose states must all check without errors unless an edit
       injected one."""
    print("== generator and edits")
    kinds = {}
    for seed in range(1, 201):
        for prog, desc in history(seed, 12):
            gen.Emit(prog).files()
            if desc != "initial program":
                k = desc.split()[0]
                kinds[k] = kinds.get(k, 0) + 1
            if not prog.error:
                gen.evaluate(prog)
    print("edits applied: " + ", ".join(f"{k} {v}" for k, v in sorted(kinds.items())))
    failed = []
    print("== fixtures")
    for d in args.dirs:
        d = os.path.abspath(d)
        configs, verify = dir_configs(d, argparse.Namespace(configs=None))
        found, _ = run_sequence(os.path.join(out_dir(), "selftest"), load_states(d), configs, verify, lambda _: None)
        known = os.path.exists(os.path.join(d, "known.txt"))
        name = os.path.basename(d.rstrip("/"))
        if (found is None) == (not known):
            print(f"{name}: {'agree' if found is None else 'disagrees, as known.txt says'}")
        else:
            print(f"{name}: {'agrees, but known.txt says it should not: fixed?' if found is None else 'DISAGREES'}")
            failed.append(name)
    print("== generated states check cleanly")
    for seed in (1, 2):
        for i, (prog, desc) in enumerate(history(seed, 4)):
            ws = os.path.join(out_dir(), "selftest", "ws")
            write_tree(ws, gen.Emit(prog).files())
            cache = os.path.join(out_dir(), "selftest", "cache")
            os.makedirs(cache, exist_ok=True)
            res = run_config(ws, cache, "check")
            errs = re.search(r"(\d+) errors?", res[0])
            n = int(errs.group(1)) if errs else 0
            if (n > 0) != bool(prog.error):
                print(f"seed {seed} state {i} ({desc}): {n} errors\n{res[0][:1500]}")
                failed.append(f"seed {seed} state {i}")
    shutil.rmtree(os.path.join(out_dir(), "selftest"), ignore_errors=True)
    if failed:
        print("selftest FAILED: " + ", ".join(failed))
        return 1
    return 0


def show(args):
    states = history(args.seed, args.steps)
    for i, (prog, desc) in enumerate(states):
        print(f"######## state {i}: {desc}")
        if i == 0 or args.all:
            for name, text in gen.Emit(prog).files().items():
                print(f"==> {name}\n{text}")
            if not prog.error:
                print("==> expected\n" + "\n".join(gen.evaluate(prog)))
    return 0


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("fuzz")
    f.add_argument("n", type=int, nargs="?", default=10)
    f.add_argument("--seed", type=int, default=1)
    f.add_argument("--steps", type=int, default=12)
    f.add_argument("--configs")
    f.add_argument("--jobs", type=int, default=int(os.environ.get("JOBS", "2")))
    f.add_argument("--keep", action="store_true")
    f.add_argument("--no-verify", action="store_true")
    f.add_argument("--no-llvm", action="store_true")
    rp = sub.add_parser("replay")
    rp.add_argument("dir")
    rp.add_argument("--configs")
    rp.add_argument("--states")
    rd = sub.add_parser("reduce")
    rd.add_argument("dir")
    rd.add_argument("--configs")
    c = sub.add_parser("check")
    c.add_argument("dirs", nargs="+")
    c.add_argument("--configs")
    st = sub.add_parser("selftest")
    st.add_argument("dirs", nargs="*")
    g = sub.add_parser("gen")
    g.add_argument("seed", type=int)
    g.add_argument("--steps", type=int, default=12)
    g.add_argument("--all", action="store_true")
    a = ap.parse_args()
    sys.setrecursionlimit(10000)
    return {"fuzz": fuzz, "replay": replay, "reduce": reduce, "check": check, "selftest": selftest,
            "gen": show}[a.cmd](a)


if __name__ == "__main__":
    sys.exit(main())
