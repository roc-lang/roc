"""Driver for the backend-diff model. Called by run.sh; see README.md.

  driver.py fuzz N [--seed S] [--configs a,b,..] [--no-reduce] [--keep]
  driver.py check DIR... [--configs ..]  DIR holds app.roc (headerless, native), its modules, optionally
                                        wasm/app.roc (test/wasm/platform app) and expected.txt
  driver.py selftest DIR...            generator check, the fixtures DIR..., a 3-program fuzz batch
  driver.py gen SEED                   print a generated program and its expected output
  driver.py reduce SEED                regenerate program SEED and reduce its disagreement

Environment: ROC (the compiler), OUT (output directory), NODE (default `node`).
"""
import argparse
import copy
import os
import re
import pickle
import shutil
from concurrent.futures import ThreadPoolExecutor
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import gen  # noqa: E402

# name: (target, mode, opt, specialize). mode "run" is `roc app.roc` (in process); "build" builds and runs the output.
CONFIGS = {
    "run-interp": ("native", "run", "interpreter", "no"),
    "run-interp-spec": ("native", "run", "interpreter", "yes"),
    "run-dev": ("native", "run", "dev", "no"),
    "dev": ("native", "build", "dev", "no"),
    "dev-spec": ("native", "build", "dev", "yes"),
    "speed": ("native", "build", "speed", "yes"),
    "size-boxy": ("native", "build", "size", "no"),
    "wasm-dev": ("wasm", "build", "dev", "no"),
    "wasm-dev-spec": ("wasm", "build", "dev", "yes"),
    "wasm-speed": ("wasm", "build", "speed", "yes"),
}
DEFAULT = list(CONFIGS)
TIMEOUT = 120
JOBS = int(os.environ.get("JOBS", "4"))


def roc():
    return os.environ["ROC"]


def out_dir():
    return os.environ.get("OUT", os.path.join(HERE, "out"))


def write_files(d, files, target):
    d = os.path.abspath(d)
    if os.path.isdir(d):
        shutil.rmtree(d)
    os.makedirs(d)
    for name, text in files.items():
        with open(os.path.join(d, name), "w") as f:
            f.write(text)
    if target == "wasm":
        os.symlink(os.path.join(HERE, "platform"), os.path.join(d, "platform"))


def run_config(d, name):
    """('ok', stdout) | ('build', message) | ('crash', message). d holds app.roc for the config's target."""
    target, mode, opt, spec = CONFIGS[name]
    base = [roc()]
    flags = [f"--opt={opt}", f"--specialize={spec}", "--no-cache"]
    try:
        if mode == "run":
            p = subprocess.run(base + flags + ["app.roc"], cwd=d, capture_output=True, text=True, timeout=TIMEOUT,
                               stdin=subprocess.DEVNULL)
            if p.returncode not in (0, 2) or "error" in tail(p.stderr).lower() and not p.stdout:
                return ("crash", f"rc={p.returncode} {tail(p.stderr)}")
            return ("ok", p.stdout)
        out = os.path.join(d, f"bin-{name}" + (".wasm" if target == "wasm" else ""))
        if os.path.exists(out):
            os.remove(out)
        cmd = base + ["build"] + flags + [f"--output={out}", "app.roc"]
        if target == "wasm":
            cmd.insert(2, "--target=wasm32")
        p = subprocess.run(cmd, cwd=d, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL)
        if not os.path.exists(out):
            return ("build", f"rc={p.returncode} {tail(p.stdout + p.stderr)}")
        run = [os.environ.get("NODE", "node"), os.path.join(HERE, "run_wasm.js"), out] if target == "wasm" else [out]
        q = subprocess.run(run, cwd=d, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL)
        os.remove(out)
        if q.returncode != 0:
            return ("crash", f"rc={q.returncode} {tail(q.stderr)}")
        return ("ok", q.stdout)
    except subprocess.TimeoutExpired:
        return ("crash", "timeout")


def tail(s, n=6):
    """The last n lines of s, without JavaScript stack frames, so a wasm trap shows its message."""
    lines = [l for l in s.strip().splitlines() if not l.lstrip().startswith("at ") and l.strip()]
    return "\n".join(lines[-n:])


def norm(s):
    return s.rstrip("\n")


def build_dirs(prog, base, collapse=False, targets=("native", "wasm")):
    em = gen.Emit(prog, collapse)
    dirs = {}
    for t in targets:
        d = os.path.join(base, t)
        write_files(d, em.files(t), t)
        dirs[t] = os.path.abspath(d)
    return dirs


def judge(prog, expected, base, configs, collapse=False):
    """{config: (verdict, detail)} with verdict ok, wrong, crash or build."""
    targets = sorted({CONFIGS[c][0] for c in configs})
    dirs = build_dirs(prog, base, collapse, targets)
    want = norm("\n".join(expected))
    res = {}
    with ThreadPoolExecutor(max_workers=JOBS) as pool:
        done = dict(zip(configs, pool.map(lambda c: run_config(dirs[CONFIGS[c][0]], c), configs)))
    for c in configs:
        kind, text = done[c]
        if kind == "ok":
            res[c] = ("ok", "") if norm(text) == want else ("wrong", norm(text))
        else:
            res[c] = (kind, text)
    return res


def bad(res):
    return sorted(c for c, (v, _) in res.items() if v != "ok")


# --------------------------------------------------------------------------------------------- reducer

def ty_of(e, tenv, by):
    k = e[0]
    if k == "lit":
        return e[1]
    if k == "ulit":
        return gen.U
    if k == "var":
        return tenv.get(e[1], gen.U)
    if k == "bin":
        if e[1] in "+-*":
            return ty_of(e[2], tenv, by)
        return gen.B
    if k in ("rem", "apply", "fold", "sum", "len", "slen"):
        return gen.I
    if k == "if":
        return ty_of(e[2], tenv, by)
    if k == "let":
        return ty_of(e[3], {**tenv, e[1]: ty_of(e[2], tenv, by)}, by)
    if k == "call":
        return by[e[1]].ret
    if k == "match":
        return ty_of(e[6], tenv, by)
    if k == "field":
        return gen.I if e[2] == "a" else gen.S
    return {"rec": gen.R, "tag": gen.T, "list": gen.L, "lam": gen.F, "fnref": gen.F, "map": gen.L, "keep": gen.L, "append": gen.L,
            "lconcat": gen.L, "concat": gen.S, "tostr": gen.S, "join": gen.S}[k]


def binders(e):
    k = e[0]
    return {"let": [e[1]], "match": [e[2], e[4]], "lam": [e[1]], "fold": [e[3], e[4]], "keep": [e[2]]}.get(k, [])


def free_ok(sub, banned):
    return not any(x[0] == "var" and x[1] in banned for x in gen.walk(sub))


SIMPLE = {gen.I: ("lit", gen.I, 0), gen.S: ("lit", gen.S, ""), gen.B: ("lit", gen.B, False), gen.L: ("list", []),
          gen.R: ("rec", ("lit", gen.I, 0), ("lit", gen.S, "")), gen.T: ("tag", "W", None)}


def replace_at(e, path, new):
    if not path:
        return new
    i = path[0]
    lst = list(e)
    if isinstance(i, tuple):
        inner = list(lst[i[0]])
        inner[i[1]] = replace_at(inner[i[1]], path[1:], new)
        lst[i[0]] = inner
    else:
        lst[i] = replace_at(lst[i], path[1:], new)
    return tuple(lst)


def node_paths(e, path=()):
    yield path, e
    for i, c in gen.children(e):
        yield from node_paths(c, path + (i,))


def get_at(e, path):
    for i in path:
        e = e[i[0]][i[1]] if isinstance(i, tuple) else e[i]
    return e


def expr_candidates(e, tenv, by):
    """Smaller same-typed replacements for each node of e, largest nodes first."""
    for path, node in node_paths(e):
        if node[0] in ("lit", "ulit", "var"):
            if node[0] == "ulit" and node[1] > 0:
                yield path, ("ulit", node[1] // 2)
            if node[0] == "lit" and node[1] == gen.I and node[2] not in (0, 1):
                yield path, ("lit", gen.I, 1)
            continue
        # The type at this point needs the binders above it; recompute along the path.
        env = dict(tenv)
        cur = e
        for i in path:
            if cur[0] == "let" and i == 3:
                env[cur[1]] = ty_of(cur[2], env, by)
            elif cur[0] == "match" and i == 3:
                env[cur[2]] = gen.I
            elif cur[0] == "match" and i == 5:
                env[cur[4]] = gen.S
            elif cur[0] == "lam" and i == 2:
                env[cur[1]] = gen.I
            elif cur[0] == "fold" and i == 5:
                env[cur[3]] = gen.I
                env[cur[4]] = gen.I
            elif cur[0] == "keep" and i == 3:
                env[cur[2]] = gen.I
            cur = cur[i[0]][i[1]] if isinstance(i, tuple) else cur[i]
        t = ty_of(node, env, by)
        if t in SIMPLE:
            yield path, SIMPLE[t]
        if t == gen.F and node != ("lam", "p0", ("var", "p0")):
            yield path, ("lam", "p0", ("var", "p0"))
        bs = binders(node)
        for i, c in gen.children(node):
            cenv = dict(env)
            if ty_of_safe(c, env, bs, by) == t and free_ok(c, bs):
                yield path, c


def ty_of_safe(c, env, bs, by):
    try:
        return ty_of(c, env, by)
    except KeyError:
        return None


def valid(prog):
    by = prog.by_name()
    for d in prog.defs:
        for x in gen.walk(d.body):
            if x[0] in ("call", "fnref"):
                c = by.get(x[1])
                if c is None or c.module > d.module or (c.module == d.module and d.group is None
                                                         and prog.defs.index(c) >= prog.defs.index(d)
                                                         and c.name != d.name):
                    return False
    for _, e in prog.outputs:
        if any(x[0] in ("call", "fnref") and x[1] not in by for x in gen.walk(e)):
            return False
    return True


def prune(prog):
    keep = gen.reachable(prog)
    prog.defs = [d for d in prog.defs if d.name in keep]
    for d in prog.defs:
        if d.group:
            d.group = [n for n in d.group if n in keep]
    return prog


def program_candidates(prog):
    by = prog.by_name()
    # Drop outputs.
    if len(prog.outputs) > 1:
        for i in range(len(prog.outputs)):
            p = copy.deepcopy(prog)
            del p.outputs[i]
            yield p
    # Fewer modules: move a def to another module.
    for di, d in enumerate(prog.defs):
        for m in range(prog.nmods):
            if m == d.module:
                continue
            p = copy.deepcopy(prog)
            members = [x for x in p.defs if x.group and d.group and x.name in d.group] or [p.defs[di]]
            for x in members:
                x.module = m
            yield p
    for m, f in enumerate(prog.forms):
        if f == "header":
            p = copy.deepcopy(prog)
            p.forms[m] = "type"
            yield p
    # Smaller expressions.
    for oi, (t, e) in enumerate(prog.outputs):
        for path, new in expr_candidates(e, {"z": gen.I, "zu": gen.U}, by):
            p = copy.deepcopy(prog)
            p.outputs[oi] = (t, replace_at(e, path, new))
            yield p
    for di, d in enumerate(prog.defs):
        tenv = {n: t for n, t in d.params}
        for path, new in expr_candidates(d.body, tenv, by):
            p = copy.deepcopy(prog)
            p.defs[di].body = replace_at(d.body, path, new)
            yield p


def reduce(prog, configs, control, base, keep_signature, log):
    """Greedy reduction keeping: reference evaluates, every config in `configs` still disagrees with it (the
    same way), and `control` still agrees."""
    prog = prune(copy.deepcopy(prog))
    steps = 0

    def holds(p):
        nonlocal steps
        if not valid(p):
            return False
        try:
            exp = gen.evaluate(p)
        except (gen.Overflow, RecursionError, KeyError, TypeError, IndexError):
            return False
        steps += 1
        res = judge(p, exp, base, configs + [control])
        return res[control][0] == "ok" and all(keep_signature(c, res[c]) for c in configs)

    changed = True
    while changed:
        changed = False
        for cand in program_candidates(prog):
            cand = prune(cand)
            if size(cand) >= size(prog):
                continue
            if holds(cand):
                prog = cand
                changed = True
                log(f"  reduced to size {size(prog)} ({steps} checks)")
                break
    return prog


def size(prog):
    return (sum(sum(1 for _ in gen.walk(d.body)) + len(d.params) for d in prog.defs)
            + sum(sum(1 for _ in gen.walk(e)) for _, e in prog.outputs) + 3 * prog_mods(prog))


def prog_mods(prog):
    return len({d.module for d in prog.defs} | {prog.nmods - 1})


# --------------------------------------------------------------------------------------------- commands

def classify(prog, expected, res, base):
    """'13356' when the only disagreement is wasm dev with boxy lowering, the program calls a tail group across
    modules, and routing each such call through a wrapper beside the group (so no importer calls a group member)
    removes it."""
    b = bad(res)
    if b and set(b) <= {"wasm-dev"} and gen.cross_module_group_calls(prog):
        wrapped = gen.wrap_cross_group_calls(prog)
        again = judge(wrapped, expected, os.path.join(base, "wrapped"), ["wasm-dev"])
        shutil.rmtree(os.path.join(base, "wrapped"), ignore_errors=True)
        if again["wasm-dev"][0] == "ok":
            return "13356"
    if all(v != "ok" for v, _ in res.values()):
        return "all-fail"
    return "new"


def check_errors(d):
    """The first error `roc check` reports for d/app.roc, or None when it has none."""
    p = subprocess.run([roc(), "check", "--no-cache", "app.roc"], cwd=d, capture_output=True, text=True,
                       timeout=TIMEOUT, stdin=subprocess.DEVNULL)
    text = p.stdout + p.stderr
    m = re.search(r"(\d+) errors? and", text)
    if m and m.group(1) != "0" or p.returncode not in (0, 2):
        blocks = [b for b in text.split("── ") if b.startswith("✗") or "error" in b.split("\n")[0].lower()]
        return (blocks[0] if blocks else text)[:800]
    return None


def fuzz(args):
    configs = args.configs.split(",") if args.configs else DEFAULT
    root = os.path.join(out_dir(), "fuzz-wrap" if args.wrap else "fuzz")
    os.makedirs(root, exist_ok=True)
    counts = {}
    findings = []
    for i in range(args.n):
        seed = args.seed + i
        prog, expected = gen.generate(seed)
        if args.wrap:
            prog = gen.wrap_cross_group_calls(prog)
        base = os.path.join(root, str(seed))
        dirs = build_dirs(prog, base, targets=("native",))
        err = check_errors(dirs["native"])
        if err:
            counts["invalid"] = counts.get("invalid", 0) + 1
            print(f"seed {seed}: invalid (the generator emitted Roc that does not check): {err!r}", flush=True)
            continue
        res = judge(prog, expected, base, configs)
        b = bad(res)
        if not b:
            counts["ok"] = counts.get("ok", 0) + 1
            if not args.keep:
                shutil.rmtree(base)
            print(f"seed {seed}: ok ({len(prog.defs)} defs, {prog_mods(prog)} modules)", flush=True)
            continue
        with open(os.path.join(base, "prog.pickle"), "wb") as f:
            pickle.dump(prog, f)  # `reduce` reads it back, so a finding outlives later generator changes
        cls = classify(prog, expected, res, base)
        counts[cls] = counts.get(cls, 0) + 1
        print(f"seed {seed}: {cls}: " + "; ".join(f"{c}={res[c][0]}" for c in b), flush=True)
        for c in b:
            print(f"    {c}: {res[c][1][:300]!r}", flush=True)
        with open(os.path.join(base, "verdict.txt"), "w") as f:
            f.write(f"class {cls}\nexpected:\n" + "\n".join(expected) + "\n")
            for c in configs:
                f.write(f"{c}: {res[c][0]} {res[c][1]}\n")
        if cls == "new":
            findings.append(seed)
            if not args.no_reduce:
                do_reduce(prog, expected, res, configs, base)
    print("summary: " + ", ".join(f"{k} {v}" for k, v in sorted(counts.items())))
    if findings:
        print("new disagreements at seeds: " + " ".join(map(str, findings)))
    return 1 if findings else 0


def do_reduce(prog, expected, res, configs, base):
    b = bad(res)
    control = next((c for c in configs if res[c][0] == "ok"), None)
    if control is None:
        print("  no agreeing config to keep as a control; not reducing")
        return
    sig = {c: res[c][0] for c in b}

    def keep(c, r):
        return r[0] == sig[c]

    small = reduce(prog, b, control, os.path.join(base, "reduce"), keep, print)
    exp = gen.evaluate(small)
    final = judge(small, exp, os.path.join(base, "min"), configs)
    cls = classify(small, exp, final, os.path.join(base, "min"))
    shutil.rmtree(os.path.join(base, "reduce"), ignore_errors=True)
    with open(os.path.join(base, "min", "verdict.txt"), "w") as f:
        f.write(f"class {cls}\nexpected:\n" + "\n".join(exp) + "\n")
        for c in configs:
            f.write(f"{c}: {final[c][0]} {final[c][1]}\n")
    print(f"  minimized ({cls}) in {os.path.join(base, 'min')}")


def check_dir(d, configs):
    """Run d/app.roc (native) and d/wasm/app.roc (wasm32, when present) on every config. True when they agree with
    d/expected.txt, or with each other when there is none."""
    d = os.path.abspath(d)
    wasm = os.path.join(d, "wasm")
    if os.path.isdir(wasm) and not os.path.exists(os.path.join(wasm, "platform")):
        os.symlink(os.path.join(HERE, "platform"), os.path.join(wasm, "platform"))
    outs = {}
    for c in configs:
        target = CONFIGS[c][0]
        if target == "wasm" and not os.path.isdir(wasm):
            continue
        outs[c] = run_config(wasm if target == "wasm" else d, c)
    exp_path = os.path.join(d, "expected.txt")
    want = None
    if os.path.exists(exp_path):
        with open(exp_path) as f:
            want = norm(f.read())
    results = {c: (k, norm(t) if k == "ok" else t) for c, (k, t) in outs.items()}
    if want is None:
        ok = len({r for r in results.values()}) == 1
    else:
        ok = all(k == "ok" and t == want for k, t in results.values())
    print(f"{os.path.basename(d.rstrip('/'))}: {'agree' if ok else 'DISAGREE'}")
    for c, (k, t) in results.items():
        if not ok or k != "ok":
            print(f"    {c}: {k} {t[:300]!r}")
    return ok, results


def check(args):
    configs = args.configs.split(",") if args.configs else DEFAULT
    bad_dirs = [d for d in args.dirs if not check_dir(d, configs)[0]]
    return 1 if bad_dirs else 0


def selftest(args):
    """1. The generator and reference alone: 300 programs, each evaluates and every recursive group is called.
    2. The fixtures in tests/: each must agree on every path, except the configs its known.txt lists as
       disagreeing because of an open issue, which must disagree (so the fixture notices the fix).
    3. A fuzz batch of 3 programs, which must produce no invalid program."""
    print("== generator and reference")
    groups = cross = 0
    for seed in range(1, 301):
        prog, _ = gen.generate(seed)
        assert gen.reachable(prog) == {d.name for d in prog.defs}, f"seed {seed}: unreachable defs"
        for files in (gen.Emit(prog).files("native"), gen.Emit(prog, collapse=True).files("wasm")):
            assert "app.roc" in files
        groups += sum(1 for d in prog.defs if d.kind == "group")
        cross += bool(gen.cross_module_group_calls(prog))
    print(f"300 programs, {groups} group members, {cross} with a cross-module tail-group call")
    assert cross > 50
    print("== fixtures")
    failed = []
    for d in args.dirs:
        known = {}
        kp = os.path.join(d, "known.txt")
        if os.path.exists(kp):
            with open(kp) as f:
                for line in f:
                    if line.strip() and not line.startswith("#"):
                        c, issue = line.split()
                        known[c] = issue
        ok, results = check_dir(d, DEFAULT)
        if ok:
            if known:
                print(f"    {', '.join(known)} now agree: has {', '.join(set(known.values()))} been fixed? "
                      f"update {kp}")
                failed.append(d)
            continue
        with open(os.path.join(d, "expected.txt")) as f:
            want = norm(f.read())
        wrong = {c for c, (k, t) in results.items() if not (k == "ok" and t == want)}
        if wrong == set(known):
            print(f"    only the known disagreements: " + ", ".join(f"{c} ({i})" for c, i in known.items()))
        else:
            failed.append(d)
    print("== fuzz")
    root = os.path.join(out_dir(), "selftest")
    for seed in range(1, 4):
        prog, expected = gen.generate(seed)
        base = os.path.join(root, str(seed))
        dirs = build_dirs(prog, base, targets=("native",))
        err = check_errors(dirs["native"])
        if err:
            print(f"seed {seed}: invalid {err!r}")
            failed.append(f"fuzz seed {seed}")
            continue
        res = judge(prog, expected, base, DEFAULT)
        b = bad(res)
        print(f"seed {seed}: " + ("agree" if not b else f"{classify(prog, expected, res, base)}: {', '.join(b)}"))
    shutil.rmtree(root, ignore_errors=True)
    if failed:
        print("selftest FAILED: " + ", ".join(failed))
        return 1
    return 0


def show(args):
    prog, expected = gen.generate(args.seed)
    for name, text in gen.Emit(prog).files(args.target).items():
        print(f"==> {name}\n{text}")
    print("==> expected\n" + "\n".join(expected))
    print("cross-module tail-group calls:", sorted(set(gen.cross_module_group_calls(prog))))
    return 0


def reduce_cmd(args):
    configs = args.configs.split(",") if args.configs else DEFAULT
    if os.path.exists(args.seed):
        with open(args.seed, "rb") as f:
            prog = pickle.load(f)
        expected = gen.evaluate(prog)
        base = os.path.dirname(os.path.abspath(args.seed))
    else:
        prog, expected = gen.generate(int(args.seed))
        base = os.path.join(out_dir(), "fuzz", str(args.seed))
    res = judge(prog, expected, base, configs)
    if not bad(res):
        print("no disagreement")
        return 0
    do_reduce(prog, expected, res, configs, base)
    return 1


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("fuzz")
    f.add_argument("n", type=int, nargs="?", default=20)
    f.add_argument("--seed", type=int, default=1)
    f.add_argument("--configs")
    f.add_argument("--no-reduce", action="store_true")
    f.add_argument("--keep", action="store_true")
    f.add_argument("--wrap", action="store_true",
                   help="route cross-module calls into tail groups through wrappers, so #13356 cannot mask others")
    c = sub.add_parser("check")
    c.add_argument("dirs", nargs="+")
    c.add_argument("--configs")
    st = sub.add_parser("selftest")
    st.add_argument("dirs", nargs="*")
    g = sub.add_parser("gen")
    g.add_argument("seed", type=int)
    g.add_argument("--target", default="native")
    r = sub.add_parser("reduce")
    r.add_argument("seed", help="a seed, or the prog.pickle a fuzz run saved beside a disagreement")
    r.add_argument("--configs")
    a = ap.parse_args()
    sys.setrecursionlimit(10000)
    return {"fuzz": fuzz, "check": check, "selftest": selftest, "gen": show, "reduce": reduce_cmd}[a.cmd](a)


if __name__ == "__main__":
    sys.exit(main())
