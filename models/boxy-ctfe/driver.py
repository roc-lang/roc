"""Driver for the boxy-ctfe model. Called by run.sh; see README.md.

  driver.py fuzz N [--seed S] [--configs a,b,..] [--no-reduce] [--keep]
  driver.py check DIR... [--configs ..]   DIR holds app.roc and optionally expected.txt
  driver.py selftest DIR...              generator check, the fixtures DIR..., a small fuzz batch
  driver.py gen SEED                     print a generated program
  driver.py reduce SEED [--configs ..]   regenerate program SEED and reduce its failure
  driver.py reach SEED                   show which definitions the compile-time evaluator completed

Environment: ROC (the compiler), OUT (output directory), JOBS (configurations run at once).
"""
import argparse
import os
import pickle
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import gen  # noqa: E402

# name: (command, opt, specialize). "run" is `roc app.roc` (in process), "build" builds and runs the executable,
# "test" is `roc test`, "cache" builds twice into a private cache with ROC_VERIFY_BOXY_OBJECT_REUSE and runs the
# second build.
CONFIGS = {
    "test": ("test", "dev", "no"),
    "test-spec": ("test", "dev", "yes"),
    "run-interp": ("run", "interpreter", "no"),
    "run-dev": ("run", "dev", "no"),
    "dev": ("build", "dev", "no"),
    "dev-spec": ("build", "dev", "yes"),
    "speed": ("build", "speed", "yes"),
    "speed-boxy": ("build", "speed", "no"),
    "cache-dev": ("cache", "dev", "no"),
}
DEFAULT = list(CONFIGS)
# fuzz runs CORE on every program and one of ROTATE (by seed), to spend less time per program
CORE = ["test", "run-interp", "dev", "dev-spec", "speed", "speed-boxy"]
ROTATE = ["cache-dev", "run-dev", "test-spec"]
TIMEOUT = 180
JOBS = int(os.environ.get("JOBS", "4"))
PANIC = re.compile(r"panic|Segmentation fault|reached unreachable|assert|integer overflow|index out of bounds|"
                   r"thread \d+ panic|compiler bug|invariant", re.I)


# Failures already drafted, by a substring of their panic message: they are reported, not reduced again.
KNOWN = {
    "static initializer ZST struct had runtime fields": "#13423",
    "guarded list invalidated: LirStore.local_ids": "#13424",
}


def known(res):
    """The draft every failing configuration's failure is already filed as, or None."""
    ids = set()
    for v, detail in res.values():
        if v == "ok":
            continue
        sig = signature(detail)
        hit = [k for pat, k in KNOWN.items() if sig and pat in sig]
        if not hit:
            return None
        ids.update(hit)
    return ",".join(sorted(ids)) or None


def roc():
    return os.environ["ROC"]


def out_dir():
    return os.environ.get("OUT", os.path.join(HERE, "out"))


def tail(s, n=8):
    """The last n lines of s, after the panic message if there is one."""
    lines = [l for l in s.strip().splitlines() if l.strip()]
    m = re.search(r"panic: (.*)", s)
    head = [f"panic: {m.group(1)}"] if m else []
    return "\n".join(head + lines[-n:])


def signature(detail):
    """What must stay the same while reducing a failure: its panic message, if any."""
    m = re.search(r"panic: (.*)", detail or "")
    return m.group(1) if m else None


def env_for(cache=None):
    e = dict(os.environ)
    e["ROC_VERIFY_BOXY_OBJECT_REUSE"] = "1"
    if cache:
        e["ROC_CACHE_DIR"] = cache
    return e


def sh(cmd, d, env=None):
    return subprocess.run(cmd, cwd=d, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL,
                          env=env or env_for())


def run_config(d, name):
    """('ok', stdout) | ('build', message) | ('crash', message) | ('test', message)."""
    d = os.path.abspath(d)
    mode, opt, spec = CONFIGS[name]
    flags = [f"--opt={opt}", f"--specialize={spec}"]
    try:
        if mode == "test":
            p = sh([roc(), "test", f"--specialize={spec}", "--no-cache", "app.roc"], d)
            text = p.stdout + p.stderr
            if p.returncode not in (0, 2) or "passed" not in text or PANIC.search(text):
                return ("test", f"rc={p.returncode} {tail(text, 14)}")
            return ("ok", None)
        if mode == "run":
            p = sh([roc()] + flags + ["--no-cache", "app.roc"], d)
            if p.returncode not in (0, 2) or PANIC.search(p.stderr):
                return ("crash", f"rc={p.returncode} {tail(p.stderr)}")
            return ("ok", p.stdout)
        out = os.path.join(d, f"bin-{name}")
        if os.path.exists(out):
            os.remove(out)
        if mode == "cache":
            cache = os.path.join(d, f"cache-{name}")
            shutil.rmtree(cache, ignore_errors=True)
            os.makedirs(cache)
            env = env_for(cache)
            first = sh([roc(), "build"] + flags + [f"--output={out}", "app.roc"], d, env)
            if not os.path.exists(out):
                return ("build", f"cold build rc={first.returncode} {tail(first.stdout + first.stderr)}")
            os.remove(out)
            p = sh([roc(), "build"] + flags + [f"--output={out}", "app.roc"], d, env)
            shutil.rmtree(cache, ignore_errors=True)
        else:
            p = sh([roc(), "build"] + flags + ["--no-cache", f"--output={out}", "app.roc"], d)
        if not os.path.exists(out) or PANIC.search(p.stderr):
            return ("build", f"rc={p.returncode} {tail(p.stdout + p.stderr)}")
        q = subprocess.run([out], cwd=d, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL)
        os.remove(out)
        if q.returncode != 0:
            return ("crash", f"rc={q.returncode} {tail(q.stderr)}")
        return ("ok", q.stdout)
    except subprocess.TimeoutExpired:
        return ("crash", "timeout")


def parse(stdout):
    """{label: value} from `label: value` lines, in order."""
    out = {}
    for line in stdout.splitlines():
        if ": " in line:
            k, v = line.split(": ", 1)
            out[k] = v
    return out


def consistency(lines):
    """Labels of probes whose sides disagree within one run."""
    bad = []
    groups = {}
    for k, v in lines.items():
        key, _, side = k.partition(" ")
        if side == "eq":
            if v != "eq":
                bad.append(key)
            continue
        groups.setdefault(key, {})[side] = v
    for key, sides in groups.items():
        if len(set(sides.values())) > 1:
            bad.append(key)
    return sorted(set(bad))


def judge_dir(d, configs, labels=None):
    """{config: (verdict, detail)}. verdict: ok, inconsistent (sides of a probe disagree), differs (another output
    than the first run config), missing (a label never printed), crash, build, test."""
    with ThreadPoolExecutor(max_workers=JOBS) as pool:
        done = dict(zip(configs, pool.map(lambda c: run_config(d, c), configs)))
    res = {}
    ref = None
    for c in configs:
        kind, text = done[c]
        if kind != "ok":
            res[c] = (kind, text)
            continue
        if text is None:
            res[c] = ("ok", "")
            continue
        lines = parse(text)
        if labels is not None:
            miss = [l for l in labels if l not in lines]
            if miss:
                res[c] = ("missing", ", ".join(miss[:5]))
                continue
        bad = consistency(lines)
        if bad:
            detail = []
            for b in bad[:3]:
                detail += [f"{k}: {v}" for k, v in lines.items() if k.split(" ")[0] == b]
            res[c] = ("inconsistent", " ".join(bad) + "\n" + "\n".join(detail))
            continue
        if ref is None:
            ref = (c, lines)
            res[c] = ("ok", "")
        elif lines != ref[1]:
            diff = [f"{k}: {ref[1].get(k)} | {lines.get(k)}" for k in lines if ref[1].get(k) != lines.get(k)][:4]
            res[c] = ("differs", f"vs {ref[0]}\n" + "\n".join(diff))
        else:
            res[c] = ("ok", "")
    return res


def bad(res):
    return sorted(c for c, (v, _) in res.items() if v != "ok")


def write_prog(prog, d):
    if os.path.isdir(d):
        shutil.rmtree(d)
    os.makedirs(d)
    em = gen.Emit(prog)
    for name, text in em.files().items():
        with open(os.path.join(d, name), "w") as f:
            f.write(text)
    return em.labels()


def roc_check(d):
    """None if `roc check` accepts the program, else its output."""
    try:
        p = sh([roc(), "check", "--no-cache", "app.roc"], d)
    except subprocess.TimeoutExpired:
        return "timeout"
    text = p.stdout + p.stderr
    m = re.search(r"(\d+) errors? and", text)
    if p.returncode not in (0, 2) or (m and m.group(1) != "0") or PANIC.search(text):
        return f"rc={p.returncode}\n{tail(text, 30)}"
    return None


def judge(prog, d, configs):
    labels = write_prog(prog, d)
    err = roc_check(d)
    if err is not None:
        kind = "panic" if PANIC.search(err) else "timeout" if err == "timeout" else "rejected"
        return {"check": (kind, err)}
    return judge_dir(d, configs, labels)


# --------------------------------------------------------------------------------------------- reducer

def children(e):
    """(path, child) for every child expression of e."""
    k = e[0]
    out = []
    for i, c in enumerate(e):
        if i < 2 or (k == "fold" and i == 6):  # a fold's element type is a type, not an expression
            continue
        if isinstance(c, tuple) and c and isinstance(c[0], str) and len(c) >= 2 and isinstance(c[1], tuple):
            out.append(((i,), c))
        elif isinstance(c, list):
            for j, x in enumerate(c):
                if isinstance(x, tuple) and x and isinstance(x[0], str) and len(x) >= 2 and isinstance(x[1], tuple):
                    out.append(((i, j), x))
                elif isinstance(x, tuple) and k == "match":  # (tag, vars, body)
                    out.append(((i, j, 2), x[2]))
    return out


def get_at(e, path):
    for i in path:
        e = e[i]
    return e


def replace_at(e, path, new):
    if not path:
        return new
    i = path[0]
    lst = list(e)
    lst[i] = replace_at(e[i], path[1:], new)
    return tuple(lst) if isinstance(e, tuple) else lst


def node_paths(e, path=()):
    yield path, e
    for sub, c in children(e):
        yield from node_paths(c, path + sub)


def free_vars_ok(sub, env_names):
    """True if sub reads no variable outside env_names (used when hoisting a child over its binders)."""
    bound = set()

    def walk(x, bound):
        if x[0] == "var":
            return x[2] in bound or x[2] in env_names
        nb = set(bound)
        if x[0] == "let":
            if not walk(x[3], nb):
                return False
            nb.add(x[2])
            return walk(x[4], nb)
        if x[0] == "lam":
            nb.add(x[2])
        if x[0] == "fold":
            nb |= {x[4], x[5]}
        if x[0] == "matchrec":
            nb |= {v for _, v, _ in x[4]}
        for path, c in children(x):
            nb2 = set(nb)
            if x[0] == "match" and len(path) == 3:
                nb2 |= set(x[3][path[1]][1])
            if not walk(c, nb2):
                return False
        return True

    return walk(sub, bound)


def simplest(g, t):
    """The smallest literal of type t."""
    k = t[0]
    if k == "int":
        return ("lit", t, 0)
    if k in ("float", "dec"):
        return ("lit", t, "0.0")
    if k == "bool":
        return ("lit", t, False)
    if k == "str":
        return ("lit", t, "")
    if k == "unit":
        return ("unit", t)
    if k == "rec":
        return ("rec", t, [simplest(g, x) for _, x in t[1]])
    if k == "tup":
        return ("tup", t, [simplest(g, x) for x in t[1]])
    if k == "tags":
        tag, pl = min(t[1], key=lambda c: len(c[1]))
        return ("tag", t, tag, [simplest(g, x) for x in pl])
    if k == "list":
        return ("list", t, [])
    if k == "try":
        return ("tag", t, "Ok", [simplest(g, t[1])])
    if k == "nom":
        return ("nomwrap", t, simplest(g, g.noms[t[1]]))
    if k == "fn":
        return ("lam", t, "xr", simplest(g, t[2]))
    if k == "box":
        return ("call", t, "Box.box", [simplest(g, t[1])])
    raise ValueError(t)


def candidates(prog, e):
    """Smaller same-typed replacements for e and its descendants: (path, new)."""
    for path, node in node_paths(e):
        t = node[1]
        s = simplest(prog, t)
        if node != s:
            yield path, s
        for sub, c in children(node):
            if c[1] == t and free_vars_ok(c, set()) or c[1] == t and node[0] not in ("let", "lam", "fold", "match", "matchrec"):
                yield path, c
        if node[0] == "let" and free_vars_ok(node[4], set()):
            yield path, node[4]


def used_helpers(prog):
    """Remove helpers no probe or helper reaches."""
    by = {h.name: h for h in prog.helpers}
    seen = set()
    stack = [pr.expr for pr in prog.probes]
    while stack:
        e = stack.pop()
        for _, n in node_paths(e):
            if n[0] == "hcall" and n[2] not in seen:
                seen.add(n[2])
                stack.append(by[n[2]].body)
    prog.helpers = [h for h in prog.helpers if h.name in seen]


def reduce(prog, d, configs, failing):
    """Shrink prog while `still(res)` holds: the same configs fail with the same verdict kinds."""
    want = {c: failing[c][0] for c in failing}
    sigs = {c: signature(failing[c][1]) for c in failing}
    refs = [m.group(1) for v, detail in failing.values() if v == "differs" for m in [re.match(r"vs (\S+)", detail)] if m]
    run = list(dict.fromkeys(refs + sorted(want)))

    def still(p):
        used_helpers(p)
        res = judge(p, d, run if "check" not in want else ["test"])
        if "check" in want:
            return "check" in res and res["check"][0] == want["check"] and signature(res["check"][1]) == sigs["check"]
        return all(c in res and res[c][0] == v and signature(res[c][1]) == sigs[c] for c, v in want.items())

    # drop probes from the end, replacing reads of a dropped probe by a literal
    changed = True
    while changed:
        changed = False
        for i in reversed(range(len(prog.probes))):
            if len(prog.probes) <= 1:
                break
            cand = prog.copy()
            dropped = cand.probes.pop(i)
            cand.probes = [drop_reads(cand, pr, dropped.idx) for pr in cand.probes]
            if still(cand):
                prog = cand
                changed = True
                print(f"  reduce: {len(prog.probes)} probes", flush=True)
    # shrink expressions: probes, then helper bodies
    changed = True
    while changed:
        changed = False
        for pi in range(len(prog.probes)):
            for path, new in list(candidates(prog, prog.probes[pi].expr)):
                cand = prog.copy()
                try:
                    cand.probes[pi].expr = replace_at(cand.probes[pi].expr, path, new)
                except (IndexError, TypeError):
                    continue
                if cand.probes[pi].expr == prog.probes[pi].expr:
                    continue
                if still(cand):
                    prog = cand
                    changed = True
                    print(f"  reduce: probe {pi} {size(prog)} nodes", flush=True)
                    break
        for hi in range(len(prog.helpers)):
            if hi >= len(prog.helpers):
                break
            h = prog.helpers[hi]
            for path, new in list(candidates(prog, h.body)):
                cand = prog.copy()
                try:
                    cand.helpers[hi].body = replace_at(h.body, path, new)
                except (IndexError, TypeError):
                    continue
                if cand.helpers[hi].body == h.body or not free_vars_ok(cand.helpers[hi].body, {n for n, _ in h.params}):
                    continue
                if still(cand):
                    prog = cand
                    changed = True
                    print(f"  reduce: helper {h.name} {size(prog)} nodes", flush=True)
                    break
    used_helpers(prog)
    return prog


def drop_reads(prog, pr, idx):
    def walk(e):
        if e[0] == "probe" and e[2] == idx:
            return simplest(prog, e[1])
        for path, c in children(e):
            e = replace_at(e, path, walk(c))
        return e
    pr.expr = walk(pr.expr)
    return pr


def size(prog):
    return sum(1 for pr in prog.probes for _ in node_paths(pr.expr)) + \
        sum(1 for h in prog.helpers for _ in node_paths(h.body))


# --------------------------------------------------------------------------------------------- commands

def report(res):
    for c in sorted(res):
        v, detail = res[c]
        if v != "ok":
            print(f"  {c}: {v}")
            for line in (detail or "").splitlines()[:16]:
                print(f"      {line}")


def fuzz(args):
    if getattr(args, "with_known", False):
        gen.AVOID_KNOWN = False
    fixed = args.configs.split(",") if args.configs else None
    base = os.path.join(out_dir(), "fuzz")
    new = 0
    for i in range(args.n):
        seed = args.seed + i
        configs = fixed or CORE + [ROTATE[seed % len(ROTATE)]]
        prog = gen.generate(seed)
        d = os.path.join(base, str(seed))
        res = judge(prog, d, configs)
        b = bad(res)
        if not b:
            print(f"seed {seed}: ok ({len(prog.probes)} probes)", flush=True)
            if not args.keep:
                shutil.rmtree(d, ignore_errors=True)
            continue
        if "check" in res and res["check"][0] == "rejected":
            print(f"seed {seed}: GENERATOR program rejected by roc check", flush=True)
            report(res)
            continue
        k = known(res)
        if k:
            print(f"seed {seed}: known {k} ({' '.join(b)})", flush=True)
            if not args.keep:
                shutil.rmtree(d, ignore_errors=True)
            continue
        new += 1
        print(f"seed {seed}: FAIL {' '.join(b)}", flush=True)
        report(res)
        with open(os.path.join(d, "prog.pickle"), "wb") as f:
            pickle.dump(prog, f)
        with open(os.path.join(d, "verdict.txt"), "w") as f:
            for c in sorted(res):
                f.write(f"{c}: {res[c][0]}\n{res[c][1] or ''}\n")
        if not args.no_reduce:
            small = reduce(prog, os.path.join(d, "min"), configs, {c: res[c] for c in b})
            write_prog(small, os.path.join(d, "min"))
            print(f"  reduced to {os.path.join(d, 'min', 'app.roc')}", flush=True)
    print(f"{args.n} programs, {new} failing")
    return 1 if new else 0


def check(args):
    configs = args.configs.split(",") if args.configs else DEFAULT
    failed = 0
    for d in args.dirs:
        # Build in a copy, so nothing a run writes lands next to the fixture.
        src = os.path.abspath(d)
        d = os.path.join(out_dir(), "check", os.path.basename(src))
        shutil.rmtree(d, ignore_errors=True)
        shutil.copytree(src, d)
        res = judge_dir(d, configs)
        exp = os.path.join(d, "expected.txt")
        if os.path.exists(exp):
            want = open(exp).read().rstrip("\n")
            for c in configs:
                mode = CONFIGS[c][0]
                if res[c][0] == "ok" and mode not in ("test",):
                    got = run_config(d, c)[1].rstrip("\n")
                    if got != want:
                        res[c] = ("expected", got)
        known = os.path.join(d, "known.txt")
        allowed = set(open(known).read().split()) if os.path.exists(known) else set()
        b = [c for c in bad(res) if c not in allowed]
        print(f"{src}: {'ok' if not b else 'FAIL ' + ' '.join(b)}")
        report({c: res[c] for c in b})
        failed += bool(b)
    return 1 if failed else 0


def selftest(args):
    # the generator: programs are produced and emitted deterministically
    for s in range(200):
        a = gen.Emit(gen.generate(s)).source()
        b = gen.Emit(gen.generate(s)).source()
        assert a == b, f"seed {s} is not deterministic"
    print("generator: 200 programs emit deterministically")
    rc = check(argparse.Namespace(dirs=args.dirs, configs=args.configs))
    if rc:
        return rc
    return fuzz(argparse.Namespace(n=3, seed=args.seed, configs=args.configs, no_reduce=True, keep=False,
                                   with_known=False))


def show(args):
    prog = gen.generate(args.seed)
    print(gen.Emit(prog).source())


def reach(args):
    """Run `roc check` under ROC_TRACE_BOXY_WORK and ROC_HOIST_ROOT_STATS and list the roots it evaluated."""
    prog = gen.generate(args.seed)
    d = os.path.join(out_dir(), "reach", str(args.seed))
    write_prog(prog, d)
    e = env_for()
    e["ROC_TRACE_BOXY_WORK"] = "1"
    e["ROC_HOIST_ROOT_STATS"] = "1"
    p = subprocess.run([roc(), "check", "--no-cache", "app.roc"], cwd=d, capture_output=True, text=True, env=e,
                       timeout=TIMEOUT)
    roots = [l for l in p.stderr.splitlines() if "pending root" in l or "hoist-roots" in l]
    print("\n".join(roots))
    s = reach_summary(p.stderr, prog)
    print(s)
    return 0


def reach_summary(trace, prog):
    """The probes whose constant the evaluator completed, and any root inside a runtime side."""
    roots = [l for l in trace.splitlines() if "pending root" in l]
    consts = sum(1 for pr in prog.probes if any(re.search(rf":\s+c{pr.idx}\b", l) for l in roots))
    leaks = [l for l in roots if "rt(z" in l and "literal_argument" not in l]
    return (f"{len(roots)} pending roots; {consts}/{len(prog.probes)} probe constants among them; "
            f"{len(leaks)} roots (other than literal arguments) start inside a tainted runtime side")


def reduce_cmd(args):
    configs = args.configs.split(",") if args.configs else DEFAULT
    if os.path.exists(args.seed):
        prog = pickle.load(open(args.seed, "rb"))
        d = os.path.join(os.path.dirname(args.seed), "min")
    else:
        prog = gen.generate(int(args.seed))
        d = os.path.join(out_dir(), "reduce", args.seed)
    res = judge(prog, d, configs)
    b = bad(res)
    if not b:
        print("no failure to reduce")
        return 0
    report(res)
    small = reduce(prog, d, configs, {c: res[c] for c in b})
    write_prog(small, d)
    print(open(os.path.join(d, "app.roc")).read())
    report(judge(small, d, configs))
    return 0


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("fuzz")
    f.add_argument("n", type=int, nargs="?", default=20)
    f.add_argument("--seed", type=int, default=1)
    f.add_argument("--configs")
    f.add_argument("--no-reduce", action="store_true")
    f.add_argument("--keep", action="store_true")
    f.add_argument("--with-known", action="store_true")
    c = sub.add_parser("check")
    c.add_argument("dirs", nargs="+")
    c.add_argument("--configs")
    s = sub.add_parser("selftest")
    s.add_argument("dirs", nargs="*")
    s.add_argument("--configs")
    s.add_argument("--seed", type=int, default=1000)
    g = sub.add_parser("gen")
    g.add_argument("seed", type=int)
    r = sub.add_parser("reach")
    r.add_argument("seed", type=int)
    rd = sub.add_parser("reduce")
    rd.add_argument("seed")
    rd.add_argument("--configs")
    a = ap.parse_args()
    fn = {"fuzz": fuzz, "check": check, "selftest": selftest, "gen": show, "reach": reach, "reduce": reduce_cmd}[a.cmd]
    sys.exit(fn(a))


if __name__ == "__main__":
    main()
