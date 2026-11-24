"""Driver for the codec-roundtrip model. Called by run.sh; see README.md.

  driver.py fuzz N [--seed S] [--cases K] [--configs a,b,..] [--keep] [--no-reduce]
  driver.py check DIR...            DIR holds Prog.roc (a type module of expects) or app.roc, and expected.txt
  driver.py selftest DIR...         reference-encoder unit checks, the fixtures DIR..., a 2-program fuzz batch
  driver.py gen SEED [--cases K]    print program SEED as a test module and its expected app output
  driver.py reduce SEED [--cases K] regenerate program SEED and reduce its disagreement to one case

Environment: ROC (the compiler), OUT (output directory), NODE (default `node`), JOBS (parallel builds, default 3).
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

# name: (kind, target, opt, specialize). kind "test" is `roc test` on the module; "run" is `roc app.roc` in
# process; "build" builds the app and runs the executable (or the wasm module on Node).
CONFIGS = {
    "test-dev": ("test", "native", "dev", "no"),
    "test-dev-spec": ("test", "native", "dev", "yes"),
    "test-speed": ("test", "native", "speed", "yes"),
    "test-speed-boxy": ("test", "native", "speed", "no"),
    "run-interp": ("run", "native", "interpreter", "no"),
    "dev": ("build", "native", "dev", "no"),
    "dev-spec": ("build", "native", "dev", "yes"),
    "speed": ("build", "native", "speed", "yes"),
    "speed-boxy": ("build", "native", "speed", "no"),
    "wasm-dev": ("build", "wasm", "dev", "no"),
    "wasm-dev-spec": ("build", "wasm", "dev", "yes"),
    "wasm-speed": ("build", "wasm", "speed", "yes"),
}
DEFAULT = list(CONFIGS)
TIMEOUT = 300
JOBS = int(os.environ.get("JOBS", "3"))


def roc():
    return os.environ["ROC"]


def out_dir():
    return os.environ.get("OUT", os.path.join(HERE, "out"))


def tail(s, n=8):
    lines = [l for l in s.strip().splitlines() if l.strip() and not l.lstrip().startswith("at ")]
    return "\n".join(lines[-n:])


def panic_line(s):
    """The compiler's panic message, if the output has one."""
    for l in s.splitlines():
        if "panic:" in l or "invariant violated" in l or "reached unreachable" in l:
            return l.strip()
    return None


def write_program(prog, base):
    """Write the test module and both apps under base. Returns {target: dir}."""
    if os.path.isdir(base):
        shutil.rmtree(base)
    dirs = {"test": os.path.join(base, "test"), "native": os.path.join(base, "native"),
            "wasm": os.path.join(base, "wasm")}
    for d in dirs.values():
        os.makedirs(d)
    text, where = gen.emit_test_module(prog)
    with open(os.path.join(dirs["test"], "Prog.roc"), "w") as f:
        f.write(text)
    with open(os.path.join(dirs["test"], "where.txt"), "w") as f:
        for line, (ci, j) in sorted(where.items()):
            f.write(f"{line} c{ci} {j}\n")
    with open(os.path.join(dirs["native"], "app.roc"), "w") as f:
        f.write(gen.emit_app(prog, "native"))
    with open(os.path.join(dirs["wasm"], "app.roc"), "w") as f:
        f.write(gen.emit_app(prog, "wasm"))
    os.symlink(os.path.join(HERE, "platform"), os.path.join(dirs["wasm"], "platform"))
    with open(os.path.join(base, "expected.txt"), "w") as f:
        f.write("\n".join(gen.expected_app_lines(prog)) + "\n")
    with open(os.path.join(base, "prog.pickle"), "wb") as f:
        pickle.dump(prog, f)
    return {k: os.path.abspath(v) for k, v in dirs.items()}, where


def run_config(dirs, name):
    """('ok', text) | ('compile', msg) | ('panic', msg) | ('crash', msg)."""
    kind, target, opt, spec = CONFIGS[name]
    flags = [f"--opt={opt}", f"--specialize={spec}", "--no-cache"]
    try:
        if kind == "test":
            d = dirs["test"]
            p = subprocess.run([roc(), "test"] + flags + ["Prog.roc"], cwd=d, capture_output=True, text=True,
                               timeout=TIMEOUT, stdin=subprocess.DEVNULL)
            both = p.stdout + p.stderr
            if panic_line(both):
                return ("panic", panic_line(both))
            return ("ok", both) if p.returncode in (0, 1, 2) else ("crash", f"rc={p.returncode} {tail(both)}")
        d = dirs[target]
        if kind == "run":
            p = subprocess.run([roc()] + flags + ["app.roc"], cwd=d, capture_output=True, text=True,
                               timeout=TIMEOUT, stdin=subprocess.DEVNULL)
            if panic_line(p.stderr):
                return ("panic", panic_line(p.stderr))
            # rc 2 means the program ran but compilation reported warnings.
            if p.returncode not in (0, 2) or not p.stdout:
                return ("crash", f"rc={p.returncode} {tail(p.stderr)}")
            return ("ok", p.stdout)
        out = os.path.join(d, f"bin-{name}" + (".wasm" if target == "wasm" else ""))
        if os.path.exists(out):
            os.remove(out)
        cmd = [roc(), "build"] + flags + [f"--output={out}", "app.roc"]
        if target == "wasm":
            cmd.insert(2, "--target=wasm32")
        p = subprocess.run(cmd, cwd=d, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL)
        both = p.stdout + p.stderr
        if panic_line(both):
            return ("panic", panic_line(both))
        if not os.path.exists(out):
            return ("compile", f"rc={p.returncode} {tail(both, 30)}")
        run = [os.environ.get("NODE", "node"), os.path.join(HERE, "run_wasm.js"), out] if target == "wasm" else [out]
        q = subprocess.run(run, cwd=d, capture_output=True, text=True, timeout=TIMEOUT, stdin=subprocess.DEVNULL)
        os.remove(out)
        if q.returncode != 0:
            return ("crash", f"rc={q.returncode} {tail(q.stderr)}")
        return ("ok", q.stdout)
    except subprocess.TimeoutExpired:
        return ("crash", "timeout")


def test_failures(text, where):
    """{(case, judgment)} for each failed expect in `roc test` output, or None if the module did not compile."""
    m = re.search(r"(\d+) compiler errors?", text)
    if (m and int(m.group(1)) > 0) or "Compilation failed" in text:
        return None
    bad = set()
    for m in re.finditer(r"\u2717 fail\W+Prog\.roc:(\d+)", text):
        line = int(m.group(1))
        cands = [l for l in where if l <= line]
        bad.add(where[max(cands)] if cands else (-1, f"line {line}"))
    if bad:
        return bad
    m = re.search(r"All \((\d+)\) tests? passed", text)
    return set() if m and int(m.group(1)) == len(where) else None


def compare_app(text, expected):
    """{(case, field)} where the app's output differs from the model."""
    got = text.rstrip("\n").split("\n")
    names = ["roundtrip", "decode", "eq", "neq", "hash", "encode"]
    bad = set()
    for i, want in enumerate(expected):
        g = got[i] if i < len(got) else ""
        wp = want.split(" ", 6)
        gp = g.split(" ", 6)
        for j, n in enumerate(names):
            if j + 1 >= len(wp) or j + 1 >= len(gp) or wp[j + 1] != gp[j + 1]:
                bad.add((i, n))
    return bad


def roc_check(dirs):
    """None if the app type-checks, else the error tail. A program the model wrote that does not check is a
    generator bug, unless the compiler panics on it."""
    p = subprocess.run([roc(), "check", "--no-cache", "app.roc"], cwd=dirs["native"], capture_output=True, text=True,
                       timeout=TIMEOUT, stdin=subprocess.DEVNULL)
    both = p.stdout + p.stderr
    if panic_line(both):
        return "panic: " + panic_line(both)
    m = re.search(r"(\d+) errors? and", both)
    return tail(both, 30) if m and int(m.group(1)) > 0 else None


def judge(prog, base, configs):
    dirs, where = write_program(prog, base)
    checked = roc_check(dirs)
    with open(os.path.join(base, "check.txt"), "w") as fh:
        fh.write(checked or "ok\n")
    expected = gen.expected_app_lines(prog)
    with ThreadPoolExecutor(max_workers=JOBS) as pool:
        done = dict(zip(configs, pool.map(lambda c: run_config(dirs, c), configs)))
    res = {}
    for c in configs:
        kind, text = done[c]
        if kind != "ok":
            res[c] = (kind, text)
        elif CONFIGS[c][0] == "test":
            f = test_failures(text, where)
            if f is None:
                res[c] = ("compile", tail(text, 30))
            else:
                res[c] = ("ok", "") if not f else ("wrong", sorted(f))
        else:
            f = compare_app(text, expected)
            res[c] = ("ok", "") if not f else ("wrong", sorted(f), text)
    if checked:
        res["roc-check"] = ("panic", checked) if checked.startswith("panic: ") else ("ill-typed", checked)
    with open(os.path.join(base, "verdicts.txt"), "w") as fh:
        for c in res:
            fh.write(f"== {c}: {res[c][0]}\n{res[c][1:]}\n")
    return res


# Open findings a panic message identifies, so a long fuzz run can tell them from new ones. This matches on the
# message only: another bug with the same message would be labelled with the finding too.
KNOWN = {
    "instantiation graph relation changed after final relation production": "#13429",
    "expected struct_ layout, got zst": "#13430",
    "field access expected struct layout, got zst": "#13430",
    "did not consume checker-required generated codec call invalid_value": "#13431",
}


def label(res):
    """The open findings whose messages appear in res, and whether every other verdict is ok."""
    ids, rest = set(), False
    for v in res.values():
        if v[0] == "ok":
            continue
        hit = [f for m, f in KNOWN.items() if isinstance(v[1], str) and m in v[1]]
        ids.update(hit)
        rest = rest or not hit
    # #13430's LLVM and interpreter failures are not in KNOWN, so a program that hits it is still labelled NEW.
    return sorted(ids), rest


def bad(res):
    return sorted(c for c, v in res.items() if v[0] != "ok")


def signature(res):
    """What a reduction must keep: each failing config's verdict kind and, for panics, the message."""
    out = {}
    for c, v in res.items():
        if v[0] == "ok":
            continue
        out[c] = (v[0], v[1] if v[0] == "panic" else None)
    return out


# --------------------------------------------------------------------------------------------- reduction

def sub_prog(prog, keep):
    p = gen.Prog()
    p.noms = prog.noms
    p.cases = [prog.cases[i] for i in keep]
    return p


def reduce(prog, base, configs, res):
    """Keep one case that still disagrees in the same way, then try smaller values. Writes base/min/. When no single
    case disagrees on its own (the cases interact), the program is kept whole."""
    sig = signature(res)
    want = [c for c in configs if c in sig]
    target = sig

    def still(p, tag):
        r = judge(p, os.path.join(base, "reduce", tag), want)
        s = signature(r)
        return all(s.get(c, ("ok",))[0] == target[c][0] for c in want) and \
            all(s.get(c) == target[c] for c in want if target[c][0] == "panic")

    cur = prog
    for i in range(len(prog.cases)):
        p = sub_prog(prog, [i])
        if still(p, f"case{i}"):
            cur = p
            break
    # Replace v and w by smaller values of the same type.
    for _ in range(3 if len(cur.cases) == 1 else 0):
        c = cur.cases[0]
        g = gen.Gen(__import__("random").Random(len(str(c.v))))
        g.prog = cur
        tried = False
        for attempt in range(4):
            v = g.value(c.ty, depth=attempt)
            if len(repr(v)) >= len(repr(c.v)):
                continue
            p = gen.Prog()
            p.noms = cur.noms
            p.cases = [gen.Case(c.ty, v, v if gen.equal(cur, c.ty, c.v, c.w) else c.w)]
            if gen.equal(cur, c.ty, v, p.cases[0].w) != gen.equal(cur, c.ty, c.v, c.w):
                continue
            if still(p, f"val{attempt}"):
                cur = p
                tried = True
                break
        if not tried:
            break
    write_program(cur, os.path.join(base, "min"))
    return cur


# --------------------------------------------------------------------------------------------- commands

def describe(res):
    lines = []
    for c, v in sorted(res.items()):
        if v[0] == "ok":
            continue
        detail = v[1] if not isinstance(v[1], str) else v[1].replace("\n", "\n      ")
        lines.append(f"  {c}: {v[0]} {detail}")
    return "\n".join(lines)


def cmd_fuzz(args):
    configs = args.configs.split(",") if args.configs else DEFAULT
    base = os.path.join(out_dir(), "fuzz")
    found = 0
    for k in range(args.n):
        seed = args.seed + k
        prog = gen.generate(seed, args.cases)
        d = os.path.join(base, str(seed))
        res = judge(prog, d, configs)
        b = bad(res)
        if not b:
            print(f"seed {seed}: ok ({len(prog.cases)} cases)", flush=True)
            if not args.keep:
                shutil.rmtree(d)
            continue
        if "roc-check" in res and res["roc-check"][0] == "ill-typed" and \
                not any(v[0] == "panic" for v in res.values()):
            print(f"seed {seed}: GENERATOR BUG (program does not type-check)\n{describe({'roc-check': res['roc-check']})}",
                  flush=True)
            found += 1
            continue
        found += 1
        ids, unexplained = label(res)
        tag = "known " + ",".join(ids) if ids and not unexplained else "NEW" + (" with " + ",".join(ids) if ids else "")
        print(f"seed {seed}: DISAGREE [{tag}] {','.join(b)}\n{describe(res)}", flush=True)
        if tag.startswith("known"):
            continue
        if not args.no_reduce:
            m = reduce(prog, d, configs, res)
            print(f"  reduced to {len(m.cases)} case(s), first: {gen.render_type(m.cases[0].ty)} -> {d}/min", flush=True)
    print(f"{args.n} programs, {found} with disagreements")
    return 1 if found else 0


def cmd_check(args):
    configs = args.configs.split(",") if args.configs else DEFAULT
    rc = 0
    for d in args.dirs:
        with open(os.path.join(d, "case.py")) as f:
            src = f.read()
        prog = eval(src, {"gen": gen})  # a fixture is a gen.Prog built from the model's own constructors
        known = set()
        kp = os.path.join(d, "known.txt")
        if os.path.exists(kp):
            known = {l.split()[0] for l in open(kp) if l.strip() and not l.startswith("#")}
        res = judge(prog, os.path.join(out_dir(), "check", os.path.basename(os.path.normpath(d))), configs)
        b = set(bad(res))
        if b != known & set(configs):
            print(f"{d}: FAIL expected failing {sorted(known)}, got {sorted(b)}\n{describe(res)}")
            rc = 1
        else:
            print(f"{d}: ok" + (f" (known: {', '.join(sorted(b))})" if b else ""))
    return rc


def unit_checks():
    assert gen.json_string('a"\\\n\x01\x7fé') == '"a\\"\\\\\\n\\u0001\x7fé"'
    assert gen.dec_str(12 * gen.DEC_ONE + gen.DEC_ONE // 2) == "12.5"
    assert gen.dec_str(gen.DEC_ONE) == "1.0"
    assert gen.dec_str(-gen.DEC_ONE // 4) == "-0.25"
    assert gen.dec_str(1) == "0.000000000000000001"
    p = gen.Prog()
    rec = ("record", [("b", ("int", "U8"), False), ("a", ("str",), True)])
    v = ("rec", {"b": 3, "a": ("tag", "Err", [("tag", "Missing", [])])})
    assert gen.encode(p, rec, v) == '{"b":3}'
    tags = ("tags", [("A", []), ("B", [("str",)]), ("C", [("bool",), ("int", "I8")])])
    assert gen.encode(p, tags, ("tag", "A", [])) == '"A"'
    assert gen.encode(p, tags, ("tag", "B", ["x"])) == '{"B":"x"}'
    assert gen.encode(p, tags, ("tag", "C", [True, -1])) == '{"C":[true,-1]}'
    nul = ("try", ("str",), ("tags", [("Null", [])]))
    assert gen.encode(p, nul, ("tag", "Err", [("tag", "Null", [])])) == "null"
    # Every generated program's reference is consistent with itself: equal is reflexive.
    for s in range(200):
        prog = gen.generate(s)
        for c in prog.cases:
            assert gen.equal(prog, c.ty, c.v, c.v), (s, c.ty)
            gen.encode(prog, c.ty, c.v)
            gen.emit_test_module(prog)
            gen.emit_app(prog)
    print("unit checks passed")


def cmd_selftest(args):
    unit_checks()
    rc = cmd_check(argparse.Namespace(dirs=args.dirs, configs=args.configs))
    f = cmd_fuzz(argparse.Namespace(n=2, seed=900000, cases=3, configs=args.configs, keep=False, no_reduce=True))
    return rc or f


def cmd_gen(args):
    prog = gen.generate(args.seed, args.cases)
    text, _ = gen.emit_test_module(prog)
    print(text)
    print("# expected app output:")
    print("\n".join(gen.expected_app_lines(prog)))


def cmd_reduce(args):
    configs = args.configs.split(",") if args.configs else DEFAULT
    prog = gen.generate(args.seed, args.cases)
    d = os.path.join(out_dir(), "fuzz", str(args.seed))
    res = judge(prog, d, configs)
    if not bad(res):
        print("no disagreement")
        return 0
    m = reduce(prog, d, configs, res)
    print(f"reduced to {len(m.cases)} case(s), first: {gen.render_type(m.cases[0].ty)} in {d}/min")
    return 0


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("fuzz")
    f.add_argument("n", type=int, nargs="?", default=10)
    f.add_argument("--seed", type=int, default=1)
    f.add_argument("--cases", type=int, default=5)
    f.add_argument("--configs")
    f.add_argument("--keep", action="store_true")
    f.add_argument("--no-reduce", action="store_true")
    c = sub.add_parser("check")
    c.add_argument("dirs", nargs="+")
    c.add_argument("--configs")
    s = sub.add_parser("selftest")
    s.add_argument("dirs", nargs="*")
    s.add_argument("--configs")
    g = sub.add_parser("gen")
    g.add_argument("seed", type=int)
    g.add_argument("--cases", type=int, default=5)
    r = sub.add_parser("reduce")
    r.add_argument("seed", type=int)
    r.add_argument("--cases", type=int, default=5)
    r.add_argument("--configs")
    a = ap.parse_args()
    fn = {"fuzz": cmd_fuzz, "check": cmd_check, "selftest": cmd_selftest, "gen": cmd_gen, "reduce": cmd_reduce}
    sys.exit(fn[a.cmd](a) or 0)


if __name__ == "__main__":
    main()
