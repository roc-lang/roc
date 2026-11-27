"""The selftest, check and fuzz commands of the error-recovery model. See README.md."""
import argparse, os, random, re, shutil, sys, time
from multiprocessing import Pool

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import gen, mutate, oracle

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.environ.get("OUT", os.path.join(HERE, "out"))


def mutant(seed, k, size):
    """The k-th mutant of seed's program: one to three typed mutations."""
    g, base = gen.generate(seed, size)
    prog = gen.clone(base)
    g.rng = random.Random(seed * 1000 + k)
    count = g.rng.choice([1, 1, 1, 2, 2, 3])
    mutate.mutate(g, prog, count)
    return g, base, prog


# ---- reduction --------------------------------------------------------------------------------------------------
def signature(labels, results):
    sig = set(l for l in labels if not l.startswith("unknown"))
    lines = set()
    for r in results:
        if r.status == "panic":
            lines.add(re.sub(r"\d+", "N", r.panic_line()))
    return frozenset(sig), frozenset(lines)


def reduce(g, prog, labels, results, d, budget=150):
    """Shrink a failing mutant while it keeps the same failure: drop expects, main lines and definitions, then
    replace subexpressions that hold no mutation with leaves of their type."""
    target = signature(labels, results)
    cmds = sorted(set(l.split(":", 1)[1] for l in labels if ":" in l)) or None
    if "disagree" in labels:
        cmds = None
    runs = [0]

    def still(p):
        if runs[0] >= budget:
            return False
        runs[0] += 1
        ls, rs = oracle.judge(gen.render(p), d, cmds and [c.replace("run-", "") for c in cmds], run=any(
            c.startswith("run") or "run-" in c for c in labels))
        oracle.clean_binaries(d)
        return signature(ls, rs) == target

    best = prog
    changed = True
    while changed and runs[0] < budget:
        changed = False
        for attr in ("expects", "main", "defs"):
            i = 0
            while i < len(getattr(best, attr)) and runs[0] < budget:
                cand = gen.clone(best)
                getattr(cand, attr).pop(i)
                if still(cand):
                    best, changed = cand, True
                else:
                    i += 1
        # replace mutation-free subtrees with leaves, outermost first
        cand_sites = []
        for s in mutate.sites(best):
            node = s[0]
            if node.kind in ("lit", "var") or any(n.mutated for n, _, _ in node.walk()) or node.ty[0] == "Oops":
                continue
            cand_sites.append(s)
        for idx in range(len(cand_sites)):
            if runs[0] >= budget:
                break
            cand = gen.clone(best)
            ss = [s for s in mutate.sites(cand) if s[0].kind not in ("lit", "var")
                  and not any(n.mutated for n, _, _ in s[0].walk()) and s[0].ty[0] != "Oops"]
            if idx >= len(ss):
                break
            node, par, slot, owner = ss[idx]
            leaf = g.leaf(node.ty, [])
            mutate.replace(cand, node, par, slot, owner, leaf)
            leaf.mutated = False
            if still(cand):
                best, changed = cand, True
    return best


# ---- fuzz -------------------------------------------------------------------------------------------------------
def fuzz_one(args):
    seed, variants, size, do_reduce, run_bins, commands = args
    d = os.path.join(OUT, "fuzz", "work", str(seed))
    rows = []
    g, base = gen.generate(seed, size)
    src = gen.render(base)
    ls, rs = oracle.judge(src, d, ["check"], run=False)
    if ls or rs[0].errors != 0:
        keep = os.path.join(OUT, "fuzz", "gen-errors", str(seed))
        os.makedirs(keep, exist_ok=True)
        with open(os.path.join(keep, "H.roc"), "w") as f:
            f.write(src)
        with open(os.path.join(keep, "log.txt"), "w") as f:
            f.write(rs[0].out)
        rows.append((seed, "base", "", oracle.summary(rs), "gen-error" if not ls else ",".join(ls)))
        shutil.rmtree(d, ignore_errors=True)
        return rows
    for k in range(variants):
        g, _, prog = mutant(seed, k, size)
        src = gen.render(prog)
        ls, rs = oracle.judge(src, d, commands, run=run_bins)
        accepted = any(r.cmd == "check" and r.status == "ok" and r.errors == 0 for r in rs)
        label = ",".join(ls) if ls else ("accepted" if accepted else "pass")
        if ls and oracle.known(rs):
            label += " known=" + ",".join(oracle.known(rs))
        rows.append((seed, k, "; ".join(prog.mutations), oracle.summary(rs), label))
        if ls:
            keep = os.path.join(OUT, "fuzz", "fail", "%d-%d" % (seed, k))
            os.makedirs(keep, exist_ok=True)
            with open(os.path.join(keep, "H.roc"), "w") as f:
                f.write("# mutations: %s\n# verdict: %s\n# %s\n" % ("; ".join(prog.mutations), label,
                                                                   oracle.summary(rs)) + src)
            with open(os.path.join(keep, "log.txt"), "w") as f:
                for r in rs:
                    out = r.out if len(r.out) <= 12000 else r.out[:4000] + "\n[...]\n" + r.out[-8000:]
                    f.write("==== %s status=%s errors=%d rc=%s\n%s\n" % (r.cmd, r.status, r.errors, r.rc, out))
            if do_reduce:
                small = reduce(g, prog, ls, rs, d)
                with open(os.path.join(keep, "reduced.roc"), "w") as f:
                    f.write("# verdict: %s\n" % label + gen.render(small))
        oracle.clean_binaries(d) if os.path.isdir(d) else None
    shutil.rmtree(d, ignore_errors=True)
    return rows


def cmd_fuzz(a):
    os.makedirs(os.path.join(OUT, "fuzz"), exist_ok=True)
    commands = a.commands.split(",") if a.commands else None
    for c in commands or []:
        if c not in oracle.COMMAND_NAMES:
            print("unknown command %s; one of %s" % (c, ", ".join(oracle.COMMAND_NAMES)))
            return 2
    jobs = [(s, a.variants, a.size, not a.no_reduce, not a.no_run, commands) for s in range(a.seed, a.seed + a.n)]
    t0 = time.time()
    counts = {}
    with open(os.path.join(OUT, "fuzz", "results.tsv"), "w") as tsv, Pool(a.j) as pool:
        tsv.write("seed\tvariant\tmutations\tresults\tverdict\n")
        for rows in pool.imap_unordered(fuzz_one, jobs):
            for row in rows:
                tsv.write("\t".join(str(x) for x in row) + "\n")
                tsv.flush()
                v = row[4]
                key = "fail" if v not in ("pass", "accepted", "gen-error") else v
                counts[key] = counts.get(key, 0) + 1
                if key == "fail":
                    print("FAIL seed %s variant %s: %s\n    %s\n    %s" % (row[0], row[1], row[4], row[2], row[3]), flush=True)
    print("fuzz: %d seeds, %s, %.0fs; failures in %s" % (a.n, ", ".join("%s %d" % kv for kv in sorted(counts.items())),
                                                       time.time() - t0, os.path.join(OUT, "fuzz", "fail")))
    return 1 if counts.get("fail") else 0


# ---- check ------------------------------------------------------------------------------------------------------
def expectation(src):
    m = re.search(r"^# expect: (.*)$", src, re.M)
    return m.group(1).strip() if m else None


def check_file(path, commands=None):
    src = open(path).read()
    d = os.path.join(OUT, "check", os.path.splitext(os.path.basename(path))[0])
    shutil.rmtree(d, ignore_errors=True)
    ls, rs = oracle.judge(src, d, commands)
    oracle.clean_binaries(d)
    return src, ls, rs


def cmd_check(a):
    bad = 0
    for path in a.files:
        src, ls, rs = check_file(path)
        print("%s: %s\n    %s" % (path, ",".join(ls) or "pass", oracle.summary(rs)))
        for r in rs:
            if r.status == "panic":
                print("    %s: %s" % (r.cmd, r.panic_line()))
        bad += bool(ls)
    return 1 if bad else 0


# ---- selftest ---------------------------------------------------------------------------------------------------
def cmd_selftest(a):
    # 1. the generator and the mutations, without the compiler
    kinds = {}
    for seed in range(300):
        for k in range(3):
            g, base, prog = mutant(seed, k, 1.0)
            gen.render(base)
            gen.render(prog)
            if not prog.mutations:
                print("selftest: seed %d variant %d applied no mutation" % (seed, k))
                return 1
            for m in prog.mutations:
                kinds[m.split(":")[0].split(" on ")[0][:40]] = 1
    print("selftest: generator and mutations: 300 seeds x 3 variants rendered, %d distinct mutation descriptions" % len(kinds))
    # 2. fixtures against the compiler
    fails = 0
    tests = os.path.join(HERE, "tests")
    for name in sorted(os.listdir(tests)):
        if not name.endswith(".roc"):
            continue
        src, ls, rs = check_file(os.path.join(tests, name))
        exp = expectation(src) or "pass"
        errs = any(r.status == "ok" and r.errors > 0 for r in rs)
        got = ",".join(ls) if ls else ("pass errors" if errs else "pass clean")
        ok = got.startswith(exp) if not exp.startswith("fail") else bool(ls) and exp.split(None, 1)[-1] in got
        print("selftest: %-28s %-4s expect %-20s got %s" % (name, "ok" if ok else "FAIL", exp, got))
        fails += not ok
    return 1 if fails else 0


def main():
    ap = argparse.ArgumentParser(prog="run.sh")
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("selftest")
    c = sub.add_parser("check")
    c.add_argument("files", nargs="+")
    f = sub.add_parser("fuzz")
    f.add_argument("n", nargs="?", type=int, default=100)
    f.add_argument("--seed", type=int, default=0)
    f.add_argument("-j", type=int, default=4)
    f.add_argument("--variants", type=int, default=2, help="mutants per program (default 2)")
    f.add_argument("--size", type=float, default=1.0, help="program size factor (default 1.0)")
    f.add_argument("--no-reduce", action="store_true", help="do not shrink failing programs")
    f.add_argument("--no-run", action="store_true", help="do not run the built executables")
    f.add_argument("--commands", help="comma-separated subset of: " + ", ".join(oracle.COMMAND_NAMES))
    a = ap.parse_args()
    sys.exit({"selftest": cmd_selftest, "check": cmd_check, "fuzz": cmd_fuzz}[a.cmd](a))


if __name__ == "__main__":
    main()
