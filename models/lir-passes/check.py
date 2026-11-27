"""Per-pass semantic preservation checker for ROC_LIR_PASS_DUMP files.

usage: python3 -I check.py DUMP [--rand N] [--seed S] [--all-stages] [--verbose]

Prints one FINDING per problem and a STATS line; exits 1 when there are findings.

For every lowering run in the dump:
  1. whole-program: every zero-argument procedure (test roots, constants) is run on each program
     snapshot; consecutive snapshots must agree (localizes to the stage between them).
  2. per-phase: for every phase block (one procedure before/after one procedure-local phase), the
     program of the preceding snapshot with that procedure replaced by `pre` and by `post` is run
     on the procedure's harvested inputs (arguments it was called with while running the roots)
     and on N random inputs generated from its argument layouts. pre defined => post must give the
     same result, crash and effects, must not hit undefined behaviour, and must not recurse deeper.
     Whole-program steps (return_slot, str_append, tag_reachability; every step with
     --all-stages) are checked the same way, per changed procedure.
  3. structural: tail-call sites (`tail=1`) must be consumed by TRMC; a self call left in tail
     position in the final snapshot is reported as a lost tail call.
Every input is run twice, with arguments unique and with arguments shared (`Machine.unique`).
"""
import argparse
import os
import random
import sys
import zlib

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import lir  # noqa: E402
from lir import (Machine, Layouts, Undefined, Unsupported, Crash, OutOfFuel, ExpectErr,  # noqa: E402
                 UnboundLocal, canon, clone, LList, Tag, Cell, INT_BITS, ZST)

FUEL = 300_000


class Gen:
    """Random values of a layout."""

    INTS = [0, 1, 2, 3, 5, 7, 10, 42, 100, -1, -2, -100]
    STRS = [b"", b"a", b"ab", b"hello", b"a-b--c", b"x y", b"0", b"12", b"abc/def", b"  pad  "]

    def __init__(self, layouts, rng):
        self.L = layouts
        self.rng = rng

    def value(self, idx, depth=3):
        L = self.L
        l = L.get(idx)
        k = l[0]
        r = self.rng
        if idx == 0:
            return r.randrange(2)
        if k == "int":
            n = r.choice(self.INTS + [r.randrange(-1000, 1000)])
            bits, signed = INT_BITS[l[1]]
            if r.random() < 0.05:
                n = (1 << (bits - (1 if signed else 0))) - 1
            return lir.wrap(n, l[1])
        if k == "frac":
            if l[1] == "dec":
                return r.choice(self.INTS) * lir.DEC_ONE
            return float(r.choice(self.INTS)) / r.choice([1, 2, 4])
        if k == "str":
            return r.choice(self.STRS)
        if k in ("list", "list_of_zst"):
            n = r.randrange(0, 4 if depth > 0 else 1)
            el = l[1] if k == "list" else ZST
            items = [self.value(el, depth - 1) for _ in range(n)]
            return LList(items, len(items) + r.choice([0, 0, 1, 3]))
        if k == "struct":
            return tuple(self.value(f, depth) for f in l[1])
        if k == "tag_union":
            variants = l[1]
            if depth <= 0:
                # prefer the variant with the shallowest payload
                order = sorted(range(len(variants)), key=lambda v: self.weight(variants[v]))
                v = order[0]
            else:
                v = r.randrange(len(variants))
            pl = variants[v]
            payload = None if L.get(pl)[0] == "zst" else self.value(pl, depth - 1)
            return Tag(v, payload)
        if k == "box":
            return Cell(self.value(l[1], depth - 1))
        if k == "box_of_zst":
            return Cell(())
        if k == "zst":
            return ()
        raise Unsupported("generate " + k)

    def weight(self, idx, seen=None):
        seen = seen or set()
        if idx in seen:
            return 100
        seen = seen | {idx}
        l = self.L.get(idx)
        if l[0] in ("zst", "int", "frac", "str"):
            return 0
        if l[0] in ("box", "list", "ptr"):
            return 1 + self.weight(l[1], seen)
        if l[0] == "struct":
            return 1 + sum(self.weight(f, seen) for f in l[1])
        if l[0] == "tag_union":
            return 1 + min(self.weight(v, seen) for v in l[1])
        return 50


def hosted_oracle(layouts):
    """Hosted calls return a value chosen only by the callee's name and the call's position in the
    effect trace, so pre and post see the same answers when their traces agree."""
    def h(name, args, ret, idx):
        rng = random.Random(zlib.crc32(("%s#%d" % (name, idx)).encode()))
        return Gen(layouts, rng).value(ret, 2)
    return h


def run(procs, layouts, pid, args, unique, fuel=FUEL, harvest=None, statics=None):
    m = Machine(procs, layouts, fuel=fuel, unique=unique, hosted=hosted_oracle(layouts), statics=statics)
    if harvest is not None:
        m.call_log = harvest
        m.call_log_limit = 400
    status = "ok"
    try:
        v = m.call(pid, [clone(a, shared=not unique) for a in args])
        res = ("ret", canon(v))
    except Crash as c:
        res = ("crash", c.msg)
    except ExpectErr as e:
        res = ("expect_err", e.msg)
    except Undefined as e:
        status, res = "undef", str(e)
    except Unsupported as e:
        status, res = "unsup", str(e)
    except OutOfFuel as e:
        status, res = "fuel", str(e)
    except UnboundLocal as e:
        status, res = "unbound", str(e)
    except RecursionError:
        status, res = "fuel", "python recursion"
    except (ValueError, TypeError, KeyError, IndexError, AttributeError) as e:
        # a model error: count it as unsupported and record where
        import traceback
        tb = traceback.extract_tb(e.__traceback__)[-1]
        status, res = "unsup", "model-error %s:%d %s" % (os.path.basename(tb.filename), tb.lineno, type(e).__name__)
    return status, res, tuple(m.effects), m.steps, m.max_depth_seen


class Report:
    def __init__(self, verbose=False):
        self.findings = []
        self.stats = {}
        self.verbose = verbose
        self.all_stages = False

    def bump(self, key, n=1):
        self.stats[key] = self.stats.get(key, 0) + n

    def find(self, kind, where, detail):
        self.findings.append((kind, where, detail))


def compare(rep, where, procs_pre, procs_post, layouts, pid, args, statics=None):
    for unique in (True, False):
        s1, r1, e1, st1, d1 = run(procs_pre, layouts, pid, args, unique, statics=statics)
        if s1 != "ok":
            rep.bump("pre_" + s1)
            if s1 == "unsup" and rep.verbose:
                rep.bump("unsup:" + r1.split(" ")[0] + " " + (r1.split(" ")[1] if " " in r1 else ""))
            continue
        s2, r2, e2, st2, d2 = run(procs_post, layouts, pid, args, unique, fuel=max(20 * st1, 100_000), statics=statics)
        if s2 == "unsup":
            rep.bump("post_unsup")
            continue
        rep.bump("compared")
        if s2 != "ok":
            rep.find("post-" + s2, where, "pid=%d unique=%s args=%s pre=%s post=%s" % (
                pid, unique, _short(args), _short(r1), r2))
            return
        if (r1, e1) != (r2, e2):
            rep.find("divergence", where, "pid=%d unique=%s args=%s\n    pre : %s %s\n    post: %s %s" % (
                pid, unique, _short(args), _short(r1), _short(e1), _short(r2), _short(e2)))
            return
        if d2 > d1 + 1 and d2 > 8:
            rep.find("deeper-recursion", where, "pid=%d pre depth %d, post depth %d args=%s" % (pid, d1, d2, _short(args)))


def _short(x, n=300):
    s = repr(canon_safe(x))
    return s if len(s) <= n else s[:n] + "..."


def canon_safe(x):
    try:
        if isinstance(x, (list, tuple)):
            return tuple(canon_safe(y) for y in x)
        return canon(x)
    except Exception:
        return x


def self_tail_sites(proc):
    return [sid for sid, s in proc.stmts.items() if s[0] == "call" and s[2] == proc.id and s[5]]


def self_calls_in_tail_position(proc):
    """Self calls whose result reaches `ret` through aliases, join-parameter writes and jumps only."""
    join_body = {}
    for s in proc.stmts.values():
        if s[0] == "join":
            join_body[s[1]] = s[3]
    out = []
    for sid, s in proc.stmts.items():
        if s[0] != "call" or s[2] != proc.id:
            continue
        val, cur, seen = s[1], s[4], set()
        tail = False
        while cur not in seen:
            seen.add(cur)
            st = proc.stmts.get(cur)
            if st is None:
                break
            if st[0] == "ret":
                tail = st[1] == val
                break
            if st[0] == "ref" and st[2] in ("local", "nominal") and st[3] == val:
                val, cur = st[1], st[7]
            elif st[0] == "set" and st[2] == val:
                val, cur = st[1], st[4]
            elif st[0] == "jump":
                cur = join_body.get(st[1])
            else:
                break
        if tail:
            out.append(sid)
    return out


WHOLE_PROGRAM_STAGES = ("after_return_slot", "after_str_append", "after_tag_reachability")


def check_proc(rep, where, pre_procs, post_procs, layouts, pid, pre, harvested, nrand, rng, statics):
    inputs = list(harvested)
    g = Gen(layouts, rng)
    for _ in range(nrand):
        try:
            inputs.append(tuple(g.value(pre.locals[a]) for a in pre.args))
        except Unsupported:
            rep.bump("gen_unsup")
            break
    if not inputs:
        rep.bump("no_inputs")
    n_before = len(rep.findings)
    for args in inputs:
        compare(rep, where, pre_procs, post_procs, layouts, pid, args, statics)
        if len(rep.findings) > n_before:
            break


def check_run(rep, run_id, blocks, nrand, rng):
    programs = [b for b in blocks if b.kind == "program"]
    phases = [b for b in blocks if b.kind == "phase"]
    if not programs:
        return
    table = {}
    for b in blocks:
        table.update(b.layouts)
    layouts = Layouts(table)
    by_stage = {b.stage: b for b in programs}

    # 1. whole-program snapshots + input harvesting
    first = programs[0]
    roots = [pid for pid, p in first.procs.items() if not p.args and p.body is not None and not p.static_init]
    harvested = {}  # stage -> pid -> [args]
    results = {}
    for b in programs:
        h = {}
        res = {}
        for pid in roots:
            if pid not in b.procs:
                continue
            log = []
            s, r, e, st, d = run(b.procs, layouts, pid, [], True, harvest=log, statics=b.statics)
            res[pid] = (s, r, e)
            for cp, a in log:
                h.setdefault(cp, [])
                if len(h[cp]) < 12:
                    h[cp].append(a)
        harvested[b.stage] = h
        results[b.stage] = res
    for prev, cur in zip(programs, programs[1:]):
        for pid in roots:
            a = results[prev.stage].get(pid)
            c = results[cur.stage].get(pid)
            if a is None or c is None:
                continue
            if a[0] != "ok":
                rep.bump("root_pre_" + a[0])
                if rep.verbose and a[0] in ("unsup", "undef", "unbound"):
                    rep.bump("root%s:%s" % (a[0], str(a[1])[:60]))
                continue
            if c[0] == "unsup":
                continue
            rep.bump("root_compared")
            if a != c:
                rep.find("program-divergence", "run %d %s -> %s" % (run_id, prev.label, cur.label),
                         "root p%d (%s)\n    before: %s\n    after : %s" % (pid, first.procs[pid].name, _short(a), _short(c)))

    # 2a. per-procedure across whole-program steps (return_slot, str_append, tag_reachability) and
    #     as a cross-check of the per-phase dumps: every procedure whose text changed between two
    #     consecutive snapshots, in the context of the earlier snapshot.
    for prev, cur in zip(programs, programs[1:]):
        if cur.label not in WHOLE_PROGRAM_STAGES and not rep.all_stages:
            continue
        for pid, p_new in cur.procs.items():
            p_old = prev.procs.get(pid)
            if p_old is None or p_old.body is None or p_new.body is None or p_old.text == p_new.text:
                continue
            rep.bump("step:" + cur.label)
            post_procs = dict(prev.procs)
            for q, pq in cur.procs.items():
                if q not in prev.procs:
                    post_procs[q] = pq  # procedures the step created (e.g. str_append variants)
            post_procs[pid] = p_new
            where = "run %d step %s -> %s proc p%d (%s)" % (run_id, prev.label, cur.label, pid, p_old.name)
            check_proc(rep, where, prev.procs, post_procs, layouts, pid, p_old,
                       harvested.get(prev.stage, {}).get(pid, []), nrand, rng, prev.statics)

    # 2b. per-phase
    for ph in phases:
        base = by_stage.get(ph.stage)
        if base is None:
            continue
        pid = ph.pre.id
        rep.bump("phase:" + ph.label)
        pre_procs = dict(base.procs)
        pre_procs[pid] = ph.pre
        post_procs = dict(base.procs)
        post_procs[pid] = ph.post
        where = "run %d stage %d phase %s proc p%d (%s)" % (run_id, ph.stage, ph.label, pid, ph.pre.name)
        check_proc(rep, where, pre_procs, post_procs, layouts, pid, ph.pre,
                   harvested.get(ph.stage, {}).get(pid, []), nrand, rng, base.statics)
        # 3a. structural: TRMC consumes every self-tail site
        if ph.label == "trmc":
            if self_tail_sites(ph.pre) and self_tail_sites(ph.post):
                rep.find("tail-site-left", where, "tail=1 self calls left after trmc: %s" % self_tail_sites(ph.post))

    # 3b. structural: final snapshot has no self call in tail position
    last = programs[-1]
    for pid, p in last.procs.items():
        if p.body is None:
            continue
        sites = self_calls_in_tail_position(p)
        if sites:
            lowered = first.procs.get(pid)
            was = self_tail_sites(lowered) if lowered else []
            if not was:
                # not a producer-proven tail site; ARC may still make it frame-replacing
                rep.bump("untagged_tail_self_call")
                continue
            rep.find("lost-tail-call", "run %d final %s p%d (%s)" % (run_id, last.label, pid, p.name),
                     "self call in tail position at stmts %s; lowered tail sites %s" % (sites, was))


def check_text(text, nrand=6, seed=1, all_stages=False, verbose=False):
    rep = Report(verbose)
    rep.all_stages = all_stages
    runs = {}
    for b in lir.parse_dump(text):
        runs.setdefault(b.run, []).append(b)
    rng = random.Random(seed)
    for run_id, bl in sorted(runs.items()):
        check_run(rep, run_id, bl, nrand, rng)
    return rep


def main():
    ap = argparse.ArgumentParser(description="Check a ROC_LIR_PASS_DUMP file pass by pass.")
    ap.add_argument("dump")
    ap.add_argument("--rand", type=int, default=6, help="random inputs per procedure (default 6)")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--all-stages", action="store_true",
                    help="also check every program step per procedure, not only the whole-program passes")
    ap.add_argument("--verbose", action="store_true", help="break unsupported inputs down by reason in STATS")
    a = ap.parse_args()
    with open(a.dump, errors="replace") as f:
        text = f.read()
    rep = check_text(text, a.rand, a.seed, a.all_stages, a.verbose)
    for kind, where, detail in rep.findings:
        print("FINDING %s @ %s\n    %s" % (kind, where, detail))
    print("STATS " + " ".join("%s=%d" % kv for kv in sorted(rep.stats.items())))
    return 1 if rep.findings else 0


if __name__ == "__main__":
    sys.exit(main())
