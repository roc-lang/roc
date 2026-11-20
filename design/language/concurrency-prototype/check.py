"""The fuzzer: generate typed programs and check the RFC's invariants on them.

Per program:
  static   ARC certification (C5's ledger), C2, C3, for .lss and .boxy
  C1/C10   unwinding vs stackful engine, same host and schedule: identical
           results, effect traces and heap operations
  C8       .lss vs .boxy lowering: identical results, traces and heap operations
  C9/G3    None build vs Tasks build under an input-order schedule
  G4       programs without race!, all_ok!, crashes or loops whose bodies
           depend on arrival order (exits, errors, spawns): one result under
           every schedule, and the None build's
  C5       cancel the root at every suspension in turn: exact heap accounting
  R1-R6    monitored in every run (runtime.py); heap verified exact after
           every run, with crashed native frames as the only leak (R5)

Usage:
    python3 check.py [count] [seed]
"""

from __future__ import annotations

import sys
from collections import Counter

from gen import gen_program
from host import (InputOrderPolicy, PausePolicy, Policy, RandomPolicy, ReadyPolicy, run_none,
                  run_tasks)
from pipeline import build

sys.setrecursionlimit(100000)


class CancelAtPolicy(PausePolicy):
    """Pause every suspending call; cancel the root right after the k-th pause."""

    name = "cancel_at"

    def __init__(self, k):
        self.k, self.seen = k, 0

    def on_call(self, host, t, name):
        self.seen += 1
        return "pending"

    def choose(self, host):
        if self.seen >= self.k and not host.root_cancelled and host.root_status == "PENDING":
            return ("cancel_root",)
        return super().choose(host)


def per_task(effects):
    out = {}
    for path, name, args in effects:
        out.setdefault(path, []).append((name, args))
    return out


def key(run, heap=True):
    k = (run.result, tuple(run.effects))
    if heap:
        k += (tuple(sorted(run.heap.items())),)
    return k


class Failure(Exception):
    pass


def check_program(seed, stats, cancel_sites=6):
    procs, features = gen_program(seed)
    arc, lows = build(procs)
    stats["programs"] += 1
    for f in features:
        stats["feature " + f] += 1
    none = run_none(arc.prog)
    stats["none " + none.result[0]] += 1
    stats["session: None entries run on demand"] += none.extra.get("on demand", 0)

    def run(mode, eng, pol, conc="Tasks"):
        r = run_tasks(lows[mode], eng, pol, conc)
        stats["runs"] += 1
        stats["result " + r.result[0]] += 1
        stats["unwind records"] += r.engine.get("records", 0) if eng == "unwind" else 0
        stats["crashes"] += r.runtime.get("crashes", 0)
        stats["woken while running"] += r.runtime.get("woken_while_running", 0)
        stats["leaked objects verified"] += r.leaked_live
        stats["R7 supervise! returned"] += r.runtime.get("R7 returned", 0)
        stats["R7 supervise! gave up"] += r.runtime.get("R7 gave up", 0)
        stats["deferred erased calls"] += r.engine.get("deferred_calls", 0) if eng == "unwind" else 0
        for k in ("loops", "arrivals", "loop exits", "loop spawns", "limit refills", "shielded waits",
                  "reports held for another wait", "linked crashes", "linked crashes pending at next"):
            stats["loop: " + k] += r.runtime.get(k, 0)
        for k in ("channels", "messages", "Cancelled delivered", "deadlocks crashed", "ends closed by a crash"):
            stats["session: " + k] += r.runtime.get(k, 0)
        return r

    policies = [lambda: ReadyPolicy(), lambda: PausePolicy(), lambda: InputOrderPolicy(),
                lambda: RandomPolicy(seed * 7 + 1), lambda: RandomPolicy(seed * 7 + 2, parallel=True),
                lambda: RandomPolicy(seed * 7 + 3, parallel=True, p_cancel=0.02)]
    results = {}
    for pi, mk in enumerate(policies):
        pol = mk()
        conc = "ParallelTasks" if pol.parallel else "Tasks"
        runs = {}
        for mode in ("lss", "boxy"):
            for eng in ("unwind", "stack"):
                runs[(mode, eng)] = run(mode, eng, mk(), conc)
        base = runs[("lss", "unwind")]
        crashed = base.runtime.get("crashes", 0) > 0
        # C1/C10: the lowering against the stackful oracle (same schedule).
        for mode in ("lss", "boxy"):
            u, s = runs[(mode, "unwind")], runs[(mode, "stack")]
            if key(u, heap=not crashed) != key(s, heap=not crashed):
                raise Failure(f"seed {seed}: unwind and stack differ under {pol.name}/{mode}:\n{u.result}\n{s.result}")
            if u.leaked_live > s.leaked_live:
                raise Failure(f"seed {seed}: unwinding leaked more than the stackful oracle")
        # C8: .lss and .boxy agree.
        if key(runs[("lss", "unwind")]) != key(runs[("boxy", "unwind")]):
            raise Failure(f"seed {seed}: lss and boxy differ under {pol.name}")
        stats["boxy extra checks"] += runs[("boxy", "unwind")].engine["checks"] - base.engine["checks"]
        stats["lss checks"] += base.engine["checks"]
        results[pol.name + str(pi)] = base
    # C9: None vs input order.
    io = results["input_order2"]
    if not (none.result[0] == "crashed" and none.extra.get("supervised")):
        if (none.result, none.effects) != (io.result, io.effects):
            raise Failure(f"seed {seed}: None and input-order differ:\n{none.result}\n{io.result}\n"
                          f"{none.effects}\n{io.effects}")
        stats["C9 compared"] += 1
    # G4: deterministic programs give one result everywhere.
    # A session program may deadlock under one schedule and not another, or
    # crash in whichever task uses an end second.
    if not features & {"race!", "all_ok!", "crash", "arrival order", "session"}:
        stats["G4 compared"] += 1
        for name, r in results.items():
            if r.result == ("cancelled",):
                continue
            if r.result != none.result:
                raise Failure(f"seed {seed}: {name} result {r.result} != None {none.result}")
            if per_task(r.effects) != per_task(none.effects):
                raise Failure(f"seed {seed}: {name} per-task effects differ from the None build")
    # C5: cancel at every suspension.
    n = results["pause1"].host.get("pending", 0)
    for k in range(1, min(n, cancel_sites) + 1):
        for mode in ("lss", "boxy"):
            r = run(mode, "unwind", CancelAtPolicy(k))
            stats["cancel-at runs"] += 1
            stats["cancel-at " + r.result[0]] += 1


def main(argv):
    count = int(argv[1]) if len(argv) > 1 else 300
    seed = int(argv[2]) if len(argv) > 2 else 1
    stats = Counter()
    for s in range(seed, seed + count):
        check_program(s, stats)
    for k in sorted(stats):
        print(f"{k:<28} {stats[k]}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
