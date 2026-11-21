"""The fuzzer: generate typed programs and check the RFC's invariants on them.

Per program:
  static   ARC certification (C5's ledger), C2, C3, for .lss and .boxy
  C1/C10   unwinding vs stackful engine, same host and schedule: identical
           results, effect traces and heap operations
  C8       .lss vs .boxy lowering: identical results and traces, and identical
           heap operations in runs without a crash
  C9/G3    None build vs Tasks build under its schedule: for a program that
           can wait, the coroutine schedule (one thread, hosted calls that
           finish in place, the ready task first in the task tree); for one
           that cannot, the input-order schedule
  G4       programs without race!, all_ok!, crashes or loops whose bodies
           depend on arrival order (exits, errors, spawns): one result under
           every schedule, and the None build's
  C5       cancel the root at every suspension in turn: exact heap accounting,
           and deferred steps of shared variables run exactly once (V5)
  None     the None build as coroutines (the unwinding lowering with hosted
           calls that never suspend, under CoroutinePolicy), which is the
           None build of a program that can wait: engines and lowerings
           agree; for a program that cannot wait, it gives the plain stack's
           results and effects
  R1-R6    monitored in every run (runtime.py); heap verified exact after
           every run, with crashed native frames as the only leak (R5)

Usage:
    python3 check.py [count] [seed] [links]

With `links`, every program uses shared state, and most use cells whose
states hold handles and jobs, which form cycles the runtime must break (V9).
"""

from __future__ import annotations

import sys
from collections import Counter

from gen import gen_program
from host import (NONE_HOSTED, CoroutinePolicy, InputOrderPolicy, PausePolicy, Policy, RandomPolicy, ReadyPolicy,
                  run_none_build, run_tasks)
from lower import Lowering
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


def seq_name(can):
    return "coroutine" if can else "input-order"


def check_program(seed, stats, cancel_sites=6, links=False):
    procs, features = gen_program(seed, links=links)
    arc, lows = build(procs)
    stats["programs"] += 1
    for f in features:
        stats["feature " + f] += 1
    none = run_none_build(arc)
    can = none.extra["engine"] == "coroutine"
    stats["None build: " + ("coroutines" if can else "one stack")] += 1
    stats["none " + none.result[0]] += 1
    # On one stack every cell is released at the end; as coroutines, when the
    # last task holding it finishes, as in a Tasks build.
    stats["None: cells released"] += none.runtime.get("cells released", 0) if can else none.extra.get(
        "cells released", 0)
    stats["session: None entries run on demand"] += none.extra.get("on demand", 0)

    def run(mode, eng, pol, conc="Tasks"):
        r = run_tasks(lows[mode], eng, pol, conc)
        stats["runs"] += 1
        stats["result " + r.result[0]] += 1
        stats["unwind records"] += r.engine.get("records", 0) if eng == "unwind" else 0
        stats["crashes"] += r.runtime.get("crashes", 0)
        stats["woken while running"] += r.runtime.get("woken_while_running", 0)
        stats["leaked objects verified"] += r.leaked_live
        for k in ("cells released", "cells released when a child task finished",
                  "cells released when the root finished", "cells released at the end of the run"):
            stats[k] += r.runtime.get(k, 0)
        stats["R7 supervise! returned"] += r.runtime.get("R7 returned", 0)
        stats["R7 supervise! gave up"] += r.runtime.get("R7 gave up", 0)
        stats["deferred erased calls"] += r.engine.get("deferred_calls", 0) if eng == "unwind" else 0
        for k in ("loops", "arrivals", "loop exits", "loop spawns", "loop spawns under the root limit",
                  "loops ended with background entries left", "background entries cancelled at the end",
                  "background entries discarded at the end", "background reports dropped at the end",
                  "limit refills", "shielded waits",
                  "reports held for another wait", "linked crashes",
                  "linked crashes pending at a suspension"):
            stats["loop: " + k] += r.runtime.get(k, 0)
        for k in ("channels", "messages", "Cancelled delivered", "deadlocks crashed",
                  "ports released after a crash", "receives abandoned", "mailboxes", "posts", "Closed delivered",
                  "semaphore cells", "permits", "permits returned"):
            stats["session: " + k] += r.runtime.get(k, 0)
        for k in ("shared variables", "steps run", "commits", "waits", "steps run again", "deferred steps",
                  "deferred steps run", "deferred steps run at drop", "errdefers dropped", "deferred steps crashed",
                  "cells poisoned", "cells poisoned by an update", "poisoned steps",
                  "cancellations turned into crashes", "steps crashed", "updates crashed",
                  "probes", "probes that would wait", "probes confirmed by the update", "wakes passed on at a drop"):
            stats["shared: " + k] += r.runtime.get(k, 0)
        return r

    # The third is the None build's schedule.
    policies = [lambda: ReadyPolicy(), lambda: PausePolicy(), (lambda: CoroutinePolicy()) if can else
                (lambda: InputOrderPolicy()),
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
        # C8: .lss and .boxy agree. A crash leaks the native frames live at it
        # (R5), and those differ by design: a call .boxy defers runs under its
        # driver's native frame, where .lss keeps the caller as a record that
        # the crash drops. Both heaps are still verified exact.
        if key(runs[("lss", "unwind")], heap=not crashed) != key(runs[("boxy", "unwind")], heap=not crashed):
            raise Failure(f"seed {seed}: lss and boxy differ under {pol.name}")
        stats["boxy extra checks"] += runs[("boxy", "unwind")].engine["checks"] - base.engine["checks"]
        stats["lss checks"] += base.engine["checks"]
        results[pol.name + str(pi)] = base
    # C9: None vs input order.
    seq = results["coroutine2" if can else "input_order2"]
    if not (none.result[0] == "crashed" and none.extra.get("supervised")):
        if (none.result, none.effects) != (seq.result, seq.effects):
            raise Failure(f"seed {seed}: the None build and the {seq_name(can)} schedule differ:\n"
                          f"{none.result}\n{seq.result}\n{none.effects}\n{seq.effects}")
        stats["C9 compared"] += 1
    # G4: deterministic programs give one result everywhere.
    # A session program may deadlock under one schedule and not another, or
    # crash in whichever task uses an end second.
    if not features & {"race!", "all_ok!", "crash", "arrival order", "session", "shared"}:
        stats["G4 compared"] += 1
        for name, r in results.items():
            if r.result == ("cancelled",):
                continue
            if r.result != none.result:
                raise Failure(f"seed {seed}: {name} result {r.result} != None {none.result}")
            if per_task(r.effects) != per_task(none.effects):
                raise Failure(f"seed {seed}: {name} per-task effects differ from the None build")
    # The None build as coroutines, in both lowerings and engines. It is the
    # None build of a program that can wait; one that cannot runs on the
    # plain stack, and the two must agree.
    co = {}
    for mode in ("lss", "boxy"):
        low = Lowering(arc, NONE_HOSTED, mode)
        low.certify()
        if "session" in features or "mailbox" in features or "semaphore cell" in features:
            stats["coroutine: user procedures that may suspend, port programs"] += sum(
                low.may[f] for f in procs if mode == "lss")
            stats["coroutine: user procedures, port programs"] += len(procs) if mode == "lss" else 0
            stats["coroutine: user procedures that may suspend in a Tasks build, port programs"] += sum(
                lows["lss"].may[f] for f in procs if mode == "lss")
        for eng in ("unwind", "stack"):
            co[(mode, eng)] = r = run_tasks(low, eng, CoroutinePolicy(), none_build=True)
            stats["coroutine runs"] += 1
            stats["leaked objects verified"] += r.leaked_live
    c = co[("lss", "unwind")]
    crashed = c.result[0] == "crashed" or c.runtime.get("crashes", 0) > 0
    for mode in ("lss", "boxy"):
        if key(co[(mode, "unwind")], heap=not crashed) != key(co[(mode, "stack")], heap=not crashed):
            raise Failure(f"seed {seed}: coroutines: unwind and stack differ under {mode}")
    if key(c, heap=not crashed) != key(co[("boxy", "unwind")], heap=not crashed):  # as C8 above
        raise Failure(f"seed {seed}: coroutines: lss and boxy differ")
    stats["coroutine: status checks"] += c.engine["checks"]
    if not can:
        if (c.result, c.effects) != (none.result, none.effects):
            raise Failure(f"seed {seed}: a program that cannot wait gives different results as coroutines "
                          f"and on one stack:\n{c.result}\n{none.result}\n{c.effects}\n{none.effects}")
        stats["cannot wait: coroutines and one stack agree"] += 1
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
    links = len(argv) > 3 and argv[3] == "links"
    stats = Counter()
    for s in range(seed, seed + count):
        check_program(s, stats, links=links)
    for k in sorted(stats):
        print(f"{k:<28} {stats[k]}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
