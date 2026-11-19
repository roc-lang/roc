"""Cost metrics from the prototype (counts, not timings).

    python3 bench.py [count]

Over generated programs run with every suspending call paused:
  - status checks per suspension site executed (.lss) and what .boxy adds;
  - records per pause, values saved per record, resume entries;
  - drop entries run by cancellation.
And, for the RC analyses (rc-atomicity/proto), where the remaining false
atomics of forward_pts come from: host equality classes or spawn seeds.

Timings are in bench/unwind.c, compiled optimized.
"""

from __future__ import annotations

import os
import sys
from collections import Counter

from check import CancelAtPolicy
from gen import gen_program
from host import PausePolicy, ReadyPolicy, run_tasks
from pipeline import build

sys.setrecursionlimit(100000)
HERE = os.path.dirname(os.path.abspath(__file__))


def lowering_costs(count):
    tot = Counter()
    for seed in range(1, count + 1):
        procs, _ = gen_program(seed)
        _, lows = build(procs)
        for mode in ("lss", "boxy"):
            r = run_tasks(lows[mode], "unwind", PausePolicy())
            for k, v in r.engine.items():
                if k == "max_chain":
                    tot[f"{mode} {k}"] = max(tot[f"{mode} {k}"], v)
                else:
                    tot[f"{mode} {k}"] += v
            q = run_tasks(lows[mode], "unwind", ReadyPolicy())
            tot[f"{mode} ready checks"] += q.engine["checks"]
            tot[f"{mode} ready records"] += q.engine["records"]
        r = run_tasks(lows["lss"], "unwind", CancelAtPolicy(2))
        tot["cancel drop entries"] += r.engine["drop_entries"]
        tot["cancel runs"] += 1
        for f in lows["lss"].may:
            tot["procs"] += 1
            tot["procs may suspend (lss)"] += lows["lss"].may[f]
            tot["procs may suspend (boxy)"] += lows["boxy"].may[f]
        tot["sites lss"] += len(lows["lss"].sites)
        tot["sites boxy"] += len(lows["boxy"].sites)
        tot["call-like statements"] += len(lows["lss"].calls)
    return tot


def rc_breakdown(count, seed):
    sys.path.insert(0, os.path.join(HERE, "..", "rc-atomicity", "proto"))
    import random

    from fuzz import gen_program as rc_gen
    from rcproto import ForwardPts, Machine, alias_classes, host_seed_locals, racy_events
    rng = random.Random(seed)
    out = Counter()
    for _ in range(count):
        prog = rc_gen(rng)
        an = ForwardPts(prog)
        m = Machine(prog, an)
        m.run()
        racy = racy_events(m.events)
        hosts = host_seed_locals(prog, alias_classes(prog))
        for i, e in enumerate(m.events):
            if e.local == ("host", "host") or i in racy or not an.atomic(e.ctx, e.local, e.path):
                continue
            out["false atomic"] += 1
            out["of which host classes" if e.local in hosts else "of which spawn seeds"] += 1
    return out


def main(argv):
    count = int(argv[1]) if len(argv) > 1 else 300
    t = lowering_costs(count)
    print(f"lowering, {count} generated programs, every suspending call paused")
    print(f"  procedures that may suspend: lss {t['procs may suspend (lss)']}/{t['procs']}, "
          f"boxy {t['procs may suspend (boxy)']}/{t['procs']}")
    print(f"  suspension sites: lss {t['sites lss']}, boxy {t['sites boxy']} "
          f"(of {t['call-like statements']} call-like statements)")
    for mode in ("lss", "boxy"):
        rec = t[f"{mode} records"]
        print(f"  {mode}: checks {t[f'{mode} checks']}, records {rec}, values saved {t[f'{mode} saved_values']} "
              f"({t[f'{mode} saved_values'] / max(rec, 1):.2f} per record), resume entries "
              f"{t[f'{mode} resume_entries']}, longest chain {t[f'{mode} max_chain']}")
    print(f"  boxy adds {100 * (t['boxy checks'] - t['lss checks']) / t['lss checks']:.1f}% checks")
    print(f"  frame-replacing calls through closures: lss {t['lss tail_calls'] - t['boxy tail_calls']}"
          f" direct, boxy {t['boxy deferred_calls']} deferred and {t['boxy driven_calls']} driven")
    print(f"  when nothing pauses: lss {t['lss ready checks']} checks and {t['lss ready records']} records "
          f"(records only at combinator waits)")
    print(f"  cancellation after the second pause: {t['cancel drop entries']} drop entries over "
          f"{t['cancel runs']} runs")
    b = rc_breakdown(3000, 2)
    print("\nRC: forward_pts false atomics, 3000 programs (seed 2)")
    for k in ("false atomic", "of which host classes", "of which spawn seeds"):
        print(f"  {k:<24} {b[k]}")


if __name__ == "__main__":
    sys.exit(main(sys.argv))
