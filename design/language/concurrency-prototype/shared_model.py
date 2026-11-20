"""The ParallelTasks protocol for atomic steps, explored over every interleaving
(concurrency-shared-variables.md, "Runtime"; invariants V2, V4 and V7).

Threads run steps over shared cells. Each action below is atomic; any thread
may take its next action at any time, so the explorer visits every
interleaving of lock operations, reads, writes, wakes and parks:

    lock      take the next lock of the step's footprint, in the global order
              (cell number); blocked while another thread holds it
    run       with every lock held, read the cells, evaluate the step, and
              either write its new values and take the waiters of the cells
              it wrote, or register as a waiter on every cell of its footprint
    unlock    release one lock
    wake      ready one waiter taken at the commit (counted: a wake that
              arrives before the waiter has parked is not lost)
    park      a step that waits parks, unless a wake arrived since it
              registered; it then runs again from its first lock

Scenarios are balanced, so every correct run finishes: a state where no thread
can act and some thread has not finished is a deadlock (V4) or a lost wakeup
(V7). Every finished run must end in the values a serial order of the steps
gives (V2).

Mutations, each of which the explorer must catch:
    unordered        each thread locks its footprint in its own order
    late_register    a step that waits releases its locks before registering
    no_locks         steps read and write without locks (read and write are
                     separate actions)

    python3 shared_model.py
"""

from __future__ import annotations

import sys


def take(c):
    return ((c,), lambda v: None if v[c] == 0 else {c: v[c] - 1})


def give(c):
    return ((c,), lambda v: {c: v[c] + 1})


def move(a, b):
    """Wait until a is nonzero, then move one from a to b."""
    return ((a, b), lambda v: None if v[a] == 0 else {a: v[a] - 1, b: v[b] + 1})


SCENARIOS = {
    # Three threads share one permit.
    "permit": ({0: 1}, [[take(0), give(0)], [take(0), give(0)], [take(0), give(0)]], {0: 1}),
    # A taker waits for a giver.
    "handoff": ({0: 0}, [[take(0)], [give(0)]], {0: 0}),
    # Two footprints of two cells, named in opposite orders.
    "transfer": ({0: 1, 1: 0}, [[move(0, 1)], [move(1, 0)], [take(0), give(0)]], {0: 1, 1: 0}),
    # Two waiters on one cell, and a step over both cells that feeds them.
    "fan": ({0: 0, 1: 2}, [[take(0)], [take(0)], [move(1, 0), move(1, 0)]], {0: 0, 1: 0}),
}


class Model:
    def __init__(self, init, progs, mutation=None):
        self.init, self.progs, self.mutation = init, progs, mutation

    def order(self, tid, fp):
        if self.mutation == "unordered" and tid % 2 == 1:
            return tuple(reversed(sorted(set(fp))))
        return tuple(sorted(set(fp)))

    def start(self):
        cells = tuple(sorted(self.init.items()))
        locks = tuple(None for _ in cells)
        waiters = tuple(frozenset() for _ in cells)
        # thread: (pc, phase, held, pending wakes, woken, snapshot)
        threads = tuple((0, "lock", (), (), False, None) for _ in self.progs)
        return (cells, locks, waiters, threads)

    def actions(self, s):
        cells, locks, waiters, threads = s
        out = []
        for tid, th in enumerate(threads):
            pc, phase, held, wakes, woken, snap = th
            prog = self.progs[tid]
            if phase == "done" or phase == "parked":
                continue
            fp, fn = prog[pc]
            order = self.order(tid, fp)
            values = dict(cells)
            if phase == "lock":
                if self.mutation == "no_locks":
                    out.append(self.with_thread(s, tid, (pc, "read", (), wakes, woken, None)))
                    continue
                nxt = order[len(held)]
                if locks[nxt] is None:
                    locks2 = list(locks)
                    locks2[nxt] = tid
                    held2 = held + (nxt,)
                    ph = "run" if len(held2) == len(order) else "lock"
                    out.append(self.with_thread((cells, tuple(locks2), waiters, threads), tid,
                                                (pc, ph, held2, wakes, woken, None)))
            elif phase == "read":  # no_locks: the read, then the write, as two actions
                out.append(self.with_thread(s, tid, (pc, "write", (), wakes, woken, tuple(sorted(values.items())))))
            elif phase in ("run", "write"):
                seen = dict(snap) if phase == "write" else values
                r = fn(seen)
                if r is None:
                    if self.mutation == "late_register":
                        out.append(self.with_thread(s, tid, (pc, "unlock_then_register", held, wakes, False, None)))
                    else:
                        w2 = tuple(ws | {tid} if c in fp else ws for c, ws in enumerate(waiters))
                        out.append(self.with_thread((cells, locks, w2, threads), tid,
                                                    (pc, "unlock_then_park", held, wakes, False, None)))
                else:
                    cells2 = dict(cells)
                    cells2.update(r)
                    taken = frozenset().union(*(waiters[c] for c in r))
                    w2 = tuple(ws - taken for ws in waiters)
                    out.append(self.with_thread((tuple(sorted(cells2.items())), locks, w2, threads), tid,
                                                (pc, "unlock_then_next", held, wakes + tuple(sorted(taken)), woken,
                                                 None)))
            elif phase.startswith("unlock_then_"):
                if held:
                    locks2 = list(locks)
                    locks2[held[-1]] = None
                    out.append(self.with_thread((cells, tuple(locks2), waiters, threads), tid,
                                                (pc, phase, held[:-1], wakes, woken, None)))
                elif phase == "unlock_then_register":
                    w2 = tuple(ws | {tid} if c in fp else ws for c, ws in enumerate(waiters))
                    out.append(self.with_thread((cells, locks, w2, threads), tid,
                                                (pc, "unlock_then_park", (), wakes, woken, None)))
                elif phase == "unlock_then_park":
                    if woken:  # a wake arrived since it registered: run again
                        out.append(self.with_thread(s, tid, (pc, "lock", (), wakes, False, None)))
                    else:
                        out.append(self.with_thread(s, tid, (pc, "parked", (), wakes, False, None)))
                elif wakes:
                    out.append(self.wake(s, tid, wakes))
                else:
                    nxt = (pc + 1, "lock") if pc + 1 < len(prog) else (pc, "done")
                    out.append(self.with_thread(s, tid, (nxt[0], nxt[1], (), (), woken, None)))
        return out

    def wake(self, s, tid, wakes):
        cells, locks, waiters, threads = s
        w, rest = wakes[0], wakes[1:]
        th = list(threads)
        pc, phase, held, wk, woken, snap = th[tid]
        th[tid] = (pc, phase, held, rest, woken, snap)
        wpc, wphase, wheld, wwk, wwoken, wsnap = th[w]
        if wphase == "parked":
            th[w] = (wpc, "lock", (), wwk, False, None)
        else:  # it registered but has not parked yet: counted, not lost
            th[w] = (wpc, wphase, wheld, wwk, True, wsnap)
        return (cells, locks, waiters, tuple(th))

    @staticmethod
    def with_thread(s, tid, th):
        cells, locks, waiters, threads = s
        t = list(threads)
        t[tid] = th
        return (cells, locks, waiters, tuple(t))


def explore(init, progs, want, mutation=None):
    """Every reachable state. Returns (states, None) or (states, violation)."""
    m = Model(init, progs, mutation)
    start = m.start()
    seen, stack = {start}, [start]
    while stack:
        s = stack.pop()
        nxt = m.actions(s)
        if not nxt:
            cells, locks, waiters, threads = s
            if all(th[1] == "done" for th in threads):
                if dict(cells) != want:
                    return len(seen), f"V2: finished with {dict(cells)}, not {want}"
                continue
            if any(th[1] == "lock" for th in threads):
                return len(seen), "V4: threads wait for each other's locks"
            return len(seen), "V7: a waiter parked for good, though a step could run"
        for n in nxt:
            if n not in seen:
                seen.add(n)
                stack.append(n)
    return len(seen), None


def main():
    ok = True
    for name, (init, progs, want) in SCENARIOS.items():
        for mutation in (None, "unordered", "late_register", "no_locks"):
            n, bad = explore(init, progs, want, mutation)
            print(f"{name:<9} {mutation or 'correct':<14} {n:>7} states  {bad or 'ok'}")
            if mutation is None and bad:
                ok = False
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
