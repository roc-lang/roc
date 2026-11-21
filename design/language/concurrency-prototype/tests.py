"""Regression tests for the concurrency prototype and the RC-atomicity models.

    python3 tests.py            everything (Maude and Lean only if installed)
    python3 tests.py -k Wake    one group

Each group pins behavior the RFC states, or shows that a check catches the bug
it exists for (the "mutation" tests).
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
import unittest
from collections import Counter

HERE = os.path.dirname(os.path.abspath(__file__))
RC = os.path.join(HERE, "..", "rc-atomicity")
sys.path.insert(0, HERE)
sys.setrecursionlimit(100000)

import engines  # noqa: E402
import host as host_mod  # noqa: E402
import library  # noqa: E402
import runtime  # noqa: E402
import sessions as sessions_mod  # noqa: E402
import shared as shared_mod  # noqa: E402
from arc import ArcError, certify  # noqa: E402
from check import CancelAtPolicy, Failure, check_program  # noqa: E402
from heap import HeapError  # noqa: E402
from host import (InputOrderPolicy, PausePolicy, RandomPolicy, ReadyPolicy, run_none, run_none_build,  # noqa: E402
                  run_tasks)
from lower import LowerError  # noqa: E402
from pipeline import build  # noqa: E402
from runtime import Violation  # noqa: E402

FETCH_TWO = r"""
(proc fetch_two (a b) eff
  (let x (host read a))
  (match x
    (Err (e) (block (ret (tag Err e))))
    (Ok (xv) (block
      (let y (host read b))
      (match y
        (Err (e2) (block (ret (tag Err e2))))
        (Ok (yv) (block (ret (tag Ok (add xv yv))))))))))
"""


def prog(main, extra=""):
    return build(FETCH_TWO + extra + main)


def tasks(lows, policy, mode="lss", engine="unwind", conc="Tasks"):
    return run_tasks(lows[mode], engine, policy, conc)


class Lowering(unittest.TestCase):
    def test_rfc_example_runs_straight_through_when_nothing_pauses(self):
        _, lows = prog("(proc main () eff (tail fetch_two 1 2))")
        r = tasks(lows, ReadyPolicy())
        self.assertEqual(r.result, ("done", ("tag", "Ok", (4 + 7,))))
        self.assertEqual(r.engine["records"], 0)  # two flag tests, no record
        self.assertEqual(r.engine["checks"], 2)

    def test_rfc_example_saves_one_record_per_pause(self):
        _, lows = prog("(proc main () eff (tail fetch_two 1 2))")
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.result, ("done", ("tag", "Ok", (11,))))
        # Each pause saves fetch_two's record; main tail-called it, so it has none.
        self.assertEqual(r.engine["records"], 2)
        self.assertEqual(r.engine["saved_values"], 2)  # {b} at site 1, {x} at site 2, as in the RFC
        self.assertEqual(r.engine["resume_entries"], 2)

    def test_ledger_is_exactly_the_live_values(self):
        arc, lows = prog("(proc main () eff (tail fetch_two 1 2))")
        sites = {sid: info for sid, info in lows["lss"].sites.items() if info[0] == "fetch_two"}
        ledgers = sorted(info[4] for info in sites.values())
        self.assertEqual(ledgers, [("b",), ("xv",)])

    def test_tail_loop_that_pauses_keeps_constant_records(self):
        src = """
        (proc loop (n acc) eff
          (let z (eq n 0))
          (if z (block (ret acc))
            (block (let v (host slow n)) (let a2 (add acc v)) (let n2 (sub n 1)) (tail loop n2 a2))))
        (proc deep (n) eff
          (let z (eq n 0))
          (if z (block (ret (host slow 0)))
            (block (let n2 (sub n 1)) (let v (call deep n2)) (ret (add v 1)))))
        (proc main () eff
          (let a (call loop 200 0))
          (let b (call deep 50))
          (ret (add a b)))
        """
        _, lows = build(src)
        r = tasks(lows, PausePolicy())
        # C7: the tail loop pauses 200 times holding one record (plus main's);
        # the 50-deep recursion holds one record per level.
        self.assertEqual(r.engine["max_chain"], 52)
        _, lows2 = build(src.replace("(let b (call deep 50))", "(let b (int 0))"))
        self.assertEqual(tasks(lows2, PausePolicy()).engine["max_chain"], 2)

    CLOSURE_LOOP = """
        (proc loop (n acc) eff
          (let z (eq n 0))
          (if z (block (ret acc))
            (block (let n2 (sub n 1)) (let k (clo step n2 acc)) (tailc k))))
        (proc step (n acc) eff
          (let v (host slow n))
          (let a2 (add acc v))
          (tail loop n a2))
        (proc wrap (n) eff
          (let v (call loop n 0))
          (ret v))
        (proc main () eff
          (let a (call wrap 200))
          (let b (call loop 3 0))
          (ret (add (add a b) 1)))
        """

    def test_tail_cycle_through_a_closure_keeps_constant_records(self):
        _, lows = build(self.CLOSURE_LOOP)
        none = none_build(lows)
        for mode in ("lss", "boxy"):
            r = tasks(lows, PausePolicy(), mode)
            self.assertEqual(r.result, none.result)
            # C7/G8: 203 pauses, and the chain never grows with the loop. Under
            # .lss it is main's, wrap's and the paused step's records. Under
            # .boxy wrap has already handed the pending call up and returned,
            # so main drives step directly: main's and step's.
            self.assertEqual(r.engine["max_chain"], 2 if mode == "boxy" else 3)
            self.assertEqual(r.engine["deferred_calls"], 203 if mode == "boxy" else 0)
            self.assertEqual(tasks(lows, ReadyPolicy(), mode).engine["max_chain"], 0)

    def test_deferred_call_is_handed_up_through_a_suspension(self):
        _, lows = build(self.CLOSURE_LOOP)
        low = lows["boxy"]
        # wrap returns loop's value unchanged: it hands a pending call up and
        # main, which uses it, drives. Neither happens under .lss.
        kinds = {(low.calls[s][0], "handed") for s in low.handed} | {(low.calls[s][0], "drive") for s in low.drive}
        self.assertIn(("wrap", "handed"), kinds)
        self.assertIn(("main", "drive"), kinds)
        self.assertFalse(lows["lss"].drive | lows["lss"].handed)
        r = tasks(lows, PausePolicy(), "boxy")
        self.assertGreater(r.engine["driven_calls"], 0)
        u, s = r, tasks(lows, PausePolicy(), "boxy", engine="stack")
        self.assertEqual((u.result, u.effects, u.heap), (s.result, s.effects, s.heap))

    def test_boxy_checks_every_erased_effectful_call(self):
        src = """
        (proc pure_thunk () eff (ret 3))
        (proc main () eff
          (let c (clo pure_thunk))
          (let v (callc! c))
          (ret v))
        """
        _, lows = build(src)
        self.assertEqual(tasks(lows, PausePolicy(), "lss").engine["checks"], 0)
        self.assertEqual(tasks(lows, PausePolicy(), "boxy").engine["checks"], 1)


class Combinators(unittest.TestCase):
    MAIN = """
    (proc child (a b) eff (tail fetch_two a b))
    (proc main () eff
      (let fs (list (clo child 1 2) (clo child 3 5) (clo child 4 6) (clo child 5 9)))
      (ret (call {comb} fs)))
    """

    def test_all_returns_in_input_order_under_every_schedule(self):
        _, lows = prog(self.MAIN.format(comb="all!"))
        want = none_build(lows).result
        for seed in range(40):
            for pol in (RandomPolicy(seed), RandomPolicy(seed, parallel=True)):
                r = tasks(lows, pol, conc="ParallelTasks" if pol.parallel else "Tasks")
                self.assertEqual(r.result, want)

    def test_all_ok_cancels_the_rest_at_the_first_err(self):
        _, lows = prog(self.MAIN.format(comb="all_ok!"))
        none = none_build(lows)
        self.assertEqual(none.result, ("done", ("tag", "Err", (("str", "e4"),))))
        # None: the fourth child never starts.
        self.assertNotIn(((3,), "read", (5,)), none.effects)
        io = tasks(lows, InputOrderPolicy())
        self.assertEqual((io.result, io.effects), (none.result, none.effects))
        for seed in range(30):
            r = tasks(lows, RandomPolicy(seed))
            self.assertEqual(r.result[1][1], "Err")
            self.assertGreaterEqual(r.runtime["cancel_requests"], 0)

    def test_race_returns_the_first_and_cancels_the_rest(self):
        src = """
        (proc fast () eff (ret 1))
        (proc slow3 () eff (let a (host slow 1)) (let b (host slow a)) (ret (host slow b)))
        (proc main () eff (ret (call race! (clo slow3) (list (clo fast)))))
        """
        _, lows = build(src)
        self.assertEqual(none_build(lows).result, ("done", 1 + 7 * 3))  # None: first one only
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.result, ("done", 1))
        self.assertEqual(r.runtime["cancelled"], 1)

    def test_supervise_restarts_exactly_max_times(self):
        src = """
        (proc bad () eff (let x (host slow 1)) (crash "boom"))
        (proc main () eff (ret (call supervise! 3 (clo bad))))
        """
        _, lows = build(src)
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.result, ("crashed", ("str", "boom")))
        self.assertEqual(r.runtime["spawns"], 4)  # one run plus three restarts (R7)
        self.assertEqual(sum(1 for e in r.effects if e[1] == "slow"), 4)
        # None builds run it once and stop the program.
        self.assertEqual(len(none_build(lows).effects), 1)

    SERVER = """
    (proc handle (i flaky) eff
      (let x (host slow i))
      (let c (host nth i))
      (let first (eq c 0))
      (let bad (eq flaky 1))
      (if bad
        (block (if first (block (crash "transient")) (block (ret 1))))
        (block (ret 1))))
    (proc serve_all (reqs i n acc) eff
      (let c (lt i n))
      (if c
        (block (let f (get reqs i)) (let v (call handle i f)) (tail serve_all reqs (add i 1) n (add acc v)))
        (block (ret acc))))
    (proc serve_each (reqs i n acc) eff
      (let c (lt i n))
      (if c
        (block (let f (get reqs i)) (let v (call supervise! 3 (clo handle i f)))
               (tail serve_each reqs (add i 1) n (add acc v)))
        (block (ret acc))))
    (proc whole (reqs) eff (let n (len reqs)) (let v (call serve_all reqs 0 n 0)) (ret v))
    (proc reqs () pure (ret (list 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)))
    (proc main_whole () eff (let r (call reqs)) (let v (call supervise! 3 (clo whole r))) (ret v))
    (proc main_each () eff (let r (call reqs)) (let n (len r)) (let v (call serve_each r 0 n 0)) (ret v))
    """

    def test_supervising_each_request_needs_no_time_window(self):
        # The RFC's decision on a time window for supervise!'s restart limit.
        # Four transient crashes, far apart, in twenty requests: a server
        # supervised as a whole spends its lifetime limit on them, gives up
        # and redoes earlier requests on every restart; supervising each
        # request handles every one, with one restart per crash.
        for entry, result, handled in (("main_whole", ("crashed", ("str", "transient")), 50),
                                       ("main_each", ("done", 20), 24)):
            _, lows = build(self.SERVER, entry)
            for mk in (PausePolicy, InputOrderPolicy, lambda: RandomPolicy(3, parallel=True)):
                pol = mk()
                r = tasks(lows, pol, conc="ParallelTasks" if pol.parallel else "Tasks")
                self.assertEqual(r.result, result)
                self.assertEqual(sum(1 for e in r.effects if e[1] == "slow"), handled)

    def test_one_for_one(self):
        src = """
        (proc bad () eff (let x (host slow 1)) (crash "boom"))
        (proc good () eff (ret (host slow 2)))
        (proc sup (f) eff (ret (call supervise! 1 f)))
        (proc main () eff
          (let r (call all! (list (clo sup (clo good)) (clo sup (clo good)))))
          (ret r))
        """
        _, lows = build(src)
        self.assertEqual(tasks(lows, PausePolicy()).result, ("done", ("list", None, (9, 9))))


class Loops(unittest.TestCase):
    """Concurrent loops over plans (RFC, "Concurrent loops")."""

    LIB = """
    (proc kid (n) eff (let a (host slow n)) (ret a))
    (proc fast () eff (ret 1))
    (proc slow2 () eff (let a (host slow 1)) (let b (host slow a)) (ret b))
    (proc sum_loop (g acc) eff
      (let a (next g))
      (match a
        (Arrived (v) (block (let acc2 (add acc v)) (tail sum_loop g acc2)))
        (Cancelled () (block (crash "unreachable")))
        (Ended () (block (ret acc)))))
    (proc first_loop (g) eff
      (let a (next g))
      (match a
        (Arrived (v) (block (do (call close_loop g)) (ret v)))
        (Cancelled () (block (crash "unreachable")))
        (Ended () (block (ret 0)))))
    """

    def run_with(self, src, policy, **switches):
        _, lows = build(self.LIB + src)
        old = {}
        orig_init = runtime.Runtime.__init__

        def init(rt, *a, **k):
            orig_init(rt, *a, **k)
            for name, v in switches.items():
                setattr(rt, name, v)

        runtime.Runtime.__init__ = init
        try:
            return tasks(lows, policy), lows
        finally:
            runtime.Runtime.__init__ = orig_init

    LIMITED = """
    (proc main () eff
      (let p (call concurrently (list (clo kid 1) (clo kid 2) (clo kid 3))))
      (let p2 (call limit p 1))
      (let g (group p2))
      (tail sum_loop g 0))
    """

    def test_limit_runs_entries_one_at_a_time_and_refills_as_each_finishes(self):
        r, lows = self.run_with(self.LIMITED, PausePolicy())
        self.assertEqual(r.result, ("done", 8 + 9 + 10))
        self.assertEqual(r.runtime["limit refills"], 2)
        self.assertEqual(none_build(lows).result, r.result)

    def test_starting_past_a_limit_trips_r8(self):
        with self.assertRaisesRegex(Violation, "R8"):
            self.run_with(self.LIMITED, PausePolicy(), limit_slack=1)

    SHIELDED = """
    (proc main () eff
      (let p1 (call concurrently (list (clo slow2))))
      (let p2 (call shielded p1))
      (let q (call concurrently (list (clo fast))))
      (let p (call concat p2 q))
      (let g (group p))
      (tail first_loop g))
    """

    def test_an_exit_waits_for_a_shielded_entry_instead_of_cancelling_it(self):
        r, _ = self.run_with(self.SHIELDED, PausePolicy())
        self.assertEqual(r.result, ("done", 1))
        self.assertIn(((0,), "slow", (8,)), r.effects)  # its second step ran after the exit
        self.assertEqual(r.runtime["shielded waits"], 1)
        r, _ = self.run_with(self.SHIELDED, PausePolicy(), ignore_shields=True)
        self.assertNotIn(((0,), "slow", (8,)), r.effects)

    def test_a_shielded_entry_that_has_not_begun_is_cancelled_at_an_exit(self):
        src = """
        (proc main () eff
          (let p (call concurrently (list (clo fast) (clo slow2))))
          (let p1 (call limit p 1))
          (let p2 (call shielded p1))
          (let g (group p2))
          (tail first_loop g))
        """
        # The schedule that starts entries only when the loop asks: the second
        # entry has not begun at the exit, so it never runs, as in None builds.
        r, lows = self.run_with(src, InputOrderPolicy())
        self.assertEqual(r.result, ("done", 1))
        self.assertEqual([e for e in r.effects if e[1] == "slow"], [])
        none = none_build(lows)
        self.assertEqual((none.result, none.effects), (r.result, r.effects))
        # FIFO starts it as soon as the limit frees a slot; once it has begun,
        # the exit waits for it.
        r, _ = self.run_with(src, PausePolicy())
        self.assertEqual(r.result, ("done", 1))
        self.assertEqual(r.runtime["shielded waits"], 1)

    def test_returning_from_inside_a_loop_without_its_exit_trips_g5(self):
        src = """
        (proc leaky (g) eff
          (let a (next g))
          (match a
            (Arrived (v) (block (ret v)))
            (Cancelled () (block (crash "unreachable")))
            (Ended () (block (ret 0)))))
        (proc main () eff
          (let g (group (call concurrently (list (clo fast) (clo slow2)))))
          (tail leaky g))
        """
        with self.assertRaisesRegex(Violation, "G5"):
            self.run_with(src, PausePolicy())

    def test_spawn_in_a_body_adds_entries(self):
        src = """
        (proc grow (g acc budget) eff
          (let a (next g))
          (match a
            (Arrived (v) (block
              (let acc2 (add acc v))
              (let more (lt 0 budget))
              (if more
                (block (do (spawn_in g (clo kid 1))) (tail grow g acc2 (sub budget 1)))
                (block (tail grow g acc2 budget)))))
            (Cancelled () (block (crash "unreachable")))
            (Ended () (block (ret acc)))))
        (proc main () eff
          (let g (group (call concurrently (list (clo kid 1)))))
          (tail grow g 0 2))
        """
        r, lows = self.run_with(src, PausePolicy())
        self.assertEqual(r.result, ("done", 3 * 8))
        self.assertEqual(r.runtime["loop spawns"], 2)
        self.assertEqual(none_build(lows).result, r.result)

    GROW = """
    (proc grow (g acc budget) eff
      (let a (next g))
      (match a
        (Arrived (v) (block
          (let acc2 (add acc v))
          (let more (lt 0 budget))
          (if more
            (block (do (spawn_in g (clo kid 1))) (do (spawn_in g (clo kid 2))) (tail grow g acc2 (sub budget 1)))
            (block (tail grow g acc2 budget)))))
        (Cancelled () (block (crash "unreachable")))
        (Ended () (block (ret acc)))))
    (proc main () eff
      (let p (call concurrently (list (clo kid 1) (clo kid 2))))
      {plan}
      (let g (group q))
      (tail grow g 0 2))
    """

    def test_spawned_entries_count_against_the_limit_at_the_plans_root(self):
        # A crawler over plan.limit(2) never runs more than two fetches, however
        # many the body spawns (G9).
        src = self.GROW.format(plan="(let q (call limit p 2))")
        want = ("done", 3 * (8 + 9))
        for mk in (PausePolicy, ReadyPolicy, lambda: RandomPolicy(5, parallel=True)):
            pol = mk()
            _, lows = build(self.LIB + src)
            r = run_tasks(lows["lss"], "unwind", pol, "ParallelTasks" if pol.parallel else "Tasks")
            self.assertEqual(r.result, want)
            self.assertEqual(r.runtime["loop spawns under the root limit"], 4)
            self.assertEqual(r.runtime["loop: most entries running under a root limit"], 2)
        r, lows = self.run_with(src, PausePolicy())
        self.assertEqual(none_build(lows).result, want)
        # Spawned entries under no limit: a third runs at once.
        with self.assertRaisesRegex(Violation, "G9"):
            self.run_with(src, PausePolicy(), spawn_outside_root_limit=True)

    def test_a_plan_whose_root_is_not_a_limit_spawns_under_no_limit(self):
        src = self.GROW.format(plan="(let l (call limit p 1)) (let e (call concurrently (list))) (let q (call concat l e))")
        r, lows = self.run_with(src, PausePolicy())
        self.assertEqual(r.result, ("done", 3 * (8 + 9)))
        self.assertEqual(r.runtime["loop spawns"], 4)
        self.assertEqual(r.runtime["loop spawns under the root limit"], 0)
        self.assertEqual(none_build(lows).result, r.result)

    def test_a_limited_loop_whose_entries_wait_on_a_spawned_one_deadlocks(self):
        # The class of deadlock spawn under the root limit adds: both slots
        # hold entries that wait for one the body spawns, which never starts.
        src = """
        (proc waiter (i) eff (let r (recv i)) (match r (Got (x) (block (ret x))) (Cancelled () (block (ret 0)))))
        (proc giver (o) eff (do (send o 5)) (ret 0))
        (proc spawn_loop (g o acc) eff
          (let a (next g))
          (match a
            (Arrived (v) (block (let acc2 (add acc v)) (tail spawn_loop g o acc2)))
            (Cancelled () (block (crash "unreachable")))
            (Ended () (block (ret acc)))))
        (proc main () eff
          (let pr (pair)) (let o (field pr 0)) (let i (field pr 1))
          (let p (call concurrently (list (clo waiter i) (clo fast))))
          (let q (call limit p 1))
          (let g (group q))
          (do (spawn_in g (clo giver o)))
          (tail spawn_loop g 0 0))
        """
        r, lows = self.run_with(src, PausePolicy())
        self.assertEqual(r.result, ("crashed", ("str", sessions_mod.DEADLOCK)))
        # Under a limit that is not at the root, the giver runs beside the waiter.
        r, _ = self.run_with(src.replace("(let q (call limit p 1))", "(let l (call limit p 1)) (let e (call concurrently (list))) (let q (call concat l e))"), PausePolicy())
        self.assertEqual(r.result, ("done", 6))

    DEADLINE = """
    (proc ticker (n) eff
      (let z (lt n 1))
      (if z
        (block (ret 0))
        (block (let a (host slow n)) (tail ticker (sub n 1)))))
    (proc main () eff
      (let p (call concurrently (list (clo kid 1) (clo kid 2))))
      (let b (call concurrently (list (clo ticker 50))))
      (let bg (call background b))
      (let q (call concat {order}))
      (let g (group q))
      (tail sum_loop g 0))
    """

    def test_a_loop_ends_once_only_background_entries_are_left(self):
        src = self.DEADLINE.format(order="p bg")
        ticks = lambda r: sum(1 for e in r.effects if e[0] == (2,))
        seen = set()
        for mk in ALL_POLICIES[:8]:
            pol = mk()
            _, lows = build(self.LIB + src)
            for mode in ("lss", "boxy"):
                for eng in ("unwind", "stack"):
                    r = run_tasks(lows[mode], eng, mk(), "ParallelTasks" if pol.parallel else "Tasks")
                    # The ticker's arrival, if it finished first, adds 0.
                    self.assertEqual(r.result, ("done", 8 + 9))
                    seen.add(ticks(r) < 50)
        # It was cancelled, except where hosted calls finish in place and it
        # ran to its end before the others (the ready schedule).
        self.assertIn(True, seen)
        r, lows = self.run_with(src, PausePolicy())
        self.assertEqual(r.runtime["background entries cancelled at the end"], 1)
        # A None build never starts it: once every unstarted entry is
        # background, the loop ends.
        for none in (none_build(lows), run_none(lows["lss"].prog)):
            self.assertEqual((none.result, ticks(none)), (("done", 8 + 9), 0))
        # If background entries kept the loop going, it would wait for all 50 ticks.
        r, _ = self.run_with(src, PausePolicy(), ignore_background=True)
        self.assertEqual(ticks(r), 50)

    def test_a_background_entry_before_the_others_runs_first_in_a_none_build(self):
        # The None build runs the earliest unstarted entry, background or not.
        _, lows = build(self.LIB + self.DEADLINE.format(order="bg p"))
        ticks = lambda r: sum(1 for e in r.effects if e[0] == (0,))
        for none in (none_build(lows), run_none(lows["lss"].prog)):
            self.assertEqual((none.result, ticks(none)), (("done", 8 + 9), 50))
        r = tasks(lows, InputOrderPolicy())
        self.assertEqual((r.result, ticks(r)), (("done", 8 + 9), 50))

    def test_plan_all_returns_results_in_input_order_without_background_ones(self):
        src = """
        (proc main () eff
          (let p (call concurrently (list (clo kid 3) (clo kid 1) (clo kid 2))))
          (let l (call limit p 2))
          (let b (call concurrently (list (clo kid 9))))
          (let bg (call background b))
          (let q (call concat l bg))
          (let r (call plan_all! q))
          (ret r))
        """
        _, lows = build(self.LIB + src)
        want = ("done", ("list", None, (10, 8, 9)))
        self.assertEqual(everywhere(lows), {want})
        self.assertEqual(none_build(lows).result, want)

    def test_plan_all_ok_leaves_at_the_first_err(self):
        src = """
        (proc okv (n) eff (let a (host slow n)) (ret (tag Ok a)))
        (proc bad () eff (let a (host slow 0)) (ret (tag Err 7)))
        (proc main () eff
          (let p (call concurrently (list (clo okv 1) (clo bad) (clo okv 2))))
          (let l (call limit p 1))
          (let r (call plan_all_ok! l))
          (ret r))
        """
        _, lows = build(self.LIB + src)
        self.assertEqual(everywhere(lows), {("done", ("tag", "Err", (7,)))})
        none = none_build(lows)
        self.assertEqual(none.result, ("done", ("tag", "Err", (7,))))
        self.assertNotIn(((2,), "slow", (2,)), none.effects)  # the third never starts

    BODY_WAITS = """
    (proc body_loop (g acc) eff
      (let a (next g))
      (match a
        (Arrived (v) (block
          (let w (host slow v))
          (let acc2 (add acc w))
          (tail body_loop g acc2)))
        (Cancelled () (block (crash "unreachable")))
        (Ended () (block (ret acc)))))
    (proc main () eff
      (let g (group (call concurrently (list (clo kid 1) (clo kid 2)))))
      (tail body_loop g 0))
    """

    def test_a_report_readies_its_parent_only_for_the_wait_it_belongs_to(self):
        r, _ = self.run_with(self.BODY_WAITS, PausePolicy())
        self.assertEqual(r.result, ("done", (8 + 7) + (9 + 7)))
        self.assertGreater(r.runtime["reports held for another wait"], 0)
        with self.assertRaisesRegex(Violation, "R1"):
            self.run_with(self.BODY_WAITS, PausePolicy(), one_wake_word=True)


    LINKED = """
    (proc bad () eff (let x (host slow 1)) (crash "boom"))
    (proc main () eff
      (let g (group (call concurrently (list (clo kid 1) (clo bad)))))
      (tail body_loop g 0))
    """

    RUNNING = """
    (proc bad () eff (crash "boom"))
    (proc body_log (g acc) eff
      (let a (next g))
      (match a
        (Arrived (v) (block
          (let l (host log "x"))
          (tail body_log g (add acc v))))
        (Cancelled () (block (crash "unreachable")))
        (Ended () (block (ret acc)))))
    (proc main () eff
      (let g (group (call concurrently (list (clo fast) (clo bad)))))
      (tail body_log g 0))
    """

    def test_an_entrys_crash_reaching_a_running_parent_crashes_it_at_its_next_suspension(self):
        # ParallelTasks: the parent takes fast's arrival and runs its body;
        # the bad entry crashes on another thread during the body's hosted
        # call. The parent's next loop head must not take a result: it
        # suspends, and the runtime crashes it there with an exact drop.
        class RootFirst(PausePolicy):
            parallel = True

            def choose(self, host):
                if host.root in host.queue:
                    return ("resume", host.queue.index(host.root))
                return super().choose(host)

            def during_call(self, host):
                while any(t is not host.root for t in host.queue):
                    host.step()

        _, lows = build(self.LIB + self.RUNNING)
        for mode in ("lss", "boxy"):
            for eng in ("unwind", "stack"):
                r = tasks(lows, RootFirst(), mode, eng, "ParallelTasks")
                self.assertEqual(r.result, ("crashed", ("str", "boom")))
                self.assertEqual(r.runtime["linked crashes pending at a suspension"], 1)
                self.assertEqual(r.leaked_live, 0)  # the parent crashed paused: nothing leaks
        with self.assertRaisesRegex(Violation, "G7"):
            with switches(rt={"crash_pending": lambda t, consumed: False}):
                tasks(lows, RootFirst(), conc="ParallelTasks")

    def test_an_entrys_crash_reaches_a_parent_waiting_elsewhere_at_once(self):
        src = self.BODY_WAITS.split("(proc main")[0] + self.LINKED
        r, _ = self.run_with(src, PausePolicy())
        self.assertEqual(r.result, ("crashed", ("str", "boom")))
        self.assertEqual(r.runtime["linked crashes"], 1)
        self.assertEqual(r.host["aborted"], 1)  # the body's hosted call was abandoned
        # Without links, the crash waits until the body's call finishes.
        r, _ = self.run_with(src, PausePolicy(), no_linked_crashes=True)
        self.assertEqual(r.result, ("crashed", ("str", "boom")))
        self.assertEqual(r.host["aborted"], 0)


class Sessions(unittest.TestCase):
    """One-shot session channels (concurrency-sessions.md)."""

    LIB = """
    (proc producer (o n) eff
      (let z (eq n 0))
      (if z
        (block (let d (tag Done)) (do (send o d)) (ret 0))
        (block
          (let a (host slow n))
          (let pr (pair))
          (let o2 (field pr 0))
          (let i2 (field pr 1))
          (let m (tag More a i2))
          (do (send o m))
          (let n2 (sub n 1))
          (tail producer o2 n2))))
    (proc consume (i acc) eff
      (let r (recv i))
      (match r
        (Got (m) (block (match m
            (More (a i2) (block (let acc2 (add acc a)) (tail consume i2 acc2)))
            (Done () (block (ret acc))))))
        (Cancelled () (block (let u (sub 0 1)) (ret u)))))
    (proc fast () eff (ret 1))
    (proc slow1 () eff (let a (host slow 1)) (ret a))
    """

    POLICIES = [lambda: InputOrderPolicy(), lambda: ReadyPolicy(), lambda: PausePolicy(),
                lambda: RandomPolicy(5), lambda: RandomPolicy(6, parallel=True)]

    def everywhere(self, src):
        """The None result, and every Tasks result under every schedule and engine."""
        _, lows = build(self.LIB + src)
        none = none_build(lows)
        out = set()
        for mk in self.POLICIES:
            for eng in ("unwind", "stack"):
                pol = mk()
                out.add(run_tasks(lows["lss"], eng, pol, "ParallelTasks" if pol.parallel else "Tasks").result)
        return none, out, lows

    def run_with(self, src, policy, rt=None, sessions=None):
        _, lows = build(self.LIB + src)
        orig_init = runtime.Runtime.__init__

        def init(r, *a, **k):
            orig_init(r, *a, **k)
            for name, v in (rt or {}).items():
                setattr(r, name, v)
            for name, v in (sessions or {}).items():
                setattr(r.sessions, name, v)

        runtime.Runtime.__init__ = init
        try:
            return tasks(lows, policy)
        finally:
            runtime.Runtime.__init__ = orig_init

    STREAM = """
    (proc main () eff
      (let pr (pair))
      (let o (field pr 0))
      (let i (field pr 1))
      (let p (call concurrently (list (clo producer o 3))))
      (let g (group p))
      (let s (call consume i 0))
      (do (call close_wait g))
      (ret s))
    """

    def test_a_stream_flows_by_continuation_passing(self):
        none, tasks_results, _ = self.everywhere(self.STREAM)
        self.assertEqual(none.result, ("done", 10 + 9 + 8))
        self.assertEqual(tasks_results, {none.result})
        # The receive switches to the producer: a None build of a program
        # that can wait runs as coroutines.
        self.assertEqual(none.extra["engine"], "coroutine")

    CONSUMER_FIRST = """
    (proc main () eff
      (let pr (pair))
      (let o (field pr 0))
      (let i (field pr 1))
      (let p (call concurrently (list (clo consume i 0) (clo producer o 2))))
      (let g (group p))
      (let a (next g))
      (match a
        (Arrived (v) (block (do (call close_loop g)) (ret v)))
        (Cancelled () (block (ret 0)))
        (Ended () (block (ret 0)))))
    """

    def test_a_none_build_runs_a_later_entry_while_an_earlier_one_waits(self):
        _, lows = build(self.LIB + self.CONSUMER_FIRST)
        none = none_build(lows)
        co = tasks(lows, host_mod.CoroutinePolicy())
        self.assertEqual((none.result, none.effects), (co.result, co.effects))
        self.assertEqual(none.result, ("done", 0))  # the producer, run while the consumer waits, finished first
        self.assertEqual(none.extra["engine"], "coroutine")
        # On one stack the receive runs the producer on demand, with the same result.
        one = run_none(lows["lss"].prog)
        self.assertEqual((one.result, one.extra["on demand"]), (("done", 0), 1))

    DROPPED = """
    (proc main () eff
      (let pr (pair))
      (let o (field pr 0))
      (let i (field pr 1))
      (let s (call consume i 0))
      (ret s))
    """

    def test_dropping_an_unused_out_end_delivers_cancelled(self):
        none, tasks_results, _ = self.everywhere(self.DROPPED)
        self.assertEqual(none.result, ("done", -1))
        self.assertEqual(tasks_results, {none.result})

    def test_without_cancelled_on_drop_the_receiver_deadlocks(self):
        r = self.run_with(self.DROPPED, PausePolicy(), sessions={"no_cancel_on_drop": True})
        self.assertEqual(r.result, ("crashed", ("str", sessions_mod.DEADLOCK)))

    CANCELLED_PRODUCER = """
    (proc main () eff
      (let pr (pair))
      (let o (field pr 0))
      (let i (field pr 1))
      (let p (clo producer o 2))
      (let f (clo fast))
      (let rest (list p))
      (let w (call race! f rest))
      (let s (call consume i 0))
      (let t (add w s))
      (ret t))
    """

    def test_cancelling_a_producer_delivers_cancelled_at_its_current_end(self):
        r = tasks(build(self.LIB + self.CANCELLED_PRODUCER)[1], PausePolicy())
        # The producer paused after its first message and was cancelled: the
        # consumer takes that message, then Cancelled at the end the cancelled
        # producer's paused records held.
        self.assertIn(r.result, {("done", 1 - 1), ("done", 1 + 9 - 1)})
        self.assertGreaterEqual(r.runtime["Cancelled delivered"], 1)
        none, tasks_results, _ = self.everywhere(self.CANCELLED_PRODUCER)
        self.assertEqual(none.result, ("done", 0))  # the race's later entrant never started

    TWICE = """
    (proc main () eff
      (let pr (pair))
      (let o (field pr 0))
      (let i (field pr 1))
      (let z (int 1))
      (do (send o z))
      (do (send o z))
      (ret 0))
    """

    def test_using_an_end_twice_crashes(self):
        none, tasks_results, _ = self.everywhere(self.TWICE)
        self.assertEqual(none.result, ("crashed", ("str", sessions_mod.USED_TWICE)))
        self.assertEqual(tasks_results, {none.result})

    CYCLE = """
    (proc waiter (i o) eff
      (let r (recv i))
      (let one (int 1))
      (do (send o one))
      (ret 0))
    (proc main () eff
      (let p1 (pair))
      (let o1 (field p1 0))
      (let i1 (field p1 1))
      (let p2 (pair))
      (let o2 (field p2 0))
      (let i2 (field p2 1))
      (let p (call concurrently (list (clo waiter i1 o2) (clo waiter i2 o1))))
      (let g (group p))
      (do (call close_wait g))
      (ret 0))
    """

    def test_a_cycle_of_sessions_deadlocks_and_crashes_the_last_receiver(self):
        none, tasks_results, _ = self.everywhere(self.CYCLE)
        self.assertEqual(none.result, ("crashed", ("str", sessions_mod.DEADLOCK)))
        self.assertEqual(tasks_results, {none.result})

    BESIDE = """
    (proc main () eff
      (let pr (pair))
      (let o (field pr 0))
      (let i (field pr 1))
      (let p (call concurrently (list (clo fast) (clo producer o 1))))
      (let g (group p))
      (let s (call consume i 0))
      (do (call close_wait g))
      (ret s))
    """

    def test_an_arrival_does_not_ready_a_task_waiting_at_a_receive(self):
        r = self.run_with(self.BESIDE, PausePolicy())
        self.assertEqual(r.result, ("done", 8))
        self.assertGreaterEqual(r.runtime["reports held for another wait"], 1)
        with self.assertRaisesRegex(Violation, "R1"):
            self.run_with(self.BESIDE, PausePolicy(), rt={"one_wake_word": True})


    SUPERVISED = """
    (proc boom () eff (let u (str "boom")) (crash u))
    (proc attempt (ri) eff
      (let r (recv ri))
      (match r
        (Got (o) (block
           (let pr (pair)) (let o2 (field pr 0)) (let i2 (field pr 1))
           (let one (int 1))
           (let m (tag More one i2))
           (do (send o m))
           (do (call boom))
           (let d (tag Done)) (do (send o2 d))
           (ret 0)))
        (Cancelled () (block (ret 0)))))
    (proc supervised (ri) eff (let z (int 1)) (let f (clo attempt ri)) (let v (call supervise! z f)) (ret v))
    (proc watch (i) eff (let s (call consume i 0)) (let u (str "saw the end")) (let l (host log u)) (ret s))
    (proc main () eff
      (let p1 (pair)) (let o (field p1 0)) (let i (field p1 1))
      (let p2 (pair)) (let ro (field p2 0)) (let ri (field p2 1))
      (do (send ro o))
      (let p (call concurrently (list (clo watch i) (clo supervised ri))))
      (let g (group p))
      (do (call close_wait g))
      (ret 0))
    """

    def test_a_crash_closes_the_ends_its_task_held(self):
        # A crash leaks what its task's running frames held, so its ends are
        # never dropped; the crash closes them instead. The attempt crashes holding the Out end of the stream's next step in
        # a running frame. Its supervisor restarts it, and the restart crashes
        # by using `ri` a second time; meanwhile the watcher, outside the
        # supervisor's subtree, has seen Cancelled and logged.
        saw = ((0,), "log", (("str", "saw the end"),))
        r = self.run_with(self.SUPERVISED, PausePolicy())
        self.assertEqual(r.result, ("crashed", ("str", sessions_mod.USED_TWICE)))
        self.assertIn(saw, r.effects)
        self.assertEqual(r.runtime["ports released after a crash"], 1)
        r = self.run_with(self.SUPERVISED, PausePolicy(), sessions={"no_close_on_leave": True})
        self.assertNotIn(saw, r.effects)  # it waited until the crash reached it

    # A one-permit semaphore as a task, and its owner-of-a-resource variant.
    # sem returns 1 when the permit comes back by Release, 2 when it comes
    # back as Cancelled, 3 when no request ever came.
    SERVERS = """
    (proc sem (req_i) eff
      (let r (recv req_i))
      (match r
        (Got (reply_o) (block
          (let pp (pair))
          (let po (field pp 0))
          (let pi (field pp 1))
          (do (send reply_o po))
          (let back (recv pi))
          (match back
            (Got (x) (block (ret 1)))
            (Cancelled () (block (ret 2))))))
        (Cancelled () (block (ret 3)))))
    (proc client (req_o) eff
      (let rp (pair))
      (let ro (field rp 0))
      (let ri (field rp 1))
      (do (send req_o ro))
      (let g (recv ri))
      (match g
        (Got (permit) (block
          (let a (host slow 2))
          (let rel (tag Release))
          (do (send permit rel))
          (ret 10)))
        (Cancelled () (block (ret 20)))))
    (proc owner (i) eff
      (let r (recv i))
      (match r
        (Got (x) (block (ret 1)))
        (Cancelled () (block (ret 2)))))
    (proc opener (o) eff
      (let a (host slow 1))
      (ret o))
    """

    PERMIT = SERVERS + """
    (proc main () eff
      (let rq (pair))
      (let req_o (field rq 0))
      (let req_i (field rq 1))
      (let p (call concurrently (list (clo sem req_i))))
      (let g (group p))
      (let c (clo client req_o))
      (let w (call race! c (list (clo slow1))))
      (let a (next g))
      (match a
        (Arrived (v) (block (do (call close_loop g)) (ret (tag R v w))))
        (Cancelled () (block (ret 0)))
        (Ended () (block (ret 0)))))
    """

    def test_a_permit_granted_to_a_cancelled_requester_returns_to_its_owner(self):
        # The timeout wins at every point of the grant: before the request,
        # while the reply is in flight, after the permit arrived. The permit
        # always comes back, by Release or as Cancelled, with the heap exact.
        _, lows = build(self.LIB + self.PERMIT)
        seen = set()
        pols = self.POLICIES[1:] + [lambda s=s: RandomPolicy(s, parallel=s % 2 == 1) for s in range(60)]
        for mk in pols:
            for eng in ("unwind", "stack"):
                pol = mk()
                r = run_tasks(lows["lss"], eng, pol, "ParallelTasks" if pol.parallel else "Tasks")
                self.assertEqual(r.result[0], "done")
                seen.add(r.result[1][2][0])
        self.assertEqual(seen, {1, 2, 3})  # released, returned by drop, never requested
        # A requester cancelled while parked at its receive abandons the
        # channel; without closing it the permit stays there, owned by
        # nothing: the semaphore deadlocks and the heap reports the permit.
        with self.assertRaisesRegex(HeapError, "count 1 but 0 references"):
            self.run_with(self.PERMIT, ReadyPolicy(), sessions={"no_close_abandoned": True})

    def test_a_request_reply_release_server_deadlocks_on_one_stack(self):
        # A known limit of None builds: the client below the semaphore on the
        # one stack cannot take its permit until the semaphore's wait for the
        # release returns. The input-order schedule models it the same way.
        _, lows = build(self.LIB + self.PERMIT)
        dead = ("crashed", ("str", sessions_mod.DEADLOCK))
        self.assertEqual(run_none(lows["lss"].prog).result, dead)
        self.assertEqual(tasks(lows, InputOrderPolicy()).result, dead)

    LOSER = SERVERS + """
    (proc main () eff
      (let p1 (pair))
      (let o1 (field p1 0))
      (let i1 (field p1 1))
      (let p2 (pair))
      (let o2 (field p2 0))
      (let i2 (field p2 1))
      (let p (call concurrently (list (clo owner i1) (clo owner i2))))
      (let g (group p))
      (let w (call race! (clo opener o1) (list (clo opener o2))))
      (let cl (tag Close))
      (do (send w cl))
      (let a (next g))
      (let b (next g))
      (match a
        (Arrived (x) (block (match b
          (Arrived (y) (block (do (call close_loop g)) (ret (tag R x y))))
          (Cancelled () (block (ret 0)))
          (Ended () (block (ret 0))))))
        (Cancelled () (block (ret 0)))
        (Ended () (block (ret 0)))))
    """

    def test_race_drops_the_end_a_finished_loser_returned(self):
        # Both openers can finish before race! takes one. The loser's result
        # is dropped at the loop's exit, and dropping the end it holds tells
        # that resource's owner.
        none, results, _ = self.everywhere(self.LOSER)
        self.assertEqual(none.result, ("done", ("tag", "R", (1, 2))))
        self.assertTrue(results <= {("done", ("tag", "R", (1, 2))), ("done", ("tag", "R", (2, 1)))}, results)


class switches:
    """Set mutation switches on every Runtime (and its Sessions and Shared) made inside."""

    def __init__(self, rt=None, sessions=None, host=None, shared=None):
        self.rt, self.sessions, self.host, self.shared = rt or {}, sessions or {}, host or {}, shared or {}

    def __enter__(self):
        self.orig = runtime.Runtime.__init__
        self.orig_attach = runtime.Runtime.attach
        orig, orig_attach, rt, ses, sh = self.orig, self.orig_attach, self.rt, self.sessions, self.shared

        def init(r, *a, **k):
            orig(r, *a, **k)
            for name, v in rt.items():
                setattr(r, name, v)
            for name, v in ses.items():
                setattr(r.sessions, name, v)

        def attach(r, *a, **k):
            orig_attach(r, *a, **k)
            for name, v in sh.items():
                setattr(r.shared, name, v)

        runtime.Runtime.__init__ = init
        runtime.Runtime.attach = attach
        return self

    def __exit__(self, *exc):
        runtime.Runtime.__init__ = self.orig
        runtime.Runtime.attach = self.orig_attach


# The first is the None build's schedule for a program that can wait.
ALL_POLICIES = [lambda: host_mod.CoroutinePolicy(), lambda: ReadyPolicy(), lambda: PausePolicy()] + \
    [lambda s=s: RandomPolicy(s, parallel=s % 2 == 1) for s in range(24)]


def none_build(lows):
    """The None build: coroutines if the program can wait, else one stack."""
    return run_none_build(lows["lss"].arc)


def all_results(lows, policies=None, host_cls=None):
    """Every Tasks result, under every schedule and engine."""
    out = set()
    for mk in policies or ALL_POLICIES:
        for eng in ("unwind", "stack"):
            pol = mk()
            out.add(run_tasks(lows["lss"], eng, pol, "ParallelTasks" if pol.parallel else "Tasks",
                              host_cls=host_cls).result)
    return out


class HolderSets(unittest.TestCase):
    """Holders are sets that follow captures, messages and results (concurrency-sessions.md, "Holders")."""

    SHARED = Sessions.LIB + """
    (proc check (n) eff
      (let z (eq n 0))
      (if z (block (crash "first try")) (block (ret 0))))
    (proc keep (o) eff (ret 0))
    (proc attempt (o) eff
      (let n (host nth 0))
      (let x (call check n))
      (do (call keep o))
      (ret x))
    (proc supervised (o) eff
      (let f (clo attempt o))
      (let v (call supervise! 1 f))
      (ret v))
    (proc watch (i) eff (let s (call consume i 0)) (ret s))
    (proc main () eff
      (let pr (pair)) (let o (field pr 0)) (let i (field pr 1))
      (let r (call both! (clo watch i) (clo supervised o)))
      (ret r))
    """

    def test_an_end_shared_with_a_restarted_child_is_released_when_its_last_live_holder_leaves(self):
        # The first attempt crashes with the Out end in a running frame, so
        # one reference leaks. The second attempt drops its reference unused,
        # and the supervisor drops the closure. The watcher must get
        # Cancelled once no live task holds the end, not wait for the leaked
        # reference. Holders follow the closure into each attempt, and the
        # supervisor holds it too, so the end is released when the
        # supervisor finishes.
        _, lows = build(self.SHARED)
        want = ("done", ("tup", None, (-1, 0)))
        self.assertEqual(all_results(lows, ALL_POLICIES[1:]), {want})
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.runtime["ports released after a crash"], 1)
        # Holders that do not follow captures leave the end with the parent,
        # which waits for the watcher: the run deadlocks.
        with switches(sessions={"no_capture_holders": True}):
            self.assertEqual(tasks(lows, PausePolicy()).result, ("crashed", ("str", sessions_mod.DEADLOCK)))


class Mailboxes(unittest.TestCase):
    """Mailboxes: many senders, one reader (concurrency-sessions.md, "Mailboxes")."""

    LIB = """
    (proc fast () eff (ret 1))
    (proc poster (p k) eff
      (do (post p k))
      (do (post p (add k 10)))
      (ret 0))
    (proc posters (p) eff
      (let r (call all! (list (clo poster p 1) (clo poster p 2) (clo poster p 3))))
      (ret 0))
    (proc reader (b acc) eff
      (let r (take b))
      (match r
        (Got (v rest) (block (tail reader rest (add acc v))))
        (Closed () (block (ret acc)))))
    """

    def everywhere(self, src):
        _, lows = build(self.LIB + src)
        return none_build(lows), all_results(lows), lows

    def test_many_posters_one_reader_closed_when_every_post_is_gone(self):
        none, results, _ = self.everywhere("""
        (proc main () eff
          (let mb (mailbox)) (let p (field mb 0)) (let b (field mb 1))
          (let r (call both! (clo reader b 0) (clo posters p)))
          (ret r))
        """)
        want = ("done", ("tup", None, (1 + 11 + 2 + 12 + 3 + 13, 0)))
        self.assertEqual(none.result, want)
        self.assertEqual(results, {want})

    def test_taking_from_an_inbox_twice_crashes(self):
        none, results, _ = self.everywhere("""
        (proc main () eff
          (let mb (mailbox)) (let p (field mb 0)) (let b (field mb 1))
          (do (post p 1))
          (let r1 (take b))
          (let r2 (take b))
          (ret 0))
        """)
        want = ("crashed", ("str", sessions_mod.USED_TWICE))
        self.assertEqual(none.result, want)
        self.assertEqual(results, {want})

    FLAKY = """
    (proc check (n) eff
      (let z (eq n 0))
      (if z (block (crash "first try")) (block (ret 0))))
    (proc flaky (p) eff
      (let n (host nth 0))
      (let x (call check n))
      (do (post p 7))
      (ret x))
    (proc supervised (p) eff
      (let v (call supervise! 1 (clo flaky p)))
      (ret v))
    (proc main () eff
      (let mb (mailbox)) (let p (field mb 0)) (let b (field mb 1))
      (let r (call both! (clo reader b 0) (clo supervised p)))
      (ret r))
    """

    def test_a_crash_releases_the_posts_its_task_held(self):
        # The first attempt crashes with a Post in a running frame. The inbox
        # closes once no live task holds a Post, so the reader ends.
        _, lows = build(self.LIB + self.FLAKY)
        self.assertEqual(all_results(lows, ALL_POLICIES[1:]), {("done", ("tup", None, (7, 0)))})
        with switches(sessions={"no_close_on_leave": True}):
            self.assertEqual(tasks(lows, PausePolicy()).result, ("crashed", ("str", sessions_mod.DEADLOCK)))

    ABANDONED = """
    (proc server (b) eff
      (let r (take b))
      (match r
        (Got (o rest) (block (do (send o 9)) (ret 0)))
        (Closed () (block (ret 3)))))
    (proc requester (p) eff
      (let rp (pair)) (let o (field rp 0)) (let i (field rp 1))
      (do (post p o))
      (let r (recv i))
      (match r
        (Got (x) (block (ret 1)))
        (Cancelled () (block (ret 2)))))
    (proc main () eff
      (let mb (mailbox)) (let p (field mb 0)) (let b (field mb 1))
      (let w (call race! (clo fast) (list (clo server b))))
      (let v (call requester p))
      (ret (tag R w v)))
    """

    def test_a_request_to_a_reader_that_was_cancelled_is_answered_with_cancelled(self):
        none, results, lows = self.everywhere(self.ABANDONED)
        want = ("done", ("tag", "R", (1, 2)))
        self.assertEqual(none.result, want)
        self.assertEqual(results, {want})
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.runtime["receives abandoned"], 1)  # the server was waiting at its take
        with switches(sessions={"no_close_abandoned": True}):
            with self.assertRaises((HeapError, AssertionError)):
                r = tasks(lows, PausePolicy())
                self.assertEqual(r.result, want)

    def test_a_reply_credit_bounds_the_queue(self):
        # Backpressure as a protocol: each sender waits for a reply before
        # sending again, so at most one message per sender is queued. Without
        # it the queue holds everything a slow reader has not taken yet.
        src = """
        (proc slow_reader (b acc) eff
          (let r (take b))
          (match r
            (Got (m rest) (block
              (let v (field m 0))
              (let ack (field m 1))
              (let w (host slow v))
              (do (send ack 0))
              (tail slow_reader rest (add acc v))))
            (Closed () (block (ret acc)))))
        (proc credit_poster (p k n) eff
          (let z (eq n 0))
          (if z
            (block (ret 0))
            (block
              (let pr (pair)) (let ack (field pr 0)) (let back (field pr 1))
              (do (post p (tup k ack)))
              (let r (recv back))
              (tail credit_poster p k (sub n 1)))))
        (proc flood_poster (p k n) eff
          (let z (eq n 0))
          (if z
            (block (ret 0))
            (block
              (let pr (pair)) (let ack (field pr 0)) (let back (field pr 1))
              (do (post p (tup k ack)))
              (tail flood_poster p k (sub n 1)))))
        (proc run (p which) eff
          (let n (int 4))
          (if which
            (block (let r (call all! (list (clo credit_poster p 1 n) (clo credit_poster p 2 n) (clo credit_poster p 3 n)))) (ret 0))
            (block (let r (call all! (list (clo flood_poster p 1 n) (clo flood_poster p 2 n) (clo flood_poster p 3 n)))) (ret 0))))
        (proc main () eff
          (let a (call one 1))
          (let b (call one 0))
          (ret (tup a b)))
        (proc one (which) eff
          (let mb (mailbox)) (let p (field mb 0)) (let b (field mb 1))
          (let r (call both! (clo slow_reader b 0) (clo run p which)))
          (ret (field r 0)))
        """
        _, lows = build(self.LIB + src)
        seen = []
        orig = sessions_mod.Sessions.post

        def post(ses, p, v):
            orig(ses, p, v)
            b = ses.boxes[max(ses.boxes)]
            seen.append((b.bid, len(b.queue)))

        sessions_mod.Sessions.post = post
        try:
            r = tasks(lows, PausePolicy())
        finally:
            sessions_mod.Sessions.post = orig
        self.assertEqual(r.result, ("done", ("tup", None, (24, 24))))
        most = {bid: max(n for b, n in seen if b == bid) for bid, _ in seen}
        self.assertEqual(most, {1: 3, 2: 12})


class Libraries(unittest.TestCase):
    """The semaphore and pool libraries of concurrency-sessions.md, and the
    semaphore cell experiment ("Decisions left to prototypes")."""

    SEM = """
    (proc job (k) eff (let a (host work k)) (ret a))
    (proc quick () eff (let a (host slow 0)) (ret 0))
    (proc client (requests k) eff (let v (call with_permit requests (clo job k))) (ret v))
    (proc timed (requests k) eff
      (let w (call race! (clo client requests k) (list (clo quick))))
      (ret w))
    (proc clients (requests) eff
      (let r (call all! (list (clo client requests 1) (clo timed requests 2) (clo client requests 3))))
      (ret r))
    (proc main () eff
      (let r (call sem_with 1 (clo clients)))
      (ret (field r 1)))
    """

    def build(self, src):
        return build(library.SOURCE + src)[1]

    def test_every_permit_comes_back_however_its_holder_ends(self):
        # The semaphore's free count is back to its permits at the end, under
        # every schedule, when the timed client is cancelled at any point.
        lows = self.build(self.SEM)
        self.assertEqual(all_results(lows, ALL_POLICIES[1:]), {("done", 1)})
        for k in range(1, 9):
            for mode in ("lss", "boxy"):
                r = tasks(lows, CancelAtPolicy(k), mode)
                self.assertIn(r.result, {("done", 1), ("cancelled",)})

    def test_turn_taking_deadlocks_on_one_stack(self):
        lows = self.build(self.SEM)
        dead = ("crashed", ("str", sessions_mod.DEADLOCK))
        self.assertEqual(run_none(lows["lss"].prog).result, dead)
        self.assertEqual(tasks(lows, InputOrderPolicy()).result, dead)

    def coroutines(self, src):
        """Option (a): the None build as coroutines on one stack. Hosted calls
        never suspend; every engine and lowering gives one result."""
        from host import NONE_HOSTED
        from lower import Lowering
        arc, _ = build(src)
        out = set()
        for mode in ("lss", "boxy"):
            low = Lowering(arc, NONE_HOSTED, mode)
            low.certify()
            for eng in ("unwind", "stack"):
                out.add(run_tasks(low, eng, host_mod.CoroutinePolicy()).result)
        self.assertEqual(len(out), 1, out)
        return out.pop()

    def test_coroutines_on_one_stack_run_turn_taking(self):
        # concurrency-sessions.md, "Decisions left to prototypes", option (a):
        # a receive that would block switches to the writer, so the
        # semaphore, the pool and the request-reply-release server, which
        # deadlock on one stack, all finish.
        self.assertEqual(self.coroutines(library.SOURCE + self.SEM), ("done", 1))
        self.assertEqual(self.coroutines(library.SOURCE + self.POOL), ("done", 2))
        # The semaphore server is an entry of the outer loop, which main is
        # not waiting on while it races the client against the timeout, so
        # it runs only once nothing else can: the timeout wins, and the
        # permit comes back as Cancelled.
        self.assertEqual(self.coroutines(Sessions.LIB + Sessions.PERMIT), ("done", ("tag", "R", (2, 8))))
        # A real cycle of receives still deadlocks.
        self.assertEqual(self.coroutines(Sessions.LIB + Sessions.CYCLE),
                         ("crashed", ("str", sessions_mod.DEADLOCK)))

    REMOTE = """
    (proc quick () eff (let a (host slow 0)) (ret 0))
    (proc late (requests k) eff
      (let w (host slow 3))
      (let v (call {use} requests k))
      (ret v))
    (proc local (requests k) eff
      (let v (call with_permit requests (clo job k)))
      (ret v))
    (proc job (k) eff (let a (host work k)) (ret a))
    (proc timed (requests k) eff
      (let w (call race! (clo {use} requests k) (list (clo quick))))
      (ret w))
    (proc clients (requests) eff
      (let r (call all! (list (clo timed requests 1) (clo late requests 2))))
      (ret r))
    (proc main () eff
      (let r (call sem_with 1 (clo clients)))
      (ret (field r 1)))
    """

    def test_a_permit_passed_to_a_hosted_call_counts_remote_operations(self):
        # The RFC's table: A takes the only permit and starts a remote
        # operation, A times out locally, B starts. A permit kept in the
        # caller's scope returns when A's scope ends, so B's operation runs
        # while A's is still in progress. A permit passed to the hosted call
        # returns only when the host releases A's arguments, after A's
        # operation finishes (P5), so the two never overlap.
        class Keep(host_mod.Host):
            keep_aborted_args = True

        most = {}
        for use, op in (("local", "work"), ("with_permit_remote", "remote")):
            lows = self.build(self.REMOTE.replace("{use}", use))
            peak = 0
            for mk in ALL_POLICIES[1:]:
                pol = mk()
                r = run_tasks(lows["lss"], "unwind", pol, "ParallelTasks" if pol.parallel else "Tasks", host_cls=Keep)
                self.assertEqual(r.result, ("done", 1))
                peak = max(peak, r.host["most " + op + " at once"])
            most[use] = peak
        self.assertEqual(most, {"local": 2, "with_permit_remote": 1})

    POOL = """
    (proc use_res (k res) eff (let a (host work res)) (ret (add a k)))
    (proc quick () eff (let a (host slow 0)) (ret 0))
    (proc borrower (requests k) eff (let v (call with_resource requests (clo use_res k))) (ret v))
    (proc timed (requests k) eff
      (let w (call race! (clo borrower requests k) (list (clo quick))))
      (ret w))
    (proc body (requests) eff
      (let r (call all! (list (clo borrower requests 1) (clo timed requests 2) (clo borrower requests 3)
                              (clo timed requests 4))))
      (ret r))
    (proc main () eff
      (let r (call pool_with 2 (clo body)))
      (let st (field r 1))
      (ret (field st 0)))
    """

    def test_a_pool_replaces_a_resource_whose_lease_was_cancelled(self):
        lows = self.build(self.POOL)
        self.assertEqual(all_results(lows, ALL_POLICIES[1:]), {("done", 2)})
        r = tasks(lows, PausePolicy())
        self.assertGreaterEqual(r.runtime["Cancelled delivered"], 1)

    CELL = """
    (proc job (k) eff (let a (host work k)) (ret a))
    (proc quick () eff (let a (host slow 0)) (ret 0))
    (proc client (s k) eff (let v (call {with} s (clo job k))) (ret v))
    (proc timed (s k) eff
      (let w (call race! (clo client s k) (list (clo quick))))
      (ret w))
    (proc main () eff
      (let s (sem 1))
      (let r (call all! (list (clo client s 1) ({second} s 2) (clo client s 3))))
      (let p (acquire s))
      (do (call release p))
      (ret r))
    """

    def test_a_semaphore_cell_runs_on_one_stack_and_returns_every_permit(self):
        for second in ("clo client", "clo timed"):
            lows = self.build(self.CELL.replace("{with}", "with_cell_permit").replace("{second}", second))
            none = none_build(lows)
            self.assertEqual(none.result[0], "done")
            io = tasks(lows, InputOrderPolicy())
            self.assertEqual((none.result, none.effects), (io.result, io.effects))
            results = all_results(lows, ALL_POLICIES[1:])
            self.assertTrue(all(r[0] == "done" for r in results), results)
            for k in range(1, 7):
                self.assertIn(tasks(lows, CancelAtPolicy(k)).result[0], {"done", "cancelled"})

    CELL_CRASH = """
    (proc check (n) eff
      (let z (eq n 0))
      (if z (block (crash "first try")) (block (ret 0))))
    (proc attempt (s) eff
      (let permit (acquire s))
      (let n (host nth 0))
      (let x (call check n))
      (do (call release permit))
      (ret x))
    (proc main () eff
      (let s (sem 1))
      (let v (call supervise! 1 (clo attempt s)))
      (let p (acquire s))
      (do (call release p))
      (ret v))
    """

    def test_a_crash_returns_the_permit_its_running_frame_held(self):
        # The first attempt crashes holding the only permit in a running
        # frame. Its holder set empties, so the permit returns, and the
        # restarted attempt can take it.
        lows = self.build(self.CELL_CRASH)
        self.assertEqual(all_results(lows, ALL_POLICIES[1:]), {("done", 0)})
        # Without that, the restart deadlocks, and the permit is still out
        # when every task has finished.
        with switches(sessions={"no_close_on_leave": True}):
            with self.assertRaisesRegex(Violation, "S9"):
                tasks(lows, PausePolicy())

    def test_a_permit_dropped_at_its_last_use_is_released_before_f_runs(self):
        peak = {}
        for w in ("with_cell_permit", "with_cell_permit_early"):
            lows = self.build(self.CELL.replace("{with}", w).replace("{second}", "clo client"))
            peak[w] = tasks(lows, PausePolicy()).host["most work at once"]
        self.assertEqual(peak, {"with_cell_permit": 1, "with_cell_permit_early": 3})


class Roots(unittest.TestCase):
    """A service root beside request roots (concurrency-sessions.md, "Services across roots")."""

    SRC = """
    (proc job (k) eff (let a (host work k)) (ret a))
    (proc boom () eff (crash "boom"))
    (proc service (inbox) eff (let r (call sem_serve inbox 1)) (ret r))
    (proc request (requests k) eff (let v (call with_permit requests (clo job k))) (ret v))
    (proc request_crash (requests k) eff (let v (call with_permit requests (clo boom))) (ret v))
    (proc waiter (i o) eff
      (let r (recv i))
      (do (send o 1))
      (ret 0))
    (proc request_cycle (requests k) eff
      (let p1 (pair)) (let o1 (field p1 0)) (let i1 (field p1 1))
      (let p2 (pair)) (let o2 (field p2 0)) (let i2 (field p2 1))
      (let r (call both! (clo waiter i1 o2) (clo waiter i2 o1)))
      (ret 0))
    """

    REQUESTS = [("request", (1,)), ("request_crash", (2,)), ("request_cycle", (3,)), ("request", (4,))]

    def run_all(self, per_root=True, policies=None):
        lows = build(library.SOURCE + self.SRC)[1]
        out = set()
        for mk in policies or ALL_POLICIES[1:]:
            pol = mk()
            res, rts, hs = host_mod.run_service(lows["lss"], "unwind", pol, "service", self.REQUESTS,
                                                "ParallelTasks" if pol.parallel else "Tasks", per_root)
            out.add(tuple(res))
        return out

    def test_a_service_root_outlives_its_requests_and_each_root_deadlocks_alone(self):
        dead = ("crashed", ("str", sessions_mod.DEADLOCK))
        want = (("done", 1), ("done", 101), ("crashed", ("str", "boom")), dead, ("done", 104))
        self.assertEqual(self.run_all(), {want})
        # Without per-root detection the cycle is found only once every other
        # root has finished, with the same results.
        self.assertEqual(self.run_all(per_root=False), {want})

    def test_a_root_waiting_on_a_port_another_root_holds_is_not_stalled(self):
        # A request waits for its grant, whose reply end the service root
        # holds. Detection that ignores holders in other roots crashes every
        # request that ever waits for one.
        with switches(rt={"stall_ignores_other_roots": True}):
            (r,) = self.run_all(policies=[lambda: PausePolicy()])
        dead = ("crashed", ("str", sessions_mod.DEADLOCK))
        self.assertEqual((r[1], r[4]), (dead, dead))

    def test_a_crashed_request_root_returns_its_permit(self):
        with switches(sessions={"no_close_on_leave": True}):
            res = self.run_all(policies=[lambda: PausePolicy()])
        (r,) = res
        self.assertEqual(r[0], ("crashed", ("str", sessions_mod.DEADLOCK)))  # the service waited forever


class Crashes(unittest.TestCase):
    SRC = """
    (proc bad (s) eff (let x (host slow 1)) (let l (len s)) (crash "boom"))
    (proc mid (s) eff
      (let t (str "kept"))
      (let v (call bad s))
      (ret (tup v t)))
    (proc sibling () eff (let a (host slow 1)) (let b (host slow a)) (ret (host slow b)))
    (proc main () eff
      (let s (list 1 2 3))
      (let r (call all! (list (clo sibling) (clo mid s) (clo sibling))))
      (ret r))
    """

    def test_crash_cancels_siblings_and_propagates_its_message(self):
        _, lows = build(self.SRC)
        for seed in range(30):
            for eng in ("unwind", "stack"):
                r = run_tasks(lows["lss"], eng, RandomPolicy(seed, parallel=True), "ParallelTasks")
                self.assertEqual(r.result, ("crashed", ("str", "boom")))

    def test_only_running_native_frames_leak(self):
        _, lows = build(self.SRC)
        # Paused before the crash: mid is a record when bad crashes, and its
        # record is dropped exactly, so nothing leaks.
        self.assertEqual(tasks(lows, PausePolicy(), engine="unwind").leaked_live, 0)
        # Never paused: mid's frame is on the native stack, and its `t` leaks.
        self.assertEqual(tasks(lows, ReadyPolicy(), engine="unwind").leaked_live, 1)
        # A stackful runtime leaks every frame of the crashed task.
        self.assertEqual(tasks(lows, PausePolicy(), engine="stack").leaked_live, 1)
        # heap.verify() inside run_tasks checked each time that nothing else leaked.


class Cancellation(unittest.TestCase):
    def test_cancel_at_every_suspension_releases_everything(self):
        src = Combinators.MAIN.format(comb="all_ok!")
        _, lows = prog(src)
        n = tasks(lows, PausePolicy()).host["pending"]
        self.assertGreater(n, 5)
        for k in range(1, n + 1):
            for mode in ("lss", "boxy"):
                r = run_tasks(lows[mode], "unwind", CancelAtPolicy(k))
                self.assertIn(r.result[0], ("cancelled", "done"))


class Wake(unittest.TestCase):
    """The race! program makes the parent cancel a parked child and read its
    Cancelled report synchronously: the wake that cancellation sends while the
    parent runs is consumed at once."""

    SRC = """
    (proc fast () eff (ret 1))
    (proc slow1 () eff (ret (host slow 1)))
    (proc main () eff
      (let a (call race! (clo fast) (list (clo slow1) (clo slow1))))
      (let b (call race! (clo fast) (list (clo slow1))))
      (ret (add a b)))
    """

    def run_with(self, protocol):
        old = runtime.PROTOCOL
        runtime.PROTOCOL = protocol
        try:
            _, lows = build(self.SRC)
            return [tasks(lows, PausePolicy()).result,
                    tasks(lows, RandomPolicy(3, parallel=True), conc="ParallelTasks").result]
        finally:
            runtime.PROTOCOL = old

    def test_flag_protocol_resumes_with_nothing_to_deliver(self):
        with self.assertRaisesRegex(Violation, "R1"):
            self.run_with("flag")

    def test_counted_protocol(self):
        self.assertEqual(self.run_with("counted"), [("done", 2), ("done", 2)])


class Mutations(unittest.TestCase):
    def test_certifier_rejects_a_short_ledger(self):
        arc, lows = prog("(proc main () eff (tail fetch_two 1 2))")
        sid = next(s for s, l in arc.ledger.items() if l == ("b",))
        arc.ledger[sid] = ()
        with self.assertRaises(ArcError):
            certify(arc)

    def test_c3_rejects_a_short_record(self):
        arc, lows = prog("(proc main () eff (tail fetch_two 1 2))")
        low = lows["lss"]
        sid = next(s for s, i in low.sites.items() if i[4] == ("b",))
        p, b, i, x, _ = low.sites[sid]
        low.sites[sid] = (p, b, i, x, ())
        with self.assertRaises(LowerError):
            low.certify()

    def test_skipped_drop_entries_leak(self):
        _, lows = prog(Combinators.MAIN.format(comb="all_ok!"))
        orig = engines.UnwindEngine.drop_paused
        engines.UnwindEngine.drop_paused = lambda self, t: None
        try:
            with self.assertRaises(HeapError):
                for k in range(1, 8):
                    run_tasks(lows["lss"], "unwind", CancelAtPolicy(k))
        finally:
            engines.UnwindEngine.drop_paused = orig

    def test_missing_site_trips_c2(self):
        _, lows = prog("(proc main () eff (tail fetch_two 1 2))")
        low = lows["lss"]
        low.sites.pop(next(iter(s for s, i in low.sites.items() if i[0] == "fetch_two")))
        with self.assertRaisesRegex(AssertionError, "C2"):
            run_tasks(low, "unwind", PausePolicy())

    def test_off_by_one_restart_limit_trips_r7(self):
        import combinators
        from ir import parse
        src = """
        (proc bad () eff (let x (host slow 1)) (crash "boom"))
        (proc main () eff (ret (call supervise! 2 (clo bad))))
        """
        broken = parse(combinators.SOURCE.replace("(let c (lt k max))", "(let c (lt k (add max 1)))"))
        orig = dict(combinators.BUILTINS)
        combinators.BUILTINS.update(broken)
        try:
            _, lows = build(src)
            with self.assertRaisesRegex(Violation, "R7"):
                tasks(lows, PausePolicy())
        finally:
            combinators.BUILTINS.clear()
            combinators.BUILTINS.update(orig)

    def test_pending_cell_in_a_native_frame_trips_c4(self):
        _, lows = build(Lowering.CLOSURE_LOOP)
        engines.UnwindEngine.cells_in_frames = True
        try:
            with self.assertRaisesRegex(AssertionError, "C4"):
                tasks(lows, PausePolicy(), "boxy")
            # Nothing pauses, so no record ever holds the cell.
            tasks(lows, ReadyPolicy(), "boxy")
        finally:
            engines.UnwindEngine.cells_in_frames = False

    def test_double_resume_is_a_host_bug(self):
        _, lows = prog("(proc main () eff (tail fetch_two 1 2))")

        class Twice(PausePolicy):
            def choose(self, host):
                act = super().choose(host)
                if act and act[0] == "resume":
                    host.queue.append(host.queue[act[1]])
                return act

        with self.assertRaisesRegex(Violation, "P1"):
            run_tasks(lows["lss"], "unwind", Twice())

    def test_write_after_abort_is_a_host_bug(self):
        from host import Host
        _, lows = prog(Combinators.MAIN.format(comb="race!").replace("(call race! fs)",
                                                                    "(call race! (clo child 1 1) fs)"))
        orig = Host.complete

        def complete(self, op):
            op.aborted = False if op.args is not None else op.aborted
            if op.aborted:
                self.ops.remove(op)
                self.rt.write_slot(op.task, 0)
                return
            orig(self, op)

        Host.complete = complete
        try:
            for seed in range(20):
                try:
                    tasks(lows, RandomPolicy(seed))
                except Violation as v:
                    self.assertRegex(str(v), "P3/P4|R4")
                    return
        finally:
            Host.complete = orig
        self.fail("no abort raced a completion")


class Conformance(unittest.TestCase):
    def test_every_broken_host_is_caught(self):
        import conformance
        results = list(conformance.suite())
        self.assertEqual({o for o, *_ in results}, {"P1", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"})
        for obligation, what, caught, tag in results:
            with self.subTest(what=what):
                self.assertIsNotNone(caught, f"{obligation}: a host that {what} went unnoticed")
                self.assertTrue(caught.startswith(tag), caught)


    def test_a_region_host_reclaims_crash_leaks(self):
        import conformance
        _, lows = build(conformance.SCENARIOS["crash_below_a_caller"])
        plain = tasks(lows, ReadyPolicy())
        region = run_tasks(lows["lss"], "unwind", ReadyPolicy(), host_cls=conformance.RegionHost)
        self.assertEqual(plain.leaked_live, 1)  # main's native frame held "kept"
        self.assertEqual((region.leaked_live, region.result), (0, plain.result))
        self.assertGreaterEqual(region.heap["region freed"], 1)


class Fuzz(unittest.TestCase):
    def test_generated_programs(self):
        stats = Counter()
        for seed in range(1, 61):
            check_program(seed, stats, cancel_sites=3)
        self.assertEqual(stats["programs"], 60)
        self.assertGreater(stats["crashes"], 0)
        self.assertGreater(stats["cancel-at runs"], 0)
        self.assertGreater(stats["shared: waits"], 0)
        self.assertGreater(stats["shared: deferred steps run at drop"], 0)
        self.assertGreater(stats["shared: cells poisoned by an update"], 0)
        for f in ("background", "loop spawn under a root limit", "plan.all!", "plan.all_ok!"):
            self.assertGreater(stats["feature " + f], 5, f)
        self.assertGreater(stats["loop: loops ended with background entries left"], 0)
        self.assertGreater(stats["loop: loop spawns under the root limit"], 0)

    def test_generated_plans_catch_mutations_of_g9(self):
        # Background entries that keep their loop going change what a None
        # build runs (C9); spawned entries outside the root limit trip G9.
        for sw, err in (("ignore_background", Failure), ("spawn_outside_root_limit", Violation)):
            with switches(rt={sw: True}):
                with self.assertRaises(err, msg=sw):
                    for seed in range(1, 61):
                        check_program(seed, Counter(), cancel_sites=1)


class RcAtomicity(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        sys.path.insert(0, os.path.join(RC, "proto"))

    def test_scenarios(self):
        import scenarios
        from fuzz import check as rc_check
        want = {  # false atomics: unify forward forward_ctx forward_pts forward_edge lean; racing after lending
            "spreading": (2081, 1950, 2, 2, 2, 3, 2),
            "moves": (1532, 166, 166, 0, 0, 210, 0),
            "path_copy_shared": (899, 459, 70, 70, 0, 23, 144),
            "shared_read": (705, 0, 0, 0, 0, 23, 0),
            "host_data": (242, 242, 242, 242, 242, 0, 2),
        }
        for make in scenarios.SCENARIOS:
            results, order = rc_check(make())
            got = tuple(results[n][0].false_atomic for n in
                        ["unify", "forward", "forward_ctx", "forward_pts", "forward_edge", "lean"])
            got += (results["lent_pts"][0].racy,)
            self.assertEqual(got, want[make.__name__], make.__name__)
            self.assertTrue(all(order))
            self.assertTrue(all(results[n][0].unsound == 0 for n in results if n != "none"))

    def test_fuzz_sound_and_ordered(self):
        from fuzz import run
        totals, failures, order_fail, _ = run(300, 2)
        self.assertEqual(failures, [])
        self.assertEqual(order_fail, [0, 0, 0, 0])

    def test_sent_values_are_seeds(self):
        # S8: a message sent between two children of a fork is reached by
        # both while the sender may still update its counts. Seeded like a
        # hosted argument, every analysis stays sound; unseeded, all go
        # unsound.
        import rcproto
        from fuzz import run
        totals, failures, order_fail, _ = run(300, 3, channels=True)
        self.assertEqual(failures, [])
        self.assertEqual(order_fail, [0, 0, 0, 0])
        rcproto.SEND_SEEDS = False
        try:
            _, failures, _, _ = run(300, 3, channels=True)
        finally:
            rcproto.SEND_SEEDS = True
        self.assertEqual({n for _, n, _ in failures} >= {"unify", "forward", "forward_ctx", "forward_pts"}, True)

    def test_happens_before_oracle_matches_series_parallel_without_channels(self):
        import random
        import rcproto
        from fuzz import gen_program
        rng = random.Random(5)
        for _ in range(300):
            prog = gen_program(rng)
            m = rcproto.Machine(prog, rcproto.Forward(prog))
            m.run()
            self.assertEqual(rcproto.racy_events(m.events), rcproto.racy_events_hb(m.events))


def maude_results(path):
    out = subprocess.run(["maude", "-no-banner", "-batch", os.path.basename(path)], cwd=os.path.dirname(path),
                         stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=600).stdout
    return re.findall(r"^(No solution\.|Solution 1 )", out, re.M)


@unittest.skipUnless(shutil.which("clang"), "clang not installed")
class ThreadSanitizer(unittest.TestCase):
    """bench/wake_tsan.c: the wake protocol on real threads."""

    def run_c(self, *flags, rounds=20000):
        import tempfile
        src = os.path.join(HERE, "bench", "wake_tsan.c")
        with tempfile.TemporaryDirectory() as d:
            exe = os.path.join(d, "wake")
            subprocess.run(["clang", "-O1", "-g", "-fsanitize=thread", *flags, src, "-o", exe], check=True)
            p = subprocess.run([exe, str(rounds), "4"], capture_output=True, text=True, timeout=600,
                               env=dict(os.environ, TSAN_OPTIONS="halt_on_error=0"))
        races = set(re.findall(r"SUMMARY: ThreadSanitizer: data race \S+:\d+ in (\w+)", p.stderr))
        return p.returncode, p.stdout + p.stderr, races

    def test_counted_wake_word_is_race_free_and_exact(self):
        code, out, races = self.run_c("-DPROTOCOL=0")
        self.assertEqual((code, races), (0, set()), out)
        parks, readies = re.search(r"parks (\d+), readies (\d+)", out).groups()
        self.assertEqual(parks, readies)

    def test_flag_protocols_break_r1_on_real_threads(self):
        for p in ("1", "2"):
            code, out, _ = self.run_c(f"-DPROTOCOL={p}")
            self.assertEqual(code, 1, out)
            self.assertIn("violation: R1", out)

    def test_the_wake_word_orders_resumes_and_p2_orders_first_runs(self):
        # A host that hands tasks over with relaxed atomics (breaking P2) races
        # only on a child's first run: every resume after a wake is already
        # ordered by the wake word's acquire.
        _, _, races = self.run_c("-DBROKEN_HANDOFF")
        self.assertTrue(races)
        self.assertTrue(races <= {"dequeue_child", "run_child", "push_report"}, races)
        # Relax the runtime's side too, and the parent's own state races.
        _, _, races = self.run_c("-DBROKEN_HANDOFF", "-DRELAXED_RESUME")
        self.assertTrue(races & {"deliver", "resume_parent", "await_any"}, races)
        # With the host's handoff ordered, the relaxed runtime is race-free: P2 alone suffices.
        code, out, races = self.run_c("-DRELAXED_RESUME", rounds=5000)
        self.assertEqual((code, races), (0, set()), out)


@unittest.skipUnless(shutil.which("maude"), "maude not installed")
class Maude(unittest.TestCase):
    def test_wake_protocols(self):
        got = maude_results(os.path.join(HERE, "maude", "wake.maude"))
        want = ["Solution 1 ", "Solution 1 ", "No solution.", "No solution.", "No solution.",
                "No solution.", "No solution.", "Solution 1 ",
                # SOURCES: one wake word per task breaks R1; one per wait source does not.
                "Solution 1 ", "No solution.", "No solution."]
        self.assertEqual(got, want)

    def test_rc_model(self):
        got = maude_results(os.path.join(RC, "maude", "checks.maude"))
        self.assertEqual(got.count("No solution."), 12)
        self.assertEqual(len(got), 15)


@unittest.skipUnless(shutil.which("lean"), "lean not installed")
class Lean(unittest.TestCase):
    def test_proofs_check(self):
        r = subprocess.run(["lean", "RcAtomicity.lean"], cwd=os.path.join(RC, "lean"), capture_output=True,
                           text=True, timeout=900)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertNotIn("sorry", r.stdout + r.stderr)



def everywhere(lows, policies=None):
    """Every result: both lowerings and engines, under every schedule."""
    out = set()
    for mk in policies or ALL_POLICIES:
        for mode in ("lss", "boxy"):
            for eng in ("unwind", "stack"):
                pol = mk()
                out.add(run_tasks(lows[mode], eng, pol, "ParallelTasks" if pol.parallel else "Tasks").result)
    return out


class SharedVariables(unittest.TestCase):
    """Shared state: Shared.new!, update! (`step`), waiting, finally! and
    on_failure! (`defer` and `errdefer`) and poisoning
    (concurrency-shared-variables.md), and the semaphore, pool and breaker
    written with them (library.SHARED_SOURCE)."""

    def build(self, src, entry="main"):
        return build(library.SOURCE + library.SHARED_SOURCE + src, entry)[1]

    SEM = """
    (proc job (k) eff (let a (host work k)) (ret a))
    (proc quick () eff (let a (host slow 0)) (ret 0))
    (proc client (s k) eff (let v (call sv_with_permit s (clo job k))) (ret v))
    (proc timed (s k) eff (let w (call race! (clo client s k) (list (clo quick)))) (ret w))
    (proc main () eff
      (let s (shared 1))
      (let r (call all! (list (clo client s 1) (clo timed s 2) (clo client s 3))))
      (let n (read s))
      (ret n))
    """

    def test_every_permit_comes_back_however_its_holder_ends(self):
        # The count is back to 1 at the end, under every schedule and when the
        # root is cancelled at any suspension; at most one job runs at once.
        lows = self.build(self.SEM)
        self.assertEqual(everywhere(lows), {("done", 1)})
        self.assertEqual(none_build(lows).result, ("done", 1))
        # Without a race, no cancelled holder leaves aborted work running
        # after it gives the permit back (Finding 9), so one job runs at once.
        plain = self.build(self.contended(3))
        for mk in ALL_POLICIES:
            pol = mk()
            r = run_tasks(plain["lss"], "unwind", pol, "ParallelTasks" if pol.parallel else "Tasks")
            self.assertEqual(r.host["most work at once"], 1)
        for k in range(1, 9):
            for mode in ("lss", "boxy"):
                r = tasks(lows, CancelAtPolicy(k), mode)
                self.assertIn(r.result, {("done", 1), ("cancelled",)})
        r = tasks(lows, PausePolicy())
        self.assertGreaterEqual(r.runtime["waits"], 1)
        self.assertGreaterEqual(r.runtime["steps run again"], 1)

    CRASH_HOLDING = """
    (proc job (k) eff
      (let a (host work k))
      (let c (host nth k))
      (let first (eq c 0))
      (if first (block (crash "boom")) (block (ret a))))
    (proc client (s k) eff (let v (call sv_with_permit s (clo job k))) (ret v))
    (proc main () eff
      (let s (shared 1))
      (let v (call supervise! 1 (clo client s 7)))
      (let n (read s))
      (ret (tup v n)))
    """

    def test_a_crash_while_holding_runs_the_deferred_step(self):
        # The first attempt crashes inside f, holding the only permit. Its
        # deferred step runs once its task is gone, so the restart can take
        # the permit.
        lows = self.build(self.CRASH_HOLDING)
        self.assertEqual(everywhere(lows), {("done", ("tup", None, (107, 1)))})
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.runtime["deferred steps run at drop"], 1)
        # Without running it, the restart waits for a permit nobody can give.
        with switches(rt={"skip_deferred_on_drop": True}):
            r = tasks(lows, PausePolicy())
        self.assertEqual(r.result, ("crashed", ("str", shared_mod.WAIT_DEADLOCK)))

    def test_a_cancelled_holder_returns_its_permit_and_a_cancelled_waiter_takes_none(self):
        # race! cancels the client that loses, whether it holds the permit or
        # still waits for it.
        lows = self.build(self.SEM)
        seen = Counter()
        for mk in ALL_POLICIES:
            pol = mk()
            r = run_tasks(lows["lss"], "unwind", pol, "ParallelTasks" if pol.parallel else "Tasks")
            seen["deferred at drop"] += r.runtime["deferred steps run at drop"]
            seen["cancelled tasks"] += r.runtime["cancelled"]
            self.assertEqual(r.result, ("done", 1))
        self.assertGreater(seen["cancelled tasks"], 0)

    POISON = """
    (proc bad_give (n) pure (crash "give failed"))
    (proc holder (s) eff
      (do (step sv_take (s)))
      (do (defer bad_give (s)))
      (let a (host work 1))
      (do (undefer))
      (ret a))
    (proc after (s) eff (let n (read s)) (ret n))
    (proc attempt (s) eff
      (let c (host nth 0))
      (let first (eq c 0))
      (if first (block (tail holder s)) (block (tail after s))))
    (proc main () eff
      (let s (shared 2))
      (let r (call supervise! 1 (clo attempt s)))
      (ret r))
    (proc attempt_fresh () eff
      (let s (shared 2))
      (tail attempt s))
    (proc main_fresh () eff
      (let r (call supervise! 1 (clo attempt_fresh)))
      (ret r))
    """

    def test_a_deferred_step_that_crashes_poisons_its_variable(self):
        # The permit's release crashes, so the variable is poisoned: the
        # restart's read crashes, and the restarts run out.
        poisoned = ("crashed", ("str", shared_mod.POISONED_BY_CLEANUP))
        lows = self.build(self.POISON)
        self.assertEqual(everywhere(lows), {poisoned})
        # Declared inside the supervised function, the restart gets a fresh one.
        lows = self.build(self.POISON, "main_fresh")
        self.assertEqual(everywhere(lows), {("done", 2)})
        # Without poisoning, the restart silently sees one permit missing.
        with switches(shared={"no_poison": True}):
            r = tasks(self.build(self.POISON), PausePolicy())
        self.assertEqual(r.result, ("done", 1))

    WAITERS_POISONED = """
    (proc bad_give (n) pure (crash "give failed"))
    (proc holder (s) eff
      (do (step sv_take (s)))
      (do (defer bad_give (s)))
      (let a (host work 1))
      (do (undefer))
      (ret a))
    (proc attempt (s) eff
      (let c (host nth 0))
      (let first (eq c 0))
      (if first (block (tail holder s)) (block (ret 0))))
    (proc job () eff (let a (host work 2)) (ret a))
    (proc waiter (s) eff (let v (call sv_with_permit s (clo job))) (ret v))
    (proc main () eff
      (let s (shared 1))
      (let r (call all! (list (clo supervised s) (clo waiter s))))
      (ret r))
    (proc supervised (s) eff (let v (call supervise! 1 (clo attempt s))) (ret v))
    """

    def test_a_waiter_on_a_poisoned_variable_crashes(self):
        # The holder runs under its own supervisor, which restarts it, so its
        # crash does not cancel the waiter. If the waiter parked behind the
        # holder, the poisoning readies it and it crashes, instead of waiting
        # for a permit that will never come back. If it went first, it
        # finished before the permit was lost.
        poisoned = ("crashed", ("str", shared_mod.POISONED_BY_CLEANUP))
        lows = self.build(self.WAITERS_POISONED)
        self.assertEqual(everywhere(lows), {poisoned, ("done", ("list", None, (0, 102)))})

    CANCEL_CRASH = """
    (proc bad_give (n) pure (crash "give failed"))
    (proc holder (s) eff
      (do (step sv_take (s)))
      (do (defer bad_give (s)))
      (let a (host work 1))
      (let b (host work 2))
      (do (undefer))
      (ret a))
    (proc fail () eff (ret (tag Err "fail")))
    (proc holder_t (s) eff (let a (call holder s)) (ret (tag Ok a)))
    (proc main () eff
      (let s (shared 1))
      (let r (call all_ok! (list (clo holder_t s) (clo fail))))
      (ret r))
    """

    def test_a_cancelled_task_whose_deferred_step_crashes_crashes(self):
        # all_ok! cancels the holder at the first Err; its deferred step
        # crashes at the drop, which turns the cancellation into a crash that
        # reaches the loop's caller.
        lows = self.build(self.CANCEL_CRASH)
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.result, ("crashed", ("str", "give failed")))
        self.assertEqual(r.runtime["cancellations turned into crashes"], 1)

    HANDOFF = """
    (proc giver (s) eff (let a (host slow 1)) (do (step sv_give (s))) (ret a))
    (proc taker (s) eff (do (step sv_take (s))) (ret 5))
    (proc main () eff
      (let s (shared 0))
      (let r (call all! (list (clo taker s) (clo giver s))))
      (ret r))
    (proc stuck () eff
      (let s (shared 0))
      (do (step sv_take (s)))
      (ret 1))
    """

    def test_a_wait_ends_when_another_task_commits(self):
        lows = self.build(self.HANDOFF)
        want = ("done", ("list", None, (5, 8)))
        self.assertEqual(everywhere(lows), {want})
        # A None build switches to the giver while the taker waits, as for a
        # receive.
        none = none_build(lows)
        self.assertEqual(none.result, want)
        self.assertEqual(none.extra["engine"], "coroutine")
        # A commit that readied no waiter would leave the taker stuck.
        with switches(shared={"no_wake": True}):
            r = tasks(lows, PausePolicy())
        self.assertEqual(r.result, ("crashed", ("str", shared_mod.WAIT_DEADLOCK)))

    def test_a_wait_nothing_can_end_is_a_deadlock_in_every_build(self):
        lows = self.build(self.HANDOFF, "stuck")
        dead = ("crashed", ("str", shared_mod.WAIT_DEADLOCK))
        self.assertEqual(everywhere(lows), {dead})
        self.assertEqual(none_build(lows).result, dead)

    POOL = """
    (proc maybe_crash (res) eff
      (let a (host work res))
      (let c (host nth 99))
      (let first (eq c 0))
      (if first (block (crash "user crashed")) (block (ret a))))
    (proc use (res) eff (let a (host work res)) (ret a))
    (proc crashing_user (p) eff (let v (call {with} p (clo maybe_crash))) (ret v))
    (proc user (p) eff (let v (call {with} p (clo use))) (ret v))
    (proc sup_user (p) eff (let v (call supervise! 1 (clo crashing_user p))) (ret v))
    (proc main () eff
      (let p (shared (tup (list 1) 0)))
      (let r (call all! (list (clo sup_user p) (clo user p) (clo user p))))
      (let st (read p))
      (ret st))
    """

    def test_a_pool_replaces_a_resource_whose_user_crashed_only_with_errdefer(self):
        # Decision (b). With errdefer, a lease that ends by a crash loses its
        # resource, and a later borrower opens a replacement (host slow 50).
        # With a plain defer, the crashed user's resource goes back to the
        # free list, in whatever state the crash left it.
        opened = lambda r: sum(1 for e in r.effects if e[1] == "slow" and e[2] == (50,))
        lows = self.build(self.POOL.replace("{with}", "sv_with_resource"))
        for mk in ALL_POLICIES:
            pol = mk()
            r = run_tasks(lows["lss"], "unwind", pol, "ParallelTasks" if pol.parallel else "Tasks")
            self.assertEqual(r.result, ("done", ("tup", None, (("list", None, (57,)), 0))))
            self.assertEqual(opened(r), 1)
        lows = self.build(self.POOL.replace("{with}", "sv_with_resource_plain"))
        for mk in ALL_POLICIES:
            pol = mk()
            r = run_tasks(lows["lss"], "unwind", pol, "ParallelTasks" if pol.parallel else "Tasks")
            self.assertEqual(r.result, ("done", ("tup", None, (("list", None, (1,)), 0))))
            self.assertEqual(opened(r), 0)

    POOL_OPEN = """
    (proc use (res) eff (let a (host work res)) (ret a))
    (proc quick () eff (let a (host slow 0)) (ret 0))
    (proc user (p) eff (let v (call sv_with_resource p (clo use))) (ret v))
    (proc main () eff
      (let p (shared (tup (list) 1)))
      (let w (call race! (clo user p) (list (clo quick))))
      (let st (read p))
      (let free (field st 0))
      (let lost (field st 1))
      (let n (len free))
      (ret (add n lost)))
    """

    def test_a_lease_cancelled_while_opening_its_resource_loses_no_slot(self):
        # The pool has no free resource and one lost, so the borrower opens a
        # replacement (a suspension) and is raced against a quick call. However
        # the race ends, the pool still accounts for one resource.
        lows = self.build(self.POOL_OPEN)
        self.assertEqual(everywhere(lows), {("done", 1)})

    BREAKER = """
    (proc flaky (i) eff
      (let x (host slow i))
      (let lo (lt 3 i))
      (let hi (lt i 7))
      (if lo
        (block (if hi (block (ret (tag Err "down"))) (block (ret (tag Ok x)))))
        (block (ret (tag Ok x)))))
    (proc req (b i) eff (let r (call br_guarded b i (clo flaky i))) (ret r))
    (proc run_reqs (b i n acc) eff
      (let c (lt i n))
      (if c
        (block (let r (call req b i)) (let ok (call br_ok r)) (tail run_reqs b (add i 1) n (push acc ok)))
        (block (ret acc))))
    (proc main () eff
      (let b (shared (tag Closed 0)))
      (let r (call run_reqs b 0 14 (list)))
      (ret r))
    (proc fail () eff (ret (tag Err "fail")))
    (proc probe (b) eff (let r (call req b 5)) (ret (tag Ok 0)))
    (proc probe_main () eff
      (let b (shared (tag Open 0)))
      (let w (call all_ok! (list (clo probe b) (clo fail))))
      (let st (read b))
      (ret st))
    """

    def test_a_breaker_opens_rejects_probes_and_closes(self):
        # Requests 4, 5 and 6 fail: the third failure opens it until 11.
        # Requests 7 to 10 are rejected without calling through; 11 probes,
        # succeeds and closes it.
        lows = self.build(self.BREAKER)
        want = ("done", ("list", None, (1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)))
        self.assertEqual(everywhere(lows), {want})
        r = tasks(lows, PausePolicy())
        self.assertEqual(sum(1 for e in r.effects if e[1] == "slow"), 10)

    def test_a_cancelled_probe_reopens_the_breaker(self):
        # all_ok! cancels the probe at the other entry's Err. The deferred
        # step on the probing branch runs at the drop and reopens the breaker;
        # without it, the breaker would stay Probing and reject forever.
        lows = self.build(self.BREAKER, "probe_main")
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.result, ("done", ("tag", "Open", (10,))))
        self.assertEqual(r.runtime["deferred steps run at drop"], 1)
        with switches(rt={"skip_deferred_on_drop": True}):
            r = tasks(lows, PausePolicy())
        self.assertEqual(r.result, ("done", ("tag", "Probing", ())))

    STEP_CRASH = """
    (proc halve (n) pure
      (let big (lt 3 n))
      (if big (block (crash "too big")) (block (ret (tag Set 0 (sub n 1))))))
    (proc attempt (s) eff
      (let c (host nth 0))
      (let first (eq c 0))
      (if first (block (do (step halve (s))) (ret 0)) (block (let n (read s)) (ret n))))
    (proc main () eff
      (let s (shared 5))
      (let r (call supervise! 1 (clo attempt s)))
      (ret r))
    """

    def test_an_update_that_crashes_poisons_its_variable(self):
        # The first attempt's update crashes with the state moved out, so the
        # cell stays empty and is poisoned: the restart's read crashes, and
        # the poison names an update.
        poisoned = ("crashed", ("str", shared_mod.POISONED_BY_UPDATE))
        lows = self.build(self.STEP_CRASH)
        self.assertEqual(everywhere(lows), {poisoned})
        r = tasks(lows, PausePolicy())
        self.assertEqual((r.runtime["steps crashed"], r.runtime["cells poisoned by an update"]), (1, 1))
        # The earlier rule, which borrowed the state and kept it when the
        # update crashed: the restart reads the value from before the crash.
        with switches(shared={"no_poison": True}):
            self.assertEqual(everywhere(lows), {("done", 5)})

    STEP_CRASH_HEAP = """
    (proc check (n) pure
      (let big (lt 2 n))
      (if big (block (crash "too long")) (block (ret n))))
    (proc first_or_crash (xs) pure
      (let n (len xs))
      (let m (call check n))
      (ret (tag Keep m)))
    (proc main () eff
      (let xs (list 1 2 3))
      (let s (shared xs))
      (do (step first_or_crash (s)))
      (ret 0))
    """

    def test_the_state_of_an_update_that_crashes_leaks_with_its_frames(self):
        # The update form keeps the list across the call, for its Keep, so
        # when the call crashes the update's frame holds the only reference:
        # a crashed-frame leak (R5). Exact heap accounting finds it held by
        # the crashed frame, not missing a free, and the empty, poisoned cell
        # is freed with its last handle.
        lows = self.build(self.STEP_CRASH_HEAP)
        for mode in ("lss", "boxy"):
            r = tasks(lows, PausePolicy(), mode)
            self.assertEqual(r.result, ("crashed", ("str", "too long")))
            self.assertEqual(r.leaked_live, 1)  # the list

    UPDATE_CRASH_WAITERS = """
    (proc need (n) pure
      (let z (eq n 0))
      (if z (block (ret (tag Wait))) (block (ret (tag Keep n)))))
    (proc explode (n) pure (crash "update failed"))
    (proc waiter (s) eff (let v (step need (s))) (ret v))
    (proc attempt (s) eff
      (let c (host nth 0))
      (let first (eq c 0))
      (if first (block (let a (host work 1)) (do (step explode (s))) (ret a)) (block (ret 0))))
    (proc supervised (s) eff (let v (call supervise! 1 (clo attempt s))) (ret v))
    (proc main () eff
      (let s (shared 0))
      (let r (call all! (list (clo supervised s) (clo waiter s))))
      (ret r))
    """

    def test_a_waiter_on_a_variable_an_update_poisoned_crashes(self):
        # The waiter waits for a state that never comes. The crashing update
        # poisons the cell and readies the parked waiter, which crashes,
        # under every schedule.
        poisoned = ("crashed", ("str", shared_mod.POISONED_BY_UPDATE))
        lows = self.build(self.UPDATE_CRASH_WAITERS)
        self.assertEqual(everywhere(lows), {poisoned})
        r = tasks(lows, PausePolicy())
        self.assertEqual((r.runtime["waits"], r.runtime["poisoned steps"]), (1, 1))
        # Under the earlier rule the waiter is never readied: a deadlock.
        with switches(shared={"no_poison": True}):
            self.assertNotIn(poisoned, everywhere(lows))

    UPDATE_FORM = """
    (proc decide (xs k) pure
      (let n (len xs))
      (let full (lt k n))
      (if full (block (ret (tag Wait))) (block (ret (tag Keep n)))))
    (proc push_or_wait (xs k) pure
      (let n (len xs))
      (let room (lt n k))
      (if room (block (let ys (push xs n)) (ret (tag Set n ys))) (block (tail decide xs k))))
    (proc pusher (s) eff (let v (step push_or_wait (s) 3)) (ret v))
    (proc reader (s) eff (let v (step decide (s) 9)) (ret v))
    (proc main () eff
      (let xs (list))
      (let s (shared xs))
      (let a (call pusher s))
      (let b (call pusher s))
      (let c (call reader s))
      (let d (call pusher s))
      (let e (call reader s))
      (ret (list a b c d e)))
    """

    def test_update_forms_put_back_the_state_they_were_given(self):
        # push_or_wait builds Set in place and tail-calls decide for Keep, so
        # its update form matches on decide's tag (V10). The runtime asserts
        # that every Keep and Wait puts back the very state it moved out.
        lows = self.build(self.UPDATE_FORM)
        self.assertEqual(everywhere(lows), {("done", ("list", None, (0, 1, 2, 2, 3)))})
        arc, _ = build(library.SOURCE + library.SHARED_SOURCE + self.UPDATE_FORM)
        procs = arc.prog.procs
        self.assertIn(shared_mod.update_form("push_or_wait", 1), procs)
        self.assertIn(shared_mod.update_form("decide", 1), procs)
        # On the path that pushes, the list is consumed by the push, not
        # duplicated: it changes in place when nothing else holds it.
        upd = procs[shared_mod.update_form("push_or_wait", 1)]
        self.assertNotIn(("dup", "xs"), upd.body.term[2].stmts)

    def test_static_checks(self):
        cases = {
            "(proc main () eff (let s (shared 1)) (do (defer sv_give (s))) (ret 0))": "ret with 1 deferred",
            "(proc main () eff (let s (shared 1)) (do (defer sv_give (s))) (tail main))": "tail with 1 deferred",
            "(proc main () eff (do (undefer)) (ret 0))": "no deferred step pending",
            "(proc j (n) eff (ret (tag Keep 0))) (proc main () eff (let s (shared 1)) (do (step j (s))) (ret 0))":
                "not a pure procedure",
            "(proc p (x) pure (let s (shared x)) (ret 0)) (proc main () eff (ret 0))": "pure p uses a shared",
        }
        for src, msg in cases.items():
            with self.assertRaisesRegex(shared_mod.SharedError, msg):
                self.build(src)

    def contended(self, n):
        clients = " ".join(f"(clo client s {k})" for k in range(1, n + 1))
        return f"""
        (proc job (k) eff (let a (host work k)) (ret a))
        (proc client (s k) eff (let v (call sv_with_permit s (clo job k))) (ret v))
        (proc main () eff
          (let s (shared 1))
          (let r (call all! (list {clients})))
          (let n (read s))
          (ret n))
        """

    def test_a_store_readies_only_the_first_waiter_that_would_not_wait(self):
        # The wake rule. 20 clients contend for one permit. At each release
        # the runtime probes the waiters in order and readies only the first
        # that would take it, so tasks are dispatched a linear number of times.
        # Readying every waiter dispatches each of them after every release,
        # and all but one wait again.
        lows = self.build(self.contended(20))
        self.assertEqual(everywhere(lows), {("done", 1)})
        r = tasks(lows, PausePolicy())
        self.assertEqual((r.runtime["resumes"], r.runtime["steps run again"]), (80, 19))
        with switches(shared={"wake_rule": "all"}):
            w = tasks(lows, PausePolicy())
            self.assertEqual(everywhere(lows), {("done", 1)})
        self.assertEqual((w.runtime["resumes"], w.runtime["steps run again"]), (251, 190))

    PASS_ON = """
    (proc flag_wait (n) pure (let z (eq n 0)) (if z (block (ret (tag Wait))) (block (ret (tag Keep 0)))))
    (proc flag_set (n) pure (ret (tag Set 0 1)))
    (proc await_flag (flag) eff (let v (step flag_wait (flag))) (ret 0))
    (proc job (k) eff (let a (host work k)) (ret a))
    (proc client (s k) eff (let v (call sv_with_permit s (clo job k))) (ret v))
    (proc raced (s flag) eff
      (let d (host slow 0))
      (let w (call race! (clo client s 7) (list (clo await_flag flag))))
      (ret w))
    (proc giver (s flag) eff
      (let a (host slow 0))
      (let b (host slow 0))
      (do (step sv_give (s)))
      (do (step flag_set (flag)))
      (ret 0))
    (proc main () eff
      (let s (shared 0))
      (let flag (shared 0))
      (let r (call all! (list (clo client s 9) (clo raced s flag) (clo giver s flag))))
      (let n (read s))
      (ret n))
    """

    def test_a_readied_waiter_cancelled_before_it_runs_passes_the_wake_on(self):
        # The giver releases the only permit, which readies the oldest
        # waiter, under some schedules the raced client, and then ends the
        # race. Under some
        # schedules the race cancels that client before it runs, and the
        # other client must be readied instead.
        lows = self.build(self.PASS_ON)
        self.assertEqual(everywhere(lows), {("done", 1)})
        with switches(shared={"no_pass_on": True}):
            self.assertIn(("crashed", ("str", shared_mod.WAIT_DEADLOCK)), everywhere(lows))

    def test_a_shared_semaphore_needs_no_server_task(self):
        # The same three clients against the sessions RFC's semaphore task and
        # against a shared variable, under the same schedule.
        actor = build(library.SOURCE + Libraries.SEM)[1]
        cell = self.build(self.SEM)
        a, c = tasks(actor, PausePolicy()), tasks(cell, PausePolicy())
        self.assertEqual((a.result, c.result), (("done", 1), ("done", 1)))
        self.assertLess(c.runtime["spawns"], a.runtime["spawns"])
        self.assertLess(c.runtime["resumes"], a.runtime["resumes"])
        self.assertLess(c.heap["alloc"], a.heap["alloc"])


class SharedConveniences(unittest.TestCase):
    """get! and modify! (library.SHARED_SOURCE), which are update! with a
    fixed answer: the RFC's download counter and its reporter."""

    PROGRESS = """
    (proc inc (n) pure (ret (add n 1)))
    (proc worker (h k) eff (let a (host work k)) (do (call sv_modify h (clo inc))) (ret a))
    (proc seen_all (n total) pure
      (let short (lt n total))
      (if short (block (ret (tag Wait))) (block (ret (tag Keep n)))))
    (proc reporter (h) eff (let n (step seen_all (h) 3)) (ret n))
    (proc main () eff
      (let h (shared 0))
      (let r (call all! (list (clo worker h 1) (clo reporter h) (clo worker h 2) (clo worker h 3))))
      (let n (call sv_get h))
      (ret (tup r n)))
    """

    def test_modify_and_get_are_updates(self):
        lows = build(library.SOURCE + library.SHARED_SOURCE + self.PROGRESS)[1]
        want = ("done", ("tup", None, (("list", None, (101, 3, 102, 103)), 3)))
        self.assertEqual(everywhere(lows), {want})
        self.assertEqual(none_build(lows).result, want)
        # The reporter waits until the third modify! and is woken by it.
        r = tasks(lows, PausePolicy())
        self.assertGreaterEqual(r.runtime["waits"], 1)


class CellRelease(unittest.TestCase):
    """Cycles through states (V9): a cell that no live task holds has its state
    dropped, which breaks any cycle through it. Every run's heap is verified
    exact afterwards, so a cycle that survives is a leak the heap reports."""

    LIB = """
    (proc cons (s x) pure (ret (tag Set 0 (tag Cons x s))))
    (proc pop (s) pure
      (match s
        (Nil () (block (ret (tag Keep (tag Empty)))))
        (Cons (x rest) (block (ret (tag Set (tag Got x) rest))))))
    """

    # A crawler: each job pushes two more onto the queue it captured, so a
    # pending job and its queue form a cycle. Two workers pop and run jobs
    # until the queue is empty. Then main pushes one more job and returns,
    # abandoning the queue with that cycle in it.
    CRAWLER = """
    (proc crawl (q d) eff
      (let more (lt 0 d))
      (if more
        (block
          (let e (sub d 1))
          (do (step cons (q) (clo crawl q e)))
          (do (step cons (q) (clo crawl q e)))
          (let one (int 1)) (ret one))
        (block (let one (int 1)) (ret one))))
    (proc work (q n) eff
      (let p (step pop (q)))
      (match p
        (Empty () (block (ret n)))
        (Got (f) (block (let k (callc! f)) (tail work q (add n k))))))
    (proc main () eff
      (let q (shared (tag Nil)))
      (let two (int 2))
      (do (step cons (q) (clo crawl q two)))
      (let r (call both! (clo work q 0) (clo work q 0)))
      (do (step cons (q) (clo crawl q two)))
      (let a (field r 0)) (let b (field r 1))
      (ret (add a b)))
    """

    # Two tasks each make two cells that hold each other's handles, and end.
    PAIRS = """
    (proc pair () eff
      (let a (shared (tag Nil))) (let b (shared (tag Nil)))
      (do (step cons (a) b)) (do (step cons (b) a))
      (let z (int 0)) (ret z))
    (proc main () eff
      (let r (call both! (clo pair) (clo pair)))
      (let z (int 0)) (ret z))
    """

    # A task stores a cell it made into a cell main holds, and ends; main then
    # takes the cell out and updates it.
    STASH = """
    (proc stash (a) eff
      (let x (shared (tag Nil)))
      (do (step cons (a) x))
      (let z (int 0)) (ret z))
    (proc main () eff
      (let a (shared (tag Nil)))
      (let r (call all! (list (clo stash a))))
      (let p (step pop (a)))
      (match p
        (Empty () (block (let z (int 0)) (ret z)))
        (Got (x) (block (do (step pop (x))) (let o (int 1)) (ret o)))))
    """

    def build(self, src):
        return build(library.SOURCE + self.LIB + src)[1]

    def test_jobs_that_capture_their_own_queue(self):
        lows = self.build(self.CRAWLER)
        self.assertEqual(everywhere(lows), {("done", 7)})
        none = none_build(lows)
        self.assertEqual(none.result, ("done", 7))
        # The abandoned queue is released when main finishes, in either
        # build, and everything it held is freed.
        self.assertEqual(tasks(lows, PausePolicy()).runtime["cells released when the root finished"], 1)
        self.assertEqual(none.runtime["cells released when the root finished"], 1)

    def test_a_cycle_made_in_a_task_is_released_when_it_finishes(self):
        lows = self.build(self.PAIRS)
        self.assertEqual(everywhere(lows), {("done", 0)})
        r = tasks(lows, PausePolicy())
        self.assertEqual(r.runtime["cells released when a child task finished"], 2)
        # Releasing one cell of each pair frees the other.
        self.assertEqual(r.runtime["cells released"], 2)
        self.assertEqual(none_build(lows).result, ("done", 0))

    def test_a_stored_cell_gains_the_holders_of_the_cell_that_holds_it(self):
        lows = self.build(self.STASH)
        self.assertEqual(everywhere(lows), {("done", 1)})
        # Without that, the cell is released when the task that made it ends,
        # though main can still reach it through its own cell.
        with switches(shared={"no_store_holders": True}):
            with self.assertRaisesRegex(AssertionError, "V9"):
                tasks(lows, PausePolicy())

    def test_generated_programs_that_form_cycles(self):
        stats = Counter()
        for seed in range(1, 61):
            check_program(seed, stats, cancel_sites=2, links=True)
        for f in ("cell holds its own handle", "job captures its own cell", "cell made in a task",
                  "item popped and run", "cell cleared"):
            self.assertGreater(stats["feature " + f], 5, f)
        self.assertGreater(stats["cells released when a child task finished"], 100)
        self.assertGreater(stats["cells released when the root finished"], 10)
        self.assertGreater(stats["None: cells released"], 10)
        # Never releasing leaks the cycles.
        with switches(sessions={"no_release_cells": True}):
            with self.assertRaisesRegex(HeapError, "leaked"):
                for seed in range(1, 61):
                    check_program(seed, Counter(), cancel_sites=1, links=True)


class NoCycles(unittest.TestCase):
    """The rejected alternative to releasing cells: type rules that keep states
    from forming cycles (cycles.py). Kept to measure what the rules reject."""

    def setUp(self):
        import cycles
        self.cycles = cycles
        self.addCleanup(lambda: cycles.MUTATIONS.update({k: False for k in cycles.MUTATIONS}))

    def test_the_rule_is_exact_sound_and_modular_on_generated_types(self):
        # precise (with lambda sets) never accepts a type that leaks and
        # always has a leaking witness for one it rejects; the checker
        # (without them) accepts nothing precise rejects, and rejects more
        # only for a function in a state; checking each declaration once
        # with summaries agrees with checking the expanded type.
        stats, failures = self.cycles.fuzz(3000, 0)
        self.assertEqual(failures, [])
        self.assertEqual(stats["witnesses that leak"], stats["precise rejects"])
        self.assertGreater(stats["precise rejects"], 100)
        self.assertGreater(stats["rejected only for a function in a state"], 100)
        self.assertGreater(stats["cells made"], 10000)

    def test_examples(self):
        for name, (decls, x, precise, checker) in self.cycles.EXAMPLES.items():
            self.assertEqual(self.cycles.precise_ok(decls, x), precise, name)
            self.assertEqual(self.cycles.checker_verdict(decls, x)[0], checker, name)
            self.assertEqual(self.cycles.modular_verdict(decls, [[n] for n in decls], x), checker, name)

    def test_each_part_of_the_rule_is_needed(self):
        def failures(mutation):
            self.cycles.MUTATIONS[mutation] = True
            try:
                return [why for _, why, _ in self.cycles.fuzz(5000, 0)[1]]
            finally:
                self.cycles.MUTATIONS[mutation] = False
        # Allowing functions in states accepts types whose cells can form a
        # cycle through a closure's captures.
        self.assertTrue(any(w.startswith("checker accepts") for w in failures("no_fn_rule")))
        # Forgetting which parameters end up inside a state accepts uses
        # that put a function or a cycle there.
        self.assertTrue(any(w.startswith("modular True") for w in failures("no_requirements")))


class SharedModel(unittest.TestCase):
    def test_every_interleaving_of_the_lock_protocol(self):
        import shared_model
        found = {}
        for name, (init, progs, want) in shared_model.SCENARIOS.items():
            for mutation in (None, "unordered", "late_register", "no_locks"):
                found[(name, mutation)] = shared_model.explore(init, progs, want, mutation)[1]
        # The protocol is correct in every scenario.
        self.assertEqual({k: v for k, v in found.items() if k[1] is None and v}, {})
        # Locking in each thread's own order deadlocks two-cell footprints (V4).
        self.assertTrue(found[("transfer", "unordered")].startswith("V4"))
        # Registering after unlocking loses a wakeup (V7).
        for name in shared_model.SCENARIOS:
            self.assertTrue(found[(name, "late_register")].startswith("V7"), name)
        # Without locks, steps lose updates or wakeups (V2, V7).
        self.assertTrue(found[("fan", "no_locks")].startswith("V2"))
        self.assertTrue(all(found[(n, "no_locks")] for n in shared_model.SCENARIOS))


class Coverage(unittest.TestCase):
    def test_every_rfc_invariant_and_obligation_is_pinned(self):
        import coverage
        rows = coverage.rows()
        self.assertEqual(len(rows), 58)  # G1-G9, C1-C11, R1-R8, P1-P10; S1-S10; V1-V10
        self.assertEqual(set(rows), set(coverage.MATRIX) | set(coverage.PENDING),
                         "the RFCs' tables and the matrix differ")
        self.assertFalse(set(coverage.MATRIX) & set(coverage.PENDING))
        for k in coverage.PENDING:
            self.assertIn("Not yet prototyped", rows[k], f"{k} is pending but its RFC row does not say so")
        here = sys.modules[__name__]
        for k, (how, names) in coverage.MATRIX.items():
            self.assertTrue(how and names, k)
            for name in names:
                cls, meth = name.split(".")
                self.assertTrue(callable(getattr(getattr(here, cls, None), meth, None)), f"{k}: no test {name}")


if __name__ == "__main__":
    unittest.main()
