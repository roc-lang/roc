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
import runtime  # noqa: E402
from arc import ArcError, certify  # noqa: E402
from check import CancelAtPolicy, check_program  # noqa: E402
from heap import HeapError  # noqa: E402
from host import (InputOrderPolicy, PausePolicy, RandomPolicy, ReadyPolicy, run_none,  # noqa: E402
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
        none = run_none(lows["lss"].prog)
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
        want = run_none(lows["lss"].prog).result
        for seed in range(40):
            for pol in (RandomPolicy(seed), RandomPolicy(seed, parallel=True)):
                r = tasks(lows, pol, conc="ParallelTasks" if pol.parallel else "Tasks")
                self.assertEqual(r.result, want)

    def test_all_ok_cancels_the_rest_at_the_first_err(self):
        _, lows = prog(self.MAIN.format(comb="all_ok!"))
        none = run_none(lows["lss"].prog)
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
        self.assertEqual(run_none(lows["lss"].prog).result, ("done", 1 + 7 * 3))  # None: first one only
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
        self.assertEqual(len(run_none(lows["lss"].prog).effects), 1)

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
                "No solution.", "No solution.", "Solution 1 "]
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


class Coverage(unittest.TestCase):
    def test_every_rfc_invariant_and_obligation_is_pinned(self):
        import coverage
        ids = coverage.rfc_ids()
        self.assertEqual(len(ids), 35)  # G1-G8, C1-C10, R1-R7, P1-P10
        self.assertEqual(set(ids), set(coverage.MATRIX), "the RFC's tables and the matrix differ")
        here = sys.modules[__name__]
        for k, (how, names) in coverage.MATRIX.items():
            self.assertTrue(how and names, k)
            for name in names:
                cls, meth = name.split(".")
                self.assertTrue(callable(getattr(getattr(here, cls, None), meth, None)), f"{k}: no test {name}")


if __name__ == "__main__":
    unittest.main()
