"""What establishes each invariant and obligation in the RFC.

This is the prototype's completion criterion: every row of the RFC's
"Language guarantees", "Compiler invariants", "Runtime invariants" and
"Platform obligations" tables, and of the companion sessions RFC's
"Invariants" table, has an entry here, each entry names how the prototype
establishes it, and each names regression tests in tests.py that exist and
pass. A row the prototype does not establish yet is in PENDING instead, and
its row in the RFC says "Not yet prototyped". `tests.py` checks all of this
(the Coverage group).

    python3 coverage.py      print the matrix
"""

from __future__ import annotations

import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
RFC = os.path.join(HERE, "..", "concurrency-explicit-suspension.md")
SESSIONS_RFC = os.path.join(HERE, "..", "concurrency-sessions.md")

FUZZ = "Fuzz.test_generated_programs"

MATRIX = {
    # ------------------------------------------------ language guarantees
    "G1": ("Tasks interleave only at suspension sites: the stackful engine yields only at hosted calls and "
           "waits, the lowering pauses only at certified sites, and a pause anywhere else trips C2",
           [FUZZ, "Mutations.test_missing_site_trips_c2"]),
    "G2": ("Atomicity analyses race-checked against an SP-order oracle and Maude's exhaustive search; "
           "the wake word race-free under ThreadSanitizer",
           ["RcAtomicity.test_fuzz_sound_and_ordered", "RcAtomicity.test_scenarios", "Maude.test_rc_model",
            "ThreadSanitizer.test_counted_wake_word_is_race_free_and_exact"]),
    "G3": ("A None build against an input-order Tasks build, on every generated program",
           [FUZZ, "Combinators.test_supervise_restarts_exactly_max_times"]),
    "G4": ("Deterministic programs give one result and the same per-task effects under every schedule",
           [FUZZ, "Combinators.test_all_returns_in_input_order_under_every_schedule"]),
    "G5": ("R3's monitor: no task finishes before its subtree; every run ends with every task finished; "
           "a task that returns from inside a concurrent loop trips G5; exits cancel or wait for every entry",
           [FUZZ, "Combinators.test_race_returns_the_first_and_cancels_the_rest",
            "Loops.test_returning_from_inside_a_loop_without_its_exit_trips_g5"]),
    "G6": ("Cancellation at every suspension leaves the heap exact; R4's monitor",
           ["Cancellation.test_cancel_at_every_suspension_releases_everything", FUZZ]),
    "G7": ("Crash propagation, supervision and containment checked by R5 and R7; an entry's crash reaches "
           "its parent at once, wherever the parent waits",
           ["Crashes.test_crash_cancels_siblings_and_propagates_its_message", "Combinators.test_one_for_one",
            "Combinators.test_supervise_restarts_exactly_max_times",
            "Loops.test_an_entrys_crash_reaches_a_parent_waiting_elsewhere_at_once"]),
    "G8": ("Paused tail loops, direct and through closures, hold a constant record chain",
           ["Lowering.test_tail_loop_that_pauses_keeps_constant_records",
            "Lowering.test_tail_cycle_through_a_closure_keeps_constant_records"]),
    "G9": ("R8's monitor in every run; shields checked by their effects, with and without the shield",
           ["Loops.test_limit_runs_entries_one_at_a_time_and_refills_as_each_finishes",
            "Loops.test_an_exit_waits_for_a_shielded_entry_instead_of_cancelling_it",
            "Loops.test_a_shielded_entry_that_has_not_begun_is_cancelled_at_an_exit", FUZZ]),
    # ------------------------------------------------ compiler invariants
    "C1": ("Unwinding against the stackful oracle under every schedule, including `ready`",
           [FUZZ, "Lowering.test_rfc_example_runs_straight_through_when_nothing_pauses"]),
    "C2": ("Certified per procedure; at run time a pending flag after a non-site call is an error",
           [FUZZ, "Mutations.test_missing_site_trips_c2"]),
    "C3": ("Certified per site: the ledger covers what the continuation uses",
           [FUZZ, "Mutations.test_c3_rejects_a_short_record"]),
    "C4": ("Records hold only values; a pending cell outside task storage in a record is an error",
           ["Mutations.test_pending_cell_in_a_native_frame_trips_c4",
            "Lowering.test_deferred_call_is_handed_up_through_a_suspension"]),
    "C5": ("ARC certification of every ledger; cancellation at every suspension with exact heap accounting",
           ["Mutations.test_certifier_rejects_a_short_ledger", "Mutations.test_skipped_drop_entries_leak",
            "Lowering.test_ledger_is_exactly_the_live_values", FUZZ]),
    "C6": ("rc-atomicity: forward modes, call-site-sensitive seeds, edge modes and lending, fuzzed against "
           "the race oracle and Maude; its two theorems proved in Lean",
           ["RcAtomicity.test_fuzz_sound_and_ordered", "RcAtomicity.test_scenarios", "Maude.test_rc_model",
            "Lean.test_proofs_check"]),
    "C7": ("Frame-replacing calls have no check and no record; record chains stay constant",
           ["Lowering.test_tail_loop_that_pauses_keeps_constant_records",
            "Lowering.test_tail_cycle_through_a_closure_keeps_constant_records"]),
    "C8": (".lss against .boxy on every generated program and schedule",
           [FUZZ, "Lowering.test_boxy_checks_every_erased_effectful_call"]),
    "C9": ("A None build against the input-order Tasks build that starts loop entries only when asked, on "
           "every generated program",
           [FUZZ, "Loops.test_a_shielded_entry_that_has_not_begun_is_cancelled_at_an_exit",
            "Loops.test_spawn_in_a_body_adds_entries"]),
    "C10": ("Unwinding, which resumes innermost first through resume entries, against the stackful oracle",
            [FUZZ, "Lowering.test_rfc_example_saves_one_record_per_pause"]),
    "C11": ("Every generated loop is lowered with its exit sequence on every edge out of the body; the "
            "runtime's G5 check catches one that is not",
            ["Loops.test_returning_from_inside_a_loop_without_its_exit_trips_g5", FUZZ]),
    # ------------------------------------------------ runtime invariants
    "R1": ("Monitored in every run; Maude over every interleaving, including two wait sources; real threads "
           "under ThreadSanitizer",
           ["Wake.test_counted_protocol", "Wake.test_flag_protocol_resumes_with_nothing_to_deliver",
            "Loops.test_a_report_readies_its_parent_only_for_the_wait_it_belongs_to",
            "Maude.test_wake_protocols", "ThreadSanitizer.test_counted_wake_word_is_race_free_and_exact",
            "ThreadSanitizer.test_flag_protocols_break_r1_on_real_threads", FUZZ]),
    "R2": ("Monitored in every run; the parent's running flag under ThreadSanitizer",
           [FUZZ, "Mutations.test_double_resume_is_a_host_bug",
            "ThreadSanitizer.test_counted_wake_word_is_race_free_and_exact"]),
    "R3": ("Monitored in every run: reports only after a subtree is gone, terminal states bottom-up",
           [FUZZ, "Crashes.test_crash_cancels_siblings_and_propagates_its_message"]),
    "R4": ("Monitored in every run: no Roc code after cancellation; drops exact",
           ["Cancellation.test_cancel_at_every_suspension_releases_everything", FUZZ]),
    "R5": ("Monitored in every run; the heap verified exactly, crashed native frames the only leak",
           ["Crashes.test_only_running_native_frames_leak",
            "Crashes.test_crash_cancels_siblings_and_propagates_its_message", FUZZ]),
    "R6": ("Every run must terminate with every task finished; a stuck run is an error",
           [FUZZ, "Conformance.test_every_broken_host_is_caught"]),
    "R7": ("Monitored at every supervise! call in every run: restarts only after crashes, at most "
           "max_restarts, giving up with the last message",
           ["Combinators.test_supervise_restarts_exactly_max_times", "Mutations.test_off_by_one_restart_limit_trips_r7",
            FUZZ]),
    "R8": ("Monitored at every start: counts the running entries under each limit; a runtime that starts one "
           "too many trips it",
           ["Loops.test_starting_past_a_limit_trips_r8",
            "Loops.test_limit_runs_entries_one_at_a_time_and_refills_as_each_finishes", FUZZ]),
    # ------------------------------------------------ platform obligations
    "P1": ("Conformance: hosts that resume twice, or resume what nothing readied, are caught",
           ["Conformance.test_every_broken_host_is_caught", "Mutations.test_double_resume_is_a_host_bug"]),
    "P2": ("Real threads under ThreadSanitizer: a relaxed handoff races on a child's first run",
           ["ThreadSanitizer.test_the_wake_word_orders_resumes_and_p2_orders_first_runs"]),
    "P3": ("Conformance: waking twice, waking before writing, writing twice are caught",
           ["Conformance.test_every_broken_host_is_caught"]),
    "P4": ("Conformance: writing or waking after an abort is caught; Maude's HOSTOP model of the abort lock",
           ["Conformance.test_every_broken_host_is_caught", "Mutations.test_write_after_abort_is_a_host_bug",
            "Maude.test_wake_protocols"]),
    "P5": ("Conformance: releasing arguments twice or never is caught by the exact heap",
           ["Conformance.test_every_broken_host_is_caught"]),
    "P6": ("Conformance: a thread-confined allocator passes when tasks stay on one thread and is caught "
           "when they do not",
           ["Conformance.test_every_broken_host_is_caught"]),
    "P7": ("Conformance: a host that never completes an operation is caught as a stuck run",
           ["Conformance.test_every_broken_host_is_caught"]),
    "P8": ("Conformance: freeing a root twice, reading the wrong outcome are caught",
           ["Conformance.test_every_broken_host_is_caught"]),
    "P9": ("Conformance: a region host that waits for aborted operations reclaims crash leaks exactly; one "
           "that reclaims at the crash frees what an aborted operation still holds",
           ["Conformance.test_every_broken_host_is_caught", "Conformance.test_a_region_host_reclaims_crash_leaks"]),
    "P10": ("Conformance: a nested resume in a Tasks build is caught",
            ["Conformance.test_every_broken_host_is_caught"]),
    # ------------------------------------------------ sessions (companion RFC)
    "S1": ("A second use of either end crashes with USED_TWICE",
           ["Sessions.test_using_an_end_twice_crashes"]),
    "S2": ("Cancelled exactly when the Out end went away unused: dropped by code, by a cancelled task's "
           "records, or closed by a crash; without Cancelled on drop the receiver deadlocks",
           ["Sessions.test_dropping_an_unused_out_end_delivers_cancelled",
            "Sessions.test_cancelling_a_producer_delivers_cancelled_at_its_current_end",
            "Sessions.test_without_cancelled_on_drop_the_receiver_deadlocks", FUZZ]),
    "S3": ("The exact heap counts a message as owned only while its In end exists; a grant sent to a "
           "cancelled requester returns, and leaving the abandoned channel open is a heap error",
           ["Sessions.test_a_permit_granted_to_a_cancelled_requester_returns_to_its_owner",
            "Sessions.test_race_drops_the_end_a_finished_loser_returned", FUZZ]),
    "S4": ("A crash closes the ends its task held; without closing, the peer waits until the crash "
           "reaches it",
           ["Sessions.test_a_crash_closes_the_ends_its_task_held"]),
    "S5": ("An arrival does not ready a task waiting at a receive; one wake word per task breaks R1",
           ["Sessions.test_an_arrival_does_not_ready_a_task_waiting_at_a_receive"]),
    "S6": ("A cycle of receives crashes the receiver that waited last; every run ends with every task "
           "finished",
           ["Sessions.test_a_cycle_of_sessions_deadlocks_and_crashes_the_last_receiver",
            "Sessions.test_a_request_reply_release_server_deadlocks_on_one_stack", FUZZ]),
    "S7": ("A None build against the input-order schedule that models the one stack, on every generated "
           "program, deadlocks included",
           ["Sessions.test_a_none_build_runs_a_later_entry_on_demand_and_matches_input_order",
            "Sessions.test_a_stream_flows_by_continuation_passing",
            "Sessions.test_a_request_reply_release_server_deadlocks_on_one_stack", FUZZ]),
}

# Rows the prototype does not establish yet.
PENDING = {
    "S8": "rc-atomicity does not model channels yet",
}


def table(path, end_heading):
    with open(path) as f:
        text = f.read()
    return text[text.index("\n## Invariants"):text.index(end_heading)]


def rows():
    """Every row of the two RFCs' invariant and obligation tables, by id."""
    found = {}
    for path, end in ((RFC, "\n### What to establish first"), (SESSIONS_RFC, "\n## Library patterns")):
        for k, line in re.findall(r"^\| ([GCRPS]\d+) \|(.*)$", table(path, end), re.M):
            found[k] = line
    return found


def rfc_ids():
    return list(rows())


def main():
    for k in rfc_ids():
        how, tests = MATRIX.get(k, ("PENDING: " + PENDING[k], []) if k in PENDING else ("MISSING", []))
        print(f"{k:<4} {how}")
        for t in tests:
            print(f"       {t}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
