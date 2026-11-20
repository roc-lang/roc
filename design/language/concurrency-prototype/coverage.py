"""What establishes each invariant and obligation in the RFC.

This is the prototype's completion criterion: every row of the RFC's
"Language guarantees", "Compiler invariants", "Runtime invariants" and
"Platform obligations" tables, and of the companion sessions and shared
variables RFCs' "Invariants" tables, has an entry here, each entry names how the prototype
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
SHARED_RFC = os.path.join(HERE, "..", "concurrency-shared-variables.md")

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
    "G3": ("A None build against a Tasks build under the coroutine schedule, or the input-order schedule for "
           "a program that cannot wait, on every generated program; the two schedules agree where no task "
           "can wait",
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
           "its parent at once, wherever the parent waits, or at its next suspension point if it is running",
           ["Crashes.test_crash_cancels_siblings_and_propagates_its_message", "Combinators.test_one_for_one",
            "Combinators.test_supervise_restarts_exactly_max_times",
            "Loops.test_an_entrys_crash_reaches_a_parent_waiting_elsewhere_at_once",
            "Loops.test_an_entrys_crash_reaching_a_running_parent_crashes_it_at_its_next_suspension"]),
    "G8": ("Paused tail loops, direct and through closures, hold a constant record chain",
           ["Lowering.test_tail_loop_that_pauses_keeps_constant_records",
            "Lowering.test_tail_cycle_through_a_closure_keeps_constant_records"]),
    "G9": ("R8's monitor in every run, and G9's: every entry of a loop over a limited plan, spawned ones "
           "included, counts against the limit at its root, and a loop ends only once every entry that is "
           "not background has arrived and every background one has stopped; shields checked by their "
           "effects, with and without the shield; background entries by their effects, with and without "
           "the mark; both mutations caught by the fuzzer",
           ["Loops.test_limit_runs_entries_one_at_a_time_and_refills_as_each_finishes",
            "Loops.test_an_exit_waits_for_a_shielded_entry_instead_of_cancelling_it",
            "Loops.test_a_shielded_entry_that_has_not_begun_is_cancelled_at_an_exit",
            "Loops.test_spawned_entries_count_against_the_limit_at_the_plans_root",
            "Loops.test_a_plan_whose_root_is_not_a_limit_spawns_under_no_limit",
            "Loops.test_a_limited_loop_whose_entries_wait_on_a_spawned_one_deadlocks",
            "Loops.test_a_loop_ends_once_only_background_entries_are_left",
            "Loops.test_a_background_entry_before_the_others_runs_first_in_a_none_build",
            "Loops.test_plan_all_returns_results_in_input_order_without_background_ones",
            "Loops.test_plan_all_ok_leaves_at_the_first_err",
            "Fuzz.test_generated_plans_catch_mutations_of_g9", FUZZ]),
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
    "C9": ("A None build against the Tasks build under the coroutine schedule, which resumes an entry that "
           "has not begun only once its loop asks or nothing else can run, on every generated program; a "
           "program that cannot wait gives the same results and effects on one stack and as coroutines",
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
           "records, or released after a crash; Closed exactly when every Post is gone and the queue is "
           "empty; without Cancelled on drop the receiver deadlocks",
           ["Sessions.test_dropping_an_unused_out_end_delivers_cancelled",
            "Sessions.test_cancelling_a_producer_delivers_cancelled_at_its_current_end",
            "Sessions.test_without_cancelled_on_drop_the_receiver_deadlocks",
            "Mailboxes.test_many_posters_one_reader_closed_when_every_post_is_gone",
            "Mailboxes.test_a_request_to_a_reader_that_was_cancelled_is_answered_with_cancelled", FUZZ]),
    "S3": ("The exact heap counts a message as owned only while its In end or Inbox exists; a grant sent "
           "to a cancelled requester returns, and leaving the abandoned channel open is a heap error",
           ["Sessions.test_a_permit_granted_to_a_cancelled_requester_returns_to_its_owner",
            "Sessions.test_race_drops_the_end_a_finished_loser_returned", FUZZ]),
    "S4": ("A port no live task can use is released, with holder sets that follow captures, sends, posts, "
           "results and hosted calls; without release the peer waits until the crash reaches it, and with "
           "one holder per end an end shared with a restarted child is never released",
           ["Sessions.test_a_crash_closes_the_ends_its_task_held",
            "HolderSets.test_an_end_shared_with_a_restarted_child_is_released_when_its_last_live_holder_leaves",
            "Mailboxes.test_a_crash_releases_the_posts_its_task_held",
            "Libraries.test_a_crash_returns_the_permit_its_running_frame_held",
            "Roots.test_a_crashed_request_root_returns_its_permit", FUZZ]),
    "S5": ("An arrival does not ready a task waiting at a receive; one wake word per task breaks R1",
           ["Sessions.test_an_arrival_does_not_ready_a_task_waiting_at_a_receive"]),
    "S6": ("A cycle of receives crashes the receiver that waited last; every run ends with every task "
           "finished",
           ["Sessions.test_a_cycle_of_sessions_deadlocks_and_crashes_the_last_receiver",
            "Sessions.test_a_request_reply_release_server_deadlocks_on_one_stack", FUZZ]),
    "S7": ("A None build against the coroutine schedule on every generated program that can wait, deadlocks "
           "included, and against the input-order schedule on every one that cannot; the coroutine None "
           "build's engines and lowerings agree on every generated program",
           ["Sessions.test_a_none_build_runs_a_later_entry_while_an_earlier_one_waits",
            "Libraries.test_coroutines_on_one_stack_run_turn_taking",
            "Sessions.test_a_stream_flows_by_continuation_passing",
            "Sessions.test_a_request_reply_release_server_deadlocks_on_one_stack", FUZZ]),
    "S8": ("The atomicity prototype's fuzzer with channels, against a happens-before race oracle that "
           "matches the series-parallel one without channels; without send seeds it finds unsound programs",
           ["RcAtomicity.test_sent_values_are_seeds",
            "RcAtomicity.test_happens_before_oracle_matches_series_parallel_without_channels"]),
    "S9": ("The semaphore and pool libraries under every schedule, cancellation at every suspension and "
           "crashes; a permit passed to a hosted call; the semaphore cell checked after every fuzzed run, "
           "and a crash mutation caught",
           ["Libraries.test_every_permit_comes_back_however_its_holder_ends",
            "Libraries.test_a_permit_passed_to_a_hosted_call_counts_remote_operations",
            "Libraries.test_a_pool_replaces_a_resource_whose_lease_was_cancelled",
            "Libraries.test_a_semaphore_cell_runs_on_one_stack_and_returns_every_permit",
            "Libraries.test_a_crash_returns_the_permit_its_running_frame_held", FUZZ]),
    "S10": ("A service root beside request roots: each root deadlocks alone, and a root waiting on a port "
            "another root holds is not stalled; ignoring other roots' holders is caught",
            ["Roots.test_a_service_root_outlives_its_requests_and_each_root_deadlocks_alone",
             "Roots.test_a_root_waiting_on_a_port_another_root_holds_is_not_stalled"]),
    # ------------------------------------------------ shared state RFC
    "V1": ("Shared operations only in effectful procedures, and updates only over pure procedures: "
           "rejected statically otherwise",
           ["SharedVariables.test_static_checks"]),
    "V2": ("A step's pure procedure runs on one snapshot and its result is committed whole; a step that crashes "
           "commits nothing; the lock protocol explored over every interleaving, where dropping the locks "
           "loses updates",
           ["SharedVariables.test_a_step_that_crashes_commits_nothing",
            "SharedModel.test_every_interleaving_of_the_lock_protocol", FUZZ]),
    "V3": ("Every step and deferred step calls a pure procedure, checked statically; pure procedures cannot "
           "reach a suspension point (C2)",
           ["SharedVariables.test_static_checks", FUZZ]),
    "V4": ("An update holds its cell's lock only while pure code runs: no deadlock in any interleaving; "
           "the model also locks two-cell steps in one global order, and locking them in each thread's own "
           "order is caught",
           ["SharedModel.test_every_interleaving_of_the_lock_protocol"]),
    "V5": ("Every cleanup push matched by a pop on every path, checked statically; cleanups run at a crash, "
           "at a cancellation and when their body returns (finally!), once each, and on_failure! cleanups "
           "are dropped unrun on a return; a task returning with cleanups pending trips V5; cancellation at "
           "every suspension of generated programs; skipping cleanups at drops is caught",
           ["SharedVariables.test_static_checks", "SharedVariables.test_a_crash_while_holding_runs_the_deferred_step",
            "SharedVariables.test_every_permit_comes_back_however_its_holder_ends",
            "SharedVariables.test_a_cancelled_probe_reopens_the_breaker", FUZZ]),
    "V6": ("A cleanup that crashes poisons its handle: later updates (and the tests' reads) and parked waiters crash; "
           "a handle created inside the supervised function is new after a restart; poisoning nothing "
           "is caught",
           ["SharedVariables.test_a_deferred_step_that_crashes_poisons_its_variable",
            "SharedVariables.test_a_waiter_on_a_poisoned_variable_crashes",
            "SharedVariables.test_a_cancelled_task_whose_deferred_step_crashes_crashes"]),
    "V7": ("Waiters register before the cell is released: no lost wakeup in any interleaving, and "
           "registering late is caught; in the runtime, a commit readies its waiters, and readying none is "
           "caught",
           ["SharedModel.test_every_interleaving_of_the_lock_protocol",
            "SharedVariables.test_a_wait_ends_when_another_task_commits"]),
    "V8": ("A None build switches to the next ready task while an update waits, against the coroutine "
           "schedule on every generated program; a wait nothing can end is the same deadlock in every build",
           ["SharedVariables.test_a_wait_ends_when_another_task_commits",
            "SharedVariables.test_a_wait_nothing_can_end_is_a_deadlock_in_every_build", FUZZ]),
    "V9": ("Cells whose states hold handles, their own included, and jobs that capture their own queue: "
           "every heap is verified exact after every run, so a surviving cycle is a reported leak; a "
           "released cell is never reached again (asserted at every use); releases happen when a child "
           "task finishes, when the root finishes and at the end of a None build; never releasing, and "
           "not passing a cell's holders to the cells stored in it, are both caught",
           ["CellRelease.test_jobs_that_capture_their_own_queue",
            "CellRelease.test_a_cycle_made_in_a_task_is_released_when_it_finishes",
            "CellRelease.test_a_stored_cell_gains_the_holders_of_the_cell_that_holds_it",
            "CellRelease.test_generated_programs_that_form_cycles"]),
}

# Rows the prototype does not establish yet.
PENDING = {}


def table(path, end_heading):
    with open(path) as f:
        text = f.read()
    return text[text.index("\n## Invariants"):text.index(end_heading)]


def rows():
    """Every row of the two RFCs' invariant and obligation tables, by id."""
    found = {}
    for path, end in ((RFC, "\n### What to establish first"), (SESSIONS_RFC, "\n## Library patterns"),
                      (SHARED_RFC, "\n## Drawbacks")):
        for k, line in re.findall(r"^\| ([GCRPSV]\d+) \|(.*)$", table(path, end), re.M):
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
