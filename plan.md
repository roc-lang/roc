# Remaining Str.inspect Eval Port Plan

## Goal

Port every remaining `origin/main` eval test that still belongs in the single
`Str.inspect((...)) -> returned RocStr` observation model.

This plan is intentionally **not** about:

- comptime-evaluator summary/problem tests
- annotation-only interpreter tests
- emitter / monomorphic-source tests
- stack unit tests
- host-side `dbg` / `crash` / `expect` plumbing assertions

Those need separate harnesses and are out of scope for this project.

## Non-Negotiable Design Rules

1. There is exactly one runtime-observation protocol for this project:
   - raw Roc source in
   - compiler wraps/observes via `Str.inspect((...))`
   - backend executes compiled program
   - test harness reads returned `RocStr`
   - assertions compare exact bytes

2. Do not introduce any secondary observation path.
   - no typed host readback
   - no helper-specific formatting
   - no “special test-only evaluator mode”

3. Port tests by preserving the original semantic intent, not by weakening them.
   - if the old test was checking aliasing/container survival through an observed value,
     the new test must still check that semantic fact
   - only the observation mechanism changes

4. Prefer one canonical shared definition over duplicated test logic.
   - if many old tests differ only in source and expected string, move them into
     the existing data-driven `TestCase` model
   - do not create another ad hoc mini-harness

5. If porting exposes a compiler bug, fix the compiler in the most explicit,
   stage-owned, cor-like way available.
   - no workarounds
   - no fallbacks
   - no backend-local repair logic

## In Scope

These are the remaining `origin/main` eval buckets that should live in the
current inspect-only parity runner.

### 1. Remaining semantic/runtime cases from `interpreter_style_test.zig`

Only the cases whose true contract is:

- evaluate Roc source
- observe the resulting Roc value
- compare that value semantically

Examples that belong here:

- ordinary rendered-value expectations
- arithmetic / equality results
- record update semantics
- tuple / record / tag / list pattern matches
- `List.len`, `List.fold`, `List.any`, `List.all`, `List.contains`
- loops, early return, and runtime control flow

Examples that do **not** belong here:

- host `dbg` message capture
- crash-message assertions
- expect-failure host plumbing
- tests that are specifically about the old interpreter’s host readback API

### 2. Entire remaining list-refcount semantic buckets

These old files are still semantically valid under `Str.inspect`:

- `src/eval/test/list_refcount_simple.zig`
- `src/eval/test/list_refcount_basic.zig`
- `src/eval/test/list_refcount_conditional.zig`
- `src/eval/test/list_refcount_strings.zig`
- `src/eval/test/list_refcount_complex.zig`

`src/eval/test/list_refcount_builtins.zig` is documentation, not a real porting target.

The already-ported list/closure/container buckets are not part of the remaining work:

- `list_refcount_alias`
- `list_refcount_function`
- `list_refcount_containers`
- `list_refcount_pattern`
- `list_refcount_nested`

### 3. Any still-unported runtime-shaped value cases from `comptime_eval_test.zig`

Only if they are genuinely source-to-value semantic cases that fit the runtime
inspect-only model.

This explicitly excludes:

- `evalAll()` summary assertions
- crash/problem-count assertions
- cross-module comptime-specific reporting behavior
- expect/dbg/crash bookkeeping

Most recursive nominal/runtime cases have already moved into
`src/eval/test/eval_recursive_data_tests.zig`, but this plan includes a final
audit for any remaining stragglers.

## Out of Scope

These are intentionally excluded and must not be silently folded into the
inspect-only runner:

- `src/eval/test/anno_only_interp_test.zig`
- the comptime-specific parts of `src/eval/test/comptime_eval_test.zig`
- `src/eval/test/mono_emit_test.zig`
- `src/eval/test/stack_test.zig`
- `interpreter_style_test.zig` cases whose contract is host `dbg` / `crash` / `expect`

If a case from one of those files looks tempting, stop and classify it first.
Do not mix harness styles in this project.

## File Ownership

The final inspect-only runtime coverage should live only in these data-driven files:

- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/eval_tests.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/eval_closure_recursion_tests.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/eval_recursive_data_tests.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/eval_low_level_tests.zig`

Do not create another parallel eval-runner test family for source-level runtime parity.

## Phase 1: Final Classification Audit

Produce a line-by-line classification of all remaining old-style tests into one of:

1. `port_to_inspect_runner`
2. `already_covered`
3. `different_harness_required`

This audit must cover, at minimum:

- `origin/main:src/eval/test/interpreter_style_test.zig`
- `origin/main:src/eval/test/list_refcount_simple.zig`
- `origin/main:src/eval/test/list_refcount_basic.zig`
- `origin/main:src/eval/test/list_refcount_conditional.zig`
- `origin/main:src/eval/test/list_refcount_strings.zig`
- `origin/main:src/eval/test/list_refcount_complex.zig`
- `origin/main:src/eval/test/comptime_eval_test.zig`

### Required End State

- no remaining ambiguity about whether a test belongs in this project
- no “maybe later” bucket
- every old test is accounted for

## Phase 2: Port Remaining `interpreter_style` Semantic Cases

Port every still-unported `interpreter_style_test.zig` case that fits the
inspect-only model.

### Porting Rules

1. Preserve the original Roc source whenever possible.
2. Replace host readback assertions with exact inspect-string expectations.
3. If multiple old tests are redundant with current coverage, keep the strongest version.
4. If an old test depended on interpreter-specific render formatting rather than
   language-level semantics, classify it out of scope instead of distorting it.

### Expected Homes

- general source-level semantics -> `eval_tests.zig`
- closure/recursion-heavy semantics -> `eval_closure_recursion_tests.zig`

## Phase 3: Port Remaining List/Container Semantic Cases

Port every remaining case from:

- `list_refcount_simple.zig`
- `list_refcount_basic.zig`
- `list_refcount_conditional.zig`
- `list_refcount_strings.zig`
- `list_refcount_complex.zig`

### Porting Rules

1. Treat these as semantic aliasing/container-survival tests, not host RC probes.
2. Preserve the original semantic shape of the test.
3. Prefer exact structural observed results over overly-reduced numeric summaries
   when the full structure gives stronger coverage.
4. Keep source snippets readable; do not obfuscate them just to force them into
   a smaller helper shape.

### Expected Homes

- list/container semantics with no special low-level dependence -> `eval_closure_recursion_tests.zig`
- genuinely builtin/low-level surface behavior -> `eval_low_level_tests.zig`

## Phase 4: Final Runtime-Shaped Audit Against `comptime_eval_test.zig`

Do one final sweep of `origin/main:src/eval/test/comptime_eval_test.zig` and port
any still-missing cases whose true contract is “runtime value shape/behavior”
rather than comptime machinery.

Examples that belong here:

- recursive nominal value construction
- cross-module runtime-shaped value use
- deep recursive data inspection

Examples that do not:

- `evalAll()` counts
- crash/problem reporting
- annotation-only access behavior
- comptime-only exposure/diagnostic assertions

### Expected Home

- recursive/runtime-shaped value tests -> `eval_recursive_data_tests.zig`

## Phase 5: Deduplicate and Normalize

Once all eligible tests are ported:

1. Remove duplicated semantic cases where the new suite now has multiple copies
   of the same behavior from different legacy files.
2. Keep the strongest and clearest variant.
3. Normalize naming so every test name clearly says what semantic behavior is under test.
4. Normalize expected inspect strings to the true current language-level rendering,
   not to historical harness accidents.

### Important Constraint

Deduplication must not reduce meaningful coverage.

If two tests look similar but stress different compilation paths
(for example closure capture vs plain local value), keep both.

## Phase 6: Bring-Up and Compiler Fixes

Only after phases 1-5 are complete:

1. Run the inspect-only eval suite.
2. Fix failures by improving the compiler/runtime/backends, not the harness.
3. Keep fixes explicit, stage-owned, and cor-like.
4. Do not reintroduce any old observation machinery.

### Fix Policy

If a test fails:

- first ask what explicit fact should have been produced earlier
- then make the responsible stage produce and preserve that fact
- then make later stages consume it directly

Never fix a failure by:

- ad hoc backend special-casing
- host-side formatting tricks
- “close enough” structural compatibility logic
- recovery from missing earlier-stage information

## Phase 7: Final Audit

After the port and bring-up are complete, run a final audit with these questions:

1. Are there any remaining `origin/main` eval tests that fit the `Str.inspect`
   observation model but are still unported?
2. Are there any inspect-only parity tests still living outside the four
   canonical data-driven files?
3. Did any typed observation or host formatting path get reintroduced?
4. Are any currently-ported cases actually using the wrong harness and needing
   to be moved out into the future separate project?

This phase is not complete until all four answers are clean.

## Completion Criteria

This project is complete only when:

1. Every remaining `origin/main` eval test that truly belongs in the
   `Str.inspect -> RocStr` model has been ported.
2. No out-of-scope harness-specific tests were incorrectly forced into this runner.
3. The inspect-only runner remains the sole runtime parity observation mechanism.
4. The resulting suite is green.
5. A final audit confirms there are no remaining port-eligible old-style eval
   tests left behind.
