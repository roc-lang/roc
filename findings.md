# Test Coverage Audit Findings

Scope: audited the current branch against `origin/main` using the PR-style diff
`origin/main...HEAD`. I focused on changed files that contain tests, deleted
test files, deleted inline Zig `test` blocks in production files, skipped or
commented-out tests, snapshot expectation reductions, and rewritten harnesses.

Mechanical summary:

- Total changed paths: 820
- Test-adjacent changed paths: 525
- Added test-adjacent paths: 68
- Deleted test-adjacent paths: 28
- Modified test-adjacent paths: 428
- Renamed test-adjacent paths: 1

## Open Findings

None.

## Resolved Findings

### Deleted Direct Dev-Backend Codegen Structural Coverage Was Restored

Deleted inline test file:

- `src/backend/dev/LirCodeGen.zig`

Replacement:

- `src/backend/dev/LirCodeGen.zig`

The deleted tests now have final-architecture replacements that build current
statement-only `LIR.CFStmt` procedures and exercise the dev backend's current
machine-code generation boundary. These replacements do not restore old
`LirExprStore`, old MIR stores, monotype helpers, or pre-cutover backend APIs.

Active restored test names:

- `proc params and mutable list cells use distinct stack slots`
- `two-arg proc list loop returns full length`
- `generate i64 literal`
- `generate bool literal`
- `tag payload bind invariant rejects mismatched pattern layout`
- `generate addition`
- `record equality uses layout-aware comparison`
- `generate modulo`
- `generate shift left`
- `generate shift right`
- `generate shift right zero-fill`
- `generate unary minus`

The primitive literal/binop/unary tests now execute generated dev code and
assert returned values instead of only asserting that code bytes were emitted.
The list-loop test executes a two-argument statement-only LIR proc and verifies
the generated loop returns the full list length. The record equality test uses
a layout whose physical field order differs from source order and verifies
layout-aware comparison at the dev codegen boundary. The tag-payload mismatch
test pins the final-LIR invariant that a payload projection target layout must
match the published runtime payload layout; executing the invalid LIR would
trigger the backend's debug assertion/release `unreachable` path.

## Reviewed Rewrites With No Open Coverage Regression

### Compile-Time Finalization Coverage Was Restored

Deleted file:

- `src/eval/test/comptime_eval_test.zig`

Replacement:

- `src/eval/test/eval_comptime_finalization_tests.zig`
- wired through `src/eval/test/eval_tests.zig`

This restores active coverage for the deleted compile-time-eval suite using
the current checked-artifact -> MIR -> IR -> LIR -> LIR interpreter path. The
suite covers constants, imports, expect/dbg behavior, dependency ordering,
fixed-width numeric annotations, division/modulo problem paths, recursive
nominal values, static-dispatch regressions, while-loop crash regressions,
tag-payload matching, opaque function field lookup, and issue regressions from
the deleted file.

The old evaluator treated some compile-time crashes as recoverable test
outcomes. Under the current plan, a crash while publishing or evaluating a
compile-time root is a compiler invariant violation after checking. The
replacement tests keep those old names active where practical, but route them
through the final architecture instead of restoring the deleted evaluator.

### Eval Runner Rewrite Looks Stronger Overall

Deleted files include:

- `src/eval/test/anno_only_interp_test.zig`
- `src/eval/test/arithmetic_comprehensive_test.zig`
- `src/eval/test/closure_test.zig`
- `src/eval/test/eval_test.zig`
- `src/eval/test/highest_lowest_test.zig`
- `src/eval/test/interpreter_polymorphism_test.zig`
- `src/eval/test/interpreter_style_test.zig`
- `src/eval/test/low_level_interp_test.zig`
- `src/eval/test/mono_emit_test.zig`

Replacement files include:

- `src/eval/test/eval_tests.zig`
- `src/eval/test/eval_low_level_tests.zig`
- `src/eval/test/eval_closure_recursion_tests.zig`
- `src/eval/test/eval_recursive_data_tests.zig`
- `src/eval/test/eval_interpreter_style_tests.zig`
- `src/eval/test/eval_polymorphism_tests.zig`
- `src/eval/test/eval_highest_lowest_tests.zig`
- `src/eval/test/eval_comptime_finalization_tests.zig`
- `src/eval/test/parallel_runner.zig`

This is an improvement for executable behavior. The new runner exercises
interpreter/dev/wasm through a common data-driven harness, reports
backend-specific failures, and compares `Str.inspect` output across backends.
The active restored suites do not use per-case backend skips as placeholder
coverage. LLVM is reported as `NOT_IMPLEMENTED` by the runner because
statement-only LIR LLVM support is not present; that is not a newly skipped
test case.

### Refcount Behavioral Coverage Was Improved

Deleted files:

- `src/eval/test/list_refcount_alias.zig`
- `src/eval/test/list_refcount_basic.zig`
- `src/eval/test/list_refcount_builtins.zig`
- `src/eval/test/list_refcount_complex.zig`
- `src/eval/test/list_refcount_conditional.zig`
- `src/eval/test/list_refcount_containers.zig`
- `src/eval/test/list_refcount_function.zig`
- `src/eval/test/list_refcount_nested.zig`
- `src/eval/test/list_refcount_pattern.zig`
- `src/eval/test/list_refcount_simple.zig`
- `src/eval/test/list_refcount_strings.zig`

Replacement:

- `src/eval/test/host_effects_tests.zig`
- `src/eval/test/RuntimeHostEnv.zig`
- `src/eval/test/host_effects_runner.zig`

This is stronger behaviorally. The new tests assert allocation/refcount balance
for nested lists, strings, records, boxes, closure captures, boxed callables,
and recursive tag unions rather than only checking returned values.

### Deleted LIR/ARC Unit Tests Have Final-Architecture Structural Replacements

Deleted files:

- `src/lir/rc_insert.zig`
- relevant old helper tests in `src/eval/test/helpers.zig`

Replacement:

- `src/lir/arc.zig`

The replacements build current statement-only `LIR.CFStmt` graphs, run
`lir.arc.insert`, and inspect explicit `incref`, `decref`, and `free`
statements. They do not restore deleted old APIs such as `MirToLir`,
`rc_insert`, `OwnershipNormalize`, old MIR stores, borrow concepts, or
monotype helpers.

Active restored test names include:

- `RC pass-through: non-refcounted i64 block unchanged`
- `RC: string binding used twice gets incref`
- `RC: unused string binding gets decref`
- `RC: unused list binding gets decref`
- `RC borrowed string expression releases original temporary binding`
- `RC explicit retained list element keeps outer binding cleanup`
- `RC if result matched later tail-cleans matched binding`
- `RC identity call result matched later tail-cleans matched binding`
- `RC repeated identity call tail-cleans the unused second result`
- `RC mutable list binding tail-cleans borrowed final use`
- `RC mutable list loop accumulator tail-cleans current binding after borrowed final use`
- `RC branch-aware: symbol used in both match branches — no incref at binding`
- `RC branch-aware: symbol used in one match branch only — decref in unused branch`
- `RC branch-aware: symbol used twice in one branch — incref in that branch, decref in other`
- `RC branch-aware: symbol used outside and inside branches`
- `RC proc body: returning refcounted param does not tail-decref it`
- `RC proc body: returning list param does not tail-decref it`
- `RC proc_call caller: consumed refcounted arg is not tail-decref'd by caller`
- `RC proc_call caller: consumed list arg is not tail-decref'd by caller`
- `RC for_loop: elem used twice gets incref`
- `RC shadowed list decl only cleans latest generation at block tail`
- `RC mutation: reassigning refcounted var emits decref before mutation`
- `RC mutation: final use of reassignable refcounted var emits tail decref`
- `RC for_loop: unused refcounted elem does not decref borrowed element`
- `RC match guard: symbol used only in guard gets proper RC ops`
- `RC match guard+body: symbol used in both guard and body gets proper RC ops`
- `RC for_loop: wrapper block has unit result layout, not elem layout`
- `RC if_then_else: symbol used in both branches — no extra incref`
- `RC if_then_else: condition preserves live list owner for branch body`
- `RC nested match: symbol used in inner and outer match branches`
- `RC match rest prelude tail-cleans outer scrutinee binding`
- `RC nested list-pattern match tail-cleans rest binding`
- `RC combined match rest prelude with nested list pattern cleans both owners`
- `RC tag-pattern match tail-cleans outer scrutinee binding with refcounted payload`
- `RC discriminant_switch: symbol used in switch branches gets per-branch RC`
- `RC discriminant_switch: body-bound symbols don't get per-branch RC ops`
- `RC tag_payload_access: retained parent temp is released after extraction`
- `RC early_return emits correct number of decrefs for multi-use symbol`
- `RC early_return inside branch accounts for branch-level increfs`
- `RC early_return nested in call arguments gets cleanup decrefs`
- `dev lowering: list rest pattern emits two list decrefs`
- `dev lowering: mutable loop append decrefs mutable result binding once`
- `dev lowering: mutable list reassignment keeps both decrefs on the reassigned symbol`

### Final Layout Store Coverage Was Restored

Deleted file:

- `src/layout/store_test.zig`

Replacement:

- inline tests in `src/layout/store.zig`

The replacement tests restore the deleted invariant names against final layout
graph/store APIs instead of the deleted monotype/layout resolver APIs. They
cover Bool/two-nullary tag-union layout, ZST container behavior, singleton tag
payloads, recursive nominal layouts, recursive graph structural equivalence,
canonical field ordering, tuple/tag interning, resolved list layout records,
erased callable RC helper plans, and the old resolver-agreement cases as final
logical-layout graph assertions.

The old tests that asserted identical root indexes for independently built
recursive graphs are replaced with structural-equivalence assertions. Under the
final store, root indexes are allocation identities; structural equivalence and
interned child/layout shape are the semantic invariant.

### Direct REPL Coverage Was Restored

Deleted files:

- `src/repl/repl_test.zig`
- `src/repl/repl_test_env.zig`

Replacement:

- `src/cli/ReplSession.zig`

The replacement keeps all 52 direct REPL test names active while moving the
testable session core under the current CLI implementation. This is better than
resurrecting the deleted `src/repl` module. The restored tests cover expression
evaluation, definitions, Bool operations, numeric operations, string/list
operations, statement splitting, paste handling, opaque field access, and issue
9364 cases.

### ModuleEnv Serialization And Type-Printing Coverage Was Restored

Changed file:

- `src/compile/test/module_env_test.zig`

The branch replaces a long old file, including several commented-out tests,
with two active tests:

- `ModuleEnv.Serialized roundtrip`
- `ModuleEnv pushExprTypesToSExprTree extracts and formats types`

The active roundtrip still checks identifiers, exposed items, line starts,
module name, imports, import deduplication after deserialization, and
store/type data. This is a cleaner replacement, not a regression.

### Shared-Memory / LirImage Integration Coverage Was Restored

Changed file:

- `src/cli/test_shared_memory_system.zig`

Restored active tests:

- `integration - shared memory setup and parsing`
- `integration - compilation pipeline for different platforms`
- `integration - error handling for non-existent file`
- `integration - automatic module dependency ordering`
- `integration - transitive module imports (module A imports module B)`
- `integration - diamond dependency pattern (A imports B and C, both import D)`
- `integration - direct Core and Utils calls from app`

These tests use the final LirImage shared-memory API and checked-module
pipeline. They assert that a parent-published ARC-inserted LIR image can be
viewed from a mapped child-side header without exposing CIR, checked modules, or
post-check IRs to the child.

### MIR Tests Were Rewritten For The New Architecture

Deleted files:

- `src/mir/test/MirTestEnv.zig`
- `src/mir/test/lower_test.zig`

Replacement:

- `src/mir/structural_test.zig`
- inline tests across final MIR/LIR modules
- broad eval coverage for the old behavioral cases

The old tests were tightly coupled to deleted MIR APIs, old lambda-set
machinery, old monotype stores, and old lowering nodes. The new structural
tests assert final stage boundaries: no post-check dispatch nodes in
mono/executable MIR, finalized row IDs, explicit capture metadata, mandatory
callable-match support, Bool-as-normal-tag-union, annotation-only exclusion, and
stage-local expression-map limits.

### Deleted Tail-Recursion Pass Tests Are Tied To A Deleted Pass

Deleted file:

- `src/lir/TailRecursion.zig`

Deleted tests:

- `TailRecursionPass initialization`
- `TailRecursionPass: tail call is transformed to jump`
- `TailRecursionPass: non-tail call is not transformed`
- `makeTailRecursive: end-to-end transforms tail-recursive body`
- `TailRecursionPass: tail call inside switch_stmt branch is transformed`
- `TailRecursionPass: direct ret f(...) is transformed to jump`
- `TailRecursionPass: call to non-target function is not detected as tail call`

I am not marking this as an open test regression because the old pass is gone,
and the final LIR has explicit joins/jumps from IR lowering rather than this
standalone tail-recursion transform. If tail-call optimization is still a
required final-architecture feature, it should get a separate design entry and
final-stage tests instead of restoring the deleted pass tests.

### Dev `roc test` Failure-Format And Cache Coverage Improved

Changed file:

- `src/cli/test/roc_subcommands.zig`

Several dev backend tests that were skipped on `origin/main` are now active:

- cached passing test results
- cached failing test results
- cache invalidation after source change
- verbose failure report cache reuse
- non-verbose-to-verbose failure report reuse
- failure output contains source snippet
- failure output contains doc comment
- verbose and non-verbose failure format parity

This is a coverage improvement.

### Boxed Erased Callable And Readonly Static Data Host Coverage Was Added

New files/tests include:

- `test/fx/host_boxed_fn_boundary.roc`
- `test/fx/platform/Host.roc` boxed callable host APIs
- `test/fx/platform/host.zig` boxed callable host ABI helpers
- `src/cli/test/fx_platform_test.zig` boxed callable interpreter/dev tests
- `test/static-data-host/app.roc`
- `test/static-data-host/platform/host.zig`
- `test/static-data-host/platform/main.roc`
- `test/snapshots/dev_object_static_data_exports.md`

This is new coverage, not a regression. It covers host-returned, host-consumed,
host-stored, and round-tripped boxed erased callables, plus readonly exported
data with nested heap-shaped values.

### `--allow-errors` Test Behavior Changed Intentionally

Changed file:

- `src/cli/test/fx_platform_test.zig`

The old `fx platform string interpolation type mismatch (interpreter)` test
asserted that `--allow-errors` still ran enough of the erroneous program to
print `two:`. The branch now asserts that stdout is empty and diagnostics are
reported. This is an intentional semantic change backed by `plan.md`: a module
graph with user-facing errors must not publish checked artifacts, run MIR/LIR,
or execute backend/interpreter code. So this is not a coverage regression; it is
a corrected assertion for the final post-check boundary.

### Existing Skipped CLI Tests Were Not Newly Hidden

The audit found `return error.SkipZigTest` in CLI/glue/fx tests. The remaining
skipped cases I checked are pre-existing from `origin/main`, architecture/OS
specific, or unrelated to the current test deletions. The branch also removes
several dev-backend skips in `src/cli/test/roc_subcommands.zig`.

No deleted test in this audit was replaced by a skipped placeholder.

### Snapshot Diagnostic Reductions Are Mostly Cascade Trimming Or Semantic Reclassification

The diff changes many snapshot files. I reviewed the high-risk removals that
had previously represented known coverage gaps:

- `test/snapshots/default_app_wrong_arity.md` still asserts
  `MAIN! SHOULD TAKE 1 ARGUMENT`.
- `test/snapshots/try_undefined_tag.md` still asserts
  `TRY OPERATOR OUTSIDE FUNCTION`.
- `test/snapshots/match_expr/list_patterns.md` still asserts the old
  undefined-variable and unused-variable diagnostics.
- `test/snapshots/match_expr/list_rest_invalid.md` still asserts the old
  undefined-variable and unused-variable diagnostics.
- `test/snapshots/type_shadowing_across_scopes.md` intentionally no longer
  asserts `TYPE REDECLARED`; that old diagnostic was semantically wrong.
- `test/snapshots/match_expr/basic_tag_union.md` trims an `UNDEFINED VARIABLE`
  cascade and keeps the direct `TYPE MISMATCH`.
- `test/snapshots/match_expr/nested_list_scoping.md` trims an
  `UNDEFINED VARIABLE` cascade and keeps `MISSING METHOD`.
- `test/snapshots/match_expr/list_rest_scoping_variables.md` trims
  undefined/unused-variable cascade diagnostics after invalid list-rest syntax.
- `test/snapshots/nominal/type_alias_with_associated.md` trims
  `TYPE MODULE REQUIRES NOMINAL TYPE` and keeps the more direct
  `TYPE ALIAS WITH ASSOCIATED ITEMS`.

Representative broader snapshot changes also look intentional:

- `test/snapshots/expr/record_builder.md` drops old `DOES NOT EXIST`,
  `UNRECOGNIZED SYNTAX`, `MALFORMED TYPE`, and `UNUSED VARIABLE` cascades in
  favor of parser `UNEXPECTED TOKEN...` diagnostics plus
  `DECLARATION HAS NO VALUE`.
- `test/snapshots/default_app_no_main.md` no longer reports
  `MISSING MAIN! FUNCTION` for a headerless file with no `main!`; the current
  CLI has explicit coverage that such files are not default apps.
- many `match_expr/*` removals are undefined/unused-variable cascades after an
  earlier invalid pattern or malformed match.

I am not marking these as open findings because the snapshot files remain
active and the direct diagnostics are still covered. If we want a separate
diagnostic-cascade policy audit, that should be a dedicated review; it is much
broader than deleted/commented tests.

## Deleted Test Inventory Reviewed

Deleted test files or test-bearing obsolete modules reviewed in this audit:

- `src/backend/dev/LirCodeGen.zig`
- `src/cli/main.zig`
- `src/cli/test/glue_test.zig`
- `src/cli/test_shared_memory_system.zig`
- `src/compile/coordinator.zig`
- `src/eval/comptime_value.zig`
- `src/eval/dev_evaluator.zig`
- `src/eval/interpreter.zig`
- `src/eval/llvm_evaluator.zig`
- `src/eval/test/TestEnv.zig`
- `src/eval/test/anno_only_interp_test.zig`
- `src/eval/test/arithmetic_comprehensive_test.zig`
- `src/eval/test/closure_test.zig`
- `src/eval/test/comptime_eval_test.zig`
- `src/eval/test/eval_test.zig`
- `src/eval/test/helpers.zig`
- `src/eval/test/highest_lowest_test.zig`
- `src/eval/test/interpreter_polymorphism_test.zig`
- `src/eval/test/interpreter_style_test.zig`
- `src/eval/test/interpreter_style_test.zig.backup`
- `src/eval/test/list_refcount_alias.zig`
- `src/eval/test/list_refcount_basic.zig`
- `src/eval/test/list_refcount_builtins.zig`
- `src/eval/test/list_refcount_complex.zig`
- `src/eval/test/list_refcount_conditional.zig`
- `src/eval/test/list_refcount_containers.zig`
- `src/eval/test/list_refcount_function.zig`
- `src/eval/test/list_refcount_nested.zig`
- `src/eval/test/list_refcount_pattern.zig`
- `src/eval/test/list_refcount_simple.zig`
- `src/eval/test/list_refcount_strings.zig`
- `src/eval/test/low_level_interp_test.zig`
- `src/eval/test/mono_emit_test.zig`
- `src/eval/test_runner.zig`
- `src/interpreter_values/mod.zig`
- `src/layout/store_test.zig`
- `src/lir/LirExprStore.zig`
- `src/lir/MirToLir.zig`
- `src/lir/OwnershipNormalize.zig`
- `src/lir/TailRecursion.zig`
- `src/lir/rc_insert.zig`
- `src/mir/test/MirTestEnv.zig`
- `src/mir/test/lower_test.zig`
- `src/repl/mod.zig`
- `src/repl/repl_test.zig`
- `src/repl/repl_test_env.zig`

Snapshot files were not deleted. One snapshot was renamed:

- `test/snapshots/can_dot_access.md` ->
  `test/snapshots/can_field_access.md`
