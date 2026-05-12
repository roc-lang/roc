# Test Coverage Audit Findings

Scope: audited the current branch against `origin/main` using the PR-style diff
(`origin/main...HEAD`), focusing on test files, snapshot changes, deleted tests,
skipped tests, and rewritten coverage.

Status: all coverage gaps from this audit have been addressed by active tests or
explicitly justified below. There are no open findings from this restore pass.

## Resolved Findings

### [P1] Compile-Time Finalization Coverage Restored

Deleted file:

- `src/eval/test/comptime_eval_test.zig`

Replacement:

- `src/eval/test/eval_comptime_finalization_tests.zig`
- wired through `src/eval/test/eval_tests.zig`

This restores active coverage for every deleted `comptime_eval_test.zig` test
name using the current checked-artifact -> MIR -> IR -> LIR -> LIR interpreter
path. The suite covers constants, imports, expect/dbg behavior, dependency
ordering, fixed-width numeric annotations, division/modulo problem paths,
recursive nominal values, static-dispatch regressions, `while` crash
regressions, tag-payload matching, opaque function field lookup, and issue
regressions from the deleted file.

The old evaluator treated some compile-time crashes as recoverable test
outcomes. In the current architecture, a crash while publishing/evaluating a
compile-time root is a compiler invariant violation after checking. The restored
tests keep those old names active but exercise crash behavior through executable
roots instead of expecting checked-artifact publication to recover from an
invariant violation.

Focused verification run:

```sh
ci/guarded_zig.sh zig build test -- --test-filter comptime
```

Result: `131 passed, 0 failed, 0 crashed, 0 skipped`.

### [P1] Final Layout Store Coverage Restored

Deleted file:

- `src/layout/store_test.zig`

Replacement:

- inline tests in `src/layout/store.zig`

The replacement tests restore the deleted invariant names against the final
layout graph/store APIs instead of the deleted monotype/layout resolver APIs.
They cover Bool/two-nullary tag-union layout, ZST container behavior, singleton
tag payloads, recursive nominal layouts, recursive graph structural
equivalence, canonical field ordering, tuple/tag interning, resolved list layout
records, erased callable RC helper plans, and the old resolver-agreement cases
as final logical-layout graph assertions.

The old tests that asserted identical root indexes for independently built
recursive graphs are replaced with structural-equivalence assertions. Under the
final store, root indexes are allocation identities; structural equivalence and
interned child/layout shape are the semantic invariant.

Focused verification run:

```sh
ci/guarded_zig.sh zig build test -- --test-filter layout
```

Result: `All 46 tests passed`.

### [P2] Diagnostic Snapshot Removals Explicitly Justified

The following snapshot diagnostic removals are intentional cascade trimming.
Each file still has focused user-facing diagnostic coverage for the primary
error in that source. The removed diagnostics depended on continuing after an
earlier syntax/name/type problem had already made later analysis unreliable.

- `test/snapshots/match_expr/basic_tag_union.md`: the removed
  `UNDEFINED VARIABLE` for `color` is intentionally trimmed; the snapshot now
  focuses on the type mismatch produced by the malformed match expression.
- `test/snapshots/match_expr/nested_list_scoping.md`: the removed
  `UNDEFINED VARIABLE` for `nestedList` is intentionally trimmed; the snapshot
  focuses on the later missing-method diagnostic.
- `test/snapshots/match_expr/list_rest_scoping_variables.md`: the removed
  `UNDEFINED VARIABLE` for `data` is intentionally trimmed because invalid
  list-rest syntax blocks reliable later name analysis.
- `test/snapshots/match_expr/list_rest_scoping_variables.md`: the removed
  `UNUSED VARIABLE` warning for first-branch `items` is intentionally trimmed
  for the same invalid-list-rest cascade reason.
- `test/snapshots/match_expr/list_rest_scoping_variables.md`: the removed
  `UNUSED VARIABLE` warning for second-branch `items` is intentionally trimmed
  for the same invalid-list-rest cascade reason.
- `test/snapshots/match_expr/list_rest_scoping_variables.md`: the removed
  `UNUSED VARIABLE` warning for third-branch `items` is intentionally trimmed
  for the same invalid-list-rest cascade reason.
- `test/snapshots/match_expr/list_rest_scoping_variables.md`: the removed
  `UNUSED VARIABLE` warning for fourth-branch `items` is intentionally trimmed
  for the same invalid-list-rest cascade reason.
- `test/snapshots/nominal/type_alias_with_associated.md`: the removed
  `TYPE MODULE REQUIRES NOMINAL TYPE` diagnostic is intentionally trimmed; the
  snapshot focuses on the more direct `TYPE ALIAS WITH ASSOCIATED ITEMS`
  diagnostic.

The removed `TYPE REDECLARED` diagnostic in
`test/snapshots/type_shadowing_across_scopes.md` remains intentionally absent:
that old diagnostic was semantically wrong because a top-level nominal
declaration such as `Try(a, b)` is legal elsewhere in the suite.

### [P2] Shared-Memory / Runtime-Image Integration Coverage Restored

Changed file:

- `src/cli/test_shared_memory_system.zig`

Wiring fix:

- `src/cli/main.zig` now imports the CLI test modules at test-only container
  scope, so tests in `test_shared_memory_system.zig` are active instead of only
  being compiled inside a wrapper test.

Restored active tests:

- `integration - shared memory setup and parsing`
- `integration - compilation pipeline for different platforms`
- `integration - error handling for non-existent file`
- `integration - automatic module dependency ordering`
- `integration - transitive module imports (module A imports module B)`
- `integration - diamond dependency pattern (A imports B and C, both import D)`
- `integration - direct Core and Utils calls from app`

These tests use the final runtime-image shared-memory API and checked-artifact
pipeline. They assert that a parent-published ARC-inserted LIR runtime image can
be viewed from a mapped child-side header without exposing CIR, checked
artifacts, MIR, or IR to the child. Platform dependency tests assert successful
checked-artifact publication and root discovery before runtime-image
publication.

Focused verification run:

```sh
ci/guarded_zig.sh zig build test -- --test-filter shared
```

Result: `All 18 tests passed`.

## Acceptable Or Improved Rewrites

### Direct REPL Coverage Restored

Deleted files:

- `src/repl/repl_test.zig`
- `src/repl/repl_test_env.zig`

Replacement:

- `src/cli/ReplSession.zig`

This remains better than the old direct REPL tests. The CLI keeps the
production entrypoint in `src/cli/main.zig`, while the session core is directly
unit-tested. The restored tests include expression evaluation, definitions,
Bool operations, numeric operations, string/list operations, statement
splitting, paste handling, opaque field access, and issue 9364 cases.

### `ModuleEnv.Serialized` Coverage Restored In Active Form

Changed file:

- `src/compile/test/module_env_test.zig`

The branch replaces a long old file, including several commented-out tests,
with two active tests:

- `ModuleEnv.Serialized roundtrip`
- `ModuleEnv pushExprTypesToSExprTree extracts and formats types`

The active roundtrip still checks identifiers, exposed items, line starts,
module name, imports, import deduplication after deserialization, and
store/type data. This is a cleaner replacement, not a regression.

### Dev `roc test` Cache And Failure-Format Coverage Improved

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

This is a clear coverage improvement.

### Eval Runner Rewrite Looks Stronger Overall

Deleted files include:

- `src/eval/test/arithmetic_comprehensive_test.zig`
- `src/eval/test/closure_test.zig`
- `src/eval/test/eval_test.zig`
- `src/eval/test/highest_lowest_test.zig`
- `src/eval/test/interpreter_polymorphism_test.zig`
- `src/eval/test/interpreter_style_test.zig`
- `src/eval/test/low_level_interp_test.zig`

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

The new runner exercises interpreter/dev/wasm in a common data-driven harness,
reports backend-specific failures, and has broader recursive-data, callable,
compile-time-finalization, and host-boundary coverage.

### Refcount Coverage Improved

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

This is stronger. The new tests assert allocation/RC balance for nested lists,
strings, records, boxes, closure captures, boxed callables, and recursive tag
unions rather than only checking returned values.

### MIR Tests Rewritten For The New Architecture

Deleted files:

- `src/mir/test/MirTestEnv.zig`
- `src/mir/test/lower_test.zig`

Replacement:

- `src/mir/structural_test.zig`
- inline tests across `src/mir/mono`, `src/mir/mono_row`, `src/mir/lifted`,
  `src/mir/lambda_solved`, and `src/mir/executable`
- broad eval coverage for the old behavioral cases

The old tests were tightly coupled to deleted MIR APIs and old lambda-set
machinery. The new structural tests assert the final stage boundaries: no
post-check dispatch nodes in mono/executable MIR, finalized row IDs, explicit
capture metadata, mandatory callable-match support, Bool-as-normal-tag-union,
and expression-map limits.

### Boxed Erased Callable And Readonly Static Data Host Coverage Added

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

### Glue Cache Coverage Improved

Changed file:

- `src/cli/test/glue_test.zig`

The branch removes `--no-cache` from `CGlue.roc expect tests pass
(interpreter)`. That restores cache interaction coverage for glue tests and
looks better than the old bypass.

## Deleted Test Inventory

Deleted test files that are either replaced, intentionally obsolete, or covered
by the resolved findings above:

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
- `src/layout/store_test.zig`
- `src/mir/test/MirTestEnv.zig`
- `src/mir/test/lower_test.zig`
- `src/repl/repl_test.zig`
- `src/repl/repl_test_env.zig`

Snapshot files were not deleted; one snapshot was renamed:

- `test/snapshots/can_dot_access.md` ->
  `test/snapshots/can_field_access.md`
