# Test Coverage Audit Findings

Scope: audited the current branch against `origin/main` using the PR-style diff (`origin/main...HEAD`), focusing on test files, snapshot changes, deleted tests, skipped tests, and rewritten coverage.

No Zig commands were run for this audit.

## Summary

The large eval/MIR rewrite did delete many old test files, but most of those deletions are paired with stronger replacement coverage:

- `src/eval/test/*.zig` was reorganized from many direct `test` declarations into the new parallel eval suites and host-effects suites.
- old list refcount tests were replaced with host-effects allocation tracking that can assert zero live allocations.
- old MIR-lowering tests tied to deleted APIs were replaced with structural tests for the new MIR boundaries.
- boxed erased callable host-boundary tests and readonly static-data host tests are new coverage, not regressions.
- several dev cache tests in `src/cli/test/roc_subcommands.zig` were unskipped.

I did find several coverage regressions or suspicious weakenings that should be addressed or explicitly justified.

## Open Findings

No open findings remain from this audit.

## Resolved Findings

### `ModuleEnv.Serialized` unit coverage restored

Added active coverage in `src/compile/test/module_env_test.zig`:

- `ModuleEnv.Serialized roundtrip`
- `ModuleEnv pushExprTypesToSExprTree extracts and formats types`

The tests are wired through `src/compile/mod.zig` and cover real `ModuleEnv`
serialization/deserialization plus type extraction/formatting.

### User-facing diagnostic snapshot coverage restored

Restored active snapshot coverage for:

- `MAIN! SHOULD TAKE 1 ARGUMENT` in `test/snapshots/default_app_wrong_arity.md`
- `TRY OPERATOR OUTSIDE FUNCTION` in `test/snapshots/try_undefined_tag.md`
- `UNDEFINED VARIABLE` and `UNUSED VARIABLE` cascades in `test/snapshots/match_expr/list_patterns.md`
- `UNDEFINED VARIABLE` and `UNUSED VARIABLE` cascades in `test/snapshots/match_expr/list_rest_invalid.md`

The old `TYPE REDECLARED` expectation in
`test/snapshots/type_shadowing_across_scopes.md` was not restored because it was
semantically wrong. A top-level nominal declaration such as `Try(a, b)` is legal
elsewhere in the suite; the previous diagnostic was an incidental cascade from
the mixed malformed source, not the intended behavior. The snapshot now checks
the real parse/malformed-type diagnostics and the remaining unused-variable
warning.

### `--allow-errors` platform coverage made plan-correct

The old expectation that `roc run --allow-errors` continues into LIR execution
after user-facing errors conflicts with `plan.md`, which explicitly says
`--allow-errors` must not authorize checked-artifact publication, platform/app
relation finalization, MIR lowering, LIR lowering, backend execution, or
interpreter execution for an erroneous module graph.

The active tests in `src/cli/test/fx_platform_test.zig` now assert the intended
behavior:

- diagnostics are reported
- the CLI mode exits successfully where appropriate
- stdout is empty because the erroneous graph was not lowered or run
- the process does not crash or read garbage status

### Direct REPL unit coverage restored

Added `src/cli/ReplSession.zig` and wired it through `src/cli/main.zig` so the
CLI keeps the production entrypoint while exposing the session core to unit
tests. The restored direct test names are active again, including:

- `Repl - initialization and cleanup`
- `Repl - special commands`
- `Repl - simple expressions`
- all restored Bool, number, string, list, lambda, definition replacement, paste,
  `splitInputIntoStatements`, for-loop, opaque access, and issue 9364 tests

The restored loop tests now use current multi-line Roc loop syntax instead of
the stale semicolon-compressed form that no longer parses.

### Dev failure-format parity tests restored as active tests

Restored active dev tests in `src/cli/test/roc_subcommands.zig`:

- `roc test failure output contains source snippet (dev)`
- `roc test failure output contains doc comment (dev)`
- `roc test verbose and non-verbose failure format match (dev)`

These are no longer deleted TODO markers; they run with the current dev backend.

## Rewrites That Look Better Than Before

### Eval runner rewrite

Deleted files include:

- `src/eval/test/arithmetic_comprehensive_test.zig`
- `src/eval/test/closure_test.zig`
- `src/eval/test/comptime_eval_test.zig`
- `src/eval/test/eval_test.zig`
- `src/eval/test/highest_lowest_test.zig`
- `src/eval/test/interpreter_polymorphism_test.zig`
- `src/eval/test/interpreter_style_test.zig`
- `src/eval/test/low_level_interp_test.zig`
- `src/eval/test/list_refcount_*.zig`

Replacement files include:

- `src/eval/test/eval_tests.zig`
- `src/eval/test/eval_low_level_tests.zig`
- `src/eval/test/eval_closure_recursion_tests.zig`
- `src/eval/test/eval_recursive_data_tests.zig`
- `src/eval/test/eval_interpreter_style_tests.zig`
- `src/eval/test/eval_polymorphism_tests.zig`
- `src/eval/test/eval_highest_lowest_tests.zig`
- `src/eval/test/host_effects_tests.zig`
- `src/eval/test/RuntimeHostEnv.zig`

This appears directionally better: the new suite is data-driven, has broader recursive-data coverage, and adds explicit host-effect/RC observations instead of only checking returned values.

### Refcount coverage

The old list-specific refcount tests were replaced with host-effects tests that assert balanced live allocations for:

- nested lists of heap strings
- aliased lists in records
- boxed lists
- closure captures with heap data
- boxed callable captures
- recursive tag unions with boxed children

That is stronger than the old isolated list-refcount coverage.

### MIR structural coverage

Deleted files:

- `src/mir/test/MirTestEnv.zig`
- `src/mir/test/lower_test.zig`

Replacement:

- `src/mir/structural_test.zig`

This is an appropriate rewrite for the new architecture. The new tests assert boundary invariants such as no post-check dispatch nodes in mono/executable MIR, finalized row IDs, explicit capture metadata, mandatory callable-match support, and Bool-as-normal-tag-union representation.

### Static data and boxed erased callable coverage

New tests/fixtures:

- `test/static-data-host/*`
- `test/fx/host_boxed_fn_boundary.roc`
- `src/cli/test/fx_platform_test.zig` boxed callable tests
- `src/cli/test/fx_platform_test.zig` readonly static-data host-linking test

These are clear coverage additions, especially for host boundary behavior that did not exist on `origin/main`.

### Dev cache tests

The branch unskips dev coverage for:

- cached passing test results
- cached failing test results
- cache invalidation after source change
- verbose failure report cache reuse
- non-verbose-to-verbose failure report reuse

That is a clear improvement over `origin/main`.

### `all_syntax` expected output convergence

Changed file: `src/cli/test/roc_subcommands.zig`

The branch removes a dev-specific expected stdout for `all_syntax_test.roc` and makes dev use the same expected output as interpreter. This reduces backend divergence and looks like an improvement.

### Glue cache coverage

Changed file: `src/cli/test/glue_test.zig`

The branch removes `--no-cache` from `CGlue.roc expect tests pass (interpreter)`. That restores cache interaction coverage for this test. If it passes, it is better than the old workaround.

## Deleted Test Inventory

Deleted test files whose coverage is either replaced, intentionally obsolete, or called out above:

- `src/compile/test/module_env_test.zig`
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

Deleted/renamed snapshot file:

- `test/snapshots/can_dot_access.md` renamed to `test/snapshots/can_field_access.md`
