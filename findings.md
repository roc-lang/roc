# Test Coverage Audit Findings

Scope: this file audits test-related changes in the current branch against `origin/main`, with special attention to deleted, commented-out, skipped, or rewritten tests.

I did not treat every deleted old-architecture test as a regression. Tests whose old assertions depended on deleted MIR/lowering APIs are only findings when the final architecture still needs equivalent coverage.

No Zig commands have been run after the latest coverage additions; these tests are intentionally committed first so the guarded Zig run can reveal the real implementation state.

## Resolved Findings

- MIR structural boundary coverage now has explicit tests in `src/mir/structural_test.zig`.
- Layout-store/RC-helper coverage now has replacement inline tests in `src/layout/store.zig`.
- Checked-artifact cache-key coverage now has positive and negative tests in `src/compile/cache_key.zig` and `src/check/checked_artifact.zig`.
- Interpreter CLI failure-format assertions were restored in `src/cli/test/roc_subcommands.zig`.
- `can_field_access.md` now preserves the old undefined-variable diagnostic coverage using explicit snapshot metadata.
- Deleted Rust ability-specialization inspect snapshots were restored, and the ignored `inspect_custom_type` fixture was repaired.
- Boxed erased callable host-boundary coverage remains a clear improvement over `origin/main`.
- Static data export host-linking coverage remains a clear improvement over `origin/main` for the dev/native object path.

## Additional Coverage Added For The Larger Gaps

### Compile-Time Finalization

New structural tests in `src/check/checked_artifact.zig` assert the final checked-artifact contract:

- `PublishInputs.compile_time_finalizer` is an explicit, non-optional field.
- compile-time root requests and ABI kinds are published explicitly.
- compile-time evaluation requests are explicit `local_root`, `const_instance`, and `callable_binding_instance` cases.
- top-level values can only be `const_ref` or `procedure_binding`.
- top-level runtime thunks, global initializer procs, and top-level closure objects are not representable in `TopLevelValueKind` or `ConstTemplateState`.
- checked artifacts own the compile-time specialization/reuse tables: `const_instances`, `callable_binding_instances`, `semantic_instantiation_procedures`, `promoted_procedures`, and dependency stores.

New eval tests in `src/eval/test/eval_tests.zig` cover user-visible compile-time finalization behavior:

- top-level callable results produced by compile-time evaluation are callable at runtime.
- boxed callable results produced by compile-time evaluation use the same callable path.
- top-level records, nested records, tag payloads, and lists may contain callable values.

New provided-export structural tests in `src/check/checked_artifact.zig` cover recursive boxed constants and boxed callable constants as data exports rather than runtime roots.

### Annotation-Only Runtime Forms

New eval tests in `src/eval/test/eval_tests.zig` assert that annotation-only top-level values are reported as problems, including when the reference appears in an untaken branch.

New MIR structural tests in `src/mir/structural_test.zig` assert that `anno_only` is not representable in mono, row-finalized mono, lifted, lambda-solved, or executable MIR, and that mono lowering has explicit invariant checks for checked non-runtime expression forms.

### Ordinary RC Balance

The runtime host-effects harness now records live allocations in `RuntimeHostEnv.RecordedRun`, serializes that count across child processes, and lets tests assert `expected_live_allocations`.

New host-effects tests in `src/eval/test/host_effects_tests.zig` assert zero live allocations after:

- nested lists of heap strings
- aliased lists stored in records
- boxed lists of heap strings
- closure captures containing heap strings
- boxed callable captures containing nested heap data
- recursive tag unions with boxed children

This complements value-oriented eval tests by asserting balanced runtime allocation/deallocation through the same interpreter/dev host-effects runner.

### Dev Cache Coverage

The cache-specific dev backend tests in `src/cli/test/roc_subcommands.zig` have been unskipped:

- passing test result cache reuse
- failing test result cache reuse
- source-change invalidation
- verbose failure cache reuse
- non-verbose-to-verbose failure cache reuse

Any failures now represent implementation work instead of hidden test coverage gaps.

### Static Data Export Scope

The native host-linking fixture remains the correct primary coverage for readonly exported constants because the feature is ordinary symbol linking from a generated native object.

Coverage now includes:

- primitive exported constants
- nested records
- strings
- nested lists
- recursive tag-union data with boxed children
- static refcount behavior for readonly data
- checked-artifact publication of boxed callable constants as data exports

If `plan.md` later requires an interpreter or Wasm host-symbol model for readonly exported constants, that would be a new design requirement rather than a missing replacement for the deleted old tests.

## Open Findings

No test-coverage gaps from the audit are intentionally left open. The next guarded Zig run may expose implementation bugs or tests that need syntax/fixture repair, but those will be failures in active coverage rather than skipped or deleted coverage.

## Deleted Test Inventory

Deleted test-related files whose coverage is now either replaced or intentionally obsolete:

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
- `src/eval/test/list_refcount_*.zig`
- `src/eval/test/low_level_interp_test.zig`
- `src/eval/test/mono_emit_test.zig`
- `src/eval/test_runner.zig`
- `src/layout/store_test.zig`
- `src/mir/test/MirTestEnv.zig`
- `src/mir/test/lower_test.zig`
- `src/repl/repl_test.zig`
- `src/repl/repl_test_env.zig`
- `test/snapshots/can_dot_access.md`
