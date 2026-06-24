# Complete Compile-Time Roots And Static Data Plan

## Goal

Complete the compiler work required for this statement to be true without
asterisks:

> Every eligible top-level value and every eligible top-level-equivalent
> expression is evaluated during checking; maximal selected roots are subsumed
> correctly; compile-time observables run during that evaluation; and every
> reachable evaluated value that needs a target static representation is
> emitted once as static data and shared by later uses.

The concrete proof case is Rocci Bird with the `on_screen_collided!`
`base_points` value left in this shape:

```roc
base_points = [
    { x: 11, y: 2 },
    ...
].iter()
```

The final proof is not just "Rocci Bird builds." The final proof is:

- `base_points` is evaluated during checking.
- The static list backing for those points is emitted once.
- The `Iter` value is emitted as static data.
- The iterator `step` callable is represented by static callable data.
- The callable captures point at the same static list backing.
- `on_screen_collided!` no longer rebuilds the base list, iterator record, or
  base iterator closure at runtime.

## Terms

An expression is eligible for compile-time evaluation when all of these are
true:

- it has no runtime data dependency
- it is unconditionally evaluated at the point where the selected root is
  evaluated
- it contains no effectful call
- it is not poisoned by an already-owned checking error

Unresolved or erroneous expressions are not eligible for hoisting, and that
poison is local to the expression or dependency region that owns the checking
error. A separate eligible expression elsewhere in the same module or program
must still be hoisted. A module or program that contains diagnostics is not
globally disqualified from compile-time evaluation; only the affected
expression/dependency region is. Poison propagates through explicit checked
dependencies, such as using an erroneous local or top-level value, and nowhere
else. This is not a general "unsupported shape" escape hatch: if a well-checked
eligible expression cannot be evaluated, stored, restored, or emitted correctly,
that is a compiler bug to fix with a regression test.

`crash`, `dbg`, and `expect` are compile-time observables. They are not
effectful calls. If an eligible selected root contains them, they must run
during checking.

Runtime-controlled branch bodies, match guards, and match branch values are not
standalone roots. Their contents can only run at compile time through an
eligible enclosing control expression.

Evaluation and target static-data emission are separate:

- all eligible top-level values and selected hoisted roots must be evaluated
  during checking so diagnostics are correct
- successful unreachable values do not need target static data
- reachable evaluated values that need a target static representation must be
  emitted once and shared

## Current State

The branch already has the major pieces of the pipeline:

- checker root frames and selected-root intervals in `src/check/Check.zig`
- selected hoisted root records in `src/check/hoist_roots.zig`
- compile-time root publication in `src/check/checked_artifact.zig`
- checked `ConstStore` support for lists, records, tags, boxes, strings, and
  function values in `src/check/const_store.zig`
- compile-time finalization through LIR interpretation in
  `src/eval/compile_time_finalization.zig`
- Monotype restoration of stored consts in `src/postcheck/monotype/lower.zig`
- LIR const plans for records, lists, boxes, finite callables, and erased
  callables in `src/postcheck/solved_lir_lower.zig`
- static-data export materialization in `src/compile/static_data_exports.zig`

This part is already implemented and tested:

- a function-valued aggregate can be a stored hoisted constant
- a bare function root is still rejected as a data root and restored through
  callable-root handling

The remaining blockers are:

- static-data selection in Monotype is still partly value-only:

```zig
fn constNodeNeedsStaticData(_: *Builder, view: ModuleView, node: checked.ConstNodeId) bool
```

- that value-only function treats `.fn_value` as "no static data," which is
  wrong for a reachable aggregate whose runtime representation contains a
  callable payload
- finite callable static-data materialization exists for the captured-list
  path, but still needs complete focused coverage for zero-capture,
  scalar-capture, nested-callable-capture, and multi-variant callable-set
  layouts
- erased callable static data exists, but needs focused tests in the
  callable-containing aggregate path
- cross-module compile-time diagnostics need tests proving every eligible
  module-level root is evaluated even when unreachable from runtime roots
- Rocci Bird has not yet been rebuilt and disassembled after the full static
  callable path is complete

## Invariants

- There is one source of truth for root eligibility: the checker's existing
  expression traversal in `src/check/Check.zig`.
- There is one subsumption rule: an eligible parent frame replaces child
  candidates in that frame's candidate interval.
- Checker-error poison is expression/dependency-local. It is never a
  module-wide, package-wide, or program-wide hoisting disable switch.
- No backend may rediscover root eligibility from source shape, names, wasm
  bytes, object symbols, or generated code.
- No stage after checking reports user-facing type/effect/static-dispatch
  errors.
- Effectful calls are never evaluated at compile time.
- Creating a function value is not an effectful call.
- Function values inside compile-time aggregates are valid `ConstStore` values.
- Static-data export writing consumes explicit `ConstStore` nodes, checked
  types, LIR layouts, and LIR const plans.
- Backends only lower explicit LIR statements and static-data exports.

## Phase 1: Lock The Bug Down With Failing Tests

Add tests before implementation changes. Each test should fail for the current
reason, not because of syntax, platform setup, or missing imports.

### Checker Root Tests

File: `src/check/test/hoist_roots_test.zig`

Add a test for a closed local iterator RHS inside a runtime-dependent
function:

```roc
main = |arg| {
    base = [{ x: 1.I64, y: 2.I64 }].iter()
    _ = arg
    base
}
```

Expected:

- the selected root for `base` exists
- the root is a binding root or root body for the full iterator value, not just
  the child list
- no standalone closure/function root is selected
- no child list root remains if the iterator parent is selected

Add a control test where the parent is runtime-dependent and the closed child
survives:

```roc
main = |arg| {
    value = {
        base: [{ x: 1.I64, y: 2.I64 }].iter(),
        runtime: arg,
    }
    value.runtime
}
```

Expected:

- the runtime-dependent record is not selected
- the closed iterator child remains selected

Add a callable aggregate subsumption test:

```roc
main = |arg| {
    value = { f: |n| n + 1.I64, bytes: [1.U8, 2.U8] }
    _ = arg
    value
}
```

Expected:

- the record root is selected
- the list child is not selected
- the function expression is not selected as a data root

Add checker-error poison locality tests:

```roc
bad = unknown_name

good = [1.I64, 2.I64].iter()

main = good
```

Expected:

- the `bad` definition owns the original checking diagnostic
- `good` is still selected/evaluated as a compile-time root
- `main` still depends on the stored `good` value
- no module-wide or program-wide "has diagnostics" bit suppresses `good`

Add the dependent poisoned case:

```roc
bad = unknown_name

dependent = [bad, 1.I64].iter()

independent = [2.I64, 3.I64].iter()
```

Expected:

- `dependent` is not selected because it explicitly depends on `bad`
- `independent` is still selected
- the original `bad` diagnostic is not duplicated by attempting to hoist
  `dependent`

### Compile-Time Diagnostic Tests

File: `src/eval/test/eval_comptime_finalization_tests.zig` or a new focused
test file under `src/eval/test`.

Use the existing publish/finalization helpers in `src/eval/test_helpers.zig`.
Add tests for:

- unused top-level `crash` in the root module reports during `roc check`
- unused top-level `crash` in an imported module reports during `roc check`
- unused top-level `dbg` in an imported module reports during `roc check`
- unused failed `expect` in an imported module reports during `roc check`
- an unreachable successful top-level constant is evaluated but does not create
  target static data
- two selected roots that share a top-level dependency produce the diagnostic
  once

### Static-Data Tests

File: `src/compile/test/hoisted_constants_test.zig`

Add a static-data test for a reachable local `List.iter` value:

```roc
main! = |args| {
    base = [{ x: 1.I64, y: 2.I64 }, { x: 3.I64, y: 4.I64 }].iter()
    total = Iter.fold(base, 0.I64, |acc, point| acc + point.x + point.y + List.len(args).to_i64_wrap())
    _ = total
    Ok({})
}
```

Expected checked/LIR state:

- at least one hoisted constant exists for the iterator value
- the stored `ConstStore` node for that hoisted constant contains a
  `.fn_value`
- lowering emits `.assign_literal .static_data` for the iterator use
- the point-list payload bytes appear once in static exports

Add erased callable aggregate coverage:

- a record containing an erased callable that captures a list
- expected static-data export has a relocation to an erased callable allocation
- that erased callable allocation has a function-pointer relocation and a
  relocation to the captured list backing

Add finite callable aggregate coverage:

- zero-capture finite callable in a record
- finite callable capturing a scalar
- finite callable capturing a list
- finite callable capturing another finite callable
- finite callable in a multi-variant callable set, so the discriminant path is
  tested

These tests should initially expose the `.fn_value` materialization invariant.

## Phase 2: Finish Checker Eligibility And Subsumption

Files:

- `src/check/Check.zig`
- `src/check/hoist_roots.zig`
- `src/check/checked_artifact.zig`
- `src/check/test/hoist_roots_test.zig`

Tasks:

1. Audit every remaining root-selection suppression path in `Check.zig`.
   Valid suppressions are only:

   - runtime data dependency
   - runtime control dependency
   - effectful call
   - checker-error poison
   - unsupported control-transfer root shape until continuation roots exist

2. Remove or rewrite any remaining source-shape filters that reject leaves,
   strings, numbers, empty lists, records, calls, `crash`, `dbg`, or `expect`
   as a category.

3. Keep lambda body root selection detached from function value creation, but
   ensure detached bodies publish their own eligible final-expression roots
   only when those bodies are unconditionally evaluated in their own context.

4. Add debug assertions after
   `filterSelectedHoistedRootsForConstStorage` proving:

   - selected stored roots are not function-typed at the root boundary
   - selected stored roots can contain nested function-typed leaves
   - every selected root that survives filtering gets a `HoistedConstTable`
     entry

5. Strengthen maximal-root tests:

   - parent record over child list
   - parent record over child list plus function field
   - parent iterator over child list plus step closure
   - runtime-dependent parent preserving child iterator
   - delayed pure dispatch replacing children
   - delayed effectful dispatch preserving children
   - runtime-controlled branch body not publishing a closed child root
   - compile-time-known branch with `crash` reporting at check time
   - untaken runtime branch with `crash` not reporting at check time

Success criteria:

- `zig build run-test-zig-module-check` passes.
- There is still exactly one parent-over-child interval replacement rule.
- The `List.iter` checker tests prove inline and named iterator shapes are
  equivalent when dependencies/effects/control are equivalent.

## Phase 3: Prove Compile-Time Evaluation Covers All Modules

Files:

- `src/check/checked_artifact.zig`
- `src/eval/compile_time_finalization.zig`
- `src/eval/test_helpers.zig`
- `src/eval/test/eval_comptime_finalization_tests.zig`

Current architecture:

- `CompileTimeRootTable.fromModule` publishes top-level values, selected
  hoisted roots, literal conversion roots, and `expect` roots for one module.
- `RootRequestTable.fromModule` requests concrete compile-time roots and test
  expects.
- `CompileTimeRequestScheduler` sorts each module's compile-time requests by
  explicit const dependencies.
- `compile_time_finalization.zig` lowers and evaluates those requests, fills
  root payloads, and fills stored const templates.
- imported modules are published before dependents, so their finalizers should
  run as each artifact is published.

Tasks:

1. Audit package/coordinator publication paths to prove every imported module
   goes through `publishFromTypedModule` with `CompileTimeFinalization.finalizer()`.

2. Add tests that would fail if imported modules were published without
   compile-time finalization.

3. Add tests that would fail if compile-time request sorting skipped a
   dependency or let a dependent run before its stored const dependency.

4. Add tests that successful unreachable constants are evaluated for
   diagnostics but not emitted as target static data when no reachable root
   references them.

5. Add tests that `dbg` and `expect` diagnostics dedupe by source/root and are
   not duplicated when a dependency is shared by multiple selected roots.

Success criteria:

- `roc check` reports compile-time `crash`, `dbg`, and failed `expect` from
  eligible top-level roots in every module in the import graph.
- successful unreachable constants do not appear in `static_data_values` or
  static-data exports unless a reachable root references them.

## Phase 4: Make Static-Data Selection Explicit And Type-Aware

Files:

- `src/postcheck/monotype/lower.zig`
- `src/postcheck/monotype/ast.zig` only if a new expression/data marker is
  needed
- `src/postcheck/monotype_lifted/*` only if a new marker must pass through
  lifting
- `src/postcheck/lambda_solved/*` only if a new marker must pass through
  solving
- `src/postcheck/lambda_mono/*` only if a new marker must pass through lambda
  mono
- `src/postcheck/solved_lir_lower.zig`
- `src/compile/test/hoisted_constants_test.zig`

The current `constNodeNeedsStaticData(view, node)` must be replaced. It is
wrong because it looks only at the `ConstStore` value tag. It cannot tell the
difference between:

- a bare finite callable constant, which should restore as a callable value
- a record/opaque/iterator value whose runtime representation contains a
  callable payload that must be stored as part of the aggregate

Implement the smallest architecture change that still obeys the pipeline
boundaries:

1. Replace `constNodeNeedsStaticData(view, node)` with a function whose inputs
   include:

   - the `ConstStore` node
   - whether this is the root node or a nested child
   - the lowered Monotype type at the use site
   - the checked type used for the static-data request

2. Recurse through the `ConstStore` node and Monotype type together. The
   recursion must unwrap named/nominal backing types through explicit checked
   backing data. It must assert if the value shape and type shape disagree.

3. Return `true` for reachable stored const uses whose target runtime
   representation needs backing/static bytes or relocations:

   - non-empty lists
   - strings that are not fully small/direct
   - boxes with non-ZST payloads
   - erased callable values
   - nested finite callable values whose stored function has capture payload or
     whose callable set needs a runtime discriminant
   - aggregates containing any child that needs static data

4. Return `false` for:

   - scalars
   - ZSTs
   - small/direct strings
   - empty lists
   - bare callable constants that should restore through callable handling
   - finite callables whose runtime representation is ZST

5. Update both Monotype stored-const restore paths:

   - `restoredHoistedConstAtType`
   - `restoreConstUseAtType`

6. If Monotype type information is still not precise enough to distinguish a
   finite callable with runtime tag payload from a ZST callable, do not add a
   source-shape guess. Instead, carry an explicit stored-const marker farther
   through the post-check pipeline until `solved_lir_lower.zig` has the
   callable const plan and layout, then make the `.static_data` decision there.
   That is a larger change, but it is the correct escape hatch if Monotype
   lacks explicit data.

Success criteria:

- an `Iter` record use lowers to `.assign_literal .static_data`
- a bare function-valued constant still restores without static-data literals
- static-data selection uses explicit checked/Monotype/LIR data only
- no backend or wasm post-pass participates in the decision

## Phase 5: Complete Callable Static-Data Materialization

File: `src/compile/static_data_exports.zig`

Current erased callable support:

- `.erased_fn` writes a pointer to an erased-callable allocation
- the erased-callable allocation contains a function-pointer relocation
- captures are written by `writeCaptures` using explicit capture slots and
  child const plans

Keep that path and cover it with tests.

Implement finite callable materialization for `.fn_value` const plans.

Tasks:

1. Replace the `.fn_value => staticDataInvariant(...)` branch in `writeValue`
   with `writeFnValue`.

2. Add `fnVariantForConstFn(set_id, fn_value)`:

   - load `lir.Program.FnSet`
   - select the `FnVariant` whose template matches the stored `ConstFn`
   - use the same template equality as erased callable materialization
   - assert if no variant matches

3. Implement `writeFnValue`:

   - verify the `ConstStore` node is `.fn_value`
   - verify the requested layout matches the `FnSet.layout`
   - if the callable value layout is ZST, verify all selected capture layout
     data is ZST and write no bytes
   - if the callable value layout is a single-variant capture payload, write
     captures directly at `base_offset` with `writeCaptures`
   - if the callable value layout is a tag union, write the selected variant
     discriminant and write captures into the selected payload layout
   - if the selected variant has no captures but the callable layout is a tag
     union, still write the discriminant

4. Reuse `writeCaptures` for finite callables. Do not duplicate capture field
   ordering logic.

5. Make capture recursion support:

   - scalar captures
   - list captures
   - string captures
   - box captures
   - finite callable captures
   - erased callable captures
   - nominal/opaque captured values

6. Make static allocation dedup include callable allocations by bytes plus
   relocations, as it already does for list/string/box allocations.

Success criteria:

- static-data export generation succeeds for erased and finite callable
  aggregates
- unsupported callable shapes fail only because explicit producer data is
  missing or inconsistent
- no source-shape fallback is introduced

## Phase 6: Keep Reachability And Target Emission Separate

Files:

- `src/lir/checked_pipeline.zig`
- `src/lir/reachable_procs.zig`
- `src/compile/static_data_exports.zig`
- `src/compile/test/hoisted_constants_test.zig`

Tasks:

1. Verify `collectStaticDataRequests` is only for provided data roots and does
   not accidentally emit unreachable compile-time values.

2. Verify internal hoisted/static values are emitted only when runtime LIR has
   an `.assign_literal .static_data` use.

3. Verify imported static consts referenced by the root module can be
   materialized from imported module `ConstStore` data.

4. Verify `ReachableProcs.run` marks procedures reachable through erased
   callable static data and marks capture const plans through both erased and
   finite callable plans.

5. Add tests for:

   - reachable imported static list
   - reachable imported iterator/callable aggregate
   - unreachable imported successful constant not emitted
   - provided data root containing function field
   - internal hoisted data root containing function field

Success criteria:

- evaluation coverage does not imply target emission
- target emission is strictly reachability-driven
- callable static data marks the procedures and child const plans it needs

## Phase 7: Rocci Bird Proof

Keep the `.iter()` change in Rocci Bird.

Source repo:

```sh
cd /home/rtfeldman/code/roc-wasm4
```

Compiler repo:

```sh
cd /home/rtfeldman/code/worktrees/roc/vivid-canyon/roc
```

Steps:

1. Build the compiler:

```sh
zig build
```

2. Build the wasm4 host in size mode:

```sh
cd /home/rtfeldman/code/roc-wasm4
zig build -Doptimize=ReleaseSmall
```

3. Build Rocci Bird:

```sh
/home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc build \
  examples/rocci-bird.roc \
  --opt=size \
  --output=rocci-bird.wasm
```

4. Record:

- byte size
- sha256
- compiler commit
- wasm4 commit

5. Disassemble the wasm.

6. Confirm in the disassembly:

- the point list bytes appear in static data
- the point list bytes appear once
- the `Iter` record is loaded from static data
- the `step` callable is loaded from static callable data
- the callable capture references the same static point-list allocation
- `on_screen_collided!` does not contain runtime construction of
  `base_points.iter()`
- any remaining iterator append code corresponds to runtime-controlled branch
  expressions, not rebuilding the base iterator

7. Run the game through wasm4 and verify behavior still matches the current
   working build.

Success criteria:

- Rocci Bird builds with `--opt=size`
- the `.iter()` version is no larger because of runtime base-iterator rebuilds
- the disassembly proves static sharing, not merely a smaller byte count

## Phase 8: Required Verification Commands

Run targeted tests as each phase lands:

```sh
zig build run-test-zig-module-check -- --test-filter "hoist roots"
zig build run-test-zig-module-compile -- --test-filter "static data"
zig build run-test-zig-module-compile -- --test-filter "hoisted"
zig build run-test-eval
```

Then run the full relevant suites:

```sh
zig build run-test-zig-module-check
zig build run-test-zig-module-compile
zig build run-test-eval
```

Before each commit:

```sh
zig fmt <changed .zig files>
git diff --check
```

Final integration verification:

```sh
zig build
cd /home/rtfeldman/code/roc-wasm4
zig build -Doptimize=ReleaseSmall
/home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc build examples/rocci-bird.roc --opt=size --output=rocci-bird.wasm
```

## Final Checklist

- [x] Failing tests capture local `List.iter` root selection.
- [x] Failing tests capture local `List.iter` static-data emission.
- [x] Function-containing aggregates are allowed stored hoisted constants.
- [x] Bare function roots still use callable-root handling, not data roots.
- [x] Root selection has no invalid leaf/source-shape/observable filters.
- [x] Imported eligible top-level diagnostics run during checking.
- [x] Unreachable successful evaluated constants do not emit target static data.
- [x] Maximal root subsumption is tested for callable-containing aggregates.
- [x] Finite callable static data materializes a captured static list.
- [ ] Static-data selection consumes explicit value and type/layout data.
- [ ] Erased callable static data is covered by tests.
- [ ] Finite callable static data has full zero/scalar/list/nested/multi-variant coverage.
- [ ] Reachability marks callable static-data procedures and capture plans.
- [ ] Rocci Bird with `base_points.iter()` builds with `--opt=size`.
- [ ] Rocci Bird disassembly proves the base iterator is static, shared data.
