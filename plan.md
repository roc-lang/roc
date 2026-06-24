# Complete Compile-Time Roots And Static Data Plan

## Objective

Finish the compiler work required for this statement to be true without hidden
exceptions:

> Every eligible top-level value and every eligible top-level-equivalent
> expression is evaluated during checking, maximal selected roots are
> subsumed correctly, compile-time observables run during that evaluation, and
> every reachable evaluated value with a valid runtime representation is emitted
> once as target static data and shared by later uses.

The concrete integration target is Rocci Bird with
`on_screen_collided!` keeping:

```roc
base_points = [
    { x: 11, y: 2 },
    ...
].iter()
```

The final Rocci Bird proof must build with `--opt=size`, disassemble the wasm,
and confirm that the `base_points` list, the `Iter` record, its step callable,
and callable captures are restored from static data instead of rebuilt as
ordinary runtime aggregate/closure construction.

## Current Findings

The current branch has real pieces of the desired pipeline:

- effect slots and delayed dispatch watchers in `src/check/Check.zig`
- root frames and maximal-root candidate intervals in `src/check/Check.zig`
- selected hoisted roots in `src/check/hoist_roots.zig`
- checked hoisted constants in `src/check/checked_artifact.zig`
- compile-time finalization into `ConstStore` in `src/eval/compile_time_finalization.zig`
- `ConstStore` support for lists, records, tags, boxes, and function values in
  `src/check/const_store.zig`
- static data materialization in `src/compile/static_data_exports.zig`

The `.iter()` experiment exposes the remaining gap. `Iter(item)` is an opaque
record with:

```roc
{
    len_if_known : [Known(U64), Unknown],
    step : () -> [One(...), Skip(...), Done],
}
```

So a static iterator is not just a static list. It is a static record containing
a function value, and that function value captures the static list plus cursor
state.

The initial investigation found two concrete blockers preventing this from
becoming static data:

- The checker storage predicate rejected all function types:

```zig
.fn_pure,
.fn_effectful,
.fn_unbound,
=> false,
```

That makes records containing function fields fail the hoisted-constant storage
filter even when the outer value is a perfectly valid compile-time value. This
is now fixed by the `varIsStorableConstType` predicate, which still rejects
bare function roots at the root boundary but accepts function leaves inside
aggregates.

- `constValueNeedsStaticData` is value-only and treats `.fn_value` as not
needing static data:

```zig
.zst,
.scalar,
.str,
.crash,
.fn_value,
=> false,
```

That is only correct for a bare function constant that can be restored as a
callable expression. It is wrong for an aggregate whose runtime layout contains
an erased callable pointer or a finite callable payload. Static-data selection
must consume the checked type / monotype / const plan, not just the
`ConstStore` value tag.

## Non-Negotiable Invariants

- Checking is the last user-facing stage for type errors, effect errors, static
  dispatch errors, compile-time `crash`, `dbg`, and `expect`.
- Effectful calls are never executed during compile-time evaluation.
- Creating a function value is not an effectful call, even if calling that
  function would be effectful.
- Function values inside a compile-time aggregate are valid const-store values.
  The aggregate must not be rejected only because it contains a function field.
- A bare function value root remains a callable root, not a hoisted data root.
- Runtime-controlled branch bodies, match guards, and match branch values are
  not independent roots. Their contents may be evaluated only through an
  enclosing eligible control expression.
- Maximal root selection has exactly one subsumption rule: an eligible parent
  replaces child candidates from its own frame interval.
- Static data emission consumes checked roots, `ConstStore`, checked types, and
  explicit const plans. It must not rediscover eligibility from source shape,
  CIR shape, wasm bytes, object symbols, or backend code.
- Backends only consume explicit LIR static-data literals and static-data
  exports. They must not know why a value was hoisted.

## Phase 1: Add Failing Focused Tests

Start by locking down the actual missing behavior. These tests should fail
before implementation changes and pass unchanged after the fix.

- Add a checker/root-selection test for a closed local `List.iter` RHS inside a
  runtime-dependent function:

```roc
main = |arg| {
    base = [{ x: 1.I64, y: 2.I64 }].iter()
    _ = arg
    base
}
```

Expected: the local RHS is eligible for compile-time evaluation as a binding
root, despite containing the generated `step` closure in its final value.

- Add a checker/root-selection test that a closed aggregate containing a
  function field is not filtered by the "concrete stored const type" pass:

```roc
main = |arg| {
    value = { f: |n| n + 1.I64, bytes: [1.U8, 2.U8] }
    _ = arg
    value.bytes
}
```

Expected: the aggregate can be selected/stored; the bare function expression is
not selected as a standalone data root.

- Add a compile/static-data test for a reachable local `List.iter` value.
  Expected checked artifact state:

  - one hoisted constant exists for the iterator value
  - the corresponding `ConstStore` node is a record/nominal backing containing
    a `fn_value`
  - the captured list bytes are present once

- Add a static-data export test for an aggregate containing an erased callable.
  Expected LIR/static-data state:

  - the aggregate is restored as an `.assign_literal .static_data`
  - the static data export for the aggregate has a relocation to a static
    erased-callable allocation
  - the erased-callable allocation has a function-pointer relocation and
    relocations for any captured heap data

- Add a static-data export test for an aggregate containing a finite callable
  value. If finite callable materialization is not implemented yet, this test
  should document the missing path and fail until that path lands.

- Add an integration-style compiler test matching the Rocci Bird shape:

```roc
main = |anim_index| {
    base_points = [
        { x: 11.I32, y: 2.I32 },
        { x: 13.I32, y: 3.I32 },
    ].iter()

    collision_points =
        if anim_index == 2 {
            base_points.append({ x: 2, y: 1 })
        } else {
            base_points
        }

    Iter.fold(collision_points, 0.I32, |acc, point| acc + point.x + point.y)
}
```

Expected: `base_points` is static. The `if` branch bodies are not independent
roots because they are runtime-control-dependent.

## Phase 2: Split Root Eligibility From Const-Storability

The checker currently uses type concreteness as a late storage filter. That
filter must distinguish these cases explicitly:

- bare function root: not a hoisted data constant
- aggregate containing function values: valid stored constant
- open/flex/rigid unresolved type: not a stored constant
- effectful function value: storable as a value; only calling it is effectful
- effectful call inside the root: not eligible for compile-time evaluation

Implementation steps:

- [x] Rename or replace `varIsConcreteHoistedConstType` with a predicate whose name
  describes the actual contract, e.g. `varIsStorableConstType`.
- [x] Keep `exprCanBeStoredConstRoot` rejecting a standalone function-typed
  expression. That prevents bare functions from becoming hoisted data roots.
- [x] Change the recursive type predicate so function types are valid leaves when
  they appear inside aggregates.
- [x] Reject only unresolved/open type content, runtime-only placeholders, and type
  forms that truly have no const-store representation.
- [x] Update `hoistedRootHasConcreteStoredConstType` to use the new predicate.
- Add debug assertions that a selected stored root with a function-containing
  aggregate reaches `HoistedConstTable.fromRoots` and gets a const template.

Success criteria:

- Function-valued aggregates are not removed by
  `filterSelectedHoistedRootsForConstStorage`.
- Existing tests that prevent bare function roots from becoming data roots
  still pass.
- Top-level callable roots still use `.compile_time_callable`.

## Phase 3: Make Static-Data Selection Type/Plan-Aware

`constValueNeedsStaticData(view, value)` is too weak. Whether a stored value
needs static data depends on the runtime representation selected for the
checked type.

Implementation steps:

- Replace `constNodeNeedsStaticData(view, node)` with a function that also
  receives the monotype/type shape available at the use site.
- Recurse through the `ConstStore` node and the monotype together.
- Return `true` for:

  - non-empty lists
  - boxes with non-ZST runtime payloads
  - erased callable values, because their runtime representation is a pointer
    to an erased-callable payload
  - finite callable values whose runtime layout contains non-zero capture/tag
    payload data
  - aggregates containing any child that needs static data

- Return `false` for:

  - scalars
  - small/direct strings that do not need backing storage
  - ZSTs
  - bare finite callables whose selected runtime layout is zero-sized and has
    no capture payload

- Update both stored-const restore paths in `src/postcheck/monotype/lower.zig`:

  - `restoredHoistedConstAtType`
  - `restoreConstUseAtType`

- Keep the decision in Monotype lowering, where the checked value and requested
  monotype are both available. Do not move this decision to a backend.

Success criteria:

- An `Iter` record restored at runtime lowers to `.static_data` instead of
  rebuilding its record and packing its `step` callable.
- A bare function-valued constant still restores as a callable expression and
  does not request a static data literal.

## Phase 4: Complete Callable Static Materialization

The static-data builder already supports erased callable payloads through
`.erased_fn`. It must also have a complete story for any callable layout that a
stored aggregate can contain.

Implementation steps:

- Keep and test the existing erased-callable path:

  - write a static allocation for the erased-callable payload
  - write the function-pointer relocation
  - write capture fields using the explicit `CaptureSlot` plans
  - use the static-data allocation header/refcount conventions

- Implement finite callable static materialization for `.fn_value` const plans:

  - select the matching `FnVariant` from the stored `ConstFn`
  - if the callable layout is ZST, write no bytes
  - if it is a single-variant capture payload, write the capture payload
    directly using `variant.captures`
  - if it is a tag-union layout, write the discriminant and selected capture
    payload into the selected variant's layout
  - recursively materialize capture values, including lists, boxes, strings,
    erased callables, and nested callable values

- Replace the current invariant:

```zig
.fn_value => staticDataInvariant(...)
```

with explicit finite-callable materialization.

- Add materialization tests for:

  - zero-capture finite callable in an aggregate
  - finite callable capturing a scalar
  - finite callable capturing a list
  - finite callable capturing another callable
  - erased callable capturing a list
  - erased callable capturing another erased callable

Success criteria:

- Static-data export generation succeeds for every callable-containing
  aggregate that the checker/ConstStore can produce.
- Unsupported callable shapes are represented by missing explicit producer data,
  not by a source-shape fallback or backend guess.

## Phase 5: Verify Compile-Time Evaluation Coverage

The plan is not complete until every module and every selected eligible root is
evaluated during checking, including unreachable top-level values needed only
for diagnostics.

Implementation steps:

- Audit `CompileTimeRootTable.fromModule`, `RootRequestTable.fromModule`, and
  `CompileTimeRequestScheduler`.
- Add or strengthen tests for:

  - unused top-level `crash` in the root module
  - unused top-level `crash` in an imported module
  - unused top-level `dbg` in an imported module
  - unused top-level failed `expect` in an imported module
  - an unreachable successful top-level constant that is evaluated for
    diagnostics but not emitted as target static data
  - selected roots shared by top-level values and local uses producing one
    diagnostic

Success criteria:

- `roc check` evaluates all eligible diagnostic roots across the import graph.
- Successful unreachable values do not appear in target static data unless a
  reachable checked root references them.

## Phase 6: Verify Maximal Root Subsumption

The checker already has root frames and delayed intervals, but the completed
work must prove this for callable-containing aggregates too.

Tests to add or strengthen:

- parent record containing list and function field replaces the child list root
- parent record containing `List.iter(...)` replaces the child list root
- runtime-dependent parent preserves an eligible callable-containing child root
- delayed static dispatch resolving pure replaces children
- delayed static dispatch resolving effectful preserves children
- runtime-controlled branch body does not publish a closed child root
- compile-time-known branch containing `crash` reports the crash during
  checking
- untaken runtime branch containing `crash` does not report during checking

Success criteria:

- There is still exactly one subsumption rule: eligible parent frame replaces
  children in its own interval.
- There are no new leaf/type/source-shape root filters.

## Phase 7: Rocci Bird Proof

Use the Rocci Bird source with `.iter()` left on `base_points`.

Steps:

1. Build the compiler with `zig build`.
2. Build the wasm4 host with `zig build -Doptimize=ReleaseSmall`.
3. Build Rocci Bird:

```sh
cd /home/rtfeldman/code/roc-wasm4
/home/rtfeldman/code/worktrees/roc/vivid-canyon/roc/zig-out/bin/roc build \
  examples/rocci-bird.roc \
  --opt=size \
  --output=rocci-bird.wasm
```

4. Record the byte size and sha256.
5. Disassemble the wasm.
6. Confirm:

   - the `base_points` list bytes are in static data
   - the `Iter` record is in static data
   - the `step` callable is represented by static callable data
   - the callable capture points at the same static list bytes
   - `update`/`on_screen_collided!` no longer constructs `base_points.iter()`
     at runtime
   - any remaining `Iter.append` helpers come only from the runtime-controlled
     `collision_points` branch expressions, not from rebuilding `base_points`

7. If the goal is also to make all three collision-point variants static, make
   that an explicit source or language decision. Under the current semantic
   rules, branch values controlled by runtime `anim_index` are not independent
   roots, because hoisting them would run compile-time observables in branch
   bodies that the source program might not evaluate.

Success criteria:

- Rocci Bird builds and runs.
- The `.iter()` change no longer increases code size by pulling in runtime
  construction of the base iterator.
- The final report distinguishes the semantic static-data win from any
  remaining runtime iterator append code caused by runtime branch control.

## Final Checklist

- [ ] Failing tests capture `List.iter` local root selection and static data.
- [x] Function-containing aggregates are allowed stored hoisted constants.
- [x] Bare function roots still use callable-root handling, not data roots.
- [ ] Static-data selection consumes type/plan information.
- [ ] Erased callable static data is covered by tests.
- [ ] Finite callable static data is implemented and covered by tests.
- [ ] Compile-time diagnostics run for eligible roots across all modules.
- [ ] Maximal root subsumption is tested for callable-containing aggregates.
- [ ] Rocci Bird with `base_points.iter()` builds with `--opt=size`.
- [ ] Rocci Bird disassembly proves the base iterator is static, shared data.
