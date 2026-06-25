# Builtin Iterator Plan Completion Plan

## Goal

Make optimized Roc iterator code compile like an explicit state machine while
preserving the public `Iter` API as ordinary pure Roc functions.

The end state is:

- every builtin iterator producer that the compiler understands lowers to an
  explicit post-check iterator plan value
- iterator plan values propagate through ordinary post-check value positions,
  including locals, blocks, `if`, `match`, and specialized function calls
- optimized consumers consume known plans directly without constructing public
  `Iter` records, public step records, or step closures in the hot loop
- public observation boundaries materialize exactly the same public `Iter`
  value that the Roc builtin implementation would have produced
- unmaterialized plans never reach LIR or any backend
- Rocci Bird with and without a top-level `.iter()` in `on_screen_collided!`
  produces equivalent optimized code shape and a comparable `--opt=size`
  wasm size

## Motivation

Rocci Bird exposed that Roc's current optimized wasm output is sensitive to
whether collision points are written as a list or as `list.iter()`. After
adding an `Append` step to the public iterator API, a `for` loop over:

```roc
base_points = [...].iter()

collision_points =
    if anim_index == 2 {
        base_points.append({ x: 2, y: 1 }).append({ x: 7, y: 1 })
    } else if anim_index == 1 {
        base_points.append({ x: 2, y: 2 })
    } else {
        base_points
    }

for point in collision_points {
    ...
}
```

should not allocate iterator wrappers, call public step closures, construct
public `One`/`Append`/`Done` values, or keep unreachable step variants alive in
the hot path. The public source is pure and reusable, but the optimized
consumer should see a private cursor state machine.

The earlier direct-recognition work proved that lowering a syntactically
visible `list.iter()`, `Iter.single(...)`, or append chain can remove the
public iterator overhead for a direct `for`. That is not enough. Rocci Bird's
important case flows through locals and conditionals. Re-recognizing the
checked expression at the `for` use site would be wrong because it can
re-evaluate declaration RHSs, branch conditions, appended item expressions,
`dbg`, `expect`, or `crash`. Mining the materialized public `Iter` record is
also wrong because that makes compiler optimization depend on the private shape
of the Roc builtin implementation.

The correct long-term design is first-class iterator plan values in the
post-check IR. A plan value is produced once, at the same source point where the
public `Iter` value would have been produced. Later consumers either consume
that plan directly or force materialization to the public representation.

## Research Findings

Rust does not carry a uniform heap-allocated public iterator wrapper through
optimized loops. After monomorphization and inlining, Rust loops are ordinary
state machines over concrete fields such as pointer, index, end, phase, and
captured functions. Roc should reach the same kind of IR shape for builtin
iterators, while still presenting a pure public `Iter` API.

Current Roc status on this branch:

- `IterPlan` storage exists in Monotype.
- direct optimized `for` over `List.iter` has a shape test and avoids the
  public step path
- visible `Iter.append` chains on a list source can be lowered directly for
  optimized `for`
- direct optimized `for` over `Iter.single` has a shape test and avoids the
  public step path
- user-defined methods named `iter` and `single` are not recognized as builtin
  iterator producers
- recognition has moved away from display-string matching toward checked method
  identity, but this must be completed for every producer

Important constraints:

- `dbg`, `expect`, and `crash` are observable and cannot be moved or duplicated
  just because an expression is otherwise pure.
- checked function effectfulness is not a sufficient "safe to replay" fact.
- LIR and backends must not understand builtin iterator semantics.
- ARC must only follow explicit LIR `incref`, `decref`, and `free` statements.
- No stage after parsing/error reporting may guess, recover, or mine missing
  semantic information.

## Core Design

### Plan Values

Add an explicit Monotype expression form for a first-class iterator plan value.
It has the public `Iter(item)` type, but its data is an `IterPlanId` rather
than a materialized public record/closure value.

The plan expression is a post-check value only. It may appear wherever an
ordinary expression may appear during Monotype and Monotype Lifted passes:

- declaration RHSs
- `let` values
- block final expressions
- `if` and `match` branches
- function return values before specialization decides whether to materialize
- call arguments before specialization decides whether the callee consumes a
  plan

Every plan expression must be eliminated before LIR lowering by either:

- optimized consumption
- materialization to the public `Iter` value
- conversion to `Public(iter_value)` where the compiler already has an ordinary
  public iterator value and no more precise plan

LIR must reject unmaterialized plan expressions as an invariant violation.

### Producer Plans

The plan vocabulary is:

```text
ListIter(list, index, len)
Range(current, end, inclusivity, step)
UnboundedRange(current, step)
Single(item, emitted)
Append(before, after, phase)
Concat(first, second, phase)
Map(source, mapping_fn)
Filter(source, predicate_fn, keep_or_drop)
Custom(state, step_fn)
Public(iter_value)
```

Each plan records:

- item type
- known or unknown length
- whether `Done` is reachable
- which public step variants are reachable if materialized
- operands as already-lowered Monotype values or child plan ids
- materialization recipe

Producer recognition must use exact checked identity:

- builtin method owner
- method name id
- resolved method target
- checked procedure/template identity
- monomorphic receiver/result types

It must not use source text, generated symbol names, closure layout shape,
display strings, wasm output, or backend code shape.

### Materialization

Materialization converts a known plan value to the public `Iter` representation
with the same behavior as the Roc builtin implementation. It is required when:

- `.step` is accessed directly
- `Iter.next` is used as a public value rather than specialized
- a plan value is stored inside a public aggregate that survives
- a plan value is returned from an unspecialized function
- a plan value is passed to a call that is not specialized to consume a plan
- lowering reaches a `Public(iter_value)` boundary

Materialization is a semantic lowering step, not a cleanup pass. It exists so
public Roc values keep their documented behavior. Later size passes may remove
dead materialization code, but correctness must not depend on that.

### Optimized Consumers

Optimized consumers consume plan values directly:

- source `for`
- specialized `Iter.fold`
- specialized `List.from_iter`

The source `for` lowering should:

- evaluate the iterable expression once, producing a plan value
- allocate private loop state fields for that plan
- step the plan with ordinary Monotype control flow
- bind produced items to the source pattern
- continue with updated private state
- break on the plan's explicit done condition
- avoid public `Iter` records and public step values in the hot loop

For `Append` and `Concat`, the loop state must include a phase or equivalent
variant. For `Map` and `Filter`, the loop state must include child plan state
and the captured callable values. For finite plans, known-length information
should flow into `List.from_iter` capacity decisions.

### Local And Branch Propagation

Plan propagation must be ordinary value propagation, not a side table that
replays checked expressions.

This means:

- `base_points = [...].iter()` creates a plan value at the declaration site
- `saved = base_points` copies the same immutable public-value meaning
- `collision_points = if ...` creates a plan value by evaluating exactly one
  branch at the declaration site
- `for point in collision_points` consumes the already-produced plan value
- if `saved` later crosses a public boundary, it materializes without affecting
  the private cursor used by the `for`

The compiler may use local side tables as indexes into plan values during
Monotype lowering, but those tables must point at already-lowered plan values or
locals, not at checked expressions that would be evaluated again.

### Function Specialization

Function specialization may propagate plan values through calls when all of
these are true:

- the callee is specialized at a monomorphic function type
- the argument position is known to be an iterator plan value
- the callee body consumes or returns the plan in a way the specialization can
  represent explicitly

Otherwise the argument must materialize before the call. This keeps the public
calling convention ordinary and prevents unspecialized code from depending on
hidden iterator-plan state.

### Existing Pass Responsibilities

SpecConstr may remain a generic call-pattern optimization, but it must not be
the owner of builtin iterator semantics.

ScalarizeJoins may clean up residual aggregate state, but optimized iterator
loops must already carry plan fields explicitly before relying on scalarization.

TagReachability may remove residual impossible public-tag branches, but plan
lowering should avoid emitting unreachable internal branches in the first
place.

ARC works after plans are lowered to ordinary values/control flow. Refcounted
payloads inside plan state are managed through normal explicit LIR ARC
statements. Iterator wrapper uniqueness is never decided by ARC.

Binaryen remains a final wasm optimizer for wasm targets. It is not responsible
for discovering Roc iterator semantics.

## Implementation Plan

### Phase 1: Lock Current Behavior With Tests

Add or keep focused shape tests for the current optimized producers:

- direct `for` over `List.iter`
- direct `for` over `Iter.single`
- direct `for` over visible `Iter.append` chains from a list source
- user-defined `.iter` methods are not recognized as builtin `List.iter`
- user-defined `.single` methods are not recognized as builtin `Iter.single`

Add failing tests for the missing plan-value behavior:

- `iter = [1, 2].iter(); for x in iter { ... }` avoids public step values
- `base = [1, 2].iter(); iter = base.append(3); for x in iter { ... }`
  avoids public step values
- `iter = if cond { [1].iter() } else { [2].iter() }; for x in iter { ... }`
  evaluates the condition once and avoids public step values
- `saved = iter; for x in iter { ... }; use(saved)` keeps `saved` public
  behavior correct
- a branch containing `dbg`, `expect`, or `crash` is not duplicated or moved
  across the declaration site

### Phase 2: Add First-Class Plan Expressions

Extend Monotype AST with a plan expression form.

Tasks:

- add `ExprData.iter_plan: IterPlanId`
- ensure the expression type remains the public `Iter(item)` type
- teach Monotype debug/dump/test helpers to print or inspect plan expressions
- teach Monotype Lifted cloning/lifting passes to preserve plan expressions
- update any expression walkers that must recurse through plan operands by
  reading the explicit plan store
- make solved LIR lowering reject unmaterialized plan expressions with an
  invariant until materialization is implemented

Tests:

- a manually lowered plan expression survives Monotype Lifted cloning
- LIR lowering rejects an unmaterialized plan expression

### Phase 3: Lower Producers To Plan Values

Move producer recognition out of consumer-only paths. A builtin producer
expression should lower to an `iter_plan` expression whenever iterator plans are
enabled.

Initial producers:

- `List.iter`
- `Iter.single`
- `Iter.append`
- `Iter.concat`
- numeric finite ranges
- numeric unbounded ranges
- `Iter.map`
- `Iter.keep_if`
- `Iter.drop_if`, if it remains public
- `Iter.custom`

Tasks:

- recognize producers by checked identity
- lower operands exactly once in source order
- store child plans by `IterPlanId`
- store public fallback/materialization recipes
- keep `Public(iter_value)` for unknown iterator values

Tests:

- each producer emits the expected plan expression
- operands with `dbg`, `expect`, or `crash` are evaluated exactly once
- user code with the same method names does not produce builtin plans
- imported builtin methods still produce plans by identity

### Phase 4: Implement Materialization

Add a Monotype materialization function from `IterPlanId` to public `Iter`
expression.

Tasks:

- materialize `ListIter` to the public list iterator representation
- materialize `Single`
- materialize `Append`
- materialize `Concat`
- materialize `Map`
- materialize `Filter`
- materialize `Range` and `UnboundedRange`
- materialize `Custom`
- materialize nested child plans recursively
- insert materialization at all public observation boundaries
- ensure no unmaterialized plan reaches LIR

Tests:

- returning `List.iter(list)` from a function works
- storing a plan in a record and reading `.step` works
- public `Iter.next` on every materialized producer matches current behavior
- materialized rest iterators can be consumed later
- `saved = iter; for x in iter { ... }; use(saved)` behaves correctly

### Phase 5: Consume Plans Directly In `for`

Replace consumer-only source recognition with plan consumption.

Tasks:

- lower the iterable expression once to a value
- if the value is a plan expression, consume its plan directly
- if the value is a local bound to a plan expression, consume that plan through
  ordinary local/plan value tracking
- if the value is public, use the current public `Iter.next` lowering
- lower `ListIter` with list/index/len state
- lower `Single` with item/emitted state
- lower `Append` with source state, appended item, and phase
- lower `Concat` with two child states and phase
- lower `Map` and `Filter` over child states
- lower finite and unbounded ranges
- lower `Custom` by calling the custom step function

Tests:

- no public step values in optimized direct loops
- no public step values through locals
- no public step values through `if` and `match`
- public behavior is unchanged for unknown/public iterators
- loop-carried mutable variables still merge correctly

### Phase 6: Specialize `Iter.fold`

Teach the builtin `Iter.fold` consumer to consume known plans directly.

Tasks:

- lower accumulator as a loop parameter
- lower plan state fields as loop parameters
- call the folding function directly for produced items
- materialize only when the source plan is public or unknown

Tests:

- fold over every known producer avoids public step values
- fold over public/unknown iterators remains correct
- accumulator refcounts are correct under ARC

### Phase 7: Specialize `List.from_iter`

Teach `List.from_iter` to consume known plans directly.

Tasks:

- use known length when available for initial capacity
- append produced items with existing list low-levels
- avoid public iterator and step allocation in the hot loop
- preserve behavior for unknown/public iterators

Tests:

- `List.from_iter` over every known producer avoids public step values
- known-length plans allocate expected capacity
- unknown-length plans grow correctly
- refcounted item payloads are correct under ARC

### Phase 8: Cleanup And Invariants

Remove obsolete consumer-only recognition paths once plan values own the model.

Tasks:

- delete source-name/display-string recognition
- delete direct-only append-chain special cases that are replaced by plan values
- add invariant checks that LIR never receives raw plan expressions
- keep backend code free of iterator semantics
- keep ARC code free of iterator-wrapper uniqueness decisions
- update `design.md` and this plan as implementation details settle

Tests:

- `zig build run-test-zig-module-postcheck`
- `zig build run-test-zig-module-lir`
- iterator-specific LIR shape tests
- builtin iterator behavior tests
- wasm build smoke tests

### Phase 9: Rocci Bird Verification

Use Rocci Bird as an integration check, not as a special case.

Tasks:

- keep `examples/rocci-bird.roc` written with the top-level `.iter()` in
  `on_screen_collided!`
- keep a temporary no-`.iter()` comparison file only for measuring parity
- build both with the current compiler using `--opt=size`
- disassemble both wasm binaries
- verify collision-point sprite/list data is static where expected
- verify `on_screen_collided!` no longer contains public iterator wrapper or
  step-value hot-path code
- verify `.iter()` and no-`.iter()` versions have comparable wasm sizes
- run the optimized game locally and verify it behaves correctly
- record the final byte sizes in this file and in the final report

## Completion Checklist

- [x] `design.md` documents public `Iter` purity and private cursor plans.
- [x] `design.md` documents first-class post-check plan values.
- [x] `design.md` states that LIR and backends must not see raw plan values.
- [x] Current direct `List.iter` optimized `for` shape test exists.
- [x] Current direct visible append-chain optimized `for` shape test exists.
- [x] Current direct `Iter.single` optimized `for` shape test exists.
- [x] User-defined `.iter` is not recognized as builtin `List.iter`.
- [x] User-defined `.single` is not recognized as builtin `Iter.single`.
- [x] Monotype has `ExprData.iter_plan`.
- [x] Monotype Lifted preserves plan expressions.
- [x] LIR lowering rejects raw plan expressions before materialization is
  implemented.
- [ ] All recognized producers lower to plan expressions.
- [ ] Recognition uses checked identity for every producer.
- [ ] `List.iter` produces `ListIter`.
- [ ] numeric finite ranges produce `Range`.
- [ ] numeric unbounded ranges produce `UnboundedRange`.
- [ ] `Iter.single` produces `Single`.
- [ ] `Iter.append` produces `Append`.
- [ ] `Iter.concat` produces `Concat`.
- [ ] `Iter.map` produces `Map`.
- [ ] `Iter.keep_if` and `Iter.drop_if` produce filter plans.
- [ ] `Iter.custom` produces `Custom`.
- [ ] `Public(iter_value)` exists for unknown iterator values.
- [ ] Materialization is implemented for every plan.
- [ ] Public `.step` access materializes.
- [ ] Public `Iter.next` materializes when not specialized.
- [ ] Public aggregate storage materializes.
- [ ] Unspecialized function return materializes.
- [ ] Unspecialized call argument materializes.
- [ ] Raw plan expressions cannot reach LIR.
- [ ] Optimized `for` consumes plan values directly.
- [ ] Optimized `for` through locals avoids public step values.
- [ ] Optimized `for` through `if` avoids public step values.
- [ ] Optimized `for` through `match` avoids public step values.
- [ ] Optimized `for` over `Append` and `Concat` uses explicit phase state.
- [ ] Optimized `for` over `Map` and `Filter` uses child plan state.
- [ ] Optimized `for` over ranges uses direct numeric state.
- [ ] Optimized `Iter.fold` consumes plan values directly.
- [ ] Optimized `List.from_iter` consumes plan values directly.
- [ ] `saved = iter; for item in iter { ... }; use(saved)` preserves public
  behavior.
- [ ] `dbg`, `expect`, and `crash` in producer operands are not duplicated or
  moved.
- [ ] Refcounted list/string/item payload tests pass under ARC.
- [ ] Infinite iterator tests pass.
- [ ] Full builtin iterator behavior tests pass.
- [ ] Post-check and LIR module tests pass.
- [ ] Rocci Bird `.iter()` and no-`.iter()` builds are both measured with
  `--opt=size`.
- [ ] Rocci Bird `.iter()` and no-`.iter()` disassemblies show comparable
  optimized collision loop shape.
- [ ] Final Rocci Bird optimized wasm sizes are recorded here.

## Final Measurements

To be filled in when the implementation checklist is complete:

- Rocci Bird with `.iter()` in `on_screen_collided!`: pending
- Rocci Bird without `.iter()` in `on_screen_collided!`: pending
