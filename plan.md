# Builtin Iterator Plan Completion Plan

## Goal

Make optimized Roc iterator code compile like explicit state machines while the
public `Iter` API remains ordinary pure Roc.

The final state is:

- builtin iterator producers lower to explicit post-check iterator plan values
- those plan values are ordinary post-check values with the public `Iter(item)`
  type until the iterator-aware post-check normalization consumes or
  materializes them
- source evaluation order is preserved for producers, conditions, branch
  selection, appended items, `dbg`, `expect`, and `crash`
- optimized consumers consume known plans directly without constructing public
  `Iter` records, step closures, or public step tags in the hot loop
- public observation boundaries materialize exactly the public `Iter` value the
  Roc builtin implementation promises
- no raw iterator plan reaches ordinary Lambda-to-LIR lowering, LIR, ARC, or
  any backend
- Rocci Bird with and without a top-level `.iter()` in `on_screen_collided!`
  has the same optimized collision-loop shape and comparable `--opt=size` wasm
  size

## Motivation

Rocci Bird exposed a size and code-shape problem in optimized wasm output. A
collision loop written over a static list was much smaller than the same loop
written over `list.iter()` plus `Iter.append`. The public iterator path builds
`Iter` records, zero-argument step closures, and public step values such as
`One`, `Append`, `Skip`, and `Done`; the optimized loop immediately destructures
those values again.

That is the wrong optimized representation. The source program is pure and
public `Iter` values are reusable, but a consuming loop should see a private
cursor state machine. For a list, the loop needs a list reference, an index, and
a length. For `Iter.append`, it needs the source plan, the appended item, and a
phase. For `Iter.map` and filters, it needs child plan state plus the captured
function.

Earlier work on this branch proves direct syntactic cases can be optimized:

- `for x in list`
- `for x in list.iter()`
- `for x in list.iter().append(a).append(b)`
- `for x in Iter.single(item)`

That is not enough. Rocci Bird's real shape flows through locals and `if`
branches:

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

Re-recognizing the checked RHS of `collision_points` at the `for` use site would
be wrong: it can move or duplicate branch conditions, appended item expressions,
`dbg`, `expect`, and `crash`. Mining the public `Iter` record is also wrong:
that makes compiler optimization depend on the private Roc implementation of
the public builtin. The right source of truth is a first-class iterator-plan
value in post-check IR.

## Research Summary

Rust iterator loops do not preserve a uniform public iterator object in
optimized code. After monomorphization and inlining, Rust generally lowers
iterator pipelines to concrete state machines over fields such as pointer,
index, end, phase, and captured function values. There is no heap-allocated
universal iterator wrapper on the hot path.

Roc should reach the same optimized shape for builtin iterators, with one
additional constraint: Roc's public `Iter` API is pure. Public iterator values
must be immutable and reusable. Any mutation used by optimized iteration must be
mutation of compiler-owned private state, not mutation of the source `Iter`
value.

Current branch status:

- Monotype has an `ExprData.iter_plan` form.
- Monotype has an `iter_plans` side store.
- Monotype Lifted preserves plan expressions and plan stores.
- Plan values carry the already-lowered public materialization expression needed
  when they cross a public observation boundary.
- A focused post-check normalization boundary currently materializes remaining
  raw plans before Lambda-to-LIR lowering.
- `List.iter` can emit a `ListIter` plan behind an explicit producer-plan flag;
  recognition checks the resolved builtin method target, and the normal
  pipeline keeps that flag off until private consumers through locals and
  branches are implemented.
- `Iter.single` can emit a `Single` plan behind the producer-plan flag; cursor
  state that belongs to first-class plans is represented as initial expressions
  rather than preexisting loop locals.
- `Iter.append` can emit an `Append` plan behind the producer-plan flag, reusing
  known child plans and wrapping unknown iterator operands as `Public`.
- `Iter.iter` forwards known iterator plans and wraps unknown iterator operands
  as `Public`.
- `Iter.concat` can emit a `Concat` plan behind the producer-plan flag.
- `Iter.map` can emit a `Map` plan behind the producer-plan flag.
- LIR lowering rejects raw plan expressions as an invariant.
- direct `List.iter`, visible append chains, and direct `Iter.single` have
  optimized `for` shape tests.
- user-defined methods named `iter` or `single` are not recognized as builtins.

That is only scaffolding. The full design still requires producer emission,
iterator-aware normalization, semantic materialization, optimized consumers
through locals and branches, and integration measurements.

## Non-Negotiable Invariants

- Checked method identity is the only way to recognize builtin iterator
  producers and consumers.
- Source names, generated symbol text, public record shape, wasm output, closure
  layout, or backend code shape must not be used for recognition.
- Plan propagation must never replay checked expressions or declaration RHSs.
- A temporary environment may map locals to already-lowered plan values while
  rewriting IR, but it must not map locals back to checked source.
- `dbg`, `expect`, and `crash` are observable. They must not be moved or
  duplicated by iterator optimization.
- Public `Iter` values remain pure and reusable.
- Private cursor mutation is allowed only for compiler-created state whose
  mutation cannot be observed by Roc source.
- Plan wrappers themselves must not require heap allocation or refcounting.
- Refcounted payloads inside plan state are ordinary Roc values and are managed
  only by explicit LIR ARC statements.
- LIR and backends must not know builtin iterator semantics.
- No raw iterator plan may reach ordinary Lambda-to-LIR lowering.

## Core Design

### Plan Values

Add and use `ExprData.iter_plan` as a first-class Monotype expression whose type
is the public `Iter(item)` type. The expression stores an `IterPlanId`; the plan
store contains explicit operands, child plan ids, known length information, step
reachability, and the item type.

An iterator plan may appear anywhere an ordinary expression can appear during
post-check optimization:

- declaration RHSs
- `let` values
- block final expressions
- `if` and `match` branch bodies
- function call arguments before specialization decides whether to materialize
- specialized function return values before callers decide how to consume them

This is a real IR value, not a source replay recipe.

### Plan Vocabulary

The initial vocabulary is:

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

Finite and infinite iterators use the same model. A finite iterator has a
reachable `Done`. An unbounded range or custom infinite iterator can have
`Done` marked unreachable unless a later adapter introduces a finite boundary.

### Producer Lowering

When iterator producer plans are enabled, builtin producer calls lower to plan
expressions that also carry the already-lowered public `Iter` expression for
semantic materialization. This producer flag stays separate from the existing
direct optimized-consumer flag until the iterator-aware normalization can
consume common plans privately instead of materializing them back to the public
representation.

Initial recognized producers:

- `List.iter`
- `Iter.iter`
- `Iter.single`
- `Iter.prepended`
- `Iter.append`
- `Iter.concat`
- `Iter.map`
- `Iter.keep_if`
- `Iter.drop_if`
- `Iter.custom`
- `Iter.exclusive_range`
- `Iter.inclusive_range`
- source range syntax that lowers through the builtin range producers

Recognition must be exact checked identity: resolved owner, method name id,
resolved procedure/template, dispatch plan, and monomorphic receiver/result
types. A user method with the same spelling is never a builtin producer.

### Iterator Normalization Boundary

Before ordinary Lambda-to-LIR lowering, the iterator-aware post-check rewrite
must remove every remaining `iter_plan` expression. This is the same owner that
knows iterator plan semantics; it is not a second broad cleanup optimizer.

For every plan value it sees, the rewrite must choose exactly one outcome:

- consume it directly in a recognized optimized consumer
- rewrite it into compiler-owned private plan state that a later consumer in
  the same body can consume without changing source evaluation order
- materialize it to the public `Iter` representation
- wrap an already-public value as `Public(iter_value)` when there is no more
  precise plan

The rewrite may maintain environments from locals to already-lowered plan values
or private plan-state values while traversing a body. It must not point an
environment entry back to checked source. It must preserve ordinary evaluation
order by rewriting producer sites, not by moving producer evaluation to consumer
sites.

Example:

```roc
iter =
    if cond {
        [1].iter().append(dbg 2)
    } else {
        Iter.single(crash "bad")
    }

side_effect_free_value = 1

for x in iter {
    ...
}
```

The condition and selected branch belong at the `iter = ...` site. The rewrite may
turn the `if` into private plan-state construction, but it must not delay or
duplicate the condition, the `dbg`, or the `crash` by replaying branch source at
the `for`.

### Private Plan State

Optimized consumers need private mutable cursor state. Iterator normalization may
represent that state using ordinary post-check IR:

- locals
- tuples
- tag unions for phase or variant selection
- loop parameters
- `if` and `match`
- direct calls
- low-level operations

This state is not the public `Iter` representation. It is compiler-owned. For
example, an `if` whose branches produce different known plan shapes may lower
to a private phase tag plus payload fields for the selected branch. A later
optimized `for` can consume that phase and payload without constructing public
step tags.

### Materialization

Materialization converts a known plan to the public `Iter` representation. It is
a semantic operation, not a cleanup pass.

Materialization is required when:

- `.step` is accessed directly
- `Iter.next` is used outside a specialized consumer
- a value is stored in an aggregate that is observed publicly
- a value is returned from unspecialized code
- a value is passed to a call not specialized to consume a plan
- the optimizer cannot prove all uses are private optimized consumers

Materialization should reuse the checked builtin method targets where that is
the most direct representation, rather than duplicating public record layout
knowledge. For example, materializing `Single(item)` can call the resolved
`Iter.single` target; materializing `Append(before, after)` can materialize
`before` and call the resolved `Iter.append` target. Generated public record
construction is allowed only when the compiler already owns that generated
representation and has explicit checked data for it.

### Optimized Consumers

The first optimized consumers are:

- source `for`
- `Iter.fold`
- `List.from_iter`

They consume known plans directly.

For source `for`, optimized lowering should:

- evaluate the iterable expression once
- consume the resulting known plan or private plan state
- allocate private loop state fields
- step child plans directly
- bind produced items to the source pattern
- update loop-carried mutable variables normally
- avoid public `Iter` records, step closures, and public step tags in the hot
  loop

For `List.from_iter`, known length information should choose the initial
capacity. For `Iter.fold`, the accumulator is a loop parameter.

## Implementation Plan

### Phase 1: Current Direct Cases And Guard Rails

Keep the existing tests:

- direct `for` over `List.iter`
- direct `for` over visible `Iter.append` chains
- direct `for` over `Iter.single`
- user-defined `.iter` is not recognized as builtin `List.iter`
- user-defined `.single` is not recognized as builtin `Iter.single`

Keep the invariant tests:

- Monotype has `ExprData.iter_plan`
- Monotype Lifted preserves plan expressions
- LIR lowering rejects unmaterialized plan expressions

### Phase 2: Producer Plan Emission

Implement producer lowering in Monotype:

- add exact checked identity helpers for all builtin producers
- lower producer operands exactly once in source order
- build `IterPlan` operands from lowered `ExprId`s and child `IterPlanId`s
- return `ExprData.iter_plan` with the public result type
- preserve `Public(iter_value)` for unknown or already-public iterators
- keep direct user methods with matching names on the ordinary public path

Tests:

- each recognized producer emits the expected plan expression
- direct calls and dispatch calls both use exact checked identity
- user methods with the same names do not emit plans
- operands containing `dbg`, `expect`, and `crash` are not duplicated in the
  Monotype tree
- finite and unbounded ranges carry the right done reachability

### Phase 3: Iterator Normalization Boundary

Extend the existing post-check lowering boundary that feeds Lambda-to-LIR so it
removes all raw plan expressions as part of the body traversal it already has to
perform. This is not a separate whole-program cleanup pass. The boundary owns
iterator semantics: when ordinary lowering reaches a plan value, it must either
consume that plan through a recognized optimized consumer or materialize it to
ordinary public `Iter` IR before continuing. General post-check passes stay
plan-opaque until this boundary has produced ordinary IR.

Tasks:

- handle definitions, nested definitions, statements, and expressions in the
  existing lowering traversal
- treat general call-pattern specialization and unrelated post-check passes as
  plan-opaque until plans have been rewritten into ordinary IR
- maintain a body-local environment from locals to plan/private-state values
- track whether a local has public observations
- rewrite known producer sites into private plan-state construction when all
  uses are optimized private consumers
- materialize producer sites when public observations exist
- preserve source evaluation order for block statements, `if`, `match`, and
  calls
- reject any raw plan that cannot be consumed or materialized

Tests:

- `iter = [1, 2].iter(); for x in iter { ... }` avoids public step values
- `base = [1, 2].iter(); iter = base.append(3); for x in iter { ... }` avoids
  public step values
- `iter = if cond { [1].iter() } else { [2].iter() }; for x in iter { ... }`
  evaluates `cond` once and avoids public step values
- `saved = iter; for x in iter { ... }; use(saved)` preserves public behavior
  for `saved`
- branch-local `dbg`, `expect`, and `crash` are not moved or duplicated

### Phase 4: Materialization For Every Plan

Implement semantic materialization:

- `ListIter`
- `Range`
- `UnboundedRange`
- `Single`
- `Append`
- `Concat`
- `Map`
- `Filter`
- `Custom`
- nested child plans
- `Public(iter_value)`

Tests:

- returning each producer from a function works
- storing each producer in a record works
- direct `.step` access works
- public `Iter.next` matches current behavior for each producer
- materialized rest iterators can be consumed later
- `saved = iter; for x in iter { ... }; use(saved)` behaves correctly

### Phase 5: Optimized `for`

Replace consumer-only source peeking with plan/private-state consumption.

Tasks:

- stop replaying checked expressions in `lowerIteratorFor`
- introduce an internal representation for source `for` that can survive until
  iterator normalization, or otherwise ensure normalization sees the consumer
  before public fallback lowering has erased it
- lower `ListIter` with list/index/len state
- lower `Single` with item/emitted state
- lower `Append` and `Concat` with phase state
- lower `Map` and `Filter` over child plan state
- lower finite and unbounded ranges directly
- lower `Custom` by calling the custom step function
- preserve loop-carried mutable variable behavior

Tests:

- no public step values in optimized direct loops
- no public step values through locals
- no public step values through `if`
- no public step values through `match`
- unknown/public iterators continue through the public path
- loop-carried mutable variables still merge correctly

### Phase 6: Optimized `Iter.fold`

Specialize `Iter.fold` for known plans.

Tasks:

- lower accumulator as a loop parameter
- lower plan state fields as loop parameters
- call the folding function directly for produced items
- materialize only when the source is public or unknown

Tests:

- fold over every known producer avoids public step values
- fold over public/unknown iterators remains correct
- accumulator refcounts are correct under ARC

### Phase 7: Optimized `List.from_iter`

Specialize `List.from_iter` for known plans.

Tasks:

- use known length for initial capacity
- append produced items with existing list low-levels
- avoid public iterator and step allocation in the hot loop
- preserve unknown/public iterator behavior

Tests:

- `List.from_iter` over every known producer avoids public step values
- known-length plans allocate expected capacity
- unknown-length plans grow correctly
- refcounted item payloads are correct under ARC

### Phase 8: Cleanup

Remove obsolete temporary paths:

- delete source-peeking append-chain recognition once plan values cover it
- delete display-string/name-shape recognition
- keep backend and ARC code free of iterator semantics
- keep LIR raw-plan rejection as a permanent invariant
- update `design.md` when implementation details settle

### Phase 9: Rocci Bird Verification

Use Rocci Bird as integration evidence, not as a special case.

Tasks:

- keep `examples/rocci-bird.roc` in the intended source style
- build a temporary no-`.iter()` comparison variant only for measurement
- build both with the current compiler using `--opt=size`
- disassemble both wasm binaries
- verify sprite/list data that should be static is static
- verify `on_screen_collided!` no longer contains public iterator-wrapper or
  step-value hot-path code
- verify `.iter()` and no-`.iter()` versions have comparable wasm sizes
- run optimized and dev builds locally and verify the game behaves correctly
- record final byte sizes in this file and in the final report

## Completion Checklist

- [x] `design.md` documents public `Iter` purity and private cursor plans.
- [x] `design.md` documents first-class post-check plan values.
- [x] `design.md` documents that iterator normalization is a post-check
  responsibility before ordinary LIR lowering.
- [x] `design.md` states that LIR and backends must not see raw plan values.
- [x] Current direct `List.iter` optimized `for` shape test exists.
- [x] Current direct visible append-chain optimized `for` shape test exists.
- [x] Current direct `Iter.single` optimized `for` shape test exists.
- [x] User-defined `.iter` is not recognized as builtin `List.iter`.
- [x] User-defined `.single` is not recognized as builtin `Iter.single`.
- [x] Monotype has `ExprData.iter_plan`.
- [x] Monotype Lifted preserves plan expressions.
- [x] Iterator plans carry a public materialization expression.
- [x] Iterator-plan normalization boundary exists before Lambda-to-LIR lowering.
- [x] General call-pattern specialization treats raw iterator plans as opaque.
- [x] `List.iter` can produce `ListIter` behind the producer-plan flag.
- [x] LIR lowering rejects raw plan expressions before materialization is
  implemented.
- [ ] All recognized producers lower to plan expressions.
- [ ] Recognition uses checked identity for every producer.
- [x] `List.iter` uses exact checked identity when producing `ListIter`.
- [x] `Iter.iter` preserves or forwards known plans correctly.
- [ ] numeric finite ranges produce `Range`.
- [ ] numeric unbounded ranges produce `UnboundedRange`.
- [x] `Iter.single` produces `Single`.
- [ ] `Iter.prepended` produces the correct plan shape.
- [x] `Iter.append` produces `Append`.
- [x] `Iter.concat` produces `Concat`.
- [x] `Iter.map` produces `Map`.
- [ ] `Iter.keep_if` and `Iter.drop_if` produce filter plans.
- [ ] `Iter.custom` produces `Custom`.
- [x] `Public(iter_value)` exists for unknown iterator values.
- [ ] Iterator normalization consumes common plans privately before ordinary
  lowering.
- [ ] Iterator normalization preserves producer-site evaluation order.
- [ ] Iterator normalization never replays checked expressions.
- [ ] Private plan state can cross locals.
- [ ] Private plan state can cross `if`.
- [ ] Private plan state can cross `match`.
- [ ] Materialization is implemented for every plan.
- [ ] Public `.step` access materializes.
- [ ] Public `Iter.next` materializes when not specialized.
- [ ] Public aggregate storage materializes.
- [ ] Unspecialized function return materializes.
- [ ] Unspecialized call argument materializes.
- [ ] Raw plan expressions cannot reach Lambda-to-LIR lowering.
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

## Required Verification Commands

Run these before marking the checklist complete:

```sh
zig build run-test-zig-module-postcheck
zig build run-test-zig-module-lir
zig build run-test-zig-lir-inline
zig build minici
```

For Rocci Bird, run:

```sh
roc build --opt=size examples/rocci-bird.roc
wasm-objdump -d <optimized wasm>
```

Also build and disassemble the temporary no-`.iter()` comparison variant.

## Final Measurements

To be filled in only when the implementation checklist is complete:

- Rocci Bird with `.iter()` in `on_screen_collided!`: pending
- Rocci Bird without `.iter()` in `on_screen_collided!`: pending
