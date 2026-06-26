# Builtin Iterator Plan Completion Plan

## Goal

Make optimized Roc iterator code compile like explicit state machines while the
public `Iter` API remains ordinary pure Roc.

The final state is:

- builtin iterator producers lower to explicit post-check iterator plan values
- those plan values are ordinary post-check values with the public `Iter(item)`
  type until an existing iterator-aware lowering path consumes or materializes
  them at the semantic point that observes the value
- source evaluation order is preserved for producers, conditions, branch
  selection, appended items, `dbg`, `expect`, and `crash`
- optimized consumers consume known plans directly without constructing public
  `Iter` records, step closures, or public step tags in the hot loop
- public observation boundaries materialize exactly the public `Iter` value the
  Roc builtin implementation promises
- no raw iterator plan reaches ordinary Lambda-to-LIR lowering, LIR, ARC, or
  any backend
- there is no standalone plan-elimination pass; plan consumption and
  materialization happen in the existing lowering paths at the semantic point
  that observes the plan
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
- Plan values that may cross a public observation boundary carry the
  already-lowered public materialization expression needed at that boundary;
  private consumer-owned plans do not need one.
- Lambda solving currently owns the conservative materialization boundary used
  when an ordinary value path observes a plan. This is temporary scaffolding;
  the long-term shape is not a separate whole-body cleanup pass, but explicit
  consumption/materialization decisions inside the existing lowering paths.
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
- `Iter.keep_if` and `Iter.drop_if` can emit `Filter` plans behind the
  producer-plan flag.
- `Iter.prepended` can emit a `Concat(Single(item), rest)` plan behind the
  producer-plan flag.
- `Iter.custom` can emit a `Custom` plan behind the producer-plan flag, with
  direct `Known(n)` length hints preserved as known plan length and `Unknown`
  length hints preserved as unknown.
- finite numeric range syntax and `Iter.exclusive_range`/`Iter.inclusive_range`
  can emit `Range` plans behind the producer-plan flag; length is currently
  recorded as unknown until checked output exposes the corresponding
  `steps_between` dispatch as explicit producer data.
- LIR lowering rejects raw plan expressions as an invariant.
- direct `List.iter`, visible append chains, and direct `Iter.single` have
  optimized `for` shape tests.
- direct `List.iter` and direct `Iter.single` source `for` loops consume
  already-lowered `ListIter` and `Single` plan values instead of replaying the
  checked producer expression.
- direct `Append(ListIter, item...)` source `for` loops consume already-lowered
  `Append` plan trees and append item expressions instead of replaying the
  checked producer expression.
- direct `Map(ListIter | Append(ListIter, item...), fn)` source `for` loops
  consume child plan state directly and skip the public `Iter.map` wrapper.
- direct `Filter(ListIter | Append(ListIter, item...), predicate)` source `for`
  loops consume child plan state directly and bind each produced item once
  before the predicate/body branch.
- direct private `ListIter` state can cross an immutable local when the local's
  only later observations are exact `for` iterable uses in the same lowering
  scope. Other uses still keep the public iterator value.
- direct private `ListIter` state can feed a private `.append(...)` local when
  that produced local is itself only observed by private iterator consumers; the
  appended item is evaluated at the append declaration site.
- direct finite numeric ranges are consumed by optimized `for` as private
  numeric cursor state, including inclusive end values at numeric maxima.
- direct `Iter.custom` is consumed by optimized `for` as private custom state
  while still calling the user's step function normally.
- user-defined methods named `iter` or `single` are not recognized as builtins.

That is only scaffolding. The full design still requires producer emission,
iterator-aware lowering decisions, semantic materialization, optimized consumers
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
The current public builtin surface has `Iter.custom` for infinite iterators but
does not currently expose source syntax or a builtin numeric producer for
`UnboundedRange`; that plan case is reserved for such a producer if one is
added.

### Producer Lowering

When iterator producer plans are enabled, builtin producer calls lower to plan
expressions. Plans that can be observed publicly also carry the already-lowered
public `Iter` expression for materialization. This producer flag stays separate
from the existing direct optimized-consumer flag until iterator-aware lowering
can consume common plans privately instead of materializing them back to the
public representation.

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

### Iterator Lowering Boundaries

Iterator plans are handled by the existing lowering paths that already own the
relevant semantic decision. A source `for` that receives a known plan consumes
it directly. Public `Iter.next`, function return, aggregate storage, or
unspecialized call argument materializes the plan at that boundary. This is not
a standalone pass whose job is to walk around after the fact and clean up
leftover plans. Direct `.step` field access is not a public boundary because
`Iter` is opaque to ordinary Roc code; only the builtin module can access that
field, and iterator producer plans are disabled while lowering the builtin
module.

For every plan value it sees, the rewrite must choose exactly one outcome:

- consume it directly in a recognized optimized consumer
- rewrite it into compiler-owned private plan state that a later consumer in
  the same body can consume without changing source evaluation order
- materialize it to the public `Iter` representation
- wrap an already-public value as `Public(iter_value)` when there is no more
  precise plan

The lowering traversal may maintain environments from locals to already-lowered
plan values or private plan-state values while traversing a body. It must not
point an environment entry back to checked source. It must preserve ordinary
evaluation order by rewriting producer sites, not by moving producer evaluation
to consumer sites.

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

Optimized consumers need private mutable cursor state. Iterator-aware lowering
may represent that state using ordinary post-check IR:

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
- finite ranges and custom iterators carry the right done reachability

### Phase 3: Iterator Lowering Boundaries

Teach the existing post-check lowering decisions to handle plan values at the
semantic boundary where each value is observed. This is not a new whole-body or
whole-program pass. The code that lowers source `for`, public field access,
returns, aggregate construction, and unspecialized calls already has to decide
what representation it is producing; those are the places that must either
consume a plan through a recognized optimized consumer or materialize it to
ordinary public `Iter` IR before continuing. General post-check passes stay
plan-opaque until a semantics-owning lowering path has produced ordinary IR.

Tasks:

- handle definitions, nested definitions, statements, and expressions in the
  existing lowering traversal without adding a plan cleanup pass
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
  iterator-aware lowering handles it, or otherwise ensure lowering sees the
  consumer before public fallback lowering has erased it
- lower `ListIter` with list/index/len state
- lower `Single` with item/emitted state
- lower `Append` and `Concat` with phase state
- lower `Map` and `Filter` over child plan state
- lower finite ranges directly
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
- [x] `design.md` documents that iterator-aware lowering is a post-check
  responsibility before ordinary LIR lowering.
- [x] `design.md` states that LIR and backends must not see raw plan values.
- [x] Current direct `List.iter` optimized `for` shape test exists.
- [x] Current direct visible append-chain optimized `for` shape test exists.
- [x] Current direct `Iter.single` optimized `for` shape test exists.
- [x] User-defined `.iter` is not recognized as builtin `List.iter`.
- [x] User-defined `.single` is not recognized as builtin `Iter.single`.
- [x] Monotype has `ExprData.iter_plan`.
- [x] Monotype Lifted preserves plan expressions.
- [x] Publicly observable iterator plans carry a public materialization
  expression.
- [x] Iterator-plan lowering boundary exists before Lambda-to-LIR lowering.
- [x] General call-pattern specialization treats raw iterator plans as opaque.
- [x] `List.iter` can produce `ListIter` behind the producer-plan flag.
- [x] LIR lowering rejects raw plan expressions before materialization is
  implemented.
- [x] All recognized producers lower to plan expressions.
- [x] Recognition uses checked identity for every producer.
- [x] `List.iter` uses exact checked identity when producing `ListIter`.
- [x] `Iter.iter` preserves or forwards known plans correctly.
- [x] numeric finite ranges produce `Range`.
- [x] `Iter.single` produces `Single`.
- [x] `Iter.prepended` produces the correct plan shape.
- [x] `Iter.append` produces `Append`.
- [x] `Iter.concat` produces `Concat`.
- [x] `Iter.map` produces `Map`.
- [x] `Iter.keep_if` and `Iter.drop_if` produce filter plans.
- [x] `Iter.custom` produces `Custom`.
- [x] `Public(iter_value)` exists for unknown iterator values.
- [ ] Iterator-aware lowering consumes common plans privately before ordinary
  lowering.
- [ ] Iterator-aware lowering preserves producer-site evaluation order.
- [ ] Iterator-aware lowering never replays checked expressions.
- [ ] Private plan state can cross locals.
  - [x] Direct `ListIter` private state can cross an immutable local when every
    later use is the exact iterable in a `for`.
  - [x] Direct `ListIter` private state can feed a local `.append(...)` producer
    whose result is consumed privately.
  - [x] Direct `Concat` of list-backed/list-append-backed plans can cross an
    immutable local and is consumed with private phase state.
  - [x] Direct `Map(ListIter | Append(ListIter, item...), fn)` plans can cross
    an immutable local and are consumed with private child state.
  - [x] Direct `Filter(ListIter | Append(ListIter, item...), predicate)` plans
    can cross an immutable local and are consumed with private child state.
- [ ] Private plan state can cross `if`.
- [ ] Private plan state can cross `match`.
  - [x] Direct `for` over a `match` whose branches are known `ListIter` /
    `Append(ListIter, item...)` plans lowers each selected branch to a private
    cursor loop while preserving scrutinee and producer operand order.
- [ ] Materialization is implemented for every plan.
  - [x] Recognized non-list producer plans (`Single`, finite `Range`,
    `Custom`, `Append`, `Concat`, `Map`, and `Filter`) materialize to public
    `Iter.next` behavior at unspecialized public boundaries.
- [x] Direct `.step` field access is not a public materialization boundary:
  external access is rejected because `Iter` is opaque, and builtin-internal
  access is lowered with iterator producer plans disabled.
- [ ] Public `Iter.next` materializes when not specialized.
  - [x] Direct `Iter.next(List.iter(...))` materializes before Lambda and
    preserves public iterator behavior.
  - [x] Public `Iter.next` through an unspecialized iterator argument preserves
    public step variants for recognized non-list producer plans.
- [ ] Public aggregate storage materializes.
  - [x] Tuple storage containing `List.iter(...)` materializes before Lambda.
- [ ] Unspecialized function return materializes.
  - [x] Function return of `List.iter(...)` materializes before Lambda and
    preserves public `Iter.next` behavior.
- [ ] Unspecialized call argument materializes.
  - [x] Function argument receiving `List.iter(...)` materializes before Lambda
    and preserves public `Iter.next` behavior.
- [x] Raw plan expressions cannot reach Lambda-to-LIR lowering.
- [x] Raw plan expressions cannot reach LIR.
- [ ] Optimized `for` consumes plan values directly.
- [ ] Optimized `for` through locals avoids public step values.
  - [x] Direct local `List.iter` avoids public step values when all uses are
    private `for` consumers.
  - [x] Direct local `List.iter` plus local `.append(...)` avoids public step
    values when the append result is consumed privately.
  - [x] Direct local `Concat` of list-backed/list-append-backed plans avoids
    public step values when consumed by a private `for`.
  - [x] Direct local `Map(ListIter | Append(ListIter, item...), fn)` avoids
    public step values when consumed by a private `for`.
  - [x] Direct local `Filter(ListIter | Append(ListIter, item...), predicate)`
    avoids public step values when consumed by a private `for`.
- [ ] Optimized `for` through `if` avoids public step values.
  - [x] Direct `for` over an `if` whose branches are known `ListIter` /
    `Append(ListIter, item...)` plans lowers to branch-local private cursor
    loops instead of public iterator steps.
- [ ] Optimized `for` through `match` avoids public step values.
  - [x] Direct `for` over a `match` whose branches are known `ListIter` /
    `Append(ListIter, item...)` plans avoids public iterator step tags.
- [x] Optimized `for` over direct `ListIter` consumes the plan value.
- [x] Optimized `for` over direct `Single` consumes the plan value.
- [x] Optimized `for` over direct `Append(ListIter, item...)` consumes plan
  values.
- [ ] Optimized `for` over `Append` and `Concat` uses explicit phase state.
  - [x] Direct `Concat` of list-backed/list-append-backed plans uses one
    private phase cursor and preserves source `break` as a whole-loop break.
  - [x] Local `Concat` of list-backed/list-append-backed plans uses one private
    phase cursor and preserves producer-site operand order.
- [ ] Optimized `for` over `Map` and `Filter` uses child plan state.
- [x] Optimized `for` over direct `Map(ListIter | Append(ListIter, item...), fn)`
  uses child plan state.
- [x] Optimized `for` over local `Map(ListIter | Append(ListIter, item...), fn)`
  uses child plan state.
- [x] Optimized `for` over direct
  `Filter(ListIter | Append(ListIter, item...), predicate)` uses child plan
  state.
- [x] Optimized `for` over local
  `Filter(ListIter | Append(ListIter, item...), predicate)` uses child plan
  state.
- [x] Optimized `for` over ranges uses direct numeric state.
- [x] Optimized `for` over direct `Iter.custom` uses private custom state.
- [ ] Optimized `Iter.fold` consumes plan values directly.
  - [x] Direct `ListIter` and `Append(ListIter, item...)` plans are consumed by
    direct-call `Iter.fold` as accumulator loop parameters without public
    iterator step values.
  - [x] Direct `Range` plans are consumed by direct-call `Iter.fold` as numeric
    cursor and accumulator loop parameters without public iterator step values.
  - [x] Direct `Single` plans are consumed by direct-call `Iter.fold` without
    public iterator step values.
  - [x] Direct `Concat` of list-backed/list-append-backed plans is consumed by
    direct-call `Iter.fold` with a private phase cursor and without public
    iterator step values.
  - [x] Direct
    `Map(ListIter | Append(ListIter, item...) | Range | Single | Concat, fn)`
    plans with direct mapping functions are consumed by direct-call `Iter.fold`
    without public iterator step values.
- [ ] Optimized `List.from_iter` consumes plan values directly.
  - [x] Direct `ListIter` and `Append(ListIter, item...)` plans are consumed by
    `List.from_iter` and list-result `Iter.collect` as exact-capacity list
    loops using list low-level operations.
  - [x] Direct `Single` plans are consumed by `List.from_iter` and list-result
    `Iter.collect` without public iterator step values.
  - [x] Direct `Concat` of list-backed/list-append-backed plans is consumed by
    `List.from_iter` and list-result `Iter.collect` with one exact-capacity
    output list and a private phase cursor.
  - [x] Direct
    `Map(ListIter | Append(ListIter, item...) | Range | Single | Concat, fn)`
    plans with direct mapping functions are consumed by `List.from_iter` and
    list-result `Iter.collect` without public collect-worker specialization.
- [ ] `saved = iter; for item in iter { ... }; use(saved)` preserves public
  behavior.
  - [x] Local `List.iter` with a public alias preserves public iterator behavior.
  - [x] Local `Append(ListIter, item...)` with a public alias preserves public
    iterator behavior.
- [ ] `dbg`, `expect`, and `crash` in producer operands are not duplicated or
  moved.
  - [x] Direct append operands consumed by `List.from_iter` preserve `dbg`
    ordering relative to collection and following expressions.
  - [x] Direct append operands and accumulator operands consumed by `Iter.fold`
    preserve `dbg` ordering relative to the fold result and following
    expressions.
  - [x] Direct range operands and accumulator operands consumed by `Iter.fold`
    preserve `dbg` ordering relative to the fold result and following
    expressions.
  - [x] Direct single operands and accumulator operands consumed by `Iter.fold`
    preserve `dbg` ordering relative to the fold result and following
    expressions.
  - [x] Direct single operands consumed by `List.from_iter` preserve `dbg`
    ordering relative to collection and following expressions.
  - [x] Direct mapped append operands consumed by `List.from_iter` preserve
    `dbg` ordering relative to collection and following expressions.
  - [x] Direct concat operands consumed by `List.from_iter` preserve `dbg`
    ordering relative to collection and following expressions.
  - [x] Direct mapped append operands and accumulator operands consumed by
    `Iter.fold` preserve `dbg` ordering relative to the fold result and
    following expressions.
  - [x] Direct concat operands and accumulator operands consumed by `Iter.fold`
    preserve `dbg` ordering relative to the fold result and following
    expressions.
  - [x] Local map producer operands consumed by optimized `for` preserve `dbg`
    ordering relative to the producer site, loop body, and following
    expressions.
  - [x] Local filter producer operands consumed by optimized `for` preserve
    `dbg` ordering relative to the producer site, loop body, and following
    expressions.
- [ ] Refcounted list/string/item payload tests pass under ARC.
  - [x] Direct `List.from_iter(List(Str).iter().append(...))` passes optimized
    LIR interpretation with the expected string list.
  - [x] Direct `Iter.fold(List(Str).iter().append(...))` passes optimized LIR
    interpretation with the expected string accumulator result.
  - [x] Direct `Iter.fold(Iter.single(Str))` passes optimized LIR
    interpretation with the expected string accumulator result.
  - [x] Direct `List.from_iter(Iter.single(Str))` passes optimized LIR
    interpretation with the expected string list.
  - [x] Direct `List.from_iter(List(Str).iter().append(...).map(...))` passes
    optimized LIR interpretation with the expected string list.
  - [x] Direct `List.from_iter(List(Str).iter().append(...).concat(...))`
    passes optimized LIR interpretation with the expected string list.
- [ ] Infinite iterator tests pass.
  - [x] An infinite `Iter.custom` source can be consumed by optimized `for`
    and exited by a source `break` without requiring a reachable `Done`.
- [ ] Full builtin iterator behavior tests pass.
  - [x] `zig build run-test-zig-builtin-doc` passes with the iterator changes.
- [x] Post-check and LIR module tests pass.
  - [x] `zig build run-test-zig-module-postcheck`,
    `zig build run-test-zig-module-lir`, and
    `zig build run-test-zig-lir-inline` pass after the latest iterator-plan
    lowering and behavior tests.
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
