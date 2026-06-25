# Builtin Iterator Internal Plans

## Goal

Convert optimized Roc iterator lowering from the current public `Iter` record
hot path into compiler-owned iterator plans that optimize to the same basic
shape as Rust iterators: concrete state carried in loop locals, direct calls to
known adapter functions, no iterator-wrapper heap allocation, and no repeated
construction/destruction of public step values in optimized consumers.

The public Roc API does not change:

- `Iter(item)` remains the public builtin iterator type.
- `Iter` methods remain pure functions.
- Public iterator values remain immutable and reusable.
- `Iter.next`, `.step`, `Iter.map`, `Iter.keep_if`, `Iter.append`,
  `Iter.concat`, `List.iter`, ranges, and custom iterators keep their current
  source-level meanings.

The optimized internal representation is allowed to be different. The compiler
may lower recognized builtin iterator pipelines to private mutable cursor state
when that mutation is not observable by Roc source.

## Motivation

Rocci Bird exposed that the current pure-Roc iterator implementation is much
bulkier than Rust's optimized iterator code. After static-data hoisting and the
`Append` branch fix, the `.iter()` and no-`.iter()` Rocci Bird builds are close
in size, which proves the original static-data rebuild problem is no longer the
dominant issue. The remaining gap is that Roc still often represents iteration
as public `Iter` records, step closures, public step tag unions, and rest
iterator records that are constructed only to be immediately destructured by a
consumer.

Rust avoids that shape. In `/home/rtfeldman/code/rust`, the core iterator trait
is `next(&mut self) -> Option<Self::Item)` in
`library/core/src/iter/traits/iterator.rs`. Adapters such as `Map`, `Filter`,
`Chain`, `Once`, slice iterators, and `Vec::IntoIter` are concrete structs:

- `library/core/src/iter/adapters/map.rs`
- `library/core/src/iter/adapters/filter.rs`
- `library/core/src/iter/adapters/chain.rs`
- `library/core/src/iter/sources/once.rs`
- `library/core/src/slice/iter.rs`
- `library/alloc/src/vec/into_iter.rs`

LLVM sees concrete state fields and private mutation. There is no universal
runtime iterator object in the hot loop, and there is no public "rest iterator"
record that has to be rebuilt on each step.

Roc can optimize to that shape without changing the public model. The key is
that mutable state must be compiler-created private cursor state, not mutation
of a public Roc value.

## Correctness Model

This source must keep working exactly as written:

```roc
iter = [1, 2].iter()
saved = iter

for item in iter {
    {}
}

use(saved)
```

The loop may advance a private cursor derived from `iter`, but it must not
mutate `saved`. If a public iterator value can be observed again, its meaning
is unchanged.

This source must also keep working:

```roc
iter = [1, 2].iter()

first = Iter.next(iter)
second = Iter.next(iter)
```

Both `first` and `second` observe the same original iterator. Any lowering that
turns public `Iter.next(iter)` into destructive mutation of shared iterator
storage is incorrect.

The allowed optimized lowering is:

```text
public_or_known_iter = ...
private_cursor = derive cursor state from public_or_known_iter

while private_cursor can produce an item:
    item = private_cursor.current_item
    private_cursor = private_cursor.advanced_state
    body(item)
```

For `List.iter`, `private_cursor` is just fields such as list pointer, index,
and length. The cursor is not heap-allocated and is not refcounted. The list
payload inside the cursor remains an ordinary Roc value and is managed by ARC.

## Current Status Quo

The current public builtin is in `src/build/roc/Builtin.roc`:

```roc
Iter(item) :: {
    len_if_known : [Known(U64), Unknown],
    step : () -> [
        Append({ before : Iter(item), after : item }),
        One({ item : item, rest : Iter(item) }),
        Skip({ rest : Iter(item) }),
        Done,
    ],
}
```

Important current implementation points:

- `List.iter` constructs public iterator records and zero-argument step
  closures.
- `Iter.map`, `Iter.keep_if`, `Iter.append`, and `Iter.concat` are ordinary Roc
  functions that wrap source iterators in new public iterator records.
- `for` lowering in `src/postcheck/monotype/lower.zig` lowers through checked
  iterator dispatch plans, calls `.iter` and `.next`, matches the public step
  union, and carries a public iterator value as loop state.
- Optimized builds run call-pattern specialization in
  `src/postcheck/monotype_lifted/spec_constr.zig`.
- LIR passes already include scalarization, box reuse, return slots, string
  append rewriting, tag reachability, reachable-proc cleanup, and ARC insertion
  in `src/lir/checked_pipeline.zig`.

The current infrastructure already has useful pieces:

- Checked iterator `for` has explicit dispatch plans, so post-check lowering
  does not need to rediscover `.iter`/`.next` from source syntax.
- Monotype lowering has exact method-target lookup for iterator dispatch.
- SpecConstr can specialize known records, tags, tuples, and callable values
  at direct call sites.
- ScalarizeJoins can split struct-like loop state once it is visible in LIR.
- TagReachability can remove impossible tag branches after earlier passes make
  value origins explicit.
- ARC borrow inference can optimize refcounted payloads inside iterator state.

These pieces are not enough by themselves. The current hot path still exposes
the public `Iter` representation too long, so later passes have to recover from
record/closure/step-union construction instead of lowering the known iterator
plan directly.

## Target Design

Add an explicit post-check iterator-plan representation for builtin `Iter`.

The initial plan vocabulary should include:

```text
ListIter(list, index, len)
Range(current, end, inclusivity, step)
UnboundedRange(current, step)
Single(item, emitted)
Append(before, after, phase)
Concat(first, second, phase)
Map(source, mapping_fn)
Filter(source, predicate_fn)
Custom(state, step_fn)
Public(iter_value)
```

`Public(iter_value)` is the materialization boundary. It means the compiler is
using the ordinary public `Iter` representation because the value is being
observed as a public Roc value, or because no explicit plan exists for the
producer.

Known plans are lowered directly in optimized consumers:

- source `for`
- `Iter.fold`
- `List.from_iter`
- later, any builtin consumer that repeatedly calls `Iter.next`

Consumers carry plan state directly as loop parameters. A `ListIter` loop
should carry the source list, current index, and length. A `Map` loop should
carry the source state plus the mapping function. A `Filter` loop should carry
the source state plus the predicate and loop internally until it finds a
matching item or the source is done. `Append` and `Concat` should be explicit
phase machines, not public iterator-record rebuilds.

Finite and infinite iterators use the same abstraction. A finite plan has a
reachable done state. An infinite plan, such as an unbounded range or a custom
Fibonacci-like state machine, has no reachable done transition unless an
adapter introduces one. Collecting an infinite iterator into a list remains
nonterminating or resource-exhausting according to source semantics.

## Non-Goals

- Do not change the public Roc `Iter` API.
- Do not require user code to annotate iterator memory behavior.
- Do not use reference counts to decide uniqueness of `Iter` wrappers.
- Do not teach ARC about iterator-wrapper ownership.
- Do not have the backend inspect generated wasm, LLVM, symbols, closure
  layouts, or object bytes to recognize iterator patterns.
- Do not introduce source-name or display-string recognition.
- Do not rely on a size cleanup pass to repair iterator lowering after the
  fact.

## Required Invariants

- Builtin iterator operations are recognized only by exact checked identity:
  method owner, method name id, function template, static-dispatch plan, and
  Monotype instantiation.
- Public Roc `Iter` values are immutable and reusable.
- Private iterator cursor mutation is allowed only for compiler-created state
  that cannot be observed as the original public value.
- The `Iter` wrapper itself does not require heap allocation or refcounting.
- Refcounted payloads inside iterator state remain ordinary Roc values and are
  managed only by LIR ARC insertion.
- If a known plan crosses a public observation boundary, the compiler
  materializes the ordinary public `Iter` value with the same meaning as the
  builtin Roc implementation.
- If a public iterator value is passed into code without an explicit plan, that
  code sees `Public(iter_value)`.
- Infinite iterators are valid plans.
- Unreachable step variants are removed from optimized consumers by explicit
  plan facts and ordinary reachability/tag-reachability data, not by matching
  generated code shape.

## Phase 1: Lock Down Semantics With Tests

Add tests before implementation changes. These tests should keep passing after
optimization, and some should inspect optimized IR to prevent regressions.

### Public Purity Tests

Add evaluator or compile/run tests proving public iterator values are reusable:

```roc
main =
    iter = [1, 2].iter()
    a = Iter.next(iter)
    b = Iter.next(iter)
    (a, b)
```

Expected:

- `a` and `b` are equal.
- The second `Iter.next` does not observe the advanced state from the first.

Add an alias-after-loop test:

```roc
main =
    iter = [1, 2].iter()
    saved = iter

    sum =
        var acc = 0
        for item in iter {
            acc = acc + item
        }
        acc

    (sum, Iter.next(saved))
```

Expected:

- `sum` is `3`.
- `Iter.next(saved)` still returns the first item.

Add rest-escaping tests:

```roc
main =
    iter = [1, 2, 3].iter()

    rest =
        when Iter.next(iter) is
            One({ rest }) -> rest
            _ -> iter

    Iter.next(rest)
```

Expected:

- The escaped rest iterator has the correct public meaning.

### Infinite Iterator Tests

Add tests for custom or builtin unbounded iteration once the relevant API
exists:

- unbounded range consumed by `take` or an equivalent finite consumer
- Fibonacci-like custom iterator consumed by a finite consumer
- direct `Iter.next` on the public value remains reusable
- optimized `for` or `fold` over a finite prefix does not allocate iterator
  wrappers

If the standard library does not yet expose a finite `take`, use a custom
consumer that breaks after `n` items.

### Refcounted Payload Tests

Add tests where iterator items and cursor state contain refcounted values:

- list of strings iterated twice through aliased public iterators
- `Map` returning strings
- `Filter` over records containing lists
- `Append` with a refcounted final item
- `Concat` over two list iterators with shared list payloads

Expected:

- No leaks.
- No double frees.
- Reusing the public iterator after a private optimized consumer is correct.

### Optimized IR Shape Tests

Add tests that compile optimized iterator consumers and inspect post-check or
LIR state. The exact test layer should be the earliest layer that can observe
the intended invariant without parsing backend output.

For `List.iter` in a `for` loop:

- no public `iter_from_step` call in the loop body
- no zero-argument step closure call in the loop body
- loop state carries list/index/len or equivalent fields
- no public `One`/`Skip`/`Append`/`Done` value is built and immediately matched

For `Range`:

- loop state carries current/end/step fields
- no public iterator record is constructed in the hot loop

For `Map`:

- loop state carries source state plus mapping callable
- produced items call the mapping function directly
- source rest iterator is not materialized unless it escapes

For `Filter`:

- loop body can step source repeatedly until predicate success or done
- `Skip` is internal control flow, not a public step value in the hot loop

For `Append` and `Concat`:

- generated loop state has an explicit phase or equivalent variant
- branch code does not rebuild public `Iter.append`/`Iter.concat` records each
  iteration

## Phase 2: Define IteratorPlan Data

Add explicit data structures in the post-check pipeline. The exact file names
can change during implementation, but the ownership should be clear:

- Monotype or Monotype Lifted owns type-specialized iterator plans.
- LIR owns only lowered statements, layouts, and explicit ARC statements.
- Backends consume LIR and do not know iterator semantics.

Suggested new module:

```text
src/postcheck/iter_plan.zig
```

Suggested core data:

```zig
const IterPlanId = enum(u32) { _ };

const IterPlan = union(enum) {
    list: ListIter,
    range: RangeIter,
    unbounded_range: UnboundedRangeIter,
    single: SingleIter,
    append: AppendIter,
    concat: ConcatIter,
    map: MapIter,
    filter: FilterIter,
    custom: CustomIter,
    public: PublicIter,
};
```

Each plan stores explicit operands as Monotype expression ids, local ids,
function ids, checked identities, or child plan ids. It must not store display
names or source strings as semantic keys.

The plan store should also expose:

- item type
- known length information when available
- whether `Done` is reachable
- which public step variants can be produced
- state fields needed by optimized consumers
- materialization recipe for the public `Iter` value

Do not make variant reachability a backend-level wasm or LLVM optimization. It
is a property of the plan.

## Phase 3: Recognize Builtin Producers

Teach Monotype lowering to build plans for known builtin iterator producers.

Initial producers:

- `List.iter`
- inclusive and exclusive numeric ranges
- `Iter.single`
- `Iter.append`
- `Iter.concat`
- `Iter.map`
- `Iter.keep_if`
- `Iter.drop_if` if it remains part of the public API
- `Iter.custom`

Recognition must use exact checked identity:

- builtin method owner
- method name id
- resolved method target
- checked function template
- Monotype instantiation

The recognition code should sit near existing static-dispatch lowering, because
that is where the compiler already has the dispatch plan, method owner, target
callable type, and monomorphic operand types.

Tests:

- each producer creates the expected plan when used by an optimized consumer
- same source names from user modules do not get recognized as builtins
- imported builtin methods are recognized by identity
- local wrappers around builtin methods are recognized only when specialization
  exposes the builtin call explicitly

## Phase 4: Add Materialization Boundaries

Implement materialization from a known plan to the public `Iter` representation.

Materialization is required when:

- the source calls `.step` directly
- the source calls public `Iter.next` and the result escapes as a public value
- the iterator value is stored in an aggregate that survives as a public value
- the iterator value is returned from a function without a specialized plan
- the iterator value is passed to a call that is not being specialized with a
  plan
- the compiler reaches `Public(iter_value)`

Materialization should reuse the existing public builtin semantics. It may call
or lower the public constructors, but the result must be ordinary Monotype/LIR,
not hidden backend behavior.

Tests:

- `saved = iter` followed by optimized consumption of `iter` keeps `saved`
  correct
- returning `List.iter(list)` from a function still returns a public iterator
- manually calling `.step` on a returned iterator works
- matching on public `Iter.next` works
- a materialized rest iterator from `Iter.next` can be consumed later

## Phase 5: Lower Optimized Consumers

Teach consumers to accept `IterPlan` directly.

Initial consumers:

- source `for`
- `Iter.fold`
- `List.from_iter`

For each consumer, lowering should produce loops over plan state fields.

### `for`

Current `for` lowering creates one loop iterator parameter, calls `.next`,
matches the public step union, and updates the loop iterator to `rest`.

New optimized lowering:

- lower the iterable source to an `IterPlan`
- allocate loop parameters for the plan state fields plus source loop carries
- generate direct step code for the plan
- bind produced item to the source pattern
- continue with updated private state
- break on the plan's done condition

The optimized path is selected only when the source iterator has a known plan.
If the source is `Public(iter_value)`, lower through the existing public
`Iter.next` semantics.

### `Iter.fold`

Specialize the builtin consumer so that:

- accumulator is a loop parameter
- plan state fields are loop parameters
- mapping/folding function is called directly
- public step values are not constructed in the hot loop

### `List.from_iter`

Specialize collection so that:

- initial capacity consumes plan known-length information when available
- the list accumulator is updated through existing list append low-levels
- plan state fields are loop parameters
- public iterator records and step closures are not rebuilt in the hot loop

Tests:

- optimized `for` over list/range/map/filter/append/concat
- optimized `Iter.fold` over the same producers
- optimized `List.from_iter` over the same producers
- public behavior is identical to the current implementation
- dev or non-optimized builds may use the public representation, but optimized
  builds must satisfy the IR shape tests

## Phase 6: Integrate With Existing Optimization Passes

Keep existing passes, but make their responsibilities explicit.

### SpecConstr

SpecConstr may still optimize ordinary known constructor call patterns. It is
not the long-term owner of builtin iterator semantics.

Tasks:

- remove iterator-specific assumptions from comments once `IterPlan` owns the
  model
- keep generic call-pattern specialization for non-builtin record/tag/callable
  cases
- ensure specialized wrappers can expose builtin producer calls to
  `IterPlan`

### ScalarizeJoins

ScalarizeJoins should become a cleanup improvement, not a requirement for basic
iterator plan lowering.

Tasks:

- verify optimized iterator loops already carry fields directly before LIR
- keep ScalarizeJoins useful for residual aggregate state
- add tests proving iterator loops do not regress if ScalarizeJoins changes

### TagReachability

Plan lowering should emit only reachable internal control flow where possible.
TagReachability should remove residual impossible public-tag branches.

Tasks:

- expose plan variant reachability as explicit lowering data
- verify `ListIter` does not keep `Append`/`Skip` hot-path branches
- verify `Filter` uses internal skip control instead of public `Skip` values

### ARC

ARC remains the only owner of reference-counting policy.

Tasks:

- verify plan state fields containing lists/strings/callables receive correct
  ARC statements
- verify no pass adds iterator-wrapper refcounts
- verify list payloads shared by public saved iterators and private cursors
  are retained/released correctly

## Phase 7: Custom Iterators

`Iter.custom` must remain possible. It is the public escape hatch for arbitrary
state machines, including infinite ones.

The compiler can represent custom iterators as:

```text
Custom(state, step_fn)
```

where `state` is a public Roc value and `step_fn` is a known callable when
available.

Optimization levels:

1. If the custom step function and state are known, specialize the consuming
   loop over the custom state directly.
2. If the custom step function is not known, materialize or use the public
   representation.
3. If the custom step result is public, preserve exact public `One`/`Skip`/
   `Append`/`Done` semantics.

Do not use function names or closure layout shape to decide this. Use checked
callable identity and Monotype function ids.

Tests:

- finite custom iterator
- infinite custom iterator consumed by finite loop
- custom iterator whose state contains refcounted values
- custom iterator reused after one consuming loop
- unknown custom iterator passed through public API

## Phase 8: Static Data And Compile-Time Constants

Iterator plans must compose with the existing compile-time root and static-data
work.

Required behavior:

- a top-level or hoisted `List.iter(static_list)` may evaluate during checking
- reachable static list payloads are emitted once as static data
- optimized consumers of that iterator should read the static list directly
  through plan state
- public materialization of that iterator should still produce a valid public
  `Iter` value
- inlining a named static iterator back into source should not change optimized
  code shape

Tests:

- named static list iterator consumed by `for`
- inline static list iterator consumed by `for`
- named and inline forms produce equivalent optimized IR
- static iterator saved as public value and also consumed privately remains
  correct
- Rocci Bird `base_points = [...].iter()` stays static and shared

## Phase 9: Rocci Bird Proof

Use Rocci Bird as the integration proof after unit tests pass.

Source repo:

```sh
cd /home/rtfeldman/code/roc-wasm4
```

Compiler repo:

```sh
cd /home/rtfeldman/code/worktrees/roc/vivid-canyon/roc
```

Build steps:

```sh
zig build
```

Then rebuild Rocci Bird with the current compiler and the wasm4 platform:

```sh
roc build --opt=size rocci-bird.roc
```

Run Binaryen optimization through the integrated wrapper used by the compiler
pipeline for wasm size builds.

Record:

- raw wasm size
- optimized wasm size
- function count
- call count
- indirect call count
- branch-table count
- presence or absence of public iterator helper calls
- presence or absence of public step closure calls in `update`

Disassembly success criteria:

- static sprite and collision-point data are in static data, not rebuilt in
  `update`
- `on_screen_collided!` does not rebuild the base points list
- the collision-point iterator loop carries direct cursor state
- `Iter.append`, `Iter.concat`, `iter_from_step`, and generic public step
  helper calls are absent from the normal gameplay hot path unless the source
  genuinely crosses a public materialization boundary
- no new heap allocation appears in normal gameplay iteration

Runtime success criteria:

- optimized wasm4 build runs correctly
- dev wasm4 build runs correctly
- gameplay behavior matches the current working build
- size is no worse than the current `.iter()` build, and the expected direction
  is smaller code in `update`

## Phase 10: Cleanup The Public Builtin Implementation

After optimized lowering no longer depends on the pure Roc implementation for
hot paths, simplify the public builtin implementation only where it remains the
best semantic definition.

Rules:

- keep public methods readable and obviously correct
- do not contort public Roc code to help optimized hot paths
- keep `Append` as part of the public step union if that is the desired public
  API
- keep public `Iter.next` behavior exact
- remove comments that claim a source-level workaround is required for compiler
  limitations once the compiler limitation is fixed

Tests:

- full builtin iterator test suite
- public `.step` tests
- public `Iter.next` tests
- optimized consumer IR shape tests

## Completion Checklist

- [ ] `design.md` documents internal iterator plans and public materialization
      boundaries.
- [ ] `IterPlan` data exists in the post-check pipeline.
- [ ] Plans are keyed by explicit checked/Monotype identity, never names or
      generated code shape.
- [ ] `List.iter` produces a `ListIter` plan for optimized consumers.
- [ ] Numeric ranges produce finite or unbounded range plans.
- [ ] `Iter.single` produces a `Single` plan.
- [ ] `Iter.append` produces an `Append` plan.
- [ ] `Iter.concat` produces a `Concat` plan.
- [ ] `Iter.map` produces a `Map` plan.
- [ ] `Iter.keep_if` and `Iter.drop_if` produce filter-like plans.
- [ ] `Iter.custom` remains supported.
- [ ] Public materialization from every plan is implemented.
- [ ] Public alias/reuse tests pass.
- [ ] Rest-escaping tests pass.
- [ ] Infinite iterator tests pass.
- [ ] Refcounted payload tests pass under ARC.
- [ ] Optimized `for` lowers known plans without public step values in the hot
      loop.
- [ ] Optimized `Iter.fold` lowers known plans without public step values in
      the hot loop.
- [ ] Optimized `List.from_iter` lowers known plans without public step values
      in the hot loop.
- [ ] Static named and inline iterator constants optimize equivalently.
- [ ] No backend knows iterator semantics.
- [ ] No iterator-wrapper heap allocation is required for optimized consumers.
- [ ] No iterator-wrapper refcount uniqueness check is introduced.
- [ ] Rocci Bird optimized wasm builds and runs.
- [ ] Rocci Bird disassembly proves normal gameplay iteration uses direct
      cursor state.
- [ ] Rocci Bird size is recorded against the current baseline and the Rust
      comparison build.

## Verification Commands

Run focused tests first:

```sh
zig build run-test-eval -- --filter "Iter." --threads 1
zig build run-test-zig-module-postcheck
zig build run-test-zig-module-lir
zig build run-test-zig-module-compile -- --test-filter "static data"
```

Then run the broader compiler checks that cover the touched stages:

```sh
zig build test
```

For wasm4/Rocci Bird verification:

```sh
cd /home/rtfeldman/code/worktrees/roc/vivid-canyon/roc
zig build

cd /home/rtfeldman/code/roc-wasm4
roc build --opt=size rocci-bird.roc
```

Use the existing wasm disassembly tools in `/tmp` or the repo-local scripts
already used on this branch to compare `update`, `on_screen_collided!`, helper
reachability, and final binary size.
