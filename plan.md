# Iter And Stream Known-Value Specialization Plan

## Goal

Make `Iter` and `Stream` optimize to concrete cursor state machines while their
public Roc APIs stay ordinary pure/effectful Roc functions.

The desired end state is:

- `Iter(item)` uses the mainline public shape:

  ```roc
  Iter(item) :: {
      len_if_known : [Known(U64), Unknown],
      step : () -> [One({ item : item, rest : Iter(item) }), Skip({ rest : Iter(item) }), Done],
  }
  ```

- `Stream(item)` remains the effectful analog:

  ```roc
  Stream(item) :: {
      len_if_known : [Known(U64), Unknown],
      step! : () => [One({ item : item, rest : Stream(item) }), Skip({ rest : Stream(item) }), Done],
  }
  ```

- There is no public or private `Append` step variant.
- There is no iterator-specific post-check plan representation in the target
  design.
- `Iter` and `Stream` builtins stay written as ordinary Roc functions.
- Optimized code specializes the ordinary known-value facts those functions
  produce: primitive leaves, expression leaves, records, tuples, tags,
  nominals, callables, and captures.
- Lambda sets carry the concrete callable/capture information that Rust carries
  in concrete iterator adapter types.
- Public iterator values remain immutable and reusable.
- Optimized consuming loops may update only compiler-owned private cursor state.
- Rocci Bird's collision loop has the same optimized shape whether its base
  collision points are written as a list or as a top-level `.iter()` value.
- A primitive wrapped in a single-field record must not become more optimizable
  merely because it was wrapped. Primitive leaves are valid private state.

## Backstory

Rocci Bird exposed two separate problems:

1. The wasm binary was much larger than expected.
2. Rewriting a collision-point list to `.iter().append(...)` made the optimized
   output larger even though iterator adapters should not allocate.

The first wave of work improved the wasm path: wasm memory handling, Binaryen
integration, host export stripping, `bulk-memory` code generation, and several
Rocci Bird source cleanups. After that, the remaining size gap was concentrated
in ordinary Roc value lowering: the optimized `update` body still contained a lot
of iterator/list/control scaffolding compared to the Rust port.

The collision code made the iterator issue concrete:

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

The source program is pure. The loop should be able to become a small private
state machine over:

- the static list cursor
- the chosen branch/phase
- zero, one, or two appended points

Instead, the public iterator representation can survive too long. The compiler
then builds or passes `Iter` records, step thunks, step tags, and `rest` records
only to destructure them immediately inside the loop.

## What We Tried

### Explicit Iterator Plans

This branch added compiler-internal iterator plans. The plan vocabulary covered
list iteration, ranges, single items, append, concat, map, filter, custom
iterators, and public iterator values.

That was useful as an experiment because it proved direct cases can become the
right low-level loop shape:

- `for x in list`
- `for x in list.iter()`
- `for x in list.iter().append(a).append(b)`
- `for x in Iter.single(item)`

It also proved what is wrong with that direction:

- It creates an iterator-specific IR concept for behavior already expressed by
  ordinary Roc functions and lambda captures.
- It requires special handling in Monotype, lifting, lambda solving, Lambda
  Mono, and LIR invariant checks.
- It encourages more iterator-specific cases for `for`, `if`, `match`,
  `fold`, and `List.from_iter`.
- It does not generalize to other APIs shaped like `Stream`, parser builders,
  or user-defined state machines.

The target design does not keep this machinery.

### The `Append` Step Variant

This branch also changed public `Iter.next` to include:

```roc
Append({ before : Iter(item), after : item })
```

The motivation was local: make `.append` cheap by representing it directly.
That is the wrong abstraction. It leaks one adapter into the public step
protocol and forces every iterator consumer to understand a fourth step value.
It also diverges from `Stream`, where the same optimization problem exists but
should not require a public `Append` step.

The mainline `Iter` shape is already the right public shape. `append` should be
ordinary Roc code that builds a step thunk. Optimized code should avoid the
wrapper by specializing the ordinary thunk/capture shape, not by changing the
public protocol.

### Rust Comparison

The Rust port is much smaller because Rust iterators do not lower through a
single heap-allocated public iterator object in optimized code. Rust's
`Iterator` is a trait, and adapter chains usually become concrete nested types
after monomorphization. A `for` loop calls `next(&mut state)`, and after
inlining LLVM sees fields such as pointer, index, end, phase, and captured
function values.

Roc must not copy Rust's typing design here. Roc's public type is the concrete
`Iter(item)`, not a trait/interface and not a type-level encoding of every
adapter chain. Roc source like the Rocci Bird branch above must keep
type-checking as one `Iter(Point)` value even when different branches build
different adapter chains.

Roc already has the tool that can preserve the internal adapter facts without exposing
it in the public type: ordinary lambdas and lambda sets. A value of type
`Iter(item)` is a record containing a step function. The step function's lambda
set can retain the concrete function identity and captures for list iterators,
append wrappers, maps, filters, ranges, and custom iterators. Later optimizer
work can split those captures and call targets into private loop state.

So the target is Rust-like optimized output, not Rust-like typing:

```text
Rust: adapter shape is visible in concrete iterator types.
Roc: adapter shape is visible in finite lambda sets and captured values.
```

## The Erasure Problem

If the compiler waits until a public `Iter(item)` value has been fully lowered
to a generic record plus an erased or opaque callable, the adapter facts are
gone. At that point the backend can only see "call this function value" and
"match this public step tag." Recovering list/range/append/map behavior from
that code would require guessing from generated code shape, names, or backend
output, which is not allowed.

The optimization must run while the compiler still has:

- ordinary lifted function identities
- explicit captures
- finite callable flow
- known primitive leaves and expression leaves
- known records/tags/tuples/nominals
- checked direct-call targets

That means the work belongs in the existing post-check optimizer area that
already specializes known constructor and callable values, especially
`src/postcheck/monotype_lifted/spec_constr.zig`, plus any necessary explicit
data handed to later stages. The solution is not a backend peephole and not an
iterator-only lowering path.

## Target Design

The optimizer's abstraction is a known-value fact, not a "shaped expression."
A fact says what the compiler still knows about a value at a specific point
while preserving the source program's evaluation order:

- `Unknown`: no useful decomposition is available.
- `Leaf(expr)`: an ordinary expression leaf, including primitive values,
  runtime computations, and other values that should be carried directly.
- `Record`, `Tuple`, `Tag`, or `Nominal`: a constructor with child facts.
- `Callable`: a known lifted target or finite lambda-set member with child
  facts for captures.
- `CallableAlternatives`: a finite set of known callable members for the same
  function type, each with its own capture facts.

This is deliberately broader than aggregate shape. Records, tuples, tags, and
nominals can expose useful children, but they are not the only useful state.
The pass must be able to carry primitive leaves directly. A loop whose private
cursor is naturally `{ list, index, len }` should split to those leaves; a loop
whose private cursor is naturally just `index` should carry only `index`, not a
synthetic `{ index }` wrapper.

The target implementation uses names such as `KnownValue` or `ValueFact` for
this optimizer data. It does not expose APIs, comments, or tests that talk
about "shaped expressions." Names such as `Shape` are reserved for actual type,
layout, serialization, or source-shape concepts, not this optimizer fact model.

Finite callable alternatives are required for real iterator state machines.
`Iter.append`, `Iter.concat`, finite ranges, and empty iterators do not keep one
fixed step function target for the whole consuming loop. For example,
`base.iter().append(last)` can continue from the append step wrapper to another
append wrapper with a different source cursor, and eventually to the empty
iterator's zero-capture step thunk. Widening that field to an ordinary callable
keeps the public iterator protocol in the hot loop. The optimizer must preserve
the finite callable alternatives and their captures, then lower calls through
that field by the same finite lambda-set machinery used for ordinary Roc
function values.

### Public Builtins

Restore the public `Iter` shape from `origin/main` manually. Do not reset the
branch. This worktree contains many unrelated compiler and wasm changes that
must be preserved.

The restore must be limited to the iterator shape and the code/tests that
necessarily follow from that shape:

- `Iter.step` returns only `One`, `Skip`, or `Done`.
- `Iter.next` returns only `One`, `Skip`, or `Done`.
- `iter_from_step` accepts only a step thunk with those three variants.
- `Iter.append`, `concat`, `map`, `keep_if`, `drop_if`, `fold`,
  `take_first`, `drop_first`, `step_by`, `stream`, and `List.from_iter` stop
  matching on `Append`.
- No user-visible docs mention `Append`.
- Tests that were written for the four-variant branch are deleted or rewritten
  to test the restored three-variant public API.

`Stream` stays in the same public family: `len_if_known` plus an effectful
`step!` thunk returning `One`, `Skip`, or `Done`.

### Optimized Representation

Optimized code should see ordinary known-value facts, not an iterator
plan:

- A list iterator is an `Iter` record whose `step` callable captures the list
  and index.
- An append iterator is an `Iter` record whose `step` callable captures the
  source iterator and appended item.
- A mapped iterator is an `Iter` record whose `step` callable captures the
  source iterator and transform.
- A filtered iterator is an `Iter` record whose `step` callable captures the
  source iterator and predicate.
- A `Stream` has the same public representation except its step function is
  effectful.

The optimizer should split records, callables, captures, and primitive leaves
into private state only when the source meaning permits it. Reusing an iterator
must remain correct:

```roc
iter = [1, 2].iter()
saved = iter

for item in iter {
    {}
}

use(saved)
```

The loop may advance a private compiler-created cursor derived from `iter`. It
must not mutate the public `iter` value or the `saved` value.

### General Known-Value Specialization

The existing specialization pass already has most of the right mechanics, but
its vocabulary should move from aggregate "shape" toward known-value facts.
The target fact model is:

- `Unknown`
- `Leaf(expr)`
- `Record(fields)`
- `Tuple(elems)`
- `Tag(name, payload)`
- `Nominal(wrapper, child)`
- `Callable(target_or_member, captures)`

It also already:

- records direct-call patterns
- splits known constructor arguments into leaves
- simplifies field reads, tuple reads, known tags, and known callable calls
- specializes loop state when loop initial values have known constructor facts
- inlines known callable calls
- has direct-call inlining machinery guarded against recursion

The work is to make that machinery complete enough and correctly staged, not
to add a new iterator optimizer.

The target pass design is one known-value specialization engine with two
products:

- **Base body rewrite:** every original Roc function body is cloned once back
  into the same function id, with the same arguments, captures, return type,
  and public ABI. This pass performs local fact rewrites such as field
  projection, tag simplification, callable inlining, branch/match fact joins,
  and loop-state splitting.
- **Extra direct-call workers:** when a direct call passes a value with useful
  known facts into an argument that the callee actually uses in a
  fact-demanding way, the same engine creates an additional worker whose ABI
  receives that argument's leaves directly.

These two products must not be conflated. Loop-state splitting is local to a
function body and must not require a constructor-valued caller argument. This
must optimize the same way:

```roc
sum : I64 -> I64
sum = |start| {
    var $state = { n: start, acc: 0 }

    while $state.n != 0 {
        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
    }

    $state.acc
}
```

as this:

```roc
sum : { n : I64 } -> I64
sum = |start| {
    var $state = { n: start.n, acc: 0 }

    while $state.n != 0 {
        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
    }

    $state.acc
}
```

The record argument only matters for a possible interprocedural worker ABI. It
must not be the reason the body gets local loop-state specialization. A
primitive leaf is already valid private state.

The pass runs only in optimized post-check lowering. Normal CLI builds already
express that through `postCheckInlineModeForOpt`: `--opt=size` and
`--opt=speed` use wrapper inlining, while dev/interpreter use `.none`. The
compiler must keep language-required compile-time evaluation and diagnostics in
all modes, but this private cursor-state specialization belongs only to the
optimized path.

The pass should run as a worklist:

1. Compute explicit argument-demand data: which callee arguments are inspected
   by field access, tuple access, match, callable call, or by propagation
   through another direct call.
2. Clone each original Roc function body in place as the base specialization,
   preserving its ABI and applying the ordinary known-value rewrites.
3. While cloning any base body or worker, record newly discovered direct-call
   worker patterns from explicit known-value facts.
4. Reserve a function id for each newly discovered worker pattern.
5. Clone each worker with split arguments, applying the same body-local
   known-value rewrites and recording any further worker patterns it discovers.
6. Continue until the worklist is empty.

There should be no separate post-clone cleanup phase that scans the finished
program and tries to rewrite calls after the fact. Calls are rewritten while
their containing body is cloned, using the same explicit known-value facts as
every other optimization. This gives the pass a single source of truth and
avoids a late pass trying to reconstruct information that the cloner already
had.

The generalized pass must expose known-value facts through:

- direct calls whose bodies produce primitive leaves, records, tags, tuples,
  nominals, or callables
- local bindings
- blocks
- `if` branches
- `match` branches
- loop initial values
- loop `continue` values
- calls through function fields such as `(iterator.step)()`
- public `Iter.next` and `Stream.next!` wrappers after inlining

When a direct call's result is demanded as a known value, the pass may inline
the callee body through the existing direct-call inliner so the returned facts
are visible. This must be demand-driven by the surrounding expression that
consumes the facts, not by iterator names.

Examples of fact-demanding contexts:

- field access on a returned record
- match on a returned tag
- calling a returned callable
- loop state that can be carried as private leaves
- a `continue` value that must match a split loop state
- a direct call argument position already selected for constructor/callable
  specialization

The pass must not infer iterator behavior from names such as `iter`, `append`,
or `next`. It sees only ordinary checked direct calls and ordinary Roc values.

### Branch And Match Fact Joins

Rocci Bird needs known iterator facts to survive this pattern:

```roc
collision_points =
    if cond_a {
        base_points.append(a).append(b)
    } else if cond_b {
        base_points.append(c)
    } else {
        base_points
    }
```

The optimizer must be able to represent a common outer constructor when every
branch returns the same kind of known value. For an `Iter`, that common outer
constructor is the record fields:

- `len_if_known`
- `step`

The fields themselves may still be ordinary branch expressions when their
values differ. This is still useful: the loop state can avoid rebuilding the
outer record, and later lambda-set solving can keep the callable flow finite.

If branches do not share a common constructor, the enclosing expression stays
an ordinary expression. That is a normal outcome, not a compiler recovery path.

Branch conditions, scrutinees, guards, branch-local `dbg`, `expect`, and
`crash` must stay at their source evaluation positions. The optimizer must
never replay a declaration RHS later at a `for` site.

### Loop State

When a loop starts with useful known-value facts, loop parameters should be
carried as private leaves. This is already the Stream precedent:

```text
one Stream value
  -> len field, callable target, callable captures
```

For `Iter`, the same rule should produce private cursor state. A list iterator
eventually becomes list, index, and length. Append and concat add phase/tail
state. Map and filter add captured functions. The public `Iter` wrapper and
public step tags should disappear from the hot loop when all uses are private
consumer uses.

Loop `continue` values must have facts that are consistent with the loop's
selected private state. If they do, the continue passes leaves. If they do not,
the loop must not be partially rewritten into an invalid mixed state.

### Public Boundaries

Some uses genuinely need the public value:

- returning an iterator from a function whose caller is not specialized with
  its facts
- storing an iterator in a data structure as an ordinary value
- passing an iterator to code whose body is not available for specialization
- matching on `Iter.next(iter)` as a public value outside a private consuming
  loop

At those boundaries, the compiler materializes the ordinary `Iter` record and
ordinary step function value. That is correct. The optimizer is allowed to win
only when ordinary specialization proves the concrete facts remain available
at the consuming use.

## Implementation Steps

### 1. Capture The Current Baseline

- Record the current Rocci Bird `--opt=size` wasm size.
- Record the current no-`.iter()` collision version wasm size if available.
- Save disassembly snippets for the collision/update path in both versions.
- Save the relevant `rg` results for current `Append` and `iter_plan` usage.
- Run the current focused tests that cover iterator plans and Rocci Bird, so
  later failures are attributable.

### 2. Restore The Public `Iter` Shape Only

- Use `git show origin/main:src/build/roc/Builtin.roc` as a reference.
- Manually edit only the relevant `Iter` shape, helper, and method bodies.
- Preserve all unrelated branch changes.
- Remove the `Append` variant from:
  - `Iter.step`
  - `Iter.next`
  - `iter_from_step`
  - every `match Iter.next(...)`
  - `Stream.from_iter`
  - `List.from_iter`
- Update or delete tests/docs that expected `Append`.
- Verify `rg "Append\\(" src/build/roc/Builtin.roc` returns no iterator-step
  usage.

### 3. Rebuild And Repair Public Iterator Tests

- Run the smallest builtin/check test target that covers `Builtin.roc`.
- Fix syntax/type errors caused by the restored three-variant shape.
- Run the broader post-check test target that currently exercises iterator
  plan usage so it exposes every stale `Append` assumption.
- Commit the public-shape restoration before removing deeper compiler code.

### 4. Remove The Explicit Iterator-Plan Path

Once public `Iter` is back to three variants and tests are green, remove the
iterator-plan design from the implementation:

- delete `src/postcheck/iter_plan.zig`
- remove `ExprData.iter_plan`
- remove `Program.iter_plans`
- remove `IterPlanId`, `IterPlan`, and `addIterPlan`
- remove lifting, lambda solving, Lambda Mono, LIR, and structural-test support
  for `iter_plan`
- remove iterator-plan recognition and lowering from
  `src/postcheck/monotype/lower.zig`
- restore source `for` lowering to the ordinary checked `.iter`/`.next` path
- keep any independently useful non-iterator optimizer improvements

This step should shrink the compiler surface area. If removing a piece breaks a
non-iterator test, the test is pointing at a real dependency that needs to be
represented with ordinary constructor/callable shape, not with an iterator
plan.

### 5. Add Focused Known-Value Specialization Tests

Start with tests that fail after iterator-plan removal and pass only after the
general optimizer handles the facts.

Required test cases:

- `for` over a local `list.iter()`
- `for` over a local `list.iter().append(x)`
- `for` over an `if` whose branches return:
  - base iterator
  - base appended once
  - base appended twice
- the same branch facts through `match`
- `Iter.map` over a list iterator
- `Iter.keep_if` and `Iter.drop_if` over a list iterator
- `Iter.concat` and `Iter.prepended`
- `List.from_iter` over the same producer forms
- `Iter.fold` over the same producer forms
- `Stream.from_iter(...).map!` and `Stream.collect!`
- a saved iterator reused after a loop, proving public values are not mutated
- branch-local `dbg`, `expect`, and `crash` are not moved or duplicated
- imported module returns `Iter(item)` and the caller consumes it while the body
  is available for specialization
- a function value captured inside the iterator step closure
- a boxed lambda or closure-carrying value that exercises the same callable
  fact machinery outside iterators

The tests should inspect the compiler IR or generated wasm/object text where
possible, not only output values. Value tests prove correctness; fact tests
prove the optimization happened.

### 6. Refactor `SpecConstr` Around Base Bodies And Worker Worklist

Refactor `src/postcheck/monotype_lifted/spec_constr.zig` so the clone engine is
not only used when a call-pattern worker exists.

Concrete work:

- Represent the original function body as the **base specialization**.
- Clone every original Roc function body once into the same function id.
- Preserve the original function's ABI exactly:
  - same function id
  - same argument list
  - same captures
  - same return type
  - existing calls to that function still type-check and lower unchanged
- Run the same fact-aware cloner for base bodies and worker bodies.
- Let the base-body clone perform local rewrites:
  - known record/tuple field projection
  - known tag match simplification
  - known callable calls
  - branch/match fact joins
  - loop-state splitting
  - demanded direct-call result exposure
- While cloning any base body or worker, record newly discovered direct-call
  worker patterns.
- Maintain an explicit worklist of unwritten worker patterns.
- Reserve worker ids when patterns are discovered, then clone worker bodies by
  popping that worklist until it is empty.
- Rewrite calls while cloning the containing body. Delete the late
  `rewriteExistingCalls` style cleanup once the cloner owns all call rewriting.
- Rename touched optimizer data structures away from legacy aggregate-shape
  terminology and toward `KnownValue` or `ValueFact`.

This step must add a regression test proving primitive arguments do not block
local loop-state specialization:

```roc
sum : I64 -> I64
sum = |start| {
    var $state = { n: start, acc: 0 }

    while $state.n != 0 {
        $state = { n: $state.n - 1, acc: $state.acc + $state.n }
    }

    $state.acc
}
```

The optimized LIR for that function must split the loop state just like
the otherwise equivalent `{ n : I64 } -> I64` version. The test should assert
the loop join parameter count, not just final output.

### 7. Generalize Direct-Call Fact Exposure

Extend `spec_constr` so a direct call can expose a constructor/callable result
when the caller is currently trying to use that result as known facts.

Concrete work:

- Make fact-demanding contexts explicit in the cloner.
- Reuse `inlineDirectCallValue` for available Roc callees with no `return`.
- Keep the existing recursion guard.
- Preserve argument evaluation order with the existing pending-let machinery.
- If the inlined body produces a constructor/callable value, keep that value.
- If it does not, materialize the original direct call as an ordinary
  expression.
- Do not special-case builtin iterator names.

This is the piece that lets calls like `Iter.append(base, point)` expose the
record containing the new step thunk.

### 8. Generalize Fact Joins

Add a known-value fact join operation for `if` and `match`.

The join operation should:

- accept branches with the same outer constructor kind
- preserve record field names and tuple positions exactly
- preserve nominal wrappers only when the checked type matches
- preserve tag facts only when the tag name and payload structure match
- preserve callable facts only when the callable target and capture structure
  match
- otherwise use ordinary expression leaves inside the outer constructor when the
  outer constructor still matches
- reject the join when no common outer constructor exists

This lets `collision_points` keep the `Iter` record facts across branches even
when the selected step callable value differs.

### 9. Strengthen Loop-State Splitting

Update loop specialization so split loop state works with:

- base-body rewrites, not only call-pattern workers
- facts returned from direct calls
- facts returned from branch/match joins
- callable fields read from known records
- step results returned by inlined `Iter.next`/`Stream.next!`
- `continue` values that rebuild the same outer constructor
- `continue` values whose callable field stays finite but changes target or
  capture count

The loop rewrite must remain all-or-nothing for each loop parameter. If the
initial facts and every reachable `continue` fact cannot be made consistent,
keep the ordinary loop value.

The consistency check must not reduce "same public function type, different
known callable target" to `any`. It should form finite callable alternatives
when all reachable callables are known. Only truly unknown, erased, or
materialized callable values force the loop state back to the ordinary public
representation.

### 10. Ensure Lambda Sets Stay Finite Until Lowering

Verify that the optimizer does not prematurely erase step callables. The step
function in `Iter` and `Stream` must remain an ordinary callable value with
finite lambda-set flow whenever the producer body is available.

Required checks:

- calls through known callable fields inline when there is exactly one target
- calls through branch-selected callable fields lower through finite lambda-set
  dispatch, not erased callable ABI, when all targets are known
- captures are split where the known-value specialization pass can split them
- public ABI or hosted boundaries still force erasure only through existing
  checked data

### 11. Delete Stale Iterator-Plan Tests And Add New Ones

- Remove structural tests asserting `iter_plan` fields exist.
- Replace them with tests asserting the absence of iterator-plan IR.
- Add known-value specialization tests for the cases listed above.
- Add regression tests for `Iter.next` returning exactly three variants.
- Add regression tests that `Stream.next!` remains the effectful analog of
  `Iter.next`.

### 12. Rebuild Rocci Bird

For Rocci Bird:

- keep the source using `base_points = [...].iter()`
- build with `roc build --opt=size`
- run Binaryen size optimization as configured by the compiler
- record final wasm byte size
- disassemble `update`
- compare to:
  - the direct-list/no-`.iter()` version
  - the Rust port built with Rust size optimizations plus Binaryen
  - the old Rocci Bird wasm from the comparison data

The expected result is not bit-for-bit parity with Rust. The required result is
that the iterator version no longer carries public iterator wrapper churn in
the collision loop and no longer regresses significantly relative to the
direct-list source.

### 13. Update Documentation

- Update `design.md` to describe the lambda/callable known-value design.
- Remove the old design claim that explicit iterator plans are the source of
  truth.
- Mention the Rust comparison explicitly:
  - Rust carries adapter state in concrete iterator types.
  - Roc carries adapter state in ordinary lambda sets and captures.
  - Both should optimize to concrete private cursor state.
- Keep `plan.md` as the execution checklist until the implementation lands.

## Completion Checklist

- [x] `Iter.step` has exactly `One`, `Skip`, and `Done`.
- [x] `Iter.next` has exactly `One`, `Skip`, and `Done`.
- [x] `Stream.step!` and `Stream.next!` remain the effectful three-variant
      analog.
- [x] No iterator public API mentions `Append`.
- [x] No `src/postcheck/iter_plan.zig` remains.
- [x] No post-check IR expression has an `iter_plan` case.
- [x] Source `for` lowering uses ordinary checked `.iter` and `.next`.
- [x] `SpecConstr` rewrites every original Roc body as a base specialization
      while preserving its ABI.
- [x] Direct-call worker creation uses an explicit worklist of discovered
      call patterns.
- [x] Call rewriting happens while cloning the containing base body or worker.
- [x] No late `rewriteExistingCalls` cleanup pass remains.
- [x] Primitive function arguments get the same local loop-state
      specialization as equivalent single-field-record arguments.
- [ ] Known-value specialization handles direct-call results in demanded
      contexts.
- [x] Known-value specialization handles `if` and `match` joins.
- [ ] Known-value specialization preserves finite callable alternatives across
      branch joins and loop `continue` refinements.
- [ ] Loop-state splitting handles iterator records and step callables.
- [x] Lambda solving keeps known step callables finite where bodies are
      available.
- [x] Public iterator reuse tests pass.
- [x] `dbg`, `expect`, and `crash` movement/duplication tests pass.
- [x] Imported-module iterator producer tests pass.
- [x] Stream optimization tests pass.
- [x] Boxed/captured callable fact tests pass outside iterator code.
- [ ] Rocci Bird `--opt=size` builds.
- [ ] Rocci Bird optimized collision loop disassembly contains no public
      iterator wrapper churn on the hot path.
- [ ] Rocci Bird `.iter()` collision source and direct-list collision source
      have equivalent optimized loop shape.
- [ ] Final Rocci Bird wasm size is recorded and compared to the Rust port.
- [ ] `zig build test` or the agreed focused compiler test set passes.
- [x] Changes are committed in small checkpoints.

## Current Failing Regression

The focused regression command is:

```sh
zig build run-test-zig-lir-inline -- --test-filter "static primitive list iter append loop lowers no bulkier"
```

It currently fails because the iterator form lowers to substantially more
reachable LIR than the direct-list form:

```text
switch_count: iter form has 29, direct-list form has 4
```

The minimized record variant fails for the same reason. The latest
investigation found these concrete gaps:

- Direct-call/result exposure is now far enough along that the minimized
  primitive test no longer fails on direct calls. The remaining excess is
  public iterator control protocol: the `.iter().append(...)` form still
  lowers to public step-callable and `One`/`Skip`/`Done` tag switches in the
  hot path.
- The LIR dump for the primitive failure shows the iterator branch carrying an
  append step callable as loop state (`tag_union#35`), matching the source
  iterator's public step result (`tag_union#49`), then matching the outer
  public iterator step result (`tag_union#43`). The direct-list baseline has
  only the list cursor loop and the source `use_extra` branch.
- Preserving already-known tag facts in loop initial state is correct, and
  loop tag refinement must treat a different tag as a real mismatch. However,
  that alone does not reduce the switch count: once a reachable `continue`
  changes from `pending_last = True` to `pending_last = False`, the current
  common-loop-fact logic still collapses that phase field to an ordinary
  runtime Bool.
- `TagReachability` can prune impossible switch arms from path-insensitive tag
  sets, but it cannot split a loop join by incoming tag/callable state. That
  means the long-term fix cannot rely on a backend or post-LIR cleanup pass to
  recover iterator facts after lowering.
- The missing implementation is explicit finite-state loop specialization.
  A loop parameter whose fact changes among a finite set of known tags or
  known callable targets across `continue` edges must remain a finite
  optimizer state, not widen to `any`. Step calls through those finite states
  must inline or dispatch through the same known callable machinery while
  keeping each target's captures as private state.
- This state must include pending setup and conditional/match branches so that
  a transition such as append-with-pending-item -> source iterator ->
  append-with-no-pending-item is represented before the cloner emits terminal
  `continue` expressions. Moving lets around the terminal `continue`, or
  recognizing iterator names after the fact, is not a valid fix.

## Non-Negotiable Invariants

- Do not reset this branch to `origin/main`.
- Do not recover iterator behavior from names, generated symbols, wasm bytes,
  object bytes, or backend output.
- Do not add another iterator-specific pass.
- Do not make `Iter` a trait/interface or encode adapter chains in Roc types.
- Do not mutate public iterator values.
- Do not use reference counting to decide whether iterator wrappers are unique.
- Do not move or duplicate `dbg`, `expect`, `crash`, branch conditions,
  appended item expressions, or stream effects.
- Do not let LIR or backends know iterator rules.
- Do not keep both explicit iterator plans and generalized known-value
  specialization as competing systems.
- Do not add a late cleanup pass that reconstructs call fact information after
  body cloning; calls must be rewritten by the same fact-aware cloner that has
  the explicit facts.
