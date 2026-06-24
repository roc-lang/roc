# Effect Propagation And Const Root Plan

## Goal

Fix Roc effect soundness and compile-time root selection together in checking.

The checker should:

- reject effectful calls in pure top-level values, pure functions, and expects
- handle delayed static-dispatch effectfulness correctly
- select maximal compile-time roots during the existing checker traversal
- run `crash`, `dbg`, and `expect` during compile-time evaluation whenever the
  containing expression has no runtime data dependency and no effectful call
- store only reachable evaluated values in checked module data and static data
- avoid a second expression walk and avoid a stored table for every expression

The current hoisting implementation should be replaced. Do not build new logic
on top of the current observable-effect pruning rules.

## Required Invariants

- Checking is the final user-facing stage for effect and compile-time
  evaluation errors.
- Static-dispatch effectfulness must be explicit checker output. No consumer may
  guess it from method names, `!` spelling, or unresolved function types.
- `crash`, `dbg`, and `expect` are not effectful calls. They must not disqualify
  compile-time roots.
- Effect dependencies are directed. A caller depending on a callee must not be
  represented as equality.
- Union-find is not the main effect solver. Strongly connected recursive groups
  may be condensed, but unrelated caller/callee pairs must remain directed.
- Ordinary nested expressions should use stack-local summaries. Store only data
  needed after the current expression returns.
- Root selection keeps maximal eligible roots. If a parent expression is a root,
  child roots inside it are removed. If the parent later resolves effectful,
  eligible children survive.
- No leaf-shape rules. Numbers, strings, empty lists, records, and other leaves
  follow the same parent-child root rule as every other expression.
- No post-check stage may repair missing checked data or rerun checked
  validation.

## Current Bugs To Reproduce First

These should fail on the current compiler and pass after the effect work lands.

### Effectful Static Dispatch At Top Level

```roc
package [] {}

Eff := [Eff].{
    tick! : Eff => U64
    tick! = |_| 1
}

top = (Eff.Eff).tick!()
```

Expected: `effectful_top_level`.

### Effectful Method In Pure Function Annotation

```roc
package [] {}

Eff := [Eff].{
    tick! : Eff => U64
    tick! = |_| 1
}

direct : Eff -> U64
direct = |x| x.tick!()
```

Expected: pure/effectful annotation mismatch. The same body with
`direct : Eff => U64` should pass.

### Pure Where Clause Resolved To Effectful Method

```roc
package [] {}

uses_tick : a -> U64 where [a.tick! : a -> U64]
uses_tick = |x| x.tick!()

Eff := [Eff].{
    tick! : Eff => U64
    tick! = |_| 1
}

value = uses_tick(Eff.Eff)
```

Expected: the implementation method does not satisfy the pure where-clause
method type.

### Effectful Where Clause Propagates To Caller

```roc
package [] {}

uses_tick : a => U64 where [a.tick! : a => U64]
uses_tick = |x| x.tick!()

Eff := [Eff].{
    tick! : Eff => U64
    tick! = |_| 1
}

top = uses_tick(Eff.Eff)
```

Expected: `uses_tick` itself is effectful, and `top` reports
`effectful_top_level`.

### Effectful Dispatch Inside Expect

```roc
package [] {}

Eff := [Eff].{
    tick! : Eff => U64
    tick! = |_| 1
}

expect (Eff.Eff).tick!() == 1
```

Expected: `effectful_expect`.

## Implementation Plan

### Phase 1: Add Focused Effect Regression Tests

Add checker tests before changing the implementation. Use the existing snapshot
or focused check-test harness that best matches these diagnostics.

Test cases:

- direct top-level effectful function call still reports `effectful_top_level`
- static-dispatch top-level method call reports `effectful_top_level`
- pure function annotation rejects an effectful direct call
- pure function annotation rejects an effectful method call
- effectful function annotation accepts an effectful method call
- pure where-clause rejects an effectful implementation method
- effectful where-clause makes the wrapper effectful
- `expect` rejects direct effectful calls
- `expect` rejects delayed effectful dispatch
- `dbg`, `expect`, and `crash` without effectful calls do not count as
  effectful calls

Additional static-dispatch shapes, where supported by current Roc syntax:

- type-dispatch call
- method equality
- binop or unary dispatch
- interpolation dispatch
- iterator `iter` or `next` dispatch from `for`
- imported nominal method

Success criteria:

- The known current bug cases fail before the implementation changes.
- Existing direct-call effect tests still pass.
- The tests distinguish direct effect calls from delayed static-dispatch effect
  calls.

### Phase 2: Add Effect Slots

Introduce checker-owned effect slots for the places where effectfulness matters.

Proposed data:

```zig
const EffectSlotId = enum(u32) { _ };

const EffectSlotKind = union(enum) {
    function_body: CIR.Expr.Idx,
    top_level_value: CIR.Def.Idx,
    expect_body: CIR.Node.Idx,
    const_root_candidate: u32,
};

const EffectSlot = struct {
    kind: EffectSlotKind,
    direct_effect: bool,
    outgoing_start: u32,
    outgoing_len: u32,
    resolved_effectful: ?bool,
};

const EffectEdge = struct {
    from: EffectSlotId,
    to: EffectSlotId,
};
```

Add an active effect-slot stack to `Check.zig`.

When checking:

- create a slot for each lambda/function body before checking its body
- create a slot for each top-level value RHS
- create a slot for each `expect` body
- mark the active slot on direct calls to resolved `fn_effectful` functions
- add an edge from active slot to callee slot when calling a known local
  function whose slot is known
- keep direct-call behavior identical to today before touching static
  dispatch

Do not store a slot for every expression.

Success criteria:

- Existing direct-call effect behavior still passes.
- Function creation is not effectful merely because the function body contains
  effects; calling the function is what propagates to the caller.
- Recursive and mutually recursive functions have directed edges and are solved
  without recursion in the implementation.

### Phase 3: Connect Static Dispatch To Effect Slots

Static-dispatch calls already create dispatch function variables and store
metadata for diagnostics. Extend this with effect watchers.

Proposed data:

```zig
const DispatchEffectWatch = struct {
    fn_var: Var,
    slot: EffectSlotId,
};
```

When `checkExpr` creates a dispatch function variable for:

- method calls
- type-method calls
- binop/unary dispatch
- interpolation dispatch
- synthetic iterator dispatch
- where-clause dispatch copied during instantiation

record that the active effect slot watches that function variable.

When static-dispatch resolution successfully checks a method against a dispatch
function variable:

- inspect the resolved dispatch function type
- if it is effectful, mark all watching slots effectful
- if it is pure, leave watchers unmarked
- if it is still unbound at a point where checking output would be finalized,
  make finalization handle it explicitly rather than guessing from source shape

Where clauses must preserve effect kind:

- `where [a.f : a -> b]` requires a pure implementation
- `where [a.f : a => b]` requires an effectful implementation
- an effectful where-clause call makes the wrapper effectful

Success criteria:

- The current method-call soundness bugs fail for the right reason before the
  fix and pass after the fix.
- A pure where-clause cannot be satisfied by an effectful method.
- An effectful where-clause propagates to callers.
- Dispatch diagnostics still point at the same source regions.

### Phase 4: Finalize Effect Slots At The Right Boundaries

Do not output a generalized function type until the effect slot for that
function body is known.

Normal function boundary:

1. check the body
2. drain static-dispatch constraints that are ready in the current environment
3. finalize the function body's effect slot
4. construct or unify the final function type as pure or effectful
5. generalize

Recursive function group boundary:

1. collect effect slots and directed edges while checking group members
2. process deferred type and dispatch constraints at the group root
3. condense strongly connected effect groups
4. finalize effectfulness for the whole group
5. construct or unify final function types
6. generalize the group

Top-level value and expect boundaries:

1. check the body
2. drain ready dispatch constraints
3. finalize that slot
4. emit `effectful_top_level` or `effectful_expect` if the slot is effectful

The implementation may keep `fn_unbound` internally while a function body is
being checked, but checked output must not rely on unresolved effect kind.

Success criteria:

- Pure annotations reject delayed effectful dispatch.
- Effectful annotations accept delayed effectful dispatch.
- No checked module output treats an effectful function as pure.
- Existing recursion tests keep passing.

### Phase 5: Replace Expression `does_fx` With A Small Check Result

After effect slots are stable, change `checkExpr` and block checking to return a
small transient result rather than only `bool`.

Proposed shape:

```zig
const ExprCheckResult = struct {
    runtime_dep: bool,
    root_state: RootState,
};
```

Effectfulness is recorded through the active effect slot, not through this
result. The expression result only needs the information required by the parent
expression and by local binding summaries.

Runtime dependency rules:

- lambda arguments are runtime-dependent
- match-bound values are runtime-dependent
- loop-bound values are runtime-dependent
- mutable `var` values and reassignments are runtime-dependent
- immutable local bindings use the stored result of their RHS
- top-level checked values are compile-time-known unless their checked result
  says otherwise
- imported checked values are compile-time-known unless their checked result
  says otherwise
- parent expressions OR child runtime dependency

Store summaries only for bindings that can be looked up later.

Success criteria:

- No full per-expression summary table is introduced.
- Runtime dependency through chains of local immutable bindings is detected.
- Runtime dependency through function arguments, matches, loops, and mutable
  variables is detected.

### Phase 6: Add Maximal Root Candidate Selection

Add a root-candidate stack managed by expression frames.

Each `checkExpr` frame records:

```text
candidate_start = root_candidates.len
```

When the expression finishes:

- if it is compile-time-known and effect-free, discard candidates added since
  `candidate_start` and append the current expression as the root
- if it is not eligible, keep child candidates
- if effectfulness is delayed, store a tentative parent candidate that owns the
  child-candidate range

The delayed case is required for this shape:

```roc
foo = |x| {
    sprite = [1, 2, 3]
    x.tick!()
    sprite
}
```

If `tick!` resolves effectful, the block is not a root but `sprite` remains a
root. If the delayed call resolves pure and the block has no runtime dependency,
the block may replace its children.

Success criteria:

- Parent roots replace child roots.
- Effectful or runtime-dependent parents preserve eligible children.
- Delayed effect parents resolve correctly after dispatch finalization.
- No expression-shape special cases are needed.

### Phase 7: Compile-Time Evaluation And Stored Consts

Update compile-time evaluation to consume final roots from the checker.

Required behavior:

- evaluate all checked top-level expressions that are eligible for compile-time
  evaluation
- run `crash`, `dbg`, and `expect` during compile-time evaluation
- report compile-time `crash`, `dbg`, and failed `expect` output in `roc check`
- store only reachable evaluated values in checked module data and static data
- do not store unreachable top-level values merely because they were evaluated
  for diagnostics

Static data requirements:

- top-level constant records should be stored statically when reachable
- inline expressions equivalent to top-level constants should produce the same
  stored static data after root selection
- repeated static lists should share the same bytes where value identity allows
  sharing
- records that contain static lists should point at the shared static list data

Success criteria:

- Named top-level constants and equivalent inline expressions produce the same
  static data for Rocci Bird sprite and animation shapes.
- Unreachable top-level `crash`, `dbg`, and `expect` still run in `roc check`.
- Unreachable successfully evaluated constants do not force binary data output.

### Phase 8: Remove The Old Hoisting Machinery

Delete the current hoist selection system after the new root selection passes
tests.

Remove or replace:

- observable-effect markers used as root blockers
- later-hoist blocking rules
- leaf/root pruning rules
- duplicate root-selection walks
- code paths that infer root eligibility from source expression shape instead
  of the checker result

Success criteria:

- No remaining code treats `dbg`, `expect`, `crash`, `return`, `break`, or loop
  syntax as a reason to reject a compile-time root.
- Root selection has one owner in checking.
- Checked module output consumes only final selected roots.

### Phase 9: Rocci Bird Verification

Use Rocci Bird as the integration check after focused compiler tests pass.

Steps:

1. Build the local compiler in debug mode.
2. Format `/home/rtfeldman/code/roc-wasm4/examples/rocci-bird.roc`.
3. Build Rocci Bird with:

   ```sh
   ./zig-out/bin/roc build /home/rtfeldman/code/roc-wasm4/examples/rocci-bird.roc --opt=size --no-cache
   ```

4. Disassemble the wasm.
5. Confirm sprite sheets and animation records are no longer rebuilt inline in
   the update body.
6. Confirm top-level and inline animation-cell expressions produce the same
   static data.
7. Record:
   - total wasm bytes
   - code section bytes
   - data section bytes
   - largest function bodies
8. Serve the optimized and dev builds on the existing local WASM-4 ports when
   requested.

Success criteria:

- Rocci Bird builds with `--opt=size`.
- The game still runs.
- Sprite list data is stored statically and shared.
- Equivalent inline/top-level constants no longer change wasm size materially.
- The optimized wasm size moves toward the Rust comparison for reasons visible
  in the disassembly.

## Test Matrix

### Effect Soundness

- direct effectful call in top-level value
- delayed static-dispatch effectful call in top-level value
- direct effectful call in pure function annotation
- delayed static-dispatch effectful call in pure function annotation
- effectful function annotation with direct effectful call
- effectful function annotation with delayed effectful dispatch
- pure where-clause with pure implementation
- pure where-clause with effectful implementation
- effectful where-clause with effectful implementation
- effectful where-clause called from top-level value
- direct effectful call in `expect`
- delayed dispatch effectful call in `expect`
- `dbg` around pure value
- `dbg` around effectful call
- `crash` in otherwise pure compile-time value
- `expect` in otherwise pure compile-time value
- direct call through local function alias
- direct call through imported function alias
- static-dispatch method imported from another module
- higher-order call with pure function parameter
- higher-order call with effectful function parameter
- closure creation with effectful body is not itself effectful
- closure call with effectful body is effectful
- self-recursive pure function
- self-recursive effectful function
- mutually recursive group where one member is effectful
- mutually recursive group where all members are pure

### Const Root Selection

- top-level list literal becomes one root
- record containing list literal becomes one root and replaces the child
- inline top-level-equivalent animation cell list equals named top-level list
- runtime-dependent parent preserves static child
- effectful parent preserves static child
- delayed-effect parent resolves pure and replaces child
- delayed-effect parent resolves effectful and preserves child
- lambda argument dependency blocks parent root
- immutable local binding depending on lambda argument blocks downstream root
- immutable local binding independent of lambda argument remains eligible
- mutable `var` blocks parent root
- reassignment blocks parent root
- match-bound value blocks parent root
- loop-bound value blocks parent root
- top-level checked value lookup remains compile-time-known
- imported checked value lookup remains compile-time-known
- `if` with runtime condition preserves pure branch roots
- `match` with runtime condition preserves pure branch roots
- `crash` runs during compile-time evaluation
- `dbg` runs during compile-time evaluation
- `expect` runs during compile-time evaluation
- unreachable top-level `crash` still reports during `roc check`
- unreachable top-level successful constant is not stored in static data

### Static Data

- static list bytes are emitted once when shared
- static record points at static list bytes
- repeated sprite sheets share bytes
- inline animation cells and named animation cells emit equivalent data
- removed root children do not emit duplicate data
- effectful parent does not prevent independent static child data

## Done Checklist

- [ ] Regression tests reproduce the static-dispatch effect soundness bug.
- [ ] Effect slots exist for function bodies, top-level values, expects, and
      root candidates.
- [ ] Direct effectful calls mark active slots.
- [ ] Calls to known local functions add directed effect edges.
- [ ] Static-dispatch function variables record effect watchers.
- [ ] Dispatch resolution marks watcher slots when the selected method is
      effectful.
- [ ] Pure/effectful where-clause implementation checking is correct.
- [ ] Function effect kinds are finalized before checked function output.
- [ ] Top-level effect errors use finalized effect slots.
- [ ] `expect` effect errors use finalized effect slots.
- [ ] `checkExpr` returns a transient runtime-dependency/root summary.
- [ ] Binding summaries are stored only where later lookups need them.
- [ ] Root-candidate stack performs parent-child replacement.
- [ ] Delayed-effect root candidates finalize correctly.
- [ ] Old observable-effect hoist blocking is removed.
- [ ] Compile-time evaluation runs `crash`, `dbg`, and `expect` for eligible
      top-level expressions and roots.
- [ ] Checked module output stores only reachable evaluated values.
- [ ] Rocci Bird inline and named static data forms compile equivalently.
- [ ] Rocci Bird `--opt=size` builds and the wasm disassembly confirms sprite
      data is static rather than rebuilt in `update`.
