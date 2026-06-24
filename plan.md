# Effect Propagation And Const Root Plan

## Goal

Replace the current hoisting/effect logic with one checked-stage system that:

- rejects effectful calls in pure top-level values, pure functions, and
  `expect` bodies
- handles delayed static-dispatch effectfulness correctly
- evaluates every eligible top-level value and top-level-equivalent expression
  at compile time
- runs `crash`, `dbg`, and `expect` during `roc check` whenever their
  containing expression is otherwise compile-time evaluable
- selects maximal compile-time roots during the existing checker expression
  traversal
- stores only reachable evaluated values in checked module data and target
  static data
- removes the old hoisting pruning rules instead of layering new behavior on
  top of them

The motivating integration case is Rocci Bird: top-level sprite sheets,
animation records, and equivalent inline expressions should be evaluated at
compile time, emitted as static data when reachable, shared where possible, and
not rebuilt in the WASM-4 `update` body.

## Non-Negotiable Invariants

- Checking is the final user-facing stage for effect errors, compile-time
  evaluation errors, static-dispatch errors, and compile-time diagnostics.
- Every compiler stage after checking consumes explicit checked data. It must
  not recover, guess, reconstruct, approximate, or best-effort missing
  information.
- Static-dispatch effectfulness must be explicit checker output. No stage may
  infer it from method names, `!` spelling, source syntax, or unresolved
  function types.
- `crash`, `dbg`, and `expect` are not effectful calls. They are compile-time
  observable constructs, and they must not disqualify an otherwise eligible
  compile-time root.
- Effect dependencies are directed. A caller depending on a callee must not be
  represented as equality.
- Union-find is not the effect solver. Strongly connected recursive groups may
  be condensed, but unrelated caller/callee pairs must remain directed.
- Function creation is not effectful merely because the function body contains
  effects. Calling the function propagates the body's effectfulness.
- Effectful functions do not run during compile-time evaluation. Expressions
  containing effectful calls cannot become compile-time roots.
- Compile-time root selection keeps maximal eligible roots. If a parent
  expression is selected, child roots inside it are removed. If a parent is
  rejected, eligible child roots survive.
- There are no expression-shape pruning rules. Numbers, strings, empty lists,
  records, `return`, `break`, loops, and other syntax follow the same
  dependency/effect/root rules as every other expression.
- Ordinary nested expressions use stack-local summaries. Store only data that
  must survive after the current expression returns.
- Checked module output must not contain unresolved effect kinds or unresolved
  compile-time root eligibility.
- Backend code generation, LIR, and static-data emission must not rescan source
  expressions to rediscover effectfulness or root eligibility.
- If a reachable evaluated value cannot yet be represented as static data, that
  limitation must be explicit in the checked/static-data contract. It must not
  be hidden behind backend fallback behavior.

## Efficiency Constraints

- Do not add a permanent summary table for every expression.
- Use the existing `checkExpr` traversal to compute runtime dependency and root
  candidate summaries.
- Store summaries for local bindings only when later lookups can consume them.
- Store effect slots only for semantic boundaries: function bodies, top-level
  value right-hand sides, `expect` bodies, and delayed-effect root candidates.
- Store dispatch watchers keyed by dispatch function variable or equivalent
  explicit constraint id, not by rescanning expressions.
- Final effect solving should be linear in effect slots plus effect edges plus
  dispatch watchers.
- Recursive effect solving may use SCC condensation. It must not union ordinary
  directed caller/callee dependencies.

## Phase 1: Lock Down Effect Soundness Tests

Add focused checker tests before replacing the implementation. These tests must
fail on the current broken behavior for the right reason and pass after the
effect-slot implementation lands.

### Required Effect Tests

- direct effectful call in a top-level value reports `effectful_top_level`
- delayed static-dispatch method call in a top-level value reports
  `effectful_top_level`
- delayed type-method call in a top-level value reports `effectful_top_level`
- effectful binop dispatch in a top-level value reports `effectful_top_level`
- effectful unary dispatch in a top-level value reports `effectful_top_level`
- interpolation dispatch propagates effectfulness
- synthetic iterator dispatch propagates effectfulness
- imported nominal method dispatch propagates effectfulness
- direct effectful call in a pure function annotation is rejected
- delayed effectful method call in a pure function annotation is rejected
- direct effectful call in an effectful function annotation is accepted
- delayed effectful method call in an effectful function annotation is accepted
- pure where-clause accepts a pure implementation method
- pure where-clause rejects an effectful implementation method
- effectful where-clause accepts an effectful implementation method
- effectful where-clause makes the wrapper/caller effectful
- direct effectful call in `expect` reports `effectful_expect`
- delayed effectful dispatch in `expect` reports `effectful_expect`
- direct call through a local function alias preserves effectfulness
- direct call through an imported function alias preserves effectfulness
- higher-order call through a pure function parameter stays pure
- higher-order call through an effectful function parameter is effectful
- closure creation with an effectful body is not itself effectful
- calling a closure with an effectful body is effectful
- self-recursive pure function stays pure
- self-recursive effectful function is effectful
- mutually recursive group with one effectful member propagates through the
  recursive group
- mutually recursive group where every member is pure stays pure
- `dbg` around a pure value is not effectful
- `dbg` around an effectful call reports through the contained effectful call
- `crash` in an otherwise pure value is not effectful
- `expect` in an otherwise pure value is not effectful

### Soundness Bug Shape To Reproduce

The checker must reject this:

```roc
package [] {}

Eff := [Eff].{
    tick! : Eff => U64
    tick! = |_| 1
}

top = (Eff.Eff).tick!()
```

Expected diagnostic: `effectful_top_level`.

The checker must also reject this:

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

Expected diagnostic: pure where-clause implementation mismatch.

### Phase 1 Success Criteria

- Current direct-call effect tests still pass.
- Known delayed-dispatch cases fail before the implementation and pass after it.
- Tests distinguish direct effect calls from delayed static-dispatch effect
  calls.
- Tests cover local and imported functions/methods.

## Phase 2: Add Sparse Effect Slots

Introduce checker-owned effect slots for places where effectfulness is part of
the checked result.

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

Implementation steps:

1. Add effect-slot storage and an active effect-slot stack to `Check.zig`.
2. Create a slot before checking each function/lambda body.
3. Create a slot before checking each top-level value right-hand side.
4. Create a slot before checking each `expect` body.
5. Mark the active slot for direct calls to resolved effectful functions.
6. Add a directed edge from the active slot to the callee body slot for calls to
   known local functions whose slot is known.
7. Represent calls through function parameters by the checked effect kind of
   the parameter's function type.
8. Do not treat closure creation as an effect. Only calls propagate effects.
9. Keep existing direct-call diagnostics passing while slots are introduced.

Phase 2 must not add a slot for every expression.

### Phase 2 Success Criteria

- Existing direct-call effect behavior is unchanged.
- Function creation is pure even when the function body is effectful.
- Calling an effectful function body propagates effectfulness to the caller.
- Recursive and mutually recursive functions have directed edges and solve
  without recursive implementation calls.
- No checked function type is emitted before its effect slot can be finalized.

## Phase 3: Connect Static Dispatch To Effect Slots

Static dispatch is the main current soundness gap. It must feed the same effect
slot graph as direct calls.

Proposed data:

```zig
const DispatchEffectWatch = struct {
    fn_var: Var,
    slot: EffectSlotId,
};
```

Implementation steps:

1. Whenever checking creates a static-dispatch function variable or equivalent
   dispatch constraint id, record that the active effect slot watches it.
2. Cover all dispatch sources:
   - receiver method calls
   - type-method calls
   - binop dispatch
   - unary dispatch
   - interpolation dispatch
   - synthetic iterator dispatch from `for`
   - where-clause dispatch copied during instantiation
   - imported nominal method dispatch
3. When static-dispatch resolution checks the selected method against the
   dispatch function variable, preserve and expose the selected method's effect
   kind.
4. If the selected method is effectful, mark all watching slots effectful or
   add explicit edges to the selected method slot.
5. If the selected method is pure, leave watchers unmarked.
6. If the selected method is unresolved at a boundary where checked output
   would otherwise be produced, finalize by reporting the existing dispatch
   error rather than guessing effectfulness.
7. Preserve effect kind through where-clause checking:
   - `where [a.f : a -> b]` requires a pure implementation
   - `where [a.f : a => b]` requires an effectful implementation
   - a call through an effectful where-clause method makes the caller effectful

### Phase 3 Success Criteria

- Delayed method calls report top-level and `expect` effects.
- Pure where-clause methods cannot be satisfied by effectful implementations.
- Effectful where-clause methods propagate to callers.
- Imported method dispatch uses imported checked effect summaries.
- Static-dispatch diagnostics keep the same source regions.

## Phase 4: Finalize Effects At Checked Boundaries

Do not output a generalized function type, checked top-level value, checked
`expect`, or checked module until relevant effect slots are finalized.

Normal function boundary:

1. Check the body.
2. Drain type and static-dispatch constraints that are ready in the current
   environment.
3. Finalize the function body's effect slot.
4. Construct or unify the final function type as pure or effectful.
5. Validate the function's annotation against the finalized effect kind.
6. Generalize.

Recursive function group boundary:

1. Collect slots and directed edges while checking group members.
2. Process deferred type and dispatch constraints at the group root.
3. Condense strongly connected effect groups.
4. Propagate effectfulness through the directed graph.
5. Construct or unify final function types for every group member.
6. Validate annotations.
7. Generalize the group.

Top-level value boundary:

1. Check the right-hand side.
2. Drain ready dispatch constraints.
3. Finalize the value slot.
4. Emit `effectful_top_level` if the slot is effectful.
5. Continue type/error recovery only through existing checking error paths.

`expect` boundary:

1. Check the body.
2. Drain ready dispatch constraints.
3. Finalize the expect slot.
4. Emit `effectful_expect` if the slot is effectful.

Module boundary:

1. Finish all value/function/expect boundaries.
2. Finish all static-dispatch constraints or report their diagnostics.
3. Finalize imported/exported effect summaries.
4. Output checked module data with no unresolved effect kinds.

### Phase 4 Success Criteria

- Pure annotations reject delayed effectful dispatch.
- Effectful annotations accept delayed effectful dispatch.
- Top-level and `expect` errors use finalized effect slots.
- Existing recursion tests still pass.
- Checked module output contains explicit effect summaries for imported use.

## Phase 5: Replace `does_fx` With A Small Check Result

After effect slots are stable, stop using expression-level `bool does_fx` as
the effect propagation mechanism. Effects flow through slots; expression
checking returns only data needed by parent expressions and local binding
lookups.

Proposed shape:

```zig
const ExprCheckResult = struct {
    runtime_dep: RuntimeDep,
    root: RootCheckState,
};
```

`RuntimeDep` should distinguish only the states needed for root selection:

- known compile-time
- runtime-dependent
- poisoned by an existing check error, if needed to avoid duplicate diagnostics

Implementation steps:

1. Change expression and block checking to return `ExprCheckResult`.
2. Compute runtime dependency bottom-up from children.
3. Store RHS summaries for immutable local definitions that can be looked up
   later.
4. Mark lambda parameters, match-bound values, loop-bound values, mutable
   variables, and reassigned variables as runtime-dependent.
5. Treat top-level checked value lookups as compile-time-known unless their
   checked summary says otherwise.
6. Treat imported checked value lookups as compile-time-known unless their
   imported checked summary says otherwise.
7. Keep effect tracking out of `ExprCheckResult` except for references to
   delayed-effect root candidates.

### Phase 5 Success Criteria

- No permanent per-expression summary table is introduced.
- Runtime dependency through chains of immutable local bindings is detected.
- Runtime dependency through lambda args, matches, loops, and mutation is
  detected.
- Effect propagation no longer depends on the old expression bool.

## Phase 6: Select Maximal Compile-Time Roots

Add a root-candidate stack managed by expression frames.

Each expression frame records:

```text
candidate_start = root_candidates.len
```

When an expression finishes:

- if it is compile-time-known and effect-free, discard candidates added since
  `candidate_start` and append the current expression as the root
- if it is runtime-dependent or effectful, keep eligible child candidates
- if its effectfulness is delayed, store a tentative parent candidate that owns
  the child-candidate range

The delayed case matters for this shape:

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

Implementation steps:

1. Add root-candidate stack storage to checking.
2. Add expression-frame push/pop helpers.
3. Replace child candidates with parent candidates only through the frame rule.
4. Add tentative candidates tied to effect slots for delayed-dispatch cases.
5. Finalize tentative candidates after effect finalization.
6. Preserve child candidates when a parent resolves effectful or
   runtime-dependent.
7. Remove all leaf-shape and observable-effect root pruning.

### Phase 6 Success Criteria

- Parent roots replace child roots.
- Effectful or runtime-dependent parents preserve eligible children.
- Delayed-effect parents resolve correctly after dispatch finalization.
- Named and equivalent inline constants choose equivalent roots.
- There are no special cases for strings, numbers, empty lists, records,
  `return`, `break`, or loop syntax.

## Phase 7: Compile-Time Evaluation And Static Data

Update compile-time evaluation to consume final roots from the checker.

Required behavior:

- evaluate every checked top-level expression that is eligible for compile-time
  evaluation, including unreachable top-level values
- evaluate every selected compile-time root
- run `crash`, `dbg`, and `expect` during compile-time evaluation
- report compile-time `crash`, `dbg`, and failed `expect` output during
  `roc check`
- store only reachable evaluated values in checked module data and target
  static data
- do not store unreachable top-level values merely because they were evaluated
  for diagnostics

Static data requirements:

- top-level constant records are stored statically when reachable
- inline expressions equivalent to top-level constants produce the same static
  data after root selection
- repeated static lists share bytes where value identity allows sharing
- records that contain static lists point at the shared static list data
- removed child roots do not emit duplicate static data

Implementation steps:

1. Define the checked output format for evaluated roots and top-level
   evaluation diagnostics.
2. Define the explicit static-storable value categories.
3. Teach compile-time evaluation to emit value data separately from diagnostic
   output.
4. Teach checked module output to retain only reachable stored values.
5. Teach static-data emission to share repeated list bytes and reference them
   from static records.
6. Make non-storable reachable evaluated values explicit as an implementation
   gap or checked representation case, not a backend guess.

### Phase 7 Success Criteria

- Unreachable top-level `crash`, `dbg`, and failed `expect` still report during
  `roc check`.
- Unreachable successfully evaluated constants do not force binary data output.
- Named top-level constants and equivalent inline expressions produce the same
  static data for Rocci Bird sprite and animation shapes.
- Static list bytes are emitted once when shared.
- Records point at static list data instead of reconstructing lists at runtime.

## Phase 8: Remove The Old Hoisting Machinery

Delete the current hoist selection system after the new root selection passes
focused tests.

Remove or replace:

- observable-effect markers used as root blockers
- later-hoist blocking rules
- leaf/root pruning rules
- duplicate root-selection walks
- source-shape checks for root eligibility
- post-check repair or cleanup paths that exist only because roots were not
  selected correctly during checking

### Phase 8 Success Criteria

- Root selection has one owner in checking.
- `dbg`, `expect`, `crash`, `return`, `break`, loop syntax, and leaf shapes do
  not appear as root blockers.
- Checked module output consumes only final selected roots.
- Post-check stages do not infer root eligibility from CIR/source shape.

## Phase 9: Focused Test Matrix For Const Roots

Add focused tests for these root-selection and evaluation shapes.

### Runtime Dependency

- top-level list literal becomes one root
- record containing a list literal becomes one root and replaces the child
- immutable local independent of a lambda argument remains eligible
- immutable local depending on a lambda argument is runtime-dependent
- chained immutable locals preserve runtime dependency correctly
- lambda argument dependency blocks the parent root
- match-bound value dependency blocks the parent root
- loop-bound value dependency blocks the parent root
- mutable `var` blocks the parent root
- reassignment blocks the parent root
- top-level checked value lookup remains compile-time-known
- imported checked value lookup remains compile-time-known
- `if` with runtime condition preserves pure branch child roots
- `match` with runtime scrutinee preserves pure branch child roots

### Effect Interaction

- effectful parent preserves independent static child root
- delayed-effect parent resolving pure replaces child roots
- delayed-effect parent resolving effectful preserves child roots
- function value with effectful body can be a compile-time value, but calling it
  is effectful
- direct effectful call blocks the containing root
- static-dispatch effectful call blocks the containing root

### Compile-Time Observables

- `crash` runs during compile-time evaluation
- `dbg` runs during compile-time evaluation
- `expect` runs during compile-time evaluation
- unreachable top-level `crash` reports during `roc check`
- unreachable top-level `dbg` reports during `roc check`
- unreachable top-level failed `expect` reports during `roc check`
- `crash`, `dbg`, and `expect` do not prevent an otherwise eligible parent
  from being selected as the maximal root

### Static Data

- static list bytes are emitted once when shared
- static record points at static list bytes
- repeated sprite sheets share bytes
- inline animation cells and named animation cells emit equivalent data
- child roots removed by a parent root do not emit duplicate data
- effectful parent does not prevent independent static child data

## Phase 10: Rocci Bird Verification

Use Rocci Bird as the integration check after focused compiler tests pass.

Steps:

1. Build the local compiler in debug mode.
2. Format `/home/rtfeldman/code/roc-wasm4/examples/rocci-bird.roc`.
3. Build Rocci Bird with Roc optimization for size.
4. Run the WASM post-processing expected for WASM-4 size builds.
5. Disassemble the final wasm.
6. Confirm sprite sheets and animation records are not rebuilt inline in the
   `update` body.
7. Confirm top-level and inline animation-cell expressions produce equivalent
   static data.
8. Confirm repeated sprite/list bytes are shared where applicable.
9. Record:
   - total wasm bytes
   - code section bytes
   - data section bytes
   - largest function bodies
   - largest remaining allocation sites in normal gameplay
10. Serve optimized and dev builds on the existing local WASM-4 ports when
    requested.

### Phase 10 Success Criteria

- Rocci Bird builds with `--opt=size`.
- Rocci Bird still runs.
- Sprite sheet list data is stored statically and shared.
- Animation records are static where reachable.
- Equivalent inline/top-level constants no longer materially change wasm size.
- The optimized wasm size moves toward the Rust comparison for reasons visible
  in the disassembly.

## Full Checklist

### Effect Tests

- [ ] Direct top-level effectful call test exists.
- [ ] Static-dispatch top-level method effect test exists.
- [ ] Type-method top-level effect test exists.
- [ ] Binop dispatch effect test exists.
- [ ] Unary dispatch effect test exists.
- [ ] Interpolation dispatch effect test exists.
- [ ] Iterator dispatch effect test exists.
- [ ] Imported nominal method effect test exists.
- [ ] Pure annotation rejects direct effect test exists.
- [ ] Pure annotation rejects delayed dispatch effect test exists.
- [ ] Effectful annotation accepts direct effect test exists.
- [ ] Effectful annotation accepts delayed dispatch effect test exists.
- [ ] Pure where-clause accepts pure implementation test exists.
- [ ] Pure where-clause rejects effectful implementation test exists.
- [ ] Effectful where-clause accepts effectful implementation test exists.
- [ ] Effectful where-clause propagates to caller test exists.
- [ ] Direct effectful `expect` test exists.
- [ ] Delayed dispatch effectful `expect` test exists.
- [ ] Function alias effect tests exist.
- [ ] Higher-order effect tests exist.
- [ ] Closure creation/call effect tests exist.
- [ ] Recursive effect tests exist.
- [ ] `crash`/`dbg`/`expect` non-effect tests exist.

### Effect Implementation

- [ ] Effect slots exist for function bodies.
- [ ] Effect slots exist for top-level value right-hand sides.
- [ ] Effect slots exist for `expect` bodies.
- [ ] Effect slots exist for delayed-effect root candidates.
- [ ] Active effect-slot stack exists.
- [ ] Direct effectful calls mark active slots.
- [ ] Calls to known local functions add directed effect edges.
- [ ] Calls through function parameters use checked function effect kinds.
- [ ] Closure creation does not mark the active slot effectful.
- [ ] Dispatch function variables record active-slot watchers.
- [ ] Dispatch watchers cover receiver method calls.
- [ ] Dispatch watchers cover type-method calls.
- [ ] Dispatch watchers cover binop and unary dispatch.
- [ ] Dispatch watchers cover interpolation dispatch.
- [ ] Dispatch watchers cover iterator dispatch.
- [ ] Dispatch watchers cover where-clause dispatch.
- [ ] Dispatch watchers cover imported nominal method dispatch.
- [ ] Dispatch resolution marks watcher slots when selected methods are
      effectful.
- [ ] Pure where-clause implementation checking rejects effectful methods.
- [ ] Effectful where-clause calls propagate to callers.
- [ ] Function effect kinds finalize before checked function output.
- [ ] Recursive effect groups solve with directed SCC propagation.
- [ ] Top-level effect errors use finalized effect slots.
- [ ] `expect` effect errors use finalized effect slots.
- [ ] Checked modules export/import explicit effect summaries.

### Root Selection

- [ ] `checkExpr` returns a transient runtime-dependency/root summary.
- [ ] Expression effect propagation no longer depends on old `does_fx`.
- [ ] Immutable local binding summaries are stored only when later lookups need
      them.
- [ ] Runtime dependency from lambda arguments is detected.
- [ ] Runtime dependency from match-bound values is detected.
- [ ] Runtime dependency from loop-bound values is detected.
- [ ] Runtime dependency from mutable variables and reassignment is detected.
- [ ] Top-level checked value lookups are compile-time-known.
- [ ] Imported checked value lookups are compile-time-known.
- [ ] Root-candidate stack exists.
- [ ] Parent root selection removes child roots in the same frame.
- [ ] Rejected parent roots preserve eligible child roots.
- [ ] Delayed-effect parent roots finalize correctly.
- [ ] Leaf/root pruning rules are removed.
- [ ] Observable-effect root blockers are removed.
- [ ] `return`/`break`/loop syntax root blockers are removed.

### Compile-Time Evaluation And Static Data

- [ ] Compile-time evaluation consumes final selected roots from checking.
- [ ] Eligible top-level expressions are evaluated even when unreachable.
- [ ] Eligible selected roots are evaluated.
- [ ] `crash` runs during compile-time evaluation.
- [ ] `dbg` runs during compile-time evaluation.
- [ ] `expect` runs during compile-time evaluation.
- [ ] `roc check` reports compile-time `crash`, `dbg`, and failed `expect`
      diagnostics.
- [ ] Checked module output stores only reachable evaluated values.
- [ ] Static-storable value categories are explicit.
- [ ] Static list bytes are shared.
- [ ] Static records point at static list data.
- [ ] Equivalent named and inline constants produce equivalent static data.
- [ ] Removed child roots do not emit duplicate static data.

### Cleanup And Integration

- [ ] Old hoist selection machinery is deleted or fully bypassed.
- [ ] No post-check stage infers root eligibility from source/CIR shape.
- [ ] Focused checker tests pass.
- [ ] Compile-time evaluation/static data tests pass.
- [ ] Existing relevant compiler tests pass.
- [ ] Rocci Bird formats successfully.
- [ ] Rocci Bird builds with `--opt=size`.
- [ ] Rocci Bird disassembly confirms sprite/list data is static.
- [ ] Rocci Bird disassembly confirms animation records are not rebuilt in
      `update`.
- [ ] Rocci Bird optimized wasm size is recorded.
- [ ] Final pass over this plan confirms every checklist item is genuinely
      complete before marking the goal complete.
