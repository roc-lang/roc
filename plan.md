# MIR Local-Def Graph Plan

## Goal

The long-term target is:

- one canonical MIR local-definition graph
- one definition per `LocalId`
- all control-flow merges represented explicitly by join parameters
- no lowering-time reconstruction of MIR defs
- no path-sensitive mutable value-def environment in `MirToLir`
- `MirToLir` consuming MIR facts and MIR analyses, not rediscovering them

This is the strongest long-term design because it gives:

- one source of truth
- correctness by construction
- explicit CFG/valueflow joins
- simpler and more reliable MIR -> LIR lowering
- cheaper and more direct callable/provenance/layout analyses

## Non-Negotiable Rules

- Do not introduce workarounds, compatibility shims, transitional artifacts, or silent fallbacks.
- Delete obsolete code completely once the replacement exists.
- If some feature is intentionally deferred, leave a loud debug panic.
- In debug builds, invariant failures must panic immediately.
- Backends must continue to follow only explicit LIR RC statements.

## Core Architectural Decision

The ideal end state is not:

- one flat ad hoc `ValueDefMap`
- plus one path-sensitive mutable `current_value_defs`

The ideal end state is:

- MIR itself owns the definition graph for locals
- multiple analyses run over that one graph
- `MirToLir` asks those analyses questions

So the real design is:

- one source of truth
- multiple consumers

not:

- multiple parallel representations of local definitions

## The Fundamental MIR Invariant

Within a MIR body:

- every `LocalId` has exactly one definition
- parameters are definitions
- `captures_param` is a definition
- join parameters are definitions
- statement targets are definitions
- the same `LocalId` must never be assigned in two disjoint CFG locations

This means strongest-form MIR must behave like SSA with explicit join parameters.

Example:

Bad:

```text
if cond then
    x = "a"
else
    x = "b"
ret x
```

Required strongest-form MIR shape:

```text
if cond then
    x1 = "a"
    jump j(x1)
else
    x2 = "b"
    jump j(x2)

join j(x3):
    ret x3
```

The key property is:

- merges happen only at explicit `join` parameters
- there is no “same local defined in multiple branches” state left in strongest-form MIR

## What MIR Must Explicitly Store

MIR needs explicit local-definition ownership.

Conceptually:

```zig
pub const LocalDef = union(enum) {
    param: u16,
    captures_param,
    join_param: struct {
        join: JoinPointId,
        param_index: u16,
    },
    symbol: Symbol,
    literal: LiteralValue,
    ref: RefOp,
    lambda: LambdaId,
    closure: struct {
        lambda: LambdaId,
        captures: LocalSpan,
    },
    call: struct {
        callee: LocalId,
        args: LocalSpan,
        exact_lambda: ?LambdaId,
        exact_requires_hidden_capture: bool,
    },
    low_level: struct {
        op: LowLevel,
        args: LocalSpan,
    },
    list: LocalSpan,
    struct_: LocalSpan,
    tag: struct {
        name: Monotype.Name,
        args: LocalSpan,
    },
};
```

The exact storage shape can vary, but the semantic requirement is fixed:

- given a `LocalId`, MIR can answer its unique defining origin directly

That answer must come from MIR itself, not from reconstructing by scanning statements.

## Control-Flow Rules

To make the above correct by construction:

- branch-local statement targets must always be fresh locals
- loop-carried values must flow through join/jump params
- match branches must not reuse the same target local across alternatives
- borrow-scope bodies and remainders must also obey the same local-definition uniqueness rules

If lowering wants “the same conceptual variable” to continue across control flow, it must:

- create fresh branch locals
- pass them through a join
- use the join param after the merge

Never:

- reuse one `LocalId` in both branches

## Analyses Over The Local-Def Graph

Once MIR owns the local-def graph, the long-term ideal is to run separate analyses over it.

These analyses are different questions and should stay different.

### Callable Analysis

Questions answered:

- what exact callable does this local represent?
- direct lambda?
- closure value with hidden captures?
- join of multiple possible callables?
- no-return?

This replaces ad hoc callable resolution from:

- scanned value-def maps
- mutable lowering context
- recursive special cases in `MirToLir`

### Origin / Provenance Analysis

Questions answered:

- is this local fresh?
- alias of which parameter/local, with which projections?
- borrow of which parameter/local, with which projections?
- what happens through a join?
- what happens through low-level results with alias/borrow contracts?

This replaces path-sensitive provenance reconstruction during lowering.

### Return-Layout / Callable-Return Analysis

Questions answered:

- if a lambda returns a callable value, what callable runtime layout does that returned local have?
- are all returned callable values layout-compatible?

This must be implemented as an analysis over:

- returned locals
- local defs
- callable analysis

not by scanning ad hoc maps in `MirToLir`.

## What `MirToLir` Should Look Like In The End

`MirToLir` should not own MIR dataflow truth.

It should not have long-term concepts like:

- `collectValueDefs(...)`
- `current_value_defs`
- `pushMirValueDefBinding(...)`
- `popMirValueDefBinding(...)`

Instead, `MirToLir` should do three things:

1. translate MIR control flow to LIR control flow
2. map MIR locals to LIR locals
3. ask MIR/MIR-analyses for facts it needs

Examples:

- `mir_store.getLocalDef(local_id)`
- `analyses.callableOf(lambda_context, local_id)`
- `analyses.originOf(lambda_context, local_id)`
- `analyses.callableReturnLayout(lambda_id)`

That is the correct separation of responsibilities.

## Correct-By-Construction Strategy

The implementation must make it hard or impossible to violate the invariant.

### 1. MIR Local IDs Must Be Single-Assignment By Construction

Lowering into strongest-form MIR must allocate fresh locals for:

- every statement target
- every branch-local intermediate
- every loop-carried rebinding
- every destructuring result

If a value crosses a control-flow merge, it must become a join param.

### 2. Join Params Must Be The Only Merge Mechanism

There must be no implicit “same local in both branches” merge model left anywhere.

All merges must happen through:

- `jump`
- `join`
- join parameter locals

### 3. MIR Must Materialize LocalDef Entries At Construction Time

When lowering builds a MIR local, it must also record that local’s `LocalDef`.

This includes:

- lambda params
- captures param
- join params
- all assignment targets

That prevents any later pass from needing to rediscover defs by rescanning statements.

### 4. MIR Debug Verification Must Enforce The Invariant

Add a MIR verifier that checks:

- every local has exactly one definition
- every use refers to an existing definition
- defs dominate uses in the CFG model
- join param arity and jump arg arity match
- join param monotypes match incoming arg monotypes
- branch-local locals are not reused across disjoint CFG regions

This verifier should run in debug builds to catch construction bugs immediately.

## Detailed Implementation Plan

### Phase 1: Make Strongest MIR Truly Single-Assignment

Audit [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/mir/Lower.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/mir/Lower.zig) and any MIR builders so that:

- branch lowering never reuses a local id across branches
- loop lowering uses join params for loop-carried values
- mutable source variables are translated to fresh MIR locals plus explicit join/jump flow
- any remaining “same local assigned in multiple CFG locations” pattern is eliminated

This phase is required before the rest is clean.

### Phase 2: Add Explicit LocalDef Ownership To MIR

Extend [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/mir/MIR.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/mir/MIR.zig) / the MIR store so that:

- each `LocalId` has an explicit `LocalDef`
- params and join params are recorded as defs
- statement target defs are recorded as locals are created

Decide the exact storage form:

- `LocalId -> LocalDef`
- or equivalent per-body/per-lambda definition table

But the API must directly answer:

- “what defines this local?”

### Phase 3: Add MIR Verification

Create or extend a MIR debug verifier to check:

- unique definitions
- dominance/use correctness
- join/jump consistency
- local-def table consistency with statements and params

This verifier should fire before MIR -> LIR if MIR construction is wrong.

### Phase 4: Introduce Callable Analysis Over LocalDef

Replace callable-resolution reconstruction in [`/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/MirToLir.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/MirToLir.zig) with a dedicated MIR analysis that consumes:

- explicit `LocalDef`
- callable bindings on lambdas
- exact call metadata
- join structure

The analysis must answer:

- exact callable for a local
- closure captures requirement
- callable joins where needed

Any unsupported case must be a loud panic, not a fallback.

### Phase 5: Introduce Origin / Provenance Analysis Over LocalDef

Replace `resolveMirLocalOrigin(...)`-style reconstruction with a dedicated analysis that consumes:

- explicit `LocalDef`
- result-summary contracts
- low-level alias/borrow contracts
- join structure

The analysis must answer:

- fresh
- alias rooted in parameter/local
- borrow rooted in parameter/local
- join of compatible origins

If an origin cannot be represented yet, panic loudly.

### Phase 6: Move Callable Return Layout Resolution To Analyses

Replace `runtimeLambdaReturnLayout(...)`’s scanned flat-map logic with:

- collect returned locals
- query callable analysis for each returned local
- derive runtime layout from resolved callable values
- verify compatibility across return sites

This must rely on the same single source of truth as normal callable analysis.

### Phase 7: Rewrite `MirToLir` To Consume Analyses

Once phases 1-6 exist:

- remove `collectValueDefs(...)` from active lowering
- remove `current_value_defs` from lowering-time provenance/callable logic
- remove push/pop value-def binding machinery
- rewrite direct-call lowering to ask analyses for result semantics
- rewrite any remaining callable-resolution code to ask analyses

At that point `MirToLir` should only:

- lower CFG
- map locals
- emit LIR

### Phase 8: Delete Obsolete Reconstruction Code

After `MirToLir` no longer depends on reconstructed value-def maps:

- delete `collectValueDefs(...)`
- delete temporary flat callable-only maps if still unused
- delete transitional helper code added only for migration

No parallel “old path” should survive.

## Dataflow Rules For Specific MIR Constructs

### `assign_ref`

For provenance:

- `.local` preserves alias/borrow/fresh state through an alias
- `.field` introduces a borrow projection
- `.tag_payload` introduces a borrow projection
- `.nominal` introduces an alias projection
- `.discriminant` is always fresh

These semantics should come from the origin analysis, not be recomputed ad hoc in `MirToLir`.

### `assign_call`

For callable analysis:

- exact direct-call metadata should be represented in the local def
- callable-return facts should be derived from callee callable analysis plus callable summaries

For provenance:

- result provenance should be derived from callee result-summary contract instantiated against argument origins

### `join` / `jump`

These are the explicit merge points.

Long-term, analyses must understand:

- a join param’s value comes from the corresponding incoming jump arg
- if incoming facts differ incompatibly, that is either:
  - unsupported and a loud panic
  - or an explicit lattice join if we implement it

The important point is:

- merges belong in analysis over explicit join structure
- not in hidden mutable lowering state

## What Should Remain Mutable In `MirToLir`

Long-term, `MirToLir` should still keep mutable state for:

- MIR local -> LIR local mapping
- statement memoization
- active borrow region
- builder proc tables

That is normal lowering state.

What it should not keep as semantic truth:

- mutable reconstructed MIR def environments

## Testing And Validation Order

Implementation and debugging should happen in this order:

1. MIR verifier passes on fundamental control-flow cases
2. callable analysis works on direct lambdas and closures
3. provenance analysis works on alias/borrow/fresh fundamentals
4. MIR -> LIR direct call result semantics use the new analyses
5. eval tests pass for control flow, closures, loops, runtime, RC behavior
6. repl tests pass
7. builtins and list-heavy cases come after the fundamentals are solid

## End-State Summary

The absolute long-term ideal is:

- strongest-form MIR is explicit CFG plus explicit local-def graph
- locals are single-assignment
- joins are explicit phi-style join params
- MIR owns the only source of truth for local definitions
- callable/provenance/return-layout facts come from analyses over that graph
- `MirToLir` only translates; it does not infer or reconstruct MIR dataflow

That is the design to implement.
