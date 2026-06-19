# Issue 9704: Lambda Solved unification invariant in platform render root

## Reproduction

The local regression test is `issue 9704: unannotated platform render root lowers to LIR` in `src/compile/test/issue_9704_test.zig`.

It uses a small custom platform so `render!` is a real platform-required root, matching the issue's important shape:

- `app [Model, program]`.
- `Model : { pins : List(U64) }`.
- `program = { init!, render! }`.
- `render!` has no source annotation.
- The platform requires `render! : model, {} => Try(model, [Exit(I64), ..])`.
- `render!` returns `Ok({ pins: f!(model.pins, model.pins, model.pins) })`.

Before the fix, the local reduced platform tripped a Lambda Solved tag-union unification invariant. The original issue with the roc-ray platform tripped:

```text
postcheck invariant violated: named type failed Lambda Solved unification
```

Both failures occur in `src/postcheck/lambda_solved/solve.zig` while reconciling checked root-required types with inferred body types.

## Background

Lambda Solved consumes Monotype Lifted IR. It expects all checked types that reach it to be internally consistent. Its unifier is intentionally invariant-based, not diagnostic-producing:

```zig
.named => |left_named| switch (right) {
    .named => ...
    else => Common.invariant("named type failed Lambda Solved unification"),
}
```

By the time this stage runs, user-facing type errors should already have been reported.

## Root Cause

Checked publication already produces an explicit substituted platform-required type for `render!`, but post-check lowering could still materialize a named type whose backing was another same-definition named wrapper. In the repro this became:

- outer `Builtin.Try(model_a, err)` with a backing of
- inner `Builtin.Try(model_b, err)`, whose backing was the actual tag union.

Lambda Solved requires named backings to be structural. That invariant is correct: when it unifies two `Try` values it recursively unifies their backing types, and a `tag_union` backing cannot be unified with a `named` backing.

The important detail is that the two `Try` wrappers had the same named definition but different temporary argument roots for `Model`. Those roots are reconciled by normal type unification; they should not prevent backing normalization from walking through same-definition named wrappers to the structural tag union.

## Fix

The fix keeps the Lambda Solved invariant and normalizes earlier/lower-boundary data instead:

1. Platform-required procedure bodies are lowered against the explicit platform relation function type, rather than the app binding's unconstrained source scheme.
2. Monotype direct nominal backing lowering walks through aliases and same-definition named wrappers to the structural backing.
3. Monotype instantiation materialization uses the same structural backing normalization.
4. Lambda Solved's type cloner also normalizes monotype named backings at the stage boundary, so Lambda Solved receives structural backing types.

This matches the borrow-inference design direction: later stages consume explicit type and ownership facts from earlier stages; Lambda Solved does not recover platform relation semantics or weaken its invariants.
