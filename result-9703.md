# Issue 9703: ARC borrow certifier release of unbound local

## Reproduction

The local regression test is `issue 9703: recursive list effect with explicit root annotation lowers to LIR` in `src/compile/test/issue_9703_test.zig`.

It preserves the relevant shape from the issue:

- `Model` contains a refcounted `List(U64)` field.
- `render!` has an explicit `Model, {} => Try(Model, _)` annotation.
- `render!` constructs a fresh `Model` using `f!(model.pins, model.pins, model.pins)`.
- `f!` recursively matches a list and calls itself with aliased list arguments: `f!(rest, c, c)`.

Before the fix, running `zig build run-test-zig-module-compile -- --test-filter "issue 9703"` crashed in ARC certification:

```text
ARC borrow certifier: ... release of unbound local ...
```

## Background

The post-check pipeline is:

```text
checked modules -> Monotype -> Monotype Lifted -> Lambda Solved -> Solved LIR -> TRMC -> ScalarizeJoins -> ReachableProcs -> ARC insertion
```

The design contract is that backends do not infer ownership. ARC insertion is the only stage that computes ownership policy and emits explicit `incref` and `decref` statements. The ARC certifier then proves that every release targets a live refcounted value with an ownership unit.

The failing invariant is in `src/lir/arc_certify.zig`:

```zig
fn applyRelease(...) {
    ...
    const value = state.valueOf(local);
    if (value == no_value) {
        return self.fail("release of unbound local {d}", ...);
    }
    ...
}
```

This means ARC insertion emitted a release for a local that has no bound value along the certified control-flow path.

## Root Cause

The source shape repeatedly reuses the same refcounted list value through:

- record field extraction from `model.pins`,
- function arguments where the same list is passed multiple times,
- recursive tail-like calls after list pattern matching,
- construction of a refcounted result record.

The emitted LIR before ARC is structurally valid enough to reach ARC insertion, but ARC insertion emits a release for a local whose value is not bound on at least one path. That is a producer bug in ARC insertion, not a backend problem and not something the certifier should tolerate.

After reading `~/Downloads/borrow-inference.pdf`, this no longer looks like a missing high-level ownership architecture. Roc's ARC implementation already matches the paper's intended shape:

- `src/lir/arc_solve.zig` performs borrow inference over LIR bindings, aliases, proc signatures, return lenders, and ownership demands.
- `src/lir/arc.zig` reifies that solution into explicit retains, moves, and releases.
- `src/lir/arc_certify.zig` checks that the reified program obeys the ownership invariants.

That lines up with the paper's model: infer modes and lifetimes first, then reify variable occurrences as borrow, move, or `dup`, and insert drops only for owned handles that were not moved.

The confirmed root cause is localized in ARC join reification.

The failing proc is the TRMC-transformed recursive helper. It has a self-loop join whose params are the same locals as the proc args:

```text
proc p8 args=[l64:list#30, l65:list#30, l66:list#30] ret=list#30 transform=tce
  decref l65
  join j11 params=[l64, l65, l66]
    remainder:
      jump j11
    body:
      decref l65
      ...
```

That first `decref l65` is invalid. `l65` is an incoming owned value that must remain live across the join remainder's initial `jump j11`, because the jump enters a join body that carries `l65` as an owned join param. ARC computed `entry_keep` only from uses visible in the join remainder. The remainder did not syntactically read `l65`, so `finishRewriteJoin` released `l65` before the join. The join body then released the same local again, but on that certified path the local had already been unbound.

The fix is to include already-bound carried body ownership in the join entry keep set:

- compute the normal `body_keep`,
- intersect it with `incoming_owned` so fresh join params are not invented in the remainder,
- union that carried set into `entry_keep`.

This keeps incoming ownership live until the jump transfers it into the join body, without changing the existing handling for join params that are only initialized inside the loop body.

While investigating this, ARC emission also gained a smaller correctness improvement for occurrence-level moves: transfer checks now use `Solution.unitLocalOf(local)`, which follows borrowed pure same-value aliases but does not conflate payload/field borrows with their liveness leader.

## Paper Alignment

This fix is consistent with the borrow-inference paper. The paper separates inference from reification: first infer owned/borrowed modes and lifetimes, then reify each occurrence as a borrow, move, or `dup`, and finally drop owned handles whose lifetime ends without a move.

The 9703 bug was in that reification step. The solver's mode/lifetime model did not need to be replaced. ARC emission was ending an incoming owned handle before a join jump whose body still owned that handle. Keeping the already-bound carried ownership live across the join entry is the same ownership-handle discipline the paper describes: a handle may be moved into the join body, retained when an independent owned occurrence is needed, or dropped after its lifetime ends, but it cannot be dropped before the control-flow edge that transfers it.

## Ideal Long-Term Fix

The ideal fix is to preserve the current architecture and tighten the reification invariants around joins and occurrence moves:

1. Join entry keep sets must include any incoming ownership unit that the join body owns on entry.
2. That inclusion must be intersected with the incoming owned set so fresh join params are not treated as bound in the remainder.
3. Ownership transfers should operate on the movable ownership unit for the occurrence, not blindly on either the occurrence local or the broader borrow-group leader.
4. Borrow-group leaders should continue to drive liveness and death placement; pure same-value alias chains should drive move-through-alias behavior.
5. Regression coverage should include TRMC/self-loop joins whose params reuse proc args, plus borrowed aliases moving into calls and aggregates.

This should remain a compiler-internal invariant. The certifier should continue to panic on unbound releases; it is correctly catching invalid ARC output.

Performance-wise, the fix should not add conservative retains. It should only retain when two independent owned occurrences are actually needed. Otherwise, borrow inference should keep using moves and borrowed occurrences exactly as described by the paper.
