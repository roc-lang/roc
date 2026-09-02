# Erased Ownership Travels as LIR Data, Not a Re-Derived Rule

## Problem

The direct LIR lowerer decides, for each `assign_ref`, whether the
target is a transparent alias of its source's erased ownership, a
fresh root, or ambiguous. It records that decision in its own state:

- `src/postcheck/solved_lir_lower.zig:4519` `noteErasedOwnerDefinition`
- `src/postcheck/solved_lir_lower.zig:4545`
  `transparentErasedOwnershipSource`
- `src/postcheck/solved_lir_lower.zig:4577`
  `sameTransparentPointerRepresentation`
- called from `:4537` `addAssignRef`

The ARC borrow certifier then re-derives the same decision from the
finished LIR with a second copy of the rule:

- `src/lir/arc_certify.zig:2636` `noteErasedOwnerDefinition`
- `src/lir/arc_certify.zig:2645`
  `transparentErasedOwnershipSource`
- consumed by `:2682` `resolvedErasedOwner` and `:2698`
  `refcountedErasedOwner`, driven from `:2712-3000`

The two `transparentErasedOwnershipSource` bodies are the same rule
written twice: the same `RefOp` switch, the same variant-index-0
condition, the same zero-discriminant tag-union condition, the same
target-usize pointer-width comparison. They differ only in receiver
plumbing and in the lowerer having factored the width check into a
named helper.

`grep -rn 'transparentErasedOwnershipSource\|noteErasedOwnerDefinition'
src/` returns hits in exactly those two files. `src/postcheck/boxy/lower.zig`
has no equivalent—so on the `.boxy` path the certifier's ownership
model has no producer to agree with at all. It is deriving facts about
a program whose lowerer never reasoned in those terms.

This is the shape design.md's core principle names directly: "A
consumer must not recover missing data from source syntax, names, body
scans, display strings, runtime bytes, object symbols, backend state,
or incidental data structure shape." The certifier is recovering the
lowerer's decision from the incidental shape of the emitted
`assign_ref` chain.

## Background

design.md's "Checked identity and runtime encoding are separate data"
rule has a general form: the stage that commits a decision outputs the
explicit mapping, and later stages consume it. Erased ownership is
such a decision—the lowerer knows, at the moment it emits the
`assign_ref`, whether it intended the target to alias the source's
owner. Nothing about that intent is recoverable in principle from the
statement alone; it happens to be recoverable today because the
lowerer emits a recognizable shape.

The certifier is Debug-only (`arc_certify.zig` is the debug borrow
certifier), which bounds the blast radius: a divergence here produces
a false certifier verdict—a spurious failure, or worse, a missed
one—rather than a miscompile. That is why this is a small project and
not a big one. It is still the wrong direction of information flow,
and the boxy gap means the certifier's coverage of that strategy is
not what it appears to be.

## Evidence

- `solved_lir_lower.zig:4545` against `arc_certify.zig:2645`—same
  switch arms, same conditions, same result.
- `grep -rn 'transparentErasedOwnershipSource' src/`—two definitions,
  zero shared home.
- `grep -rn 'erased_owner' src/postcheck/boxy/`—empty; the boxy
  lowerer records nothing here.
- `arc_certify.zig:2712-3000`—the certifier's ~20 `noteErasedOwnerDefinition`
  call sites, one per statement kind, reconstructing per-statement
  what the lowerer knew per-statement.

## Solution design

1. **Add the fact to LIR.** A per-local side table on `LirStore`
   holding the lowerer's erased-owner state: `root`, `alias(LocalId)`,
   or `ambiguous`—the same three-state lattice `arc_certify` already
   models in `erased_owner_states`. Dense over locals, like the
   existing per-local metadata.
2. **The LSS lowerer writes it.** `solved_lir_lower.zig`'s
   `noteErasedOwnerDefinition` becomes a store write instead of
   private state. `transparentErasedOwnershipSource` stays in the
   lowerer—it is the producer's rule and belongs to the producer.
3. **The boxy lowerer writes it too.** This is the substantive part:
   decide, for each boxy `assign_ref`-emitting site, what the erased
   ownership actually is. Where boxy's erased-callable representation
   makes the answer `ambiguous`, record `ambiguous` explicitly—an
   honest coarse answer beats an absent one. Where boxy genuinely has
   no erased values, record that the table is empty for a reason, not
   by omission.
4. **The certifier reads and checks.** Delete
   `arc_certify.zig:2636` and `:2645` and the ~20 reconstruction call
   sites. `resolvedErasedOwner` walks the recorded table. The
   certifier's new job is to *verify* the recorded states are
   consistent with the emitted statements—which is a real check, and a
   different one from recomputing them.
5. **Pin it.** A source-text test asserting one definition of
   `transparentErasedOwnershipSource` under `src/`, and a debug
   assertion that every `assign_ref` target has a recorded state.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -rn 'fn transparentErasedOwnershipSource\|fn
  noteErasedOwnerDefinition' src/` shows one definition each, in the
  producer.
- Both lowering strategies populate the erased-owner table for every
  `assign_ref` they emit; a Debug assertion fires if one does not.
- `arc_certify.zig` contains no reconstruction of erased ownership—it
  reads the table and validates it.
- The certifier passes on the full corpus under both `.lss` and
  `.boxy` in a Debug build. If enabling it on boxy surfaces real
  violations, those are fixed (or filed with a scoped, commented
  allowlist)—not worked around by leaving boxy uncovered.
- `git diff test/snapshots` is empty; no runtime behavior changes.

## How to evaluate the result

### Correctness ideal

The certifier checks the lowerer's actual intent instead of a
reconstruction of it, so the class of bug where a lowering change
quietly stops matching the certifier's pattern-recognition cannot
occur. `.boxy` gains real borrow-certification coverage rather than
apparent coverage. The information flows producer-to-consumer, which
is what design.md's post-check principles require.

### Performance ideal

Slightly positive on the certifier path and neutral elsewhere. The
table is one dense per-local enum—smaller than the
`erased_owner_states` hash map the certifier builds today—and reading
it replaces a full statement walk with a lookup. The lowerer's write
is one store per `assign_ref`, work it already does into private
state. Memory: one byte per local (plus the alias id where present),
which is noise against existing per-local metadata; confirm with a
peak-RSS check on a large corpus program rather than assuming.

## Tests to add

- Single-definition source-text lint for both function names.
- A Debug assertion (and a test that trips it under a deliberately
  incomplete lowering) that every `assign_ref` target has a recorded
  erased-owner state.
- Certifier runs over the corpus under `.boxy`, newly meaningful.
- A case per `RefOp` variant, so the recorded state for each is
  pinned rather than incidental.

## Related projects

- [arc-shared-predicates.md](arc-shared-predicates.md)—the other
  duplicated-predicate cluster in the same stage; land either first.
- [hoist-consumes-dispatch-evidence.md](hoist-consumes-dispatch-evidence.md)—
  the same "consume the recorded decision instead of re-deriving it"
  cure at the checker boundary.
- [../big/one-value-semantics-layer.md](../big/one-value-semantics-layer.md)—
  step 3 here needs boxy's erased-callable representation understood;
  that project touches the same ground.
