## Typed CIR Architecture

Status: complete

This document records the completed typed-CIR transition.

The implementation followed the long-term-ideal rule throughout:

- prefer explicit stage contracts over convenience
- prefer one source of truth over duplicated trees or side tables
- prefer explicit published facts over later recovery, reconstruction, or guesswork
- prefer clean ownership and specialization boundaries over short-term test-driven shortcuts
- use `~/code/cor` as the architectural guide in spirit, while diverging only when the Roc representation can be strictly leaner without weakening the stage contract

## Final Architecture

### Published Typed CIR

The checker now publishes typed CIR as the sole checker-to-monotype source boundary.

Typed CIR owns:

- source CIR nodes
- the canonical solved type store for those source nodes
- identifier/string/import/method/evaluation-order facts needed downstream

Typed CIR exposes typed access through its API instead of making later stages depend on storage tricks.

Important representation detail:

- the dense `node idx == type idx` relationship still exists internally
- that relationship is now an implementation detail of typed-CIR accessors
- later stages consume typed-CIR APIs such as `exprType`, `patternType`, `defType`, `resolvedImportModule`, `resolvedStaticDispatchTarget`, and related helpers

This is the intentional divergence from `cor`:

- `cor` physically stores the solved type handle on each typed node
- Roc keeps one solved type store and encapsulates the dense index invariant behind typed-CIR APIs

That divergence is long-term ideal here because it preserves the same semantic contract as `cor` while avoiding per-node memory duplication.

### Monotype

Monotype now consumes typed CIR as immutable published source input.

Monotype owns:

- its own type interner
- its own specialization tables
- its own specialization-local cloning and workspace state
- its own explicit semantic facts used during lowering

Monotype no longer:

- clones whole checker module `types.Store` values
- clones whole checker module `Ident.Store` values
- runs checker unification as specialization workspace
- copies checker vars across module-store clones
- consumes the old `copy_import` API as public boundary machinery

### Static Dispatch

Static dispatch now follows the explicit-facts rule all the way through monotype lowering.

There are two explicit publication paths:

1. Checker-resolved dispatch facts published directly through typed CIR for source sites that are already concrete at check time.
2. Monotype specialization-local dispatch fact publication for generic method-call sites whose receiver becomes exact only after specialization binds a source var to an exact workspace type.

Later monotype lowering consumes only explicit `ResolvedDispatchTargetFact` values.
It no longer performs per-call recovery of dispatch targets during ordinary lowering.

This is the long-term ideal architecture for Roc, even though it differs slightly from the earlier draft of the plan that required the checker to resolve every dispatch site itself. That earlier draft turned out to be too strict for genuinely polymorphic dispatch sites whose concrete receiver identity exists only inside a monotype specialization. The correct design is still explicit-facts-only; the publication point for those facts is simply specialization-local rather than checker-global.

## Non-Negotiable Invariants

1. Typed CIR is the only published checker-to-monotype source boundary.
2. Source solved typing is accessed through typed-CIR APIs, not by raw node/type index knowledge outside typed CIR.
3. Later stages consume explicit published facts instead of reconstructing missing semantics after the fact.
4. Specialization-specific state lives on the monotype side, not in typed CIR.
5. Ordinary monotype lowering consumes explicit dispatch target facts only.
6. Any dispatch target not already concrete in typed CIR must be published explicitly during specialization-local fact publication before later lowering touches the call.
7. No duplicate published typed tree exists.
8. No old MIR boundary artifact remains.

## What Was Removed

The completed transition removed the old boundary residue from `src/`, including:

- duplicate MIR-tree publication
- MIR naming as the architectural boundary
- whole-module checker-store cloning in monotype
- checker-unification-driven specialization workspace in monotype
- public `check.copy_import` boundary residue
- ordinary monotype-time per-call static-dispatch target recovery

## Things To Watch Out For In Future Work

1. Do not let later stages bypass typed CIR and reopen raw checker storage conventions.
2. Do not move specialization-specific facts back into typed CIR.
3. Do not reintroduce dispatch recovery in ordinary monotype lowering; any new dispatch form must publish explicit facts before that point.
4. Do not reintroduce public compatibility layers around the old boundary.

## Verification

The completed transition is verified by:

- targeted eval regressions for generic local method dispatch specialization
- `zig build test-monotype`
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- `zig build test-cor-pipeline`

## Definition Of Done

This transition is complete because all of the following are now true:

- typed CIR is the sole published typed source boundary
- the dense source-node/type-store invariant is encapsulated behind typed-CIR APIs
- monotype owns specialization state directly
- monotype no longer clones whole checker module stores
- monotype no longer uses checker unification as specialization infrastructure
- later monotype lowering consumes explicit dispatch target facts only
- the remaining generic dispatch resolution is an explicit specialization-local fact publication step, not a fallback or heuristic
- old boundary residue has been deleted from `src/`
