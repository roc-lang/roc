# A Shared Cycle-Guarded Checked-Type Traversal

## Problem

Every traversal of checked types must guard against cycles, and today that
discipline is per-site folklore. Grepping for the dominant idiom —
`AutoHashMap(_, void).init` visited/active sets — finds **105** instantiation
sites across `src/check`, `src/types`, and `src/postcheck`, **34 of them in
`src/check/checked_artifact.zig` alone**. At least four *other* cycle-guard
idioms coexist:

- `src/check/occurs.zig` uses a shared `Scratch` of visited vars,
- `src/types/TypeWriter.zig` keeps `seen` / `seen_count_var_occurrences`
  linear lists,
- `src/check/snapshot.zig` keeps `seen_vars: base.Scratch(Var)`,
- `src/postcheck/monotype_lifted/spec_constr.zig` linear-scans active stacks
  (`inline_stack`, `callable_stack`, `loop_stack`).

On top of that, `checked_artifact.zig` contains many near-copies of the same
"does this type contain identity variables" walk: `checkedTypeContains-`
`IdentityVariables` (+ slice/payload variants), two separate
`typeContainsIdentityVariables` families on different structs,
`typeWriteContainsIdentityVariables`, `sourceTypeContainsIdentityVariables`,
`finalizeContainsIdentityVariables`, `mergeContainsIdentityVariables` (record
and tag variants), and `pendingRootContainsIdentityVariables` — each with its
own hand-rolled cycle guard.

The failure this causes is not hypothetical. In the platform-relation
resolver (`PlatformAppRelationTypeResolver` in `checked_artifact.zig`, which
merges platform and app types across a platform `requires` boundary), the
`finalizing` memo map was declared, initialized, deinited, and **read** —
`if (self.finalizing.get(root)) |existing| return existing;` — but never
**written**, from the commit that introduced it. Dead scaffolding. A
recursive type crossing the boundary recursed forever
(`test/cli/issue9717-platform`, the recursive `Elem` tree), fixed by PR
roc-lang/roc#9727. When each traversal hand-rolls its guard, "forgot the
`put`" compiles cleanly and only fails on recursive inputs.

## Background

The compiler pipeline: parse → canonicalize → type-check (produces "checked
artifacts" per module) → postcheck: Monotype IR (monomorphization/
specialization, `src/postcheck/monotype/` — `lower.zig` is ~21k lines) →
Monotype Lifted (closure lifting; also `spec_constr.zig` call-pattern
specialization for optimized builds) → Lambda Solved → Lambda Mono → LIR →
ARC → backends. `design.md` at the repo root is authoritative.

Checked artifacts store types in a `CheckedTypeStore`
(`src/check/checked_artifact.zig`; ids in `src/check/checked_ids.zig` as
`CheckedTypeId`). Recursive types are represented as **cyclic `CheckedTypeId`
graphs** — there is no implicit tree structure — so every traversal, without
exception, must either memoize visited roots or track an active path. The
store's house pattern for *building* possibly-recursive results is
reserve-then-fill with content-addressed keys: `reserveSyntheticTypeRoot`
allocates a root with a `.pending` payload before descending, so back-edges
can reference it, and `fillSyntheticTypeRoot` completes it. Keys are computed
by a shadow digest traversal that encodes back-edges as depth indices into
the active path (de Bruijn-style), so structurally identical recursive
results share one root. `design.md`'s "Type Alias Invariant" section
describes the degenerate self-referential backing case any such traversal
must tolerate.

## Evidence

- Counts (verified by grep): `AutoHashMap(..., void).init` — 34 sites in
  `src/check/checked_artifact.zig`, 105 total across `src/check`,
  `src/types`, `src/postcheck`. Files include `Check.zig`,
  `exhaustive.zig`, `generalize.zig`, `monotype/lower.zig`,
  `monotype/type.zig`, `monotype/solve.zig`, `monotype_lifted/lift.zig`,
  `lambda_solved/solve.zig`, `solved_lir_lower.zig`.
- Other idioms: `src/check/occurs.zig` (`Scratch`, `occurs`);
  `src/types/TypeWriter.zig` (`seen`, `seen_count_var_occurrences`);
  `src/check/snapshot.zig` (`seen_vars`);
  `src/postcheck/monotype_lifted/spec_constr.zig` (linear scan over
  `inline_stack.items` for recursion detection).
- PR roc-lang/roc#9727 ("Fix platform relation recursive stack overflow"):
  the introducing commit shows `finalizing` read but never written; the fix
  added `reserveSyntheticTypeRoot` + `finalizing.put` before descending, and
  the `test/cli/issue9717-platform` suite (recursive `Elem` across
  `requires`).
- The fix duplicated merge/finalize semantics roughly four times inside
  `checked_artifact.zig`, as four parallel walks over the same input space:
  1. the resolver's clone traversal (`PlatformAppRelationTypeResolver`, memo
     maps `finalizing: AutoHashMap(PlatformAppRelationFinalizeInput,
     CheckedTypeId)` and `merging: AutoHashMap(PlatformAppRelationMergeInput,
     CheckedTypeId)`);
  2. the shadow digest hasher (`PlatformAppRelationTypeDigestBuilder`, with
     `source_active`/`finalizing`/`merging` maps to `u32` depths and
     `activeDepth` for back-edge encoding — feeding
     `platformAppRelationMergeResultKey` / `...FinalizeResultKey`);
  3. the empty-normalization prescans
     (`platformAppRelationMergeResultIsEmptyRecord` /
     `...IsEmptyTagUnion` / `...FinalizeResultIsEmptyRecord` /
     `...IsEmptyTagUnion`, each running `mergeIsEmpty*` / `finalizeIsEmpty*`
     walks on a fresh digest-builder instance);
  4. the pending-tolerant `containsIdentityVariables` fork
     (`pendingRootContainsIdentityVariables` plus the resolver-local
     `typeContainsIdentityVariables` family), needed because reserved roots
     have `.pending` payloads mid-build.

## Solution design

One utility (suggested home: `src/types/` or `src/check/`, imported by both
check and postcheck) providing the three traversal shapes that exist today as
copies:

1. **Memoized visit-map traversal** over `CheckedTypeId` graphs: generic over
   a caller Context with `visit(root) → Result`; the utility owns the
   `AutoHashMap(Key, Result)` memo, guarantees the memo is written *before*
   descending (a reserve slot or sentinel), and re-entrancy returns the
   in-progress entry. Boolean predicates (`containsIdentityVariables`-style)
   are this shape with `Result = bool` and an active-set short-circuit.
2. **Reserve-then-fill variant** for graph-building consumers: the utility
   calls `reserveSyntheticTypeRoot` (or a caller-supplied reserve hook)
   before descending and `fillSyntheticTypeRoot` after, so back-edges tie the
   knot; the memo maps input keys to reserved roots. The 9727 bug —
   reading a memo nobody writes — is unrepresentable because the caller never
   touches the memo directly.
3. **Digest variant** with de Bruijn-encoded back-edges for
   content-addressed keys: the utility owns the active-path depth maps and
   emits `back_edge(depth)` markers into a caller-supplied hasher, matching
   `PlatformAppRelationTypeDigestBuilder.activeDepth` semantics.

Keys are caller-defined (plain `CheckedTypeId`, or composite inputs like
`PlatformAppRelationMergeInput`) so multi-input walks fit.

Migration order:

1. Land the utility with unit tests on hand-built cyclic stores.
2. Migrate the platform-relation resolver's four parallel walks — the proof
   case: clone traversal, digest hasher, empty prescans, and the
   pending-tolerant identity-variable fork should collapse onto shapes 2, 3,
   1, and 1 respectively, sharing one traversal skeleton and one
   pending-tolerance policy. DELETE the bespoke memo plumbing they replace.
3. Migrate the remaining `*ContainsIdentityVariables` copies in
   `checked_artifact.zig` onto shape 1; DELETE the duplicates.
4. Opportunistic migration of the other 100-odd sites as they are touched.
5. Add a review/lint note (CONTRIBUTING or a comment on `CheckedTypeStore`):
   new `CheckedTypeId` traversals must use the utility.

## What success looks like

- The four parallel platform-relation walks share one traversal skeleton;
  the resolver's memo maps are owned by the utility, not by hand.
- All `containsIdentityVariables` logic in `checked_artifact.zig` exists
  exactly once (per payload policy), parameterized over pending-tolerance.
- New traversals cannot ship without cycle discipline: the utility API has no
  "read the memo but skip the write" path.
- The `AutoHashMap(_, void).init` count in `checked_artifact.zig` drops
  substantially and trends down repo-wide.

## How to evaluate the result

### Correctness ideal

The utility is unit-tested on cyclic stores, including a self-referential
backing (the degenerate case `design.md`'s Type Alias Invariant describes)
and mutually recursive roots. Mutation test: reintroduce 9727's missing memo
write in a copy of the old resolver code and show that expressing the same
walk through the utility API makes the bug unrepresentable (the write happens
inside the utility before user code can descend). The
`test/cli/issue9717-platform` suite passes before and after each migration
step, byte-identical artifacts.

### Performance ideal

Memoized traversal is O(nodes + edges) with one hash probe per node — no
worse than today's correct sites. No digest recomputation for shared
subgraphs (the digest variant memoizes completed-subtree digests). Verify no
regression in checked-artifact publication time on the largest test
platforms (`test/cli/issue9717-platform` and the compiler's own corpus);
scratch-map reuse (retained-capacity reset) keeps allocation churn at or
below current levels.

## Tests to add

- Issue #9717 repro kept green: recursive `Elem` across `requires`
  (`test/cli/issue9717-platform`), plus
  `test/cli/Issue9717SpecConstrSpanInvalidation.roc`.
- Utility unit tests: cyclic graph memoization (each node visited once),
  reserve-then-fill producing a well-formed recursive root, digest stability
  across traversal order, back-edge depth encoding, pending-payload
  tolerance.
- A stress test with deep alias/backing chains and a wide mutually-recursive
  tag-union family, run in debug mode (catches both stack overflow and
  missing-memo livelock via a step budget).

## Related projects

- [Debug Generation Counters on Growable Stores](../small/store-generation-counters.md)
  — the sibling "make the invariant structural" project for store borrows.
- [Immutable Specialization Identity](../big/immutable-specialization-identity.md)
  — the Monotype-side consumer of digest/traversal discipline.
