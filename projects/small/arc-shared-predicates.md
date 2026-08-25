# One Definition per ARC Predicate

## Problem

The ARC stage is four modules that must agree exactly—`arc_solve.zig`
(borrow inference), `arc_dismantle.zig` (field takes), `arc.zig` (RC
statement insertion), and `arc_certify.zig` (the debug borrow
certifier). Four of the predicates they share are implemented twice,
and one pair **has already diverged**.

**`computeLocalContainsRefcounted`.** `src/lir/arc_solve.zig:354`
declares it `pub`. `src/lir/arc.zig:547` declares a private copy.
`arc_certify.zig:192` calls the `arc_solve` one; `arc.zig:280` and
`arc.zig:7889` call their own. `arc.zig` already imports `arc_solve`
at `:24` and calls `arc_solve.solve` eight lines below the site where
it calls its own copy. The two bodies now differ:

- `arc_solve`'s version asserts the Boxy descriptor table covers every
  local (`solveInvariant("ARC Boxy descriptor table did not cover
  every local")`) and falls back to `local.boxy_desc` when the table
  is empty. `arc.zig`'s version indexes `boxy_rc_descs[index]`
  unconditionally and has no invariant check.

This is the predicate that decides whether a local is refcounted at
all. The RC inserter and the certifier that is supposed to check the
inserter compute it with different code.

Its three helpers are duplicated alongside: `markLocalRc`,
`markLocalRcIfSourceRc`, `markLocalRcIfSpanContainsRc` at
`arc.zig:702, 709, 715` and `arc_solve.zig:410, 417, 423`.

**`defaultOutcomeMask`.** Byte-identical bodies at `arc.zig:4791`
(a method on `Inserter`) and `arc_dismantle.zig:607` (a free
function). Same outcome-discriminant scan, same `restituted_params`
intersection, same `?ParamMask` result.

**`findOutcomeRefinement`.** `arc.zig:4677` and
`arc_dismantle.zig:554`—the same alias-chain walk looking for the
discriminant read that refines a call's outcome, written twice
(0.88 normalized-body similarity; the differences are the arena
plumbing and one `for` loop written two ways).

**`outcomeBindingTarget` / `resultBindingTarget`.** A 50-line exact
duplicate under two names: `arc_solve.zig:970` and
`arc_certify.zig:1060`.

## Background

The module split is deliberate and documented in `arc.zig`'s own
header: solve, then dismantle, then insert, with `arc_certify` as the
debug checker over the result. The intended data flow is one-way—
`arc.zig` imports `arc_sig`, `arc_solve`, `arc_certify`, and
`arc_dismantle` (`arc.zig:23-26`), and `arc_certify` imports
`arc_solve`. Nothing about the layering forces any of these copies;
in three of the four cases the shared definition is already public and
already imported at the copying site.

The `arc_solve` version of `computeLocalContainsRefcounted` is the
better one—it is the one with the coverage invariant—which also tells
you which direction the merge goes.

## Evidence

- `grep -n computeLocalContainsRefcounted src/lir/*.zig`—one `pub`
  definition (`arc_solve.zig:354`), one private definition
  (`arc.zig:547`), three call sites split across them.
- `arc.zig:24`—`const arc_solve = @import("arc_solve.zig");`, and
  `arc.zig:288`—`arc_solve.solve(...)`, eight lines after
  `arc.zig:280` calls the local copy.
- Diff `arc.zig:547-600` against `arc_solve.zig:354-407`: the
  `boxy_rc_descs.len` handling and the invariant differ.
- `arc.zig:4791` against `arc_dismantle.zig:607`—identical.
- `arc_solve.zig:970` against `arc_certify.zig:1060`—identical, 50
  lines.

## Solution design

1. **Delete `arc.zig`'s `computeLocalContainsRefcounted` and its
   three `markLocalRc*` helpers.** Point `arc.zig:280` and
   `arc.zig:7889` at `arc_solve.computeLocalContainsRefcounted`. The
   surviving body is `arc_solve`'s, invariant included—confirm the
   invariant holds at both new call sites (`arc.zig` passes a
   `computeBoxyRcDescs(store)` result, which is full-length, so it
   should; if it does not, that is a bug this change surfaces rather
   than causes).
2. **Move `defaultOutcomeMask` and `findOutcomeRefinement` to a
   single owner.** `arc_sig.zig` is the natural home for
   `defaultOutcomeMask` (it is pure `Outcome`/`ParamMask` arithmetic);
   `findOutcomeRefinement` is a store walk and belongs in
   `arc_solve.zig` or `arc_dismantle.zig`. Whichever is chosen, the
   other two modules import it and delete their copies. Reconcile the
   two `findOutcomeRefinement` signatures deliberately—one returns
   `?OutcomeRefinement`, the other `?LIR.CFStmtId`—rather than keeping
   both shapes.
3. **Delete `arc_certify.zig`'s `resultBindingTarget`** and call
   `arc_solve.outcomeBindingTarget`, exporting it if needed.
4. **Pin the result.** Add a source-text test in
   `src/postcheck/structural_test.zig` (or a sibling under `src/lir/`)
   asserting one definition each for these four predicate names across
   `src/lir/`, so the copies cannot reappear.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -rn 'fn computeLocalContainsRefcounted\|fn markLocalRc\|fn
  markLocalRcIfSourceRc\|fn markLocalRcIfSpanContainsRc\|fn
  defaultOutcomeMask\|fn findOutcomeRefinement\|fn
  outcomeBindingTarget\|fn resultBindingTarget' src/lir/` shows
  exactly one definition per predicate.
- The surviving `computeLocalContainsRefcounted` is the one with the
  descriptor-coverage invariant, and that invariant is live on the
  `arc.zig` insertion path (verified by a debug run over the corpus,
  not by inspection).
- The single-definition source-text test is in-tree and green.
- ARC-related snapshots and the refcount conformance suite are
  unchanged; `git diff test/snapshots` is empty.
- The certifier passes on the full corpus in a Debug build, since it
  now checks against the same predicate the inserter used.

## How to evaluate the result

### Correctness ideal

The inserter and its certifier cannot disagree about which locals are
refcounted, because there is one function. The already-present
divergence (empty descriptor table handled on one side only) is
resolved rather than latent, and the coverage invariant now guards
both paths. Future changes to Boxy descriptor handling land on solve,
insert, dismantle, and certify atomically.

### Performance ideal

Neutral: identical work, one implementation, and the deleted copies
were not doing anything cheaper. `computeLocalContainsRefcounted` is
a fixpoint over all statements and runs twice per compile today
(once in `arc.zig`, once in `arc_certify` under Debug); this project
does not change that count. If profiling shows the double run
matters, threading the already-computed table from `arc.zig` into
`arc_certify` is a natural follow-up—but measure before doing it, and
keep it a separate change.

## Tests to add

- Single-definition source-text lint for the four predicate names.
- A regression case exercising the divergent branch: a program whose
  Boxy descriptor table is empty at the `arc.zig` call site (or an
  assertion that this cannot occur), so the merged behavior is pinned
  rather than assumed.
- Existing ARC certifier runs over the corpus stay green in Debug.

## Related projects

- [erased-ownership-as-lir-data.md](erased-ownership-as-lir-data.md)—
  the other lowerer/certifier re-derivation in the same stage.
- [rceffect-conformance.md](rceffect-conformance.md)—the same
  "one ownership table, conformance-tested" discipline for `LowLevel`.
- [cross-phase-coverage-parity-tests.md](cross-phase-coverage-parity-tests.md)—
  the parity-suite discipline the single-definition lint complements.
