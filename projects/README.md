# Compiler Improvement Projects

This folder contains self-contained project specifications for structural
improvements to the compiler. Each `.md` file is written so that someone brand
new to the codebase (human or agent) can read that one file and understand the
problem, the solution approach, what success looks like, how to evaluate the
result for long-term correctness and performance, and what tests to add.

- `small/` — projects on the order of days each: localized, mostly additive
  checks or deletions, low design risk.
- `big/` — projects on the order of weeks each: cross-cutting, and several
  require a design decision before implementation starts.

The projects came out of a root-cause analysis of eight weeks of bug fixes
(May–June 2026). The recurring disease across independent bug clusters was:
facts proven during checking get re-derived downstream from type, name, or
structure content instead of traveling as explicit data, keyed by fragile
identity (name strings, positional order, mutable keys) and enforced only by
panics at the consumption site. Most of these projects either move a fact into
an explicit artifact, assign an identity once and carry it, or delete a
duplicated computation. `design.md` at the repo root is the authoritative
post-check design; these projects implement its stated principles more
completely.

## Recommended order

### Start here

1. [small/check-app-against-platform-requires.md](small/check-app-against-platform-requires.md)
   — highest leverage per unit of work in the whole set. Deletes the
   coordinator's shadow-validation layer, kills the most active bug cascade,
   and is a prerequisite for two big projects (total dispatch plans, exact
   numeral pipeline).

### Dependency chains

**Chain A — dispatch:**
1. `small/check-app-against-platform-requires.md` (prerequisite: dispatch
   plans cannot be total while platform-typed values are still flex at check
   time)
2. [big/total-dispatch-plans.md](big/total-dispatch-plans.md)
3. [big/generalization-time-ambiguity.md](big/generalization-time-ambiguity.md)
   — shares the constraint-provenance foundation with total dispatch plans;
   doable before it, but cheaper after.

**Chain B — identity:**
1. [small/single-package-identity-function.md](small/single-package-identity-function.md)
   — the interim fix; small and immediately useful.
2. [big/content-based-nominal-identity.md](big/content-based-nominal-identity.md)
   — the durable fix; absorbs and supersedes the interim one.
3. [big/immutable-specialization-identity.md](big/immutable-specialization-identity.md)
   — can proceed independently using today's module digests, but its
   `CallableIdentity` components get cleaner after content-based identity.

Chain B also strengthens Chain A (stable cross-module references for resolved
dispatch targets) and the glue project, but does not block them.

**Chain C — captures:**
- [big/canonical-capture-id.md](big/canonical-capture-id.md) is independent,
  but do it before or together with the evidence-threading part of
  `big/total-dispatch-plans.md`: where-clause evidence flows through nested
  closures exactly like a compile-time capture, and it should ride the same
  identity discipline rather than invent a parallel one.

**Chain D — ARC:**
1. [small/surface-arc-certifier-skips.md](small/surface-arc-certifier-skips.md)
   — trivial; do immediately so the current certification hole is visible.
2. [big/arc-certifier-lattice-join.md](big/arc-certifier-lattice-join.md)
   — closes the hole for real and centralizes ownership-transfer keying.

**Chain E — numerics:**
1. `small/check-app-against-platform-requires.md` (removes one of the two
   places literal range facts get dropped)
2. [big/exact-numeral-pipeline.md](big/exact-numeral-pipeline.md)
- [small/checked-arithmetic-lir-ops.md](small/checked-arithmetic-lir-ops.md)
  is independent of both and can land any time.

### Independent — start any time, in any order

Small:
- [small/cross-phase-coverage-parity-tests.md](small/cross-phase-coverage-parity-tests.md)
  — cheap insurance; ideally land early so later projects inherit the harness.
- [small/centralize-slice-reuse-predicate.md](small/centralize-slice-reuse-predicate.md)
- [small/store-generation-counters.md](small/store-generation-counters.md)
- [small/shared-checked-type-traversal.md](small/shared-checked-type-traversal.md)
- [small/cache-hardening.md](small/cache-hardening.md)
- [small/glue-consumes-committed-layouts.md](small/glue-consumes-committed-layouts.md)
  — benefits mildly from content-based identity but does not depend on it.
- [small/structural-hoist-contexts.md](small/structural-hoist-contexts.md)

Big:
- [big/decision-tree-match-compiler.md](big/decision-tree-match-compiler.md)
  — independent; benefits from landing the coverage-parity test harness first,
  and pairs naturally with pipeline unification (below) since today every
  match-lowering change must be made twice.
- [big/unify-build-pipelines.md](big/unify-build-pipelines.md) — independent;
  benefits from `small/single-package-identity-function.md` landing first so
  the shared layer starts with one naming scheme.
- [big/row-subsumption.md](big/row-subsumption.md) — independent of all other
  projects, but requires a language-semantics decision before implementation
  starts (see the file).

### Suggested overall sequence

If one person or agent works through everything serially, this order front-loads
leverage and keeps prerequisites satisfied:

1. `small/surface-arc-certifier-skips.md`
2. `small/cross-phase-coverage-parity-tests.md`
3. `small/check-app-against-platform-requires.md`
4. `small/single-package-identity-function.md`
5. `small/centralize-slice-reuse-predicate.md`
6. `small/store-generation-counters.md`
7. `small/checked-arithmetic-lir-ops.md`
8. `big/total-dispatch-plans.md`
9. `big/canonical-capture-id.md` (or interleave with 8; see Chain C)
10. `big/content-based-nominal-identity.md`
11. `big/immutable-specialization-identity.md`
12. `small/shared-checked-type-traversal.md`
13. `small/cache-hardening.md`
14. `big/arc-certifier-lattice-join.md`
15. `big/exact-numeral-pipeline.md`
16. `big/generalization-time-ambiguity.md`
17. `big/unify-build-pipelines.md`
18. `big/decision-tree-match-compiler.md`
19. `small/glue-consumes-committed-layouts.md`
20. `small/structural-hoist-contexts.md`
21. `big/row-subsumption.md` (whenever the language decision is made; nothing
    blocks on it)
