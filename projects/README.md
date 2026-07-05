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

1. [small/cross-phase-coverage-parity-tests.md](small/cross-phase-coverage-parity-tests.md)
   — cheap insurance before larger migrations: it pins producer/consumer
   predicate parity so later refactors have a focused regression net.

### Dependency chains

**Chain A — dispatch:**
1. [big/total-dispatch-plans.md](big/total-dispatch-plans.md)
2. [big/generalization-time-ambiguity.md](big/generalization-time-ambiguity.md)
   — shares the constraint-provenance foundation with total dispatch plans;
   doable before it, but cheaper after.

**Chain B — identity:**
1. [big/immutable-specialization-identity.md](big/immutable-specialization-identity.md)
   — the remaining identity project. Content-based nominal identity has landed,
   but specialization records still rekey themselves during Monotype lowering.

Chain B also strengthens Chain A (stable cross-module references for resolved
dispatch targets) and the glue project, but does not block them.

**Chain C — ARC:**
1. [big/arc-certifier-lattice-join.md](big/arc-certifier-lattice-join.md)
   — removes the remaining certifier skip path and centralizes
   ownership-transfer keying. The current code already warns on skips and
   fails CI when skips occur; this project closes the hole for real.

**Chain D — numerics:**
1. [big/exact-numeral-pipeline.md](big/exact-numeral-pipeline.md)
- [small/checked-arithmetic-lir-ops.md](small/checked-arithmetic-lir-ops.md)
  is independent of both and can land any time.

### Independent — start any time, in any order

Small:
- [small/cross-phase-coverage-parity-tests.md](small/cross-phase-coverage-parity-tests.md)
  — cheap insurance; ideally land early so later projects inherit the harness.
- [small/centralize-slice-reuse-predicate.md](small/centralize-slice-reuse-predicate.md)
- [small/store-generation-counters.md](small/store-generation-counters.md)
- [small/checked-arithmetic-lir-ops.md](small/checked-arithmetic-lir-ops.md)
- [small/shared-checked-type-traversal.md](small/shared-checked-type-traversal.md)
- [small/cache-hardening.md](small/cache-hardening.md)
- [small/glue-consumes-committed-layouts.md](small/glue-consumes-committed-layouts.md)
- [small/structural-hoist-contexts.md](small/structural-hoist-contexts.md)

Big:
- [big/decision-tree-match-compiler.md](big/decision-tree-match-compiler.md)
  — independent; benefits from landing the coverage-parity test harness first,
  and pairs naturally with pipeline unification (below) since today every
  match-lowering change must be made twice.
- [big/unify-build-pipelines.md](big/unify-build-pipelines.md) — independent;
  package identity is already centralized, but the run path still hand-wires
  coordinator setup and report rendering.

### Suggested overall sequence

If one person or agent works through everything serially, this order front-loads
leverage and keeps prerequisites satisfied:

1. `small/cross-phase-coverage-parity-tests.md`
2. `small/centralize-slice-reuse-predicate.md`
3. `small/store-generation-counters.md`
4. `small/checked-arithmetic-lir-ops.md`
5. `big/total-dispatch-plans.md`
6. `big/immutable-specialization-identity.md`
7. `small/shared-checked-type-traversal.md`
8. `small/cache-hardening.md`
9. `big/arc-certifier-lattice-join.md`
10. `big/exact-numeral-pipeline.md`
11. `big/generalization-time-ambiguity.md`
12. `big/unify-build-pipelines.md`
13. `big/decision-tree-match-compiler.md`
14. `small/glue-consumes-committed-layouts.md`
15. `small/structural-hoist-contexts.md`
