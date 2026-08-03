# Hoist Selection Consumes Dispatch Evidence

## Problem

Compile-time hoisting decides "may this expression become a module-level
compile-time constant?" — and for dispatch-bearing expressions, part of
that answer is "is this dispatch evidence-dependent?" The compiler already
computes that fact exactly once, as the total dispatch plan:
`StaticDispatchResolution` (`src/check/static_dispatch_registry.zig:852`)
partitions every dispatch into `direct`/`structural` (safe to evaluate
with no specialization evidence) versus `constraint` (each specialization
edge supplies the target as evidence) and `unreachable_dispatch`.

Hoisting does not read that fact. It re-derives it from raw type-var
content: `staticDispatchAllowsHoistedRoot` (`src/check/Check.zig:7817`)
calls `varIsConcreteHoistedConstType` (`Check.zig:5617`), a recursive walk
over the resolved type graph that treats `.flex`/`.rigid`/`.err` as
"needs evidence." It reaches the dispatcher var through statement-kind
dispatch (`typeDispatchOwnerVar`, `Check.zig:14572`) and a re-derivation
for interpolation (`interpolationDispatchOwnerVar`, `Check.zig:14581`)
that carries its own debug panic for the case it cannot re-derive.

This is the check→postcheck disease inside a single phase: a fact the
compiler proves (the resolution) is re-derived from structure by a
parallel mechanism, and the two are reconciled by nothing. PR
roc-lang/roc#10073 (issue #10062: a where-clause dispatch selected as a
caller-less compile-time constant, panicking in monotype lowering) is the
proof that this seam produces bugs: its fix *added* the re-derivation
rather than consuming the plan, because the plan does not exist yet when
hoisting runs.

There is also a generated-code cost: `varIsConcreteHoistedConstType` is
conservative. Any dispatcher still flex at prune time is refused hoisting
even when its dispatch plan resolves `direct` — compile-time evaluation
the program is entitled to is left as runtime work.

## Background

The checker selects hoist roots eagerly during checking (`HoistFrame`,
positional eligibility via `HoistPosition` on `Expected`, `Check.zig:710`)
and prunes them after solving (`pruneSelectedHoistedRootsAfterSolving`,
`Check.zig:5523`). Total dispatch plans are assigned later, at
publication (`resolveTotalDispatchPlans`,
`src/check/checked_artifact.zig:13807`). So at prune time the plan
partition exists conceptually (solving is done; the registry has all the
facts) but has not been materialized — a pass-ordering gap, not a missing
fact.

`HoistPosition` is the structural home for the *positional* axis of
hoistability (guarded/suppressed/eligible). The *semantic* axis — "does
evaluating this require specialization evidence?" — has no structural
home; it lives in the `varIsConcreteHoistedConstType` walk.

## Evidence

- `src/check/Check.zig:5617` `varIsConcreteHoistedConstType` +
  `:5653` `...Internal`: recursive type walk with a visited set, run per
  dispatch-bearing hoist candidate.
- `src/check/Check.zig:7817` `staticDispatchAllowsHoistedRoot`: effect
  gate + the concreteness walk; applied to `e_dispatch_call`,
  `e_type_dispatch_call`, `e_method_eq`, `e_interpolation`;
  `e_method_call`/`e_type_method_call` are hard-coded ineligible.
- `src/check/Check.zig:14572/14581` `typeDispatchOwnerVar` /
  `interpolationDispatchOwnerVar`: dispatcher-var re-derivation by
  statement kind / numeric-suffix target, the latter with a debug panic
  arm.
- `src/check/static_dispatch_registry.zig:852`
  `StaticDispatchResolution`: the already-computed partition hoisting
  needs.
- PR #10073 / issue #10062: the panic this seam produced; PR #9925
  (hoist roots invalidated by runtime-error replacement) is the same
  two-passes-disagreeing-about-one-node shape one step earlier.

## Solution design

Make hoist pruning consume the dispatch-evidence fact instead of
re-deriving it. Two viable shapes; pick one:

1. **Stamp the fact at constraint creation/resolution.** When a dispatch
   constraint is created (or when its resolution becomes known during
   solving), record `hoist_safe: bool` (or the resolution kind) on the
   dispatch expression's recorded constraint metadata. Pruning reads the
   bit. `varIsConcreteHoistedConstType` and both owner-var re-derivations
   are DELETED.
2. **Materialize the plan partition before pruning.** Run the
   direct/structural-vs-constraint classification (the pure part of
   `resolveTotalDispatchPlans`) at the end of solving, before
   `pruneSelectedHoistedRootsAfterSolving`, and let both pruning and
   publication consume the one partition.

Either way, fold the resulting fact into the structural home: extend
`HoistPosition`/the completed-frame result so that "hoistability" is one
carried capability (position + evidence-independence + effect kind, the
last already carried on the where-clause node since #10073), not a
position fact plus a bolted-on type walk.

Recover the conservatism: a dispatch whose plan is `direct`/`structural`
is hoist-eligible even if its dispatcher var was never forced concrete.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -rn "varIsConcreteHoistedConstType" src/` matches nothing.
- `grep -rn "interpolationDispatchOwnerVar\|typeDispatchOwnerVar" src/check/Check.zig`
  matches nothing (the registry/publication copies may keep theirs — they
  run after plans exist).
- Hoist pruning contains no recursive walk over resolved type content;
  its dispatch gate is a field read.
- The #10062 repro (`test/cli/issue_10062_where_clause_segfault/`) stays
  green.
- A new test proves the conservatism is gone: a where-constrained
  dispatch whose plan resolves `direct` at a concrete receiver IS hoisted
  (assert via the hoisted-const test hooks the hoist suite already uses),
  where the walk-based gate refused it.
- A new test proves the safety direction: a `constraint`-resolved
  dispatch in an otherwise-hoistable position is NOT selected, with no
  panic anywhere in postcheck.
- Debug invariant: at publication, every hoisted root that carries a
  dispatch has a `direct`/`structural` resolution — the cross-check
  between the stamped bit and the final plan, so the two can never drift
  silently.

## How to evaluate the result

### Correctness ideal

"Evidence-dependent dispatch selected as a caller-less compile-time root"
is unrepresentable, because the selector never inspects type content — it
reads the same fact publication publishes. The publication-time debug
invariant makes any future divergence loud at the producer.

### Performance ideal

Checker: strictly less work — a per-candidate recursive type walk is
replaced by a field read. Generated code: strictly more compile-time
evaluation — every `direct`/`structural` dispatch in a hoistable position
is precomputed; none was lost. Verify on the snapshot corpus that the set
of hoisted roots only grows (diff the hoist-selection debug output before
and after on a corpus sweep).

## Tests to add

- The two eligibility tests above (direct-plan hoisted;
  constraint-plan refused) as checker unit tests beside the existing
  hoist-context matrix in `src/check/test/hoist_roots_test.zig`.
- A corpus assertion (debug builds) that every selected root with a
  dispatch has a compile-time-safe resolution at publication.
- Keep `test/cli/issue_10062_where_clause_segfault/` and the #9815/#9925
  hoist-invalidation tests green throughout.

## Related projects

- The completed checked-dispatch-evidence migration — the postcheck side
  of the same principle: dispatch facts are consumed from checked
  evidence, never re-derived. This project is the checker's own
  consumption of its own evidence.
