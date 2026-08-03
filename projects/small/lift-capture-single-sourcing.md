# Single-Source the Lift Stage's Capture Solving

## Problem

The Monotype Lifted stage's capture analysis is the best-guarded code the
2026-07 cor-comparison review looked at — the flattening fixpoint is a
true least fixed point, and `verifyCaptureInvariants` machine-checks the
slot/operand joins after lifting and after every rewrite. Three residues
keep it from being fully single-sourced, and each is the seed of a
familiar failure mode:

**L1 — the capture fixpoint has two driver loops.** The initial lift
solves captures via `computeCaptureFixpoint` + `solveInto`
(`src/postcheck/monotype_lifted/lift.zig:762`, `:794`); the
post-spec_constr re-solve (`recomputeCaptures`, `:178`, called from
`src/lir/checked_pipeline.zig:240`) uses a near-verbatim duplicate,
`solveCaptureFixpoint` (`:1020`). Both delegate the actual free-variable
walk to the shared `CaptureSet.collectExpr`, so the core is
single-sourced — but the driver (iteration, scratch handling,
convergence condition) exists twice. If one loop is edited and the other
is not, initial-lift captures and post-spec_constr captures can disagree,
and the disagreement would surface as a Debug verifier panic at best and
a wrong capture set in release at worst. This is precisely the
duplicated-driver drift the codebase's audits keep finding.

**L2 — `if_initialized_payload`'s payload has ambiguous binder
semantics.** `CaptureSet.collectExpr` treats the construct's `payload`
local as *potentially free* (`addIfFree`, `lift.zig:1358`), i.e. an outer
local that becomes a capture if unbound locally — while `rewriteExpr`
for the same construct never touches `payload` (`:668-672`), and no code
path binds it the way match binders are bound. If the construct
semantically *binds* `payload` (the natural reading: a payload extracted
in the initialized branch), the current handling over-captures — a
harmless-but-wasteful extra capture slot threaded through every closure
that contains the construct. If it is genuinely an outer local, the code
is right and one comment away from staying right. The review could not
determine the intent from the lifted stage alone; the producer of the
construct (Monotype lowering) knows.

**L3 — the capture-id override path tolerates stale ids by trusting a
live one.** `operandValueForSlotId` (`lift.zig:915`) lets a value-local's
*current* `CaptureId` override the operand's stored id, explicitly to
tolerate spec_constr substitution leaving stale ids behind. The keyed
Debug verifier checks operand-id/slot-id equality, but not this override
path: a spec_constr-substituted operand whose value coincidentally
carries a `CaptureId` equal to a *different* slot's id would be silently
routed to the wrong slot. No such case is known; the tolerance is
unverified rather than wrong.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR → **Monotype Lifted** (`lift.zig` closure lifting;
`spec_constr.zig` call-pattern specialization in optimized builds, after
which `recomputeCaptures` re-solves all capture sets) → Lambda Solved →
Lambda Mono decisions → LIR → ARC → backends. Capture identity is carried
as `CaptureId` (binder-derived or lift-synthesized), and all
slot/operand/field joins are keyed by it; ordering is ascending
`CaptureId` throughout.

The re-solve after spec_constr is intentional — specialization rewrites
bodies, so captures must be re-derived — which is exactly why the two
derivations must share one driver: they are *supposed* to be the same
computation run twice.

## Evidence

All symbols verified in the current tree.

- `src/postcheck/monotype_lifted/lift.zig`: `computeCaptureFixpoint`
  (`:762-789`) + `solveInto` (`:794`); `solveCaptureFixpoint` (`:1020`);
  `recomputeCaptures` (`:178`); shared walk `CaptureSet.collectExpr`
  (`:~1225`); `if_initialized_payload` in collect (`:1356-1361`,
  `addIfFree` on `payload` at `:1358`) vs rewrite (`:668-672`, payload
  untouched); `operandValueForSlotId` (`:915-933`); the capture-invariant
  verifier (`verifyCaptureInvariants`, `:~208`).
- `src/lir/checked_pipeline.zig:237-240`: SpecConstr then
  `recomputeCaptures` sequencing.
- Review provenance: 2026-07 cor-vs-production review, lambda-lifting
  stage report (C2, C3, open question 4).

## Solution design

1. **L1 — one driver.** Extract the fixpoint loop into a single function
   parameterized by its input program view, and have both the Lifter and
   `recomputeCaptures` call it. The diff should delete one of the two
   loops; if the extraction reveals a real difference between them (not
   just drift potential), that difference is a finding — document which
   behavior is correct and pin it with a test before unifying.
2. **L2 — resolve the binder question at the producer.** Read the
   Monotype lowering site that emits `if_initialized_payload` and decide:
   if `payload` is construct-bound, add it to the bound set in
   `collectExpr` (mirroring match binders) and confirm no capture slot
   for it survives — the over-capture disappears; if it is an outer
   local, state that in a comment on both the collect and rewrite arms
   citing the producer. Either way the ambiguity dies.
3. **L3 — verify the override.** Extend the keyed capture verifier to
   cover the override path: when `operandValueForSlotId` accepts a value
   whose current id differs from the operand's stored id, Debug-assert
   the *stored* id does not match any other slot in the same span (the
   coincidental-collision case). If spec_constr can be cheaply taught to
   rewrite stored operand ids at substitution time instead, prefer that —
   it deletes the tolerance rather than guarding it — but only if the
   rewrite is genuinely local; otherwise the assert suffices.

## What success looks like

- Exactly one capture-fixpoint driver exists; `recomputeCaptures` and the
  Lifter call the same function.
- `if_initialized_payload`'s payload is either bound (with the capture
  slot gone) or documented as an outer local at both arms — not silently
  split between the two readings.
- The override path is covered by the verifier; a synthetic
  colliding-id case trips it in Debug.

## How to evaluate the result

### Correctness ideal

- Post-spec_constr capture sets are the same computation as initial-lift
  capture sets, by construction. The existing verifier (which already
  runs after both) becomes a check on one algorithm, not a referee
  between two.
- Behavioral: full snapshot corpus and cross-opt eval corpus unchanged.
  If L2 resolves to "bound", closures containing the construct shrink by
  one capture slot — confirm via the Debug capture verifier and spot-check
  one affected closure's capture record; no other output may change.

### Performance ideal

Zero release-cost change from L1/L3 (refactor + Debug assert). L2's
bound resolution removes a capture slot from affected closures — a small
size/copy win, not a regression risk. Debug corpus time parity.

## Tests to add

- L1: a unit test running lift, a body-mutating rewrite, and
  `recomputeCaptures`, asserting capture sets match a from-scratch lift
  of the rewritten program (the two-derivations-agree property, now
  trivially true, pinned against future re-forking).
- L2: whichever resolution — a program exercising
  `if_initialized_payload` inside a closure, asserting the expected
  capture set (with or without the payload slot).
- L3: a Debug unit test constructing the coincidental-collision case
  (operand stored id matching a sibling slot, value carrying a different
  live id), asserting the new verifier check fires.

## Related projects

- [spec-constr-specialization-limits.md](./spec-constr-specialization-limits.md)
  — same pass pairing: spec_constr is why `recomputeCaptures` exists.
- [store-generation-counters.md](./store-generation-counters.md) — the
  landed guarded-store work; L1's refactor must keep using the guarded
  span APIs it introduced.
