# Lambda Mono Oracle Fidelity and Dead Machinery

## Problem

Per design.md ("Debug Lambda Mono Verification"), release builds never
materialize the Lambda Mono tree; `src/postcheck/lambda_mono/lower.zig`
runs only inside the Debug verifier (`verifyMaterializedDecisions`,
`src/postcheck/solved_lir_lower.zig:1876`) as the oracle the direct
solved-to-LIR path is checked against. An oracle is only as good as its
own fidelity, and the 2026-07 cor-comparison review found three hygiene
gaps in it — none a shipped-code bug (the file cannot reach release
output), all of them ways the verifier can drift from what it is supposed
to verify, which is the failure mode that makes a verifier worse than
none:

**G1 — `localFor` is first-write-wins with no agreement check.**
`localFor` (`lambda_mono/lower.zig:1222-1230`) caches a local's Lambda Mono
type from whichever caller reaches it first and ignores the type every
later caller passes. Callers derive that type from *different* sources
(function signature args, pattern types, expression types, loop-param
types, payload-local types). In a correctly unified Lambda Solved program
all sources share one root, so the cache is deterministic — but nothing
asserts later callers agree with the cached type. If those sources ever
drift to structurally-equal-but-distinct roots (the codebase's recurring
disease), the oracle's local types become traversal-order-dependent, and
the verifier would be comparing the direct path against a nondeterministic
reference. cor avoids this structurally: within one specialization its
`ty_cache`/`venv` share one physical tvar per logical variable
(`lambdamono/type_clone_inst.ml`).

**G2 — the folded-match replay channel is unpinned.** The direct lowerer
folds statically-impossible `list_map_can_reuse` matches and records each
fold as explicit data (`FoldedMatch`, appended Debug-only at
`solved_lir_lower.zig:3730`); the oracle replays the recorded resolutions
(`lambda_mono/lower.zig:617-623`) by emitting the folded body and skipping
the scrutinee entirely. Today this is sound because the only recorded
scrutinees are compiler-synthesized `list_map_can_reuse` ops — pure, no
user code, no bindings (`Lifted.Program.exprIsListMapCanReuseOp`,
`monotype_lifted/ast.zig:~982`). But the replay site checks none of that:
any future producer that appends a `FoldedMatch` with an effectful
scrutinee or a binding-carrying pattern would be replayed as silent
elision, and the oracle would ratify whatever the direct path did instead
of checking it.

**G3 — dead parallel machinery.** `lambda_mono/specialize.zig` exports a
`Queue` (`:15`) with O(n²) `std.meta.eql` dedup (`:27-34`) that the
lowerer does not use (it uses `fn_spec_map`). It is a vestigial parallel
of the cor prototype's `Specializations` module — the
dead-but-still-maintained pattern the duplication audit targets, one file
over from live code that does the same job.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR → Monotype Lifted → Lambda Solved → **Lambda Mono decisions +
Debug oracle** → LIR → ARC → backends. design.md's "Debug Lambda Mono
Verification" section defines the oracle's contract: materialized in Debug
only, never an input to production lowering, checked against the direct
path's decisions. The direct path records the decisions; the oracle
re-derives them independently — so every place the oracle *copies* a
direct-path decision instead of re-deriving it (the folded-match channel
is exactly such a place, by documented necessity: the oracle runs before
layout selection and cannot recompute layout-dependent folds) must pin
what it is copying, or that slice of the verifier is circular.

## Evidence

All symbols verified in the current tree.

- `src/postcheck/lambda_mono/lower.zig`: `localFor` first-write-wins
  (`:1222-1230`); typed sources feeding it from signatures (`:~354`),
  patterns (`:~919`), expressions (`:~547`), loop params (`:~1274`),
  payload locals (`:~637-652`); folded-match replay (`:617-623`).
- `src/postcheck/solved_lir_lower.zig`: fold decision + Debug-only record
  (`foldListMapCanReuseMatch`, `:3713-3736`, append at `:3730`) with the
  layout-interchangeability bailout that keeps genuinely layout-dependent
  matches unfolded (`:3724-3728`); oracle invocation passing
  `folded_map_matches` (`:1884`).
- `src/postcheck/monotype_lifted/ast.zig`: `FoldedMatch` contract comment
  ("recorded so the debug materializer replays the identical resolution",
  `:~977`), `exprIsListMapCanReuseOp` (`:~982`).
- `src/postcheck/lambda_mono/specialize.zig`: unused `Queue` (`:15`),
  linear-scan `enqueue` (`:27-34`). Consumers: only
  `postcheck/mod.zig`'s export and `refAllDecls`-style test references
  (`src/collections/guarded_list_violation_test.zig` uses the *ast/type*
  modules, not `Queue` — confirm during implementation with a final grep).
- Review provenance: 2026-07 cor-vs-production review, Lambda Mono stage
  report (divergences D4, C1; performance note PERF-3).

## Solution design

1. **G1 — assert agreement on every `localFor` call.** The whole file is
   Debug-only, so the check is free where it matters: when the cache hits,
   assert the passed `ty` equals the cached `ty` (`Common.invariant` on
   mismatch, per policy). This converts "all five type sources agree" from
   folklore into a machine-checked property of every Debug run — which is
   also exactly the early-warning tripwire for root-drift upstream in
   Lambda Solved.

2. **G2 — pin the replay channel's contract.** At the replay site (or at
   `FoldedMatch` append), assert the recorded scrutinee satisfies
   `exprIsListMapCanReuseOp` — the structural predicate already written —
   so the channel's "pure, binding-free, compiler-synthesized" premise is
   enforced, not assumed. A future fold kind then has to either satisfy
   the predicate or extend it consciously, with the contract comment
   updated in the same commit.

3. **G3 — delete `Queue`.** Remove it and its export; final grep for
   consumers first. If anything does consume it, that consumer is using
   O(n²) dedup one file away from the O(1) live index and should be
   migrated in the same change.

## What success looks like

- A Debug corpus run (snapshot corpus + CLI suite) passes with the G1 and
  G2 asserts active and zero firings.
- `lambda_mono/specialize.zig` contains only what the live path uses.
- The oracle's copied-decision surface (folded matches) is exactly
  enumerated: one channel, one structural predicate, both asserted.

## How to evaluate the result

### Correctness ideal

- The verifier verifies: every value it compares against the direct path
  is either independently re-derived or pinned by an asserted structural
  contract — no unchecked copies.
- G1's assert doubles as the detection mechanism for the Lambda Solved
  root-sharing invariant (see related project): if type sources ever
  stop sharing roots, Debug CI fails at the first affected local with a
  named site, instead of the oracle silently absorbing the drift.

### Performance ideal

All three changes are Debug-only or deletions; release builds are
bit-identical (verify by hashing `--opt=speed` output on the corpus).
Debug-build verifier time may rise negligibly from the per-call type
compare; measure Debug corpus wall time, require parity within noise.

## Tests to add

- G1 tripwire: a unit test over the oracle constructing a program whose
  local is reached with two distinct (structurally equal, different-id)
  types; asserts the new invariant fires. This is the negative control
  proving the assert works.
- G2 tripwire: unit test appending a `FoldedMatch` whose scrutinee is not
  a `list_map_can_reuse` op; asserts the replay-site invariant fires.
- G2 positive: the `List.map` fold path end-to-end in Debug (a program
  where element layouts are not interchangeable, so the fold fires),
  asserting the oracle verification passes — pins that the legitimate
  channel still flows.
- Grep-level guard in `ci/semantic_audit.pl` if `Queue` deletion reveals
  the export pattern recurring elsewhere (optional; judgment call during
  implementation).

## Related projects

- [pin-lambda-solved-invariants.md](./pin-lambda-solved-invariants.md) —
  G1's assert is the downstream detector for that project's root-sharing
  invariant; land in either order, they reinforce each other.
- The Lambda Mono differential harness
  (`src/eval/test/lambda_mono_differential_runner.zig`, run with
  `zig build run-test-lambda-mono-differential`) executes the materialized
  oracle tree against the LIR interpreter over the eval corpus — it extends
  what the oracle *covers*; this project fixes what the oracle *is*. A
  higher-fidelity oracle strengthens every comparison that harness makes.
