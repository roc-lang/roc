# Coverage-Parity Tests for Cross-Phase Predicates

## Problem

Divergence is computed twice across the checked-artifact boundary.
`checkedExprDataDiverges` (src/check/checked_artifact.zig) decides which
expression kinds can be marked divergent; `lowerDivergentExprDataAtType`
(src/postcheck/monotype/lower.zig) decides which kinds have a divergent
lowering path. Both switches are comptime-exhaustive with no `else`, so a
NEW expression kind forces an arm in each — but the two CLASSIFICATIONS
are kept in agreement only by hand. The consumer partitions kinds into
real lowering arms plus one `Common.invariant("non-divergent checked
expression reached divergent lowering")` arm, and nothing checks that this
partition matches the set the producer can mark. Re-classify one side
without the other — say, make the producer propagate divergence through a
kind the consumer lists as never-divergent — and the failure is a runtime
panic on some future user program, not a test failure at development time.
Issue roc-lang/roc#9696 shipped exactly this way for `if`.

The consumer's inner invariants have the same exposure: the `binop`,
`structural_eq`, `structural_hash`, and `record` arms panic when no
divergent child is found, and the `expect` arm panics when inline expects
are omitted — all runtime-only checks of producer/consumer agreement.

## Background

The compiler pipeline: parse → canonicalize → type-check (src/check/Check.zig;
unification in src/check/unify.zig; rows are how records and tag unions are
typed — a row can be open or closed) → checked artifacts → postcheck:
Monotype IR → Lifted → Lambda Solved → Lambda Mono → LIR → ARC → backends.
design.md at the repo root is authoritative.

The checker publishes checked artifacts (src/check/checked_artifact.zig)
that postcheck consumes without re-inference. The other predicates that
cross this boundary are single-sourced: refutability is one shared
predicate (`canMiss` in src/canonicalize/pattern_refutability.zig) that
`patternNeedsExhaustiveness` (src/check/Check.zig) and the `patternCanMiss`
helpers in src/postcheck/monotype/lower.zig, src/postcheck/lir_lower.zig,
and src/postcheck/solved_lir_lower.zig all call through per-IR adapters;
comptime-exhaustiveness site classification is one shared module
(src/check/exhaustiveness_context.zig) held by both the checker
(`exhaustiveness_context` field) and monotype lowering
(`comptime_exhaustiveness_context`); problem-kind lowerability
(`problemAllowsLoweringWithUserErrors` in src/compile/compile_package.zig)
is a comptime-exhaustive switch with rationale-per-kind tests. Divergence
is the one cross-phase predicate still enforced only by runtime
invariants.

## Evidence

- Producer: `checkedExprDataDiverges` (src/check/checked_artifact.zig) —
  exhaustive switch, no `else`. Marks `crash` / `ellipsis` / `break_` /
  `return_` / `expect_err` divergent directly and 21 more kinds via
  divergent children (26 markable total); lists 20 kinds (`closure`,
  `lambda`, `hosted_lambda`, `pending`, `numeral`, lookups, literals,
  `dispatch_call`, `runtime_error`, ...) as never divergent.
- Consumer: `lowerDivergentExprDataAtType`
  (src/postcheck/monotype/lower.zig) — exhaustive switch, no `else`; a
  lowering arm for every kind the producer can mark, and one invariant arm
  listing the never-divergent kinds. The two never-divergent lists match
  by inspection only; no test compares them.
- Runtime-only agreement checks: `Common.invariant` calls inside the
  consumer's `binop` / `structural_eq` / `structural_hash` / `record` /
  `expect` arms and the shared never-divergent arm.
- The only divergence test is the 9696-shape regression: "fx platform
  divergent if with all crash branches does not hit postcheck invariant"
  (src/cli/test/fx_platform_test.zig), driving
  test/fx/divergent_if_all_branches_crash_repro.roc. No fixture exercises
  the other divergent kinds — e.g. a divergent `.nominal` via
  `Ok({ crash "x" })`, a divergent `.call` argument, a divergent `.binop`
  operand.
- No parity-test convention appears in CONTRIBUTING.md or AGENTS.md for
  producer/consumer predicate pairs.

## Solution design

1. **Divergence parity suite** — ordinary Zig tests run by
   `zig build test`: a table-driven fixture that builds a minimal
   checked-artifact instance of every expression kind in a divergent
   position (wrapping a `crash` child) and asserts that a producer-marked
   expression lowers through `lowerDivergentExprDataAtType` without
   reaching any invariant arm. Use comptime reflection over the
   `CheckedExprData` enum to require one fixture (or an explicit
   never-divergent justification entry) per variant, so a newly added
   variant is a compile error in the test until it is classified.
2. **Pinned regression fixtures** for representative kinds:
   `Ok({ crash "x" })` (divergent `.nominal`), a divergent `.call`
   argument, a divergent `.binop` operand — alongside the 9696 `if` shape.
3. **Write the pattern down** where contributors see it (CONTRIBUTING.md):
   a producer enumeration consumed by a separate hand-enumerated switch in
   another phase ships with a parity test, and extracting one shared
   predicate (the pattern_refutability.zig / exhaustiveness_context.zig
   shape) is the structural alternative when feasible.

Nothing in the compiler's runtime path changes; the suite is tests only.

## What success looks like

- Marking an expression kind divergent in the producer while the consumer
  classifies it never-divergent fails `zig build test`, naming the
  variant — not a user's build, months later.
- Representative divergent-kind fixtures are pinned so the arms that exist
  for them cannot silently regress to invariants.
- A CONTRIBUTING-visible convention exists: new producer/consumer
  predicate pairs ship with a parity test or a shared predicate.

## How to evaluate the result

### Correctness ideal

- Coverage is total over `CheckedExprData` variants BY CONSTRUCTION: the
  test enumerates variants from the type itself, so coverage cannot rot
  through fixture-list discipline.
- The suite fails with a message naming the exact variant and which side
  (producer or consumer) drifted.
- The consumer's runtime invariants remain as last-resort backstops but
  are no longer the FIRST line of defense against classification drift.

### Performance ideal

- Zero compiler runtime cost: tests only; no new checks, passes, or data
  structures in the compiled compiler.
- The suite runs in seconds as part of `zig build test` — fixtures are
  minimal constructed checked-artifact instances, not end-to-end compiles.

## Tests to add

1. Comptime-enumerated fixture per `CheckedExprData` variant in a
   divergent position; assert producer-marked implies consumer-lowerable
   without invariants; explicit justification entries for never-divergent
   variants.
2. Pinned fixtures: `Ok({ crash "x" })` (divergent `.nominal`), divergent
   `.call` argument, divergent `.binop` operand, the 9696 `if` shape.

## Related projects

- The decision-tree match compiler has landed: match lowering goes through
  one authoritative analysis in src/postcheck/match_tree.zig, so pattern
  coverage no longer depends on per-IR refutability adapter walks.
