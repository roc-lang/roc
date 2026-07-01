# Coverage-Parity Tests for Cross-Phase Predicates

## Problem

Several facts in the compiler are computed by a PRODUCER enumeration in one
phase and consumed by a SEPARATE hand-enumerated switch in another phase,
with no mechanical coupling between the two. When the enumerations drift —
a new variant on the producer side, a missing arm on the consumer side —
the failure mode is a user-visible compiler panic on some future program,
not a test failure at development time.

This has already happened: issue roc-lang/roc#9696 was a panic because the
divergence producer could mark an `if` expression divergent while the
Monotype consumer had no lowering arm for it; PR roc-lang/roc#9719 added
the `.if_` arm AFTER the panic shipped. An audit of the two switches today
shows the same gap is still open for roughly seventeen more expression
kinds (eighteen by direct comparison; see Evidence). Nothing prevents the
next drift, and nothing detects it before a user does.

## Background

The compiler pipeline: parse → canonicalize → type-check (src/check/Check.zig;
unification in src/check/unify.zig; rows are how records and tag unions are
typed — a row can be open or closed) → checked artifacts → postcheck:
Monotype IR → Lifted → Lambda Solved → Lambda Mono → LIR → ARC → backends.
design.md at the repo root is authoritative.

The checker publishes checked artifacts (src/check/checked_artifact.zig)
that postcheck consumes without re-inference. Several properties cross that
boundary as PREDICATES rather than as explicit data: "this expression
diverges", "this pattern can fail to match", "this exhaustiveness site is
compile-time-only", "this problem still permits lowering". Each side of the
boundary encodes the predicate as its own switch over its own IR's variants.
The two switches are supposed to agree; agreement is currently enforced by
nothing (cases 1, 2), by runtime panics during compile-time finalization
(case 3), or by a conservative default chosen by human judgment (case 4).

## Evidence

Four concrete producer/consumer pairs, all verified in the current tree:

1. **Divergence.** Producer: `checkedExprDataDiverges` in
   src/check/checked_artifact.zig can mark ~26 expression kinds divergent —
   `crash`/`ellipsis`/`break_`/`return_`/`expect_err` directly, and
   `str`, `list`, `tuple`, `match_`, `if_`, `call`, `record`, `block`,
   `tag`, `nominal`, `binop`, `unary_minus`, `unary_not`, `dbg`, `expect`,
   `field_access`, `structural_eq`, `structural_hash`, `tuple_access`,
   `for_`, `run_low_level` via divergent children. Consumer:
   `lowerDivergentExprDataAtType` in src/postcheck/monotype/lower.zig
   hand-enumerates NINE arms (`block`, `match_`, `if_`, `ellipsis`,
   `crash`, `runtime_error`, `expect_err`, `break_`, `return_`) and ends
   with `else => Common.invariant("checked expression was marked divergent
   but has no divergent lowering path")`. Direct comparison: 18 kinds the
   producer can mark that have NO consumer arm — e.g. a divergent
   `.nominal` via `Ok({ crash "x" })`, a divergent `.call` argument, a
   divergent `.binop` operand. The 9696-class panic is open TODAY for all
   of them. Divergence is also independently re-derived at least four
   times across the pipeline: `checkedExprDataDiverges` (producer),
   `checkedExprDivergesInLoweredRuntime` /
   `checkedStatementDivergesInLoweredRuntime` (monotype/lower.zig, because
   optimized builds omit inline expects), and the statement-position
   variants `exprAlwaysCrashes` / `exprIsAllCrashConditional` in
   src/check/Check.zig from PR 9719 — the latter recognizes only `e_if`
   (looking through `e_block` finals).

2. **Refutability.** Three syntactic predicates over three different IRs:
   `patternNeedsExhaustiveness` (src/check/Check.zig, over CIR patterns),
   `patternCanMiss` (src/postcheck/monotype/lower.zig `BodyContext`, over
   checked patterns), and `patternCanMiss` (src/postcheck/lir_lower.zig
   `Lowerer`, over Lambda Mono patterns). Their verdicts are NOT identical:
   the Check.zig and monotype versions treat every `.list` pattern as
   refutable, while the LIR version knows `[..]` / `[.. as rest]` is
   irrefutable. All disagreements happen to err conservative today (extra
   miss paths, never missing ones), but nothing checks the three against
   each other or against the exhaustiveness matrix's verdict.

3. **Comptime-site classification.** Two parallel depth counters that must
   stay synchronized: `empirical_exhaustiveness_depth` (src/check/Check.zig
   checker field) classifies exhaustiveness diagnostics under compile-time
   value construction as "empirical candidates";
   `comptime_exhaustiveness_depth` (src/postcheck/monotype/lower.zig,
   specialization-context field) must classify the SAME sites the same way
   during lowering. Agreement is enforced only by finalization panics in
   src/eval/compile_time_finalization.zig (`finalizationInvariant("empirical
   exhaustiveness failure had no pending static diagnostic")`, "...had an
   impossible site policy") — PR roc-lang/roc#9642's regime. Its site
   identity already broke once within five days of landing, when PR
   roc-lang/roc#9722's hoisting duplicated checked regions.

4. **Error-recovery lowerability.** `problemAllowsLoweringWithUserErrors`
   in src/compile/compile_package.zig (from PR roc-lang/roc#9819) decides
   per problem kind whether lowering may proceed with user errors present.
   Discrepancy with the expected shape, noted per the code: the top-level
   switch is NOT comptime-exhaustive — it enumerates only
   `.static_dispatch` (whose inner switch IS exhaustive) and ends with
   `else => false`. New problem kinds therefore get a silent conservative
   default by omission, not an explicit entry by human judgment: safe
   against panics, but silently degrades error-recovery behavior with no
   forcing function to consider each new kind.

## Solution design

Build an architecture-test suite — ordinary Zig test files, run by
`zig build test` — that mechanically asserts parity for each pair. The
suite is also a copyable PATTERN: future producer/consumer pairs get parity
tests by convention, with these four as the templates.

1. **Divergence parity suite.** A table-driven fixture that constructs a
   minimal checked-artifact instance of EVERY expression kind placed in a
   divergent position (e.g. wrapping a `crash` child), then asserts: if
   `checkedExprDataDiverges` marks it, `lowerDivergentExprDataAtType`
   lowers it without reaching the `else => invariant` arm. Where feasible,
   use comptime reflection over the `CheckedExprData` enum to force the
   fixture list to cover every variant — a newly added variant becomes a
   COMPILE ERROR in the test until a fixture (or an explicit
   cannot-be-divergent justification entry) is added. The FIRST deliverable
   of this suite is triaging the 18 currently-open kinds: for each, either
   fix the consumer (add a lowering arm), fix the producer (prove the kind
   cannot be marked divergent and encode that), or document why the
   combination is unreachable — with the test enforcing whichever answer.
2. **Refutability parity suite.** A generated pattern corpus covering every
   pattern kind of each IR, including nested combinations (tags in tuples,
   lists in records, `as` wrappers, nominal backing patterns). For each
   pattern, run all three predicates on the corresponding representation
   and assert either agreement, or that any disagreement is in the
   documented-conservative direction (a later phase may say "cannot miss"
   only if an earlier phase agreed, never the reverse). Cross-check against
   the exhaustiveness matrix where a checked type is available.
3. **Depth-counter parity suite.** Preferred fix: EXTRACT one shared
   site classifier that both `empirical_exhaustiveness_depth` maintenance
   in Check.zig and `comptime_exhaustiveness_depth` maintenance in
   monotype/lower.zig call, so divergence is structurally impossible.
   If extraction proves impractical across the two IRs, the fallback is a
   cross-check test: compile a set of representative programs (comptime
   constructions containing matches, destructures, hoisted roots, nested
   guards) and assert the two counters classify every exhaustiveness site
   identically — turning the finalization panic into a test failure.
4. **Problem-kind suite.** Restructure `problemAllowsLoweringWithUserErrors`
   so the top-level switch is comptime-exhaustive over problem kinds (no
   `else`), forcing an explicit entry per kind; then add a test asserting
   every entry carries an explicit rationale — either by restructuring into
   a table with a required `rationale` field per row, or by a doc-comment
   convention checked in review. The conservative meaning of "not listed"
   is preserved by writing it explicitly.

Nothing in the compiler's runtime path changes except where a suite's first
deliverable finds a real bug (the divergence arms, possibly a shared site
classifier). What gets DELETED: nothing initially; if suite 3's shared
classifier lands, the duplicated per-phase classification logic around the
two depth counters is deleted in favor of the shared function.

## What success looks like

- Adding a divergence-markable expression kind without a corresponding
  lowering arm fails `zig build test` — not a user's build, months later.
- The 18 open divergence kinds are resolved: lowered, producer-restricted,
  or proven unreachable, each with a test pinning the answer.
- The three refutability predicates are either in agreement or their
  disagreements are enumerated in one test file with the conservative
  direction asserted.
- The two exhaustiveness depth counters cannot silently diverge: either one
  shared classifier exists, or the cross-check test covers every site kind.
- Every problem kind has an explicit, visible lowerability decision.
- A CONTRIBUTING-visible pattern exists: new producer/consumer predicate
  pairs ship with a parity test.

## How to evaluate the result

### Correctness ideal

- The parity property is total over enum variants BY CONSTRUCTION: where
  comptime reflection is feasible, the test enumerates variants from the
  type itself, so coverage cannot rot through fixture-list discipline. Only
  where reflection is genuinely infeasible does a hand-maintained fixture
  list (guarded by a variant-count assertion) substitute.
- Each suite fails with a message naming the exact variant and the exact
  producer/consumer pair that drifted.
- The finalization invariants in src/eval/compile_time_finalization.zig
  remain as last-resort backstops but are no longer the FIRST line of
  defense for counter drift.

### Performance ideal

- Zero compiler runtime cost: these are tests only; no new checks, passes,
  or data structures in the compiled compiler (except the optional shared
  site classifier, which replaces duplicated logic rather than adding any).
- The whole suite runs in seconds as part of `zig build test` — fixtures
  are minimal constructed IR instances, not end-to-end compiles, except the
  small representative-program set in suite 3.

## Tests to add

This project IS tests. The four suites, concretely:

1. Divergence: comptime-enumerated fixture per `CheckedExprData` variant in
   a divergent position; assert producer-marked implies consumer-lowerable;
   regression fixtures for `Ok({ crash "x" })` (divergent `.nominal`) and
   the original 9696 `if` shape.
2. Refutability: generated nested-pattern corpus; three-way predicate
   agreement with documented-conservative exceptions; list-rest patterns
   (`[..]`, `[.. as rest]`, `[x, ..]`) as pinned cases.
3. Depth counters: representative comptime programs; assert per-site
   classification parity between checker and monotype lowering (or unit
   tests of the extracted shared classifier).
4. Problem kinds: comptime-exhaustive switch (compile failure on a new
   kind) plus the rationale-per-entry assertion.

## Related projects

- [../big/decision-tree-match-compiler.md](../big/decision-tree-match-compiler.md)
  — consumes the refutability suite: a decision-tree match compiler
  replaces per-IR `patternCanMiss` walks with one authoritative analysis.
- [../big/total-dispatch-plans.md](../big/total-dispatch-plans.md) — the
  same explicit-data discipline applied to dispatch, shrinking the set of
  cross-phase predicates that need parity tests at all.
- [../small/check-app-against-platform-requires.md](../small/check-app-against-platform-requires.md)
  — another boundary where producer and consumer enumerations must agree.
- [./structural-hoist-contexts.md](./structural-hoist-contexts.md) — PR
  9722's hoisting broke comptime-site identity (suite 3's history); its
  structural-context fix removes one source of site duplication.
