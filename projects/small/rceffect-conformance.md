# RcEffect Table Conformance

## Problem

Ownership behavior for every low-level builtin lives in one central
table — `RcEffect`, returned per-op by `LowLevel.rcEffect()`
(`src/base/LowLevel.zig:658`) — and consumed by borrow inference and ARC
insertion (`src/lir/arc_solve.zig`: `result_shares_args` links result to
lender at `:1415`; `result_unique` births a fresh unique allocation at
`:1770`). The architecture is right: ownership is decided once and
carried, not re-decided per backend.

But every row of the table is a hand-written claim about what the
corresponding Zig builtin actually does, and nothing checks the claim.
The flags are expressive enough to describe subtle regimes (a fresh
outer value whose interior shares an argument's allocation; runtime
uniqueness probes that are unique on both paths), which means they are
also expressive enough to describe the builtin *wrongly* — and a wrong
row is not a compile error, not a panic, not a test failure. It is a
silent refcount imbalance in generated code: a leak or a use-after-free.

PR roc-lang/roc#10023 (issue #9953) is the proof: the
`retainsSharingArgs` constructor family set `result_unique = true`
alongside `result_shares_args`, so ARC both linked the result to its
lender argument *and* counted a fresh birth — every successful
`Json.parse` leaked one reference to its input string. The bug lived in
the table, shipped through all backends uniformly (the centralization
worked), and was caught only because one hand-written host-effects test
happened to assert live-allocation counts on that path.

## Background

`RcEffect` (`src/base/LowLevel.zig:495-517`) carries per-op masks and
flags: `retain_args`, `result_borrows_args`, `result_shares_args`,
`result_aliases_consumed_args`, `retain_result`, `result_unique`,
`may_allocate`, `may_retain_or_release`, built through constructor
helpers (`retainsSharingArgs`, `allocatesRetainingArgs`, …). The doc
comments now state the intended semantics precisely (e.g.
`result_unique` means "the outermost allocation has count 1 on return;
interior sharing is irrelevant to the outermost count").

Two kinds of wrongness are possible:

1. **Structurally invalid rows** — combinations no builtin can truthfully
   have, or masks that reference argument positions the op does not have.
   These are checkable at comptime from the table alone.
2. **Non-conforming rows** — structurally valid claims that simply do not
   match what the implementation in `src/builtins/*.zig` does (the #10023
   case). These are only checkable by running the builtin and observing
   refcounts.

Today there is no layer for either: the table is validated by nothing,
and refcount-balance coverage exists only where an individual test
thought to assert it.

## Evidence

- `src/base/LowLevel.zig:495-660`: the flag fields, constructor helpers,
  and the per-op `rcEffect()` switch — hundreds of hand-written rows.
- `src/lir/arc_solve.zig:1415, 1770`: consumption sites whose correctness
  is exactly the table's correctness.
- PR #10023: three constructor-family rows wrong in a way that type-checks,
  passes all functional tests, and leaks.
- The catch that did work: a live-allocation assertion in
  `src/builtins/host_effects_tests.zig` — ad-hoc, per-path, not per-op.

## Solution design

1. **Comptime structural validation.** A comptime block (test-adjacent to
   the table, in `LowLevel.zig`) that iterates every op and rejects rows
   that are internally inconsistent: any mask bit at or above the op's
   arity; `retain_args`/release masks set while `may_retain_or_release`
   is false; `result_borrows_args` overlapping
   `result_aliases_consumed_args` (a result cannot borrow from an
   argument the op consumed); and whatever further implications the flag
   doc comments state, encoded one-for-one. Follow the
   `serde_validation.zig` pattern: violations are compile errors naming
   the op.
2. **Per-op conformance harness.** A debug-build test harness that, for
   each op with a nontrivial `RcEffect` row, executes the builtin through
   the interpreter (or dev backend) with refcounted inputs in each
   uniqueness regime (unique input, shared input) under the counting
   test allocator, and asserts the observed refcount/allocation deltas
   match what the row declares: births match `result_unique`, lender
   liveness matches `result_shares_args`/`result_borrows_args`, retains
   match `retain_args`. Ops whose flags cannot be exercised generically
   (host effects) get targeted cases; the harness must FAIL for any op
   with a nontrivial row and no coverage, so a new builtin cannot ship an
   unverified row.
3. **Document the vocabulary.** One doc comment on `RcEffect` stating the
   proof obligations a row makes, so a new row is written against a
   contract rather than by pattern-matching neighboring rows.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- The comptime validator exists, covers every field of `RcEffect` with at
  least one implication rule, and a deliberately-wrong row (added in a
  test) fails compilation with a message naming the op and the violated
  rule.
- Re-introducing the exact #10023 row (`result_unique = true` on
  `retainsSharingArgs` for an op whose outer value is the argument's
  allocation) is caught: by the comptime layer if expressible there, and
  demonstrably by the conformance harness regardless.
- The conformance harness runs in `zig build test` (debug), covers every
  op whose `rcEffect()` is not `none()`, and fails loudly for any op
  added without coverage — verified by adding a dummy op in a test build
  and observing the failure.
- The #9953 regression (Json.parse input refcount balance) remains as a
  pinned end-to-end test.
- Zero release-build cost: validator is comptime; harness is test-only.
  `rcEffect()` itself is untouched.

## How to evaluate the result

### Correctness ideal

A wrong ownership row cannot ship silently: it is either a compile error
(structural) or a failing debug test naming the op and the mismatched
delta (conformance). The table remains the single source; this project
adds enforcement, not a second source.

### Performance ideal

None at runtime — this is test/comptime machinery only. The harness
should keep total test-suite time reasonable by driving ops through the
interpreter with small values.

## Tests to add

- The negative-compile check for a structurally invalid row (Zig
  `error` tests or a build-step probe, matching how other comptime
  contracts in the repo are tested).
- The per-op conformance sweep itself.
- Pinned regressions: #9953 (str sharing), plus one list op and one str
  op per constructor family so each helper's semantics are exercised.

## Related projects

- [../small/silent-drift-guards.md](../small/silent-drift-guards.md) —
  the same enforcement philosophy for other mirrored facts; this doc is
  the ownership-table instance.
