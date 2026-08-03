# Pin Deferred Spec Requests to Checked Use-Site Types

## Problem

Deferred template requests consume the checker's solved use-site type:
`pinDeferredTemplateRequestToCheckedRoot`
(`src/postcheck/monotype/lower.zig:2695`) instantiates the requested
template's checked function root into the requester's graph at request
creation, so type-level-only facts such as phantom nominal type arguments
reach the request before sealing can default anything. Its doc comment
asserts the resulting invariant: a row that still takes its `row_default`
at seal time is genuinely unconstrained rather than starved.

Three gaps keep that invariant a doc-comment promise rather than a fact:

1. **The pin skips the snapshot regime.** When a request type carries
   generated-opaque evidence, `stableSpecializationRequestType`
   (`lower.zig:2149`) replaces the live `mono_fn_ty` with a detached,
   *sealed* snapshot — and `pinDeferredTemplateRequestToCheckedRoot`
   early-returns exactly there (`monoViewNode(fn_ty) == null` means "a
   sealed snapshot, nothing to pin"). Rows sealed inside
   `stableSpecializationRequestType` take their defaults with no
   checked-root delivery. A request that is both snapshot-regime and
   phantom-row-carrying therefore has an open path to the exact
   #9968-class contradiction panic the pin was built to kill
   ("instantiation unified a non-empty record with an empty record").
   This regime is also where PR #10065's bug lived (the frozen snapshot
   doubling as the evidence back-channel), which is independent evidence
   that the snapshot regime drifts from the pinned regime.
2. **`unifyThroughBacking` never pairs named type arguments.**
   `src/postcheck/monotype/solve.zig:735` relates a named node to a
   structural node through the *backing* type; named arguments are
   paired only on the named↔named path in `unifyConcrete`
   (`solve.zig:602`). Intra-graph nodes that connect only through a
   backing can be starved of named-argument evidence, and if one is,
   sealing silently closes it to its `row_default`.
3. **Nothing observes the invariant.** No counter, assert, or log
   anywhere reports "row defaulted at seal while its checked counterpart
   was concrete" — the failure mode is a downstream panic
   (`unifyRowWithEmpty`, `solve.zig:871`) or, worse, a silently wrong
   default.

## Background

The compiler pipeline: parse → canonicalize → type-check (checked
artifacts; all user-facing failures end here) → postcheck: Monotype IR
(monomorphization; `src/postcheck/monotype/`) → Monotype Lifted → Lambda
Solved → Lambda Mono → LIR → ARC → backends. `design.md` is authoritative;
read "Monotype Instantiation" and "Row, Nominal, Alias, And Opaque
Authority" before starting.

Monotype lowering solves each specialization in a per-spec instantiation
graph (`solve.zig`). Template body requests discovered while lowering a
specialization are deferred and sealed at the end of the requesting
specialization. Each `DeferredTemplate` (`solve.zig:~41`) now carries
three formerly-conflated things as separate fields — the frozen
dedup/seal key (`fn_ty`, possibly a snapshot), the live requester cell
for evidence flow-back (`requester_fn_node`, added by PR #10065), and
checked-root delivery (the pin, added by PR #9997). The pin covers the
live-graph regime; the snapshot regime seals through
`GraphTypeFinals.sealType` inside `stableSpecializationRequestType`
without it. A row node that no evidence has pinned by seal time takes
its `row_default` — the correct treatment for genuinely unconstrained
rows only.

## Evidence

- `lower.zig:2695` `pinDeferredTemplateRequestToCheckedRoot` and its
  early return for snapshot-regime requests; `lower.zig:2149`
  `stableSpecializationRequestType` sealing without checked-root
  delivery; `lower.zig:2065` `monoTypeHasGeneratedOpaqueEvidence`
  deciding the regime.
- `solve.zig:735` `unifyThroughBacking`: the named↔structural path
  appends the backing/other pair without pairing `named.args`.
- `row_default` consumption: `solve.zig` node field, materialization,
  and the row-closing seal sites.
- Regression coverage that must stay green throughout:
  `test/cli/issue_9968_pin_deferred_spec_requests/` (phantom record/tag
  rows, nested nominal, I64-arg variant, concrete-phantom control,
  cross-module package repro) registered in
  `src/cli/test/parallel_cli_runner.zig`, and issue #10021's
  stored-closure evidence tests from PR #10065.

## Solution design

1. **Instrument seal-time defaulting (permanent, debug-only).** At every
   site where a row node closes to its `row_default` during sealing —
   both `sealDeferredSpecRequestsFrom` and the seal inside
   `stableSpecializationRequestType` — compare against the checked type
   behind the request (`source_fn_ty` is on the `DeferredTemplate`);
   assert when the defaulted row's checked counterpart is concrete. This
   converts the pin's invariant from prose to an enforced contract.
2. **Pin the snapshot regime.** `stableSpecializationRequestType` pins
   the checked root before it seals, mirroring what
   `pinDeferredTemplateRequestToCheckedRoot` does for the live regime —
   so the two regimes cannot drift and "row took its default" means the
   same thing in both. Construct the snapshot+phantom repro first (a
   generated-opaque-evidence request whose type carries a phantom
   nominal argument); if the combination is provably unreachable
   instead, write that proof at the early return, citing the invariant
   that excludes it.
3. **Decide `unifyThroughBacking` with evidence.** Run the instrumented
   corpus (snapshot corpus via `zig build run-snapshot-tool`, the CLI
   suite, `examples/`, roc-parser) in debug. If the check fires on a
   backing-only path, extend `unifyThroughBacking` to pair `named.args`
   when relating named to structural; if it fires nowhere, record the
   negative result in `unifyThroughBacking`'s doc comment as
   current-state fact.
4. **The end state that kills the class:** every row node that has a
   checked counterpart is seeded from that checked type at creation, so
   `row_default` is *unreachable* for checker-constrained rows and
   remains only for rows with genuinely no checked origin. Treat 1–3 as
   the staged path there; if seeding-at-creation turns out to be
   directly implementable in this project's scope, prefer it and let the
   instrumentation become the proof it holds.
5. **Enforcement stays.** The seal-time contradiction panic
   (`unifyRowWithEmpty`) remains exactly as it is — the regression
   tripwire, not a condition to soften.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- Both seal sites (deferred-request sealing and
  `stableSpecializationRequestType`) carry the debug check; there is no
  seal path that closes a row to `row_default` without consulting the
  checked counterpart. Verified by grep: every `row_default` close site
  is either instrumented or unreachable-with-proof.
- The snapshot regime is pinned (or proven unreachable for
  phantom-carrying types, with the proof at the early return). If
  pinned: a repro test exists for snapshot+phantom and passes on all
  engines. There is no regime in which the pin is silently skipped.
- The debug check fires nowhere across: the full snapshot corpus, the
  CLI suite, `examples/`, and the roc-parser package suite — all run in
  debug mode with the check live, and the runs are cited in the PR.
- The `unifyThroughBacking` question is closed in one of exactly two
  ways: named-arg pairing implemented plus a repro test (a
  phantom-carrying named type meeting a structural type only through
  its backing, no deferred request involved), or the negative result
  recorded in the doc comment.
- `test/cli/issue_9968_pin_deferred_spec_requests/` and the #10021
  tests stay green; `unifyRowWithEmpty` is byte-identical.
- Release builds are unchanged: the instrumentation compiles out
  (verified by a release build).

## How to evaluate the result

### Correctness ideal

A deferred request's sealed type is a function of `source_fn_ty` plus
genuine defaults — never of which value-flow edges happened to exist and
never of which regime (live vs snapshot) the request landed in. The
debug check is the enforcement; the solver's conflict-over-rewrite
doctrine is untouched.

### Performance ideal

The instrumentation is debug-only: zero release cost. Snapshot-regime
pinning adds one checked-root instantiation per opaque-evidence request
(same cost the live regime already pays). If named-argument pairing is
added, it appends one pending pair per named argument at
named↔structural meets. Measure Monotype lowering time on the
specialization-heavy corpus (roc-parser, `examples/`); require parity
within noise.

## Tests to add

- The snapshot+phantom repro (or the unreachability proof).
- Debug-corpus check: zero defaulted-while-checked-concrete rows across
  the suites listed above.
- If pairing is added: the backing-only phantom repro asserting build +
  run output.
