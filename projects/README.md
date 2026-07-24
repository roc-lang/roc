# Compiler Improvement Projects

This folder contains self-contained project specifications for structural
improvements to the compiler. Each `.md` file is written so that someone brand
new to the codebase (human or agent) can read that one file and understand the
problem, the solution approach, what success looks like, how to evaluate the
result for long-term correctness and performance, and what tests to add. Each
doc's "What success looks like" section is a completion contract: the project
is not done until every criterion listed there holds.

- `small/` — localized, mostly additive checks or deletions, low design risk;
  hours to days each.
- `big/` — projects on the order of weeks each: cross-cutting, and several
  require a design decision before implementation starts.

The projects come from a root-cause analysis of eight weeks of bug fixes
(May–June 2026), a July 2026 duplication audit, and a July 2026 differential
re-analysis of the fixes that landed since. The recurring disease across
independent bug clusters was: facts proven during checking get re-derived
downstream from type, name, or structure content instead of traveling as
explicit data, keyed by fragile identity (name strings, positional order,
mutable keys) and enforced only by panics at the consumption site. The
re-analysis added a second-order lesson: generators whose mechanism was
*deleted* stayed dead, while generators that were centralized but left a
parallel old path kept firing on uncovered axes — so each project's finishing
move is deleting the re-derivation path, not just adding the carried fact
beside it. `design.md` at the repo root is the authoritative post-check
design; these projects implement its stated principles more completely.

A second batch of projects came out of the 2026-07 comparative review of
the post-check pipeline against the cor `lss` prototype it was
productionized from (stage-by-stage divergence review; no shipped
miscompile found, but several unstated load-bearing invariants, one
termination hazard, and one verification coverage gap):

- [small/spec-constr-specialization-limits.md](small/spec-constr-specialization-limits.md)
  — termination budgets for call-pattern specialization (compile-time
  divergence is reachable today).
- [small/empty-tag-union-yield-provenance.md](small/empty-tag-union-yield-provenance.md)
  — key Lambda Solved's one unification escape hatch on carried provenance
  instead of shape.
- [small/pin-lambda-solved-invariants.md](small/pin-lambda-solved-invariants.md)
  — state, assert, and test the four invariants that make monomorphic
  lambda-set solving sound.
- [small/lambda-mono-oracle-fidelity.md](small/lambda-mono-oracle-fidelity.md)
  — agreement asserts and contract pins for the Debug Lambda Mono oracle;
  delete its dead Queue.
- [small/monotype-machinery-hardening.md](small/monotype-machinery-hardening.md)
  — release-gate verification-only type checks; measure-first fixes for
  digest depth fallback, unify memo, spec duplication, cross-store reuse.
- [small/lift-capture-single-sourcing.md](small/lift-capture-single-sourcing.md)
  — one capture-fixpoint driver, the `if_initialized_payload` binder
  question, and the capture-id override path.

Within this batch the projects are independent.
`spec-constr-specialization-limits` pairs naturally with
`spec-constr-static-match-soundness`.

A third batch came out of a whole-codebase competing-sources-of-truth
audit (2026-07-18): a sweep of every subsystem for the same fact
encoded independently in N places with no cross-check. The sweep also
confirmed the prevailing single-source pattern holds in most places
(escape alphabet, `LowLevel` vocabulary, `layout/abi` classification,
`RocTarget`, precedence table, snapshot file format, serde audits) —
these projects close the holdouts. All are independent of the earlier
batches and of each other:

- [big/runtime-representation-single-sourcing.md](big/runtime-representation-single-sourcing.md)
  — backends stop re-encoding RocStr/RocList offsets, flag bits, the
  refcount contract, and C-ABI thresholds as magic numbers.
- [big/host-boundary-single-sourcing.md](big/host-boundary-single-sourcing.md)
  — glue templates, glue-platform struct mirrors, shim symbol
  strings, and test-host boilerplate get generated or lock-tested
  against `host_abi.zig`/builtins.
- [big/one-report-renderer.md](big/one-report-renderer.md) — collapse
  the four per-target diagnostic renderers onto one walker plus style
  data; delete the duplicated annotation-color switch.
- [small/llvm-conversion-op-explicit-dispatch.md](small/llvm-conversion-op-explicit-dispatch.md)
  — end the LLVM backend's `@tagName`-substring dispatch for numeric
  conversion ops (the one backend exempt from switch exhaustiveness).
- [small/bundle-unbundle-shared-path-rules.md](small/bundle-unbundle-shared-path-rules.md)
  — one archive path-safety validator; the writer's and reader's
  copies already disagree.
- [small/nodestore-serde-enrollment.md](small/nodestore-serde-enrollment.md)
  — comptime-drive NodeStore's eight hand-enumerated field lists;
  derive the parse-side node counts.
- [small/frontend-name-and-sentinel-single-sourcing.md](small/frontend-name-and-sentinel-single-sourcing.md)
  — six frontend seams: duplicate NumKind map, hardcoded Bool
  discriminant, method-name literals, hand-written builtin-name maps,
  five row comparators, default-cased lowering switches.
- [small/syntax-fact-single-sourcing.md](small/syntax-fact-single-sourcing.md)
  — keyword spellings (tokenizer vs ~25 fmt literals), the
  numeric-suffix bidirectional pair, the twice-scanned number
  grammar.
- [small/severity-and-report-collection.md](small/severity-and-report-collection.md)
  — `Severity.isError`/`toLspSeverity` helpers; snapshot tool and
  playground call the compiler's report-collection loop instead of
  copying it.
- [small/lsp-and-docs-truth-reuse.md](small/lsp-and-docs-truth-reuse.md)
  — the forked doc-comment gatherer (LSP and docs already disagree on
  `###`), three line/column implementations, the positional
  semantic-token legend, the hand-copied completion roster.
- [small/build-and-ci-single-lists.md](small/build-and-ci-single-lists.md)
  — one module inventory (seven restatements plus minici's copy, with
  existing test-coverage divergence), one CI gate list, one Zig pin.
- [small/cli-declarative-flags.md](small/cli-declarative-flags.md)
  — each subcommand's struct/parser/help triple becomes one table;
  target rosters and defaults render from their enums.

## Recommended order

### Start here — enforcement layers, cheap and load-bearing

1. [small/cross-phase-coverage-parity-tests.md](small/cross-phase-coverage-parity-tests.md)
   — the divergence-classification parity suite; a regression net the big
   lowering projects inherit.
2. [small/silent-drift-guards.md](small/silent-drift-guards.md)
   — finishes the monotype identity unification: one identity-field
   visitor for digest and equality, and alias-transparent cached digests.
3. [small/rceffect-conformance.md](small/rceffect-conformance.md)
   — comptime validity plus a per-op refcount conformance harness for the
   central ownership table (the PR 10023 bug class).
4. [small/cache-and-identity-residuals.md](small/cache-and-identity-residuals.md)
   — closes the four small seams left after the identity/cache cures
   (name-text fallback, hand-enrolled serde contracts, split version
   hashes, `type_name` in nominal keys).

### Chain A — dispatch evidence, consumed everywhere

1. [small/hoist-consumes-dispatch-evidence.md](small/hoist-consumes-dispatch-evidence.md)
   — hoist selection reads the dispatch resolution instead of re-deriving
   evidence-dependence from type-var content (the PR 10073 seam), and
   recovers the hoisting its conservative gate gives up.

### Chain B — the host/platform boundary

1. [small/hosted-extern-declared-abi.md](small/hosted-extern-declared-abi.md)
   — the invariant that a hosted extern is only specialized at its declared
   type, enforced at the producer instead of by a checker rewrite.

The solver-rewrite audit that followed this chain has landed: design.md's
"Solver-Mutating Rewrites" section holds the mechanism/policy inventory,
`Store.dangerousSetVarRedirect` requires a declared `RedirectRule` at every
call site, and the 9966 widening is the declared Hosted Try Question
Widening rule, scoped to direct hosted calls with both sides pinned by
tests.

The platform-relation migration that concluded this chain has landed:
checking records each platform requirement's solution in the app's checked
artifact, finalization consumes it, and the platform root is published
once — the name-keyed re-resolution and double publication are deleted.

### Chain C — specialization sealing

1. [small/pin-deferred-spec-requests.md](small/pin-deferred-spec-requests.md)
   — seal-time instrumentation, the snapshot-regime pin, and the
   `unifyThroughBacking` decision; end state: `row_default` unreachable
   for checker-constrained rows.

### Independent — start any time, in any order

Small:
- [small/frame-partitioned-checker-state.md](small/frame-partitioned-checker-state.md)
  — inventory and convert frame-scoped checker/canonicalizer state to
  dedicated frame storage (the 9929→10010 and 10001 shape).
- [small/compact-constant-aggregates.md](small/compact-constant-aggregates.md)
  — static-data and builtin-call materialization for constant/repeated
  lists, ending the one-local-per-element explosion behind issue 9898.

The decision-tree match compiler has landed: both LIR pipelines lower
`match` through one shared Maranget-style module
(src/postcheck/match_tree.zig) — one multiway switch per tested position,
one discriminant read, strings and list-length buckets as ordinary arms —
with the sharing invariant documented in design.md and enforced by a debug
statement-count lint.

Single-source builtin registration has landed: the seven hand-typed
`roc_builtins_*` symbol/ABI tables now derive from one comptime registry
(src/builtins/builtin_registry.zig), and the LowLevel-to-builtin member
choice is shared across backends via `base.LowLevelBuiltins`.

### Suggested overall sequence

If one person or agent works through everything serially, this order
front-loads leverage and keeps prerequisites satisfied:

1. `small/cross-phase-coverage-parity-tests.md`
2. `small/silent-drift-guards.md`
3. `small/rceffect-conformance.md`
4. `small/cache-and-identity-residuals.md`
5. `small/pin-deferred-spec-requests.md`
6. `small/hoist-consumes-dispatch-evidence.md`
7. `small/hosted-extern-declared-abi.md`
8. `small/frame-partitioned-checker-state.md`
9. `small/compact-constant-aggregates.md`
