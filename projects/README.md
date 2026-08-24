# Compiler Improvement Projects

This folder contains self-contained project specifications for structural
improvements to the compiler. Each `.md` file is written so that someone brand
new to the codebase (human or agent) can read that one file and understand the
problem, the solution approach, what success looks like, how to evaluate the
result for long-term correctness and performance, and what tests to add. Each
doc's "What success looks like" section is a completion contract: the project
is not done until every criterion listed there holds.

- `small/`—localized, mostly additive checks or deletions, low design risk;
  hours to days each.
- `big/`—projects on the order of weeks each: cross-cutting, and several
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
parallel old path kept firing on uncovered axes—so each project's finishing
move is deleting the re-derivation path, not just adding the carried fact
beside it. `design.md` at the repo root is the authoritative post-check
design; these projects implement its stated principles more completely.

A second batch of projects came out of the 2026-07 comparative review of
the post-check pipeline against the cor `lss` prototype it was
productionized from (stage-by-stage divergence review; no shipped
miscompile found, but several unstated load-bearing invariants, one
termination hazard, and one verification coverage gap):

- [small/spec-constr-specialization-limits.md](small/spec-constr-specialization-limits.md)—
  termination budgets for call-pattern specialization (compile-time
  divergence is reachable today).
- [small/empty-tag-union-yield-provenance.md](small/empty-tag-union-yield-provenance.md)—
  key Lambda Solved's one unification escape hatch on carried provenance
  instead of shape.
- [small/pin-lambda-solved-invariants.md](small/pin-lambda-solved-invariants.md)—
  state, assert, and test the four invariants that make monomorphic
  lambda-set solving sound.
- [small/lambda-mono-oracle-fidelity.md](small/lambda-mono-oracle-fidelity.md)—
  agreement asserts and contract pins for the Debug Lambda Mono oracle;
  delete its dead Queue.
- [small/monotype-machinery-hardening.md](small/monotype-machinery-hardening.md)—
  release-gate verification-only type checks; measure-first fixes for
  digest depth fallback, unify memo, spec duplication, cross-store reuse.
- [small/lift-capture-single-sourcing.md](small/lift-capture-single-sourcing.md)—
  one capture-fixpoint driver, the `if_initialized_payload` binder
  question, and the capture-id override path.

Within this batch the projects are independent.
(`spec-constr-specialization-limits` used to be paired here with
`spec-constr-static-match-soundness`, which has since landed and had its
doc removed.)

A third batch came out of a whole-codebase competing-sources-of-truth
audit (2026-07-18): a sweep of every subsystem for the same fact
encoded independently in N places with no cross-check. The sweep also
confirmed the prevailing single-source pattern holds in most places
(escape alphabet, `LowLevel` vocabulary, `layout/abi` classification,
`RocTarget`, precedence table, snapshot file format, serde audits)—
these projects close the holdouts. All are independent of the earlier
batches and of each other:

- [big/runtime-representation-single-sourcing.md](big/runtime-representation-single-sourcing.md)—
  backends stop re-encoding RocStr/RocList offsets, flag bits, the
  refcount contract, and C-ABI thresholds as magic numbers.
- [big/host-boundary-single-sourcing.md](big/host-boundary-single-sourcing.md)—
  glue templates, glue-platform struct mirrors, shim symbol
  strings, and test-host boilerplate get generated or lock-tested
  against `host_abi.zig`/builtins.
- [big/one-report-renderer.md](big/one-report-renderer.md)—collapse
  the four per-target diagnostic renderers onto one walker plus style
  data; delete the duplicated annotation-color switch.
- [small/llvm-conversion-op-explicit-dispatch.md](small/llvm-conversion-op-explicit-dispatch.md)—
  end the LLVM backend's `@tagName`-substring dispatch for numeric
  conversion ops (the one backend exempt from switch exhaustiveness).
- [small/bundle-unbundle-shared-path-rules.md](small/bundle-unbundle-shared-path-rules.md)—
  one archive path-safety validator; the writer's and reader's
  copies already disagree.
- [small/nodestore-serde-enrollment.md](small/nodestore-serde-enrollment.md)—
  comptime-drive NodeStore's eight hand-enumerated field lists;
  derive the parse-side node counts.
- [small/frontend-name-and-sentinel-single-sourcing.md](small/frontend-name-and-sentinel-single-sourcing.md)—
  six frontend seams: duplicate NumKind map, hardcoded Bool
  discriminant, method-name literals, hand-written builtin-name maps,
  five row comparators, default-cased lowering switches.
- [small/syntax-fact-single-sourcing.md](small/syntax-fact-single-sourcing.md)—
  keyword spellings (tokenizer vs ~25 fmt literals), the
  numeric-suffix bidirectional pair, the twice-scanned number
  grammar.
- [small/severity-and-report-collection.md](small/severity-and-report-collection.md)—
  `Severity.isError`/`toLspSeverity` helpers; snapshot tool and
  playground call the compiler's report-collection loop instead of
  copying it.
- [small/lsp-and-docs-truth-reuse.md](small/lsp-and-docs-truth-reuse.md)—
  the forked doc-comment gatherer (LSP and docs already disagree on
  `###`), three line/column implementations, the positional
  semantic-token legend, the hand-copied completion roster.
- [small/build-and-ci-single-lists.md](small/build-and-ci-single-lists.md)—
  one module inventory (seven restatements plus minici's copy, with
  existing test-coverage divergence), one CI gate list, one Zig pin.
- [small/cli-declarative-flags.md](small/cli-declarative-flags.md)—
  each subcommand's struct/parser/help triple becomes one table;
  target rosters and defaults render from their enums.

A fourth batch (2026-07-20) targets operational robustness and
build-throughput gaps rather than sources of truth. The projects are
independent of the earlier batches and of each other:

- [big/parallel-backend-codegen.md](big/parallel-backend-codegen.md)—
  dev-backend code generation moves from one sequential proc loop to
  per-proc worker generation feeding a single deterministic writer; no
  new IR, the per-proc code/relocation artifacts are the handoff unit.
- [big/unreachable-rationale-comments.md](big/unreachable-rationale-comments.md)—
  every `unreachable` under `src/` carries a same-line rationale (or
  is converted to a checked invariant), enforced forever by a CI lint
  with a shrinking allowlist.
- [small/ice-crash-context.md](small/ice-crash-context.md)—
  a panic handler that prints what the compiler was doing (phase,
  module, def) plus a repro command before the stack trace, built from
  thread-local context frames pushed at existing phase boundaries.

A fifth batch (2026-08-24) came out of a duplication audit of the
stages between type checking and code generation—Monotype, Monotype
Lifted, SpecConstr, Lambda Solved, the two LIR lowerers, the LIR
passes, and ARC. It looked specifically for *multiple implementations
of one semantic rule* rather than for repeated text. The recurring
shape: `.lss` and `.boxy` were scoped by the phases they skip, but
value semantics (equality, hashing, `Inspect`, `match` compilation)
are not phase artifacts and got duplicated along with the phases; and
inside each strategy, producer/consumer boundaries that design.md
states as "record the decision, consume it later" are implemented as
"re-derive it from the emitted shape". Several of these pairs have
already diverged in behavior, not just in text.

- [big/one-value-semantics-layer.md](big/one-value-semantics-layer.md)—
  `Inspect` (four implementations, four copies of the format
  strings), structural equality and hashing (two), and `match`
  compilation (`.boxy` never adopted the shared decision-tree
  compiler, contrary to this README and `postcheck/mod.zig`) collapse
  onto one shape-parameterized layer, plus a standing `.lss`/`.boxy`
  differential harness.
- [big/postcheck-lowerer-decomposition.md](big/postcheck-lowerer-decomposition.md)—
  the god-structs (`BodyContext` at 36.6k lines, `ProcBodyBuilder` at
  25.1k) that force helpers to be copied rather than shared; 53 and 17
  duplicated method names respectively, with divergence already
  present in both.
- [small/arc-shared-predicates.md](small/arc-shared-predicates.md)—
  four ARC predicates defined twice, including the refcounted-local
  predicate where the RC inserter and its own certifier already use
  different code.
- [small/erased-ownership-as-lir-data.md](small/erased-ownership-as-lir-data.md)—
  the lowerer records erased ownership and the certifier re-derives it
  with a copy of the rule; `.boxy` has no producer at all.

Single-source primitive tables have landed (the batch's first
project): `CheckedPrimitive`'s four post-check mappings—storage layout,
inspect low-level op, hasher write op, and builtin owner—each have one
definition. The first three live in `src/postcheck/common.zig`; the
owner table lives beside `CheckedPrimitive` itself in
`src/check/checked_artifact.zig`. A `structural_test.zig` lint fails the
build if a second definition of any of them appears. None of the copies
had been forced by a type barrier: `MonoType.Primitive` is a plain alias
of `checked.CheckedPrimitive`.

Boxy representation queries on the plan have landed: the ~45 queries
Boxy lowering re-derived are now `Plan.RepQuery` / `Plan.NamedRepQuery`,
one definition each, and a `structural_test.zig` lint keeps every
consumer calling them. This closed a live plan/lower disagreement—the
planner's `repSubtreeHasDescriptorInOtherChildren` skips siblings the
selected child's subtree already covers, and both lowering copies
lacked that carve-out, so lowering could refuse an unwrap the planner
had planned. It also separated two predicates a shared name had blurred:
exact-role equality and `sameChildRoleKind`, which answers false for
every role carrying a payload.

One call-result fusion machinery has landed: `return_slot` and
`str_append` were the same pass twice, and the walk, the single-use
liveness guard, and the variant clone now live once in `body_clone.zig`,
whose charter already named them. Each pass keeps only what is genuinely
its rule—which consumer statement it matches, and how it rewrites the
variant's returns. The merge fixed a silent drop: `str_append`'s variant
clone did not carry `erased_reuse_arg` through, so a fused
erased-callable proc lost it. `box_reuse` adopted the shared
eligibility predicate; `loop_append_promote` deliberately did not,
because its gate is a genuinely different one (it admits non-Roc ABI
procs), which is recorded here rather than papered over.

One correction to the batch, from implementing it. The audit filed a
`spec-constr-single-cloner` project on the claim that Monotype Lifted's
two cloners cover 29 and 44 expression variants and that the narrower
one declines the difference *silently*, so a new variant would be
half-supported without a build error. That claim was wrong.
`Pass.cloneExprFresh` ends in an explicit 19-variant arm returning null,
and `grep -c 'else =>' src/postcheck/monotype_lifted/spec_constr.zig` is
zero—the repo's ban on non-exhaustive switch prongs already forces both
cloners to make a decision about every variant. The coverage gap is a
deliberate, compiler-enforced decline set, not a silent one, so the
project's premise did not survive contact and the risky part of it (one
traversal through a compile-time-hot pass, for a drift the compiler
already catches) was not worth doing.

The real duplication that project found *was* there, one layer down:
the pattern walk that collects a pattern's bound locals existed three
times—the lift pass's bound-set scan, its capture graph builder, and
SpecConstr's body-local scope—identical in every position, differing
only in what each does at a binding site. That is now
`Ast.forEachBoundLocal`, taking the binding action as a comptime
parameter, with a `structural_test.zig` lint keeping it single. Three
copies could have come to disagree about whether a given position
binds—a list rest pattern, say—and only one would have been right.

A second correction, also from implementing it. The audit filed a
`postcheck-ir-store-boilerplate` project proposing a comptime
`FlatStore(Spec)` mixin that *generates* each store's `add*`, `get*`,
`set*`, `*Count`, `*View`, and `*Span` surface. Zig 0.16 removed
`usingnamespace`, so a mixin cannot inject named declarations into a
struct at all; every accessor has to stay a hand-written name whatever
sits behind it. The project's mechanism does not exist in this compiler,
so the doc is deleted rather than left proposing it.

The duplication it counted is real—57 byte-identical bodies between
`monotype/ast.zig` and `monotype_lifted/ast.zig`—but most of those
bodies are already one-line delegations whose only content is the field
they name, which is the part that legitimately differs per store. The
part with real duplicated logic was the span append: three lines
(`len()`, `appendSlice`, construct the span) written out 33 times across
the three stores. That is now `Common.appendSpan` /
`Common.appendNonemptySpan`, with `Span` itself single-sourced, and a
`structural_test.zig` lint keeps the hand-written form from coming back.
`addTypedLocalSpan` deliberately kept its own body: it preallocates
through `ensureUnusedCapacity`, which is a different append, not the
same one spelled differently.

Two doc claims were also corrected in the code. `match_tree.zig` and
`postcheck/mod.zig` both described the decision-tree match compiler as
"shared by both LIR lowerers"; it is used by `.lss` only, and a doc
comment asserting a sharing invariant that does not hold is worse than
no comment, because the next reader budgets for one match semantics when
there are two. Both now say what is true, and a lint pins the claim to
the imports so it moves when the gap closes.

Housekeeping in the same pass. `float-range-flat-representation.md` was
carrying its own "Status: Resolved" section rather than a project: range
syntax now constructs a reusable `Num.Range(num)` whose single
`Builtin.Num.Range.iter` is the monomorphization-recognized iterator
source, so integer, `Dec`, and float ranges all keep flat by-value
iterator state and the element-type performance cliff is gone. Its
verification lives in `src/eval/test/eval_iter_alloc_tests.zig` and
`src/eval/test/lir_inline_test.zig`, both present and covering it, so
the doc is deleted. Twelve cross-references to four project docs that
landed and were removed (`silent-drift-guards`,
`store-generation-counters`, `spec-constr-static-match-soundness`,
`audit-solver-mutating-rewrites`) were dangling links; they now read as
plain names marked landed, and the two recommended-order lists dropped
their landed entry and renumbered.

Within the rest of this batch, the two `big` projects compose in either
order. The rest are independent.

## Recommended order

### Start here—enforcement layers, cheap and load-bearing

1. [small/cross-phase-coverage-parity-tests.md](small/cross-phase-coverage-parity-tests.md)—
   the divergence-classification parity suite; a regression net the big
   lowering projects inherit.
2. [small/rceffect-conformance.md](small/rceffect-conformance.md)—
   comptime validity plus a per-op refcount conformance harness for the
   central ownership table (the PR 10023 bug class).
3. [small/cache-and-identity-residuals.md](small/cache-and-identity-residuals.md)—
   closes the four small seams left after the identity/cache cures
   (name-text fallback, hand-enrolled serde contracts, split version
   hashes, `type_name` in nominal keys).

### Chain A—dispatch evidence, consumed everywhere

1. [small/hoist-consumes-dispatch-evidence.md](small/hoist-consumes-dispatch-evidence.md)—
   hoist selection reads the dispatch resolution instead of re-deriving
   evidence-dependence from type-var content (the PR 10073 seam), and
   recovers the hoisting its conservative gate gives up.

### Chain B—the host/platform boundary

1. [small/hosted-extern-declared-abi.md](small/hosted-extern-declared-abi.md)—
   the invariant that a hosted extern is only specialized at its declared
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
once—the name-keyed re-resolution and double publication are deleted.

### Chain C—specialization sealing

1. [small/pin-deferred-spec-requests.md](small/pin-deferred-spec-requests.md)—
   seal-time instrumentation, the snapshot-regime pin, and the
   `unifyThroughBacking` decision; end state: `row_default` unreachable
   for checker-constrained rows.

### Independent—start any time, in any order

Small:
- [small/frame-partitioned-checker-state.md](small/frame-partitioned-checker-state.md)—
  inventory and convert frame-scoped checker/canonicalizer state to
  dedicated frame storage (the 9929→10010 and 10001 shape).
- [small/compact-constant-aggregates.md](small/compact-constant-aggregates.md)—
  static-data and builtin-call materialization for constant/repeated
  lists, ending the one-local-per-element explosion behind issue 9898.

The decision-tree match compiler has landed for `.lss`: the direct
solved-to-LIR lowering compiles `match` through one shared Maranget-style
module (src/postcheck/match_tree.zig)—one multiway switch per tested
position, one discriminant read, strings and list-length buckets as
ordinary arms—with the sharing invariant documented in design.md and
enforced by a debug statement-count lint. The 2026-08-24 audit found
`.boxy` never adopted it (`grep -rn match_tree src/postcheck/boxy/` is
empty; `boxy/lower.zig` folds branches into a sequential chain), so the
"shared by both LIR lowerers" claim in `src/postcheck/mod.zig` is not yet
true. Closing that is step 6 of
[big/one-value-semantics-layer.md](big/one-value-semantics-layer.md).

Single-source builtin registration has landed: the seven hand-typed
`roc_builtins_*` symbol/ABI tables now derive from one comptime registry
(src/builtins/builtin_registry.zig), and the LowLevel-to-builtin member
choice is shared across backends via `base.LowLevelBuiltins`.

### Suggested overall sequence

If one person or agent works through everything serially, this order
front-loads leverage and keeps prerequisites satisfied:

1. `small/cross-phase-coverage-parity-tests.md`
2. `small/rceffect-conformance.md`
3. `small/cache-and-identity-residuals.md`
4. `small/pin-deferred-spec-requests.md`
5. `small/hoist-consumes-dispatch-evidence.md`
6. `small/hosted-extern-declared-abi.md`
7. `small/frame-partitioned-checker-state.md`
8. `small/compact-constant-aggregates.md`
