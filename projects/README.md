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
- [small/pin-lambda-solved-invariants.md](small/pin-lambda-solved-invariants.md)—
  state, assert, and test the four invariants that make monomorphic
  lambda-set solving sound.

Within this batch the projects are independent.
(`spec-constr-specialization-limits` used to be paired here with
`spec-constr-static-match-soundness`, which has since landed and had its
doc removed.)

The Monotype machinery hardening project has landed, mostly by finding its
targets already gone. Every verification-only type-equality check in
Monotype lowering is now a debug-only `assertSameType` (or a debug-gated
`typeEql`), and `run-check-postcheck-architecture` rejects a release-computed
one; on sampled echo and fx apps those checks had cost at most 0.16% of
Monotype cycles, so this is policy, not speed. The digest walk has no depth
cap any more (a 300-level nesting test pins that), the cross-call unify memo
is a per-call set, and the checked source type is no longer part of
specialization identity, so the depth-fallback, memo, and duplicate-spec
items had nothing left to fix. The reserved-request refinement API had no
production caller and is deleted, so a record's request view is its
identity's requested type. Union-find paths averaged under 0.3 hops per
`find` (at most 13), so `union_` keeps no rank, and `layoutOfType` already
sizes its scratch by the visited subgraph.

The lift capture single-sourcing project has landed, mostly by having
already happened. The initial lift and the post-spec_constr re-solve both
run `CaptureDependencyGraph.solve`, so there is one fixpoint driver, and the
stale-id override it worried about is gone: operand keys join to slots by
their explicit namespace. The `if_initialized_payload` payload is a read of
the parser loop's payload parameter, not a binder, so the capture arms were
already right and now say so. The free-variable walk itself had also
existed twice (the graph builder's and the Lifter's `CaptureSet`, used for
inline lambdas and operand fill-in); both now run the one `CaptureWalk`,
supplying only their actions, and `run-check-postcheck-architecture` rejects
a second walk in `lift.zig`.

The Lambda Mono oracle-fidelity project has landed. Its dead `Queue` had
already been deleted. The materializer now asserts that every type source of
a local lowers to one Lambda Mono type (including the typed-local sequence
path, which had reused the cached type instead of checking it), and it
re-derives each replayed `List.map` fold through `FoldedMatch.isResolutionOf`
instead of trusting the recorded body. The negative controls test that
predicate directly, because invariants panic and the test suite has no
harness for observing a panic.

The empty-tag-union yield project has landed, by deletion rather than by
carrying provenance. Sealing unresolved Monotype rows before publication had
already removed every legitimate firing; a Debug corpus sweep (echo, fx, the
remaining `test/` apps) found the yield, by then broadened to any proven
uninhabited type, linking only identical uninhabited records. Lambda Solved
now unifies uninhabited types structurally, and an empty row meeting a
populated one is an invariant violation, so no provenance set is needed.

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

The LLVM conversion-op dispatch project from this batch has landed, mostly
before it was picked up: `base/numeric_conversion.zig` already built the
comptime-checked conversion table and LLVM's `emitLowLevel` already switched
exhaustively. What remained was the drift profile: the conversion prong now
passes a comptime `Conversion`, like the interpreter, so a conversion op that
LLVM has no lowering for is a `@compileError` instead of a runtime
`UnsupportedLowLevel`. Emitted code is unchanged.

One CI gate list outside pull requests has landed. The nightly legs on
hosts PR CI never uses (`minici-other-hosts` in `ci_zig.yml`) and the
Nix test leg run `zig build minici -- --minici-lane secondary` instead
of naming jobs, and a `workflows` table in `src/build/minici.zig`
declares how each CI file runs MiniCI; `--minici-verify-workflows`, run
on every PR, checks each file against it, including that none runs a
MiniCI job by hand. That check found a fifth hand-listed job the
project had missed (`run-coverage-parser` on arm64 Linux). `glue`'s
module tests joined MiniCI (about 20 s, so no shard moved). The Debug
`-Dfuzz` `run-test-zig` aggregate still runs nightly, because several
of its suites (rc-conformance, boxy-abi, and others) are not MiniCI jobs.

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

The ICE crash-context project from this batch has landed (design.md
"Crash Reports"). Corrections: the module is `base.ice_context`, since
`eval` already has an unrelated `crash_context`; the repro command is the
process's own command line and working directory rather than one derived
from the crashing phase, so it reproduces imported-module and run-command
crashes too; and frames name the module per front-end task, the stage per
post-check step, and the procedure per codegen or interpreter unit, not
individual defs.

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

One definition per ARC predicate has landed. The refcounted-local
predicate had already collapsed to a single layout query by the time the
project ran, but the outcome predicates had not: the outcome-row mask
arithmetic now lives once in `arc_sig.zig`, and the refinement walk and
outcome binding target once in `arc_solve.zig`, pinned by a source-text
lint in `arc_solve.zig`. The merge closed a live divergence: insertion's
walk had learned to step over a statement `match`'s merge join while
field dismantling's copy had not, so a field projection passed into a
fallible call under such a `match` never got a restitution receipt. The
shared walk also admits joins and aliases only before the discriminant
read, which is what design.md states and what the certifier accepts.

Erased-ownership single sourcing has landed, smaller than filed. The
lowerer's decision was already LIR data—`assign_call_erased.reuse_source`
—and the certifier's half of the rule had already moved into
`lir/erased_owner.zig`, where it independently verifies that field
against the emitted `assign_ref` chain (the check a certifier should make,
so it stays; a per-local side table on `LirStore` would only have been
re-checked by that same walk). What remained was the rule itself written
twice: the lowerer's `transparentErasedOwnershipSource` and the LIR
module's `transparentSource`. The edge now lives once in `lir_core`
(`ErasedOwnerEdge`), every `assign_ref` the direct lowerer emits goes
through `addAssignRef` so its target always gets recorded provenance, and
source-text lints pin both. `.boxy` has no gap: it emits no owned erased
calls, so it has no reuse source to choose, and a lint keeps it that way.

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

1. [small/rceffect-conformance.md](small/rceffect-conformance.md)—
   comptime validity plus a per-op refcount conformance harness for the
   central ownership table (the PR 10023 bug class).

The divergence-classification parity suite has landed. Divergence is one
classification, `checkedExprDivergence` in `checked_artifact.zig`; Monotype's
and Boxy's never-divergent arms check it at compile time, and checked-artifact
tests drive `DivergenceScan` over every expression kind against it, alongside
pinned echo fixtures for a divergent nominal payload, call argument, and
arithmetic operand. Correction: Boxy is a third consumer the doc missed, and
both consumers lowered a divergent custom interpolation only through its
operands although its dispatch plan can crash; it now lowers through its
dispatch like the other plan-carrying kinds. The convention is written down in
CONTRIBUTING/README.md.

The cache-and-identity residuals project has landed. Its name-text
auto-import fallback was already gone (auto-imports carry an explicit
`AutoImportIdentity`). Every admitted serialization root now declares a
`RootContract`, and a layout fingerprint exists only for a type that does;
`compile/cache_versioning.zig` composes every entry's admission hash, and the
platform-pairing entry gained its own instead of folding fingerprints into its
key. `type_name` was redundant in `NominalTypeKey` and is removed—identity is
the module content identity plus declaring statement. Correction: one consumer
(imported nominal declaration resolution) had been using the name text to tell
apart embedded declarations of different modules that share a statement index;
it now compares the declaration's module identity.

### Chain A—dispatch evidence, consumed everywhere

The `hoist-consumes-dispatch-evidence` project is retired: its premise did
not survive contact. Hoist selection cannot read the dispatch resolution,
because `resolveTotalDispatchPlans` takes the pruned compile-time roots as
input, so the plan does not exist yet when hoisting is decided. Nor would
an outer `direct` plan prove a root hoistable: `e == e` with `e : List(a)`
under a where clause on `a` resolves `List.is_eq` directly while its nested
`a.is_eq` still needs the caller's evidence. The conservative gate is
therefore not re-deriving a fact that exists elsewhere. Recovering what it
gives up (for example `n = [].len()` in a function body, whose dispatcher is
`List(flex)`) would need the checker to record, as solving decides it,
which obligations bind to an enclosing scheme and which stay free.

### Chain B—the host/platform boundary

The hosted-extern declared-ABI project has landed: Monotype lowering asserts
(`requireHostedExternAtDeclaredAbi`) that every hosted extern is specialized
at its declared type, with the non-`?` widening channels and the host
`Ok`-not-misread case pinned on native and wasm32. Correction: the guard is
a debug-only invariant assertion like every other, not a release-build stop,
and the checker's hosted `?` widening rewrite is deleted rather than kept
(PR 12118), so direct hosted errors are reconstructed explicitly in Roc.

The solver-rewrite audit that followed this chain has landed: design.md's
"Solver-Mutating Rewrites" section holds the mechanism/policy inventory, and
`Store.dangerousSetVarRedirect` requires a declared `RedirectRule` at every
call site.

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

Compact constant aggregates have landed for cold builds: constant lists of
any shape cost the runtime program fixed-size LIR, a literal-count repeat
of a runtime value is one builtin call, and the freezing, merging, and
re-encoding steps that were quadratic in a list's pointer count are linear.
The correction to the original doc: the per-element explosion it described
no longer exists on the cold path; it survives only when a warm cache
restores a non-packable constant list, which still builds it through a
one-local-per-element static initializer.

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

Frame-partitioned phase state has landed: every field of `Can` and `Check`
is classified in a compile-checked state inventory (design.md
"Frame-Partitioned Phase State"), frame stacks assert they close their
innermost frame, and both phases assert at completion that no frame-owned
state leaked. One obligation remains off main: the
`iterative-checker-prologue-epilogue` branch carries the tag-and-filter
version of the issue 9827 fix and must adopt per-frame return-constraint
storage, and pass the issue 9827 repro, before it merges.

Single-source builtin registration has landed: the seven hand-typed
`roc_builtins_*` symbol/ABI tables now derive from one comptime registry
(src/builtins/builtin_registry.zig), and the LowLevel-to-builtin member
choice is shared across backends via `base.LowLevelBuiltins`.

### Suggested overall sequence

If one person or agent works through everything serially, this order
front-loads leverage and keeps prerequisites satisfied:

1. `small/rceffect-conformance.md`
2. `small/pin-deferred-spec-requests.md`
