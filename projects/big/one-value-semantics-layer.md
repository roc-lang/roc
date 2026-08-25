# One Value-Semantics Layer for Both Lowering Strategies

## Problem

`Inspect` rendering, structural equality, structural hashing, and
`match` compilation are *value semantics*: they depend on a type's
shape, not on which post-check phase list produced it. Today each is
implemented independently once per lowering strategy—and `Inspect` is
implemented four times.

**Inspect—three implementations, three copies of the format strings.**

- `src/postcheck/monotype/lower.zig` (`BodyContext.inspectBody`,
  `toInspectCall`, `inspectTuple`, `inspectRecord`, `inspectFieldSlot`,
  `inspectTagUnion`, `inspectList`). A fourth copy lived in `Builder`
  and was dead; it has been deleted.
- `src/postcheck/boxy/lower.zig:27699-28270`
  (`lowerInspectExprInto`, `lowerToInspectMethodInto`,
  `lowerRecordInspectLocalsInto`, `lowerTupleInspectLocalsInto`,
  `lowerTagUnionInspectLocalsInto`,
  `lowerPresenceSlotInspectLocalsInto`, `lowerBoxInspectLocalsInto`,
  `lowerListInspectLocalsInto`).
- `src/eval/boxy_runtime.zig:5371-5800` (`appendBoxyInspect`,
  `appendLayoutInspect`, `appendPresenceSlotInspect`,
  `appendScalarInspect`, `appendListInspect`, `appendStructInspect`,
  `appendTagUnionInspect`).

The literals `"{ "`, `" }"`, `"{}"`, `", "`, `": "`, `"("`, `")"`,
`"()"`, `"<missing>"`, `"<function>"`, `"Box("` are written out
independently in each. A change to how Roc renders a record, a tuple,
an optional field, or a boxed value must be made in four places, with
nothing that fails if it is made in three.

The two `monotype/lower.zig` copies had diverged on behavior, not just
plumbing: `Builder.toInspectCall` returned `null`—falling back to
structural rendering—for `.structural` and `.local_proc` method targets,
with no error-payload and no capture handling, while
`BodyContext.toInspectCall` routed an `err` callable payload to
`runtimeCrashExpr`, consulted `frozen_inspect_method_calls`, and passed
a capture span. That divergence turned out to be unreachable: every call
site of `Builder`'s inspect family was inside the family itself, so the
weaker copy was dead and has been deleted rather than merged. Three
implementations remain, and the audit's original claim that inspect
behavior depended on which context derived it was wrong.

**Structural equality and hashing—two implementations that currently
agree.** The `lowerEqLocalsInto` / `lowerHashLocalsInto` families in
`solved_lir_lower.zig` are mirrored arm for arm in `boxy/lower.zig`:
primitive, bool, record, tuple, field-step, tag-union, tag-payload, and
the hash counterparts. Roughly 20 function pairs define what `==` and
`hash` *mean* for aggregates, in two places.

An earlier draft of this doc called several of those pairs "already
diverged", citing normalized-body similarity as low as 0.18. That was
wrong, and the way it was wrong is worth recording: the metric was
measuring receiver plumbing (`self.result.store` versus
`self.parent.result.store`), helper choice (`assignRefRead` versus an
inlined `assign_ref`), and `if`-chains expanded into exhaustive
switches. Reading the pairs shows the same algorithm on both sides—the
same right-to-left AND fold over a shared `failed` target, the same
ZST skip, the same SIMD-to-`u128` bit comparison, the same op tables.
The genuine differences are forced by the input: LSS walks a monotype
span, `.boxy` walks `RepChild` roles.

So this is duplication without present divergence: a standing drift
risk across ~20 pairs, not a live bug. It should still be single-sourced
by step 5, but it does not carry the urgency the `Inspect` or
`match` items do. **Text similarity is not semantic divergence**—check
by reading, and check reachability, before filing a pair as diverged.

**`match`—the shared decision-tree compiler has one user, not two.**
`src/postcheck/mod.zig:37` documents `match_tree.zig` as the
"decision-tree match compiler shared by both LIR lowerers", and
`projects/README.md` records the project as landed. Only
`solved_lir_lower.zig:5598` uses `match_tree.Compiler`;
`grep -rn match_tree src/postcheck/boxy/` is empty.
`boxy/lower.zig:19865` (`lowerMatchInto`) instead folds branches into
a reverse-order sequential chain via `lowerMatchBranchInto`, with its
own pattern-test lowering (`lowerListPatternThen`,
`lowerRecordPatternThen`, `lowerStrPatternArm`, `bindRecordPattern`,
`bindTuplePattern`—all also present in `solved_lir_lower.zig`, with
similarity as low as 0.14). Boxy gets neither the one-test-per-tested-
position property nor the statement-count lint at
`match_tree.zig:1097`.

**Some of the duplication has no barrier at all.**
`Common.hasherWriteOp` (`src/postcheck/common.zig:104`) is declared
over `MonoType.Primitive`, and `boxy/lower.zig:36368` copies it over
`checked.CheckedPrimitive`—but `monotype/type.zig:45` reads
`pub const Primitive = checked.CheckedPrimitive`. The two signatures
name the same type. The copy was never forced by anything.

## Background

design.md defines `.boxy` by the phases it skips: "It skips Monotype,
Monotype Lifted, Lambda Solved, and Lambda Mono." That scoping is
correct for *specialization*—boxy's whole point is reaching LIR
without whole-program lambda-set solving. It says nothing about
equality, hashing, inspection, or pattern-match compilation, none of
which depend on how a type arrived. Those got duplicated along with
the phases anyway, because both strategies emit LIR directly and each
grew its own emit-side helpers.

The repo already contains three working instances of the sharing
pattern this project generalizes:

- `match_tree.Compiler(Ctx)`—one algorithm, comptime-parameterized
  over how the caller reads patterns and emits tests. It is the
  template; it just has one instantiation.
- `EqDeriver` (`monotype/lower.zig:49742`) and `HashDeriver`
  (`:49965`)—policy structs (`leaf`, `combine`, `componentForField`,
  `componentForTuple`, `named`, `tagUnion`, `tagBranch`) plugged into
  one generic derivation walker. Exactly the right shape, currently
  reachable only from `BodyContext`.
- `layout/graph.zig` + `layout/store.zig`—four independent producers
  (`monotype/lower.zig`, `solved_lir_lower.zig`, `boxy/layouts.zig`,
  `glue/checked_artifact_layout_resolver.zig`) build a
  `GraphInput`/`GraphNode` description and commit through one store.
  This seam holds and should be the model for shape description.

`src/postcheck/lambda_mono/` (6,294 lines, reachable only from
`src/eval/test/lambda_mono_differential_runner.zig`) is the model for
duplication that is *kept on purpose*: its header states it is "a
derivation that shares no lowering code with the direct path", and a
differential harness asserts the two agree on inspect string, abort
kind and message, `dbg` transcript, and expect-failure transcript.
That is what makes its duplication safe. Nothing equivalent exists
between `.lss` and `.boxy`.

## Evidence

- `grep -rn 'match_tree' src/postcheck/boxy/`—empty, against
  `src/postcheck/mod.zig:37`'s claim and the README's "has landed"
  note.
- `grep -rn '"<missing>"' src/`—`boxy/lower.zig:28047`,
  `boxy_runtime.zig:5527`, and `Builder.optional_field_missing_render`
  consumed at `monotype/lower.zig:8926` and `:14931`.
- `monotype/lower.zig:8808` vs `:14777`—diff the two
  `toInspectCall` bodies; the capability sets differ.
- `monotype/lower.zig:8876` vs `boxy/lower.zig:27896`—`"{ "`,
  `", "`, `": "`, `" }"` emitted by two unrelated code paths.
- `common.zig:104` vs `boxy/lower.zig:36368` against
  `monotype/type.zig:45`—identical parameter type, copied body.
- Normalized-body similarity for the 70 function names shared between
  `solved_lir_lower.zig` and `boxy/lower.zig` spans 0.05–1.00; the
  spread is the drift already in flight.
- The tax being paid live: PR #10834 (in review as this was written)
  adds unset optional fields, and its lowering commit states the work
  was done in "both lowering pipelines (monotype and boxy)"—one
  language change, two independent lowerings, each separately
  eval-tested across interpreter/dev/wasm. Its hunks in
  `monotype/lower.zig` and `boxy/lower.zig` do not touch the inspect,
  equality, hashing, or match families, so this project and that PR do
  not collide; the point is that the next such change will pay the
  same double cost.

## Solution design

1. **The unforced copies are already gone.** `hasherWriteOp`,
   `primitiveLayout`, and `primitiveInspectLowLevelOp` have one
   definition each in `src/postcheck/common.zig`, and the primitive-to-
   owner table lives beside `CheckedPrimitive` in
   `src/check/checked_artifact.zig`, with a `structural_test.zig` lint
   holding them there. The shared walkers below consume those tables;
   this step is a prerequisite, not part of this project's design work.

2. **Define one shape-reading context.** A comptime interface in a new
   `src/postcheck/semantics/` module, in the spirit of
   `match_tree.Compiler(Ctx)` and `layout.GraphInput`:
   `primitiveOf(ty)`, `recordFields(ty)`, `tupleElems(ty)`,
   `tagVariants(ty)`, `listElem(ty)`, `boxPayload(ty)`,
   `nominalBacking(ty)`, `optionalFieldSlot(ty)`, plus the emit hooks
   each consumer already has (`fieldAccess`, `tagPayload`,
   `discriminant`, `stringLiteral`, `concat`, `switch`). Three
   instances: Monotype/Lifted types, `Plan.TypeRepresentation`, and
   the interpreter's `BoxyTypeDesc`. The interpreter instance emits
   bytes rather than LIR, so its hooks differ—see step 4 for the split
   that keeps it in the shared layer anyway.

3. **`InspectSpec(Ctx)`.** One walker produces an ordered part list
   (literal | child reference | method call) from a shape. Boxy
   already has exactly this data type—`InspectPart` and
   `lowerInspectPartsInto` (`boxy/lower.zig:28232`)—so the part list
   is the right currency; promote it out of boxy and make it the
   shared output. Every format string then exists once. The
   `to_inspect` method-target rule is decided once, at the union of
   the two current `toInspectCall` capability sets: honor
   `.structural` and `.local_proc` targets, handle `err` payloads,
   carry captures.

4. **The interpreter consumes the same spec.**
   `eval/boxy_runtime.zig`'s `append*Inspect` family cannot share the
   LIR emitters, but it can and must share the part list: given a
   shape it renders the same ordered parts to bytes. That removes the
   fourth copy of the format strings without pretending the runtime is
   a lowerer.

5. **`EqLower(Ctx)` / `HashLower(Ctx)`.** Hoist `EqDeriver` and
   `HashDeriver` out of `BodyContext` into the semantics module and
   give them a third and fourth instantiation: the direct
   `structural_eq`/`structural_hash` lowering in
   `solved_lir_lower.zig` and in `boxy/lower.zig`. The per-primitive
   op choice comes from the single tables of step 1.

6. **Boxy adopts `match_tree`.** Implement `MatchTreeCtx` over boxy's
   checked patterns and representation plan, replace
   `boxy/lower.zig:19865`'s sequential chain, and delete boxy's
   parallel `lower*PatternThen` / `bind*Pattern` family. Then the
   `mod.zig:37` doc comment and the README's "has landed" note become
   true, and the statement-count lint at `match_tree.zig:1097` covers
   both strategies.

7. **The standing agreement harness.** Some duplication survives by
   design—two strategies, two emit paths. Make the survivors safe the
   way `lambda_mono` is: run one shared corpus under `--opt=dev` (LSS)
   and under `.boxy`, and require identical inspect strings, `dbg`
   transcripts, abort kinds and messages, and expect-failure
   transcripts. `src/eval/test/parallel_cli_runner.zig:867` is a
   single hand-written instance of this today; it becomes the general
   harness, modeled on
   `src/eval/test/lambda_mono_differential_runner.zig`.

Order matters: steps 1 and 2 unblock everything; 3+4 and 5 are
independent of each other; 6 is independent of 3–5; 7 should land
early enough to protect 3–6 rather than after them.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- Every `Inspect` format literal appears exactly once under `src/`.
  `grep -rn '"<missing>"\|"<function>"\|"Box("' src/` shows one
  definition site each, in `src/postcheck/semantics/`.
- `grep -rn 'fn toInspectCall' src/` shows one definition, whose
  behavior is the union of the two current ones (structural and
  local-proc targets honored, `err` payload handled, captures
  carried), with a test pinning each of those three cases.
- `grep -rn 'fn lower.*EqLocalsInto\|fn lower.*HashLocalsInto' src/`
  shows one family, not two.
- `grep -rn 'match_tree' src/postcheck/boxy/` is non-empty and
  `boxy/lower.zig` has no sequential branch-chain match lowering;
  boxy's `lower*PatternThen` / `bind*Pattern` duplicates are deleted.
- The `mod.zig:37` doc comment and the README's decision-tree note are
  factually true.
- The cross-strategy differential harness is in-tree, runs the shared
  corpus under both strategies, and is green. Constructs a strategy
  does not support are counted and reported per reason—never silently
  skipped (the `lambda_mono` runner's rule).
- Snapshot output is unchanged (`git diff test/snapshots` empty) and
  `roc check` / `roc run` output on a fixture set is byte-identical
  before and after, under both strategies.

## How to evaluate the result

### Correctness ideal

A change to what `==`, `hash`, `Inspect`, or `match` *mean* is a
change to one function, and the type system forces every consumer to
be updated. The class of bug where two strategies render, compare, or
match the same value differently becomes unreachable for shared
concerns and test-detectable for the per-strategy emit hooks. Boxy
inherits `match_tree`'s one-test-per-position property and its
size lint instead of relying on a hand-written chain.

### Performance ideal

Neutral to positive, and it must be measured rather than assumed.
Comptime-parameterized contexts monomorphize, so no dynamic dispatch
is introduced; the shared walkers must not allocate where the current
inline code does not. Boxy adopting `match_tree` should *reduce*
emitted statement count on multi-arm matches (that is what the
`match_tree.zig:1097` lint measures)—record before/after statement
counts on a match-heavy corpus. Compile-time: track post-check phase
timing through `checked_pipeline.zig`'s `Timing` counters over a
fixed corpus; regressions beyond noise are a defect, not a cost of
sharing.

## Tests to add

- Cross-strategy differential harness (step 7), corpus-driven, with
  per-reason unsupported-construct counts.
- Comptime-enumerated coverage: every shape variant the `Ctx`
  interface exposes has at least one inspect case, one equality case,
  and one hash case; a new variant fails the suite until covered.
- `to_inspect` capability pins: a nominal with a structural
  `to_inspect`, one with a local-proc `to_inspect`, one whose
  callable payload is `err`, and one with captures—each asserted
  under both strategies.
- Boxy match-tree statement-count lint (the existing
  `match_tree.zig:1097` assertion, now reached from boxy).
- A source-text test in `src/postcheck/structural_test.zig` asserting
  a single definition site for each consolidated predicate, so the
  copies cannot come back.

## Related projects

- [postcheck-lowerer-decomposition.md](postcheck-lowerer-decomposition.md)—
  the god-structs that make helpers uncopyable-without-copying; the
  `Builder`/`BodyContext` inspect double is removed there or here,
  whichever lands first.
- [../small/lambda-mono-oracle-fidelity.md](../small/lambda-mono-oracle-fidelity.md)—
  the differential-oracle pattern step 7 generalizes.
- [../small/cross-phase-coverage-parity-tests.md](../small/cross-phase-coverage-parity-tests.md)—
  the parity-suite discipline this project applies across strategies.
