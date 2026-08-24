# One Framework for the Call-Result Fusion Passes

## Problem

`src/lir/return_slot.zig` and `src/lir/str_append.zig` are the same
pass written twice. Both run before ARC, both look for an
`assign_call` whose single-use result feeds one specific consumer
statement, both clone the callee proc into a cached variant with a
rewritten return, and both replace the pair with a call to the
variant. Only the matched consumer and the rewrite differ.

The shared skeleton is literally identical:

- `run`: `return_slot.zig:43` / `str_append.zig:51`, 0.95 similar
  (same proc loop, same `VariantKey` map init and teardown).
- `transformProc`: `return_slot.zig:68` / `str_append.zig:73`,
  **byte-identical** 18 lines (same eligibility gate, same worklist,
  same `DenseMap` visited set, same
  `body_clone.appendSuccessors` walk).
- `chainIsSingleUse`: `return_slot.zig:147` / `str_append.zig:147`,
  **byte-identical**, including its doc comment modulo the word
  "stored"/"concatenated".
- `rewriteAt`: `return_slot.zig:88` / `str_append.zig:93`, 0.74
  similar. The 25 lines of scaffolding (statement-kind check, callee
  eligibility, `forwardLocalAliasChainInto`, single-use check, variant
  lookup, argument prepend, `assign_call` overwrite) are the same;
  only the middle—"the consumer is `ptr_store` into a matching `ptr`
  layout" versus "the consumer is `str_concat` with the result on the
  right"—differs.

`src/lir/loop_append_promote.zig` (`run` at `:60`, `transformProc` at
`:700`) and `src/lir/box_reuse.zig` (`run` at `:56`, `transformProc` at
`:65`, `rewriteAt` at `:100`) repeat parts of the same shape: the
`run`-over-all-procs loop, the visited-set body walk, and the
match-then-rewrite structure.

Nothing enforces that they stay aligned. A fix to `chainIsSingleUse`'s
liveness reasoning—the guard that stops the fusion from orphaning a
still-live local, i.e. the correctness-critical half of both passes—
has to be applied twice, in two files, with no test that would notice
if it were applied once.

## Background

`src/lir/body_clone.zig` already is the shared layer for this family
and both passes already use it: `appendSuccessors` (`:82`),
`forwardLocalAliasChain` / `...Into` (`:34`, `:41`),
`countReachableReads` (`:186`), and `BodyCloner(Rewriter)` (`:396`)—a
comptime-parameterized body cloner, which is exactly the shape this
project extends one level up. The traversal and cloning primitives
were factored out; the pass *skeleton* around them was not.

`BodyCloner(comptime Rewriter: type)` is the in-repo precedent for the
mechanism: one algorithm, a caller-supplied policy type.

## Evidence

- `return_slot.zig:68` vs `str_append.zig:73`—diff is empty.
- `return_slot.zig:147` vs `str_append.zig:147`—diff is empty.
- `return_slot.zig:43` vs `str_append.zig:51`—differs only in the
  extra `layouts` field and the `VariantKey` shape.
- `body_clone.zig:396`—`BodyCloner(Rewriter)`, the pattern to extend.
- No test in `src/lir/` exercises the two passes' shared liveness
  guard as one thing.

## Solution design

1. **Add `CallResultFusion(Rule)` to `body_clone.zig`** (or a new
   `src/lir/call_fusion.zig`). It owns: the per-proc loop, the
   eligibility gate (`body != null`, `hosted == null`, `abi == .roc`),
   the visited-set body walk, the variant cache keyed by
   `Rule.VariantKey`, `chainIsSingleUse`, the alias-chain forwarding,
   the argument prepend, and the `assign_call` overwrite.
2. **`Rule` supplies only what differs**: `VariantKey`,
   `matchConsumer(store, layouts, call, forwarded) ?Match` (the
   `ptr_store`/`str_concat` test plus the extra operand the variant
   needs), `resultEligible(layout)`, and `cloneVariantReturn` (the
   `cloneStructReturn`/`cloneTagReturn` vs `cloneConcatReturn`
   difference). `ReturnSlotRule` and `StrAppendRule` become the two
   instances; `return_slot.zig` and `str_append.zig` shrink to their
   rules plus their tests.
3. **Assess `loop_append_promote.zig` and `box_reuse.zig`
   separately.** They share the outer skeleton but not the
   clone-a-variant core—`box_reuse.zig` rewrites in place and
   `loop_append_promote.zig` classifies helper procs first. Factor out
   what genuinely matches (the proc loop and the visited-set walk, as
   a small `forEachReachableStmt` helper in `body_clone.zig`) and
   leave the rest. Do not force these two into `CallResultFusion` if
   the fit is not clean; record the finding either way.
4. **Test the shared guard once.** `chainIsSingleUse` is the safety
   property both passes depend on. Move its tests to the framework and
   make them exercise every escape it must reject: an extra read of
   the call result, an extra read of an intermediate alias, an extra
   read of the final value, and a read from a different branch.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -rn 'fn chainIsSingleUse\|fn transformProc' src/lir/` shows
  one `chainIsSingleUse` and no duplicate `transformProc` between
  `return_slot.zig` and `str_append.zig`.
- `return_slot.zig` and `str_append.zig` each contain only their rule
  (consumer match, eligibility, variant return clone) and their
  tests—no traversal, no variant cache, no liveness guard.
- The `chainIsSingleUse` escape-case tests live in the framework and
  cover the four escapes above.
- The `loop_append_promote` / `box_reuse` question from step 3 is
  answered in writing—either folded in, or documented as a different
  enough shape to stay separate.
- `git diff test/snapshots` is empty; LIR dumps for a corpus of
  programs exercising both fusions are byte-identical before and
  after.

## How to evaluate the result

### Correctness ideal

The liveness guard that makes both fusions sound exists once, so a fix
to it cannot land on one pass and miss the other. Adding a third
fusion (an obvious future want—`list_concat`, `dict_insert`) is a rule
struct rather than a third copy of the skeleton, and it inherits the
guard and its tests for free.

### Performance ideal

Neutral by construction: same traversal, same variant caching, same
number of passes over each proc. `Rule` is a comptime type parameter,
so the framework monomorphizes per pass and the emitted code should
match today's. Confirm with LIR-dump byte-identity on the corpus
(which also serves as the correctness check) and with post-check phase
timings from `checked_pipeline.zig`'s `lir_passes` counter over a
fixed corpus. If step 3 introduces a shared `forEachReachableStmt`,
check that it does not add an allocation the inlined walks avoided.

## Tests to add

- The four `chainIsSingleUse` escape cases, in the framework.
- A rule-level test per instance: the consumer match accepts its
  intended shape and rejects the neighboring one (a `ptr_store` whose
  destination layout does not match, a `str_concat` with the result on
  the left).
- LIR-dump golden output for one program per fusion, so a future rule
  change is visible.

## Related projects

- [../big/postcheck-lowerer-decomposition.md](../big/postcheck-lowerer-decomposition.md)—
  the same "extract the shared skeleton, keep the scopes" move, one
  stage earlier and much larger.
- [arc-shared-predicates.md](arc-shared-predicates.md)—the other
  `src/lir/` duplication cluster; independent.
