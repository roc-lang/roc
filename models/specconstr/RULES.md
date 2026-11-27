# SpecConstr model: rules

Every rule below is described in design.md, "SpecConstr And Loop Scalarization", unless the row names another
section. Compiler sites are functions in `src/postcheck/monotype_lifted/spec_constr.zig` unless another file is named.
Model rules are Maude operators in `maude/sc.maude` (the cloner and pass phases) and `maude/pass.maude` (the driver and
checks).

## The IR fragment (`maude/ir.maude`)

| Model | Lifted IR | Notes |
|---|---|---|
| `v i tg tp pj` | local, int literal, tag, tuple/record, field read | Records are tuples (positional fields). |
| `lt mt cl pr` | let, match, direct call, low-level op | No guards. Patterns: constructor, binder, wildcard, tuple, int literal. |
| `lp ct bk` | `loop_`, `continue_`, `break_` | `ct`/`bk` target the innermost lexical loop. |
| `jn jp` | typed join and jump | Joins are lexically scoped and may be recursive. |
| `ivk` | a `Common.invariant(...)` the pass reaches | Ill typed by construction, so check (1) reports it. |

Not modeled: callables (`call_value`, captures, capture finalization), iterator fusion, `return`, `typed_boundary`,
nominal wrappers, strings and lists, prefix splits, tail-recursive worker localization, record update, and
`BindingChain` structure beyond "strict work is named once, in order". The `wide` generator family covers the
callable, string, list and prefix-split shapes by running them through `roc` only.

## Checks (`maude/pass.maude`)

| Check | Model | What it says |
|---|---|---|
| (1) type preservation | `wellTyped(sc(P))` | arguments fit parameters, branches agree, continue and jump arity and types fit the innermost loop or join |
| (1) no new types | `newTypes(P, sc(P)) == tsnil` | every type of the output, closed under components, is already a type of the source (design.md: SpecConstr creates no types) |
| (2) semantic preservation | `semPres` | `run(P) == run(sc(P))` on every input where the source terminates within fuel |
| (3) termination | `SC-PASS` with `search ... such that viol(C)` | every drain step strictly decreases (free worker capacity, originals not yet normalized, unwritten workers), lexicographically, under every coordinator order |

`passes(check(P, flags, f, inputs, fuel))` is checks (1) and (2) together.

## Rules

| Rule (`maude/sc.maude`) | What it says | Compiler site | Model vs. compiler |
|---|---|---|---|
| **C-Collect** `collectE`, `recordPattern`, `ctorShape` | call patterns come from explicit constructor arguments | `collectCallPatternsInExpr`, `recordCallPattern`, `constructorShape` | Discovery-task patterns are not a separate phase; the model finds them while cloning. |
| **W-Admit** `stepCallProc`, `recordPattern` | admit a pattern when the source body is within 200 nodes, the function has fewer than 3 workers, the pattern has a constructor, and it is not a duplicate | `newSpecAdmission`, `ensureCallPatternForValues`, `recordCallPatternForValues`, `spec_constr_body_expr_threshold` | design.md says dedup comes before admission. `recordCallPatternForValues` checks admission first; the model does size, count and dedup in one condition. |
| **W-Used** `usedArg`, `usedMask` | an argument the callee does not consume becomes `any` | `collectArgUses` | The model bounds the use fixpoint at depth 3. |
| **W-Select** `bestSpec`, `better`, `smatch` | the most specific matching worker: most constructor nodes, ties by admission order; no type check at `any` | `specSelectionOrder`, `callPatternMatchesValues`, `shapeMatchesValue` | Same. |
| **W-Call** `stepCallProc`, `leaves` | a call becomes `worker(leaves in pre-order)` | `stepCallProc`, `stepAppendExprs` | `leaves` on a non-matching value yields `ivk(...)`, as the compiler's invariant does. |
| **W-Write** `writeSpec`, `shArgs`, `shParams` | clone the sealed source body with parameters rebuilt from fresh leaves, under a root frame | `writeSpecialization`, `buildArgs`, `valueFromShapeArgs` | Flag `'B10760` reads the rewritten output body instead. |
| **W-Redirect** `redirect`, `redirCall`, `synVal` | once, between drains, calls inside emitted workers are redirected by syntactic argument shape | `rewriteExistingCalls`, `rewriteCallProc` | `is_cold` is not modeled. |
| **N-Norm** `normalize` | value-aware clone of each original body, with no root frame | `rewriteAllOriginalBodies`, `cloneFnBody` | Same: an original body may inline one copy of itself. |
| **Drain** `drain`; rules `write1`, `write2`, `norm` | write unwritten workers until none are left; under `search` the order is free | `createSpecializations` | `renormalizeSplitBodies` is not modeled. |
| **I-Inline** `clCall` | inline when the callee is a wrapper or the call is productive, the active-stack guard holds, and the budget covers the body; push a frame and clone the source body | `admitsDirectInline`, `stepInlineDirect`, `admitInlineBodyGrowth` | Productivity is approximated as a known constructor argument at a parameter the callee consumes. Result crossing (`resultCrosses` in `inline_crossing.zig`) is not modeled. The 65,536 budget is per clone, as in the compiler. |
| **I-Guard** `guardOk`, `guardFrame`, `ksize` | for each active frame of the callee, refuse if the frame has no size, the size is 0, or the new size is not strictly smaller | `stepInlineDirect`, `knownConstructorSize` | Same. Opaque arguments count 0. |
| **I-Wrapper** `wrapper` | a body that is one call, constructor or tuple over atoms, and not self-recursive | `wrapper_body.zig`, `classifyWrappers` in `inline_crossing.zig` | The model detects cycles only through self-calls. |
| **KC** `clMatch`, `select`, `verdict`, `vmerge` | pick the first branch whose verdict is yes when every branch before it is no; any unknown verdict, or all no, declines | `stepSelectKnownMatch`, `stepBindPat`, `mergeMatchUnknown` | Literal patterns give unknown. Flag `'B9975` makes them no and an all-no match an invariant. `if` on a known condition is never folded: the model's `if` matches a `Bool` built by a primitive, which is opaque. |
| **Residual** `residual`, `cloneArms`, `renamePat` | arms are cloned with fresh binders | hygiene | Same. |
| **CoC** `cln(mt(mt ...))`, `coc1`..`coc5`, `reach`, `useJoin` | case-of-case: count the leaves reaching each outer arm; an arm reached twice or more becomes a join unless it only builds a value; any opaque leaf declines the whole distribution | `stepCaseOfCase`, `planCaseOfCaseArms`, `countCaseOfCaseArmReach`, `caseArmOnlyBuildsValue` | Outer branches must be binder-free. Nested inner cases decline. Flag `'B10124` copies the outer arm at every leaf. |
| **LoC** `cln(lt(X, mt ...))`, `loc1`, `loc2`, `locDispatch`, `locPattern` | let-of-case: when the rest immediately matches the bound value with binder-free branches, one join per continuation branch; otherwise one join whose parameters are the leaves of the shared skeleton; a join with one site is cloned at the site | `stepLetOfCase`, `letCaseJoinPlan`, `stepFinalizeLetCaseJoin`, `inline_let_case_join`, `rebuild_let_case_join_value` | design.md inlines a one-site join only when its body is closed under loop control. Neither the model nor `stepFinalizeLetCaseJoin` checks that. |
| **J-Copy** `cln(jn ...)`, `cln(jp ...)` | source joins are copied hygienically | hygiene | |
| **L-Scal** `clLoop`, `loopAttempt*`, `slotParams` | scalarize loop slots whose initial value is a known constructor, clone the body, and retry on demotion | `stepLoopValue`, `nextLoopAttempt`, `rewindLoopAttempt` | The attempt measure (constructor nodes in the slot shapes) must strictly decrease; the model states it as a rule condition. |
| **L-Cont** `cln(ct ...)`, `clCont`, `supply` | check arity against the top loop frame, then supply leaves or demote the slot | `stepContinue`, `supply_loop_slot`, `demoteLoopSlotLeaf` | Same, including the frame sharing the slot shapes. |
| **L-Exit** `exitSel`, `exitRewrite`, `selected`, `selBreaks`, `exitJoin` | for `let r = loop ...` with a tuple result of two or more components read only by projection, keep the read components: one kept gives the loop that component's type; two or more give a join that every owned break jumps to | `projectUnusedLoopResultsInFn`, `loop_exit_demand.zig`, `stepLoopExitValues`, `stepSelectedLoopExit` | Each exit names every component in order, which keeps strict work. Flag `'B10438` mints a tuple type of the kept components. |
| **J-Inline1** `exitJoin2`, `inlineJump` | a one-site exit join is inlined at its jump unless its body has free loop control | `exprContainsFreeLoopControl` | Flag `'B10461` drops the check. The inlined loop's type becomes the continuation's (`tAuto`). |
| **P-Fold** `maude/paths.maude` `fold` | the inline-productivity demand-path fixed point folds a path back where a part's type repeats (`new`); the earlier rule folded on a repeated payload label (`old`) | demand paths in `inline_crossing.zig` | Paths only; the rest of inline crossing is not modeled. |

## Bug flags

A flag puts one rule back to its behavior before a fix. The number is the roc-lang/roc issue or PR.
`tests/calib.maude` and `tests/growth.maude` show each fixed rule passing and each flag failing.

| Flag | Pre-fix behavior | Shown by |
|---|---|---|
| `'B9975` | literal patterns verdict no, and an all-no match is an invariant | wrong result, or a reached invariant |
| `'B10124` | case-of-case copies the outer arm into every leaf | exponential code growth |
| `'B10438` | loop exit selection mints a new tuple type | check (1), new type |
| `'B10461` | one-site exit join inlined across a `continue` of an outer loop | check (1), continue arity |
| `'B10760` | worker clones read the rewritten output, not the sealed source | no fixture distinguishes it yet |

## Model errors found and fixed

- A loop whose exits all jump (after L-Exit) looked ill typed when every loop took its annotation. Such a loop now has type ⊥.
- The one-site exit-join inline gives a loop that falls through with the continuation's value. Its annotation is `tAuto`.
- L-Exit rewrote breaks in a `let` body and in match arms, but not in a `let`'s bound expression or a match scrutinee,
  so `x = match v { 3 => break ... }` inside the loop kept its old break and the output was ill typed. Fixed; pinned by
  `PLETBRK` in `tests/calib.maude`.
