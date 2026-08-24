# One Cloner for Monotype Lifted

## Problem

`src/postcheck/monotype_lifted/spec_constr.zig` (13,647 lines)
contains two independent cloners over the same IR, each with its own
exhaustive-ish switch over `Ast.ExprData`:

- `Pass.cloneExprFresh` (`:3370`) with `cloneStmtFresh` (`:3347`),
  `clonePatFresh` (`:3574`), `cloneExprSpanFresh` (`:3532`),
  `cloneCaptureOperandSpanFresh` (`:3543`), `cloneFieldSpanFresh`
  (`:3557`), `clonePatSpanFresh` (`:3621`). Covers 29 expression
  variants and returns `?Ast.ExprId`—`null` for everything it does not
  handle.
- `Cloner.cloneExprPlain` (`:5727`), part of the `Cloner` struct
  (`:4528`), with `cloneJoinPoint` (`:5916`), `cloneLetValue`
  (`:5967`), `cloneLoopBody` (`:6176`), and the substitution-aware
  `cloneExpr` / `cloneExprValue` family (`:5081`–`:5458`). Covers 44
  expression variants.

The 15-variant gap is the problem's shape. `cloneExprFresh` silently
declines `break_`, `loop_`, `join_point`, `jump`, `record_update`,
`expect`, `expect_err`, `dbg`, `try_sequence`,
`try_record_sequence`, `if_initialized_payload`, `uninitialized`,
`uninitialized_payload`, `comptime_branch_taken`, and
`comptime_exhaustiveness_failed`. Because it returns an optional
rather than switching exhaustively, adding a new `Ast.ExprData`
variant compiles clean and is silently declined by one cloner and
handled by the other—and "declined" here means a specialization
opportunity is dropped, which is invisible except as a performance
regression nobody attributes to it.

`monotype_lifted/lift.zig` adds a third partial traversal of the same
IR: `bindPat` at `lift.zig:1570` and `:2043` against
`spec_constr.zig:10512` (0.68 and 0.89 similar respectively).

## Background

The two cloners genuinely do different jobs. `Cloner.cloneExprPlain`
is the specialization cloner: it copies a function body while
substituting known-shape arguments, tracks binding chains, and manages
loop-exit selection. `Pass.cloneExprFresh` is the narrower "duplicate
this subtree with fresh locals" helper used for carry duplication
(`cloneNewCarry`, `:3234`) and in-place body cloning
(`cloneFnBodyInPlace`, `:3636`).

That difference justifies two *policies*, not two *traversals*. The
part that must stay in lockstep with the IR—which variants exist, what
children each has, which spans need cloning—is identical between them,
and it is the part that a new variant changes.

`src/lir/body_clone.zig:396` shows the intended shape at the LIR
layer: `BodyCloner(comptime Rewriter: type)`—one traversal, a
caller-supplied policy. Monotype Lifted has no equivalent.

This project sits directly on top of two landed concerns:
`project_issue_10561` (cloneCallProc cloning each argument twice,
producing 2^depth growth on nested call chains) and
`project_issue_9801` (a slice into `program.fns` invalidated by a
`Cloner` append). Both were bugs in cloning mechanics. A single
traversal is the structural answer to that class.

## Evidence

- `spec_constr.zig:3370` vs `:5727`—29 variants against 44, over the
  same `Ast.ExprData`.
- `cloneExprFresh` returns `?Ast.ExprId`; its callers treat `null` as
  "skip this specialization", so an unhandled variant is silent.
- `lift.zig:1570` / `lift.zig:2043` / `spec_constr.zig:10512`—three
  `bindPat`.
- `src/lir/body_clone.zig:396`—the pattern this project ports upward.

## Solution design

1. **Write `LiftedCloner(comptime Policy: type)`** in
   `src/postcheck/monotype_lifted/`. It owns the exhaustive switch
   over `Ast.ExprData`, `Ast.PatData`, `Ast.StmtData`, and every span
   kind, and it dispatches to `Policy` at the points where the two
   current cloners differ: local renaming, value substitution, join
   retargeting, and the decision to decline.
2. **Two policies.** `FreshPolicy` renames locals and declines what it
   must; `SpecPolicy` substitutes known shapes and threads binding
   chains. Both get every variant handed to them, so declining is an
   explicit `Policy` decision at a named variant rather than an
   omitted switch arm.
3. **Make declining explicit and countable.** Where `FreshPolicy`
   cannot clone a variant, it says so by name and the pass counts it.
   The `lambda_mono` differential runner's rule applies:
   unsupported constructs are counted and reported per reason, never
   silently skipped. That turns today's invisible dropped
   specializations into a number someone can look at.
4. **Fold `bindPat`.** The three copies in `lift.zig` and
   `spec_constr.zig` become one, in whichever module owns pattern
   binding for the lifted IR.
5. **Carry the existing hazards into the framework.** Two traps are
   already documented in this file's history and must be encoded in
   the shared traversal, not rediscovered per policy: no slice into
   `program.fns` (or any store table) may be held across a clone
   append, and no argument may be cloned twice on a call path. Assert
   both in Debug.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- One exhaustive traversal of `Ast.ExprData` for cloning exists;
  adding a variant is a compile error until both policies handle it.
- `grep -rn 'fn cloneExprFresh\|fn cloneExprPlain' src/postcheck/` is
  empty—both are policies over the shared traversal.
- `grep -rn 'fn bindPat' src/postcheck/monotype_lifted/` shows one
  definition.
- Declined variants are counted and reportable per reason, and the
  count is zero or explained for the corpus.
- Debug assertions cover the store-slice-across-append and
  double-clone hazards, with a test that trips each.
- `git diff test/snapshots` is empty and SpecConstr's lifted program
  size on a corpus is unchanged or smaller—measured on the lifted
  program, not the resulting LIR.

## How to evaluate the result

### Correctness ideal

A new Monotype Lifted expression variant cannot be half-supported. The
two cloning *policies* stay honest about what they decline, and the
mechanics that both bug fixes in this file's history were about—append
invalidation and double cloning—are asserted in one place instead of
being properties of two hand-written switches.

### Performance ideal

This is a compile-time-only pass, and the risk is real in both
directions. Upside: `cloneExprFresh`'s 15 silently-declined variants
are today's dropped specializations; handling any of them may improve
generated code, and step 3's counter tells you which are worth
handling. Downside: SpecConstr is a known compile-time hot spot with a
history of superlinear blowups, so the shared traversal must not add
per-node work or allocation. Measure lifted-program expression count
and post-check phase timing (`checked_pipeline.zig`'s `spec_constr`
counter) over a fixed corpus before and after, and treat any growth in
either as a defect to explain before landing.

## Tests to add

- A comptime assertion that the shared traversal handles every
  `Ast.ExprData`, `Ast.PatData`, and `Ast.StmtData` variant.
- A test per policy asserting the decline set is exactly what is
  intended, so a variant silently entering or leaving it fails.
- Regression pins for the two encoded hazards: a nested call chain
  that would double-clone, and a clone sequence that would invalidate
  a held store slice.
- Lifted-program-size pins on the SpecConstr corpus.

## Related projects

- [spec-constr-specialization-limits.md](spec-constr-specialization-limits.md)—
  termination budgets for the same pass; land either first, but the
  budget work is easier once there is one traversal to budget.
- [postcheck-ir-store-boilerplate.md](postcheck-ir-store-boilerplate.md)—
  the storage layer beneath this traversal.
- [lift-capture-single-sourcing.md](lift-capture-single-sourcing.md)—
  the other `monotype_lifted` single-sourcing project; `bindPat`
  (step 4) touches the same file.
