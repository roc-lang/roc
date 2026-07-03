# Replace the Dispatch Ambiguity Sweep with a Generalization-Time Rule

## Problem

A static dispatch is *ambiguous* when the receiver's type variable can never
be pinned to a concrete type by anything in the program — no argument, no
annotation, no data flow determines it. Such a dispatch has no principled
target; if it survives checking, monomorphization either picks arbitrarily or
crashes. Issue roc-lang/roc#9485 showed the stakes: `roc check` reported a
clean program while `roc build` produced undefined behavior, because a
dispatch receiver was an unpinnable flex var.

PR roc-lang/roc#9504 fixed this by rejecting ambiguous dispatch at check time
— but its mechanism is a whole-CIR post-pass that *reconstructs* pinnability
after the fact, because the information needed to judge ambiguity was not
kept when the constraint was created. In `src/check/Check.zig` today:

- `collectPinnableVars` walks the module collecting every var reachable
  through argument positions and lambda parameters ("someone could pin this
  from outside").
- `collectDataReachableVars` implements a data-position test.
- `instantiateVarHelp` records per-instantiation receivers so
  `reportAmbiguousStaticDispatchPerInstantiation` can judge instantiated
  copies; `reportAmbiguousStaticDispatch` judges the rest.
- Side tables map vars back to source expressions across instantiation:
  `constraint_fn_var_map`, `constraint_expr_by_fn_var`,
  `expect_region_by_constraint_fn_var` (added/extended by PR
  roc-lang/roc#9660 for nested where-clauses).
- Special carve-outs exempt `from_numeral` (literal) and `is_eq` constraints
  (see the "non-literal/non-`is_eq` dispatch constraint" bookkeeping around
  the sweep's worklists).
- PR roc-lang/roc#9819 threaded `discarded_binding_rhs_expr` so discarded
  `_ =` bindings are judged correctly.
- PR roc-lang/roc#9746 added a `body_required` bit on where-clause constraint
  origins (`src/types/types.zig`, `where_clause: struct { body_required: bool
  = false }`), set by fn-name matching in `src/check/Check.zig` (the
  `backing[i].fn_name.eql(constraint.fn_name)` loop) and preserved across
  unification merges in `src/check/unify.zig` — where a comment documents
  that the bit is deliberately dropped for `from_literal`/operator origins.
  The sweep is now a four-way decision table over constraint origins.

This reconstruction has misfired in both directions: false positives (issue
roc-lang/roc#9632 — valid recursive method calls rejected; fixed by PR
roc-lang/roc#9664) and false negatives (issues roc-lang/roc#9657 and #9815 —
ambiguous programs accepted, #9815 reaching a runtime crash). Each fix adds
another side table or origin bit. The sweep grows because it stands at the
wrong place: it tries to recover, module-wide and after the fact, a judgment
that is local and cheap at the moment a type variable generalizes.

## Background

The compiler pipeline is: parse → canonicalize → type-check (producing
"checked artifacts" per module; all user-facing diagnostics end here) →
postcheck: Monotype IR (monomorphization/specialization,
`src/postcheck/monotype/`) → Monotype Lifted (closure lifting) → Lambda
Solved → Lambda Mono → LIR lowering (`src/postcheck/solved_lir_lower.zig`) →
ARC insertion (`src/lir/arc*.zig`) → backends. `design.md` at the repo root
is the authoritative post-check design; its Core Principles require all
user-facing failures to be reported during checking, and forbid later stages
from recovering missing facts — which is why ambiguity *must* be a check-time
error and why the monotype stage's crash fallback for it (see Related
projects) is a design violation.

Key type-checker concepts:

- *Flex var*: an unsolved type variable. *Rigid var*: a variable bound by an
  annotation. Static dispatch constraints (`where a.method : ...`, or
  implicit ones from method calls, literals via `from_numeral`, and operators
  like `is_eq`) attach to variables.
- *Generalization*: when a definition's type is finalized, unsolved variables
  local to it are promoted into a *scheme* (a polymorphic type). Each use
  *instantiates* the scheme with fresh variables.
- A dispatch-constrained variable is legitimate in a scheme if a *use site
  can pin it* — i.e. it is reachable from the scheme's argument or data
  positions, so instantiation at a concrete type determines the dispatch. It
  is *ambiguous* if it appears nowhere a caller can influence (the classic
  example: a variable appearing only in constraints, like Haskell's
  `show . read`).
- Haskell performs exactly this judgment as the *ambiguity check* at
  generalization time. That is the model here.

Why the sweep exists instead: rank information is not usable for this
judgment in the current solver — the generalizer promotes orphan vars to
generalized rank, erasing the distinction the check needs — and the
constraint store lacks *provenance*: when a constraint is created there is no
durable record of which expression introduced it and in what role, so error
reporting after the fact requires the side tables listed above.

## Evidence

All symbols verified in the current tree; search `src/check/Check.zig` for
`reportAmbiguousStaticDispatch` and `collectPinnableVars` to find the sweep.

- `src/check/Check.zig`: `collectPinnableVars`, `collectDataReachableVars`,
  `reportAmbiguousStaticDispatch`,
  `reportAmbiguousStaticDispatchPerInstantiation`, `instantiateVarHelp`
  (constraint-metadata copying), fields `constraint_fn_var_map`,
  `constraint_expr_by_fn_var`, `expect_region_by_constraint_fn_var`,
  `discarded_binding_rhs_expr`; the sweep runs from `checkFileInternal` after
  def checking.
- `src/types/types.zig`: constraint origin union with `where_clause { body_required }`,
  `method_call`, `from_literal`, `desugared_binop`, `desugared_unaryop`.
- `src/check/unify.zig`: the constraint-merge logic preserving/promoting
  `body_required`, with the comment explaining why `from_literal`/operator
  origins deliberately drop it.
- History: PR #9504 (introduced sweep; fixed issue #9485), PR #9660 (nested
  where-clauses; added the var→expr side tables), PR #9762 and PR #9819
  (further tuning; discarded `_ =` bindings), PR #9746 (`body_required`),
  issue #9632 / PR #9664 (false positive on recursive method calls), issues
  #9657 and #9815 (false negatives; #9815 escaped to a runtime crash in
  monotype lowering).

## Solution design

Enforce ambiguity **at the moment of generalization**: when a
dispatch-constrained type variable is about to be generalized into a scheme
without being reachable from the scheme's argument or data positions, report
the error right there, pointing at the dispatch site that introduced the
constraint.

1. **Constraint provenance (foundation).** Extend the static dispatch
   constraint record so that every constraint carries, from creation:
   - the introducing expression (`CIR.Expr.Idx`) and region,
   - its role (where-clause of an annotation, method call, literal
     `from_numeral`, desugared operator — the origin union already encodes
     role; add the expression/region payload to it),
   - and make instantiation copy provenance verbatim (instantiated
     constraints point at the original introducing site).
   This replaces the after-the-fact var→expr side tables: provenance travels
   *inside* the constraint instead of alongside it in maps keyed by mutable
   vars.
2. **The generalization-time rule.** At generalization of a definition:
   - compute the set of variables reachable from the scheme's argument and
     data positions (a local traversal of the def's type, not the module —
     this is the existing `collectDataReachableVars` logic scoped to one
     scheme);
   - for each variable being generalized that carries a dispatch constraint
     whose role requires pinning (where-clause with a forcing body, method
     call; literals default instead, `is_eq` resolves structurally), check
     membership;
   - if unreachable, emit the ambiguity diagnostic using the constraint's
     provenance: "this method call's receiver type can never be determined",
     pointing at the introducing expression.
   Recursive definitions judge against the fully-solved scheme (the #9632
   lesson: a constraint on the function's own type variable that unifies with
   the definition's scheme is not ambiguous).
3. **Literal and operator handling.** `from_literal` constraints are pinned
   by numeric defaulting, which runs before generalization finalizes — the
   rule sees them already solved, so no carve-out logic is needed; the same
   for structural `is_eq`. What remains is a single reachable-or-error
   judgment, not a four-way origin table.
4. **Deletions.** After the rule lands and the regression corpus passes,
   delete from `src/check/Check.zig`: `collectPinnableVars`,
   `reportAmbiguousStaticDispatch`,
   `reportAmbiguousStaticDispatchPerInstantiation`, the per-instantiation
   receiver recording in `instantiateVarHelp`, `constraint_fn_var_map`,
   `constraint_expr_by_fn_var`, `expect_region_by_constraint_fn_var`,
   `discarded_binding_rhs_expr` threading, and the sweep invocations in
   `checkFileInternal`. `body_required` in `src/types/types.zig` and its
   unify.zig merge logic go too if provenance + the local rule subsume them
   (the "body forces this where-clause" fact becomes: the constraint's
   introducing site is a body dispatch — provenance carries it).
5. **Migration order.** (a) add provenance to constraint creation and
   instantiation (mechanical, no behavior change); (b) implement the
   generalization-time rule behind a debug flag that runs *both* rule and
   sweep, logging disagreements; (c) burn down disagreements against the
   regression corpus (all five Evidence issues); (d) flip to the rule alone
   and delete the sweep in one PR.

Interaction with total dispatch plans: that project's `constraint(k)`
representation (see Related projects) gives each where-clause constraint a
stable index and site — natural provenance. Either project can land first,
but they share the constraint-provenance foundation; build it once so both
consume the same record.

## What success looks like

- Ambiguity is judged by a local rule inside generalization; no whole-module
  pass named `reportAmbiguousStaticDispatch*` or `collectPinnableVars`
  exists in the tree.
- The three var→expr side tables and `discarded_binding_rhs_expr` are gone
  from `src/check/Check.zig`.
- The #9632 repro (recursive method calls) is accepted; the #9485, #9657,
  #9815, and #9819 discarded-binding repros are rejected at check time, each
  diagnostic naming the introducing dispatch site.
- Every ambiguity diagnostic points at an expression the user wrote, derived
  from stored provenance rather than reconstructed mappings.

## How to evaluate the result

### Correctness ideal

The rule is a *local judgment at generalization*: its inputs are one scheme,
its constraint set, and per-constraint provenance — never a module-wide
traversal. Invariants and enforcement points:

- *No dispatch-constrained var generalizes unpinnably*: enforced inside the
  generalizer itself; in debug builds, additionally assert at artifact
  publication that every generalized var carrying a pin-requiring dispatch
  constraint is argument/data-reachable in its scheme (a cheap re-walk, debug
  only).
- *Provenance totality*: debug-assert at constraint creation and at
  instantiation copy that every dispatch constraint has an introducing
  expression/region.
- *Downstream corollary*: monotype lowering never sees an unpinned dispatch
  receiver — the crash fallback for it can be deleted (coordinated with the
  total-dispatch-plans project).
- Maintain a false-positive/false-negative regression corpus built from all
  five issues (#9485, #9632, #9657, #9815, #9819's case) plus the existing
  snapshot suite; the rule must match the corpus exactly, with no
  origin-specific carve-out code paths left to drift.

### Performance ideal

Check time strictly improves: the sweep's whole-CIR passes
(`collectPinnableVars` over every def, per-instantiation receiver recording
in `instantiateVarHelp`, the final report passes in `checkFileInternal`)
disappear, replaced by an O(scheme size) traversal per generalized
definition. Instantiation gets cheaper: no side-table writes per instantiated
constraint (provenance is copied inline with the constraint it already
copies). Measure `roc check` wall time on a module-heavy corpus (the builtins
plus a many-module app or package workspace) before and after; expect neutral
to slightly faster, and any regression in single-module check time is a bug.
Memory: three hash maps per Check instance are removed; constraint records
grow by one expr index + region, which must not measurably increase peak
check memory.

## Tests to add

- Acceptance: the #9632 recursive-method-call repro stays accepted (guard
  against reintroducing the false positive).
- Rejection with provenance: #9485, #9657, #9815, and the #9819 discarded
  `_ =` binding case each rejected at check time; snapshot the diagnostics
  and assert they name the introducing dispatch expression's region.
- Scheme-position matrix: dispatch-constrained var reachable via (a) direct
  argument, (b) nested data position inside an argument, (c) return-only
  position, (d) constraint-only position — (a)/(b) accepted, (c)/(d) rejected
  per the rule's definition of pinnable.
- Instantiation provenance: a generic function whose ambiguous constraint is
  introduced in module A and instantiated in module B reports against the
  site in A.
- Literal/operator non-interference: numeric-literal-only and `==`-only
  programs never trigger the rule (defaulting/structural resolution pins
  them first).
- Differential harness (temporary, during migration step b): run sweep and
  rule together over the snapshot corpus and fail on any disagreement.

## Related projects

- [../big/total-dispatch-plans.md](../big/total-dispatch-plans.md) — shares
  the constraint-provenance foundation; its `constraint(k)` indices are
  natural provenance carriers. Either order works; build provenance once.
- [../small/check-app-against-platform-requires.md](../small/check-app-against-platform-requires.md)
  — upstream of both dispatch projects: platform-typed receivers must be
  concrete at check time for pinnability judgments to be meaningful in app
  modules.
