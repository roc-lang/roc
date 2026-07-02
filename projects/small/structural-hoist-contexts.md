# Structural Hoist-Suppression Contexts

## Problem

Compile-time constant hoisting selects eligible expressions inside runtime
bodies as compile-time-evaluation roots DURING type checking. The contexts
where selection must NOT fire are a manually-maintained blacklist: a wrapper
function, `checkExprWithHoistSelectionSuppressed` in src/check/Check.zig, is
called at roughly ten hand-picked sites (if/match branch bodies, match
guards, certain block final expressions, and — since PR roc-lang/roc#9862 —
expect bodies), bumping a suppression-depth counter around the recursive
`checkExpr` call.

Every context MISSING from that blacklist is a latent bug of the same shape.
Issue roc-lang/roc#9747: `expect { result = prime(10001); ... }` ran a
multi-minute interpreter compile-time evaluation during `roc check`, because
expect bodies were not yet in the blacklist — the binding inside the expect
was selected as a hoisted root and evaluated at checking finalization.
PR 9862 added that ONE case. The debug consistency assertion
(`debugAssertHoistSelectionConsistent`) verifies selected-root index
bookkeeping and can catch frame errors, but it CANNOT catch a missing
suppression context: to the assert, a wrongly-selected root is perfectly
consistent state.

The next syntactic context anyone adds to the checker — or the next existing
context someone realizes should suppress — is one forgotten wrapper call
away from being the next 9747.

## Background

The compiler pipeline: parse → canonicalize → type-check (src/check/Check.zig;
unification in src/check/unify.zig; rows are how records and tag unions are
typed — a row can be open or closed) → checked artifacts → postcheck:
Monotype IR → Lifted → Lambda Solved → Lambda Mono → LIR → ARC → backends.
design.md at the repo root is authoritative; its section "Compile-Time
Constants and Hoisted Roots" governs this feature.

Hoisting (PR roc-lang/roc#9722) selects "top-level-equivalent" expressions —
closed, pure, concrete, no runtime arguments or mutable state — from runtime
function bodies. Selected roots are evaluated at checking finalization and
stored in `ConstStore`; later lowering restores the stored value instead of
emitting runtime work. Per design.md, hoistability is computed as part of
the SINGLE recursive checking traversal that already determines types and
effects; it must not add a second pass or permanent per-expression
summaries. That single-traversal design is mandated for compile speed and
must be preserved by this project.

**Policy constraint (a deliberate decision by the project lead):** hoisting
must NOT gain a cost model. Whether an eligible expression evaluates at
compile time is LANGUAGE SEMANTICS and must not depend on a heuristic
threshold ("this looks expensive, skip it"). The cost of compile-time
evaluation is addressed separately by making CTFE fast — native-code CTFE
via the dev backend (PR roc-lang/roc#9868) and caching of hoisted-root
results — never by refusing to hoist. This project is about WHERE selection
fires, not WHETHER eligible work is worth doing.

## Evidence

- src/check/Check.zig — `checkExprWithHoistSelectionSuppressed` increments
  `hoist_selection_suppressed_depth` around `checkExpr`. Eleven call sites
  today: expect bodies (via `checkExpectBody`, the PR 9862 fix), block
  final expressions when a preceding statement blocks later hoists, if
  branch bodies and the final else, and match guards and branch values.
- src/check/Check.zig — selection entry points check the counters, e.g.
  `recordHoistBindingCandidate` begins with
  `if (self.hoist_suppressed_depth != 0 or
  self.hoist_selection_suppressed_depth != 0) return;`.
- src/check/Check.zig — hoist state threaded through the checker: about
  fifteen fields (`hoist_frames`, `hoist_expr_candidates`,
  `hoist_binding_candidates`, `hoist_known_values`,
  `hoist_contextual_bindings`, `hoist_selected_bindings`,
  `hoist_selected_exprs`, `selected_hoisted_roots`, `last_hoist_result`,
  `hoist_suppressed_depth`, `hoist_selection_suppressed_depth`, plus scope
  stacks), the `HoistFrame` struct, and helpers like
  `beginHoistLexicalScope` / `currentHoistFrameIndexForExpr`. The word
  "hoist" appears ~500 times in Check.zig (511 at time of writing).
- src/check/Check.zig — `debugAssertHoistSelectionConsistent`: checks that
  `hoist_selected_exprs` / `hoist_selected_bindings` indexes agree with
  `selected_hoisted_roots`; nothing checks WHERE selection happened.
- src/check/hoist_roots.zig — `SelectedHoistedRoot`, `Body`
  (`expr` / `pattern_extraction`): the producer-side selected-root data.
- design.md, "Compile-Time Constants and Hoisted Roots" — single-traversal
  mandate, allowed/rejected dependency rules, sparse selected-root storage.
- Issue roc-lang/roc#9747; PR roc-lang/roc#9862 (merge be8fdf22cc); PR
  roc-lang/roc#9722 (merge 5264f0ea95); PR roc-lang/roc#9868 (merge
  c3e1d7472f, native CTFE via dev backend).

## Solution design

Invert the blacklist into a STRUCTURAL WHITELIST: define hoist-ELIGIBLE
positions structurally, make everything else suppressed by default, and
carry the context as data passed down `checkExpr` rather than as mutable
counter state that call sites must remember to bump.

1. Derive the exact current-eligible position set by reading the code. As
   implemented today, selection fires for: immutable statement-level def
   right-hand sides in unconditionally-executed statement positions of
   runtime bodies (`recordHoistBindingCandidate`), whole-expression
   candidates accumulated per `HoistFrame` (`hoist_expr_candidates`), and
   single-branch match pattern extractions
   (`recordHoistPatternExtractionProvenance`). Write this set down in the
   code as the definition of eligibility, and confirm it against the
   suppressed-site list (anything neither eligible nor suppressed today is
   a bug to triage before the refactor).
2. Add a hoist-position field to the checking context that flows down
   `checkExpr` — the `Expected` struct already flows to every recursive
   call and already models per-position facts (e.g.
   `comptime_condition_warnings`); either extend it or introduce a small
   context struct passed alongside. The field is an enum such as
   `hoist_position: enum { suppressed, eligible_statement_rhs, ... }` with
   `suppressed` as the DEFAULT for every derived context (`forBranchBody`,
   `forStatement`, guard contexts, etc. all produce `suppressed` unless
   they explicitly opt in).
3. Only the enumerated structural positions construct an eligible context.
   A NEW syntactic context added to the checker gets `suppressed` for free:
   a missed enumeration is now impossible rather than a latent bug —
   forgetting to opt IN costs a missed optimization caught by snapshot
   tests, never a wrong compile-time evaluation.
4. Selection entry points (`recordHoistBindingCandidate` and friends) read
   the context field instead of the counters.
5. **DELETE**: `checkExprWithHoistSelectionSuppressed` and all eleven call
   sites; the `hoist_selection_suppressed_depth` counter; fold
   `hoist_suppressed_depth` (suppression inside top-level constant bodies,
   where the whole body is already a compile-time root) into the same
   context field so one mechanism remains.
6. Keep the single-traversal design. This project moves a fact from mutable
   checker state into the down-flowing context; it must NOT add a second
   pass over the CIR (design.md forbids it).

## What success looks like

- `checkExprWithHoistSelectionSuppressed` no longer exists; grep finds no
  suppression-depth counters in the checker.
- Adding a new expression or statement kind to the checker CANNOT silently
  make its subexpressions hoist-eligible: eligibility requires explicitly
  constructing an eligible context, and the default is suppressed.
- The set of hoist-eligible positions is readable in ONE place (the context
  constructors), not reverse-engineered from wrapper call sites.
- All currently-selected roots are still selected (no optimization
  regression) and all currently-suppressed contexts stay suppressed.

## How to evaluate the result

### Correctness ideal

- A context-matrix snapshot test exists: one eligible expression placed in
  EVERY syntactic position (def RHS, if condition, if branch, match
  scrutinee, match guard, match branch, block final, expect body, lambda
  body, call argument, record field, ...) with a snapshot of which
  positions produced hoisted roots. Future syntax must extend the matrix or
  fail review.
- Hoisted semantics are unchanged for all currently-eligible positions:
  the full snapshot corpus (src/snapshots/) shows no diffs in selected
  roots, diagnostics, or evaluated values.
- The 9747 scenario (expensive pure call bound inside an `expect` body)
  performs no compile-time evaluation during `roc check`.

### Performance ideal

- No additional traversal of the CIR; the context is computed from the
  parent context in O(1) at each recursive call.
- The context struct passes by value in registers; checker struct size
  shrinks (two counters and their bookkeeping removed).
- `roc check` wall-time is unchanged on a large module (measure before and
  after on the same corpus).
- CTFE RUNTIME performance is explicitly out of scope here: it is tracked
  by the native-CTFE dev-backend work (PR 9868) and hoisted-root result
  caching, per the no-cost-model policy above.

## Tests to add

- The context matrix described above, as a snapshot test.
- A regression test for issue 9747: an `expect` body binding a pure but
  expensive call, asserting no hoisted root is selected from it (checkable
  via the selected-roots debug output rather than by timing).
- A new-context default-suppressed unit test: introduce a synthetic derived
  context in test code (mimicking a future syntax addition) and assert that
  an otherwise-eligible expression under it is NOT selected — proving the
  default is suppression, not eligibility.
- Unit tests for each eligible position constructor asserting selection
  still fires there (guards the no-regression half of the inversion).

## Related projects

- [./cross-phase-coverage-parity-tests.md](./cross-phase-coverage-parity-tests.md)
  — the same class of fix (replacing hand-enumerated site lists with
  structural guarantees); its comptime-site suite exercises hoisting's
  interaction with exhaustiveness site identity, which PR 9722 once broke.
