# Debug Generation Counters on Growable Stores

## Problem

The postcheck passes store their IRs in growable `std.ArrayList`-backed
stores and routinely take slices or pointers into them ("borrows") while the
same pass appends to the same store. An append that reallocates dangles the
borrow. This exact bug has recurred as independent fixes at least eight times
in May–June 2026 alone:

- **PR roc-lang/roc#9808** (issue roc-lang/roc#9801):
  `src/postcheck/monotype_lifted/spec_constr.zig` iterated a slice of
  `program.fns` while the loop body materialized specialized callables via
  `Cloner.specializedCallableRef`, which appends to `program.fns` — the first
  realloc dangled the slice. Symptom: a function index of `2863311530`
  (`0xAAAAAAAA`, Zig's freed-memory fill) and a panic. Fixed by index-based
  re-reads in `collectCallPatterns` and a debug `items.ptr` equality assert
  in `collectArgUses`; repro at `test/wasm/issue_9801_spec_constr_realloc/`.
- **PR roc-lang/roc#9727**: `Cloner.cloneExprValue` and friends in the same
  file held `exprSpan`/`typedLocalSpan`/`fieldExprSpan` slices into program
  storage while recursive cloning appended. Fixed by dupe-before-iterate at
  six sites (`buildArgs`, tag payloads, record fields, tuple items,
  `cloneLoop` params/initial values, continue values); repro at
  `test/cli/Issue9717SpecConstrSpanInvalidation.roc`. Git history shows the
  same shape fixed repeatedly in this file before (dozens of commits touch
  `allocator.dupe` in `spec_constr.zig`).
- **PR roc-lang/roc#9459**: three result-location stale-slot bugs of the form
  `list.items[i] = .{ .f = try appendToSameList(...) }` — the address
  `list.items[i]` is computed before the call, the call reallocates, the
  write lands in freed memory — in `src/postcheck/monotype/lower.zig`,
  `src/postcheck/lambda_mono/lower.zig`, and
  `src/postcheck/monotype_lifted/lift.zig`.
- **PR roc-lang/roc#9478**: equality lowering in
  `src/postcheck/monotype/lower.zig` iterated `types.spans` while recursion
  appended; fixed by copying ("Copy because recursive lowerEqualityExpr may
  reallocate types.spans, invalidating the slice" — the comment survives
  today naming `lowerDerivation`, alongside a twin for `types.fields`).

Current mitigations are all per-site convention: dupe-before-iterate,
index-based re-reads, and hand-written pointer asserts. A live
guarded-by-comment instance sits in `src/lir/arc.zig`, in the loop that
rewrites emitted procs: "`rewritePath` may append mode-specialized proc
variants via `addProcSpec`, which can reallocate `store.proc_specs` and
invalidate the `proc` pointer captured above. Re-fetch the slot before
writing the rewritten body and join points so they land in the live backing
storage rather than a freed buffer." — followed by a manual
`store.getProcSpecPtr(emit_proc)` re-fetch. Nothing enforces any of this.
The recurrence of the bug in files where it had already been fixed proves
convention does not hold.

## Background

The compiler pipeline: parse → canonicalize → type-check (produces "checked
artifacts" per module) → postcheck: Monotype IR (monomorphization/
specialization, `src/postcheck/monotype/` — `lower.zig` is ~21k lines) →
Monotype Lifted (closure lifting; also `spec_constr.zig` call-pattern
specialization for optimized builds) → Lambda Solved → Lambda Mono → LIR →
ARC → backends. `design.md` at the repo root is authoritative.

Each pass mutates flat index-addressed stores in place. The main offenders
(all `std.ArrayList` fields, verified):

- `src/postcheck/monotype/ast.zig` `ProgramBuilder` (aliased as
  `Ast.Program`): `fns`, `exprs`, `pats`, `stmts`, `locals`, `expr_ids`,
  `pat_ids`, `typed_locals`, `stmt_ids`, `field_exprs`, `specs`, and more.
- `src/postcheck/monotype/type.zig` `Store`: `spans` (plus `fields`, tag
  arrays); same shape in `src/postcheck/lambda_mono/type.zig`.
- `src/postcheck/monotype_lifted/ast.zig` `Program`: `fns`, `exprs`,
  `typed_locals`, `field_exprs`, etc. (span getters `exprSpan`,
  `typedLocalSpan`, `fieldExprSpan` return raw slices into these lists).
- `src/postcheck/lambda_mono/ast.zig` `Program`: `fns`, `exprs`, etc.
- `src/lir/LirStore.zig`: `proc_specs: std.ArrayList(LirProcSpec)` with
  `addProcSpec` (appends) and `getProcSpecPtr` (returns a pointer into it).

Recursive lowering makes append-while-borrowing natural to write: cloning an
expression clones its children, and any child may append to the very list
the parent took its span slice from.

## Evidence

See the PR list above; all symbols verified in the current tree:
`collectCallPatterns` / `collectArgUses` / `Cloner.specializedCallableRef` /
`Cloner.cloneExprValue` in `src/postcheck/monotype_lifted/spec_constr.zig`
(the `fns_base = self.program.fns.items.ptr` capture and
`std.debug.assert(self.program.fns.items.ptr == fns_base)` in
`collectArgUses`; the "Re-read `items` by index" comment in
`collectCallPatterns`); the `addProcSpec` re-fetch comment and
`getProcSpecPtr` call in `src/lir/arc.zig`; the "may reallocate
types.spans/types.fields, invalidating the slice" comments in
`src/postcheck/monotype/lower.zig`.

## Solution design

Add **debug-build generation counters** to the growable index stores that
passes mutate in place, so a stale borrow panics deterministically at the
point of use instead of corrupting memory.

Data structures:

1. `generation: u32` on each store listed in Background (one counter per
   store struct is sufficient; per-list counters are optional refinement for
   `ProgramBuilder`). Every append path that may move memory — anything that
   calls `ArrayList.append`/`appendSlice`/`ensureUnusedCapacity` growth —
   bumps it. Wrap the raw lists behind existing add-methods (`addProcSpec`,
   `addExpr`, span-add helpers) so the bump has a single home per list.
2. A borrow-token API on each store:
   `store.borrowSpan(span)` / `store.borrowPtr(id)` returns a small guard
   struct recording `{ data, generation_at_borrow, *store }`. In debug
   builds, every access through the guard (or a single `guard.assertValid()`
   at scope end, for hot paths) asserts
   `store.generation == generation_at_borrow` and panics with the store and
   call-site names otherwise.
3. Comptime gating: guard fields and checks exist only when
   `builtin.mode == .Debug` (the pattern `collectArgUses` already uses); in
   `ReleaseFast` the guard is a zero-sized wrapper around the slice/pointer
   and compiles to nothing.
4. Where a pass genuinely must append while iterating, the API forces one of
   the two already-correct idioms explicitly: `store.dupeSpan(allocator,
   span)` (owned copy, the PR #9727 fix shape) or index-based re-reads
   (`store.at(i)` per step, the PR #9808 fix shape). Raw
   `store.list.items[...]` access from pass code is removed as call sites
   migrate.

Migration order:

1. Land the counter + guard machinery in one store
   (`src/lir/LirStore.zig` `proc_specs` — smallest surface), convert the
   `arc.zig` re-fetch site to a guard, and DELETE the hand-written comment
   discipline there.
2. `monotype_lifted` `Program` + `spec_constr.zig`: replace the `fns_base`
   pointer asserts with the store guard; route `exprSpan`-style getters
   through `borrowSpan`.
3. `monotype` `ProgramBuilder` and `Type.Store` spans (the PR #9459 / #9478
   sites), then `lambda_mono`.
4. Audit sweep: list every remaining store a pass mutates and either add the
   counter or document why it cannot realloc (fixed capacity).

Stronger alternative, noted for the hottest stores: segmented arenas
(chunked storage with stable addresses) make borrows immune to appends
entirely, trading memory locality for immunity. Generation counters are the
cheap first step and remain useful even for segmented stores (they still
catch index-stability violations like shrink/reorder).

## What success looks like

- Reintroducing any of the four historical bugs (revert the fix hunk from
  PRs #9808, #9727, #9459, or #9478 locally) panics in debug tests with a
  message naming the store, instead of corrupting memory or producing
  `0xAAAAAAAA` indices.
- The `fns_base` pointer asserts in `spec_constr.zig` and the comment-plus-
  re-fetch discipline in `arc.zig` are replaced by store guards (the ad-hoc
  versions are DELETED).
- Every store in the Background list has a generation counter; the audit
  list of exceptions is empty or justified in code comments.

## How to evaluate the result

### Correctness ideal

Every store a pass mutates carries the counter, and every cross-append
borrow in postcheck/LIR code flows through the guard API — verified by the
audit list in this doc's implementation PR and by grepping for raw
`.items` access into guarded lists from pass code. A CI debug-mode run of
the existing test corpus passes with guards active: the counters catch
violations passively, so the whole corpus becomes a borrow-safety test.

### Performance ideal

Zero `ReleaseFast` overhead: the guard type is zero-sized and the counter
bump is behind `comptime` gating — verify via an IR/assembly spot-check of
one hot function (e.g. `collectCallPatterns`) or a comptime proof that the
release-mode guard struct has size 0 and no runtime methods. Debug overhead
is one `u32` compare per guarded access (or per scope with
`assertValid`) — acceptable; confirm debug-suite wall time regresses by less
than a few percent.

## Tests to add

- Unit tests that intentionally violate a borrow and expect the debug panic,
  one per store type: `LirStore.proc_specs` (borrow a proc ptr, call
  `addProcSpec`, access), `monotype_lifted` `Program.fns`/`exprs` span
  borrows, `monotype` `ProgramBuilder.exprs` and `Type.Store.spans`,
  `lambda_mono` equivalents. Use `std.testing` expected-panic harnesses in
  debug builds; skip in release.
- Keep the existing regression repros green:
  `test/wasm/issue_9801_spec_constr_realloc/`,
  `test/cli/Issue9717SpecConstrSpanInvalidation.roc`, and the PR #9478
  nested-list-pattern tests.
- A CI debug-mode run of the full existing test corpus with counters active.

## Related projects

- [A Shared Cycle-Guarded Checked-Type Traversal](../small/shared-checked-type-traversal.md)
  — the sibling "make the invariant structural" project for traversals.
- [Immutable Specialization Identity](../big/immutable-specialization-identity.md)
  — reduces how often specialization code appends mid-iteration in the first
  place.
