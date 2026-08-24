# Generate the Post-Check IR Store Boilerplate Once

## Problem

The post-check pipeline has four flat IR stores, and their storage
boilerplate is copy-pasted between them.

- `src/postcheck/monotype/ast.zig` (2,464 lines, 173 functions)
- `src/postcheck/monotype_lifted/ast.zig` (1,369 lines, 141 functions)
- `src/postcheck/lambda_mono/ast.zig` (907 lines, 64 functions)
- `src/postcheck/lambda_solved/ast.zig` (110 lines—this one is thin
  and mostly a re-export)

Monotype and Monotype Lifted share **69 function names, 57 of which
have byte-identical bodies**. Lifted and Lambda Mono share 51 names
with 44 byte-identical bodies. The identical set is the whole flat-store
surface: `addExpr`, `addPat`, `addStmt`, `addLocal`,
`addLocalWithBinder`, `addFn`, `addRoot`, `addStringLiteral`,
`addLayoutRequest`, `addRuntimeSchemaRequest`, `addComptimeSite`, the
eleven `add*Span` functions, the matching eleven `*Span` accessors,
`getExpr`/`getPat`/`getStmt`/`getFn`/`getLocal` and their `*At`
variants, the five `*Count` functions, the five `*View` functions,
`exprLoc`/`exprRegion`/`stmtLoc`/`stmtRegion`, `procDebugName`,
`setProcDebugName`, `localName`, `captureIdOfLocal`, `deinit`, `init`,
`view`, and `Span`/`ProgramList`/`ProgramSpanBorrow`.

A fifth copy of one of them lives outside `postcheck`:
`addComptimeSite` at `src/lir/program.zig:415` is a 0.94 match for the
`monotype/ast.zig:1752` and `lambda_mono/ast.zig:670` versions.

Everything that is *actually* per-IR—the `ExprData`, `PatData`, and
`StmtData` unions, the type ids, the stage-specific invariants—is a
minority of each file. Each new field on a stored node, each new span
kind, each fix to a bounds check or a `deinit` free-list means the same
edit in three or four files, with only reviewer attention as the check.

## Background

The four IRs are deliberately separate types: `structural_test.zig`
enforces that stage expression forms "only shrink checked syntax or
add runtime encoding forms" (`structural_test.zig:240`), and that
Monotype Lifted "owns captures and consumes Monotype expression
storage" (`:155`). That separation is the point and must survive—this
project does not merge the IRs.

It merges only their *storage mechanics*, which are genuinely identical:
append to an `ArrayList`, hand back a dense id, store spans as
`(start, len)` into a flat side array, guard reads through
`GuardedList`. Nothing about that is stage-specific.

The repo already accepts comptime-driven store generation as a
technique: `small/nodestore-serde-enrollment.md` proposes exactly this
for the canonicalizer's `NodeStore` field lists, and
`src/builtins/builtin_registry.zig` (a landed project) derives seven
hand-typed tables from one comptime registry.

## Evidence

- Normalized-body comparison of `monotype/ast.zig` against
  `monotype_lifted/ast.zig`: 69 shared names, 57 with identical
  bodies.
- Same against `lambda_mono/ast.zig`: 51 shared, 44 identical.
- `monotype/ast.zig:1752` vs `lambda_mono/ast.zig:670` vs
  `lir/program.zig:415`—three `addComptimeSite`.
- `monotype_lifted/ast.zig:1037` vs `monotype/ast.zig:1809`—
  `addLocalWithBinder`, 0.98.
- The `deinit` triple (`monotype/ast.zig:1417`,
  `monotype_lifted/ast.zig:603`, `lambda_mono/ast.zig:563`) at
  0.92–0.94: the same free sequence, one list longer or shorter.
- The tax being paid live: PR #10834 (in review as this was written)
  widens `source_files` from `[]const u8` to `base.SourceFileEntry`.
  Inside `monotype/ast.zig` alone that is the same field edit in
  `Program`, `ProgramView`, and `ProgramBuilder`, plus the same
  two-field `deinit` free loop written out twice—one change to one
  stored table, five hand-synchronized edits.

## Solution design

1. **Write `FlatStore(comptime Spec: type)`** in
   `src/postcheck/common.zig` or a new
   `src/postcheck/flat_store.zig`. `Spec` declares the node tables
   (`Expr`, `Pat`, `Stmt`, `Local`, `Fn`, ...) and the span tables
   (`ExprSpan`, `PatSpan`, `BranchSpan`, ...), each with its id enum.
   The mixin generates `add*`, `get*`, `get*At`, `set*`, `*Count`,
   `*View`, `*Span`, `add*Span`, `init`, `deinit`, and `view` from
   that declaration.
2. **Each `ast.zig` keeps only what is its own**: the data unions, the
   loc/region side tables where they differ, the stage-specific
   accessors (`captureIdOfLocal`, `procDebugName`), and the invariants
   `structural_test.zig` pins. Everything generated is deleted.
3. **Fold `lir/program.zig`'s `addComptimeSite` in** if `Program` can
   reasonably use the same mixin; if it cannot (it is an LIR store,
   not a post-check AST store), leave it and note why—do not stretch
   the abstraction across the LIR boundary just to absorb one
   function.
4. **Keep the guarded-read discipline.** The generated accessors must
   go through `GuardedList` exactly as the hand-written ones do; the
   existing `src/collections/guarded_list_violation_test.zig` cases
   (`lambda_mono_expr_ids`, `lambda_mono_type_spans`) must keep
   failing on violation after generation.
5. **Preserve node allocation order exactly.** Snapshot output
   contains type-variable and node indices, so any change to the order
   or count of appends shows up as a snapshot diff. That makes
   `git diff test/snapshots` the primary verification for this
   refactor—if it is empty, the mechanics are equivalent.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- No function body is duplicated across the four `ast.zig` files;
  the generated surface exists once.
- Each `ast.zig` contains only its data unions, its stage-specific
  accessors, and its `Spec` declaration.
- The `guarded_list_violation_test.zig` cases still detect violations
  through the generated accessors.
- Every `structural_test.zig` invariant about stage IR shape still
  holds (these embed and grep the sources, so some will need their
  search strings updated—updating them is fine; deleting one is not).
- `git diff test/snapshots` is empty, confirming node allocation order
  and count are unchanged.
- Adding a span kind to one IR is a one-line `Spec` edit, not a
  four-function addition.

## How to evaluate the result

### Correctness ideal

A bounds check, a `deinit` free, or a guarded-read fix lands in one
place for every IR at once. The four stores cannot drift into
subtly different bounds or ownership behavior, which is a class of bug
that would surface as a use-after-free or a silent wrong-node read
rather than a clean failure.

### Performance ideal

Neutral, and worth verifying rather than assuming: comptime generation
monomorphizes per `Spec`, so the emitted accessors should be identical
to today's. Two things to check explicitly—that the generated `add*`
functions keep the same `ArrayList` growth behavior (a different
`ensureTotalCapacity` pattern changes allocation counts on large
programs), and that Zig's compile time for the four files does not
regress from heavy comptime reflection. Measure both: allocation
counts on a large corpus program, and `zig build` wall time.

## Tests to add

- A comptime assertion per `Spec` that every declared table has a
  matching id enum and that ids are dense from zero (the
  "Dense IDs and structural keys" rule in design.md).
- Round-trip tests for each store: append N nodes of each kind, read
  them all back, assert ids and contents match.
- Keep the existing guarded-list violation cases; add one per store so
  all four are covered rather than only `lambda_mono`.

## Related projects

- [nodestore-serde-enrollment.md](nodestore-serde-enrollment.md)—the
  same comptime-drive-the-field-lists cure for the canonicalizer's
  `NodeStore`; land either first, they share technique.
- [spec-constr-single-cloner.md](spec-constr-single-cloner.md)—the
  other Monotype Lifted duplication, at the traversal layer rather
  than the storage layer.
- [../big/postcheck-lowerer-decomposition.md](../big/postcheck-lowerer-decomposition.md)—
  independent; both reduce the post-check line count, neither blocks
  the other.
