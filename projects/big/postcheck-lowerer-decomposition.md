# Decompose the Post-Check Lowerer God-Structs

## Problem

Two files in the post-check pipeline are dominated by single enormous
structs, and each of those files contains a *second* struct that
duplicates a large slice of the first because there is no free-function
layer for a helper to live in.

`src/postcheck/monotype/lower.zig` is dominated by two structs:

- `const Builder`: ~7,100 lines, 220 methods
- `const BodyContext`: ~36,200 lines, 1,139 methods

They share 38 method names. Normalizing away receiver plumbing
(`self.program.` versus `self.builder.` versus `self.`), 26 of those
pairs are ≥0.85 similar, about 254 lines. The coherent cluster is const
restoration: `restoreConstData` (identical), `restoreConstListData`
(identical), `restoreConstRecord`, `restoreConstTuple`,
`restoreConstList`, `restoreConstTagPayloads` (all at or above 0.98).
Both copies are live: `Builder`'s reaches production through
`restoreConstNodeAtTypeWithStaticRoot`, which `lowerStaticDataRequest`
and the codec paths call, so this cluster needs extraction rather than
deletion. Smaller live pairs: `bindPatLocals` (0.98),
`stmtDependsOnFreeLocal` (0.85), `lowerCallableEvalBindingValue` (0.88),
`recordFieldByTextOptional` (0.89), `bindTypedLocalLocals`,
`constBoxPayloadType`, `constListElemType` (identical).

These numbers are smaller than this project's first draft because two
clusters turned out to be dead rather than duplicated, and were deleted:
`Builder`'s whole inspect family (13 functions, and `toInspectCall` with
it), then the expression helpers that had served only it (`stringExpr`,
`concatExpr`, `lowLevelExpr`, `intLiteralExpr`, `ifExpr`). **Check
reachability before filing a pair as duplicated.** Two of this doc's
original findings did not survive that check.

One thing that made those clusters hard to see is worth stating,
because it will hide the next one too: the repo's dead-code lint is
name-based, so a dead `Builder.stringExpr` looks used as long as a live
`BodyContext.stringExpr` is called somewhere in the file. Duplicate
method names across containers defeat the lint that would otherwise
find the duplication. The compiler is the authority here: deleting a
candidate and building is the check that works, and it is how
`removeBoundLocals` was caught still being called (as a bare identifier,
which a `self.name(` search misses).

The same structure repeats in `src/postcheck/boxy/lower.zig`:

- `const ProcedureBuilder`: ~10,700 lines
- `const ProcBodyBuilder`: ~25,100 lines

Its 17 shared method names were representation queries, and those have
since moved onto the plan as `Plan.RepQuery` / `Plan.NamedRepQuery` with
a lint holding them there. The `childRolesMatch` divergence this doc
originally cited (two `std.meta.eql`-fallback `if` chains against one
exhaustive switch) was resolved by that move, which kept the exhaustive
version. What remains in `lower.zig` is emit-side state, which is the
part the scope split actually justifies.

Beyond the cross-struct doubles, `monotype/lower.zig` carries 109
function names defined more than once in the file, and ~800 lines of
pairwise-cloned codec paths:
`restoreConstParserRuntimeFnAtNode` (`:30653`, 176 lines) against
`restoreConstEncoderForRuntimeFnAtNode` (`:30987`, 203 lines) at 0.88;
`restoreConstParserRuntimeFn` (`:30521`) against
`restoreConstEncoderForRuntimeFn` (`:30838`) at 0.84;
`restoreConstParserRuntimeFnExpr` (`:8213`) against
`restoreConstEncoderForRuntimeFnExpr` (`:8339`) at 0.83; and
`lowerParseListFromState` (`:24394`) against `lowerParseDictFromState`
(`:24556`) at 0.86.

## Background

The structs are large for a defensible reason: Monotype lowering
carries a lot of scoped state (the active graph, the body draft store,
binder maps, evidence chains, guard frames, deferred const uses), and
methods are how that state gets threaded. `BodyContext` and `Builder`
are two *scopes* of that state—`BodyContext` holds a
`builder: *Builder` back-pointer—not two implementations of one thing.
Same for `ProcBodyBuilder`, which holds `parent: *ProcedureBuilder`.

That is exactly why the duplication happened: a helper written against
`Builder`'s field set cannot be called from `BodyContext` without
either threading the back-pointer or copying. Copying was the cheaper
edit each time, 53 times.

The fix is not to merge the structs—the scope split is real. It is to
introduce the missing layer: helpers that take what they need as
parameters (or via a small comptime-duck-typed emitter) instead of
reaching into a specific receiver's fields. The file already does this
correctly in places: `bindLocalName` (`:50120`), `moduleView`
(`:50136`), `restoreScalar` (`:50173`), `builtinOwnerFromPrimitive`
(`:50516`) are free functions, and `EqDeriver` (`:49742`) /
`HashDeriver` (`:49965`) are policy structs over a generic walker.
The pattern exists; it is just not where the bulk is.

## Evidence

- Container extents above, reproducible by listing top-level
  `const X = struct {` declarations and their brace extents.
- 53 shared method names between `Builder` and `BodyContext`; 36 at
  ≥0.70 normalized-body similarity.
- `monotype/lower.zig:8859` vs `:14869`—`inspectTuple`, identical but
  for `self.program.addExpr` vs `self.addExpr`.
- `monotype/lower.zig:8547` vs `:29646`—`restoreConstData` at 0.99.
- `boxy/lower.zig:4084` vs `:36158`—same predicate, one exhaustive,
  one with a `std.meta.eql` fallback.
- 109 duplicate function-name definitions inside
  `monotype/lower.zig`.

## Solution design

This is a mechanical-but-large refactor with one design decision. Do
it family by family; each family is independently shippable and
independently verifiable by unchanged snapshot output.

1. **Decide the sharing mechanism.** Two options, and the choice
   should be made once for the whole file rather than per family:
   *(a)* free functions taking the pieces they need
   (`allocator`, `*ProgramStore`, `*CanonicalNameStore`, the emit
   target) explicitly, or *(b)* a comptime-duck-typed `Emitter`
   parameter, where `Builder` and `BodyContext` both already satisfy
   `addExpr` / `addPat` / `addExprSpan` / `stringExpr` / `concatExpr`
   / `addLocal`. Option (b) is a smaller diff and matches
   `match_tree.Compiler(Ctx)` and `EqDeriver`; option (a) is blunter
   but leaves nothing implicit. Recommendation: (b), with the required
   method set written down as a doc comment on the parameter, because
   the two receivers already differ only in how they reach the store.

2. **Extract the inspect family** out of both `Builder` and
   `BodyContext`. This is the highest-value family (it is also a live
   behavioral divergence) and the natural first cut. If
   [one-value-semantics-layer.md](one-value-semantics-layer.md) lands
   first, this step is already done and the inspect helpers move to
   `src/postcheck/semantics/` instead of staying local.

3. **Extract the const-restoration family** the same way. These six
   are near-byte-identical and carry no divergence to reconcile;
   they are the cheapest confidence-builder if the inspect family
   looks too entangled to start with.

4. **Extract the singles**: `bindPatLocals`, `stringExpr`,
   `intLiteralExpr`, `ifExpr`, `lowLevelExpr`, `constBoxPayloadType`,
   `constListElemType`, `typeHasBuiltinOwner`, `bindTypedLocalLocals`,
   `removeBoundLocals`, `recordFieldByTextOptional`. Each is small
   enough that "move it and delete the copy" is one commit.

5. **`boxy/lower.zig`'s shared helpers are already handled.** The 17
   `ProcedureBuilder`/`ProcBodyBuilder` doubles were representation
   queries, and they now live on the plan as `Plan.RepQuery` /
   `Plan.NamedRepQuery` with a lint holding them there. What remains in
   `lower.zig` for this project is the emit-side state, which is the
   part the scope split actually justifies.

6. **Reconcile every divergence deliberately as it is merged.** For
   each pair, the merged version is the union of capabilities unless
   there is a stated reason one scope must be weaker—and if there is,
   it is a comment at the call site, not a silently different body.
   `childRolesMatch` merges to the exhaustive switch.
   `toInspectCall` merges to the richer `BodyContext` behavior.

7. **The codec pairs are assessed; most should not merge.** This step
   asked for confirmation before extraction, and the answer is mostly no.

   - `restoreConstFnTemplateAtNode` and `restoreConstFnTemplate` were one
     function twice, differing only in whether `fn_def`, `source_fn_ty`,
     and `source_fn_key` arrived loose or bundled in a `FnTemplate`.
     Merged as `restoreConstFnTemplateAt`, with all three passed
     explicitly, because the two call sites read `source_fn_ty` from
     *different records* and folding that choice into the callee would
     have been wrong.
   - `restoreConstUseAtType` and `restoreConstUseAtNode` are not
     duplicates despite 0.86 similarity. One works in sealed types
     (`sameType`, `constrainTypeToMono`, `runtimeCrashExpr`), the other
     in the instantiation graph (`graph.unify`,
     `relateRequestComponent`, `runtimeCrashExprAtCell`), and they call
     different downstream families throughout. Two solver phases wearing
     the same shape. Leave them.
   - `restoreConstParserRuntimeFnExpr` and
     `restoreConstEncoderForRuntimeFnExpr` call an identical set of
     eight helpers with none unique to either side, which is the
     strongest available evidence that they are one algorithm over two
     codec roles. This is the real extraction candidate in this cluster
     and it is still open.
   - `lowerParseListFromState` and `lowerParseDictFromState` share 21 of
     23 callees, differing only in the loop body and the container
     constructor. A merge wants a container policy parameter, not a
     direction one. Also still open.
   - The `restoreConstParserRuntimeFn` / `...AtNode` pairs call no
     helpers at all, so callee overlap says nothing about them; they
     need reading before any judgement.

   The general lesson, which cost this batch four wrong findings: raw
   text similarity ranks these pairs almost identically, and it is
   wrong about which ones are duplicates. Callee-set overlap separates
   them far better, and reading separates them properly.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- No function name is defined twice inside `monotype/lower.zig` or
  `boxy/lower.zig` with ≥0.70-similar bodies. A source-text test in
  `src/postcheck/structural_test.zig` enforces this going forward
  (the file already embeds and greps its own sources for exactly this
  kind of invariant).
- The inspect family and the const-restoration family each have one
  definition, callable from both scopes.
- Every `ChildRole`, `Type.Content`, and pattern-kind switch reached
  by the moved code is exhaustive; no `std.meta.eql` or `else`
  fallback survives in a role/variant dispatch.
- `BodyContext` is materially smaller, and the reduction is in shared
  helpers moved out, not in code deleted as dead—`zig build` and the
  full snapshot suite confirm nothing was dropped.
- Each divergence reconciled in step 6 has either a test pinning the
  merged behavior or a call-site comment stating why one scope is
  deliberately weaker.
- `git diff test/snapshots` is empty and `roc check` / `roc run`
  output on a fixture set is byte-identical before and after.
- The codec-pair question from step 7 is answered in writing—either
  extracted, or documented as not-actually-parallel.

## How to evaluate the result

### Correctness ideal

A change to how an inspect renders, a const is restored, or a pattern
binds is one edit, and the compiler—not a reviewer's memory—finds
every consumer. The specific failure mode already observed
(`toInspectCall` and `childRolesMatch` diverging silently between two
scopes of the same file) becomes structurally impossible, and the
structural test prevents its recurrence.

### Performance ideal

Strictly neutral, and this must be checked rather than assumed:
comptime-duck-typed helpers monomorphize per receiver, so the emitted
code should be equivalent. Watch for two specific regressions—an
extracted helper that now takes a slice where it previously indexed a
field in place, and one that allocates a temporary that the inlined
version did not. Compare post-check phase timings via
`checked_pipeline.zig`'s `Timing` counters over a fixed corpus before
and after each family extraction, not only at the end. Compile-time
of `monotype/lower.zig` itself should improve or hold; a 52k-line
file with a 36k-line struct is also a build-throughput cost.

## Tests to add

- A `structural_test.zig` lint: no two functions in
  `src/postcheck/monotype/lower.zig` or `src/postcheck/boxy/lower.zig`
  share a name with near-identical bodies, with an explicit shrinking
  allowlist if the first pass cannot reach zero.
- Behavior pins for each reconciled divergence (step 6), reachable
  from both scopes—for `toInspectCall`, an inspect derived from the
  `Builder` scope and one from the `BodyContext` scope must render
  identically for the same type.
- A comptime-enumerated exhaustiveness assertion for `ChildRole`
  handling, so a new role fails to compile rather than falling into a
  fallback.

## Related projects

- [one-value-semantics-layer.md](one-value-semantics-layer.md)—
  removes the inspect family from both scopes by moving it out of the
  file entirely; land whichever is convenient first, they compose.
- [unreachable-rationale-comments.md](unreachable-rationale-comments.md)—
  same "CI lint with a shrinking allowlist" enforcement shape.
