# Decompose the Post-Check Lowerer God-Structs

## Problem

Two files in the post-check pipeline are dominated by single enormous
structs, and each of those files contains a *second* struct that
duplicates a large slice of the first because there is no free-function
layer for a helper to live in.

`src/postcheck/monotype/lower.zig` is 52,019 lines. Two of its
top-level declarations account for 44,174 of them:

- `const Builder`: L2124–9652 (7,528 lines, 238 methods)
- `const BodyContext`: L13096–49742 (**36,646 lines**, 1,154 methods)

They share 53 method names. Thirty-six of those pairs have ≥0.70
normalized-body similarity, ~470 lines, in two coherent families:

- **Inspect derivation**: `inspectBody` (`:8769` / `:14738`),
  `toInspectCall` (`:8808` / `:14777`), `inspectTuple`
  (`:8859` / `:14869`), `inspectRecord` (`:8876` / `:14886`),
  `inspectFieldSlot` (`:8903` / `:14908`), `inspectTagUnion`
  (`:8998` / `:14940`), `inspectList` (`:9070` / `:15012`),
  `inspectListStep`, `inspectCall`, `inspectDefForType`,
  `inspectTagBody`, `uninhabitedInspect`.
- **Const restoration**: `restoreConstData` (`:8547` / `:29646`,
  0.99 similar), `restoreConstRecord` (0.98), `restoreConstList`
  (0.98), `restoreConstListData` (0.99), `restoreConstTuple` (0.97),
  `restoreConstTagPayloads` (0.97).

Plus scattered singles: `bindPatLocals` (`:7994` / `:15974`, 0.96),
`stmtDependsOnFreeLocal`, `lowerCallableEvalBindingValue`,
`recordFieldByTextOptional`, `ifExpr`, `lowLevelExpr`, `stringExpr`,
`intLiteralExpr`, `constBoxPayloadType`, `constListElemType`,
`typeHasBuiltinOwner`, `bindTypedLocalLocals`, `removeBoundLocals`.

Most of these pairs differ only in receiver plumbing—`self.program.addExpr`
versus `self.addExpr`, `self.symbols` versus `self.builder.symbols`. That
is precisely the shape that stays byte-identical until one day it
doesn't: `toInspectCall` has already diverged in capability (see
[one-value-semantics-layer.md](one-value-semantics-layer.md)).

The same structure repeats in `src/postcheck/boxy/lower.zig`
(47,107 lines):

- `const ProcedureBuilder`: L994–11713 (10,718 lines, 245 methods)
- `const ProcBodyBuilder`: L11714–36863 (25,148 lines, 747 methods)

with 17 shared method names, and again the same pattern of one copy
being modernized while the other is not: `childRolesMatch` is a
`std.meta.eql`-fallback `if`-chain in `ProcedureBuilder` (`:4084`)
and an exhaustive switch in `ProcBodyBuilder` (`:36158`).
`findMatchingTagPayloadInRep` (`:4064` / `:36138`),
`repSubtreeHasDescriptorInner` (`:3999` / `:36031`),
`descriptorArgumentIdentityRep` (`:4159` / `:36218`),
`findMatchingChildBySourceType` (`:4025` / `:36078`),
`structuralWrapperBackingRep` (`:4144` / `:35467` neighborhood),
`recordFieldNameMatches`, `tagLabelNameMatches` complete the set.

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

7. **Assess the codec pairs before touching them.** The
   parser/encoder quadruple and the parse-list/parse-dict pair are
   measured-similar but not verified to be semantically parallel.
   Step one is confirming they are the same algorithm over different
   directions/containers; only then extract a shared skeleton with a
   direction/container policy parameter. If they turn out to be
   genuinely different algorithms that merely rhyme, record that
   finding and leave them alone—do not force a shared shape onto
   them.

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
