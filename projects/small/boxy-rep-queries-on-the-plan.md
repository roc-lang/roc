# Boxy Lowering Consumes the Plan Instead of Re-Querying It

## Problem

The `.boxy` strategy is planner-plus-lowerer: `boxy/plan.zig` (14,158
lines) analyzes checked types into an explicit representation plan, and
`boxy/lower.zig` (47,107 lines) lowers checked CIR under that plan.
design.md states the contract—"In `.boxy`, checked CIR is consumed
directly with explicit boxy representation plans owned by that
lowerer", and "Boxy planning records a generated-codec worker from the
checked structural..."—the planner decides, the lowerer consumes.

In practice the lowerer re-runs the planner's analysis. The two files
share ~55 function names, of which roughly 45 are representation
queries with near-identical bodies, ~390 lines:

- `repSubtreeHasDescriptor` / `...InOtherChildren` / `...Inner`
  (`lower.zig:3993, 3999` and `:35999, 36031`; `plan.zig:9011, 9017`)
- `repSubtreeHasDictionary` / `...InOtherChildren` / `...Inner`
  (`lower.zig:36046, 36052`; `plan.zig:9034, 9040`)
- `repSubtreeContainsRep` / `...Inner` (`lower.zig:36005, 36015`;
  `plan.zig:9395, 9401`)
- `findMatchingChildByRole` (`lower.zig:4014, 36067`;
  `plan.zig:9110`)—three copies
- `findMatchingChildBySourceType`,
  `findMatchingDictionaryChildBySourceType`
- `findMatchingTagPayloadInRep` / `...InRowExtension` / `...Inner`
  (`lower.zig:4040, 4051, 4064` and `:36138`; `plan.zig:9206, 9218,
  9231`)
- `descriptorArgumentIdentityRep` (`lower.zig:4159, 36218`;
  `plan.zig:9294`), `dictionaryArgumentIdentityRep`,
  `structuralWrapperBackingRep`
- `workerSourceForMethodTarget`, `workerSourceForConstFnValue`,
  `workerSourceForCallableRootExpr`,
  `workerSourceForCallableEvalTemplate`,
  `workerSourceForProcedureUse`
- `topLevelProcedureBindingForExpr` (`lower.zig:311`;
  `plan.zig:11443`), `importedProcedureBinding`,
  `nestedCallableSiteExprForExpr`, `checkedBinderType`,
  `checkedLambdaExprForNestedFn`, `methodOwnerFor*`,
  `recordFieldNameMatches`, `tagLabelNameMatches`,
  `requiredSingleChild`, `lookupMethodTarget`

A further set is shared with `boxy/layouts.zig`: `functionChildren`
(`plan.zig:9064` / `layouts.zig:329`), `functionIdentityRep`
(`plan.zig:9089` / `layouts.zig:354`), `requiredSingleChild`,
`builtinNominal`, `sameChildRole`.

The sharpest instance is `childRolesMatch`, which exists three times
with **two different exhaustiveness postures**:

- `plan.zig:9254`—`if (target.role == .record_field) { ... }`,
  `if (target.role == .tag_payload) { ... }`, then
  `return std.meta.eql(target.role, candidate.role);`
- `lower.zig:4084` (`ProcedureBuilder`)—same `if`-chain plus
  `std.meta.eql` fallback
- `lower.zig:36158` (`ProcBodyBuilder`)—an exhaustive `switch` over
  every `ChildRole`, with all fourteen non-matching roles listed by
  name in each arm

Adding a `ChildRole` that needs name-based or index-based matching
fails to compile in the third copy and silently falls into
`std.meta.eql` in the other two. That is a wrong-answer-by-default,
not a missing-arm error.

## Background

`plan.zig` already exposes the right shape in places:
`ProgramPlan.inspectMethodForRep` (`plan.zig:1106`) is a `pub` method
on the plan that the lowerer calls. The queries above are simply the
ones that never got promoted—each was written as a `Builder` method
during planning, then needed during lowering, and copied.

`boxy/lower.zig`'s own internal split makes it worse: `ProcedureBuilder`
(L994–11713) and `ProcBodyBuilder` (L11714–36862) are two scopes of the
same lowering pass, so several queries exist twice *inside* `lower.zig`
before you count `plan.zig`'s copy.

## Evidence

- The paired declarations above; a side-by-side diff of any
  `repSubtree*` pair shows only receiver plumbing differences
  (`self.plan.representations` vs `self.parent.plan.representations`,
  `self.moduleForId(...)` vs `procedureModuleById(self.modules, ...)`).
- `plan.zig:9254` vs `lower.zig:4084` vs `lower.zig:36158`—the
  three-way `childRolesMatch` with divergent exhaustiveness.
- `plan.zig:1106`—the pattern already in use, for one query.
- `grep -c 'fn ' src/postcheck/boxy/lower.zig`—1,077 functions in one
  file, which is why "just call the other one" was never the easy
  edit.

## Solution design

1. **Promote the queries to `ProgramPlan` methods.** Every
   `rep*`/`findMatching*`/`*IdentityRep`/`requiredSingleChild`/
   `sameChildRole`/`childRolesMatch` query becomes `pub fn` on
   `Plan.ProgramPlan`, taking only plan data. They are pure functions
   of the plan; none needs the planner's mutable builder state or the
   lowerer's emit state.
2. **Delete all copies** in `plan.zig`'s `Builder`, in
   `lower.zig`'s `ProcedureBuilder` and `ProcBodyBuilder`, and in
   `layouts.zig`. Each becomes a call.
3. **Make `childRolesMatch` exhaustive**—the surviving version is
   `ProcBodyBuilder`'s switch. Sweep the same way for every other
   `ChildRole` dispatch reached by the moved code; no
   `std.meta.eql` fallback and no `else` may survive in a role
   dispatch.
4. **Resolve the name-matching signature split.** `tagLabelNameMatches`
   takes a `checked.ModuleId` in one copy and a module view in
   another; `recordFieldNameMatches` likewise. Pick one and thread it,
   rather than keeping an adapter on each side.
5. **The `workerSourceFor*` and `*BindingForExpr` families need a
   judgment call**, not a mechanical move: these read checked CIR, not
   just the plan. Either they become plan methods taking the checked
   module as a parameter, or—better where it applies—the planner
   *records* the answer in the plan and the lowerer looks it up. Prefer
   recording: that is the design.md contract, and it removes the query
   rather than sharing it. Decide per family and state the decision in
   the code.
6. **Pin it.** A source-text test asserting a single definition per
   promoted query name across `src/postcheck/boxy/`.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -rn 'fn repSubtree\|fn findMatching\|fn childRolesMatch\|fn
  requiredSingleChild\|fn sameChildRole\|fn .*IdentityRep' src/postcheck/boxy/`
  shows one definition per name, all on `ProgramPlan`.
- No `ChildRole` dispatch under `src/postcheck/boxy/` uses
  `std.meta.eql` as a fallback or an `else` arm; adding a role is a
  compile error at every site that must handle it.
- Each `workerSourceFor*` / `*BindingForExpr` family is either a
  single plan method or replaced by a recorded plan field, with the
  choice stated in a code comment.
- The single-definition source-text test is in-tree and green.
- `.boxy` snapshot and CLI output are unchanged; the boxy cases in
  `src/eval/test/parallel_cli_runner.zig` pass.

## How to evaluate the result

### Correctness ideal

The plan is the single answer to "what does this representation
contain", so planning and lowering cannot reach different conclusions
about descriptors, dictionaries, tag payloads, or child roles. A new
`ChildRole` is a compile error at every site that must handle it,
instead of a silent `std.meta.eql` false in two of three.

### Performance ideal

Neutral to positive. The queries are pure plan walks, so moving them
changes no algorithmic cost; several are recursive subtree scans
(`repSubtreeHasDescriptorInner`, `repSubtreeContainsRepInner`) that
today run once per copy per call site. Where step 5 converts a query
into a recorded plan field, that is a strict win—the walk happens once
during planning instead of per lowering site. Measure boxy
lowering time on a corpus before and after; a regression means a
promoted query lost an inlining opportunity, and `inline fn` is the
fix—applied on evidence, not preemptively.

## Tests to add

- Single-definition source-text lint for the promoted query names.
- A comptime exhaustiveness assertion over `ChildRole`, so a new role
  fails the build until every dispatch handles it.
- Plan/lower agreement pins: for a corpus of representations, assert
  the plan's recorded descriptor and dictionary requirements match
  what lowering materializes—so step 5's recorded facts are checked,
  not trusted.

## Related projects

- [../big/postcheck-lowerer-decomposition.md](../big/postcheck-lowerer-decomposition.md)—
  the `ProcedureBuilder`/`ProcBodyBuilder` split that creates the
  second copy inside `lower.zig`; this project supplies the
  destination for its step 5.
- [../big/one-value-semantics-layer.md](../big/one-value-semantics-layer.md)—
  its `Plan.TypeRepresentation` shape context wants these queries to
  already be plan methods.
- [hoist-consumes-dispatch-evidence.md](hoist-consumes-dispatch-evidence.md)—
  the same record-instead-of-re-derive cure, one stage earlier.
