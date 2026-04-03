# Remaining CoreCIR Architectural Plan

## Absolute Rules

- Do not run `zig` for any reason at any time while executing this plan.
- Do not run `zig build`, `zig test`, `zig build minici`, eval tests, repl tests, or any compiler entrypoint driven by Zig.
- Do not introduce workarounds, hacks, rescue paths, compatibility shims, temporary placeholders, transitional stopgaps, or fallback behavior.
- Do not preserve old architecture for backwards compatibility.
- If an invariant is missing during the cutover, the only acceptable temporary behavior is:
  - `std.debug.panic(...)` in debug builds
  - `unreachable` in release builds

## Current State

The following architectural cuts are already complete:

- `TemplateCatalog` owns callable-template extraction and root inventory.
- `DispatchSolved` owns normalized dispatch results.
- `ContextMono` owns structural monotype/name comparison.
- `Lambdasolved` owns `SemanticThread`, root-analysis state construction, and top-level root-analysis execution.
- `Lambdamono.Program` owns packed/indirect callable construction.
- `Lambdamono` owns executable-program assembly through a stage-owned assembler.
- `Lambdamono.Expr` is now a tagged executable union instead of a payload-overlay record.
- `Pipeline` no longer owns:
  - external-template invariant helpers
  - dispatch-target resolution
  - packed-callable construction logic
  - root-analysis re-entrancy helpers
  - top-level root-analysis state construction
  - executable-program node assembly

What remains is structural: finishing the move from a large integrated `Pipeline` shell to true stage-owned semantic scanning, and deleting the last residual builder/driver seams left after the executable AST cutover.

## Remaining Goal

Reach the long-term ideal end state:

1. `Pipeline.zig` is orchestration-only.
2. `Lambdasolved` owns solved semantic scanning and root-analysis execution, not just its state containers.
3. `Lambdamono` owns executable-program construction, not just executable data types plus helper utilities.
4. `Lambdamono.Expr` remains a strict executable AST where invalid states are unrepresentable.
5. No remaining stage reads another stage's mutable internal state directly.
6. No remaining transitional/legacy artifacts survive anywhere in the Zig compiler.

## Remaining Work

### A. Delete The Remaining Integrated Pipeline Shell

Current remaining shell in `Pipeline.zig`:

- run-time orchestration in:
  - `runRootSourceExprsInto(...)`
  - `runModuleInto(...)`
- module-def pattern-origin seeding still owned by `Pipeline`
- thin stage-driver structs still live in `Pipeline`

Required end state:

- `Pipeline` only wires together already-defined stage runners.
- Stage runners live with their owning stage modules.
- `Pass` must stop owning stage-local mutable execution state.

Concrete work:

1. Keep template-catalog execution in its stage API.
2. Keep root-analysis execution in its `Lambdasolved` stage API.
3. Keep executable-program construction in its `Lambdamono` stage API.
4. Reduce `Pipeline` to:
   - allocate `Result`
   - invoke stage runners in order
   - return the final result

Success condition:

- `Pipeline.zig` no longer contains stage-specific execution helpers.

### B. Move Remaining Semantic Scan Algorithms Out Of `Pipeline`

Current remaining semantic algorithms still in `Pipeline.zig`:

- `scanStmt(...)`
- `scanCirExprInternal(...)`
- `scanCirExprChildren(...)`
- `realizeLookupExprSemantics(...)`
- `realizeStructuredExprCallableSemantics(...)`
- `realizeDispatchExprSemantics(...)`
- related scanning/materialization helpers that are still part of the same root-analysis engine

Required end state:

- `Lambdasolved` owns solved scanning and semantic realization.
- `Pipeline` does not contain the body of the semantic scan engine.

Concrete work:

1. Move root-analysis scan routines into `Lambdasolved` runner/solver APIs.
2. Move lookup/callable/dispatch realization into `Lambdasolved`.
3. Replace `Pipeline` direct calls with stage-owned entrypoints.
4. Leave only thin orchestration in `Pipeline`.

Success condition:

- the semantic scan/realization engine lives in `Lambdasolved`, not in `Pipeline`.

### C. Tighten The Strict `Lambdamono.Expr` End State

Current remaining non-ideal representation in `Lambdamono.zig`:

- `Expr` is now a tagged union, but still uses:
  - a generic common header
  - convenience getters that decode semantic meaning
- assembly still uses a local recursion/assembled-state helper

Required end state:

- `Lambdamono.Expr` remains a strict executable AST.
- Invalid combinations stay unrepresentable.
- Any remaining generic helper surface is only structural, not semantic-overlay based.

Concrete work:

1. Keep executable variants concrete and direct.
2. Reduce any remaining getter usage that only exists because callers have not yet been updated to switch on variants directly where that is clearer.
3. Keep shared metadata structural only.

Success condition:

- no semantic payload-overlay representation remains
- invalid executable states are unrepresentable in the IR type itself

### D. Delete Transitional Builder State Once The Strict AST Exists

Current remaining transitional state:

- local `Lambdamono` assembly scratch still exists, even though it is now stage-private

Required end state:

- any remaining builder scratch is narrowly scoped to the builder implementation
- no user-visible or pipeline-visible transitional executable-state layer remains

Concrete work:

1. After strict AST emission is in place, reduce or delete the remaining assembly scratch.
2. Keep only truly local builder recursion guards if still needed.
3. Ensure the final program is the only source of truth.

Success condition:

- there is no broad transitional builder overlay remaining around executable expr state

## Mandatory Re-Audit Loop

When the remaining work appears complete:

1. Audit `src/corecir`, `src/mir`, `src/lir`, `src/types`, `src/backend`, and `src/eval`.
2. Search specifically for:
   - stage-local execution still owned by `Pipeline`
   - direct reads of another stage's mutable internal state
   - late scan-then-assemble duplication
   - fat/role-based executable expr representation
   - builder overlay state that duplicates final executable facts
3. If even one remaining trace is found:
   - loop back to the beginning of this plan
   - delete it
   - continue until the audit is clean

## Success Criteria

This plan is complete only when all of the following are true:

- `zig` has not been run at any point while carrying out this plan.
- `Pipeline.zig` is orchestration-only.
- `TemplateCatalog`, `ContextMono`, `DispatchSolved`, `Lambdasolved`, and `Lambdamono` each own their own execution and data responsibilities.
- `Lambdasolved` owns semantic scanning/root analysis rather than merely storing scan state.
- `Lambdamono` owns executable-program construction rather than merely storing output structs.
- `Lambdamono.Expr` is a strict executable AST with invalid states unrepresentable.
- no direct inspection of another stage's mutable internal state remains.
- a final artifact-by-artifact audit finds no remaining trace of the old integrated/transitional design anywhere in the Zig compiler.
