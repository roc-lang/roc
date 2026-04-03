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
- `Lambdasolved` owns `SemanticThread` and the root-analysis control-flow helpers.
- `Lambdamono.Program` owns packed/indirect callable construction.
- `Pipeline` no longer owns:
  - external-template invariant helpers
  - dispatch-target resolution
  - packed-callable construction logic
  - root-analysis re-entrancy helpers

What remains is structural: finishing the move from a large integrated `Pipeline` shell to true stage-owned scanning/assembly, and replacing the last flexible `Lambdamono.Expr` representation with the strict executable AST end state.

## Remaining Goal

Reach the long-term ideal end state:

1. `Pipeline.zig` is orchestration-only.
2. `Lambdasolved` owns solved semantic scanning and root-analysis execution, not just its state containers.
3. `Lambdamono` owns executable-program construction, not just executable data types plus helper utilities.
4. `Lambdamono.Expr` is a strict executable AST where invalid states are unrepresentable.
5. No remaining stage reads another stage's mutable internal state directly.
6. No remaining transitional/legacy artifacts survive anywhere in the Zig compiler.

## Remaining Work

### A. Delete The Remaining Integrated Pipeline Shell

Current remaining shell in `Pipeline.zig`:

- run-time orchestration in:
  - `runRootSourceExprsInto(...)`
  - `runModuleInto(...)`
- module-def pattern-origin seeding still owned by `Pipeline`

Required end state:

- `Pipeline` only wires together already-defined stage runners.
- Stage runners live with their owning stage modules.
- `Pass` must stop owning stage-local mutable execution state.

Concrete work:

1. Move template-catalog execution into a `TemplateCatalog` runner API.
2. Move root-analysis execution into a `Lambdasolved` runner API.
3. Move executable-program construction into a `Lambdamono` builder/runner API.
4. Reduce `Pipeline` to:
   - allocate `Result`
   - invoke stage runners in order
   - return the final result

Success condition:

- `Pipeline.zig` no longer contains stage-specific execution helpers.

### B. Move Executable Program Construction Out Of `Pipeline`

Current remaining assembly ownership in `Pipeline.zig`:

- `assembleRootProgramExprs(...)`
- `assembleCallableDefExprGraph(...)`
- `assembleProgramExprNode(...)`
- `assembleProgramExprNodeInternal(...)`
- `ensureProgramExprSemanticShape(...)`
- `assembleProgramStmtNode(...)`
- `ensureProgramExprRefNode(...)`
- `ensureProgramExpr(...)`
- `exprHasExactProgramSemantics(...)`

Required end state:

- executable-program construction is owned by `Lambdamono`
- `Pipeline` does not perform late assembly itself
- any builder scratch state lives in `Lambdamono`, not in `Pipeline`

Concrete work:

1. Move the remaining assembly routines themselves under `Lambdamono`.
2. Introduce `Lambdamono` builder/assembler APIs that:
   - assemble root exprs
   - assemble callable bodies
   - assemble stmt/expr children
   - ensure semantic shape before node emission
3. Replace direct assembly calls in `Pipeline` with `Lambdamono` stage-owned APIs.

Success condition:

- `Pipeline.zig` no longer assembles executable nodes directly.

### C. Move Remaining Semantic Scan Algorithms Out Of `Pipeline`

Current remaining semantic algorithms still in `Pipeline.zig`:

- `scanStmt(...)`
- `scanCirExprInternal(...)`
- `scanCirExprChildren(...)`
- `realizeLookupExprSemantics(...)`
- `realizeStructuredExprCallableSemantics(...)`
- `realizeDispatchExprSemantics(...)`
- related scanning/materialization helpers that are still part of the same root-analysis engine

Required end state:

- `Lambdasolved` owns solved scanning and semantic realization
- `Pipeline` does not contain the body of the semantic scan engine

Concrete work:

1. Move root-analysis scan routines into `Lambdasolved` runner/solver APIs.
2. Move lookup/callable/dispatch realization into `Lambdasolved`.
3. Replace `Pipeline` direct calls with stage-owned entrypoints.
4. Leave only thin orchestration in `Pipeline`.

Success condition:

- the semantic scan/realization engine lives in `Lambdasolved`, not in `Pipeline`.

### D. Delete Direct Reads Of Stage-Internal Scan State

Current remaining leak:

- `Pipeline.zig` still owns local root-analysis orchestration in its run methods instead of delegating execution to a stage-owned runner type

Required end state:

- `Pipeline` does not inspect stage-internal mutable state directly
- that decision becomes a `Lambdasolved` stage API or is eliminated by the architectural move in Stage C

Concrete work:

1. Keep root-analysis state owned by `Lambdasolved`, not by `Pipeline`.
2. Move the remaining run-method orchestration that creates and drives root-analysis into a stage-owned runner.
3. If Stage C makes even that local runner state unnecessary to mention in `Pipeline`, delete the leak entirely.

Success condition:

- no stage-internal root-analysis state is owned by `Pipeline`.

### E. Replace The Remaining Fat `Lambdamono.Expr`

Current remaining non-ideal representation in `Lambdamono.zig`:

- `Expr` as a fat struct with:
  - `payload`
  - `origin`
  - generic child spans
  - helper getters that still decode semantic meaning after the fact
- `AssemblyState` still exists as local assembly tracking around this struct

Required end state:

- `Lambdamono.Expr` becomes a strict executable AST
- invalid combinations become unrepresentable

Concrete work:

1. Replace the current `Expr` struct with a tagged union of executable node kinds.
2. Make each variant carry exactly the data it needs.
3. Eliminate generic semantic decoding helpers that only exist because `Expr` is over-flexible.
4. Push shared metadata into a small common header if needed, but not semantic role slots.
5. Rework assembly/build logic to emit concrete variants directly.

Required variants include at least:

- local lookup
- def lookup
- direct callable
- packed callable
- closure introduction
- direct call
- indirect call
- low-level call
- dispatch target
- dispatch intrinsic
- projection / origin-bearing lookup node as needed
- block
- switch/match
- return
- literals / tuple / record / tag forms as needed

Success condition:

- `ExprPayload` no longer acts as a semantic overlay on a generic fat record
- fat `Expr` payload/origin decoding is gone
- invalid executable states are unrepresentable in the IR type itself

### F. Delete Transitional Builder State Once The Strict AST Exists

Current remaining transitional state:

- `Lambdamono.AssemblyState`

Required end state:

- any remaining builder scratch is narrowly scoped to the builder implementation
- no user-visible or pipeline-visible transitional executable-state layer remains

Concrete work:

1. After strict AST emission is in place, reduce or delete `AssemblyState`.
2. Keep only truly local builder recursion guards if still needed.
3. Ensure the final program is the only source of truth.

Success condition:

- there is no broad transitional builder overlay remaining around executable expr state.

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
