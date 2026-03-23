# LLVM Evaluator Bitrot

The `LlvmEvaluator` (`src/eval/llvm_evaluator.zig`) and `MonoLlvmCodeGen`
(`src/backend/llvm/MonoLlvmCodeGen.zig`) have fallen out of sync with the
rest of the compiler pipeline. Because `llvmEvaluatorStr` in
`src/eval/test/helpers.zig` has always aliased `devEvaluatorStr`, these
files were never compiled in practice, so the breakage went undetected.

## Current state

`llvmEvaluatorStr` delegates to `devEvaluatorStr`, which means the
parallel eval test runner (`zig build test-eval`) runs the dev backend
twice instead of exercising the real LLVM pipeline. The "llvm" column in
test output is therefore identical to "dev".

## Compilation errors (as of 2026-03-23)

### 1. Missing monomorphization step — `llvm_evaluator.zig`

`mir.Lower.init` now requires a `*const Monomorphize.Result` parameter.
The LLVM evaluator was calling the old 6-argument form. **Fixed** in this
branch (monomorphization step added), but the remaining errors below
prevent compilation.

### 2. `LirExprStore.getProcs` removed — `MonoLlvmCodeGen.zig:430`

`MonoLlvmCodeGen` calls `lir_store.getProcs()`, but `LirExprStore` no
longer exposes that method. The procedure/function definition storage API
has changed.

### 3. `.call` variant removed from `LirExpr` — `llvm_evaluator.zig:66`

`lirExprResultLayout` switches on `LirExpr` tags including `.call`, but
that variant no longer exists in `lir.LIR.LirExpr`. The enum has been
restructured.

## What needs to happen

1. Update `lirExprResultLayout` in `llvm_evaluator.zig` to match the
   current `LirExpr` enum variants.
2. Update `MonoLlvmCodeGen` to use the current `LirExprStore` API for
   accessing procedure definitions.
3. Once both compile, update `llvmEvaluatorStr` in `helpers.zig` to use
   the real `LlvmEvaluator` (the implementation was written and reverted
   in this branch — see git history).
4. Verify that LLVM-generated code produces correct results by running
   `zig build test-eval --verbose` and checking for backend mismatches.

## Files involved

- `src/eval/llvm_evaluator.zig` — orchestrates the LLVM pipeline
- `src/backend/llvm/MonoLlvmCodeGen.zig` — LLVM IR generation from LIR
- `src/eval/test/helpers.zig` — `llvmEvaluatorStr` (currently aliases dev)
- `src/lir/LirExprStore.zig` — LIR expression storage (API changed)
- `src/lir/LIR.zig` — LIR expression enum (variants changed)
