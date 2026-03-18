DO NOT COMMIT THIS FILE. `plan.md` must NEVER be committed.

# LLVM Restoration Plan

Execution strategy: the restored LLVM path must use the dylib approach. LLVM should emit a temporary shared library (`.dylib` / `.so` / `.dll` as appropriate), and Roc should load it via the platform dynamic loader and call the entrypoint from there. We are explicitly not planning to use ORC, and we are explicitly not planning to rely on custom object relocation handling as the long-term LLVM execution path.

## Current Gaps

- The real Zig LLVM implementation was deleted: `src/backend/llvm/*`, `src/eval/llvm_evaluator.zig`, and `src/llvm_compile/*`.
- Current eval parity is fake: `src/eval/test/helpers.zig` claims to compare interpreter/dev/wasm/llvm, but `llvmEvaluatorStr` just calls `devEvaluatorStr`.
- Current REPL LLVM is fake: `src/repl/eval.zig` initializes `DevEvaluator` for `.llvm` and executes `.llvm` through the dev path.
- REPL tests only check interpreter+dev.
- Snapshot tool already tries to compare `dev` and `llvm`, but today that `llvm` route is still the dev backend.
- Build wiring is incomplete: `eval` tests only get `bytebox`, not LLVM; `repl` tests get neither; snapshot artifacts no longer get explicit LLVM wiring at all.

## Plan

1. Restore the deleted source as the baseline.
   Recover `src/backend/llvm`, `src/eval/llvm_evaluator.zig`, and `src/llvm_compile` from the pre-delete tree (`40603a718d` / parent of `22ceb68`), then compile them against current `main` instead of blindly trusting the old versions.

2. Reconcile the restored LLVM code with current `src/`.
   Fix API drift in CIR, layouts, backend enums, build options, crash handling, and any stdlib-copy exclusions in `build.zig` so `src/backend/llvm` is a real buildable module again.

3. Make LLVM real in `eval`.
   Re-export `LlvmEvaluator` from `src/eval/mod.zig`, replace the fake alias in `src/eval/test/helpers.zig` with actual LLVM bitcode generation + execution through the temporary dylib + dynamic loader path, and require 4-way agreement among interpreter/dev/wasm/llvm everywhere eval helpers compare backends.

4. Make LLVM real in `repl`.
   Stop treating `.llvm` as `.dev` in `src/repl/eval.zig`. Add a real LLVM-backed REPL execution path that goes through `LlvmEvaluator` and the temporary dylib + dynamic loader pipeline.

5. Bring wasm into the same parity harnesses where we care about backend agreement.
   For eval this already exists conceptually, so make it real 4-way. For REPL/snapshots, add a wasm-backed execution adapter if needed so the same expressions/steps can be checked against interpreter, dev, wasm, and llvm instead of having REPL be a weaker 2- or 3-backend surface.

6. Fix the build graph so the parity surfaces can actually run.
   Reintroduce LLVM headers/libs and `llvm_compile` imports for snapshot artifacts, and add equivalent wiring for `eval` and `repl` tests. Add the shared-library link/load requirements for the LLVM evaluator path, and add `bytebox` anywhere REPL/snapshot code now needs wasm execution, not just `eval`.

7. Expand the tests so there are no fake passes.
   Replace REPL `expectBoth` with an all-backends helper. Make snapshot validation fail if any of interpreter/dev/wasm/llvm disagree. Remove any direct `compareWithLlvmEvaluator` calls that are just aliases and route everything through shared 4-way parity helpers.

8. Run the full matrix and fix until green.
   The plan is not done until `src/backend/llvm` is present, the LLVM path is actually executing, and the following are green:
   - `zig build test-eval`
   - `zig build test-repl`
   - `zig build snapshot -- --check-expected`
   - full `zig build` / CI-equivalent `src/` test coverage

## Done Means

- `src/backend/llvm` exists again and builds on current `main`.
- `llvm` execution is no longer aliased to `dev`, and it runs via the temporary dylib + dynamic loader path.
- Any place that currently compares backend outputs is upgraded to assert agreement with `llvm`, and wherever the surface is meant to cover all execution backends, it checks interpreter/dev/wasm/llvm together.
- Snapshot, eval, and REPL parity failures are hard test failures, not comments or best-effort logging.
