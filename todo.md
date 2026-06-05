# OOM-swallow audit — remaining items

Tier 1 (compiler-core, real compile path, silently returned a WRONG value) is **DONE** —
these now propagate `error.OutOfMemory` instead of swallowing it:

- `src/check/Check.zig`: `typeSupportsIsEq`, `varContainsUnboxedFunctionInHostedSignature`,
  `nominalSupportsImplicitIsEq`, `varSupportsIsEq` (all `…Internal(...) catch false` → `try`).
- `src/check/exhaustive.zig`: `getCtorArgTypes`, `getRecordFieldTypeByName`.
- `src/check/unify.zig`: `unifyTwoAliases` (`catch {}` now re-raises non-`TypeMismatch`).
- `src/canonicalize/CIR.zig`: `resolveImportsByExactModuleName`.
- `src/canonicalize/Can.zig`: `parseIntText`, `canonicalizeFileImport` (OOM split out from file-IO diagnostic).
- `src/canonicalize/NodeStore.zig`: `replaceExprWithCallConstraint`, `replaceExprWithDispatchCall`,
  `replaceExprWithTypeDispatchCall` (`catch unreachable` → `try`).
- `src/canonicalize/BuiltinLowLevel.zig`, `src/canonicalize/HostedCompiler.zig`: `fresh() catch unreachable` → `try`.
- `src/parse/NumericLiteral.zig`: `parse`, `decimalParts`, `compactFrac` (OOM re-raised, invalid-numeral kept).
- `src/compile/compile_build.zig`: `detectPackageCycle` (no longer reports "no cycle" on OOM),
  `renderDiagnostics`/`drainReports`/`OrderedSink.emitReport` (diagnostics no longer silently dropped),
  import/module scheduling (`ensureModule`/`scheduleModule`/`scheduleExternal`/`dottedToPath`).
- `src/compile/coordinator.zig`: `appendWorkerFailureReport` + worker-thread boundary (atomic OOM flag
  observed by `coordinatorLoop`, which aborts instead of silently dropping the failure).

---

## Deferred — each with a reason it is NOT a simple "add `try`"

### Cannot propagate (C ABI) — panic is the only option
- [ ] `src/eval/compiler_host.zig`: `rocAlloc`, `rocRealloc`, `allocateBytes` (`@panic("OOM")`)
- [ ] `src/eval/interpreter.zig`: `interpreterErasedCallableTrampoline` OOM arm (→ `ops.crash`)
  > These are `callconv(.c)` callbacks invoked from generated/host code; they cannot return a Zig
  > error. Panic/crash is the only available behavior.

### `defer`-based cleanup — Zig `defer` cannot return an error (needs restructuring)
- [ ] `src/canonicalize/Can.zig`: ~16 `defer { self.scopeExit(self.env.gpa) catch {} }` sites
  (`canonicalizeExpr`, the `?`/`??` binops, `canonicalizeForLoop`, the map2 builders, `canonicalizeBlock`, …)
  plus `processAssociatedBlock`'s `scopeExit(...) catch unreachable`.
  > Fixing requires making `scopeExit` non-allocating or hoisting it out of `defer`.

### Backend-wide cascade — needs a precomputed field, not error propagation
- [ ] `src/layout/store.zig`: `layoutContainsRefcounted` (`catch @panic`)
- [ ] `src/interpreter_layout/store.zig`: its own `layoutContainsRefcounted` (`catch @panic`)
  > Making these fallible cascades `try` through the entire backend (llvm/dev/wasm codegen,
  > interpreter, glue, static-data exports). The right fix is a precomputed `contains_refcounted`
  > flag on the layout so the query stays infallible.

### Best-effort cache — propagating OOM would abort the build on a transient cache failure (design call)
- [ ] `src/compile/coordinator.zig`: checked-module disk cache (`storeCheckedModuleInCache`,
  `tryLoadCachedCheckedModule` — ~12 `catch return false`/recordStoreFailure sites)
- [ ] `src/compile/cache_manager.zig`: `storeRawBytes`, `loadRawBytes`
- [ ] `src/compile/cache_cleanup.zig`: ~12 best-effort housekeeping sites
  > These intentionally degrade to cache-miss / skipped-store / skipped-cleanup. Propagating OOM
  > here turns a transient cache allocation failure into a hard build abort. Decide whether that
  > tradeoff is wanted before changing.

### Diagnostic-message quality only (crash/expect message lost on OOM)
- [ ] `src/eval/interpreter.zig`: `recordCrash`, `rocExpectFailedFn`, `recordFailedCallStackIfUnset` sites
- [ ] `src/eval/compiler_host.zig`: `rocExpectFailed`, `rocCrashed`
  > The crash/expect still fires; only the captured message/stack is lost on OOM. Low value.

### Tooling — not compilation stages (peripheral; large IO error sets, many already explicit)
- [ ] `src/lsp/**` (~120 sites): handlers return empty/null on OOM → editor features degrade
- [ ] `src/cli/**` diagnostic rendering + `getEnvVar`/`readDefaultAppSource` paths
- [ ] `src/build/**` (test_harness, modules, ci_steps — dev/CI only)
- [ ] `src/glue`, `src/playground_wasm`, `src/snapshot_tool`, `src/echo_platform`, `src/fmt`, `src/unbundle`
  > Peripheral to the compiler proper. Many of these functions' error sets became explicit during the
  > inferred-error-union migration; the remaining swallows here are IO-boundary best-effort handling.

## Already fixed (pre-existing PRs)
- [x] `src/check/Check.zig` `probeBranchCompatible` snapshot — PR #9543
- [x] `src/check/Check.zig` `varContainsError` family — PR #9547
