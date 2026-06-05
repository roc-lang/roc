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

### C-ABI callbacks — DONE / correct
- [x] `src/eval/compiler_host.zig` `rocAlloc`/`rocRealloc`/`allocateBytes`: no longer `@panic`.
  They now write a null `answer`; the interpreter's alloc/realloc forwarders detect it and unwind
  to the eval boundary via the existing crash `longjmp`, so a compile-time program that exhausts
  memory becomes a reported crash instead of aborting `roc`.
- already correct: `interpreterErasedCallableTrampoline` translates OOM → `ops.crash(...)` (a Roc
  crash that unwinds), and `rocExpectFailed`/`rocCrashed` lose only the captured message string on
  OOM (the crash/expect still fires; a generic fallback message is used). Inherent to `callconv(.c)`.

### `defer`-based scope cleanup — DONE
- [x] `src/canonicalize/Can.zig`: the ~17 `defer { self.scopeExit(gpa) catch {} }` / `catch unreachable`
  sites now record OOM into a `scope_exit_oom` flag that `canonicalizeFile` re-raises as
  `error.OutOfMemory`. (A failed scope-exit leaves a surplus scope, never an underflow, so OOM is
  the only reachable failure.)

### Layout refcount query — deferred (corrected analysis; perf-relevant, correctness-critical)
- [ ] `src/layout/store.zig` + `src/interpreter_layout/store.zig`: `layoutContainsRefcounted` (`catch @panic`)
  > Correction to an earlier note: there is NO existing `contains_refcounted` field on general
  > layouts (only on the transient List/Box ABI descriptors). The query re-walks the struct/
  > tag-union/closure graph, allocating an `AutoHashMap` for cycle detection over recursive
  > placeholder indirections — that allocation is the only OOM source. The clean fix is to
  > precompute a `contains_refcounted` bit per layout `Idx` at insert time, making the query O(1)
  > and infallible (also a perf win, since it's called per-op from refcounting/codegen). Deferred
  > because it drives refcounting decisions (a wrong bit = use-after-free/leak) and the recursive-
  > placeholder representation must be understood to populate the bit correctly — worth doing as a
  > focused, carefully-reviewed change rather than bundled here. Note the panic is on genuine
  > compiler-internal OOM (not user input) and is loud, not a silent wrong-value.

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
