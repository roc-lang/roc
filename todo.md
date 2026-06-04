# OOM-swallow audit

Sites where an `error.OutOfMemory` (or an `Allocator.Error`/wrapper containing it) is
caught and **not propagated** — instead a default value is returned, the error is
ignored, or it `unreachable`/`@panic`s. Found by a systematic sweep of `src/`
(every flagged site confirmed to have OOM in the caught error set).

Principle: **always propagate OOM.** Two already fixed: `probeBranchCompatible`
(PR #9543) and `varContainsError` (PR #9547).

Legend: FORM ∈ {RETURN-DEFAULT, IGNORE, UNREACHABLE, PANIC}. "runtime-path" =
reachable during normal `roc` compilation of a user program (vs dev/LSP/build tooling).

---

## Tier 1 — compiler core, real compile path, silently returns a WRONG value (fix first)

Enclosing fn already returns an error union, so a clean `try` fix exists.

### src/check — type-property probes return `false` on OOM (direct siblings of the varContainsError fix)
- [ ] `src/check/Check.zig:10092` | RETURN-DEFAULT `false` | `typeSupportsIsEqInternal(...) catch false` | fn `typeSupportsIsEq`
- [ ] `src/check/Check.zig:10166` | RETURN-DEFAULT `false` | `varContainsUnboxedFunctionInHostedSignatureInternal(...) catch false` | fn `varContainsUnboxedFunctionInHostedSignature`
- [ ] `src/check/Check.zig:10267` | RETURN-DEFAULT `false` | `varSupportsIsEqInternal(getNominalBackingVar(...)) catch false` | fn `nominalSupportsImplicitIsEq`
- [ ] `src/check/Check.zig:10483` | RETURN-DEFAULT `false` | `varSupportsIsEqInternal(...) catch false` | fn `varSupportsIsEq`

### src/check/exhaustive — match-exhaustiveness helpers (wrong coverage on OOM)
- [ ] `src/check/exhaustive.zig:1569` | RETURN-DEFAULT empty slice | `seen_exts.getOrPut(ext_var) catch { return &[_]Var{} }` | fn `getCtorArgTypes` (comment admits "return empty to avoid crash")
- [ ] `src/check/exhaustive.zig:1632` | RETURN-DEFAULT `backing_args` | `collectTypeParamsFromBackingType(...) catch` | fn `getCtorArgTypes`
- [ ] `src/check/exhaustive.zig:1645` | RETURN-DEFAULT `backing_args` | `allocator.alloc(Var, ...) catch` | fn `getCtorArgTypes`
- [ ] `src/check/exhaustive.zig:1720` | RETURN-DEFAULT `null` | `seen.getOrPut(gpa, resolved_var) catch return null` | fn `getRecordFieldTypeByName`

### src/check/unify — alias-backing unification result dropped (incl OOM)
- [ ] `src/check/unify.zig:679` | IGNORE | `self.unifyGuarded(a_backing, b_backing) catch {}` | fn `unifyAlias`

### src/canonicalize — OOM conflated with "invalid input" / silently skipped
- [ ] `src/canonicalize/CIR.zig:895` | RETURN-DEFAULT `return;` | `name_to_idx.ensureTotalCapacity(...) catch return` | fn `resolveImportsByExactModuleName` (on OOM, ALL import resolution skipped)
- [ ] `src/canonicalize/Can.zig:6126` | RETURN-DEFAULT `null` | `parseIntWithUnderscores(...) catch` | fn `parseIntText` (OOM → "invalid number")
- [ ] `src/canonicalize/Can.zig:5372` | RETURN-DEFAULT diagnostic+return | `roc_ctx.readFile(...) catch (else =>)` | fn `canonicalizeFileImport` (OOM folded into generic file IO error)

### src/parse/NumericLiteral — allocating numeral parse → "invalid" on OOM
- [ ] `src/parse/NumericLiteral.zig:185` | RETURN-DEFAULT `.invalid` | `parseExact(...) catch` | fn `parse`
- [ ] `src/parse/NumericLiteral.zig:406` | RETURN-DEFAULT `error.InvalidNumeral` | `parseI64NoUnderscores(...) catch` | fn `decimalParts`
- [ ] `src/parse/NumericLiteral.zig:540` | RETURN-DEFAULT `.invalid` | `decimalParts(...) catch` | fn `compactFrac`

### src/compile/compile_build — package cycle detection (HIGH RISK: "no cycle" on OOM → recursion hazard)
- [ ] `src/compile/compile_build.zig:962` | RETURN-DEFAULT `false` | `stack.append(gpa, to_pkg) catch` | fn `detectPackageCycle`
- [ ] `src/compile/compile_build.zig:974` | RETURN-DEFAULT `false` | `visited.put(gpa, cur, {}) catch` | fn `detectPackageCycle`
- [ ] `src/compile/compile_build.zig:984` | RETURN-DEFAULT `false` | `stack.append(gpa, next) catch` | fn `detectPackageCycle`

### src/compile — user-facing diagnostics silently dropped on OOM (HIGH RISK: failing compile looks clean)
- [ ] `src/compile/compile_build.zig:2301` | RETURN-DEFAULT empty slice | `self.drainReports() catch return &.{}` | fn `renderDiagnostics` (ALL diagnostics lost)
- [ ] `src/compile/compile_build.zig:2568` | RETURN-DEFAULT return | `self.entries.append(...) catch` | fn `OrderedSink.emitReport`
- [ ] `src/compile/compile_build.zig:2569` | RETURN-DEFAULT return | `self.index.put(...) catch null` | fn `OrderedSink.emitReport`
- [ ] `src/compile/compile_build.zig:2576` | RETURN-DEFAULT deinit+return | `entries.items[i].reports.append(report) catch` | fn `OrderedSink.emitReport`
- [ ] `src/compile/coordinator.zig:2472` | RETURN-DEFAULT `null` | `std.fmt.allocPrint(...) catch` | fn `appendWorkerFailureReport` (report loses path context)
- [ ] `src/compile/coordinator.zig:2474` | IGNORE | `rep.addErrorMessage(...) catch` | fn `appendWorkerFailureReport`
- [ ] `src/compile/coordinator.zig:2477` | IGNORE | `reports.append(allocator, rep) catch` | fn `appendWorkerFailureReport` (failure report dropped)
- [ ] `src/compile/compile_build.zig:1812` | RETURN-DEFAULT `""` path | `self.moduleToPath(...) catch` | fn `drainReports` (diagnostic loses file path)

### src/compile/compile_build — import/module scheduling silently skipped on OOM
- [ ] `src/compile/compile_build.zig:651` | IGNORE `continue` | `sched.ensureModule(...) catch` | result-transfer loop (module skipped)
- [ ] `src/compile/compile_build.zig:786` | RETURN-DEFAULT return | `self.ws.dottedToPath(...) catch` | fn `resolverScheduleExternal`
- [ ] `src/compile/compile_build.zig:794` | IGNORE | `sched.scheduleModule(...) catch` | import-resolution callback
- [ ] `src/compile/compile_build.zig:861` | RETURN-DEFAULT unresolved `import_name` | `self.ws.dottedToPath(...) catch` | fn `resolverResolveLocalPath`

---

## Tier 2 — compiler core, real path, graceful-degradation by design (cache/diagnostic-quality; lower risk)

Result is still correct; OOM just means cache-miss/recompile or a lost diagnostic message.

### src/compile/coordinator — checked-module disk cache (OOM → cache miss / skipped store)
- [ ] `src/compile/coordinator.zig:2553` RETURN-DEFAULT | `getCheckedArtifactCacheDir(...)` | `storeCheckedModuleInCache` (MIXED)
- [ ] `src/compile/coordinator.zig:2559` RETURN-DEFAULT | `serializeModuleEnvForCache(...)` | `storeCheckedModuleInCache`
- [ ] `src/compile/coordinator.zig:2565` RETURN-DEFAULT | `encodeCheckedModuleCacheEntry(...)` | `storeCheckedModuleInCache`
- [ ] `src/compile/coordinator.zig:2586` RETURN-DEFAULT `false` | `checkedModuleCacheKey(...)` | `tryLoadCachedCheckedModule`
- [ ] `src/compile/coordinator.zig:2588` RETURN-DEFAULT `false` | `getCheckedArtifactCacheDir(...)` | `tryLoadCachedCheckedModule` (MIXED)
- [ ] `src/compile/coordinator.zig:2603` RETURN-DEFAULT `false` | `module_alloc.alignedAlloc(...)` | `tryLoadCachedCheckedModule`
- [ ] `src/compile/coordinator.zig:2611` RETURN-DEFAULT `false` | `module_alloc.dupe(...)` | `tryLoadCachedCheckedModule`
- [ ] `src/compile/coordinator.zig:2624` RETURN-DEFAULT `false` | `deserializeWithMutableTypes(...)` | `tryLoadCachedCheckedModule` (MIXED)
- [ ] `src/compile/coordinator.zig:2645` RETURN-DEFAULT `false` | `publishCheckedArtifactFromCheckedModuleWithStorage(...)` | `tryLoadCachedCheckedModule` (MIXED)
- [ ] `src/compile/coordinator.zig:2659` RETURN-DEFAULT `false` | `allocateCheckedArtifact(...)` | `tryLoadCachedCheckedModule`
- [ ] `src/compile/coordinator.zig:2676` RETURN-DEFAULT `false` | `retired_checked_artifacts.append(...)` | `tryLoadCachedCheckedModule`
- [ ] `src/compile/coordinator.zig:2689` RETURN-DEFAULT `false` | `registerCheckedArtifact(...)` | `tryLoadCachedCheckedModule`

### src/compile/cache_manager — best-effort store/load
- [ ] `src/compile/cache_manager.zig:79` RETURN-DEFAULT | `ensureCacheSubdirIn(...)` | `storeRawBytes` (MIXED)
- [ ] `src/compile/cache_manager.zig:85` RETURN-DEFAULT | `computeCacheFilePath(...)` | `storeRawBytes`
- [ ] `src/compile/cache_manager.zig:91` RETURN-DEFAULT | `std.fmt.allocPrint(...)` | `storeRawBytes`
- [ ] `src/compile/cache_manager.zig:115` RETURN-DEFAULT `null` | `computeCacheFilePath(...)` | `loadRawBytes`
- [ ] `src/compile/cache_manager.zig:126` RETURN-DEFAULT `null` | `roc_ctx.readFile(...)` | `loadRawBytes` (MIXED)

### src/compile/cache_cleanup — best-effort housekeeping (12 sites)
- [ ] `src/compile/cache_cleanup.zig:128,161,167,181,186,191,205,256,274,279,312,316` | IGNORE/RETURN-DEFAULT | `allocPrint`/`path.join`/`listDir`/`getEffectiveCacheDir`/`dupe` | various cleanup fns

### src/compile worker wrappers (OOM surfaces as parse_failed/compile_failed)
- [ ] `src/compile/coordinator.zig:3581` RETURN-DEFAULT `.parse_failed` | `executeParseFallible(...)` | `executeParse` (MIXED)
- [ ] `src/compile/coordinator.zig:3712` RETURN-DEFAULT `.compile_failed` | `executeCanonicalizeFallible(...)` | `executeCanonicalize` (MIXED)
- [ ] `src/compile/coordinator.zig:3796` RETURN-DEFAULT `.compile_failed` | `executeTypeCheckFallible(...)` | `executeTypeCheck` (MIXED)
- [ ] `src/compile/compile_package.zig:568` IGNORE | `modules.ensureTotalCapacity(gpa, 256)` | `init` (benign capacity hint)
- [ ] `src/compile/compile_package.zig:608` IGNORE | `modules.ensureTotalCapacity(gpa, 256)` | `initWithResolver` (benign)
- [ ] `src/compile/compile_package.zig:1056` RETURN-DEFAULT | `parse.parse(...)` | `doParse` (MIXED; OOM masked as "parse failed, proceed")

### src/eval — crash/expect message + failed-call-stack capture (message lost on OOM)
- [ ] `src/eval/interpreter.zig:146` RETURN-DEFAULT `null` | `allocator.dupe(u8, msg)` | `recordCrash`
- [ ] `src/eval/interpreter.zig:193` RETURN-DEFAULT `null` | `allocator.dupe(u8, source)` | `rocExpectFailedFn`
- [ ] `src/eval/interpreter.zig:781` IGNORE | `recordFailedCallStackIfUnset()` | `eval` (setjmp crash path)
- [ ] `src/eval/interpreter.zig:1414` IGNORE | `recordFailedCallStackIfUnset()` | `evalProcSpec` (errdefer)
- [ ] `src/eval/compiler_host.zig:99` RETURN-DEFAULT `null` | `allocator.dupe(...)` | `rocExpectFailed`
- [ ] `src/eval/compiler_host.zig:105` RETURN-DEFAULT `null` | `allocator.dupe(...)` | `rocCrashed`

---

## Tier 3 — `catch unreachable` / `catch @panic` on allocations (crashes, not silent; still not "propagate")

### Could be restructured to propagate
- [ ] `src/canonicalize/NodeStore.zig:1210` UNREACHABLE | `span2_data.append(...)` | `replaceExprWithCallConstraint`
- [ ] `src/canonicalize/NodeStore.zig:1252` UNREACHABLE | `addMethodCallData(...)` | `replaceExprWithDispatchCall`
- [ ] `src/canonicalize/NodeStore.zig:1274` UNREACHABLE | `addMethodCallData(...)` | `replaceExprWithTypeDispatchCall`
- [ ] `src/canonicalize/BuiltinLowLevel.zig:117` UNREACHABLE | `env.types.fresh()` | `replaceProvidedByCompilerLowLevels`
- [ ] `src/canonicalize/HostedCompiler.zig:27` UNREACHABLE | `env.types.fresh()` | hosted compile entry
- [ ] `src/canonicalize/Can.zig:2307` UNREACHABLE | `self.scopeExit(...)` | `processAssociatedBlock`
- [ ] `src/layout/store.zig:1903` PANIC | `layoutContainsRefcountedInner(...)` | `layoutContainsRefcounted`
- [ ] `src/compile/compile_build.zig:865` PANIC | `self.gpa.create(ResolverCtx)` | `makeResolver`

### ABI-constrained — `callconv(.c)` callbacks, genuinely CANNOT propagate a Zig error (likely leave as panic)
- [ ] `src/eval/compiler_host.zig:65` PANIC | `allocations.put(...)` | `rocAlloc`
- [ ] `src/eval/compiler_host.zig:90` PANIC | `allocations.put(...)` | `rocRealloc`
- [ ] `src/eval/compiler_host.zig:110-114` PANIC (5×) | `allocator.alignedAlloc(...)` | `allocateBytes`
- [ ] `src/eval/interpreter.zig:2399` PANIC→crash | `callInterpreterErasedCallable(...)` OOM arm | `interpreterErasedCallableTrampoline`

---

## Tier 4 — `defer`-based cleanup swallows (CANNOT be a simple `try` — defer can't return error; needs restructuring)

### src/canonicalize/Can.zig — `defer { self.scopeExit(self.env.gpa) catch {} }`
- [ ] `src/canonicalize/Can.zig:7249, 7795, 7865, 8247, 8275, 8607, 8677, 8820, 8881, 9084, 9372, 9424, 9486, 9527, 12221, 12917` (16 sites)

---

## Tier 5 — tooling (NOT the compile path of a user program) — lower priority

### src/lsp (~120 sites) — handlers return empty slice / null on OOM → editor features silently degrade
- [ ] `src/lsp/syntax.zig:300, 301, 319, 320, 978, 998, 1476, 1477, 1502, 1519, 1884, 1888, 1904, 1931, 2022, 2090-2110 (6×), 2155`
- [ ] `src/lsp/completion/builder.zig:99, 275, 461, 318/484/534/591/760/924/1150/1224 (tw.write, 8×), 1458-1545 (buf.append, 27×)`
- [ ] `src/lsp/build_session.zig:72, 85`
- [ ] `src/lsp/cir_queries.zig:271`
- [ ] `src/lsp/semantic_tokens.zig:333`
- [ ] `src/lsp/build_env_handle.zig:76, 85` (debug-only)
- [ ] `src/lsp/handlers/folding_range.zig:92, 97`
- [ ] `src/lsp/handlers/document_highlight.zig:90, 158, 163`
- [ ] `src/lsp/handlers/document_symbol.zig:57`
- [ ] `src/lsp/handlers/selection_range.zig:110`
- [ ] `src/lsp/handlers/semantic_tokens.zig:50`

### src/cli — diagnostic rendering aborts on OOM (~90 sites) + misc
- [ ] `src/cli/builder.zig:331-337, 351-360, 374-379, 393-402, 416-438, 452-461` (clustered `addText/addAnnotated ... catch return`)
- [ ] `src/cli/platform_validation.zig:114-122, 137-145, 160-177` (same pattern)
- [ ] `src/cli/main.zig:1401 (readDefaultAppSource), 2362, 2366 (getEnvVar), 5218 (processModuleByName catch break), 5863 (toOwnedSlice)`
- [ ] `src/cli/CliCtx.zig:320` (toReport → drop report)

### src/build — dev/test tooling (test_harness.zig, modules.zig, ci_steps.zig)
- [ ] `src/build/test_harness.zig:483, 807, 854, 856, 902, 1043, 1089, 1134, 1135, 1282, 1287-1290, 1343`
- [ ] `src/build/modules.zig:69, 73, 74, 167, 191, 195` (build-script; `@panic("OOM")`)
- [ ] `src/build/ci_steps.zig:61, 89` (build-script; `@panic("OOM")`)

### Peripheral generators / FFI (glue, playground_wasm, snapshot_tool, echo_platform, fmt, unbundle)
- [ ] `src/glue/glue.zig:216, 936, 937, 947, 963, 1003-1004, 1035, 1039, 1062, 1070, 1145, 1167, 1194, 1336, 1953, 2101, 2103, 2309-2313, 2346, 2360, 2392-2422, 2429, 2445-2609 (printTypeAnnoToBuf, 42×)`
- [ ] `src/glue/platform/host.zig:208-212, 297-301`
- [ ] `src/playground_wasm/main.zig:508, 519, 819, 851, 1305, 1315, 1393, 1563-1566, 1764-1772, 2122, 2284`
- [ ] `src/playground_wasm/WasmFilesystem.zig:33, 47, 114, 199` (`@panic`)
- [ ] `src/snapshot_tool/main.zig:498, 3661, 3662, 4400, 4432`
- [ ] `src/echo_platform/echo.zig:118`, `src/echo_platform/mod.zig:192, 208` (returns unsanitized input!), `src/echo_platform/runner.zig:306`
- [ ] `src/fmt/fmt.zig:179, 228, 3212` (`@panic`)
- [ ] `src/unbundle/unbundle.zig:247` (extracted file dropped)

---

## UNCERTAIN (verify before touching)
- `src/compile/coordinator.zig:3919` — `result_channel.send(...) catch break`: `ChannelError` declares `OutOfMemory` but `Channel.send` never returns it (only `Closed`). Likely not a real OOM swallow.
- `src/cli/builder.zig:344,367,386,409,445,468`, `CliCtx.zig:324`, `main.zig:1870,1875,5751` — `renderReportToTerminal(...) catch {}`: dominant error is `WriteFailed`; unclear if it allocates internally.
- `src/cli/libc_finder.zig:85,90` — `process.run(...) catch continue`: MIXED (run allocates), but swallow targets spawn-failure while probing libc candidates.
- `src/check/Check.zig:10928, 10932` — `EnvPool.release` (returns `void`): `reset`/`append` OOM → `catch { deinit }`. Genuine cleanup recovery, not a wrong-value substitution.
- `src/parse/tokenize.zig:1707` — `gpa.dupe(u8, "") catch unreachable` in `checkTokenizerInvariants` (debug/test-only helper).

## Already fixed
- [x] `src/check/Check.zig` `probeBranchCompatible` snapshot — PR #9543
- [x] `src/check/Check.zig` `varContainsError` family — PR #9547
