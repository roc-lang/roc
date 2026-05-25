# Zig 0.16 Migration — Remaining Work

Snapshot taken after consolidating 9 of 12 review workstreams (W1, W2, W3, W4, W6, W7, W10, W11, W12 — landed in commits `83454b2f0c` and `a4b08883a4`). Open items below.

## High priority — finish before merge

### W9 — `replayTestCache` (rebase needed)

Implementation is **complete and verified** but lives uncommitted in an isolated worktree, with a baseline mismatch against W12's CacheConfig edits.

- **Worktree:** `.claude/worktrees/agent-a109a359fc2613ecd`
- **Patch (does not apply cleanly):** `/tmp/w9.patch` (or regenerate from the worktree)
- **Conflict:** W9 was authored against pre-W12 cli/main.zig; W12 added `.roc_ctx = ctx.coreCtx()` to 7 `CacheConfig` literals in `rocTest`. W9's edits overlap.
- **Verification when re-applied:** manual smoke showed 610 ms → 70 ms cache-hit speedup on a failing-test fixture; `roc_subcommands_test` 75 pass / 5 skip.
- **Cache key composition** (good to preserve when reconciling): magic `ROC_TEST_TOP_V1`, `build_options.compiler_version`, `@tagName(args.opt)`, `args.verbose`, `args.main` (presence + bytes), `args.path`, source bytes. No `--filter` flag exists in this codebase today; agent left a note that a `hasher.update(filter)` line is the only addition needed if one is added later.
- **Wire format** (in the worktree's encoder): magic / u32 passed / u32 failed / u32 stdout_tail_len+bytes / u32 stderr_tail_len+bytes.

Approach to reconcile: open the worktree's `src/cli/main.zig`, copy the three new top-level functions (`replayTestCache`, `encodeTopTestCache`, `decodeTopTestCache`) plus the `cli_test_top_cache_magic` constant and `cliTestTopCacheKey` helper into the consolidated tree, then re-wire the `rocTest` call site to invoke `replayTestCache` on cache hit (replacing the current TODO no-op block) while preserving W12's `.roc_ctx = ctx.coreCtx()` on the surrounding `CacheConfig{...}` literal.

### W5 — `cli/main.zig` coordinator-report drain on error

W5 agent already implemented this (L2001 `ctx.coreCtx()` replacement + L2047 `coordinatorLoop catch` wrapper) in a worktree that has since been cleaned up. The L2001 change is *already* in the consolidated commit via W12's separate edit, but the L2047 `coordinatorLoop` drain is **not**. Re-apply by hand:

```zig
coord.coordinatorLoop() catch |err| {
    _ = renderCoordinatorReports(ctx, &coord, roc_file_path);
    return err;
};
```

Mirror the success-path arg list at the next `renderCoordinatorReports` call. `_ =` because that function returns `CoordinatorReportCounts`.

### W8 — Playground REPL `publishFromTypedModule` trap

Investigation stopped mid-run during the consolidation pause. Carry-forward findings:

- **Trap fires in:** the **first** call to `compileProgramForTarget`, which is the **first REPL_STEP that's an expression** (not a definition) after session state has accumulated.
- **Reproducer:** REPL steps `x = 10`, `y = x + 5`, then `y`. Wrapper code generated: `module []\n\nx = 10\ny = x + 5\nmain = || Str.inspect((y))`.
- **Trap site narrowed to:** `src/check/checked_artifact.zig` between template index `40` and `50` inside `sealCheckedProcedureTemplateRefs` → `EntryWrapperTable.get()` unchecked index access at `:5154`.
- **Affected tests:** `test/playground-integration/main.zig:1187,1210` (still `.skip = true`).
- **Why CLI .wasm passes but playground WASM doesn't:** REPL state reuse across steps in `src/playground_wasm/main.zig` (~`compileReplInspectedModule`) is the suspect; CLI .wasm compiles each module in isolation.
- **Next step:** rerun the W8 agent (or do by hand) with bounds-checked logging at `checked_artifact.zig:5154` printing `wrappers.len` vs `@intFromEnum(id)` across both `definition` and `expression` REPL steps. Compare. If structural (allocator lifecycle mismatch), file a GH issue and leave the skips.
- **Worktree** (origin/main base — useless as a starting point): `.claude/worktrees/agent-a3e0025dbc2060436`. Just dispatch a fresh agent.

## Medium priority — should land in this PR

### W13 — Tidy ban-list expansion

Add to `ci/tidy.zig` `banned_io_patterns` (around L276) and adjacent ban lists. Land **after** the workstreams above so it doesn't false-trip mid-flight.

- `std.fs.cwd(` (note: `std.Io.Dir.cwd(` is already banned; old spelling slips through)
- `std.process.run(.{` (single-options-struct 0.15 signature — would have caught W3)
- `Term.Exited`, `Term.Signal`, `Term.Stopped`, `Term.Unknown` (capitalised — would have caught W3)
- `std.posix.getenv(` (would have caught W11)
- `std.posix.fork(`, `std.posix.waitpid(`, `std.posix.pipe(` — carve out `src/shim_io.zig`, `src/sljmp/`, `src/base/stack_overflow_posix.zig`
- `std.process.Child.init(` (rename to `std.process.spawn`)
- `std.io.getStdOut(`, `std.io.getStdErr(`, `std.io.getStdIn(` — currently only checked in `src/cli/`; widen
- `getEndPos(` — none in tree today, cheap insurance

Also add `src/llvm_compile/` to `core_modules` so it's covered by `tidyBannedStdIo`.

### Sibling UB in `src/compile/compile_build.zig:329` — `BuildEnv.setCoreCtx`

W4 (Coordinator) caught and fixed the `orelse CoreCtx.default(self.roc_ctx.gpa, …)` pattern in `coordinator.zig` but flagged that `BuildEnv.setCoreCtx` at `compile_build.zig:329` has the same bug, out of W4 scope. Apply the same fix: make the param non-optional, drop the `orelse`-fallback.

## Tracking issues to file (W14)

File one GH issue per item via `gh issue create`. Title format: `[zig-16 follow-up] <topic>`. Body should include file:line refs and acceptance criteria.

1. **arm64win cross-compile** — `build.zig:34-35`, `src/cli/test/platform_config.zig:67-68`. Upstream Zig stdlib `@ptrCast` alignment bug at `std/debug/SelfInfo/Windows.zig:569`. Re-enable when fixed upstream.
2. **`defer_numeric_defaults` re-enable** — `src/compile/compile_package.zig:1600`. Waiting on `ModuleEnv` to regain the field upstream; restore the conditional once it lands. Confirmed not a regression from this branch (field doesn't exist on origin/main either).
3. **`runViaDev` parking** — `src/cli/main.zig` (around L5485). Documented temporary shim; reference this PR + commit `deedd0cc93` so future readers see it's intentional. Long-term plan is dev backends + interpreter as the only `roc run` options.
4. **CIR-level test backend port** — `src/cli/main.zig:4427`. Intentional `{}` stub for the CIR-level test backend (dev/interpreter); not yet ported to zig-16.
5. **Comptime evaluator for mono snapshots** — `src/snapshot_tool/main.zig:1257-1258`. Re-wire `ComptimeEvaluator` once it lands so mono snapshots match origin/main output.
6. **Bytebox vendored 0.15 APIs** — `vendor/bytebox/src/wasi.zig`, `vendor/bytebox/src/definition.zig`. Heavy `std.posix.*` and `std.mem.indexOf` use; currently riding deprecated aliases. Upstream bytebox fix needed.
7. **`builtin_doc_tests` identity tracking** — `src/eval/test/builtin_doc_tests.zig` (around the `expected_failure_count == 49` assertion). Count-only assertion can't detect a fixed+regressed pair. Move to a set of failing-block IDs.
8. **Linux x86_64 coverage / lazy-dependency restructure** — `build.zig:1141, 3500, 3508, 3659, 3680`. Same upstream Zig 0.15 bug references.
9. **Channel timing test Windows skip** — `src/compile/channel.zig:515`. `std.c.nanosleep`/`timespec.sec` is `void` on Windows.
10. **`numeric_fold` Windows Dec-param-passing bug** — `src/cli/test/fx_test_specs.zig:114-115`. Dev backend issue on Windows x86_64.
11. **`test_issue9034` opaque-type panic** — `src/cli/test/fx_test_specs.zig:99-102`. `src/check/canonical_type_keys.zig:192` panics on platform-exposed opaque types in annotations.
12. **REPL playground trap** — once W8 is either fixed or definitively triaged; include the trap site (`checked_artifact.zig:5154`), the narrowed location (`sealCheckedProcedureTemplateRefs` template index 40–50), the two failing scenarios, and the CLI-vs-playground divergence.
13. **`semantic_audit.pl` comment-discipline replacement** — port the comment-word check (`fallback`/`heuristic`/`reconstruct`/`rebuild`/`best-effort`) to `ci/tidy.zig` if the team still wants the discipline.

## Worktree cleanup

Before final merge, prune the worktrees and their branches:

```
git worktree remove .claude/worktrees/agent-a109a359fc2613ecd   # W9 (after reconciliation above)
git worktree remove .claude/worktrees/agent-a3e0025dbc2060436   # W8
git worktree prune
git branch -D worktree-agent-a109a359fc2613ecd worktree-agent-a3e0025dbc2060436
# Other prunable worktree branches: worktree-agent-a646e99b, worktree-agent-ae361559 (older)
```

## End-to-end verification gate before merge

1. `zig build` — clean.
2. `zig build minici` — green (validates W13 ban-list doesn't false-trip once added).
3. `zig build test --summary all` — green; in particular `test-eval`, `test-cli`, `test-base`, `test-playground` if W8 fixed.
4. Manual smoke: `roc docs --serve test/snapshots/` + `curl 127.0.0.1:8080/` (W7).
5. Manual smoke: `roc run` on a Roc app using `echo!` in a loop; watch RSS to verify no leak (W1's decref fix).
6. Manual smoke: macOS dynamic-lib build path exercised (W3) — any platform compiling to `.dylib`.
7. Confirm every tracking issue from W14 is filed and linked from the PR description.
