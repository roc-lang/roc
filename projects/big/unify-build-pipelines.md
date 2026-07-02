# Unify the Build Pipelines

## Problem

The compiler has two parallel build-orchestration paths that both wrap the
same `Coordinator` but independently reimplement package registration, cache
wiring, report rendering, and platform-relation setup. Every feature or fix
must land twice, and five recent shipped bugs were exactly "one pipeline had
it, the other didn't":

- PR 9811 — the run path passed a literal `null` cache manager to the
  Coordinator (with a comment to the effect of "no cache for IPC"), so
  `roc run` NEVER used the checked-module cache — a month after PR 9459 had
  wired caching into the check path. Issue 9788 presented as "the compiler
  feels slow"; nobody realized an entire pipeline was compiling from source
  every time.
- PR 9698 (issue 9694) — `roc file.roc --opt=speed` silently ran the
  interpreter anyway because the run entry point was hard-wired to the
  interpreter path and ignored the optimization flag.
- PR 9627 — the run path registered only the packages named directly in the
  app header, so transitive URL dependencies failed on `roc run` but worked
  on `roc build`.
- PR 9759 — duplicate diagnostics on run only: the run path rendered the
  Coordinator's accumulated reports both before and after executable
  finalization, and the rendering function re-renders ALL accumulated
  reports on every call.
- Also fixed in PR 9759 — the shared-memory shim path did not exit with
  code 2 on warnings, while the build path did.

Each fix patched one divergence. The generator of the bug class — two code
paths that must be kept in sync by hand — is still in the tree.

## Background

The compiler pipeline is: parse → canonicalize → type-check (producing
checked artifacts per module) → postcheck lowering → LIR → backends. All
compilation is scheduled by the `Coordinator`
(`src/compile/coordinator.zig`), which owns packages, modules, worker
threads, the checked-module cache load/store, and per-module report
storage.

Two wrappers drive the Coordinator today:

1. **BuildEnv** (`pub const BuildEnv` in `src/compile/compile_build.zig`).
   Used by `roc check` (`rocCheck` → `checkFileWithBuildEnvPreserved` in
   `src/cli/main.zig`), `roc build` (`rocBuildNative`/`rocBuildLlvm`/...),
   docs generation (which reuses the preserved check BuildEnv), `roc glue`
   (`rocGlueInner` in `src/glue/glue.zig` constructs a `BuildEnv`), and —
   since its migration — the compile half of `roc test` (`rocTest` in
   `src/cli/main.zig` calls `BuildEnv.init`, `buildWithMain` /
   `discoverDependencies` + `compileDiscovered`, then `renderDiagnostics`).
   BuildEnv owns package materialization, an ordered report sink
   (`emitReport`/`drainReports` — reports are drained, not re-iterated),
   and cache wiring via `setCacheManager`.

2. **`lowerLirWithCoordinator`** (`src/cli/main.zig`). The interpreter /
   LIR-image path used by dev-mode `roc run` and hot reload
   (`rocRunSharedMemoryShim`, `rocRunDefaultAppSharedMemoryShim`,
   `rocInternalHotReloadDev`, all via `buildLirImageWithCoordinator`). It
   hand-wires everything BuildEnv also does: parses the app header
   (`compile.app_header.parseAppHeader`), runs package version resolution,
   constructs the Coordinator, registers the "app" package and every
   resolved package with `ensurePackage`/`ensurePackageWithUrl`, builds a
   `package_names` array mapping root → `"app"` and the platform → `"pf"`,
   wires shorthands, enqueues parse tasks, runs `coordinatorLoop`,
   finalizes executable artifacts, and renders reports via
   `renderCoordinatorReports`.

The checked-module cache both paths feed: the key is a Merkle DAG —
SHA-256 over source hash (including ingested deps), compiler artifact
hash, module identity hash, checking-context hash, and the ordered direct
import artifact keys — stored content-addressed under
`~/.cache/roc/<version>/mod/` with atomic temp+rename writes. PR 9720 made
artifacts relocate-on-load (memcpy + comptime-audited pointer fixups); PR
9768 removed body checksums as redundant. `design.md`'s "Cache Boundary"
and "Host Symbol ABI" sections are authoritative for what the cache and
the platform relation must preserve. A pipeline that forgets to pass a
cache manager silently opts out of all of this.

Note one discrepancy with older descriptions of this split: `roc test` no
longer goes through `lowerLirWithCoordinator` — its compile half already
uses BuildEnv (see `rocTest`), though it still hand-wires its own cache
pass-through (`if (args.no_cache) null else build_env.cache_manager`) into
`runCheckedArtifactTests`. The run path is the remaining full duplicate.

## Evidence

- `src/compile/compile_build.zig` — `BuildEnv`, `build`, `buildWithMain`,
  `discoverDependencies`, `compileDiscovered`, `setCacheManager`,
  `drainReports`, `emitReport` (ordered sink).
- `src/cli/main.zig` — `lowerLirWithCoordinator`,
  `buildLirImageWithCoordinator`, `rocRun`, `rocRunBuildAndExec`,
  `rocRunSharedMemoryShim`, `rocRunDefaultAppSharedMemoryShim`,
  `rocInternalHotReloadDev`, `renderCoordinatorReports`, `rocTest`.
- Report re-rendering is accommodated, not prevented:
  `Coordinator.iterReports` returns a `ReportIter` that walks every report
  of every module of every package from the start on each call
  (`src/compile/coordinator.zig`), and `renderCoordinatorReports` carries a
  comment on its version-note handling — "Consuming the note keeps
  re-renders from attaching it twice." PR 9759's fix is call-ordering:
  `lowerLirWithCoordinator` renders early only under
  `coord.hasUserErrors()` and returns, otherwise renders exactly once
  after `finalizeExecutableArtifacts` / the AllowUserErrors variant.
- The naming skew PR 9811 left behind: `lowerLirWithCoordinator` builds
  `package_names` with root → `"app"`, platform → `"pf"`, justified only
  by the comment "matching Coordinator.discoverAppFromPath". See
  `../small/single-package-identity-function.md`.
- A `null` cache manager call site still exists:
  `setupSharedMemoryWithCoordinator` (`src/cli/main.zig`) forwards to
  `buildLirImageWithCoordinator` with `null` for the cache manager (no
  in-tree callers were found; it is a `pub` embedding surface, and it
  reproduces the exact PR 9811 pattern for anything that calls it).
- The delegation model that already works: `rocRun` dispatches
  `.size, .speed => rocRunBuildAndExec(ctx, args, arg0)`, and
  `rocRunBuildAndExec` simply calls `rocBuild` with run-shaped `BuildArgs`
  and then executes the produced binary (including the exit-2-on-warnings
  contract). One back half reusing the other path's front half — this is
  the shape the whole CLI should have.
- PRs (roc-lang/roc): 9811, 9459, 9698, 9627, 9759, 9720, 9768. Issues:
  9788 (run never cached, filed as slowness), 9694 (`--opt` ignored),
  9509 (transitive package failure on run, fixed by PR 9627).

## Solution design

One orchestration core, used by every entry path, owning exactly four
things: package registration / graph construction, cache wiring, report
accumulation and rendering, and the platform relation. Entry paths differ
only in their back half: executable link (`roc build`), LIR image +
interpreter/shim (`roc run` dev), test execution (`roc test`), diagnostics
only (`roc check`), docs extraction, glue extraction.

1. **Extract the core from BuildEnv** (or promote BuildEnv to be it).
   BuildEnv is the closest existing thing: it already serves check, build,
   docs, glue, and test compilation. Factor its front half (header parse,
   package resolution and registration, Coordinator construction, cache
   manager attachment, coordinator loop, finalization) into an API that
   yields a finished Coordinator plus drained diagnostics, without
   assuming the caller wants linked output.
2. **Collapse `lowerLirWithCoordinator` onto the core.** Its LIR-image
   production becomes a back half consuming the core's finished
   Coordinator. DELETE from `src/cli/main.zig`: the hand-rolled header
   parse + `resolvePackages` + `ensurePackage`/`ensurePackageWithUrl`
   loops, the `package_names` array and its "matching
   Coordinator.discoverAppFromPath" comment, the shorthand wiring, and the
   parse-task enqueueing — all of it exists in the core.
3. **Make report rendering structurally idempotent.** Replace the
   "render exactly once by call-site discipline" contract with either a
   draining cursor (BuildEnv's `drainReports` model, extended to the run
   path) or a per-report `rendered` flag consulted by the renderer. DELETE
   the re-render accommodations in `renderCoordinatorReports` (the
   version-note `fetchRemove` dance) once re-rendering is impossible.
   Keep `hasUserErrors`-style queries as pure queries, fully separate from
   rendering side effects.
4. **Unify exit-code policy.** Warning exit 2 and error exit codes are
   computed by the core from drained counts; back halves never invent
   their own exit logic (the shared-memory shim bug class).
5. **Route the stragglers.** `roc test`'s cache pass-through and execution
   half consume the core's cache manager rather than re-deriving it;
   `setupSharedMemoryWithCoordinator` either gains the core's cache wiring
   or is deleted if truly dead. Glue, the LSP, and the playground consume
   the core — no new entry path may construct a Coordinator directly.
6. **Enumerate the known divergences as the acceptance checklist**:
   caching parity, `--opt` parity, transitive package registration parity,
   diagnostic single-render, warning exit codes, package-name parity.

Land `../small/single-package-identity-function.md` first: it shrinks the
naming surface the core must absorb to a single function.

## What success looks like

- The five bug classes above are impossible by construction: there is one
  code path for package registration, one for cache attachment, one for
  report rendering, one for exit codes.
- No Coordinator setup code (ensurePackage loops, parse-task enqueueing,
  package-name computation) exists outside the core — `grep -rn
  "ensurePackage" src/cli src/glue` matches nothing.
- `renderCoordinatorReports` in its current form is gone; rendering the
  same report twice is a type/state impossibility, not a convention.
- Adding a feature to build orchestration (a new cache, a new package
  source, a new diagnostic channel) lands in run/test/docs/glue
  automatically.
- No call site anywhere passes a `null` cache manager except an explicit
  `--no-cache` flag path.

## How to evaluate the result

### Correctness ideal

The pipeline-parity test suite (below) is total over entry paths: every
assertion runs against check, build, run, and test, and any future entry
path is added to the same matrix or fails CI. Divergence between two entry
paths on diagnostics, exit code, cache behavior, or package resolution is
a test failure, not a user bug report.

### Performance ideal

Run-path compile times are unchanged or better — it gains caching by
default, so measure second-invocation speedup on a warm cache, which today
is zero on any path that opted out (issue 9788's repro is the benchmark).
`roc check` must not regress: the core must not force LIR-image or
interpreter machinery onto paths that do not need it (check's back half
stops after diagnostics). Cold-build times are unchanged: the core adds no
work, it deduplicates it.

## Tests to add

- **Pipeline-parity suite**: drive the SAME multi-package project (with a
  transitive URL dependency and at least one warning and one error
  variant) through `roc check`, `roc build`, `roc run`, and `roc test`;
  assert identical diagnostic text and counts, identical exit codes
  (including warning exit 2), identical package resolution, and identical
  cache behavior (second invocation hits the checked-module cache on every
  path — assert via cache stats).
- Issue 9788 repro: default app, `roc run` twice, second run must load
  checked modules from cache.
- Issue 9694 repro: `roc file.roc --opt=speed` produces and executes a
  compiled binary (not the interpreter).
- Issue 9509 repro: app → package → transitive URL package, `roc run`
  succeeds exactly when `roc build` does.
- Single-render test: a program with N diagnostics renders exactly N
  reports on the run path (PR 9759 regression test).

## Related projects

- [Single package-identity function](../small/single-package-identity-function.md)
  — land first; removes the naming skew the core must otherwise absorb.
- [Cache hardening](../small/cache-hardening.md) — the cache the unified
  core wires everywhere; hardening its edges pays off more once every
  pipeline uses it.
- [Decision-tree match compiler](../big/decision-tree-match-compiler.md)
  — the single-lowerer story pairs with this project's single-orchestrator
  story.
