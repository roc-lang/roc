# Zig 0.16 Migration — Remaining Issues

## TL;DR

`zig build minici` is mostly green — `fmt`, `zig lints`, `tidy`, `check-test-wiring`,
`zig build`, Builtin.roc formatting, `snapshot`, checkfx, and the core module-test
pipeline all compile. The remaining blocker is a single **architectural regression**
introduced by the 0.16 migration:

> The shims (`libroc_interpreter_shim.a`, `libroc_dev_shim.a`) transitively link
> libc and reference compiler_rt symbols. That breaks Roc's promise that user
> programs are libc- and compiler_rt-independent.

Symptom when linking a user Roc app against `test/fx/platform` or `test/int/platform`:

```
ld.lld: error: undefined symbol: statx
  >>> referenced by std/Io/Threaded.zig:3893  (Io.Threaded.fileStatLinux)
  >>>              main.o in libroc_shim.a

ld.lld: error: undefined symbol: __modti3
  >>> referenced by std/Io/Threaded.zig:14253 (Io.Threaded.timestampToPosix)
  >>>              roc_builtins.o / host.o
```

The bundled platform `libc.a` is intentionally minimal and doesn't ship `statx`,
and user programs should never need compiler_rt's 128-bit math helpers.

---

## Root Cause

Commit `df29a20f08` ("WIP: Zig 0.16 migration - rename Io to RocIo and thread
std.Io through codebase") introduced into each shim main.zig:

```zig
// src/interpreter_shim/main.zig:58
// src/dev_shim/main.zig:34
var app_std_io: std.Io = std.Io.Threaded.global_single_threaded.io();
```

`app_std_io` is passed to `std.Thread.Mutex.lockUncancelable`/`unlock` and
`SharedMemoryAllocator.fromCoordination` — both of which gained a mandatory
`std.Io` parameter in 0.16.

Referencing `std.Io.Threaded.global_single_threaded.io()` instantiates the full
`std.Io.Threaded` vtable, which pulls the entire file/network/timestamp/stat
implementation into the compile graph. On Linux-musl:

- `fileStatLinux` / `dirStatFileLinux` → `std.c.statx` (because `statx_use_c`
  returns `true` on musl) → `U statx`
- `timestampToPosix` → 128-bit `@mod`/`@divTrunc` → `__modti3`, `__divti3`

Before 0.16 the shim used `std.Thread.Mutex.lock()` and `SharedMemoryAllocator
.fromCoordination(allocator, page_size)` — neither took an `Io`, neither pulled
in `std.Io.Threaded`.

## Fix Direction

Need a minimal `std.Io` for the shim that only implements what mutex/futex and
mmap-style shared memory actually use. Options, roughly in increasing invasiveness:

1. **Thin wrapper Io.** Audit what methods `std.Thread.Mutex.lockUncancelable`,
   `std.Thread.Mutex.unlock`, and `ipc.SharedMemoryAllocator.fromCoordination`
   call on the Io vtable. Likely just `futexWait`/`futexWake` for mutex, and
   a couple of mmap-adjacent calls for shm. Implement a custom `std.Io` whose
   other vtable entries `@panic`/`unreachable`. This keeps `std.Io.Threaded`
   out of the shim's compile graph entirely.
2. **Drop the Io parameter at our boundary.** Rework `ipc.SharedMemoryAllocator
   .fromCoordination` so it doesn't take an `Io` — it can call `std.os.linux`
   syscalls directly for shared memory. Similarly, replace `std.Thread.Mutex`
   with a direct futex wrapper for the shim's single PlatformMutex.
3. **Two compile modes.** Move the `app_std_io` line behind a `comptime` flag
   and provide a syscall-direct mutex implementation when building the shim.
   Everything else (roc exe, tests) keeps using `std.Io.Threaded`.

(1) is probably the cleanest. (2) is less code but pushes into the ipc module.

Do **not**:
- Stub `statx` into `libhost.a` of each platform (I prototyped this — it works
  mechanically but hides the real regression and the stub misses callers that
  link against other platforms).
- Flip `createTestPlatformHostLib`'s `bundle_compiler_rt` on for musl targets
  (same objection: paper-over).
- Regenerate the bundled `libc.a` files with a newer musl. The bundled libc is
  a platform-provided contract; expanding it to satisfy compiler-side additions
  is the wrong direction.

See also: `~/.claude/projects/-home-lbw-Documents-Github-roc/memory/feedback_roc_libc_independence.md`.

---

## What's Already Fixed on This Branch (uncommitted)

| Area | Change | File |
|---|---|---|
| std.posix removals | `std.posix.close`/`inotify_init1`/`inotify_add_watch` → `std.os.linux.*` with manual errno handling. `std.Io.Dir.openDirAbsolute`/`dir.close`/iterator `next` gained required `io` parameter (currently supplied via `std.Io.Threaded.global_single_threaded.io()` locally — TODO: thread io through the Watcher struct). | `src/watch/watch.zig` |
| Darwin `std.c._NSGetExecutablePath` | Wrapped in `switch (comptime builtin.os.tag)` with Linux (`readLinkAbsolute /proc/self/exe`) and Windows (`peb.ImagePathName`) branches. 0.16's comptime assertion on `std.c.darwin` now passes on non-Darwin hosts. | `src/cli/linker.zig` |
| Test-level libc linkage | Expanded `createModuleTests` link_libc to `true` for all module tests (was just `ipc`, `bundle`, `eval`, `repl`, `sljmp`). Any module whose source touches `std.c` or transitively imports one that does now compiles its tests. | `src/build/modules.zig` |
| Exe-level libc linkage | Added `.link_libc = true` explicitly to `builtin_compiler_exe` (via `ctx.CoreCtx`), `test_runner_exe` (direct `std.c` use), `roc_subcommands_test`, `glue_test`, `fx_platform_test`. | `build.zig` |

## Reverted (attempted workarounds — do not re-apply)

- `.link_libc = false` on shim_lib root module (doesn't override transitive
  `.link_libc = true` from `bundle → zstd` import chain, and breaks other
  modules that legitimately need libc on shim's compile graph).
- `@export(&statxCompat, ...)` from `test/fx/platform/host.zig` forwarding to
  the `SYS_statx` syscall with manual errno.
- `lib.bundle_compiler_rt = … or is_linux_musl` in `createTestPlatformHostLib`
  to drag `__modti3`/`__divti3` into each musl `libhost.a`.

---

## Other Follow-Ups Spotted (not blockers for `minici`)

- **ci_zig.yml still pins zig 0.15.2** for the matrix jobs and the
  `check-once` nix/benchmark jobs are on 0.16.0. Once the shim fix lands and CI
  is green on 0.16, flip the remaining `version: 0.15.2 # TODO ZIG 16` pins in
  `.github/workflows/ci_zig.yml`.
- **watch.zig io threading.** The `std.Io.Threaded.global_single_threaded.io()`
  call sites the sub-agent added as part of the posix-removal fix violate the
  spirit of `ci/tidy.zig`'s ban (the ban is scoped to core modules, so tidy
  passes today). Thread a real `std.Io` through the `Watcher` struct later.
- **DWARF bug re-check.** `ci_zig.yml` has `TODO ZIG 16: re-check if DWARF bug
  is fixed in 0.16` around the ARM64-only kcov coverage job. Should retest.

---

## How to Reproduce Today

From repo root:

```bash
rm -rf .zig-cache src/cli/libroc_*.a
zig build                        # should succeed
zig build test-cli 2>&1 | tail   # fails with statx / __modti3 symbols
```

Or the full pipeline:

```bash
zig build minici                 # fails at the `zig build test-cli` sub-step
```

The earlier sub-steps (`fmt`, `zig lints`, `tidy`, snapshot, module tests) all
pass.
