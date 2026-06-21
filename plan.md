# Roc Check/Test Watch Plan

## Goal

Add `roc check --watch` and `roc test --watch` as a standalone PR. This PR does
not touch the default `roc` execution path or dev-shim hot-code-loading.

## Design Checklist

- [x] Add `--watch` parsing to `roc check` and `roc test`.
- [x] Wire the already-parsed `--main` option for `roc check` and `roc test`.
- [x] Run watch mode through a child-process supervisor so existing one-shot
  check/test exit behavior stays isolated from the long-lived watch parent.
- [x] Add explicit file-import dependencies to `ModuleEnv`.
- [x] Store file-import dependencies as source-relative strings only.
- [x] Include file-import content digests in checked-module cache keys.
- [x] Serialize and deserialize file-import dependencies with cached `ModuleEnv`
  data.
- [x] Collect exact watch inputs from `BuildEnv` after each check/test run.
- [x] Include root files, transitive Roc modules, filesystem package/platform
  files actually read, and file imports.
- [x] Exclude URL package files from watch inputs.
- [x] Include missing file-import paths in watch inputs.
- [x] Watch directories but filter events against the exact file set.
- [x] For missing paths whose parent does not exist, watch the nearest existing
  ancestor.
- [x] Hash watched file bytes and ignore metadata-only changes such as `touch`.
- [x] Debounce filesystem event bursts for 25ms before deciding whether to rerun.
- [x] Refresh the watch set after every completed run, successful or failed.
- [x] Cancel any in-progress watch rerun as soon as a newer byte-changing event
  arrives.
- [x] Print a separator before rerun output instead of clearing the terminal.
- [x] Support all existing `roc test --opt` modes in watch mode.
- [x] Keep normal non-watch `roc check` and `roc test` exit behavior unchanged.

## Implementation Notes

- `BuildEnv` is the shared source of truth for watch inputs because both
  commands already compile through it and coordinator results are transferred
  there after compilation.
- Watch inputs are explicit compiler output. Watch code must not scan source
  text to rediscover module imports or file imports.
- File-import paths are relative to the module's source directory. They must not
  be realpathed or made absolute before storage in `ModuleEnv`.
- The watch supervisor owns long-running process behavior. The one-shot
  check/test functions stay reusable and do not know about filesystem events.
- Rerun cancellation should be implemented at the process/task boundary used by
  watch mode. Completed outputs are printed; cancelled runs are discarded.

## Success Criteria

- [x] `roc check --watch` runs immediately, prints diagnostics or success output,
  and waits for relevant source changes.
- [x] `roc test --watch` runs immediately, prints test output, and waits for relevant
  source changes.
- [x] Touching a watched file without changing bytes does not rerun or reprint.
- [x] Changing a watched Roc module reruns and refreshes diagnostics/test output.
- [x] Changing a watched file import reruns and refreshes diagnostics/test output.
- [x] Creating a previously missing file import reruns.
- [x] Adding a new import to an already watched module reruns and refreshes the
  watch set to include the new module/file.
- [x] URL package files are not included in the watch input set.
- [x] Filesystem package/platform files are included only when actually read by the
  current compile.
- [x] A new byte-changing event during a rerun cancels the in-progress rerun and
  starts a newer one.
- [x] Existing non-watch `roc check` and `roc test` behavior and exit codes remain
  unchanged.
- [x] Relevant Zig unit tests and CLI tests pass.

## Verification Evidence

- `zig build run-test-zig-cli-main --summary all --color off`
- `zig build run-test-zig-watch-cli --summary all --color off`
- `zig build run-test-zig-module-can --summary all --color off`
- `zig build run-test-zig-module-check --summary all --color off`
- `zig build run-test-zig-module-compile --summary all --color off`
- `zig build run-check-type-checker-patterns --summary all --color off`
- `zig build run-check-tidy --summary all --color off`
- `zig build run-check-zig-lints --summary all --color off`
- repo stage-word audit check
- `zig build build-ci --summary all --color off`
- `zig build run-test-cli --summary all --color off`
- `zig build minici --summary all --color off`

## Review Follow-Up Plan

These items came out of the whole-branch review and should be completed before
committing and pushing the PR branch.

### Required Fixes

- [x] Ban absolute file imports in general.
  - Reject POSIX absolute paths such as `/tmp/data.txt`.
  - Reject Windows absolute paths such as `C:\tmp\data.txt` and UNC-style paths.
  - Report a dedicated canonicalization diagnostic before recording a
    `ModuleEnv.FileDependency`.
  - Preserve the invariant that serialized file dependencies are source-relative
    strings only.
- [x] Ensure absolute file imports do not appear in watch inputs.
  - A rejected absolute file import should keep the root `.roc` file in the
    watch set.
  - It should not add the absolute imported path to `ModuleEnv.file_dependencies`
    or the child watch-input file.
- [x] Close the watch refresh race.
  - Do not let edits between snapshot creation and watcher activation become the
    quiet baseline.
  - Do not let edits made after a child compile but before parent-side watch
    refresh become the quiet baseline.
  - If bytes changed during refresh, immediately schedule another rerun instead
    of waiting for a future filesystem event.
- [x] Keep cancellation behavior intact.
  - A byte-changing event during an in-progress rerun still kills the current
    child and starts a newer run.
  - Output from cancelled children remains discarded.

### Tests To Add

- [x] Canonicalization test for absolute file imports.
  - Assert the diagnostic is the new absolute-import diagnostic.
  - Assert `ModuleEnv.file_dependencies` stays empty for the rejected import.
- [x] CLI/watch-input regression for absolute file imports.
  - Run `roc check --watch-inputs-file=...` on a file that imports an absolute
    path.
  - Assert the command reports the absolute-import diagnostic.
  - Assert the watch-input file contains the root source path but not the
    absolute imported path.
- [x] Watch refresh race unit coverage around `refreshWatchState`.
  - Simulate bytes changing during refresh and assert the caller can detect that
    a rerun is required.
  - Cover both newly discovered watch inputs and an existing watch set refresh.
- [x] CLI supervisor coverage for completed-run watch refresh.
  - Exercise that a byte change occurring between child completion and refreshed
    state causes another rerun.

### Success Criteria

- [x] Absolute file imports are rejected consistently on all supported host path
  syntaxes.
- [x] No absolute file-import path can be serialized in `ModuleEnv` or used in a
  checked-module cache key.
- [x] Watch mode never adopts changed bytes as the new baseline without
  rerunning check/test.
- [x] The existing verified commands still pass, including:
  - `zig build run-test-zig-cli-main --summary all --color off`
  - `zig build run-test-zig-watch-cli --summary all --color off`
  - `zig build run-test-zig-module-can --summary all --color off`
  - `zig build run-test-zig-module-check --summary all --color off`
  - `zig build run-test-zig-module-compile --summary all --color off`
  - `zig build run-check-tidy --summary all --color off`
  - `zig build run-check-zig-lints --summary all --color off`
  - `zig build minici --summary all --color off`

## Hot Code Loading Plan

This extends the default `roc` dev-shim execution path, not the
`roc check --watch` / `roc test --watch` supervisor and not any nonexistent
run subcommand.

### Design Goals

- [x] Keep the compiler process alive while the host shim child process runs.
- [x] Continue sending only dev backend `RunImage` bytes through shared memory.
  - LIR must not be allocated into the `SharedMemoryAllocator` on this path.
  - Shared memory is for `RunImage` bytes the host shim needs to run or reload.
- [x] Spawn the host shim child first, then set up watches in the compiler
  parent.
- [x] Tear down watches by exiting the compiler parent when the host shim child
  exits.
- [x] Reuse explicit watch-input collection from the check/test watch work.
  - Do not scan source text to rediscover imports.
  - Use `BuildEnv.collectWatchInputs` or its successor as the single source of
    truth.
- [x] Watch the exact logical file set from the latest successful or diagnostic
  compile.
  - App source file.
  - Transitive Roc modules.
  - File imports.
  - Filesystem package and platform files actually read by the current compile.
  - Missing file imports, so later creation triggers a rebuild.
  - Exclude URL packages/platforms because their source identity is immutable.
- [x] Preserve source-relative file-import dependency storage in `ModuleEnv`.
  - Cached modules must contribute file-import watch inputs by joining module
    source directory with the cached relative dependency string.
  - Never store realpaths, symlink-resolved paths, cwd-dependent paths, or
    absolute paths in checked-module cache keys.
- [x] Refresh the watch set after every completed rebuild.
  - A source edit can add or remove imports.
  - A source edit can switch between filesystem and URL dependencies.
  - A missing file import can become present.
- [x] Ignore metadata-only changes.
  - Debounce event bursts for 25ms.
  - Re-read watched file bytes and compare source state before rebuilding.
- [x] Cancel stale rebuilds.
  - If a byte-changing event arrives while a rebuild is in progress, kill that
    rebuild immediately.
  - Discard captured stdout/stderr text and uncommitted `RunImage` bytes from cancelled rebuilds.
  - Start a newer rebuild from the latest watched file state.
- [x] Keep the previous `RunImage` active on rebuild failure.
  - Report diagnostics from the failed rebuild.
  - Do not send a replacement image to the host shim unless compilation and
    `RunImage` construction succeed.
- [x] Refresh the host shim on successful rebuild.
  - Write replacement `RunImage` bytes to shared memory.
  - Notify the host shim that a new `RunImage` is available.
  - Define an acknowledgement path so the compiler knows whether the host
    accepted or rejected the hot-load update.

### Protocol Work

- [x] Extend the compiler-to-host-shim protocol with a hot-load update message.
  - Include enough metadata for the host to validate the image before swapping.
  - Keep image bytes separate from control messages.
- [x] Define host-shim behavior for receiving a replacement image.
  - Load/prepare the new image.
  - Swap to the new code at a well-defined safe point.
  - Keep using the old image if validation or load fails.
- [x] Define lifecycle behavior.
  - Compiler exits when the host shim child exits.
  - Compiler kills in-progress rebuild work before exiting.
  - Host shim handles compiler-parent exit without corrupting the running
    process.
- [x] Define diagnostics and stdout/stderr text behavior.
  - Initial compile diagnostics remain normal command stdout/stderr text.
  - Rebuild diagnostics print with a separator.
  - Successful reload messages are concise and should not clear the terminal.

### Implementation Checklist

- [x] Reuse watch-loop path collection, directory watcher setup, byte-state
  snapshotting, debounce, metadata filtering, and child cancellation logic from
  check/test watch mode where applicable.
  - Exact path collection.
  - Directory watcher setup.
  - Byte-state snapshotting.
  - Debounce and metadata-only filtering.
  - Stale-rebuild cancellation.
- [x] Add a long-lived parent mode to the default `roc` dev-shim execution path.
- [x] Split initial execution from later rebuilds.
  - Initial compile must still launch the app normally.
  - Rebuilds must only produce replacement `RunImage` bytes and notify the host.
- [x] Add rebuild task management.
  - One active rebuild at a time.
  - New byte-changing events cancel the active rebuild.
  - Completed rebuilds refresh the watch inputs.
- [x] Add hot-load shared-memory control-block updates.
  - Allocate shared memory for `RunImage` bytes only.
  - Send image size, target metadata, and any required handles/descriptors.
  - Clean up superseded shared-memory objects after host acknowledgement.
- [x] Update the host shim.
  - Listen for hot-load update messages while the app is running.
  - Validate and map the replacement `RunImage`.
  - Swap execution targets at the agreed safe point.
  - Report acceptance/failure to the compiler parent.
- [x] Preserve non-watch/default behavior when hot loading is not enabled.
  - Existing one-shot default `roc` dev-shim execution should remain unchanged
    unless hot loading is requested or enabled by design.
- [x] Add CLI/help once the activation model is decided.
  - `roc --watch` enables hot loading for the default dev-shim execution path.
  - Avoid referring to a run subcommand; this path is the default `roc` command.

### Resolved Limitations

- [x] Support `roc --watch` hot loading on Windows.
- [x] Support headerless default apps in `roc --watch`.
- [x] Have the compiler parent report host-shim acknowledgement status instead
  of only defining the shared-memory acknowledgement path.

### Tests To Add

- [x] Unit tests for hot-load control block store/acknowledge state.
- [x] Unit tests that replacement `RunImage` bytes are the only payload allocated
  in shared memory.
- [x] Integration test for initial dev-shim execution still working.
- [x] Integration test for a source edit causing recompilation and host reload.
- [x] Integration test for a failed rebuild preserving the previously running
  image.
- [x] Integration test for a second edit cancelling an in-progress rebuild.
- [x] Integration test for file-import edits triggering reload.
- [x] Integration test for adding/removing imports refreshing the watch set.
- [x] Integration test that URL package files are excluded from watches.
- [x] Integration test that filesystem package/platform edits trigger reload
  when those files are part of the current compile.
- [x] Lifecycle test that the compiler parent exits when the host shim child
  exits.
- [x] Regression test that metadata-only touches do not rebuild or reload.

### Verification Completed

- [x] Manual `roc --watch` smoke with a temporary platform whose host calls
  `roc_main` twice. The first call returned `1`, an app edit stored
  generation 2, and the second call returned `2` without restarting the host.
- [x] Existing focused Zig test suites for CLI, IPC, watcher, backend, and watch
  CLI continue to pass.
- [x] Tidy and Zig lint checks pass, including the `src/` `anyerror` ban.
- [x] Full CLI test runner passes with hot-load integration cases.
- [x] `zig build -Dtarget=x86_64-windows-gnu roc --summary all --color off`
  compiles the Windows hot-load handle path.
- [x] `zig build minici --summary all --color off` passes.

### Success Criteria

- [x] Default `roc` dev-shim execution can reload changed code without restarting
  the host shim process.
- [x] Failed rebuilds report diagnostics and leave the old `RunImage`
  running.
- [x] No LIR is allocated into shared memory on the hot-code-loading path.
- [x] Watch inputs are explicit compiler data and are refreshed after each
  completed rebuild.
- [x] Byte-unchanged filesystem events do not rebuild.
- [x] Stale rebuilds are killed promptly when newer byte-changing events arrive.
- [x] Host shim child exit cleanly terminates the compiler parent and all watches.
- [x] The current check/test watch behavior and non-hot-load execution behavior
  continue to pass their existing tests.
