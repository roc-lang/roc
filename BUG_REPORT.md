# CLI Integration Test Failures on `zig-16` Branch

**Context:** This branch is a work-in-progress migration to Zig 0.16.
All bugs below were already present on the branch before the session that
discovered them; they became visible only after fixing an earlier hang in
`src/compile/channel.zig` (which was blocking `zig build minici` from reaching
the `test-cli` step).

**Reproduce the full suite:** `zig build test-cli --summary all`

---

## Bug 1 — Echo-platform apps crash with SIGSEGV (fault addr 0x6e)

### Failing tests (18 total, both backends)

```
echo platform: hello (interpreter)
echo platform: hello (dev backend)
echo platform: multiple echo calls (interpreter)
echo platform: multiple echo calls (dev backend)
echo platform: exit ok (interpreter)
echo platform: exit ok (dev backend)
echo platform: custom error issue 9255 repro (dev backend)
echo platform: list concat with refcounted elements issue 9316 (interpreter)
echo platform: list concat with refcounted elements issue 9316 (dev backend)
echo platform: all_syntax_test.roc prints expected output (dev backend)
```

### What happens

Running any echo-platform app produces:

```
Segmentation fault (SIGSEGV) in the Roc compiler.
Fault address: 0x6e

Please report this issue at: https://github.com/roc-lang/roc/issues
```

Exit code 139.  Fault address 0x6e (110 decimal) is a near-null pointer
dereference **inside the compiler itself**, not inside a compiled Roc program.
The message comes from `src/base/stack_overflow.zig`'s `handleAccessViolation`
callback.

### Reproduce

```bash
zig-out/bin/roc test/echo/hello.roc        # crashes
zig-out/bin/roc --opt=dev test/echo/hello.roc  # crashes
```

### Where to look

- Echo apps use the `rocRunDefaultApp` code path in `src/cli/main.zig` (search
  for `rocRunDefaultApp`).  Set a breakpoint there and step forward to find the
  null dereference.
- `test/echo/hello.roc` is the minimal repro:
  ```roc
  main! = |_args| {
      echo!("Hello, World!")
      Ok({})
  }
  ```
- Fault address 0x6e = integer offset 110 into some struct — likely a field
  access on a null pointer.  Run under gdb:
  ```bash
  gdb zig-out/bin/roc
  (gdb) run test/echo/hello.roc
  # inspect $rip and the faulting instruction
  ```
- All echo tests pass for non-echo apps (`test/int/app.roc`, `test/cli/`),
  so the bug is specific to the echo-platform compilation path.

---

## Bug 2 — `roc build` + run tests report empty stdout

### Failing tests

```
roc build executable runs correctly (interpreter)
roc build --opt=dev executable runs correctly for test/int/app.roc
```

### What happens

The build step (`roc build --opt=interpreter --output=…`) succeeds (exit 0).
Running the output binary also exits 0.  But `run_result.stdout` is apparently
empty (or does not contain "SUCCESS"/"PASSED"/"ALL TESTS PASSED"), causing:

```
try testing.expect(has_success);  // roc_subcommands.zig:669
```

### Reproduce

Run these manually — they work:

```bash
zig-out/bin/roc build --opt=interpreter --output=/tmp/app_int test/int/app.roc
/tmp/app_int          # prints "ALL TESTS PASSED"

zig-out/bin/roc build --opt=dev --output=/tmp/app_dev test/int/app.roc
/tmp/app_dev          # prints "ALL TESTS PASSED"
```

The test uses `std.process.run` with `.stdin = .ignore` and captures stdout.
When run manually with piped stdout the output is present; something in the
test harness environment causes stdout to be empty.

### Where to look

- Test code: `src/cli/test/roc_subcommands.zig` lines 623–670 (interpreter)
  and 672–714 (dev).
- The test's `std.process.run` call uses `.stdin = .ignore`.  Check whether
  the interpreter shim or platform host reads from stdin during startup —
  if so, the ignored stdin might cause it to exit early without printing.
- Also check whether the output binary writes to **stderr** instead of stdout
  when stdout is not a TTY; `std.process.run` captures both separately.
- Insert `std.debug.print("stdout: {s}\n", .{run_result.stdout})` just before
  the failing assertion to see what was actually captured.
- The interpreter-shim IPC uses a shared-memory segment (look at
  `src/interpreter_shim/main.zig`).  Verify that the shim still initialises
  correctly when stdin is closed.

---

## Bug 3 — `roc test` caching does not mark results as cached

### Failing tests

```
roc test caches passing results (interpreter)
roc test caches failing results (interpreter)
roc test --verbose works from cache (interpreter)
roc test --verbose works from cache (dev)
roc test --verbose caches failure reports (interpreter)
roc test non-verbose run caches verbose failure reports for later verbose run (interpreter)
```

### What happens

A second `roc test` run on the same file is expected to print `(cached)` in
stdout.  The assertion:

```zig
try testing.expect(std.mem.find(u8, result2.stdout, "(cached)") != null);
```

fails, meaning either (a) the result was not cached, or (b) the cache hit
message format changed.

### Reproduce

```bash
ROC_CACHE_DIR=/tmp/roc_tc zig-out/bin/roc test test/int/app.roc   # first run
ROC_CACHE_DIR=/tmp/roc_tc zig-out/bin/roc test test/int/app.roc   # second run — should say (cached)
```

### Where to look

- Test code: `src/cli/test/roc_subcommands.zig` — search for `cached`.
- Cache output string: search for `"(cached)"` in `src/cli/` to find where
  the message is printed.  It may have been renamed or removed.
- Cache logic: `src/compile/cache_config.zig`, `src/compile/cache_manager.zig`,
  and the coordinator at `src/compile/coordinator.zig`.
- The `channel.zig` change (commit `c2db5d47`) replaced
  `waitUncancelable` with a 1 ms polling sleep in `recvTimeout`/`sendTimeout`.
  If the coordinator uses these for cache checks, verify the timing is still
  correct.

---

## General investigation approach

1. **Start with Bug 1** — it is the most severe (crash) and affects the most
   tests.  The echo-platform path is isolated from the `int` platform, so it
   should be straightforward to bisect.

2. **Bug 2** probably has a single root cause (stdin handling or IPC).  Add a
   debug print to the test to confirm whether `run_result.stdout` is truly
   empty before spending time on the binary itself.

3. **Bug 3** may be a simple string change in the caching output rather than a
   logic bug — check the cache-hit print statement first.

### Useful commands

```bash
# Run only the echo tests
zig build test-cli --summary all -- --test-filter "echo platform"

# Run only the build tests
zig build test-cli --summary all -- --test-filter "roc build executable"

# Run only the caching tests
zig build test-cli --summary all -- --test-filter "caches"

# Build with debug symbols and run under gdb
zig build  # produces zig-out/bin/roc with debug info
gdb zig-out/bin/roc
(gdb) run test/echo/hello.roc
```

### Branch state

Current branch: `zig-16`  
Last commit when this report was written: `1063846d`  
All fixes from the current session have been committed; these are the
remaining open failures in `zig build test-cli`.
