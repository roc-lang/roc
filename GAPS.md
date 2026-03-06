# Io Abstraction — Known Gaps

These items were intentionally deferred from the initial `Filesystem → Io` migration.
Each represents a direct `std.fs`/`std.io`/`std.debug` call in compiler-core code that
should eventually route through the injected `Io` context instead.

---

## 1. Reporting TTY detection (`src/reporting/config.zig`)

**Current code (line ~84):**
```zig
if (comptime builtin.os.tag == .freestanding) {
    break :isTty false;
} else {
    break :isTty std.fs.File.stdout().isTty();
}
```

**Gap:** `Io` is not yet threaded into the reporting layer. TTY detection should
call `io.isTty()` so callers (WASM, tests) can control colour output.

**Prerequisite:** Pass `Io` into `ReportConfig` / wherever `isTty` is evaluated at startup.

---

## 2. Eval `[dbg]` output (`src/eval/comptime_evaluator.zig`, `src/eval/dev_evaluator.zig`)

**Current code (comptime_evaluator.zig ~line 98):**
```zig
if (comptime builtin.os.tag != .freestanding) {
    var stderr_buffer: [256]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    // ...
    stderr.print("[dbg] {s}\n", .{msg_slice}) catch {};
}
```

**Gap:** `[dbg]` output and `rocExpectFailed` messages bypass `Io`. On WASM/freestanding
these are silently dropped; on native they always go to process stderr even if a caller
has injected a capturing `Io`.

**Prerequisite:** Thread `Io` through `Coordinator` → worker tasks → `Interpreter`.
This is a larger change touching the interpreter's init signature.

---

## 3. REPL stdin (`src/cli/repl.zig`)

**Current code:** reads from `std.io.getStdIn()` directly.

**Gap:** `io.readStdin()` exists in the vtable but the REPL doesn't use it. No real
impact today (REPL is CLI-only, always has a real stdin), but inconsistent.

**Prerequisite:** Thread `Io` into the REPL entry point.

---

## 4. `FileProvider` source-reading abstraction

**Location:** `src/compile/compile_package.zig`

**Current:** A separate, minimal vtable (`read: *const fn`) for reading module source files.
Lives alongside `Io` rather than being folded into it.

**Gap:** Conceptually `FileProvider.read` is a subset of `Io.readFile`. Unifying them
would reduce the number of context pointers threaded through `PackageEnv` / `Coordinator`.

**Prerequisite:** Evaluate whether the threading cost is worth the simplification.
