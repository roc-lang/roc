# Document and Track LLD Global State Issue

## Problem

LLD's Mach-O linker has global state that gets corrupted when invoked multiple times in the same process. This is currently worked around by using JIT compilation for the snapshot tests, but the underlying issue remains unresolved for the AOT (ahead-of-time) compilation path.

## Current Workaround

`src/llvm_compile/compile.zig`, lines 6-10:

```zig
//! This approach avoids the LLD linker entirely, which is important because
//! LLD's Mach-O port has global state issues that cause corruption when
//! invoked multiple times in the same process.
```

The snapshot tests run thousands of evaluations, so using the linker for each would trigger this corruption. The JIT approach compiles and executes code in-memory without invoking LLD.

## Why This Matters

1. **`roc build` will need LLD**: The AOT compilation path in `src/cli/llvm_eval.zig` does use LLD (via `LinkMachO`, `LinkELF`, `LinkCOFF`). If users run `roc build` multiple times in quick succession, or if there's any internal retry logic, the corruption could occur.

2. **Testing limitations**: The current setup means AOT compilation is less tested than JIT compilation.

3. **Future multi-module builds**: If Roc ever compiles multiple modules in a single process and links them separately, this would trigger the issue.

## Background on LLD Global State

LLD (LLVM's linker) was originally designed as a standalone command-line tool. Its Mach-O port (ld64.lld) has static/global variables that:

- Accumulate state across invocations
- Don't get properly reset between runs
- Can cause crashes or incorrect output on subsequent calls

This is a known issue in the LLVM project. Some references:
- LLVM has `lld::mach_o::link()` which attempts to be reentrant but has issues
- The ELF linker (ld.lld) has similar but less severe issues
- The COFF linker (lld-link) is generally better about this

## Potential Solutions

### Solution 1: Process Isolation (Simple but Slow)

Fork a new process for each LLD invocation:

```zig
fn linkWithLldIsolated(allocator: Allocator, args: []const []const u8) !void {
    var child = std.process.Child.init(.{
        .allocator = allocator,
        .argv = &.{ "ld64.lld" } ++ args,
    });
    child.spawn() catch return error.LinkingFailed;
    const result = child.wait() catch return error.LinkingFailed;
    if (result.Exited != 0) return error.LinkingFailed;
}
```

This avoids the global state issue but adds process spawn overhead.

### Solution 2: Use System Linker for AOT

On macOS, use the system `ld` instead of `ld64.lld`:

```zig
fn linkWithSystemLinker(allocator: Allocator, obj_path: []const u8, exe_path: []const u8) !void {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "ld", "-o", exe_path, obj_path, "-lSystem" },
    });
    // ...
}
```

Pros: Avoids LLD entirely
Cons: Different linker behavior, may not be available in all environments

### Solution 3: Single-Use LLD Wrapper

Create a small executable that links exactly once then exits:

```zig
// roc-linker.zig - standalone binary
pub fn main() !void {
    // Read args, call LLD once, exit
    const success = llvm_bindings.LinkMachO(...);
    std.process.exit(if (success) 0 else 1);
}
```

Then call this from the main Roc process:

```zig
fn linkWithLld(args: [][]const u8) !void {
    var child = std.process.Child.init(.{
        .argv = &.{ "roc-linker" } ++ args,
    });
    // ...
}
```

### Solution 4: Monitor LLVM Upstream

Track LLVM issues related to LLD reentrancy. This may be fixed in future LLVM versions.

## What to Document

Create or update documentation to:

1. **Explain the limitation**: Users should know that rapid repeated `roc build` calls might fail on macOS

2. **Track the issue**: Note which LLVM version is being used and whether newer versions fix it

3. **Document the workaround**: If process isolation is implemented, document the performance implications

## Files to Create/Modify

1. **Add to `LLVM_BACKEND_STATUS.md`**: Document this as a known limitation

2. **Consider `src/cli/llvm_eval.zig`**: May need to add process isolation for the linking step

3. **Add integration tests**: If possible, add a test that invokes linking multiple times to catch regressions

## Investigation Needed

Before implementing a fix, investigate:

1. Does the ELF linker (`ld.lld`) have the same issue on Linux?
2. Does the COFF linker (`lld-link`) have the same issue on Windows?
3. What's the minimum number of invocations before corruption occurs?
4. Does LLVM 18/19 improve on this?

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
