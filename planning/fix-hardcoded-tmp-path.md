# Fix Hardcoded /tmp Path

## Problem

The LLVM backend hardcodes `/tmp` as the temporary directory path. This works on Unix-like systems but will fail on Windows, which uses a different temporary directory (typically `C:\Users\<username>\AppData\Local\Temp` accessed via the `TEMP` environment variable).

## Locations

### Location 1: `src/eval/llvm_evaluator.zig`, line 524

```zig
fn getTempPath(self: *LlvmEvaluator, extension: []const u8) Error![]const u8 {
    self.counter += 1;
    return std.fmt.allocPrint(
        self.allocator,
        "/tmp/roc_llvm_{d}{s}",
        .{ self.counter, extension },
    ) catch return error.OutOfMemory;
}
```

### Location 2: `src/cli/llvm_eval.zig`, lines 42-46

```zig
var counter: u64 = @intCast(@as(u64, @bitCast(std.time.milliTimestamp())));
const obj_path = std.fmt.allocPrint(allocator, "/tmp/roc_llvm_{d}.o", .{counter}) catch return error.OutOfMemory;
defer allocator.free(obj_path);
counter += 1;
const exe_path = std.fmt.allocPrint(allocator, "/tmp/roc_llvm_{d}", .{counter}) catch return error.OutOfMemory;
```

## How the Rust Backend Handles This

The Rust backend uses the standard library's cross-platform temp directory function:

```rust
// crates/compiler/gen_llvm/src/llvm/build.rs:5430
Some(&std::env::temp_dir().join("test.ll")),
```

`std::env::temp_dir()` returns the appropriate temp directory for the current OS.

## What Needs to Change

Zig's standard library provides cross-platform temp directory access. Use one of these approaches:

### Option 1: Use `std.fs.tmpDir()` (Recommended)

Zig's `std.fs` module has a `tmpDir()` function that opens a handle to a temporary directory:

```zig
const tmp_dir = std.fs.tmpDir(.{});
defer tmp_dir.close();
```

However, for generating file paths (not opening handles), you may need to get the path as a string.

### Option 2: Use Environment Variables

```zig
const tmp_path = std.process.getEnvVarOwned(allocator, "TMPDIR") catch |err| switch (err) {
    error.EnvironmentVariableNotFound => blk: {
        // Try Windows-style TEMP variable
        break :blk std.process.getEnvVarOwned(allocator, "TEMP") catch |e| switch (e) {
            error.EnvironmentVariableNotFound => "/tmp", // Fallback for Unix
            else => return error.OutOfMemory,
        };
    },
    else => return error.OutOfMemory,
};
```

### Option 3: Use `std.fs.path` with Platform Detection

```zig
const tmp_base = switch (builtin.os.tag) {
    .windows => std.process.getEnvVarOwned(allocator, "TEMP") catch "C:\\Windows\\Temp",
    else => "/tmp",
};
```

## Recommended Implementation

Create a helper function that can be shared:

```zig
fn getTempDirectory(allocator: Allocator) ![]const u8 {
    // Try TMPDIR first (Unix convention)
    if (std.process.getEnvVarOwned(allocator, "TMPDIR")) |path| {
        return path;
    } else |_| {}

    // Try TEMP (Windows convention)
    if (std.process.getEnvVarOwned(allocator, "TEMP")) |path| {
        return path;
    } else |_| {}

    // Try TMP (Windows alternative)
    if (std.process.getEnvVarOwned(allocator, "TMP")) |path| {
        return path;
    } else |_| {}

    // Fallback to /tmp for Unix-like systems
    return allocator.dupe(u8, "/tmp");
}
```

Then use it like:

```zig
const tmp_dir = try getTempDirectory(allocator);
defer allocator.free(tmp_dir);
const obj_path = try std.fmt.allocPrint(allocator, "{s}/roc_llvm_{d}.o", .{tmp_dir, counter});
```

## Files to Modify

- `src/eval/llvm_evaluator.zig` - Update `getTempPath()` function
- `src/cli/llvm_eval.zig` - Update temp file path generation

## Testing

- On macOS/Linux: Verify temp files are still created in `/tmp` or `$TMPDIR`
- On Windows: Verify temp files are created in `%TEMP%` directory
- Verify temp files are cleaned up after use (the existing `defer` cleanup should still work)

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
