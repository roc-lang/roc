# Fix Temporary File Name Collision Risk

## Problem

The temporary file naming scheme uses a millisecond timestamp as a counter, which could cause collisions if:

1. Two processes run at the same millisecond
2. The same process creates multiple temp files within a millisecond (the code does increment a counter, but starts from the timestamp)

## Location

`src/cli/llvm_eval.zig`, lines 42-46:

```zig
var counter: u64 = @intCast(@as(u64, @bitCast(std.time.milliTimestamp())));
const obj_path = std.fmt.allocPrint(allocator, "/tmp/roc_llvm_{d}.o", .{counter}) catch return error.OutOfMemory;
defer allocator.free(obj_path);
counter += 1;
const exe_path = std.fmt.allocPrint(allocator, "/tmp/roc_llvm_{d}", .{counter}) catch return error.OutOfMemory;
```

And in `src/eval/llvm_evaluator.zig`, line 520-527:

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

The evaluator version is better (uses an instance counter), but the CLI version initializes from timestamp each time.

## Why This Matters

While the collision risk is low in practice, it can cause confusing failures:

- Two `roc repl` sessions started simultaneously might overwrite each other's files
- In CI environments running parallel tests, collisions are more likely
- Race conditions are notoriously hard to debug when they occur

## What Needs to Change

### Option 1: Use Process ID + Counter (Simple)

Include the process ID to ensure uniqueness across processes:

```zig
const pid = std.os.linux.getpid(); // or std.c.getpid() for cross-platform
var counter: u64 = 0;

fn getUniquePath(allocator: Allocator, counter: *u64, extension: []const u8) ![]const u8 {
    counter.* += 1;
    return std.fmt.allocPrint(
        allocator,
        "/tmp/roc_llvm_{d}_{d}{s}",
        .{ pid, counter.*, extension },
    );
}
```

### Option 2: Use Zig's tmpFile (Recommended)

Zig's standard library can create unique temporary files:

```zig
const tmp_dir = std.fs.openDirAbsolute("/tmp", .{}) catch return error.TempDirError;
defer tmp_dir.close();

// Create a unique temp file
var tmp_file = tmp_dir.createFile("roc_llvm_XXXXXX", .{}) catch return error.TempFileError;
// ... or use makePath for directories
```

Actually, Zig doesn't have mkstemp-style random naming built in. But you can use:

```zig
const std = @import("std");

fn createUniqueTempPath(allocator: Allocator, prefix: []const u8, suffix: []const u8) ![]const u8 {
    var prng = std.rand.DefaultPrng.init(@intCast(std.time.nanoTimestamp()));
    const random = prng.random();

    var random_bytes: [8]u8 = undefined;
    random.bytes(&random_bytes);

    // Convert to hex string for filename-safe characters
    var hex_str: [16]u8 = undefined;
    _ = std.fmt.bufPrint(&hex_str, "{x:0>16}", .{std.mem.readInt(u64, &random_bytes, .little)}) catch unreachable;

    return std.fmt.allocPrint(allocator, "/tmp/{s}_{s}{s}", .{ prefix, hex_str, suffix });
}
```

### Option 3: Use Atomic Counter with PID

For the LlvmEvaluator which already has a counter field:

```zig
pub const LlvmEvaluator = struct {
    // ... existing fields ...
    counter: u64,
    pid: std.os.pid_t,

    pub fn init(allocator: Allocator) Error!LlvmEvaluator {
        return LlvmEvaluator{
            // ... other fields ...
            .counter = 0,
            .pid = std.c.getpid(),
        };
    }

    fn getTempPath(self: *LlvmEvaluator, extension: []const u8) Error![]const u8 {
        self.counter += 1;
        return std.fmt.allocPrint(
            self.allocator,
            "/tmp/roc_llvm_{d}_{d}{s}",
            .{ self.pid, self.counter, extension },
        ) catch return error.OutOfMemory;
    }
};
```

## Additional Consideration: Cleanup on Crash

Currently, temp files are cleaned up with `defer`. But if the process crashes, temp files will be left behind. Consider:

1. Using a subdirectory like `/tmp/roc-{pid}/` that can be cleaned up on startup
2. Adding a cleanup routine that removes old temp files on evaluator init
3. Using the OS temp file facilities that auto-cleanup (though Zig doesn't expose these directly)

## Files to Modify

- `src/cli/llvm_eval.zig` - Fix the temp file naming
- `src/eval/llvm_evaluator.zig` - Add PID to temp file naming

## Testing

- Run multiple instances of the REPL simultaneously and verify no conflicts
- Run snapshot tests in parallel (if possible) to stress-test uniqueness
- Verify temp files are still cleaned up properly after execution

## Commit Guidelines

Commit your changes frequently as you make progress. However, **never commit any files in the `planning/` directory** - these planning documents are for reference only and should not be checked into version control.
