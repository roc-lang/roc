///! Platform host that tests effectful functions writing to stdout and stderr.

const std = @import("std");
const builtins = @import("builtins");

/// Host environment - contains our arena allocator
const HostEnv = struct {
    arena: std.heap.ArenaAllocator,
};

/// Roc allocation function
fn rocAllocFn(roc_alloc: *builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.arena.allocator();

    const log2_align = std.math.log2_int(u32, @intCast(roc_alloc.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);

    const result = allocator.rawAlloc(roc_alloc.length, align_enum, @returnAddress());

    roc_alloc.answer = result orelse {
        @panic("Host allocation failed");
    };
}

/// Roc deallocation function
fn rocDeallocFn(roc_dealloc: *builtins.host_abi.RocDealloc, env: *anyopaque) callconv(.c) void {
    _ = roc_dealloc;
    _ = env;
    // NoOp as our arena frees all memory at once
}

/// Roc reallocation function
fn rocReallocFn(roc_realloc: *builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.c) void {
    _ = roc_realloc;
    _ = env;
    @panic("Realloc not implemented in this example");
}

/// Roc debug function
fn rocDbgFn(roc_dbg: *const builtins.host_abi.RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(roc_expect: *const builtins.host_abi.RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = env;
    const message = roc_expect.utf8_bytes[0..roc_expect.len];
    std.debug.print("ROC EXPECT FAILED: {s}\n", .{message});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    @panic(message);
}

// External symbols provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc__writeToStdout(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;
extern fn roc__writeToStderr(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

// OS-specific entry point handling
comptime {
    // Export main for all platforms
    @export(&main, .{ .name = "main" });

    // Windows MinGW/MSVCRT compatibility: export __main stub
    if (@import("builtin").os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

// Windows MinGW/MSVCRT compatibility stub
// The C runtime on Windows calls __main from main for constructor initialization
fn __main() callconv(.c) void {}

// C compatible main for runtime
fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = argc;
    _ = argv;
    platform_main() catch |err| {
        std.fs.File.stderr().deprecatedWriter().print("HOST ERROR: {s}\n", .{@errorName(err)}) catch unreachable;
        return 1;
    };
    return 0;
}

/// RocStr type matching Roc's internal string representation
const RocStr = extern struct {
    bytes: ?[*]u8,
    len: usize,
    capacity: isize,

    /// Create a RocStr from a Zig slice (small string optimization aware)
    fn fromSlice(slice: []const u8, ops: *builtins.host_abi.RocOps) RocStr {
        const len = slice.len;

        // Small string optimization: strings <= 23 bytes are stored inline
        if (len <= 23) {
            var result = RocStr{
                .bytes = null,
                .len = len,
                .capacity = -1, // Negative capacity indicates small string
            };

            // Copy bytes into the inline storage (stored in the bytes pointer space)
            const dest: [*]u8 = @ptrCast(&result.bytes);
            @memcpy(dest[0..len], slice);

            return result;
        }

        // Large string: allocate on heap
        var roc_alloc = builtins.host_abi.RocAlloc{
            .length = len,
            .alignment = @alignOf(u8),
            .answer = undefined,
        };

        ops.roc_alloc(&roc_alloc, ops.env);

        const bytes: [*]u8 = @ptrCast(@alignCast(roc_alloc.answer));
        @memcpy(bytes[0..len], slice);

        return RocStr{
            .bytes = bytes,
            .len = len,
            .capacity = @intCast(len),
        };
    }

    /// Get the bytes as a Zig slice
    fn asSlice(self: *const RocStr) []const u8 {
        if (self.capacity < 0) {
            // Small string: bytes are stored inline
            const inline_bytes: [*]const u8 = @ptrCast(&self.bytes);
            return inline_bytes[0..self.len];
        } else {
            // Large string: bytes are on heap
            return if (self.bytes) |ptr| ptr[0..self.len] else &[_]u8{};
        }
    }
};

/// Platform host entrypoint
fn platform_main() !void {
    var host_env = HostEnv{
        .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
    };
    defer host_env.arena.deinit();

    const stdout = std.fs.File.stdout().deprecatedWriter();
    const stderr = std.fs.File.stderr().deprecatedWriter();

    // Create the RocOps struct
    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .host_fns = undefined, // No host functions needed for this test
    };

    // Test writeToStdout
    try stdout.print("=== Testing writeToStdout ===\n", .{});

    const stdout_msg = "Hello from stdout!";
    var stdout_roc_str = RocStr.fromSlice(stdout_msg, &roc_ops);

    var stdout_result: [0]u8 = undefined; // Result is {} which is zero-sized
    roc__writeToStdout(&roc_ops, @as(*anyopaque, @ptrCast(&stdout_result)), @as(*anyopaque, @ptrCast(&stdout_roc_str)));

    try stdout.print("STDOUT: {s}\n", .{stdout_msg});

    // Test writeToStderr
    try stdout.print("\n=== Testing writeToStderr ===\n", .{});

    const stderr_msg = "Error from stderr!";
    var stderr_roc_str = RocStr.fromSlice(stderr_msg, &roc_ops);

    var stderr_result: [0]u8 = undefined; // Result is {} which is zero-sized
    roc__writeToStderr(&roc_ops, @as(*anyopaque, @ptrCast(&stderr_result)), @as(*anyopaque, @ptrCast(&stderr_roc_str)));

    try stderr.print("STDERR: {s}\n", .{stderr_msg});

    // Test both together
    try stdout.print("\n=== Testing both ===\n", .{});

    const msg1 = "Line 1 to stdout";
    const msg2 = "Line 2 to stderr";
    const msg3 = "Line 3 to stdout";

    var msg1_roc = RocStr.fromSlice(msg1, &roc_ops);
    var msg2_roc = RocStr.fromSlice(msg2, &roc_ops);
    var msg3_roc = RocStr.fromSlice(msg3, &roc_ops);

    roc__writeToStdout(&roc_ops, @as(*anyopaque, @ptrCast(&stdout_result)), @as(*anyopaque, @ptrCast(&msg1_roc)));
    try stdout.print("STDOUT: {s}\n", .{msg1});

    roc__writeToStderr(&roc_ops, @as(*anyopaque, @ptrCast(&stderr_result)), @as(*anyopaque, @ptrCast(&msg2_roc)));
    try stderr.print("STDERR: {s}\n", .{msg2});

    roc__writeToStdout(&roc_ops, @as(*anyopaque, @ptrCast(&stdout_result)), @as(*anyopaque, @ptrCast(&msg3_roc)));
    try stdout.print("STDOUT: {s}\n", .{msg3});

    try stdout.print("\n=== ALL TESTS COMPLETED ===\n", .{});
}
