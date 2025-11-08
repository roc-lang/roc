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
extern fn roc__main_for_host(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

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

/// Hosted function: Stderr.line! (index 0 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStderrLine(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ops;
    _ = ret_ptr; // Return value is {} which is zero-sized

    // Arguments struct for single Str parameter
    const Args = extern struct { str: RocStr };
    const args: *Args = @ptrCast(@alignCast(args_ptr));

    const message = args.str.asSlice();
    std.fs.File.stderr().deprecatedWriter().print("{s}\n", .{message}) catch unreachable;
}

/// Hosted function: Stdout.line! (index 1 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStdoutLine(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ops;
    _ = ret_ptr; // Return value is {} which is zero-sized

    // Arguments struct for single Str parameter
    const Args = extern struct { str: RocStr };
    const args: *Args = @ptrCast(@alignCast(args_ptr));

    const message = args.str.asSlice();
    std.fs.File.stdout().deprecatedWriter().print("{s}\n", .{message}) catch unreachable;
}

/// Array of hosted function pointers, sorted alphabetically by fully-qualified name
/// These correspond to the hosted functions defined in Stderr and Stdout Type Modules
const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{
    hostedStderrLine, // Stderr.line! (index 0)
    hostedStdoutLine, // Stdout.line! (index 1)
};

/// Platform host entrypoint
fn platform_main() !void {
    var host_env = HostEnv{
        .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
    };
    defer host_env.arena.deinit();

    // Create the RocOps struct
    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{
            .count = hosted_function_ptrs.len,
            .fns = @constCast(&hosted_function_ptrs),
        },
    };

    // Call the app's main! entrypoint
    var unit_result: [0]u8 = undefined; // Result is {} which is zero-sized
    // For a function with signature () => {}, the argument is an empty tuple (zero parameters)
    // An empty tuple is zero-sized, so we pass a zero-sized value
    var args: [0]u8 = undefined;
    roc__main_for_host(&roc_ops, @as(*anyopaque, @ptrCast(&unit_result)), @as(*anyopaque, @ptrCast(&args)));
}
