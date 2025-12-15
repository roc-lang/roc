//! Platform host that tests Box(model) type variable across the host boundary.
//! Tests init/update/render pattern where Box is opaque to the host.

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
    const source_bytes = roc_expect.utf8_bytes[0..roc_expect.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    @panic(message);
}

// Box is opaque to the host - just a pointer-sized value
const Box = usize;

// External symbols provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc__init(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;
extern fn roc__update(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;
extern fn roc__render(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

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
        std.fs.File.stderr().deprecatedWriter().print("HOST ERROR: {s}", .{@errorName(err)}) catch unreachable;
        return 1;
    };
    return 0;
}

/// Platform host entrypoint -- tests Box(model) across the host boundary
fn platform_main() !void {
    var host_env = HostEnv{
        .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
    };
    defer host_env.arena.deinit(); // Clean up all allocations on exit

    const stdout = std.fs.File.stdout().deprecatedWriter();

    // Create the RocOps struct
    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{ .count = 0, .fns = undefined },
    };

    var success_count: u32 = 0;

    // Test 1: init returns Box(model)
    try stdout.print("\n=== Test 1: init returns Box(model) ===\n", .{});
    var boxed_model: Box = undefined;
    var empty_input: u8 = 0;
    roc__init(&roc_ops, @as(*anyopaque, @ptrCast(&boxed_model)), @as(*anyopaque, @ptrCast(&empty_input)));
    try stdout.print("init returned Box: 0x{x}\n", .{boxed_model});
    try stdout.print("\x1b[32mSUCCESS\x1b[0m: init completed!\n", .{});
    success_count += 1;

    // Test 2: render takes Box(model), returns Simple(Model) - an opaque type
    // Simple(Model) is a tag union, so it has a discriminant + payload
    // For now, just treat it as a blob and check we can call without crashing
    try stdout.print("\n=== Test 2: render(Box(model)) -> Simple(Model) ===\n", .{});
    var render_result: [64]u8 = undefined; // Buffer for opaque result
    roc__render(&roc_ops, @as(*anyopaque, @ptrCast(&render_result)), @as(*anyopaque, @ptrCast(&boxed_model)));
    try stdout.print("render completed without crash\n", .{});
    try stdout.print("\x1b[32mSUCCESS\x1b[0m: render returned Simple(Model)!\n", .{});
    success_count += 1;

    // Test 3: update takes (Box(model), I64), returns Box(model)
    try stdout.print("\n=== Test 3: update(Box(model), 42) -> Box(model) ===\n", .{});
    const UpdateArgs = extern struct { boxed_model: Box, delta: i64 };
    var update_args = UpdateArgs{ .boxed_model = boxed_model, .delta = 42 };
    var new_boxed_model: Box = undefined;
    roc__update(&roc_ops, @as(*anyopaque, @ptrCast(&new_boxed_model)), @as(*anyopaque, @ptrCast(&update_args)));
    try stdout.print("update returned new Box: 0x{x}\n", .{new_boxed_model});
    try stdout.print("\x1b[32mSUCCESS\x1b[0m: update completed!\n", .{});
    success_count += 1;

    // Test 4: render the updated model
    try stdout.print("\n=== Test 4: render(updated Box(model)) -> Simple(Model) ===\n", .{});
    var final_result: [64]u8 = undefined;
    roc__render(&roc_ops, @as(*anyopaque, @ptrCast(&final_result)), @as(*anyopaque, @ptrCast(&new_boxed_model)));
    try stdout.print("render completed without crash\n", .{});
    try stdout.print("\x1b[32mSUCCESS\x1b[0m: render returned Simple(Model)!\n", .{});
    success_count += 1;

    // Final summary
    try stdout.print("\n=== FINAL RESULT ===\n", .{});
    if (success_count == 4) {
        try stdout.print("\x1b[32mALL TESTS PASSED\x1b[0m: Box(model) works correctly across host boundary!\n", .{});
    } else {
        try stdout.print("\x1b[31mSOME TESTS FAILED\x1b[0m: {}/4 tests passed\n", .{success_count});
        std.process.exit(1);
    }
}
