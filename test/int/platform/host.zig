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
extern fn roc__test_mixed_args(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;
extern fn roc__test_struct_arg(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

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

    // Test 2: update takes (Box(model), I64), returns Box(model)
    // Note: update calls Box.unbox which CONSUMES the input Box, so we can't reuse boxed_model after this
    try stdout.print("\n=== Test 2: update(Box(model), 42) -> Box(model) ===\n", .{});
    const UpdateArgs = extern struct { boxed_model: Box, delta: i64 };
    var update_args = UpdateArgs{ .boxed_model = boxed_model, .delta = 42 };
    var updated_model: Box = undefined;
    roc__update(&roc_ops, @as(*anyopaque, @ptrCast(&updated_model)), @as(*anyopaque, @ptrCast(&update_args)));
    try stdout.print("update returned new Box: 0x{x}\n", .{updated_model});
    try stdout.print("\x1b[32mSUCCESS\x1b[0m: update completed!\n", .{});
    success_count += 1;

    // Test 3: render takes Box(model), returns Simple(Model) - an opaque type
    // Simple(Model) is a tag union, so it has a discriminant + payload
    // Note: render calls Box.unbox which CONSUMES the input Box
    try stdout.print("\n=== Test 3: render(Box(model)) -> Simple(Model) ===\n", .{});
    // Use aligned buffer - Simple(Model) contains Str which requires pointer alignment
    var render_result: [64]u8 align(@alignOf(usize)) = undefined;
    roc__render(&roc_ops, @as(*anyopaque, @ptrCast(&render_result)), @as(*anyopaque, @ptrCast(&updated_model)));
    try stdout.print("render completed without crash\n", .{});
    try stdout.print("\x1b[32mSUCCESS\x1b[0m: render returned Simple(Model)!\n", .{});
    success_count += 1;

    // Test 4: init again to get a fresh Box, then render it
    // This tests that we can create and consume multiple Boxes
    try stdout.print("\n=== Test 4: init + render (fresh Box) ===\n", .{});
    var fresh_model: Box = undefined;
    roc__init(&roc_ops, @as(*anyopaque, @ptrCast(&fresh_model)), @as(*anyopaque, @ptrCast(&empty_input)));
    var final_result: [64]u8 align(@alignOf(usize)) = undefined;
    roc__render(&roc_ops, @as(*anyopaque, @ptrCast(&final_result)), @as(*anyopaque, @ptrCast(&fresh_model)));
    try stdout.print("render completed without crash\n", .{});
    try stdout.print("\x1b[32mSUCCESS\x1b[0m: fresh init + render worked!\n", .{});
    success_count += 1;

    // Test 5: test_mixed_args takes (Bool, I64) - tests issue 8991
    // This tests that arguments with different alignments are correctly passed from host to Roc.
    // Bool has 1-byte alignment, I64 has 8-byte alignment.
    // According to Roc ABI, arguments are sorted by alignment descending, so I64 comes first in memory.
    try stdout.print("\n=== Test 5: test_mixed_args(Bool, I64) -> (Bool, I64) (issue 8991) ===\n", .{});

    // Arguments struct must match Roc's sorted layout: I64 (8-byte alignment) before Bool (1-byte alignment)
    // Even though the Roc function signature is (Bool, I64), the memory layout is sorted by alignment.
    const MixedArgs = extern struct { value: i64, flag: bool };
    var mixed_args = MixedArgs{ .value = 12345, .flag = true };

    // Result struct: (Bool, I64) tuple - laid out by Roc's tuple layout rules
    // Since tuples are sorted by alignment, the layout is: I64 at offset 0, Bool at offset 8
    const MixedResult = extern struct { value: i64, flag: bool };
    var mixed_result: MixedResult = undefined;

    roc__test_mixed_args(&roc_ops, @as(*anyopaque, @ptrCast(&mixed_result)), @as(*anyopaque, @ptrCast(&mixed_args)));

    // Verify the values came through correctly
    try stdout.print("Input: flag={}, value={}\n", .{ mixed_args.flag, mixed_args.value });
    try stdout.print("Output: flag={}, value={}\n", .{ mixed_result.flag, mixed_result.value });

    if (mixed_result.flag == mixed_args.flag and mixed_result.value == mixed_args.value) {
        try stdout.print("\x1b[32mSUCCESS\x1b[0m: mixed args passed correctly!\n", .{});
        success_count += 1;
    } else {
        try stdout.print("\x1b[31mFAILED\x1b[0m: values corrupted! Expected flag={}, value={}\n", .{ mixed_args.flag, mixed_args.value });
    }

    // Test 6: test_struct_arg takes FrameInput struct - tests issue 8991 with multiple mixed-alignment fields
    // This matches the bug report example more closely.
    // FrameInput has fields: frame_count (U64), mouse_x (F32), mouse_y (F32),
    //                        mouse_left (Bool), mouse_middle (Bool), mouse_right (Bool), mouse_wheel (F32)
    //
    // Memory layout sorted by alignment descending, then field name alphabetically:
    // - frame_count: U64 (8 bytes) at offset 0
    // - mouse_wheel: F32 (4 bytes) at offset 8
    // - mouse_x: F32 (4 bytes) at offset 12
    // - mouse_y: F32 (4 bytes) at offset 16
    // - mouse_left: Bool (1 byte) at offset 20
    // - mouse_middle: Bool (1 byte) at offset 21
    // - mouse_right: Bool (1 byte) at offset 22
    // - padding (1 byte) at offset 23
    // Total size: 24 bytes
    try stdout.print("\n=== Test 6: test_struct_arg(FrameInput) -> FrameInput (issue 8991 full test) ===\n", .{});

    // FrameInput struct must match Roc's sorted record layout
    const FrameInput = extern struct {
        frame_count: u64, // U64, 8-byte alignment - first by alignment
        mouse_wheel: f32, // F32, 4-byte alignment - first alphabetically among F32s
        mouse_x: f32, // F32, 4-byte alignment
        mouse_y: f32, // F32, 4-byte alignment
        mouse_left: bool, // Bool, 1-byte alignment - first alphabetically among Bools
        mouse_middle: bool, // Bool, 1-byte alignment
        mouse_right: bool, // Bool, 1-byte alignment
    };

    var frame_input = FrameInput{
        .frame_count = 123456789,
        .mouse_wheel = 0.5,
        .mouse_x = 100.25,
        .mouse_y = 200.75,
        .mouse_left = true,
        .mouse_middle = false,
        .mouse_right = true,
    };

    var frame_output: FrameInput = undefined;

    roc__test_struct_arg(&roc_ops, @as(*anyopaque, @ptrCast(&frame_output)), @as(*anyopaque, @ptrCast(&frame_input)));

    // Verify all field values came through correctly
    try stdout.print("Input:  frame_count={}, mouse_wheel={d:.2}, mouse_x={d:.2}, mouse_y={d:.2}, mouse_left={}, mouse_middle={}, mouse_right={}\n", .{
        frame_input.frame_count,
        frame_input.mouse_wheel,
        frame_input.mouse_x,
        frame_input.mouse_y,
        frame_input.mouse_left,
        frame_input.mouse_middle,
        frame_input.mouse_right,
    });
    try stdout.print("Output: frame_count={}, mouse_wheel={d:.2}, mouse_x={d:.2}, mouse_y={d:.2}, mouse_left={}, mouse_middle={}, mouse_right={}\n", .{
        frame_output.frame_count,
        frame_output.mouse_wheel,
        frame_output.mouse_x,
        frame_output.mouse_y,
        frame_output.mouse_left,
        frame_output.mouse_middle,
        frame_output.mouse_right,
    });

    const struct_match = frame_output.frame_count == frame_input.frame_count and
        frame_output.mouse_wheel == frame_input.mouse_wheel and
        frame_output.mouse_x == frame_input.mouse_x and
        frame_output.mouse_y == frame_input.mouse_y and
        frame_output.mouse_left == frame_input.mouse_left and
        frame_output.mouse_middle == frame_input.mouse_middle and
        frame_output.mouse_right == frame_input.mouse_right;

    if (struct_match) {
        try stdout.print("\x1b[32mSUCCESS\x1b[0m: FrameInput struct passed correctly!\n", .{});
        success_count += 1;
    } else {
        try stdout.print("\x1b[31mFAILED\x1b[0m: FrameInput values corrupted!\n", .{});
    }

    // Final summary
    try stdout.print("\n=== FINAL RESULT ===\n", .{});
    if (success_count == 6) {
        try stdout.print("\x1b[32mALL TESTS PASSED\x1b[0m: Box(model) and mixed args work correctly across host boundary!\n", .{});
    } else {
        try stdout.print("\x1b[31mSOME TESTS FAILED\x1b[0m: {}/6 tests passed\n", .{success_count});
        std.process.exit(1);
    }
}
