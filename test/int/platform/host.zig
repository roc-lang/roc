//! Platform host that tests Box(model) type variable across the host boundary.
//! Tests init/update/render pattern where Box is opaque to the host.

const std = @import("std");
const shim_io = @import("shim_io");
const builtins = @import("builtins");
const f32str = builtins.compiler_rt_128.f32_to_str;

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
// See `shim_io.std_options_no_stack_tracing` for why stack tracing is disabled.
pub const std_options = shim_io.std_options_no_stack_tracing;

var app_std_io: std.Io = shim_io.io();

fn print(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
    std.Io.File.stdout().writeStreamingAll(app_std_io, msg) catch {};
}

/// Host environment - contains our arena allocator
const HostEnv = struct {
    arena: std.heap.ArenaAllocator,
};

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(ops: *builtins.host_abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.arena.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Prepend size metadata so realloc can know the old size
    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const total_size = length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
        @panic("Host allocation failed");
    };

    // Store total size right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    return @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

/// Roc deallocation function
fn rocDeallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    _ = ops;
    _ = ptr;
    _ = alignment;
    // NoOp as our arena frees all memory at once
}

/// Roc reallocation function
fn rocReallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.arena.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    const size_storage_bytes = @max(alignment, @alignOf(usize));

    // Read old size from metadata
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;

    // Allocate new block
    const new_total_size = new_length + size_storage_bytes;
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        @panic("Host reallocation failed");
    };

    // Copy old data to new location
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);
    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_ptr[0..copy_size], old_base_ptr[0..copy_size]);

    // Store new size in metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    return @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes);
}

/// Roc debug function
fn rocDbgFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const source_bytes = bytes[0..len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    @panic(message);
}

// Box is opaque to the host - just a pointer-sized value
const Box = usize;

// Roc value layouts at the boundary (record fields sorted by alignment
// descending, then name; same convention the old args buffers used).
const MixedResult = extern struct { value: i64, flag: bool };
const FrameInput = extern struct {
    frame_count: u64,
    mouse_wheel: f32,
    mouse_x: f32,
    mouse_y: f32,
    mouse_left: bool,
    mouse_middle: bool,
    mouse_right: bool,
};
const SimpleInput = extern struct {
    number: u64,
    flag: bool,
};
const ThreeFloats = extern struct {
    aaa: f32,
    bbb: f32,
    ccc: f32,
};
/// Simple(Model) is opaque to the host; large enough for any app model and
/// pointer-aligned (it contains a Str).
const RenderResult = extern struct { opaque_bytes: [8]u64 };

// The app's entrypoints, exported under their provides symbols with their
// natural C ABIs. Entrypoint arguments are passed in source order; record
// values keep their alignment-sorted field layout.
extern fn roc_init() callconv(.c) Box;
extern fn roc_update(boxed_model: Box, delta: i64) callconv(.c) Box;
extern fn roc_render(boxed_model: Box) callconv(.c) RenderResult;
extern fn roc_test_mixed_args(flag: bool, value: i64) callconv(.c) MixedResult;
extern fn roc_test_struct_arg(input: FrameInput) callconv(.c) FrameInput;
extern fn roc_test_effectful_struct_arg(input: FrameInput) callconv(.c) FrameInput;
extern fn roc_test_simple_pure(input: SimpleInput) callconv(.c) SimpleInput;
extern fn roc_test_simple_effectful(input: SimpleInput) callconv(.c) SimpleInput;
extern fn roc_test_three_floats_pure(input: ThreeFloats) callconv(.c) ThreeFloats;
extern fn roc_test_three_floats_effectful(input: ThreeFloats) callconv(.c) ThreeFloats;

// --- Symbol-ABI runtime exports
var g_host_env = HostEnv{
    .arena = .init(std.heap.page_allocator),
};

var g_roc_ops = builtins.host_abi.RocOps{
    .env = @as(*anyopaque, @ptrCast(&g_host_env)),
    .roc_alloc = rocAllocFn,
    .roc_dealloc = rocDeallocFn,
    .roc_realloc = rocReallocFn,
    .roc_dbg = rocDbgFn,
    .roc_expect_failed = rocExpectFailedFn,
    .roc_crashed = rocCrashedFn,
    .hosted_fns = .{ .count = 0, .fns = undefined },
};

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocAllocFn(&g_roc_ops, length, alignment);
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    rocDeallocFn(&g_roc_ops, ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocReallocFn(&g_roc_ops, ptr, new_length, alignment);
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocDbgFn(&g_roc_ops, bytes, len);
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocExpectFailedFn(&g_roc_ops, bytes, len);
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocCrashedFn(&g_roc_ops, bytes, len);
}

comptime {
    @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
    @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
    @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
    @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
    @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
    @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });
}

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
    platform_main();
    return 0;
}

/// Platform host entrypoint -- tests Box(model) across the host boundary
fn platform_main() void {
    var success_count: u32 = 0;

    // Test 1: init returns Box(model)
    print("\n=== Test 1: init returns Box(model) ===\n", .{});
    const boxed_model = roc_init();
    print("init returned Box: 0x{x}\n", .{boxed_model});
    print("\x1b[32mSUCCESS\x1b[0m: init completed!\n", .{});
    success_count += 1;

    // Test 2: update takes (Box(model), I64), returns Box(model)
    // Note: update calls Box.unbox which CONSUMES the input Box, so we can't reuse boxed_model after this
    print("\n=== Test 2: update(Box(model), 42) -> Box(model) ===\n", .{});
    const updated_model = roc_update(boxed_model, 42);
    print("update returned new Box: 0x{x}\n", .{updated_model});
    print("\x1b[32mSUCCESS\x1b[0m: update completed!\n", .{});
    success_count += 1;

    // Test 3: render takes Box(model), returns Simple(Model) - an opaque type
    // Simple(Model) is a tag union, so it has a discriminant + payload
    // Note: render calls Box.unbox which CONSUMES the input Box
    print("\n=== Test 3: render(Box(model)) -> Simple(Model) ===\n", .{});
    const render_result = roc_render(updated_model);
    _ = render_result;
    print("render completed without crash\n", .{});
    print("\x1b[32mSUCCESS\x1b[0m: render returned Simple(Model)!\n", .{});
    success_count += 1;

    // Test 4: init again to get a fresh Box, then render it
    // This tests that we can create and consume multiple Boxes
    print("\n=== Test 4: init + render (fresh Box) ===\n", .{});
    const fresh_model = roc_init();
    const final_result = roc_render(fresh_model);
    _ = final_result;
    print("render completed without crash\n", .{});
    print("\x1b[32mSUCCESS\x1b[0m: fresh init + render worked!\n", .{});
    success_count += 1;

    // Test 5: test_mixed_args takes (Bool, I64) - tests issue 8991
    // This tests that arguments with different alignments are correctly passed from host to Roc.
    // Bool has 1-byte alignment, I64 has 8-byte alignment.
    // According to Roc ABI, arguments are sorted by alignment descending, so I64 comes first in memory.
    print("\n=== Test 5: test_mixed_args(Bool, I64) -> (Bool, I64) (issue 8991) ===\n", .{});

    // Arguments struct must match Roc's sorted layout: I64 (8-byte alignment) before Bool (1-byte alignment)
    // Even though the Roc function signature is (Bool, I64), the memory layout is sorted by alignment.
    const MixedArgs = struct { value: i64, flag: bool };
    const mixed_args = MixedArgs{ .value = 12345, .flag = true };

    const mixed_result = roc_test_mixed_args(mixed_args.flag, mixed_args.value);

    // Verify the values came through correctly
    print("Input: flag={}, value={}\n", .{ mixed_args.flag, mixed_args.value });
    print("Output: flag={}, value={}\n", .{ mixed_result.flag, mixed_result.value });

    if (mixed_result.flag == mixed_args.flag and mixed_result.value == mixed_args.value) {
        print("\x1b[32mSUCCESS\x1b[0m: mixed args passed correctly!\n", .{});
        success_count += 1;
    } else {
        print("\x1b[31mFAILED\x1b[0m: values corrupted! Expected flag={}, value={}\n", .{ mixed_args.flag, mixed_args.value });
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
    print("\n=== Test 6: test_struct_arg(FrameInput) -> FrameInput (issue 8991 full test) ===\n", .{});

    const frame_input = FrameInput{
        .frame_count = 123456789,
        .mouse_wheel = 0.5,
        .mouse_x = 100.25,
        .mouse_y = 200.75,
        .mouse_left = true,
        .mouse_middle = false,
        .mouse_right = true,
    };

    const frame_output = roc_test_struct_arg(frame_input);

    // Verify all field values came through correctly
    {
        var b1: [32]u8 = undefined;
        var b2: [32]u8 = undefined;
        var b3: [32]u8 = undefined;
        print("Input:  frame_count={}, mouse_wheel={s}, mouse_x={s}, mouse_y={s}, mouse_left={}, mouse_middle={}, mouse_right={}\n", .{
            frame_input.frame_count,
            f32str(&b1, frame_input.mouse_wheel),
            f32str(&b2, frame_input.mouse_x),
            f32str(&b3, frame_input.mouse_y),
            frame_input.mouse_left,
            frame_input.mouse_middle,
            frame_input.mouse_right,
        });
    }
    {
        var b1: [32]u8 = undefined;
        var b2: [32]u8 = undefined;
        var b3: [32]u8 = undefined;
        print("Output: frame_count={}, mouse_wheel={s}, mouse_x={s}, mouse_y={s}, mouse_left={}, mouse_middle={}, mouse_right={}\n", .{
            frame_output.frame_count,
            f32str(&b1, frame_output.mouse_wheel),
            f32str(&b2, frame_output.mouse_x),
            f32str(&b3, frame_output.mouse_y),
            frame_output.mouse_left,
            frame_output.mouse_middle,
            frame_output.mouse_right,
        });
    }

    const struct_match = frame_output.frame_count == frame_input.frame_count and
        frame_output.mouse_wheel == frame_input.mouse_wheel and
        frame_output.mouse_x == frame_input.mouse_x and
        frame_output.mouse_y == frame_input.mouse_y and
        frame_output.mouse_left == frame_input.mouse_left and
        frame_output.mouse_middle == frame_input.mouse_middle and
        frame_output.mouse_right == frame_input.mouse_right;

    if (struct_match) {
        print("\x1b[32mSUCCESS\x1b[0m: FrameInput struct passed correctly!\n", .{});
        success_count += 1;
    } else {
        print("\x1b[31mFAILED\x1b[0m: FrameInput values corrupted!\n", .{});
    }

    // Test 7: EFFECTFUL version of test_struct_arg - tests issue 8991 with effectful entry points
    // The bug report indicates that effectful entry points (`=>`) have a layout mismatch while
    // pure entry points (`->`) work correctly. This test should reveal the bug.
    print("\n=== Test 7: EFFECTFUL test_struct_arg!(FrameInput) -> FrameInput (issue 8991 effectful) ===\n", .{});

    // Use same input values as test 6
    const effectful_frame_input = FrameInput{
        .frame_count = 123456789,
        .mouse_wheel = 0.5,
        .mouse_x = 100.25,
        .mouse_y = 200.75,
        .mouse_left = true,
        .mouse_middle = false,
        .mouse_right = true,
    };

    const effectful_frame_output = roc_test_effectful_struct_arg(effectful_frame_input);

    {
        var b1: [32]u8 = undefined;
        var b2: [32]u8 = undefined;
        var b3: [32]u8 = undefined;
        print("Input:  frame_count={}, mouse_wheel={s}, mouse_x={s}, mouse_y={s}, mouse_left={}, mouse_middle={}, mouse_right={}\n", .{
            effectful_frame_input.frame_count,
            f32str(&b1, effectful_frame_input.mouse_wheel),
            f32str(&b2, effectful_frame_input.mouse_x),
            f32str(&b3, effectful_frame_input.mouse_y),
            effectful_frame_input.mouse_left,
            effectful_frame_input.mouse_middle,
            effectful_frame_input.mouse_right,
        });
    }
    {
        var b1: [32]u8 = undefined;
        var b2: [32]u8 = undefined;
        var b3: [32]u8 = undefined;
        print("Output: frame_count={}, mouse_wheel={s}, mouse_x={s}, mouse_y={s}, mouse_left={}, mouse_middle={}, mouse_right={}\n", .{
            effectful_frame_output.frame_count,
            f32str(&b1, effectful_frame_output.mouse_wheel),
            f32str(&b2, effectful_frame_output.mouse_x),
            f32str(&b3, effectful_frame_output.mouse_y),
            effectful_frame_output.mouse_left,
            effectful_frame_output.mouse_middle,
            effectful_frame_output.mouse_right,
        });
    }

    const effectful_struct_match = effectful_frame_output.frame_count == effectful_frame_input.frame_count and
        effectful_frame_output.mouse_wheel == effectful_frame_input.mouse_wheel and
        effectful_frame_output.mouse_x == effectful_frame_input.mouse_x and
        effectful_frame_output.mouse_y == effectful_frame_input.mouse_y and
        effectful_frame_output.mouse_left == effectful_frame_input.mouse_left and
        effectful_frame_output.mouse_middle == effectful_frame_input.mouse_middle and
        effectful_frame_output.mouse_right == effectful_frame_input.mouse_right;

    if (effectful_struct_match) {
        print("\x1b[32mSUCCESS\x1b[0m: EFFECTFUL FrameInput struct passed correctly!\n", .{});
        success_count += 1;
    } else {
        print("\x1b[31mFAILED\x1b[0m: EFFECTFUL FrameInput values corrupted! This is issue 8991.\n", .{});
    }

    // Test 8 & 9: Simple 2-field struct tests to isolate the issue
    // SimpleInput layout: number (U64) at offset 0, flag (Bool) at offset 8
    // Test 8: Pure function with SimpleInput
    print("\n=== Test 8: PURE test_simple_pure(SimpleInput) -> SimpleInput ===\n", .{});
    const simple_pure_input = SimpleInput{ .number = 42, .flag = true };
    const simple_pure_output = roc_test_simple_pure(simple_pure_input);
    print("Input:  number={}, flag={}\n", .{ simple_pure_input.number, simple_pure_input.flag });
    print("Output: number={}, flag={}\n", .{ simple_pure_output.number, simple_pure_output.flag });
    if (simple_pure_output.number == simple_pure_input.number and simple_pure_output.flag == simple_pure_input.flag) {
        print("\x1b[32mSUCCESS\x1b[0m: PURE SimpleInput passed correctly!\n", .{});
        success_count += 1;
    } else {
        print("\x1b[31mFAILED\x1b[0m: PURE SimpleInput values corrupted!\n", .{});
    }

    // Test 9: Effectful function with SimpleInput
    print("\n=== Test 9: EFFECTFUL test_simple_effectful!(SimpleInput) -> SimpleInput ===\n", .{});
    const simple_eff_input = SimpleInput{ .number = 42, .flag = true };
    const simple_eff_output = roc_test_simple_effectful(simple_eff_input);
    print("Input:  number={}, flag={}\n", .{ simple_eff_input.number, simple_eff_input.flag });
    print("Output: number={}, flag={}\n", .{ simple_eff_output.number, simple_eff_output.flag });
    if (simple_eff_output.number == simple_eff_input.number and simple_eff_output.flag == simple_eff_input.flag) {
        print("\x1b[32mSUCCESS\x1b[0m: EFFECTFUL SimpleInput passed correctly!\n", .{});
        success_count += 1;
    } else {
        print("\x1b[31mFAILED\x1b[0m: EFFECTFUL SimpleInput values corrupted! This is issue 8991.\n", .{});
    }

    // Test 10 & 11: Three floats with same alignment to test alphabetical sorting
    // ThreeFloats layout: aaa (F32) at offset 0, bbb (F32) at offset 4, ccc (F32) at offset 8
    // Test 10: Pure function with ThreeFloats
    print("\n=== Test 10: PURE test_three_floats_pure(ThreeFloats) -> ThreeFloats ===\n", .{});
    const three_pure_input = ThreeFloats{ .aaa = 1.0, .bbb = 2.0, .ccc = 3.0 };
    const three_pure_output = roc_test_three_floats_pure(three_pure_input);
    {
        var b1: [32]u8 = undefined;
        var b2: [32]u8 = undefined;
        var b3: [32]u8 = undefined;
        var b4: [32]u8 = undefined;
        var b5: [32]u8 = undefined;
        var b6: [32]u8 = undefined;
        print("Input:  aaa={s}, bbb={s}, ccc={s}\n", .{ f32str(&b1, three_pure_input.aaa), f32str(&b2, three_pure_input.bbb), f32str(&b3, three_pure_input.ccc) });
        print("Output: aaa={s}, bbb={s}, ccc={s}\n", .{ f32str(&b4, three_pure_output.aaa), f32str(&b5, three_pure_output.bbb), f32str(&b6, three_pure_output.ccc) });
    }
    if (three_pure_output.aaa == three_pure_input.aaa and three_pure_output.bbb == three_pure_input.bbb and three_pure_output.ccc == three_pure_input.ccc) {
        print("\x1b[32mSUCCESS\x1b[0m: PURE ThreeFloats passed correctly!\n", .{});
        success_count += 1;
    } else {
        print("\x1b[31mFAILED\x1b[0m: PURE ThreeFloats values corrupted!\n", .{});
    }

    // Test 11: Effectful function with ThreeFloats
    print("\n=== Test 11: EFFECTFUL test_three_floats_effectful!(ThreeFloats) -> ThreeFloats ===\n", .{});
    const three_eff_input = ThreeFloats{ .aaa = 1.0, .bbb = 2.0, .ccc = 3.0 };
    const three_eff_output = roc_test_three_floats_effectful(three_eff_input);
    {
        var b1: [32]u8 = undefined;
        var b2: [32]u8 = undefined;
        var b3: [32]u8 = undefined;
        var b4: [32]u8 = undefined;
        var b5: [32]u8 = undefined;
        var b6: [32]u8 = undefined;
        print("Input:  aaa={s}, bbb={s}, ccc={s}\n", .{ f32str(&b1, three_eff_input.aaa), f32str(&b2, three_eff_input.bbb), f32str(&b3, three_eff_input.ccc) });
        print("Output: aaa={s}, bbb={s}, ccc={s}\n", .{ f32str(&b4, three_eff_output.aaa), f32str(&b5, three_eff_output.bbb), f32str(&b6, three_eff_output.ccc) });
    }
    if (three_eff_output.aaa == three_eff_input.aaa and three_eff_output.bbb == three_eff_input.bbb and three_eff_output.ccc == three_eff_input.ccc) {
        print("\x1b[32mSUCCESS\x1b[0m: EFFECTFUL ThreeFloats passed correctly!\n", .{});
        success_count += 1;
    } else {
        print("\x1b[31mFAILED\x1b[0m: EFFECTFUL ThreeFloats values corrupted! This is issue 8991.\n", .{});
    }

    // Final summary
    print("\n=== FINAL RESULT ===\n", .{});
    if (success_count == 11) {
        print("\x1b[32mALL TESTS PASSED\x1b[0m: Box(model) and mixed args work correctly across host boundary!\n", .{});
    } else {
        print("\x1b[31mSOME TESTS FAILED\x1b[0m: {}/11 tests passed\n", .{success_count});
        std.process.exit(1);
    }
}
