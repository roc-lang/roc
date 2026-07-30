const std = @import("std");
const builtin = @import("builtin");
const abi = @import("roc_platform_abi.zig");

extern fn malloc(size: usize) callconv(.c) ?*anyopaque;
extern fn free(ptr: ?*anyopaque) callconv(.c) void;
extern fn write(fd: c_int, buf: [*]const u8, count: usize) callconv(.c) isize;
extern fn _write(fd: c_int, buf: [*]const u8, count: c_uint) callconv(.c) c_int;

var failure_count: usize = 0;
var report: [512]u8 = [_]u8{0} ** 512;
var report_len: usize = 0;
var alloc_count: usize = 0;
var dealloc_count: usize = 0;

fn fail(comptime fmt: []const u8, args: anytype) void {
    if (failure_count == 0) {
        const text = std.fmt.bufPrint(&report, "FAIL layout-probe ZigGlue: " ++ fmt, args) catch "FAIL layout-probe ZigGlue: report overflow";
        report_len = text.len;
    }
    failure_count += 1;
}

fn writeStderr(bytes: []const u8) void {
    if (bytes.len == 0) return;
    if (comptime builtin.os.tag == .windows) {
        _ = _write(2, bytes.ptr, @intCast(bytes.len));
    } else {
        _ = write(2, bytes.ptr, bytes.len);
    }
}

fn stderrPrint(comptime fmt: []const u8, args: anytype) void {
    var buffer: [512]u8 = undefined;
    const text = std.fmt.bufPrint(&buffer, fmt, args) catch "stderr format overflow\n";
    writeStderr(text);
}

fn allocRaw(length: usize, alignment: usize) ?*anyopaque {
    const total = length + alignment - 1 + @sizeOf(usize);
    const raw: [*]u8 = @ptrCast(malloc(total) orelse return null);
    const aligned = std.mem.alignForward(usize, @intFromPtr(raw) + @sizeOf(usize), alignment);
    const slot: *usize = @ptrFromInt(aligned - @sizeOf(usize));
    slot.* = @intFromPtr(raw);
    alloc_count += 1;
    return @ptrFromInt(aligned);
}

fn deallocRaw(ptr: ?*anyopaque) void {
    const p = ptr orelse return;
    const slot: *usize = @ptrFromInt(@intFromPtr(p) - @sizeOf(usize));
    free(@ptrFromInt(slot.*));
    dealloc_count += 1;
}

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRaw(length, alignment);
}

export fn roc_dealloc(ptr: ?*anyopaque, _: usize) callconv(.c) void {
    deallocRaw(ptr);
}

export fn roc_realloc(ptr: ?*anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    deallocRaw(ptr);
    return allocRaw(new_length, alignment);
}

export fn roc_dbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    writeStderr(bytes[0..len]);
    writeStderr("\n");
}

export fn roc_expect_failed(_: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_expect_failed", .{});
}

export fn roc_crashed(_: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_crashed", .{});
}

export fn roc_probe_roundtrip(arg0: abi.ProbeLayoutProbe) callconv(.c) abi.ProbeLayoutProbe {
    return arg0;
}

export fn roc_probe_roundtrip_u8x16(arg0: abi.RocU8x16) callconv(.c) abi.RocU8x16 {
    return arg0;
}
export fn roc_probe_roundtrip_i8x16(arg0: abi.RocI8x16) callconv(.c) abi.RocI8x16 {
    return arg0;
}
export fn roc_probe_roundtrip_u16x8(arg0: abi.RocU16x8) callconv(.c) abi.RocU16x8 {
    return arg0;
}
export fn roc_probe_roundtrip_i16x8(arg0: abi.RocI16x8) callconv(.c) abi.RocI16x8 {
    return arg0;
}
export fn roc_probe_roundtrip_u32x4(arg0: abi.RocU32x4) callconv(.c) abi.RocU32x4 {
    return arg0;
}
export fn roc_probe_roundtrip_i32x4(arg0: abi.RocI32x4) callconv(.c) abi.RocI32x4 {
    return arg0;
}
export fn roc_probe_roundtrip_u64x2(arg0: abi.RocU64x2) callconv(.c) abi.RocU64x2 {
    return arg0;
}
export fn roc_probe_roundtrip_i64x2(arg0: abi.RocI64x2) callconv(.c) abi.RocI64x2 {
    return arg0;
}
export fn roc_probe_roundtrip_vector_record(arg0: abi.ProbeVectorRecord) callconv(.c) abi.ProbeVectorRecord {
    return arg0;
}
export fn roc_probe_roundtrip_vector_quad(arg0: abi.ProbeVectorQuad) callconv(.c) abi.ProbeVectorQuad {
    return arg0;
}
export fn roc_probe_roundtrip_vector_hva(arg0: abi.ProbeVectorHva) callconv(.c) abi.ProbeVectorHva {
    return arg0;
}
export fn roc_probe_roundtrip_vector_wrapper(arg0: abi.ProbeVectorWrapper) callconv(.c) abi.ProbeVectorWrapper {
    return arg0;
}
export fn roc_probe_roundtrip_vector_tag(arg0: abi.ProbeVectorTag) callconv(.c) abi.ProbeVectorTag {
    return arg0;
}
export fn roc_probe_roundtrip_vector_tuple(arg0: abi.ProbeRoundtrip_vector_tupleArgs) callconv(.c) abi.__AnonStruct_fbe9eaebfd8c38fd {
    return .{ ._1 = arg0._1, ._2 = arg0._2, ._0 = arg0._0 };
}

export fn roc_probe_exhaust_registers(
    arg0: i64,
    arg1: i64,
    arg2: i64,
    arg3: i64,
    arg4: i64,
    arg5: i64,
    arg6: f64,
    arg7: f64,
    arg8: f64,
    arg9: f64,
    arg10: f64,
    arg11: f64,
    arg12: f64,
    arg13: f64,
    arg14: abi.RocU8x16,
) callconv(.c) abi.RocU8x16 {
    _ = .{ arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13 };
    return arg14;
}

export fn roc_probe_spill_vector_hva(
    arg0: abi.RocU8x16,
    arg1: abi.RocU8x16,
    arg2: abi.RocU8x16,
    arg3: abi.RocU8x16,
    arg4: abi.RocU8x16,
    arg5: abi.RocU8x16,
    arg6: abi.RocU8x16,
    arg7: abi.ProbeNestedVectorHva,
) callconv(.c) abi.ProbeNestedVectorHva {
    _ = .{ arg0, arg1, arg2, arg3, arg4, arg5, arg6 };
    return arg7;
}

export fn roc_probe_spill_float_hfa(
    arg0: f64,
    arg1: f64,
    arg2: f64,
    arg3: f64,
    arg4: f64,
    arg5: f64,
    arg6: f64,
    arg7: abi.ProbeNestedFloatHfa,
) callconv(.c) abi.ProbeNestedFloatHfa {
    _ = .{ arg0, arg1, arg2, arg3, arg4, arg5, arg6 };
    return arg7;
}

export fn roc_probe_spill_integer_pair(
    arg0: i64,
    arg1: i64,
    arg2: i64,
    arg3: i64,
    arg4: i64,
    arg5: i64,
    arg6: i64,
    arg7: abi.ProbeIntegerPair,
) callconv(.c) abi.ProbeIntegerPair {
    _ = .{ arg0, arg1, arg2, arg3, arg4, arg5, arg6 };
    return arg7;
}

export fn roc_probe_align_i128(_: i64, value: i128) callconv(.c) i128 {
    return value;
}

export fn roc_probe_spill_i128(arg0: i64, arg1: i64, arg2: i64, arg3: i64, arg4: i64, value: i128) callconv(.c) i128 {
    _ = .{ arg0, arg1, arg2, arg3, arg4 };
    return value;
}

export fn roc_probe_spill_dec(arg0: i64, arg1: i64, arg2: i64, arg3: i64, arg4: i64, value: abi.RocDec) callconv(.c) abi.RocDec {
    _ = .{ arg0, arg1, arg2, arg3, arg4 };
    return value;
}

export fn roc_probe_compact_stack(
    arg0: i64,
    arg1: i64,
    arg2: i64,
    arg3: i64,
    arg4: i64,
    arg5: i64,
    arg6: i64,
    arg7: i64,
    tiny: u8,
    short: u16,
    word: u32,
) callconv(.c) u64 {
    _ = .{ arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7 };
    return @as(u64, tiny) + short + word;
}

fn sameBytes(lhs: anytype, rhs: @TypeOf(lhs)) bool {
    return std.mem.eql(u8, std.mem.asBytes(&lhs), std.mem.asBytes(&rhs));
}

fn checkProvidedAbi() void {
    const bits: u128 = 0x0ffeedfccbbaa9988776655443322110;
    const u8x16: abi.RocU8x16 = @bitCast(bits);
    const i8x16: abi.RocI8x16 = @bitCast(bits);
    const u16x8: abi.RocU16x8 = @bitCast(bits);
    const i16x8: abi.RocI16x8 = @bitCast(bits);
    const u32x4: abi.RocU32x4 = @bitCast(bits);
    const i32x4: abi.RocI32x4 = @bitCast(bits);
    const u64x2: abi.RocU64x2 = @bitCast(bits);
    const i64x2: abi.RocI64x2 = @bitCast(bits);

    if (!sameBytes(abi.roc_provide_u8x16(u8x16), u8x16)) fail("provided U8x16 mismatch", .{});
    if (!sameBytes(abi.roc_provide_i8x16(i8x16), i8x16)) fail("provided I8x16 mismatch", .{});
    if (!sameBytes(abi.roc_provide_u16x8(u16x8), u16x8)) fail("provided U16x8 mismatch", .{});
    if (!sameBytes(abi.roc_provide_i16x8(i16x8), i16x8)) fail("provided I16x8 mismatch", .{});
    if (!sameBytes(abi.roc_provide_u32x4(u32x4), u32x4)) fail("provided U32x4 mismatch", .{});
    if (!sameBytes(abi.roc_provide_i32x4(i32x4), i32x4)) fail("provided I32x4 mismatch", .{});
    if (!sameBytes(abi.roc_provide_u64x2(u64x2), u64x2)) fail("provided U64x2 mismatch", .{});
    if (!sameBytes(abi.roc_provide_i64x2(i64x2), i64x2)) fail("provided I64x2 mismatch", .{});

    const wrapper: abi.ProbeVectorWrapper = .{ .only = u8x16 };
    if (!sameBytes(abi.roc_provide_vector_wrapper(wrapper), wrapper)) fail("provided vector wrapper mismatch", .{});

    const record: abi.ProbeVectorRecord = .{ .bytes = u8x16, .words = i32x4, .before = 0x1020304050607080, .after = 0xa0b0c0d0 };
    if (!sameBytes(abi.roc_provide_vector_record(record), record)) fail("provided vector record mismatch", .{});

    const quad: abi.ProbeVectorQuad = .{ .a = u8x16, .b = i16x8, .c = u32x4, .d = i64x2 };
    if (!sameBytes(abi.roc_provide_vector_quad(quad), quad)) fail("provided vector quad mismatch", .{});

    const hva: abi.ProbeVectorHva = .{ .a = u8x16, .b = u8x16, .c = u8x16, .d = u8x16 };
    if (!sameBytes(abi.roc_provide_vector_hva(hva), hva)) fail("provided vector HVA mismatch", .{});

    const tuple: abi.__AnonStruct_fbe9eaebfd8c38fd = .{ ._1 = u8x16, ._2 = i16x8, ._0 = 0x1020304050607080 };
    if (!sameBytes(abi.roc_provide_vector_tuple(tuple), tuple)) fail("provided vector tuple mismatch", .{});

    const tag = abi.roc_make_vector_tag();
    if (!sameBytes(abi.roc_provide_vector_tag(tag), tag)) fail("provided vector tag mismatch", .{});

    const exhausted = abi.roc_provide_exhaust_registers(1, 2, 3, 4, 5, 6, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, u8x16);
    if (!sameBytes(exhausted, u8x16)) fail("provided exhausted-register vector mismatch", .{});

    const nested_hva: abi.ProbeNestedVectorHva = .{ .wrapped = u8x16, .raw = u8x16 };
    const nested_hva_back = abi.roc_provide_spill_vector_hva(u8x16, u8x16, u8x16, u8x16, u8x16, u8x16, u8x16, nested_hva);
    if (!sameBytes(nested_hva_back, nested_hva)) fail("provided spilled HVA mismatch", .{});

    const nested_hfa: abi.ProbeNestedFloatHfa = .{ .wrapped = 12.5, .raw = -7.25 };
    const nested_hfa_back = abi.roc_provide_spill_float_hfa(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, nested_hfa);
    if (!sameBytes(nested_hfa_back, nested_hfa)) fail("provided spilled HFA mismatch", .{});

    const pair: abi.ProbeIntegerPair = .{ .first = -0x102030405060708, .second = 0x8877665544332211 };
    const pair_back = abi.roc_provide_spill_integer_pair(1, 2, 3, 4, 5, 6, 7, pair);
    if (!sameBytes(pair_back, pair)) fail("provided spilled integer pair mismatch", .{});

    const wide_i128: i128 = 0x00112233445566778899aabbccddeeff;
    if (abi.roc_provide_align_i128(9, wide_i128) != wide_i128) fail("provided aligned i128 mismatch", .{});
    if (abi.roc_provide_spill_i128(1, 2, 3, 4, 5, wide_i128) != wide_i128) fail("provided spilled i128 mismatch", .{});
    const wide_dec: abi.RocDec = .{ .num = 0x0123456789abcdeffedcba9876543210 };
    if (abi.roc_provide_spill_dec(1, 2, 3, 4, 5, wide_dec).num != wide_dec.num) fail("provided spilled Dec mismatch", .{});
    const compact = abi.roc_provide_compact_stack(1, 2, 3, 4, 5, 6, 7, 8, 0x12, 0x3456, 0x789abcde);
    if (compact != 0x12 + 0x3456 + 0x789abcde) fail("provided compact stack mismatch", .{});
}

fn runContract() c_int {
    abi.roc_main();
    checkProvidedAbi();
    if (failure_count != 0) {
        const message = if (report_len == 0) "FAIL layout-probe ZigGlue: unknown failure" else report[0..report_len];
        writeStderr(message);
        writeStderr("\n");
        return 1;
    }
    stderrPrint("PASS glue-runtime layout-probe ZigGlue native alloc={} dealloc={}\n", .{ alloc_count, dealloc_count });
    return 0;
}

export fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = .{ argc, argv };
    return runContract();
}
