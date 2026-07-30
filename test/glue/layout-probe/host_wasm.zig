const std = @import("std");
const abi = @import("roc_platform_abi.zig");

const wasm_allocator = std.heap.wasm_allocator;
var failure_count: usize = 0;
var report: [512]u8 = [_]u8{0} ** 512;
var report_len: usize = 0;
var alloc_count: usize = 0;
var dealloc_count: usize = 0;

fn fail(comptime fmt: []const u8, args: anytype) void {
    if (failure_count == 0) {
        const text = std.fmt.bufPrint(&report, "FAIL layout-probe ZigGlue wasm32: " ++ fmt, args) catch "FAIL layout-probe ZigGlue wasm32: report overflow";
        report_len = text.len;
    }
    failure_count += 1;
}

fn finishPass() void {
    const message = "PASS glue-runtime layout-probe ZigGlue wasm32";
    @memcpy(report[0..message.len], message);
    report_len = message.len;
}

fn allocRaw(length: usize, alignment: usize) ?*anyopaque {
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));
    const mem = wasm_allocator.rawAlloc(length, align_log2, @returnAddress()) orelse return null;
    alloc_count += 1;
    return @ptrCast(mem);
}

fn deallocRaw(ptr: ?*anyopaque, length: usize, alignment: usize) void {
    const p = ptr orelse return;
    const align_log2: std.mem.Alignment = @enumFromInt(std.math.log2_int(usize, alignment));
    const bytes: [*]u8 = @ptrCast(p);
    wasm_allocator.rawFree(bytes[0..length], align_log2, @returnAddress());
    dealloc_count += 1;
}

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRaw(length, alignment);
}
export fn roc_dealloc(ptr: ?*anyopaque, alignment: usize) callconv(.c) void {
    deallocRaw(ptr, 0, alignment);
}
export fn roc_realloc(ptr: ?*anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    _ = ptr;
    return allocRaw(new_length, alignment);
}
export fn roc_dbg(_: [*]const u8, _: usize) callconv(.c) void {}
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
}

export fn wasm_main() [*]const u8 {
    failure_count = 0;
    report_len = 0;
    abi.roc_main();
    checkProvidedAbi();
    if (failure_count == 0) finishPass();
    return &report;
}

export fn wasm_result_len() usize {
    return report_len;
}

export fn wasm_alloc_count() usize {
    return alloc_count;
}

export fn wasm_dealloc_count() usize {
    return dealloc_count;
}
