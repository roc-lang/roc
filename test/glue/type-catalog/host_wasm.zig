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
        const text = std.fmt.bufPrint(&report, "FAIL type-catalog ZigGlue wasm32: " ++ fmt, args) catch "FAIL type-catalog ZigGlue wasm32: report overflow";
        report_len = text.len;
    }
    failure_count += 1;
}

fn finishPass() void {
    const message = "PASS glue-runtime type-catalog ZigGlue wasm32";
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

fn hostAlloc(_: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRaw(length, alignment);
}

fn hostDealloc(_: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    deallocRaw(ptr, 0, alignment);
}

fn hostRealloc(_: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    _ = ptr;
    return allocRaw(new_length, alignment);
}

fn hostDbg(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {}
fn hostExpectFailed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_expect_failed", .{});
}
fn hostCrashed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_crashed", .{});
}

var roc_host = abi.RocHost{
    .env = @ptrCast(&failure_count),
    .roc_alloc = &hostAlloc,
    .roc_dealloc = &hostDealloc,
    .roc_realloc = &hostRealloc,
    .roc_dbg = &hostDbg,
    .roc_expect_failed = &hostExpectFailed,
    .roc_crashed = &hostCrashed,
};

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

export fn roc_catalog_roundtrip(arg0: abi.EmptyOrPairOrPayloadOrRecursiveType0) callconv(.c) abi.EmptyOrPairOrPayloadOrRecursiveType0 {
    return arg0;
}
export fn roc_catalog_single_no_payload() callconv(.c) *anyopaque {
    return undefined;
}
export fn roc_catalog_single_payload_roundtrip(arg0: abi.CatalogPayload) callconv(.c) abi.CatalogPayload {
    return arg0;
}

fn expectStr(str: *const abi.RocStr, expected: []const u8, label: []const u8) void {
    if (!std.mem.eql(u8, str.asSlice(), expected)) fail("{s} mismatch", .{label});
}

fn runContract() void {
    const point = abi.roc_point();
    if (point.@"x" != -17 or point.@"y" != 42) fail("point mismatch", .{});
    const structural = abi.roc_structural();
    if (structural.@"count" != 19) fail("structural count mismatch", .{});
    expectStr(&structural.@"name", "catalog", "structural name");
    if (structural.@"nested".@"byte" != 7 or structural.@"nested".@"flag" != true) fail("structural nested mismatch", .{});
    const result_a = abi.roc_result_a();
    if (result_a.tag != .Ok) fail("A.Result tag mismatch", .{});
    var a_payload = result_a.payload_ok();
    expectStr(&a_payload, "alpha", "A.Result payload");
    const result_b = abi.roc_result_b();
    if (result_b.tag != .Err) fail("B.Result tag mismatch", .{});
    const b_payload = result_b.payload_err();
    if (b_payload.@"code" != 5) fail("B.Result code mismatch", .{});
    expectStr(&b_payload.@"message", "bravo", "B.Result message");
    const dec = abi.RocDec{ .num = 1_250_000_000_000_000_000 };
    if (abi.roc_dec(dec).num != dec.num) fail("Dec identity mismatch", .{});
    if (abi.roc_i128(-123456789) != -123456789) fail("I128 identity mismatch", .{});
    if (abi.roc_u128(123456789) != 123456789) fail("U128 identity mismatch", .{});
    result_a.decref(&roc_host);
    result_b.decref(&roc_host);
    structural.decref(&roc_host);
}

export fn wasm_main() [*]const u8 {
    failure_count = 0;
    report_len = 0;
    runContract();
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
