const backing = std.heap.wasm_allocator;
const std = @import("std");
const host_alloc = @import("host_alloc");
const shim_symbols = @import("shim_symbols");
const abi = @import("roc_platform_abi.zig");

var failure_count: usize = 0;
var report: [512]u8 = [_]u8{0} ** 512;
var report_len: usize = 0;
var alloc_count: usize = 0;
var dealloc_count: usize = 0;

fn fail(comptime message: []const u8) void {
    if (failure_count == 0) {
        const text = "FAIL type-catalog ZigGlue wasm32: " ++ message;
        @memcpy(report[0..text.len], text);
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
    if (alignment == 0 or (alignment & (alignment - 1)) != 0) {
        fail("invalid allocation alignment");
        return null;
    }
    const ptr = host_alloc.alloc(backing, length, alignment) orelse return null;
    alloc_count += 1;
    return ptr;
}

fn deallocRaw(ptr: ?*anyopaque, alignment: usize) void {
    const p = ptr orelse return;
    host_alloc.dealloc(backing, p, alignment);
    dealloc_count += 1;
}

fn reallocRaw(ptr: ?*anyopaque, length: usize, alignment: usize) ?*anyopaque {
    const old = ptr orelse return allocRaw(length, alignment);
    const answer = host_alloc.realloc(backing, old, length, alignment) orelse return null;
    alloc_count += 1;
    dealloc_count += 1;
    return answer;
}

fn hostAlloc(_: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRaw(length, alignment);
}

fn hostDealloc(_: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    deallocRaw(ptr, alignment);
}

fn hostRealloc(_: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return reallocRaw(ptr, new_length, alignment);
}

fn hostDbg(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {}
fn hostExpectFailed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_expect_failed");
}
fn hostCrashed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_crashed");
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

fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRaw(length, alignment);
}
fn roc_dealloc(ptr: ?*anyopaque, alignment: usize) callconv(.c) void {
    deallocRaw(ptr, alignment);
}
fn roc_realloc(ptr: ?*anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return reallocRaw(ptr, new_length, alignment);
}
fn roc_dbg(_: [*]const u8, _: usize) callconv(.c) void {}
fn roc_expect_failed(_: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_expect_failed");
}
fn roc_crashed(_: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_crashed");
}

export fn roc_catalog_roundtrip(arg0: abi.EmptyOrPairOrPayloadOrRecursive) callconv(.c) abi.EmptyOrPairOrPayloadOrRecursive {
    return arg0;
}
export fn roc_catalog_single_no_payload() callconv(.c) *anyopaque {
    return undefined;
}
export fn roc_catalog_single_payload_roundtrip(arg0: abi.CatalogPayload) callconv(.c) abi.CatalogPayload {
    return arg0;
}

fn expectStr(str: *const abi.RocStr, expected: []const u8, comptime message: []const u8) void {
    const actual = str.asSlice();
    if (actual.len != expected.len) return fail(message);
    for (actual, expected) |actual_byte, expected_byte| if (actual_byte != expected_byte) return fail(message);
}

fn runContract() void {
    const point = abi.roc_point();
    if (point.x != -17 or point.y != 42) fail("point mismatch");
    const structural = abi.roc_structural();
    if (structural.count != 19) fail("structural count mismatch");
    expectStr(&structural.name, "catalog", "structural name");
    if (structural.nested.byte != 7 or structural.nested.flag != true) fail("structural nested mismatch");
    const result_a = abi.roc_result_a();
    if (result_a.tag != .Ok) fail("A.Result tag mismatch");
    var a_payload = result_a.payload_ok();
    expectStr(&a_payload, "alpha", "A.Result payload");
    const result_b = abi.roc_result_b();
    if (result_b.tag != .Err) fail("B.Result tag mismatch");
    const b_payload = result_b.payload_err();
    if (b_payload.code != 5) fail("B.Result code mismatch");
    expectStr(&b_payload.message, "bravo", "B.Result message");
    const dec = abi.RocDec{ .num = 1_250_000_000_000_000_000 };
    if (abi.roc_dec(dec).num != dec.num) fail("Dec identity mismatch");
    if (abi.roc_i128(-123456789) != -123456789) fail("I128 identity mismatch");
    if (abi.roc_u128(123456789) != 123456789) fail("U128 identity mismatch");
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

comptime {
    shim_symbols.exportRuntimeFns(.{
        .alloc = &roc_alloc,
        .dealloc = &roc_dealloc,
        .realloc = &roc_realloc,
        .dbg = &roc_dbg,
        .expect_failed = &roc_expect_failed,
        .crashed = &roc_crashed,
    }, .default);
}
