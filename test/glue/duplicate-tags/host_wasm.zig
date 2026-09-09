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
        const text = "FAIL duplicate-tags ZigGlue wasm32: " ++ message;
        @memcpy(report[0..text.len], text);
        report_len = text.len;
    }
    failure_count += 1;
}

fn finishPass() void {
    const message = "PASS glue-runtime duplicate-tags ZigGlue wasm32";
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

fn okResult(comptime T: type) T {
    var result: T = undefined;
    const bytes: [*]u8 = @ptrCast(&result);
    @memset(bytes[0..@sizeOf(T)], 0);
    result.tag = .Ok;
    return result;
}

export fn roc_a_unit(arg0: abi.RocStr) callconv(.c) abi.AUnitResult {
    _ = arg0;
    return okResult(abi.AUnitResult);
}
export fn roc_a_str(arg0: abi.RocStr) callconv(.c) abi.AStrResult {
    _ = arg0;
    return okResult(abi.AStrResult);
}
export fn roc_a_bytes(arg0: abi.RocStr) callconv(.c) abi.ABytesResult {
    _ = arg0;
    return okResult(abi.ABytesResult);
}
export fn roc_a_record(arg0: abi.RocStr) callconv(.c) abi.ARecordResult {
    _ = arg0;
    return okResult(abi.ARecordResult);
}
export fn roc_a_nested(arg0: abi.RocStr) callconv(.c) abi.ANestedResult {
    _ = arg0;
    return okResult(abi.ANestedResult);
}

export fn roc_b_unit(arg0: abi.RocStr) callconv(.c) abi.BUnitResult {
    _ = arg0;
    return okResult(abi.BUnitResult);
}
export fn roc_b_str(arg0: abi.RocStr) callconv(.c) abi.BStrResult {
    _ = arg0;
    return okResult(abi.BStrResult);
}
export fn roc_b_bytes(arg0: abi.RocStr) callconv(.c) abi.BBytesResult {
    _ = arg0;
    return okResult(abi.BBytesResult);
}
export fn roc_b_record(arg0: abi.RocStr) callconv(.c) abi.BRecordResult {
    _ = arg0;
    return okResult(abi.BRecordResult);
}
export fn roc_b_nested(arg0: abi.RocStr) callconv(.c) abi.BNestedResult {
    _ = arg0;
    return okResult(abi.BNestedResult);
}

export fn roc_c_unit(arg0: abi.RocStr) callconv(.c) abi.CUnitResult {
    _ = arg0;
    return okResult(abi.CUnitResult);
}
export fn roc_c_str(arg0: abi.RocStr) callconv(.c) abi.CStrResult {
    _ = arg0;
    return okResult(abi.CStrResult);
}
export fn roc_c_bytes(arg0: abi.RocStr) callconv(.c) abi.CBytesResult {
    _ = arg0;
    return okResult(abi.CBytesResult);
}
export fn roc_c_record(arg0: abi.RocStr) callconv(.c) abi.CRecordResult {
    _ = arg0;
    return okResult(abi.CRecordResult);
}
export fn roc_c_nested(arg0: abi.RocStr) callconv(.c) abi.CNestedResult {
    _ = arg0;
    return okResult(abi.CNestedResult);
}

export fn roc_d_unit(arg0: abi.RocStr) callconv(.c) abi.DUnitResult {
    _ = arg0;
    return okResult(abi.DUnitResult);
}
export fn roc_d_str(arg0: abi.RocStr) callconv(.c) abi.DStrResult {
    _ = arg0;
    return okResult(abi.DStrResult);
}
export fn roc_d_bytes(arg0: abi.RocStr) callconv(.c) abi.DBytesResult {
    _ = arg0;
    return okResult(abi.DBytesResult);
}
export fn roc_d_record(arg0: abi.RocStr) callconv(.c) abi.DRecordResult {
    _ = arg0;
    return okResult(abi.DRecordResult);
}
export fn roc_d_nested(arg0: abi.RocStr) callconv(.c) abi.DNestedResult {
    _ = arg0;
    return okResult(abi.DNestedResult);
}

export fn roc_fallible_unit(arg0: abi.RocStr) callconv(.c) abi.HostFallible_unitResult {
    _ = arg0;
    return okResult(abi.HostFallible_unitResult);
}
export fn roc_fallible_str(arg0: abi.RocStr) callconv(.c) abi.HostFallible_strResult {
    _ = arg0;
    return okResult(abi.HostFallible_strResult);
}
export fn roc_fallible_bytes(arg0: abi.RocStr) callconv(.c) abi.HostFallible_bytesResult {
    _ = arg0;
    return okResult(abi.HostFallible_bytesResult);
}
export fn roc_fallible_record(arg0: abi.RocStr) callconv(.c) abi.HostFallible_recordResult {
    _ = arg0;
    return okResult(abi.HostFallible_recordResult);
}
export fn roc_fallible_nested(arg0: abi.RocStr) callconv(.c) abi.HostFallible_nestedResult {
    _ = arg0;
    return okResult(abi.HostFallible_nestedResult);
}

export fn wasm_main() [*]const u8 {
    failure_count = 0;
    report_len = 0;
    abi.roc_main();
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
