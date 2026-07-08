const std = @import("std");
const abi = @import("roc_platform_abi.zig");

extern fn malloc(size: usize) callconv(.c) ?*anyopaque;
extern fn free(ptr: ?*anyopaque) callconv(.c) void;
extern fn write(fd: c_int, buf: [*]const u8, count: usize) callconv(.c) isize;

var failure_count: usize = 0;
var report: [512]u8 = [_]u8{0} ** 512;
var report_len: usize = 0;
var alloc_count: usize = 0;
var dealloc_count: usize = 0;

fn fail(comptime fmt: []const u8, args: anytype) void {
    if (failure_count == 0) {
        const text = std.fmt.bufPrint(&report, "FAIL duplicate-tags ZigGlue: " ++ fmt, args) catch "FAIL duplicate-tags ZigGlue: report overflow";
        report_len = text.len;
    }
    failure_count += 1;
}

fn writeStderr(bytes: []const u8) void {
    if (bytes.len != 0) _ = write(2, bytes.ptr, bytes.len);
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

fn okResult(comptime T: type) T {
    var result = std.mem.zeroes(T);
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

fn runContract() c_int {
    abi.roc_main();
    if (failure_count != 0) {
        const message = if (report_len == 0) "FAIL duplicate-tags ZigGlue: unknown failure" else report[0..report_len];
        stderrPrint("{s}\nalloc={} dealloc={}\n", .{ message, alloc_count, dealloc_count });
        return 1;
    }
    stderrPrint("PASS glue-runtime duplicate-tags ZigGlue native alloc={} dealloc={}\n", .{ alloc_count, dealloc_count });
    return 0;
}

export fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = .{ argc, argv };
    return runContract();
}
