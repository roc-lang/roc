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
