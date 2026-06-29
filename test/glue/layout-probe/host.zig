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
        const text = std.fmt.bufPrint(&report, "FAIL layout-probe ZigGlue: " ++ fmt, args) catch "FAIL layout-probe ZigGlue: report overflow";
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

fn hostAlloc(_: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRaw(length, alignment);
}

fn hostDealloc(_: *abi.RocHost, ptr: *anyopaque, _: usize) callconv(.c) void {
    deallocRaw(ptr);
}

fn hostRealloc(_: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    deallocRaw(ptr);
    return allocRaw(new_length, alignment);
}

fn hostDbg(_: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    writeStderr(bytes[0..len]);
    writeStderr("\n");
}

fn hostExpectFailed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_expect_failed", .{});
}

fn hostCrashed(_: *abi.RocHost, _: [*]const u8, _: usize) callconv(.c) void {
    fail("roc_crashed", .{});
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

fn runContract() c_int {
    abi.roc_main();
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
