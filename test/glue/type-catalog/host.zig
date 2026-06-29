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
        const text = std.fmt.bufPrint(&report, "FAIL type-catalog ZigGlue: " ++ fmt, args) catch "FAIL type-catalog ZigGlue: report overflow";
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
    if (!std.mem.eql(u8, str.asSlice(), expected)) {
        fail("{s} mismatch", .{label});
    }
}

fn runContract() void {
    const point = abi.roc_point();
    if (point.@"x" != -17 or point.@"y" != 42) fail("point mismatch", .{});

    const structural = abi.roc_structural();
    if (structural.@"count" != 19) fail("structural count mismatch", .{});
    expectStr(&structural.@"name", "catalog", "structural name");
    if (structural.@"nested".@"byte" != 7 or structural.@"nested".@"flag" != true) {
        fail("structural nested mismatch", .{});
    }

    const result_a = abi.roc_result_a();
    if (result_a.tag != .Ok) fail("A.Result tag mismatch", .{});
    var a_payload = result_a.payload_ok();
    expectStr(&a_payload, "alpha", "A.Result payload");

    const result_b = abi.roc_result_b();
    if (result_b.tag != .Err) fail("B.Result tag mismatch", .{});
    const b_payload = result_b.payload_err();
    if (b_payload.@"code" != 5) fail("B.Result code mismatch", .{});
    expectStr(&b_payload.@"message", "bravo", "B.Result message");

    if (abi.roc_dec(12.5) != 12.5) fail("Dec identity mismatch", .{});
    if (abi.roc_i128(-123456789) != -123456789) fail("I128 identity mismatch", .{});
    if (abi.roc_u128(123456789) != 123456789) fail("U128 identity mismatch", .{});

    result_a.decref(&roc_host);
    result_b.decref(&roc_host);
    structural.decref(&roc_host);
}

export fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    _ = .{ argc, argv };
    runContract();
    if (failure_count != 0) {
        const message = if (report_len == 0) "FAIL type-catalog ZigGlue: unknown failure" else report[0..report_len];
        stderrPrint("{s}\nalloc={} dealloc={}\n", .{ message, alloc_count, dealloc_count });
        return 1;
    }
    stderrPrint("PASS glue-runtime type-catalog ZigGlue native alloc={} dealloc={}\n", .{ alloc_count, dealloc_count });
    return 0;
}
