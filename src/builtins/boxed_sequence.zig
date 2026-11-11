//! Boxed iterator and stream handles share an erased step's owning allocation.
//!
//! The step's private captures stay inline. A suffix holds the ordinary
//! sequence record, with a borrowed pointer back to this same allocation as
//! its step. Copying that view gives an unboxed sequence its own step reference.
//! Hosts see neither the record nor the captures.

const std = @import("std");
const callable = @import("erased_callable.zig");
const utils = @import("utils.zig");

/// Maximum size of the compiler-owned sequence record on supported targets.
pub const state_capacity = 32;

fn stateOffset(capture_size: usize) usize {
    return std.mem.alignForward(usize, callable.payloadSize(capture_size), callable.capture_alignment);
}

/// Consume a sequence record's step into one allocation containing both its
/// captures and the record. The producer supplies the committed step offset.
pub fn pack(record: [*]const u8, record_size: usize, step_offset: usize, ops: *utils.RocOps) [*]u8 {
    std.debug.assert(record_size <= state_capacity);
    std.debug.assert(step_offset + @sizeOf(usize) <= record_size);
    // The input may be the view at the end of the very handle being resized.
    var snapshot: [state_capacity]u8 = undefined;
    @memcpy(snapshot[0..record_size], record[0..record_size]);
    const step = @as(*align(1) const [*]u8, @ptrCast(snapshot[step_offset..].ptr)).*;
    const header = callable.payloadPtr(step).*;
    const unique = utils.isUnique(step, ops);
    const live_size = stateOffset(header.capture_size);
    const size = utils.checkedAllocLen(live_size, record_size, ops);
    const result = if (unique)
        // Only the header and captures are live; any old suffix is replaced.
        utils.unsafeReallocate(step, callable.payload_alignment, callable.payloadSize(header.capture_size), size, 1, false, ops)
    else blk: {
        const copy = utils.allocateWithRefcount(size, callable.payload_alignment, false, ops);
        callable.payloadPtr(copy).* = header;
        @memcpy(callable.capturePtr(copy)[0..header.capture_size], callable.capturePtr(step)[0..header.capture_size]);
        if (header.on_copy) |retain| retain(callable.capturePtr(copy), 1);
        callable.decref(step, ops);
        break :blk copy;
    };
    const suffix = result + live_size;
    @memcpy(suffix[0..record_size], snapshot[0..record_size]);
    @as(*align(1) [*]u8, @ptrCast(suffix + step_offset)).* = result;
    return result;
}

/// Borrow the ordinary sequence record stored after a handle's private captures.
pub fn recordPtr(handle: [*]u8) [*]u8 {
    return handle + stateOffset(callable.payloadPtr(handle).capture_size);
}

/// Copy the borrowed record view; the caller's ARC retains its step reference.
pub fn unpack(out: [*]u8, handle: [*]u8, record_size: usize) void {
    std.debug.assert(record_size <= state_capacity);
    @memcpy(out[0..record_size], recordPtr(handle)[0..record_size]);
}

const TestRecord = extern struct {
    length: [3]u8,
    step: [*]u8,
};

const TestCapture = extern struct {
    refs: *usize,
    bytes: [37]u8,
};

fn testCall(_: *utils.RocOps, _: ?[*]u8, _: ?[*]const u8, _: ?[*]u8, _: ?[*]u8, _: *?*const anyopaque) callconv(.c) void {}

fn testDrop(ptr: ?[*]u8, _: *utils.RocOps) callconv(.c) void {
    const capture: *TestCapture = @ptrCast(@alignCast(ptr.?));
    capture.refs.* -= 1;
}

fn testRetain(ptr: ?[*]u8, amount: isize) callconv(.c) void {
    const capture: *TestCapture = @ptrCast(@alignCast(ptr.?));
    capture.refs.* += @intCast(amount);
}

test "boxed sequence moves a unique capture into one owning allocation" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    var refs: usize = 1;
    const step = callable.allocate(@ptrCast(&testCall), &testDrop, @sizeOf(TestCapture), env.getOps());
    callable.payloadPtr(step).on_copy = &testRetain;
    const capture: *TestCapture = @ptrCast(@alignCast(callable.capturePtr(step)));
    capture.* = .{ .refs = &refs, .bytes = @splat(73) };
    const record = TestRecord{ .length = .{ 3, 4, 5 }, .step = step };
    const handle = pack(@ptrCast(&record), @sizeOf(TestRecord), @offsetOf(TestRecord, "step"), env.getOps());
    try std.testing.expectEqual(@as(usize, 1), env.getAllocationCount());
    try std.testing.expectEqual(@as(usize, 1), refs);
    var restored: TestRecord = undefined;
    unpack(@ptrCast(&restored), handle, @sizeOf(TestRecord));
    try std.testing.expectEqualSlices(u8, &record.length, &restored.length);
    try std.testing.expectEqual(handle, restored.step);
    const stored: *TestCapture = @ptrCast(@alignCast(callable.capturePtr(handle)));
    try std.testing.expectEqualSlices(u8, &(@as([37]u8, @splat(73))), &stored.bytes);
    callable.decref(handle, env.getOps());
    try std.testing.expectEqual(@as(usize, 0), refs);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "boxed sequence copies a shared capture without changing the original" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    var refs: usize = 1;
    const step = callable.allocate(@ptrCast(&testCall), &testDrop, @sizeOf(TestCapture), env.getOps());
    callable.payloadPtr(step).on_copy = &testRetain;
    const capture: *TestCapture = @ptrCast(@alignCast(callable.capturePtr(step)));
    capture.* = .{ .refs = &refs, .bytes = @splat(42) };
    callable.incref(step, 1, env.getOps());
    const record = TestRecord{ .length = .{ 7, 0, 0 }, .step = step };
    const handle = pack(@ptrCast(&record), @sizeOf(TestRecord), @offsetOf(TestRecord, "step"), env.getOps());
    try std.testing.expectEqual(@as(usize, 2), refs);
    try std.testing.expectEqual(@as(usize, 2), env.getAllocationCount());
    callable.decref(step, env.getOps());
    try std.testing.expectEqual(@as(usize, 1), refs);
    callable.decref(handle, env.getOps());
    try std.testing.expectEqual(@as(usize, 0), refs);
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}

test "boxed sequence aligns its record and repacks its own borrowed view" {
    var env = utils.TestEnv.init(std.testing.allocator);
    defer env.deinit();
    const step = callable.allocate(&testCall, null, 1, env.getOps());
    callable.capturePtr(step)[0] = 9;
    const record = TestRecord{ .length = .{ 1, 2, 3 }, .step = step };
    const first = pack(@ptrCast(&record), @sizeOf(TestRecord), @offsetOf(TestRecord, "step"), env.getOps());
    try std.testing.expectEqual(@as(usize, 0), @intFromPtr(recordPtr(first)) % @alignOf(TestRecord));
    const second = pack(recordPtr(first), @sizeOf(TestRecord), @offsetOf(TestRecord, "step"), env.getOps());
    try std.testing.expectEqual(@as(usize, 1), env.getAllocationCount());
    try std.testing.expectEqual(@as(u8, 9), callable.capturePtr(second)[0]);
    const view: *const TestRecord = @ptrCast(@alignCast(recordPtr(second)));
    try std.testing.expectEqualSlices(u8, &record.length, &view.length);
    try std.testing.expectEqual(second, view.step);
    callable.decref(second, env.getOps());
    try std.testing.expectEqual(@as(usize, 0), env.getAllocationCount());
}
