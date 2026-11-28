//! Tests of the Debug-build stale-borrow hunt: `StaleBorrowAllocator`, and the
//! growth paths of `SafeList` and `ReservedList`, which move on every growth in
//! a Debug build.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");
const StaleBorrowAllocator = @import("stale_borrow_allocator");
const death_test = @import("death_test.zig");

const testing = std.testing;
const poison = StaleBorrowAllocator.poison_byte;
/// A wrapper over the test allocator with its own quarantine, so a test can
/// observe and flush it.
fn Wrapped(comptime config: StaleBorrowAllocator.Config) type {
    return struct {
        var instance: StaleBorrowAllocator = undefined;

        fn init() std.mem.Allocator {
            instance = .init(testing.allocator, config);
            return instance.allocator();
        }
    };
}

test "a wrapped list moves on every growth and poisons the backing it left" {
    const W = Wrapped(.{ .protect_pages = false });
    const gpa = W.init();
    defer W.instance.flush();

    var list: std.ArrayList(u32) = .empty;
    defer list.deinit(gpa);
    try list.ensureTotalCapacityPrecise(gpa, 4);
    try list.appendSlice(gpa, &.{ 1, 2, 3, 4 });
    const stale = list.items;
    try list.append(gpa, 5);

    try testing.expect(stale.ptr != list.items.ptr);
    try testing.expectEqualSlices(u32, &.{ 1, 2, 3, 4, 5 }, list.items);
    for (std.mem.sliceAsBytes(stale)) |byte| try testing.expectEqual(poison, byte);
    // A poisoned index is far outside any list.
    try testing.expect(stale[0] > std.math.maxInt(u31));
}

test "a wrapped allocation never changes length in place" {
    const W = Wrapped(.{ .protect_pages = false });
    const gpa = W.init();
    defer W.instance.flush();

    const memory = try gpa.alloc(u8, 64);
    defer gpa.free(memory);
    try testing.expect(!gpa.resize(memory, 128));
    try testing.expect(!gpa.resize(memory, 32));
    try testing.expect(gpa.remap(memory, 128) == null);
    try testing.expect(gpa.resize(memory, 64));
}

test "the quarantine is bounded in entries and in bytes" {
    const W = Wrapped(.{ .protect_pages = false, .max_poisoned = 4, .max_poisoned_bytes = 1000 });
    const gpa = W.init();
    defer W.instance.flush();

    for (0..10) |_| gpa.free(try gpa.alloc(u8, 16));
    try testing.expectEqual(@as(usize, 4), W.instance.quarantinedCount());
    try testing.expectEqual(@as(usize, 64), W.instance.quarantinedBytes());

    for (0..3) |_| gpa.free(try gpa.alloc(u8, 400));
    try testing.expect(W.instance.quarantinedBytes() <= 1000);
    try testing.expectEqual(@as(usize, 2), W.instance.quarantinedCount());

    W.instance.flush();
    try testing.expectEqual(@as(usize, 0), W.instance.quarantinedCount());
}

test "protected allocations are bounded in entries" {
    if (!StaleBorrowAllocator.page_protection_available) return error.SkipZigTest;
    const W = Wrapped(.{ .protect_min_pages = 1, .max_protected = 3 });
    const gpa = W.init();
    defer W.instance.flush();

    for (0..5) |_| gpa.free(try gpa.alloc(u8, std.heap.pageSize() + 1));
    try testing.expectEqual(@as(usize, 3), W.instance.quarantinedCount());
    try testing.expectEqual(3 * 2 * std.heap.pageSize(), W.instance.quarantinedBytes());
}

fn writeThroughStaleSlice() error{OutOfMemory}!void {
    const W = Wrapped(.{ .protect_pages = false });
    const gpa = W.init();
    var list: std.ArrayList(u32) = .empty;
    try list.ensureTotalCapacityPrecise(gpa, 2);
    list.appendSliceAssumeCapacity(&.{ 1, 2 });
    const stale = list.items;
    try list.append(gpa, 3);
    stale[1] = 7;
    W.instance.flush();
}

test "a write through a stale slice of a small list panics when its backing leaves quarantine" {
    try death_test.expectPanic("stale borrow: byte 4 of a freed 8-byte allocation", writeThroughStaleSlice, .{});
}

fn readThroughStaleSliceOfLargeList() error{OutOfMemory}!void {
    const W = Wrapped(.{});
    const gpa = W.init();
    var list: std.ArrayList(u8) = .empty;
    const len = 4 * std.heap.pageSize();
    try list.ensureTotalCapacityPrecise(gpa, len);
    list.appendNTimesAssumeCapacity(1, len);
    const stale = list.items;
    try list.append(gpa, 2);
    std.mem.doNotOptimizeAway(@as(*volatile u8, &stale[len / 2]).*);
}

test "a read through a stale slice of a large list faults" {
    if (!StaleBorrowAllocator.page_protection_available) return error.SkipZigTest;
    try death_test.expectPanic(" at address 0x", readThroughStaleSliceOfLargeList, .{});
}

test "a SafeList moves on every growth even where the allocator could grow it in place" {
    var buffer: [4096]u8 align(16) = undefined;
    var fixed = std.heap.FixedBufferAllocator.init(&buffer);
    const gpa = fixed.allocator();

    var list: collections.SafeList(u32) = .{};
    try list.ensureTotalCapacityPrecise(gpa, 2);
    _ = try list.appendSlice(gpa, &.{ 1, 2 });
    const stale = list.items();
    // A fixed buffer allocator grows its last allocation in place.
    try testing.expect(gpa.resize(std.mem.sliceAsBytes(stale), 16));
    try testing.expect(gpa.resize(std.mem.sliceAsBytes(stale), 8));
    try list.ensureTotalCapacityPrecise(gpa, 3);
    try testing.expect(stale.ptr != list.items().ptr);
    try testing.expectEqualSlices(u32, &.{ 1, 2 }, list.items());
    for (std.mem.sliceAsBytes(stale)) |byte| try testing.expectEqual(@as(u8, 0xAA), byte);
}

test "a SafeList overwrites the items a shrink drops" {
    const gpa = testing.allocator;
    var list: collections.SafeList(u32) = .{};
    defer list.deinit(gpa);
    _ = try list.appendSlice(gpa, &.{ 1, 2, 3 });
    const all = list.items();
    list.shrinkRetainingCapacity(1);
    try testing.expectEqual(@as(u32, 0xAAAAAAAA), all[1]);
    try testing.expectEqual(@as(u32, 0xAAAAAAAA), all[2]);
    try testing.expectEqual(@as(?u32, 1), list.pop());
    try testing.expectEqual(@as(u32, 0xAAAAAAAA), all[0]);
}

const ReservedU32 = collections.ReservedList(u32, .dense);
/// The size at which a column starts on a reservation.
const reserve_threshold_bytes = collections.reserved_list_threshold_bytes;

test "a reserved column moves to a fresh reservation on every growth step" {
    const gpa = testing.allocator;
    var list = try ReservedU32.init(gpa, reserve_threshold_bytes / @sizeOf(u32));
    defer list.deinit(gpa);
    _ = try list.append(gpa, 1);
    const first = list.items.ptr;
    const step = list.capacity;
    for (0..step) |i| _ = try list.append(gpa, @intCast(i));
    try testing.expect(first != list.items.ptr);
    try testing.expectEqual(@as(u32, 1), list.items[0]);
    try testing.expectEqual(@as(u32, @intCast(step - 1)), list.items[step]);
}

fn readThroughStaleSliceOfReservedColumn() error{OutOfMemory}!void {
    const gpa = std.heap.page_allocator;
    var list = try ReservedU32.init(gpa, reserve_threshold_bytes / @sizeOf(u32));
    _ = try list.append(gpa, 1);
    const stale = list.items;
    for (0..list.capacity) |_| _ = try list.append(gpa, 2);
    std.mem.doNotOptimizeAway(@as(*volatile u32, &stale[0]).*);
}

test "a read through a stale slice of a reserved column faults" {
    if (builtin.os.tag == .windows or builtin.cpu.arch.isWasm()) return error.SkipZigTest;
    try death_test.expectPanic(" at address 0x", readThroughStaleSliceOfReservedColumn, .{});
}
