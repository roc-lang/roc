//! Death tests pinning the compiler invariants of the table storage types in
//! `collections`, which cannot reach `death_test` themselves.

const std = @import("std");
const collections = @import("collections");
const death_test = @import("death_test.zig");

const TestIdx = enum(u32) { _ };

fn readBeforeWrite() error{OutOfMemory}!void {
    var table = try collections.WriteFirstSlice(u32).init(std.heap.page_allocator, 2);
    table.set(0, 1);
    _ = table.get(1);
}

fn finishWithUnwrittenEntry() error{OutOfMemory}!void {
    var table = try collections.WriteFirstSlice(u32).init(std.heap.page_allocator, 3);
    table.set(0, 1);
    table.set(2, 1);
    _ = table.finish(std.heap.page_allocator);
}

noinline fn reservedIndex() TestIdx {
    return @fromBackingInt(std.math.maxInt(u32));
}

fn storeReservedIndex() error{OutOfMemory}!void {
    var column = try collections.OptionalIndexColumn(TestIdx).initFilled(std.heap.page_allocator, 1);
    column.set(0, reservedIndex());
}

fn growSealedList() error{OutOfMemory}!void {
    var list = try collections.ReservedList(u32, .dense).init(std.heap.page_allocator, 8);
    _ = try list.append(std.heap.page_allocator, 1);
    list.seal();
    _ = try list.append(std.heap.page_allocator, 2);
}

test "a write-first table read before its write fails the invariant" {
    try death_test.expectPanic("write-first table entry 1 read before it was written", readBeforeWrite, .{});
}

test "a write-first table handed over with an unwritten entry fails the invariant" {
    try death_test.expectPanic("write-first table handed over with entry 1 of 3 never written", finishWithUnwrittenEntry, .{});
}

test "an optional index column rejects the reserved index" {
    try death_test.expectPanic("reached unreachable code", storeReservedIndex, .{});
}

test "a sealed reserved list refuses to grow" {
    try death_test.expectPanic("reached unreachable code", growSealedList, .{});
}

fn readEpochOutsideLogicalDomain() error{OutOfMemory}!void {
    var set = try collections.EpochSet(u32, .stamps, .internal).init(std.heap.page_allocator, 1);
    _ = set.contains(1);
}

test "epoch sets reject spare capacity outside their logical domain" {
    try death_test.expectPanic("reached unreachable code", readEpochOutsideLogicalDomain, .{});
}
