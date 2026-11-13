//! Model inlined optional-index reads, with or without a payload lookup.
const std = @import("std");
const collections = @import("collections");
const direct = @import("variant").direct;
const Id = enum(u32) { _ };
const Column = if (direct) collections.ZeroSentinelIndexColumn(Id) else collections.OptionalIndexColumn(Id);

noinline fn payloadReads(column: *const Column, payloads: []const u64, rounds: usize) u64 {
    var total: u64 = 0;
    for (0..rounds) |_| {
        for (0..column.len()) |i| {
            if (column.get(i)) |id| total +%= payloads[@backingInt(id)];
        }
    }
    return total;
}

noinline fn indexReads(column: *const Column, rounds: usize) u64 {
    var total: u64 = 0;
    for (0..rounds) |_| {
        for (0..column.len()) |i| {
            if (column.get(i)) |id| total +%= @backingInt(id);
        }
    }
    return total;
}

pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 4) return error.ExpectedLengthRoundsAndMode;
    const len = try std.fmt.parseInt(usize, args[1], 10);
    const rounds = try std.fmt.parseInt(usize, args[2], 10);
    if (len >= std.math.maxInt(u32)) return error.DomainTooLarge;
    const payload_mode = std.mem.eql(u8, args[3], "payload");
    if (!payload_mode and !std.mem.eql(u8, args[3], "index")) return error.InvalidMode;
    const gpa = std.heap.c_allocator;
    var column = try Column.initFilled(gpa, len);
    defer column.deinit(gpa);
    var payloads: std.ArrayList(u64) = .empty;
    defer payloads.deinit(gpa);
    if (direct) try payloads.append(gpa, 0);
    var expected_payload: u64 = 0;
    var expected_indices: u64 = 0;
    var present: u64 = 0;
    for (0..len) |i| {
        if (i % 4 == 3) continue;
        const id: Id = @fromBackingInt(@intCast(payloads.items.len));
        const value = @as(u64, i) *% 0x9e3779b97f4a7c15;
        try payloads.append(gpa, value);
        column.set(i, id);
        expected_payload +%= value;
        expected_indices +%= @backingInt(id);
        present += 1;
    }
    const result = if (payload_mode) payloadReads(&column, payloads.items, rounds) else indexReads(&column, rounds);
    const expected = (if (payload_mode) expected_payload else expected_indices) *% rounds;
    if (result != expected) return error.IncorrectResult;
    // Normalize outside the measured loop, not on every direct-index lookup.
    const normalized = if (!payload_mode and direct) result -% (present *% rounds) else result;
    std.debug.print("result={d} table_bytes={d} payload_bytes={d}\n", .{ normalized, len * 4, payloads.items.len * 8 });
}
