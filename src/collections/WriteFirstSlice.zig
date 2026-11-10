//! A table whose entries are each written before any of them is read: the
//! scatter target of a CSR build, a postorder, a prefix-sum output, a result
//! column produced by one sweep.
//!
//! Release builds allocate it uninitialized and index it directly: it is
//! exactly a `[]T`. Builds with runtime safety poison the entries, track a
//! written bit per entry, fail a compiler invariant on any read of an
//! unwritten entry, and fail one again at hand-over (`finish`) if an entry was
//! never written.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// Whether this build tracks which entries were written.
pub const tracks_writes = std.debug.runtime_safety;

/// A slice of `T` whose entries must each be written before being read.
pub fn WriteFirstSlice(comptime T: type) type {
    return struct {
        const Self = @This();

        entries: []T,
        written: if (tracks_writes) std.DynamicBitSetUnmanaged else void,

        /// Allocate `entry_count` uninitialized entries.
        pub fn init(gpa: Allocator, entry_count: usize) Allocator.Error!Self {
            const entries = try gpa.alloc(T, entry_count);
            errdefer gpa.free(entries);
            return wrap(gpa, entries);
        }

        /// Track writes to `entries`, which the caller owns and which hold no
        /// value yet (for example a list's newly added suffix).
        pub fn wrap(gpa: Allocator, entries: []T) Allocator.Error!Self {
            if (comptime !tracks_writes) return .{ .entries = entries, .written = {} };
            @memset(entries, undefined);
            return .{ .entries = entries, .written = try .initEmpty(gpa, entries.len) };
        }

        /// Free the entries of a table made by `init` that will not be
        /// handed over, for example on an error path.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.release(gpa);
            gpa.free(self.entries);
            self.entries = &.{};
        }

        /// Stop tracking a table made by `wrap` that will not be handed
        /// over. The caller still owns the entries.
        pub fn release(self: *Self, gpa: Allocator) void {
            if (comptime tracks_writes) {
                self.written.deinit(gpa);
                self.written = .{};
            }
        }

        /// The number of entries.
        pub inline fn len(self: *const Self) usize {
            return self.entries.len;
        }

        /// Write entry `index`.
        pub inline fn set(self: *Self, index: usize, value: T) void {
            self.entries[index] = value;
            if (comptime tracks_writes) self.written.set(index);
        }

        /// Read entry `index`, which must have been written.
        pub inline fn get(self: *const Self, index: usize) T {
            if (comptime tracks_writes) {
                if (!self.written.isSet(index)) std.debug.panic("write-first table entry {d} read before it was written", .{index});
            }
            return self.entries[index];
        }

        /// Hand the table over once every entry has been written. The returned
        /// slice is the caller's: for a table made by `init` it is freed with
        /// the same allocator.
        pub fn finish(self: *Self, gpa: Allocator) []T {
            if (comptime tracks_writes) {
                if (self.written.count() != self.entries.len) {
                    var unwritten = self.written.iterator(.{ .kind = .unset });
                    std.debug.panic("write-first table handed over with entry {d} of {d} never written", .{ unwritten.next().?, self.entries.len });
                }
                self.release(gpa);
            }
            const entries = self.entries;
            self.entries = &.{};
            return entries;
        }
    };
}

const testing = std.testing;

test "entries written in any order are handed over intact" {
    var table = try WriteFirstSlice(u32).init(testing.allocator, 4);
    for ([_]usize{ 2, 0, 3, 1 }) |index| table.set(index, @intCast(index * 10));
    try testing.expectEqual(@as(u32, 20), table.get(2));
    const entries = table.finish(testing.allocator);
    defer testing.allocator.free(entries);
    try testing.expectEqualSlices(u32, &.{ 0, 10, 20, 30 }, entries);
}

test "a wrapped suffix is tracked without being owned" {
    var backing: [3]u16 = .{ 1, 2, 3 };
    var table = try WriteFirstSlice(u16).wrap(testing.allocator, backing[1..]);
    table.set(0, 7);
    table.set(1, 8);
    _ = table.finish(testing.allocator);
    try testing.expectEqualSlices(u16, &.{ 1, 7, 8 }, &backing);
}

test "release builds store exactly a slice" {
    if (tracks_writes) return error.SkipZigTest;
    try testing.expectEqual(@sizeOf([]u64), @sizeOf(WriteFirstSlice(u64)));
}

test "builds with runtime safety track one bit per entry" {
    if (!tracks_writes) return error.SkipZigTest;
    var table = try WriteFirstSlice(u8).init(testing.allocator, 70);
    defer table.deinit(testing.allocator);
    try testing.expectEqual(@as(usize, 70), table.written.bit_length);
    table.set(69, 1);
    try testing.expectEqual(@as(usize, 1), table.written.count());
}

test "handing over a write-first table leaves safe deferred cleanup" {
    var table = try WriteFirstSlice(u32).init(testing.allocator, 1);
    defer table.deinit(testing.allocator);
    table.set(0, 13);
    const entries = table.finish(testing.allocator);
    defer testing.allocator.free(entries);
    try testing.expectEqual(@as(u32, 13), entries[0]);
}
