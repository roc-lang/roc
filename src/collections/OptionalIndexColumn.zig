//! A dense column of optional typed indices in four bytes per entry.
//!
//! Entry 0 means absent and `n + 1` means index `n`. Every store's length is
//! bounded by `SafeList`'s `maxInt(u32)` rows, with `maxInt(u32)` reserved as
//! the empty-range start, so the largest real index is `maxInt(u32) - 1` and
//! `n + 1` never wraps; `set` asserts that bound.
//!
//! The storage policy is chosen once, by the constructor: `initFilled` is an
//! ordinary allocation cleared to absent, for columns whose entries are mostly
//! touched; `initSparse` maps fresh zero pages, for columns sized by a large
//! domain of which few entries are touched, so untouched pages are never
//! resident.

const std = @import("std");
const storage_work = @import("storage_work.zig");
const zeroed_pages = @import("zeroed_pages.zig");

const Allocator = std.mem.Allocator;

/// A column of optional `Idx` values, where `Idx` is a typed index enum with a
/// `u32` backing integer.
pub fn OptionalIndexColumn(comptime Idx: type) type {
    return IndexColumn(Idx, false);
}

/// Optional indices whose owning store reserves physical slot zero. The owner
/// starts issuing IDs at one; this column never biases or decodes those IDs.
pub fn ZeroSentinelIndexColumn(comptime Idx: type) type {
    return IndexColumn(Idx, true);
}

fn IndexColumn(comptime Idx: type, comptime reserved_zero: bool) type {
    comptime {
        const info = @typeInfo(Idx);
        if (info != .@"enum" or info.@"enum".tag_type != u32) {
            @compileError("OptionalIndexColumn stores typed u32 index enums, not " ++ @typeName(Idx));
        }
    }
    return struct {
        const Self = @This();

        /// Zero is absent; the owner-selected policy determines whether IDs are biased.
        words: []u32 = &.{},
        /// The mapping that backs `words` for a sparse column; empty for a
        /// filled one.
        pages: zeroed_pages.Pages = .{ .bytes = &.{} },

        /// A column of `entry_count` absent entries in an ordinary allocation.
        pub fn initFilled(gpa: Allocator, entry_count: usize) Allocator.Error!Self {
            const words = try gpa.alloc(u32, entry_count);
            @memset(words, 0);
            storage_work.add(.bytes_filled, words.len * @sizeOf(u32));
            return .{ .words = words };
        }

        /// A column of `entry_count` absent entries on fresh zero pages, with
        /// no fill.
        pub fn initSparse(entry_count: usize) Allocator.Error!Self {
            const bytes = std.math.mul(usize, entry_count, @sizeOf(u32)) catch return error.OutOfMemory;
            const pages = try zeroed_pages.map(bytes, .sparse);
            const words: [*]u32 = @ptrCast(pages.bytes.ptr);
            return .{ .words = words[0..entry_count], .pages = pages };
        }

        /// Free the column.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            if (self.pages.bytes.len != 0) {
                self.pages.unmap();
            } else {
                gpa.free(self.words);
            }
            self.* = .{};
        }

        /// The number of entries.
        pub inline fn len(self: *const Self) usize {
            return self.words.len;
        }

        /// The index at entry `i`, or null when it is absent.
        pub inline fn get(self: *const Self, i: usize) ?Idx {
            return self.view().get(i);
        }

        /// A borrowed column which owns neither its allocation nor its pages.
        pub const View = struct {
            words: []const u32 = &.{},

            /// The number of entries.
            pub inline fn len(self: View) usize {
                return self.words.len;
            }

            /// The optional index at one entry.
            pub inline fn get(self: View, i: usize) ?Idx {
                const word = self.words[i];
                if (word == 0) return null;
                return @fromBackingInt(if (reserved_zero) word else word - 1);
            }
        };

        /// Borrow this column without transferring ownership.
        pub inline fn view(self: *const Self) View {
            return .{ .words = self.words };
        }

        /// Whether entry `i` holds an index.
        pub inline fn isPresent(self: *const Self, i: usize) bool {
            return self.words[i] != 0;
        }

        /// Store `idx` at entry `i`.
        pub inline fn set(self: *Self, i: usize, idx: Idx) void {
            const raw: u32 = @backingInt(idx);
            std.debug.assert(raw < std.math.maxInt(u32));
            if (reserved_zero) std.debug.assert(raw != 0);
            self.words[i] = if (reserved_zero) raw else raw + 1;
        }

        /// Make entry `i` absent.
        pub inline fn clear(self: *Self, i: usize) void {
            self.words[i] = 0;
        }

        /// Make every entry of a filled column absent.
        pub fn clearAll(self: *Self) void {
            std.debug.assert(self.pages.bytes.len == 0);
            @memset(self.words, 0);
            storage_work.add(.bytes_filled, self.words.len * @sizeOf(u32));
        }

        /// Grow or shrink a filled column to `new_len`; new entries are absent.
        pub fn resize(self: *Self, gpa: Allocator, new_len: usize) Allocator.Error!void {
            std.debug.assert(self.pages.bytes.len == 0);
            const old_len = self.words.len;
            self.words = try gpa.realloc(self.words, new_len);
            if (new_len > old_len) {
                @memset(self.words[old_len..], 0);
                storage_work.add(.bytes_filled, (new_len - old_len) * @sizeOf(u32));
            }
        }
    };
}

const testing = std.testing;
const TestIdx = enum(u32) { first = 0, _ };

test "the first and the largest real index round-trip" {
    var column = try OptionalIndexColumn(TestIdx).initFilled(testing.allocator, 3);
    defer column.deinit(testing.allocator);
    try testing.expectEqual(@as(?TestIdx, null), column.get(0));
    column.set(0, .first);
    column.set(2, @fromBackingInt(std.math.maxInt(u32) - 1));
    try testing.expectEqual(@as(?TestIdx, .first), column.get(0));
    try testing.expect(!column.isPresent(1));
    try testing.expectEqual(@as(?TestIdx, @fromBackingInt(std.math.maxInt(u32) - 1)), column.get(2));
}

test "a cleared entry is absent" {
    var column = try OptionalIndexColumn(TestIdx).initFilled(testing.allocator, 2);
    defer column.deinit(testing.allocator);
    column.set(1, @fromBackingInt(5));
    column.clear(1);
    try testing.expectEqual(@as(?TestIdx, null), column.get(1));
    column.set(0, @fromBackingInt(9));
    column.clearAll();
    try testing.expect(!column.isPresent(0));
}

test "resizing a filled column adds absent entries" {
    var column = try OptionalIndexColumn(TestIdx).initFilled(testing.allocator, 1);
    defer column.deinit(testing.allocator);
    column.set(0, @fromBackingInt(4));
    try column.resize(testing.allocator, 5);
    try testing.expectEqual(@as(?TestIdx, @fromBackingInt(4)), column.get(0));
    for (1..5) |i| try testing.expect(!column.isPresent(i));
}

test "a sparse column reads absent without a fill" {
    const filled_before = storage_work.get(.bytes_filled);
    const mapped_before = storage_work.get(.mappings);
    var column = try OptionalIndexColumn(TestIdx).initSparse(1 << 20);
    defer column.deinit(testing.allocator);
    try testing.expectEqual(@as(?TestIdx, null), column.get(0));
    try testing.expectEqual(@as(?TestIdx, null), column.get((1 << 20) - 1));
    column.set(12345, @fromBackingInt(77));
    try testing.expectEqual(@as(?TestIdx, @fromBackingInt(77)), column.get(12345));
    try testing.expectEqual(filled_before, storage_work.get(.bytes_filled));
    if (storage_work.enabled) try testing.expectEqual(mapped_before + 1, storage_work.get(.mappings));
}

test "reserved-zero columns store real physical indices without encoding" {
    var column = try ZeroSentinelIndexColumn(TestIdx).initFilled(testing.allocator, 3);
    defer column.deinit(testing.allocator);
    column.set(0, @fromBackingInt(1));
    column.set(2, @fromBackingInt(std.math.maxInt(u32) - 1));
    try testing.expectEqual(@as(u32, 1), column.words[0]);
    try testing.expectEqual(@as(u32, std.math.maxInt(u32) - 1), column.words[2]);
    try testing.expectEqual(@as(?TestIdx, @fromBackingInt(1)), column.get(0));
    try testing.expect(column.get(1) == null);
    column.clear(0);
    try testing.expect(column.get(0) == null);
}
