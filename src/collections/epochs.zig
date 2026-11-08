//! Scratch that is reset many times: visited sets, on-stack flags, SCC
//! indices and low-links, per-procedure solver tables.
//!
//! Each entry carries a `u32` stamp and is valid only when its stamp equals
//! the current epoch, so a reset is one increment and data riding alongside a
//! stamp is never initialized: it is read only when its stamp matches, which
//! makes it a write-before-read table.
//!
//! The wrap rule is defined once, here: when the epoch would reach
//! `maxInt(u32)`, the stamps are cleared once and the epoch restarts at 1.
//! Stamp 0 is never an epoch, so a cleared or newly grown stamp is never
//! valid.
//!
//! A site whose reset boundary already has a monotonically increasing
//! identifier (the procedure index within a pass) supplies it as a natural
//! epoch through `resetTo` instead of counting resets.
//!
//! Capacity is retained across resets; `ensureLen` clears only the new
//! suffix of the stamps.

const std = @import("std");
const storage_work = @import("storage_work.zig");
const zeroed_pages = @import("zeroed_pages.zig");

const Allocator = std.mem.Allocator;

/// Where a set's epochs come from.
pub const EpochSource = enum {
    /// `reset` increments an internal counter.
    internal,
    /// `resetTo` installs an increasing identifier the site already has.
    natural,
};

/// How a flag-only set stores membership.
pub const SetRepresentation = enum {
    /// A stamp per entry: reset is one increment. For large domains.
    stamps,
    /// A bit per entry, cleared wholesale on reset. For small domains.
    bitset,
};

fn keyIndex(comptime Key: type, k: Key) usize {
    const info = @typeInfo(Key);
    if (info == .@"enum") return @backingInt(k);
    if (info == .int) return k;
    @compileError("epoch tables are keyed by an integer or a typed index enum, not " ++ @typeName(Key));
}

const first_epoch: u32 = 1;

/// The shared epoch rule: the current epoch, its advance, and its wrap.
const Epoch = struct {
    current: u32 = first_epoch,

    /// Advance to the next epoch. Returns true when the stamps must be
    /// cleared because the epoch wrapped.
    fn advance(self: *Epoch) bool {
        storage_work.add(.epoch_resets, 1);
        if (self.current + 1 == std.math.maxInt(u32)) {
            storage_work.add(.epoch_wraps, 1);
            self.current = first_epoch;
            return true;
        }
        self.current += 1;
        return false;
    }

    /// Install a natural epoch `identifier`, which must exceed every
    /// identifier installed since the stamps were last cleared.
    fn install(self: *Epoch, identifier: u32) void {
        std.debug.assert(identifier < std.math.maxInt(u32));
        const next = identifier + first_epoch;
        std.debug.assert(next > self.current);
        storage_work.add(.epoch_resets, 1);
        self.current = next;
    }
};

fn growStamps(gpa: Allocator, stamps: *[]u32, new_len: usize) Allocator.Error!void {
    const old_len = stamps.len;
    if (new_len <= old_len) return;
    stamps.* = try gpa.realloc(stamps.*, new_len);
    @memset(stamps.*[old_len..], 0);
    storage_work.add(.bytes_filled, (new_len - old_len) * @sizeOf(u32));
}

fn clearStamps(stamps: []u32) void {
    @memset(stamps, 0);
    storage_work.add(.bytes_filled, stamps.len * @sizeOf(u32));
}

/// A set of `Key`s that empties in constant time.
pub fn EpochSet(comptime Key: type, comptime representation: SetRepresentation, comptime source: EpochSource) type {
    if (representation == .bitset and source == .natural) {
        @compileError("a wholesale-cleared bitset has no epochs to supply");
    }
    return struct {
        const Self = @This();

        storage: switch (representation) {
            .stamps => []u32,
            .bitset => std.DynamicBitSetUnmanaged,
        } = switch (representation) {
            .stamps => &.{},
            .bitset => .{},
        },
        epoch: if (representation == .stamps) Epoch else void = if (representation == .stamps) .{ .current = if (source == .natural) 0 else first_epoch } else {},

        logical_len: usize = 0,
        borrowed: bool = false,
        pages: zeroed_pages.Pages = .{ .bytes = &.{} },

        /// An empty fixed-domain stamped set on fresh zero pages.
        pub fn initSparse(key_count: usize) Allocator.Error!Self {
            if (representation != .stamps) @compileError("sparse epoch storage requires stamps");
            const bytes = std.math.mul(usize, key_count, @sizeOf(u32)) catch return error.OutOfMemory;
            const pages = try zeroed_pages.map(bytes, .sparse);
            return .{
                .storage = @as([*]u32, @ptrCast(pages.bytes.ptr))[0..key_count],
                .logical_len = key_count,
                .pages = pages,
            };
        }

        /// Borrow an owner's zeroed stamp domain without taking its storage.
        /// The owner must not reuse that domain while this cursor is active.
        pub fn borrow(stamps: []u32) Self {
            if (representation != .stamps) @compileError("only stamp domains can be borrowed");
            return .{ .storage = stamps, .logical_len = stamps.len, .borrowed = true };
        }

        /// An empty set over keys below `key_count`.
        pub fn init(gpa: Allocator, key_count: usize) Allocator.Error!Self {
            var self: Self = .{};
            try self.ensureLen(gpa, key_count);
            return self;
        }

        /// Free the set.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            switch (representation) {
                .stamps => if (!self.borrowed) {
                    if (self.pages.bytes.len != 0) self.pages.unmap() else gpa.free(self.storage);
                },
                .bitset => self.storage.deinit(gpa),
            }
            self.* = .{};
        }

        /// The number of keys the set can hold.
        pub fn len(self: *const Self) usize {
            return switch (representation) {
                .stamps => self.logical_len,
                .bitset => self.storage.bit_length,
            };
        }

        /// Accept keys below `new_len`; the new keys are absent.
        pub fn ensureLen(self: *Self, gpa: Allocator, new_len: usize) Allocator.Error!void {
            switch (representation) {
                .stamps => {
                    if (new_len <= self.logical_len) return;
                    // A mapped set has a fixed domain; frozen graph clones
                    // never grow. Mutable graphs retain amortized capacity.
                    std.debug.assert(self.pages.bytes.len == 0 and !self.borrowed);
                    if (new_len > self.storage.len) {
                        const capacity = @max(new_len, self.storage.len +| self.storage.len / 2 +| 8);
                        self.storage = try gpa.realloc(self.storage, capacity);
                    }
                    clearStamps(self.storage[self.logical_len..new_len]);
                    self.logical_len = new_len;
                },
                .bitset => if (new_len > self.storage.bit_length) {
                    const old_len = self.storage.bit_length;
                    try self.storage.resize(gpa, new_len, false);
                    storage_work.add(.bytes_filled, (new_len - old_len + 7) / 8);
                },
            }
        }

        /// Discard a mutable graph suffix while retaining its stamp capacity.
        pub fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
            std.debug.assert(new_len <= self.len());
            self.clearRange(new_len, self.len());
            if (representation == .stamps) {
                self.logical_len = new_len;
            } else {
                @compileError("bitset domains are retained across resets, not truncated");
            }
        }

        /// Whether `k` is in the set.
        pub inline fn contains(self: *const Self, k: Key) bool {
            const i = keyIndex(Key, k);
            std.debug.assert(i < self.len());
            return switch (representation) {
                .stamps => self.epoch.current != 0 and self.storage[i] == self.epoch.current,
                .bitset => self.storage.isSet(i),
            };
        }

        /// Add `k`.
        pub inline fn insert(self: *Self, k: Key) void {
            if (source == .natural) std.debug.assert(self.epoch.current != 0);
            const i = keyIndex(Key, k);
            std.debug.assert(i < self.len());
            switch (representation) {
                .stamps => self.storage[i] = self.epoch.current,
                .bitset => self.storage.set(i),
            }
        }

        /// Add `k`, returning whether it was already present.
        pub inline fn testAndInsert(self: *Self, k: Key) bool {
            const present = self.contains(k);
            self.insert(k);
            return present;
        }

        /// Remove `k`.
        pub inline fn remove(self: *Self, k: Key) void {
            const i = keyIndex(Key, k);
            std.debug.assert(i < self.len());
            switch (representation) {
                .stamps => self.storage[i] = 0,
                .bitset => self.storage.unset(i),
            }
        }

        /// Empty the set.
        pub fn reset(self: *Self) void {
            if (source != .internal) @compileError("a natural-epoch set is reset with resetTo");
            switch (representation) {
                .stamps => if (self.epoch.advance()) clearStamps(self.storage),
                .bitset => {
                    self.storage.unsetAll();
                    storage_work.add(.bytes_filled, (self.storage.bit_length + 7) / 8);
                },
            }
        }

        /// Empty the set by installing the natural epoch `identifier`.
        pub fn resetTo(self: *Self, identifier: u32) void {
            if (source != .natural) @compileError("an internal-epoch set is reset with reset");
            self.epoch.install(identifier);
        }

        /// Empty a natural-epoch set before its identifiers restart.
        pub fn clear(self: *Self) void {
            switch (representation) {
                .stamps => {
                    clearStamps(self.storage);
                    self.epoch = .{ .current = if (source == .natural) 0 else first_epoch };
                },
                .bitset => self.storage.unsetAll(),
            }
        }

        /// Remove every key in `[start, end)`; for rolling back a suffix.
        pub fn clearRange(self: *Self, start: usize, end: usize) void {
            std.debug.assert(start <= end and end <= self.len());
            switch (representation) {
                .stamps => clearStamps(self.storage[start..end]),
                .bitset => self.storage.setRangeValue(.{ .start = start, .end = end }, false),
            }
        }
    };
}

/// A column of `T` per `Key` that empties in constant time: an entry holds a
/// datum only when it was set in the current epoch. Data of at most four
/// bytes are interleaved with their stamps, so one probe touches one cache
/// line; wider data live in a separate column. Data are never initialized:
/// `set` always writes the stamp and the datum together, and `get` reads the
/// datum only when its stamp matches.
pub fn EpochColumn(comptime Key: type, comptime T: type, comptime source: EpochSource) type {
    const interleaved = @sizeOf(T) <= 4;
    return struct {
        const Self = @This();

        const Entry = struct { stamp: u32, datum: T };

        entries: if (interleaved) []Entry else void = if (interleaved) &.{} else {},
        stamps: if (interleaved) void else []u32 = if (interleaved) {} else &.{},
        data: if (interleaved) void else []T = if (interleaved) {} else &.{},
        epoch: Epoch = .{ .current = if (source == .natural) 0 else first_epoch },

        /// An empty column over keys below `key_count`.
        pub fn init(gpa: Allocator, key_count: usize) Allocator.Error!Self {
            var self: Self = .{};
            errdefer self.deinit(gpa);
            try self.ensureLen(gpa, key_count);
            return self;
        }

        /// Free the column.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            if (interleaved) {
                gpa.free(self.entries);
            } else {
                gpa.free(self.stamps);
                gpa.free(self.data);
            }
            self.* = .{};
        }

        /// The number of keys the column can hold.
        pub fn len(self: *const Self) usize {
            return if (interleaved) self.entries.len else self.stamps.len;
        }

        /// Accept keys below `new_len`; the new keys hold nothing.
        pub fn ensureLen(self: *Self, gpa: Allocator, new_len: usize) Allocator.Error!void {
            if (interleaved) {
                const old_len = self.entries.len;
                if (new_len <= old_len) return;
                self.entries = try gpa.realloc(self.entries, new_len);
                for (self.entries[old_len..]) |*entry| entry.* = .{ .stamp = 0, .datum = undefined };
                storage_work.add(.bytes_filled, (new_len - old_len) * @sizeOf(u32));
            } else {
                if (new_len <= self.stamps.len) return;
                const old_len = self.data.len;
                // A failed stamp allocation can leave spare data capacity.
                if (new_len > old_len) {
                    self.data = try gpa.realloc(self.data, new_len);
                    if (std.debug.runtime_safety) @memset(self.data[old_len..], undefined);
                }
                try growStamps(gpa, &self.stamps, new_len);
            }
        }

        /// The datum at `k` in the current epoch, or null.
        pub inline fn get(self: *const Self, k: Key) ?T {
            const i = keyIndex(Key, k);
            if (interleaved) {
                const entry = &self.entries[i];
                return if (self.epoch.current != 0 and entry.stamp == self.epoch.current) entry.datum else null;
            }
            return if (self.epoch.current != 0 and self.stamps[i] == self.epoch.current) self.data[i] else null;
        }

        /// A pointer to the datum at `k`, which must be set in the current
        /// epoch.
        pub inline fn ptr(self: *Self, k: Key) *T {
            if (source == .natural) std.debug.assert(self.epoch.current != 0);
            const i = keyIndex(Key, k);
            if (interleaved) {
                std.debug.assert(self.entries[i].stamp == self.epoch.current);
                return &self.entries[i].datum;
            }
            std.debug.assert(self.stamps[i] == self.epoch.current);
            return &self.data[i];
        }

        /// Whether `k` holds a datum in the current epoch.
        pub inline fn contains(self: *const Self, k: Key) bool {
            const i = keyIndex(Key, k);
            if (self.epoch.current == 0) return false;
            if (interleaved) return self.entries[i].stamp == self.epoch.current;
            return self.stamps[i] == self.epoch.current;
        }

        /// Store `value` at `k` in the current epoch.
        pub inline fn set(self: *Self, k: Key, value: T) void {
            if (source == .natural) std.debug.assert(self.epoch.current != 0);
            const i = keyIndex(Key, k);
            if (interleaved) {
                self.entries[i] = .{ .stamp = self.epoch.current, .datum = value };
            } else {
                self.stamps[i] = self.epoch.current;
                self.data[i] = value;
            }
        }

        /// Remove the datum at `k`.
        pub inline fn remove(self: *Self, k: Key) void {
            const i = keyIndex(Key, k);
            if (interleaved) self.entries[i].stamp = 0 else self.stamps[i] = 0;
        }

        /// Empty the column.
        pub fn reset(self: *Self) void {
            if (source != .internal) @compileError("a natural-epoch column is reset with resetTo");
            if (self.epoch.advance()) self.clearAllStamps();
        }

        /// Empty the column by installing the natural epoch `identifier`.
        pub fn resetTo(self: *Self, identifier: u32) void {
            if (source != .natural) @compileError("an internal-epoch column is reset with reset");
            self.epoch.install(identifier);
        }

        /// Empty a natural-epoch column before its identifiers restart.
        pub fn clear(self: *Self) void {
            self.clearAllStamps();
            self.epoch = .{ .current = if (source == .natural) 0 else first_epoch };
        }

        /// Remove every datum in `[start, end)`; for rolling back a suffix.
        pub fn clearRange(self: *Self, start: usize, end: usize) void {
            if (interleaved) {
                for (self.entries[start..end]) |*entry| entry.stamp = 0;
                storage_work.add(.bytes_filled, (end - start) * @sizeOf(u32));
            } else {
                clearStamps(self.stamps[start..end]);
            }
        }

        fn clearAllStamps(self: *Self) void {
            if (interleaved) {
                for (self.entries) |*entry| entry.stamp = 0;
                storage_work.add(.bytes_filled, self.entries.len * @sizeOf(u32));
            } else {
                clearStamps(self.stamps);
            }
        }
    };
}

const testing = std.testing;
const TestKey = enum(u32) { _ };

fn testKey(i: u32) TestKey {
    return @fromBackingInt(i);
}

test "a reset empties a stamped set" {
    var set = try EpochSet(TestKey, .stamps, .internal).init(testing.allocator, 8);
    defer set.deinit(testing.allocator);
    set.insert(testKey(3));
    try testing.expect(set.contains(testKey(3)));
    try testing.expect(!set.testAndInsert(testKey(5)));
    try testing.expect(set.testAndInsert(testKey(5)));
    set.reset();
    try testing.expect(!set.contains(testKey(3)));
    try testing.expect(!set.contains(testKey(5)));
}

test "a reset empties a bitset" {
    var set = try EpochSet(u32, .bitset, .internal).init(testing.allocator, 100);
    defer set.deinit(testing.allocator);
    set.insert(99);
    set.reset();
    try testing.expect(!set.contains(99));
    set.insert(1);
    set.remove(1);
    try testing.expect(!set.contains(1));
}

test "the wrap clears every stamp and restarts at one" {
    var set = try EpochSet(TestKey, .stamps, .internal).init(testing.allocator, 4);
    defer set.deinit(testing.allocator);
    set.insert(testKey(0));
    set.epoch.current = std.math.maxInt(u32) - 2;
    set.insert(testKey(1));
    set.reset();
    try testing.expectEqual(std.math.maxInt(u32) - 1, set.epoch.current);
    set.insert(testKey(2));
    set.reset();
    try testing.expectEqual(@as(u32, 1), set.epoch.current);
    // Key 0 was stamped with epoch 1 long ago; the wrap cleared it.
    for (0..4) |i| try testing.expect(!set.contains(testKey(@intCast(i))));
}

test "ensureLen clears only the new suffix" {
    var set = try EpochSet(TestKey, .stamps, .internal).init(testing.allocator, 2);
    defer set.deinit(testing.allocator);
    set.insert(testKey(1));
    const filled_before = storage_work.get(.bytes_filled);
    try set.ensureLen(testing.allocator, 10);
    if (storage_work.enabled) try testing.expectEqual(filled_before + 8 * @sizeOf(u32), storage_work.get(.bytes_filled));
    try testing.expect(set.contains(testKey(1)));
    try testing.expect(!set.contains(testKey(9)));
}

test "natural epochs empty the set without counting" {
    var set = try EpochSet(TestKey, .stamps, .natural).init(testing.allocator, 4);
    defer set.deinit(testing.allocator);
    set.resetTo(0);
    set.insert(testKey(2));
    set.resetTo(1);
    try testing.expect(!set.contains(testKey(2)));
    set.insert(testKey(3));
    set.clear();
    set.resetTo(0);
    try testing.expect(!set.contains(testKey(3)));
}

test "an interleaved column holds data only in its epoch" {
    var column = try EpochColumn(TestKey, u16, .internal).init(testing.allocator, 4);
    defer column.deinit(testing.allocator);
    try testing.expectEqual(@as(?u16, null), column.get(testKey(1)));
    column.set(testKey(1), 42);
    try testing.expectEqual(@as(?u16, 42), column.get(testKey(1)));
    column.ptr(testKey(1)).* += 1;
    try testing.expectEqual(@as(?u16, 43), column.get(testKey(1)));
    column.reset();
    try testing.expectEqual(@as(?u16, null), column.get(testKey(1)));
    try testing.expect(!column.contains(testKey(1)));
}

test "a wide column keeps separate stamps and survives a wrap" {
    var column = try EpochColumn(TestKey, u64, .internal).init(testing.allocator, 3);
    defer column.deinit(testing.allocator);
    column.set(testKey(0), 7);
    column.epoch.current = std.math.maxInt(u32) - 1;
    column.reset();
    try testing.expectEqual(@as(u32, 1), column.epoch.current);
    try testing.expectEqual(@as(?u64, null), column.get(testKey(0)));
    column.set(testKey(2), 9);
    try column.ensureLen(testing.allocator, 6);
    try testing.expectEqual(@as(?u64, 9), column.get(testKey(2)));
    try testing.expectEqual(@as(?u64, null), column.get(testKey(5)));
    column.clearRange(2, 3);
    try testing.expectEqual(@as(?u64, null), column.get(testKey(2)));
}

test "a natural-epoch column empties on each identifier" {
    var column = try EpochColumn(u32, u32, .natural).init(testing.allocator, 2);
    defer column.deinit(testing.allocator);
    column.resetTo(4);
    column.set(0, 1);
    column.resetTo(5);
    try testing.expectEqual(@as(?u32, null), column.get(0));
}

fn exerciseWideColumnAllocationFailures(gpa: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
    var column = try EpochColumn(u32, u64, .internal).init(gpa, 3);
    defer column.deinit(gpa);
    column.set(2, 91);
    try column.ensureLen(gpa, 20);
    try testing.expectEqual(@as(?u64, 91), column.get(2));
    try testing.expectEqual(@as(?u64, null), column.get(19));
}

test "wide column construction and growth release allocations on failure" {
    try testing.checkAllAllocationFailures(testing.allocator, exerciseWideColumnAllocationFailures, .{});
}

test "wide column retries a smaller growth after stamp allocation fails" {
    var failing = testing.FailingAllocator.init(testing.allocator, .{});
    const gpa = failing.allocator();
    var column = try EpochColumn(u32, u64, .internal).init(gpa, 3);
    defer column.deinit(gpa);
    column.set(2, 91);
    // Data growth succeeds, but the stamp allocation fails.
    failing.fail_index = failing.alloc_index + 1;
    failing.resize_fail_index = failing.resize_index;
    try testing.expectError(error.OutOfMemory, column.ensureLen(gpa, 2000));
    failing.fail_index = std.math.maxInt(usize);
    failing.resize_fail_index = std.math.maxInt(usize);
    try column.ensureLen(gpa, 10);
    try testing.expectEqual(@as(?u64, 91), column.get(2));
    try testing.expectEqual(@as(?u64, null), column.get(9));
}

test "natural epochs accept the largest real u32 identifier" {
    var set = try EpochSet(u32, .stamps, .natural).init(testing.allocator, 2);
    defer set.deinit(testing.allocator);
    try testing.expect(!set.contains(0));
    set.resetTo(std.math.maxInt(u32) - 2);
    set.insert(0);
    set.resetTo(std.math.maxInt(u32) - 1);
    try testing.expect(!set.contains(0));
    set.insert(1);
    try testing.expect(set.contains(1));
    set.clear();
    try testing.expect(!set.contains(1));
    set.resetTo(0);
    try testing.expect(!set.contains(1));
}

test "epoch suffix rollback clears reused indices without reallocating" {
    var set = try EpochSet(u32, .stamps, .internal).init(testing.allocator, 3);
    defer set.deinit(testing.allocator);
    set.insert(2);
    const capacity = set.storage.len;
    set.shrinkRetainingCapacity(1);
    try set.ensureLen(testing.allocator, 3);
    try testing.expect(!set.contains(2));
    try testing.expectEqual(capacity, set.storage.len);
}

test "sparse epoch domains begin absent and reset without filling" {
    var set = try EpochSet(u32, .stamps, .internal).initSparse(100000);
    defer set.deinit(testing.allocator);
    try testing.expect(!set.contains(99999));
    set.insert(99999);
    set.reset();
    try testing.expect(!set.contains(99999));
}

test "borrowed epoch cursors never free their owner's stamps" {
    var stamps: [4]u32 = @splat(0);
    var set = EpochSet(u32, .stamps, .internal).borrow(&stamps);
    defer set.deinit(testing.allocator);
    set.insert(2);
    try testing.expect(set.contains(2));
    set.epoch.current = std.math.maxInt(u32) - 1;
    set.reset();
    try testing.expect(!set.contains(2));
    try testing.expectEqualSlices(u32, &.{ 0, 0, 0, 0 }, &stamps);
}
