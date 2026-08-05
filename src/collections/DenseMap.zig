//! Direct-indexed maps for dense compiler identifiers.
//!
//! Compiler IDs name rows in owning stores and are therefore array indices, not
//! hash keys. `DenseMap` keeps one optional value column indexed by that ID. It
//! intentionally mirrors the small managed `AutoHashMap` surface used by compiler
//! passes so callers do not need to trade constant-time indexing for convenience.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// A managed direct-indexed map whose keys are integer or enum IDs.
pub fn DenseMap(comptime K: type, comptime V: type) type {
    assertDenseKey(K);

    return struct {
        const Self = @This();

        const Slot = union(enum) {
            empty,
            value: V,
        };

        /// Entry returned by map iterators.
        pub const Entry = struct {
            key_ptr: *const K,
            value_ptr: *V,
        };

        /// Removed key-value pair returned by `fetchRemove`.
        pub const KV = struct {
            key: K,
            value: V,
        };

        /// Result returned by `getOrPut`.
        pub const GetOrPutResult = struct {
            value_ptr: *V,
            found_existing: bool,
        };

        allocator: Allocator,
        slots: std.ArrayList(Slot) = .empty,
        occupied_count: usize = 0,

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            self.slots.deinit(self.allocator);
            self.* = undefined;
        }

        pub fn count(self: *const Self) usize {
            return self.occupied_count;
        }

        pub fn capacity(self: *const Self) usize {
            return self.slots.capacity;
        }

        pub fn contains(self: *const Self, key: K) bool {
            return self.getPtrConst(key) != null;
        }

        pub fn get(self: *const Self, key: K) ?V {
            const value = self.getPtrConst(key) orelse return null;
            return value.*;
        }

        pub fn getPtr(self: *Self, key: K) ?*V {
            const index = keyIndex(key);
            if (index >= self.slots.items.len) return null;
            return switch (self.slots.items[index]) {
                .empty => null,
                .value => |*value| value,
            };
        }

        pub fn getPtrConst(self: *const Self, key: K) ?*const V {
            const index = keyIndex(key);
            if (index >= self.slots.items.len) return null;
            return switch (self.slots.items[index]) {
                .empty => null,
                .value => |*value| value,
            };
        }

        pub fn put(self: *Self, key: K, value: V) Allocator.Error!void {
            const result = try self.getOrPut(key);
            result.value_ptr.* = value;
        }

        pub fn putNoClobber(self: *Self, key: K, value: V) Allocator.Error!void {
            const result = try self.getOrPut(key);
            std.debug.assert(!result.found_existing);
            result.value_ptr.* = value;
        }

        pub fn getOrPut(self: *Self, key: K) Allocator.Error!GetOrPutResult {
            const index = keyIndex(key);
            try self.ensureIndex(index);

            return switch (self.slots.items[index]) {
                .empty => blk: {
                    self.slots.items[index] = .{ .value = undefined };
                    self.occupied_count += 1;
                    break :blk .{
                        .value_ptr = &self.slots.items[index].value,
                        .found_existing = false,
                    };
                },
                .value => |*value| .{
                    .value_ptr = value,
                    .found_existing = true,
                },
            };
        }

        pub fn getOrPutValue(self: *Self, key: K, value: V) Allocator.Error!GetOrPutResult {
            const result = try self.getOrPut(key);
            if (!result.found_existing) result.value_ptr.* = value;
            return result;
        }

        pub fn remove(self: *Self, key: K) bool {
            const index = keyIndex(key);
            if (index >= self.slots.items.len) return false;

            return switch (self.slots.items[index]) {
                .empty => false,
                .value => blk: {
                    self.slots.items[index] = .empty;
                    self.occupied_count -= 1;
                    break :blk true;
                },
            };
        }

        pub fn fetchRemove(self: *Self, key: K) ?KV {
            const index = keyIndex(key);
            if (index >= self.slots.items.len) return null;

            return switch (self.slots.items[index]) {
                .empty => null,
                .value => |value| blk: {
                    self.slots.items[index] = .empty;
                    self.occupied_count -= 1;
                    break :blk .{ .key = key, .value = value };
                },
            };
        }

        pub fn ensureTotalCapacity(self: *Self, expected_count: usize) Allocator.Error!void {
            try self.slots.ensureTotalCapacity(self.allocator, expected_count);
        }

        pub fn ensureUnusedCapacity(self: *Self, additional_count: usize) Allocator.Error!void {
            try self.slots.ensureUnusedCapacity(self.allocator, additional_count);
        }

        pub fn clearRetainingCapacity(self: *Self) void {
            for (self.slots.items) |*slot| slot.* = .empty;
            self.occupied_count = 0;
        }

        pub fn clearAndFree(self: *Self) void {
            self.slots.clearAndFree(self.allocator);
            self.occupied_count = 0;
        }

        pub fn iterator(self: *Self) Iterator {
            return .{ .slots = self.slots.items };
        }

        pub fn keyIterator(self: *Self) KeyIterator {
            return .{ .slots = self.slots.items };
        }

        pub fn valueIterator(self: *Self) ValueIterator {
            return .{ .slots = self.slots.items };
        }

        pub const Iterator = struct {
            slots: []Slot,
            index: usize = 0,
            key: K = undefined,

            pub fn next(self: *Iterator) ?Entry {
                while (self.index < self.slots.len) {
                    const index = self.index;
                    self.index += 1;
                    switch (self.slots[index]) {
                        .empty => {},
                        .value => |*value| {
                            self.key = keyFromIndex(K, index);
                            return .{ .key_ptr = &self.key, .value_ptr = value };
                        },
                    }
                }
                return null;
            }
        };

        pub const KeyIterator = struct {
            inner: Iterator,

            pub fn next(self: *KeyIterator) ?*const K {
                const entry = self.inner.next() orelse return null;
                return entry.key_ptr;
            }
        };

        pub const ValueIterator = struct {
            slots: []Slot,
            index: usize = 0,

            pub fn next(self: *ValueIterator) ?*V {
                while (self.index < self.slots.len) {
                    const index = self.index;
                    self.index += 1;
                    switch (self.slots[index]) {
                        .empty => {},
                        .value => |*value| return value,
                    }
                }
                return null;
            }
        };

        fn ensureIndex(self: *Self, index: usize) Allocator.Error!void {
            if (index < self.slots.items.len) return;

            const old_len = self.slots.items.len;
            try self.slots.resize(self.allocator, index + 1);
            for (self.slots.items[old_len..]) |*slot| slot.* = .empty;
        }
    };
}

fn assertDenseKey(comptime K: type) void {
    switch (@typeInfo(K)) {
        .int, .@"enum" => {},
        else => @compileError("DenseMap keys must be integer or enum IDs, not " ++ @typeName(K)),
    }
}

fn keyIndex(key: anytype) usize {
    return switch (@typeInfo(@TypeOf(key))) {
        .int => @intCast(key),
        .@"enum" => @intCast(@intFromEnum(key)),
        else => unreachable,
    };
}

fn keyFromIndex(comptime K: type, index: usize) K {
    return switch (@typeInfo(K)) {
        .int => @intCast(index),
        .@"enum" => @enumFromInt(index),
        else => unreachable,
    };
}

test "DenseMap directly indexes integer and enum IDs" {
    const TestId = enum(u32) { _ };
    var map = DenseMap(TestId, u32).init(std.testing.allocator);
    defer map.deinit();

    try map.put(@enumFromInt(7), 42);
    try std.testing.expectEqual(@as(?u32, 42), map.get(@enumFromInt(7)));
    try std.testing.expectEqual(@as(?u32, null), map.get(@enumFromInt(6)));
    try std.testing.expectEqual(@as(usize, 1), map.count());

    const existing = try map.getOrPut(@enumFromInt(7));
    try std.testing.expect(existing.found_existing);
    existing.value_ptr.* = 43;

    const inserted = try map.getOrPut(@enumFromInt(2));
    try std.testing.expect(!inserted.found_existing);
    inserted.value_ptr.* = 11;

    var iterator = map.iterator();
    const first = iterator.next().?;
    try std.testing.expectEqual(TestId, @TypeOf(first.key_ptr.*));
    try std.testing.expectEqual(@as(u32, 11), first.value_ptr.*);
    const second = iterator.next().?;
    try std.testing.expectEqual(@as(u32, 43), second.value_ptr.*);
    try std.testing.expect(iterator.next() == null);

    try std.testing.expectEqual(@as(u32, 43), map.fetchRemove(@enumFromInt(7)).?.value);
    try std.testing.expect(!map.contains(@enumFromInt(7)));
}
