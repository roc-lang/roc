//! Direct-indexed maps for dense compiler identifiers.
//!
//! Compiler IDs name rows in owning stores and are therefore array indices, not
//! hash keys. `DenseMap` keeps a paged optional-value column indexed by that ID.
//! Paging avoids materializing the untouched prefix of a store-global ID domain
//! for a short-lived local scope. A compact occupied-index column makes clearing
//! and iteration proportional to live entries instead of the largest ID seen.
//! The type intentionally mirrors the small managed `AutoHashMap` surface used by
//! compiler passes so callers do not need to trade direct indexing for convenience.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// A managed direct-indexed map whose keys are integer or enum IDs.
pub fn DenseMap(comptime K: type, comptime V: type) type {
    assertDenseKey(K);

    return struct {
        const Self = @This();

        const chunk_shift = 8;
        const chunk_len = 1 << chunk_shift;
        const chunk_mask = chunk_len - 1;

        const Occupied = struct {
            value: V,
            active_position: u32,
        };

        const Slot = union(enum) {
            empty,
            value: Occupied,
        };

        const Chunk = [chunk_len]Slot;

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
        chunks: std.ArrayList(?*Chunk) = .empty,
        active_indices: std.ArrayList(usize) = .empty,

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            self.freeChunks();
            self.chunks.deinit(self.allocator);
            self.active_indices.deinit(self.allocator);
            self.* = undefined;
        }

        pub fn count(self: *const Self) usize {
            return self.active_indices.items.len;
        }

        pub fn capacity(self: *const Self) usize {
            return self.active_indices.capacity;
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
            const slot = self.slotPtr(index) orelse return null;
            return switch (slot.*) {
                .empty => null,
                .value => |*occupied| &occupied.value,
            };
        }

        pub fn getPtrConst(self: *const Self, key: K) ?*const V {
            const index = keyIndex(key);
            const slot = self.slotPtrConst(index) orelse return null;
            return switch (slot.*) {
                .empty => null,
                .value => |*occupied| &occupied.value,
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

        pub fn putAssumeCapacity(self: *Self, key: K, value: V) void {
            self.put(key, value) catch @panic("DenseMap.putAssumeCapacity exceeded reserved storage");
        }

        pub fn getOrPut(self: *Self, key: K) Allocator.Error!GetOrPutResult {
            const index = keyIndex(key);
            const slot = try self.ensureSlot(index);

            return switch (slot.*) {
                .empty => blk: {
                    const active_position: u32 = @intCast(self.active_indices.items.len);
                    try self.active_indices.append(self.allocator, index);
                    slot.* = .{ .value = .{
                        .value = undefined,
                        .active_position = active_position,
                    } };
                    break :blk .{
                        .value_ptr = &slot.value.value,
                        .found_existing = false,
                    };
                },
                .value => |*occupied| .{
                    .value_ptr = &occupied.value,
                    .found_existing = true,
                },
            };
        }

        pub fn getOrPutAssumeCapacity(self: *Self, key: K) GetOrPutResult {
            return self.getOrPut(key) catch @panic("DenseMap.getOrPutAssumeCapacity exceeded reserved storage");
        }

        pub fn getOrPutValue(self: *Self, key: K, value: V) Allocator.Error!GetOrPutResult {
            const result = try self.getOrPut(key);
            if (!result.found_existing) result.value_ptr.* = value;
            return result;
        }

        pub fn remove(self: *Self, key: K) bool {
            const index = keyIndex(key);
            const slot = self.slotPtr(index) orelse return false;

            return switch (slot.*) {
                .empty => false,
                .value => |occupied| blk: {
                    self.removeActive(slot, occupied.active_position);
                    break :blk true;
                },
            };
        }

        pub fn fetchRemove(self: *Self, key: K) ?KV {
            const index = keyIndex(key);
            const slot = self.slotPtr(index) orelse return null;

            return switch (slot.*) {
                .empty => null,
                .value => |occupied| blk: {
                    const value = occupied.value;
                    self.removeActive(slot, occupied.active_position);
                    break :blk .{ .key = key, .value = value };
                },
            };
        }

        pub fn ensureTotalCapacity(self: *Self, expected_count: usize) Allocator.Error!void {
            try self.active_indices.ensureTotalCapacity(self.allocator, expected_count);
        }

        pub fn ensureUnusedCapacity(self: *Self, additional_count: usize) Allocator.Error!void {
            try self.active_indices.ensureUnusedCapacity(self.allocator, additional_count);
        }

        pub fn clearRetainingCapacity(self: *Self) void {
            for (self.active_indices.items) |index| self.slotPtr(index).?.* = .empty;
            self.active_indices.clearRetainingCapacity();
        }

        pub fn clearAndFree(self: *Self) void {
            self.freeChunks();
            self.chunks.clearAndFree(self.allocator);
            self.active_indices.clearAndFree(self.allocator);
        }

        pub fn iterator(self: *Self) Iterator {
            return .{ .map = self };
        }

        pub fn keyIterator(self: *Self) KeyIterator {
            return .{ .inner = .{ .map = self } };
        }

        pub fn valueIterator(self: *Self) ValueIterator {
            return .{ .map = self };
        }

        pub const Iterator = struct {
            map: *Self,
            position: usize = 0,
            key: K = undefined,

            pub fn next(self: *Iterator) ?Entry {
                if (self.position >= self.map.active_indices.items.len) return null;
                const index = self.map.active_indices.items[self.position];
                self.position += 1;
                const occupied = &self.map.slotPtr(index).?.value;
                self.key = keyFromIndex(K, index);
                return .{ .key_ptr = &self.key, .value_ptr = &occupied.value };
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
            map: *Self,
            position: usize = 0,

            pub fn next(self: *ValueIterator) ?*V {
                if (self.position >= self.map.active_indices.items.len) return null;
                const index = self.map.active_indices.items[self.position];
                self.position += 1;
                return &self.map.slotPtr(index).?.value.value;
            }
        };

        fn ensureSlot(self: *Self, index: usize) Allocator.Error!*Slot {
            const chunk_index = index >> chunk_shift;
            if (chunk_index >= self.chunks.items.len) {
                const old_len = self.chunks.items.len;
                try self.chunks.resize(self.allocator, chunk_index + 1);
                @memset(self.chunks.items[old_len..], null);
            }

            if (self.chunks.items[chunk_index] == null) {
                const chunk = try self.allocator.create(Chunk);
                @memset(chunk, .empty);
                self.chunks.items[chunk_index] = chunk;
            }

            return &self.chunks.items[chunk_index].?[index & chunk_mask];
        }

        fn slotPtr(self: *Self, index: usize) ?*Slot {
            const chunk_index = index >> chunk_shift;
            if (chunk_index >= self.chunks.items.len) return null;
            const chunk = self.chunks.items[chunk_index] orelse return null;
            return &chunk[index & chunk_mask];
        }

        fn slotPtrConst(self: *const Self, index: usize) ?*const Slot {
            const chunk_index = index >> chunk_shift;
            if (chunk_index >= self.chunks.items.len) return null;
            const chunk = self.chunks.items[chunk_index] orelse return null;
            return &chunk[index & chunk_mask];
        }

        fn removeActive(self: *Self, slot: *Slot, active_position: u32) void {
            const position: usize = active_position;
            const last_index = self.active_indices.pop().?;
            if (position < self.active_indices.items.len) {
                self.active_indices.items[position] = last_index;
                self.slotPtr(last_index).?.value.active_position = @intCast(position);
            }
            slot.* = .empty;
        }

        fn freeChunks(self: *Self) void {
            for (self.chunks.items) |maybe_chunk| {
                if (maybe_chunk) |chunk| self.allocator.destroy(chunk);
            }
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
    const K = @TypeOf(key);
    if (comptime @typeInfo(K) == .@"enum" and @hasDecl(K, "denseIndex")) return key.denseIndex();
    return switch (@typeInfo(K)) {
        .int => @intCast(key),
        .@"enum" => @intCast(@intFromEnum(key)),
        else => unreachable,
    };
}

fn keyFromIndex(comptime K: type, index: usize) K {
    if (comptime @typeInfo(K) == .@"enum" and @hasDecl(K, "fromDenseIndex")) return K.fromDenseIndex(index);
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
    try std.testing.expectEqual(@as(u32, 43), first.value_ptr.*);
    const second = iterator.next().?;
    try std.testing.expectEqual(@as(u32, 11), second.value_ptr.*);
    try std.testing.expect(iterator.next() == null);

    try std.testing.expectEqual(@as(u32, 43), map.fetchRemove(@enumFromInt(7)).?.value);
    try std.testing.expect(!map.contains(@enumFromInt(7)));

    var integer_map = DenseMap(u32, bool).init(std.testing.allocator);
    defer integer_map.deinit();
    try integer_map.put(3, true);
    try std.testing.expectEqual(true, integer_map.get(3));

    var sparse_map = DenseMap(u32, u8).init(std.testing.allocator);
    defer sparse_map.deinit();
    try sparse_map.put(1_000_000, 9);
    try std.testing.expectEqual(@as(?u8, 9), sparse_map.get(1_000_000));
    try std.testing.expect(sparse_map.chunks.items.len < 4_000);
}
