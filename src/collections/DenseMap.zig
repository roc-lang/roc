//! Direct-indexed maps for dense compiler identifiers.
//!
//! Compiler IDs name rows in owning stores and are therefore array indices, not
//! hash keys. `DenseMap` keeps paged ID-to-position columns and stores only live
//! values in a compact dense column. Paging avoids materializing the untouched
//! prefix of a store-global ID domain for a short-lived local scope. Compact key
//! and value columns make clearing and iteration proportional to live entries
//! instead of the largest ID seen, without initializing value-sized sparse pages.
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

        const empty_position = std.math.maxInt(u32);
        const SparseChunk = [chunk_len]u32;

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
        sparse_chunks: std.ArrayList(?*SparseChunk) = .empty,
        active_indices: std.ArrayList(usize) = .empty,
        values: std.ArrayList(V) = .empty,

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            self.freeChunks();
            self.sparse_chunks.deinit(self.allocator);
            self.active_indices.deinit(self.allocator);
            self.values.deinit(self.allocator);
            self.* = undefined;
        }

        pub fn count(self: *const Self) usize {
            return self.active_indices.items.len;
        }

        pub fn capacity(self: *const Self) usize {
            return @min(self.active_indices.capacity, self.values.capacity);
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
            const position = self.densePosition(index) orelse return null;
            return &self.values.items[position];
        }

        pub fn getPtrConst(self: *const Self, key: K) ?*const V {
            const index = keyIndex(key);
            const position = self.densePosition(index) orelse return null;
            return &self.values.items[position];
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
            if (self.densePosition(index)) |existing| return .{
                .value_ptr = &self.values.items[existing],
                .found_existing = true,
            };

            const slot = try self.ensureSparseSlot(index);
            try self.active_indices.ensureUnusedCapacity(self.allocator, 1);
            try self.values.ensureUnusedCapacity(self.allocator, 1);
            const position: u32 = @intCast(self.active_indices.items.len);
            self.active_indices.appendAssumeCapacity(index);
            self.values.appendAssumeCapacity(undefined);
            slot.* = position;
            return .{
                .value_ptr = &self.values.items[position],
                .found_existing = false,
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
            const position = self.densePosition(index) orelse return false;
            self.removeActive(index, position);
            return true;
        }

        pub fn fetchRemove(self: *Self, key: K) ?KV {
            const index = keyIndex(key);
            const position = self.densePosition(index) orelse return null;
            const value = self.values.items[position];
            self.removeActive(index, position);
            return .{ .key = key, .value = value };
        }

        pub fn ensureTotalCapacity(self: *Self, expected_count: usize) Allocator.Error!void {
            try self.active_indices.ensureTotalCapacity(self.allocator, expected_count);
            try self.values.ensureTotalCapacity(self.allocator, expected_count);
        }

        pub fn ensureUnusedCapacity(self: *Self, additional_count: usize) Allocator.Error!void {
            try self.active_indices.ensureUnusedCapacity(self.allocator, additional_count);
            try self.values.ensureUnusedCapacity(self.allocator, additional_count);
        }

        pub fn clearRetainingCapacity(self: *Self) void {
            for (self.active_indices.items) |index| self.sparseSlotPtr(index).?.* = empty_position;
            self.active_indices.clearRetainingCapacity();
            self.values.clearRetainingCapacity();
        }

        pub fn clearAndFree(self: *Self) void {
            self.freeChunks();
            self.sparse_chunks.clearAndFree(self.allocator);
            self.active_indices.clearAndFree(self.allocator);
            self.values.clearAndFree(self.allocator);
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
                const position = self.position;
                self.position += 1;
                const index = self.map.active_indices.items[position];
                self.key = keyFromIndex(K, index);
                return .{ .key_ptr = &self.key, .value_ptr = &self.map.values.items[position] };
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
                if (self.position >= self.map.values.items.len) return null;
                const position = self.position;
                self.position += 1;
                return &self.map.values.items[position];
            }
        };

        fn ensureSparseSlot(self: *Self, index: usize) Allocator.Error!*u32 {
            const chunk_index = index >> chunk_shift;
            if (chunk_index >= self.sparse_chunks.items.len) {
                const old_len = self.sparse_chunks.items.len;
                try self.sparse_chunks.resize(self.allocator, chunk_index + 1);
                @memset(self.sparse_chunks.items[old_len..], null);
            }

            if (self.sparse_chunks.items[chunk_index] == null) {
                const chunk = try self.allocator.create(SparseChunk);
                @memset(chunk, empty_position);
                self.sparse_chunks.items[chunk_index] = chunk;
            }

            return &self.sparse_chunks.items[chunk_index].?[index & chunk_mask];
        }

        fn sparseSlotPtr(self: *Self, index: usize) ?*u32 {
            const chunk_index = index >> chunk_shift;
            if (chunk_index >= self.sparse_chunks.items.len) return null;
            const chunk = self.sparse_chunks.items[chunk_index] orelse return null;
            return &chunk[index & chunk_mask];
        }

        fn sparseSlotPtrConst(self: *const Self, index: usize) ?*const u32 {
            const chunk_index = index >> chunk_shift;
            if (chunk_index >= self.sparse_chunks.items.len) return null;
            const chunk = self.sparse_chunks.items[chunk_index] orelse return null;
            return &chunk[index & chunk_mask];
        }

        fn densePosition(self: *const Self, index: usize) ?usize {
            const raw_position = (self.sparseSlotPtrConst(index) orelse return null).*;
            if (raw_position == empty_position) return null;
            const dense_position: usize = raw_position;
            std.debug.assert(dense_position < self.active_indices.items.len);
            std.debug.assert(self.active_indices.items[dense_position] == index);
            return dense_position;
        }

        fn removeActive(self: *Self, index: usize, position: usize) void {
            const last_position = self.active_indices.items.len - 1;
            if (position < last_position) {
                const last_index = self.active_indices.items[last_position];
                self.active_indices.items[position] = last_index;
                self.values.items[position] = self.values.items[last_position];
                self.sparseSlotPtr(last_index).?.* = @intCast(position);
            }
            _ = self.active_indices.pop().?;
            _ = self.values.pop().?;
            self.sparseSlotPtr(index).?.* = empty_position;
        }

        fn freeChunks(self: *Self) void {
            for (self.sparse_chunks.items) |maybe_chunk| {
                if (maybe_chunk) |chunk| self.allocator.destroy(chunk);
            }
        }
    };
}

fn assertDenseKey(comptime K: type) void {
    const type_info = @typeInfo(K);
    if (type_info != .int and type_info != .@"enum") {
        @compileError("DenseMap keys must be integer or enum IDs, not " ++ @typeName(K));
    }
}

fn keyIndex(key: anytype) usize {
    const K = @TypeOf(key);
    if (comptime @typeInfo(K) == .@"enum" and @hasDecl(K, "denseIndex")) return key.denseIndex();
    if (comptime @typeInfo(K) == .int) return @intCast(key);
    if (comptime @typeInfo(K) == .@"enum") return @intCast(@intFromEnum(key));
    unreachable;
}

fn keyFromIndex(comptime K: type, index: usize) K {
    if (comptime @typeInfo(K) == .@"enum" and @hasDecl(K, "fromDenseIndex")) return K.fromDenseIndex(index);
    if (comptime @typeInfo(K) == .int) return @intCast(index);
    if (comptime @typeInfo(K) == .@"enum") return @enumFromInt(index);
    unreachable;
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
    try std.testing.expect(sparse_map.sparse_chunks.items.len < 4_000);
    try std.testing.expectEqual(@as(usize, 1), sparse_map.values.items.len);
}

test "DenseMap swap-removes and clears compact values" {
    const TestId = enum(u32) { _ };
    const LargeValue = [1024]u8;
    var map = DenseMap(TestId, LargeValue).init(std.testing.allocator);
    defer map.deinit();

    try map.put(@enumFromInt(300), [_]u8{1} ** 1024);
    try map.put(@enumFromInt(700), [_]u8{2} ** 1024);
    try std.testing.expectEqual(@as(usize, 2), map.values.items.len);
    try std.testing.expect(map.remove(@enumFromInt(300)));
    try std.testing.expectEqual(@as(usize, 1), map.values.items.len);
    try std.testing.expectEqual(@as(u8, 2), map.get(@enumFromInt(700)).?[0]);

    map.clearRetainingCapacity();
    try std.testing.expectEqual(@as(usize, 0), map.count());
    try std.testing.expectEqual(@as(?LargeValue, null), map.get(@enumFromInt(700)));

    try map.put(@enumFromInt(300), [_]u8{3} ** 1024);
    try std.testing.expectEqual(@as(u8, 3), map.get(@enumFromInt(300)).?[0]);
    try std.testing.expectEqual(@as(usize, 1), map.values.items.len);

    var set = DenseMap(TestId, void).init(std.testing.allocator);
    defer set.deinit();
    try set.put(@enumFromInt(900), {});
    try std.testing.expect(set.contains(@enumFromInt(900)));
    try std.testing.expect(set.remove(@enumFromInt(900)));
}
