/// Utilities for managing collections.
pub const utils = @import("collections/utils.zig");

const std = @import("std");

/// The highest alignment any Roc type can have.
/// This is used as the base alignment for the allocation used
/// in the interpreter for stack allocations.
pub const max_roc_alignment: std.mem.Alignment = .@"16";

pub const SafeList = @import("collections/safe_list.zig").SafeList;

pub const SafeMultiList = @import("collections/safe_list.zig").SafeMultiList;

pub const NonEmptyRange = @import("collections/safe_list.zig").NonEmptyRange;

pub const SmallStringInterner = @import("collections/SmallStringInterner.zig");

/// A key-value map that uses direct array indexing instead of hashing.
/// Keys must be enums that are convertible to indices, and key value 0 is reserved
/// as a sentinel value which indicates an empty slot.
pub fn ArrayListMap(comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();

        // We store V directly, using 0 as a sentinel value meaning "empty"
        entries: []V,

        pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
            // Allocate zeroed memory
            const entries = try allocator.alloc(V, capacity);
            @memset(entries, std.mem.zeroes(V));

            return .{
                .entries = entries,
            };
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            allocator.free(self.entries);
        }

        pub fn put(self: *Self, allocator: std.mem.Allocator, key: K, value: V) !void {
            const index = @as(usize, @intFromEnum(key));
            std.debug.assert(index != 0); // 0 is reserved as sentinel

            // Ensure we have capacity for this index
            if (index >= self.entries.len) {
                const new_capacity = index + 1;
                const new_entries = try allocator.alloc(V, new_capacity);

                // Copy old entries
                @memcpy(new_entries[0..self.entries.len], self.entries);

                // Zero out the new bytes after the copied-over entries
                @memset(new_entries[self.entries.len..], std.mem.zeroes(V));

                // Free old allocation and update backing memory
                allocator.free(self.entries);
                self.entries = new_entries;
            }

            self.entries[index] = value;
        }

        pub fn get(self: Self, key: K) ?V {
            const index = @as(usize, @intFromEnum(key));
            std.debug.assert(index != 0); // 0 is reserved as sentinel

            if (index >= self.entries.len) {
                return null;
            }

            const value = self.entries[index];

            if (isZero(V, value)) {
                return null;
            } else {
                return value;
            }
        }

        pub fn contains(self: Self, key: K) bool {
            const index = @as(usize, @intFromEnum(key));

            if (index >= self.entries.len) {
                return false;
            }

            return isZero(V, self.entries[index]);
        }

        pub fn remove(self: *Self, key: K) bool {
            const index = @as(usize, @intFromEnum(key));

            if (index >= self.entries.len) {
                return false;
            }

            const was_present = !isZero(self[index]);

            self.entries[index] = std.mem.zeroes(V);

            return was_present;
        }

        /// How many slots are filled in the map
        pub fn count(self: Self) usize {
            var total: usize = 0;
            for (self.entries) |value| {
                if (!isZero(value)) total += 1;
            }
            return total;
        }

        pub fn iterator(self: *const Self) Iterator {
            return .{
                .map = self,
                .index = 0,
            };
        }

        pub const Iterator = struct {
            map: *const Self,
            index: usize,

            pub fn next(it: *Iterator) ?Entry {
                while (it.index < it.map.entries.len) {
                    const current_index = it.index;
                    it.index += 1;

                    const value = it.map.entries[current_index];
                    if (!isZero(value)) {
                        return .{
                            .key = @as(K, @enumFromInt(current_index)),
                            .value = value,
                        };
                    }
                }
                return null;
            }

            pub const Entry = struct {
                key: K,
                value: V,
            };
        };
    };
}

fn isZero(comptime T: type, value: T) bool {
    return std.meta.eql(value, std.mem.zeroes(T));
}
