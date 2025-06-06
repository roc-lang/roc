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
/// Keys must enums that are convertible to indices, and key value 0 is reserved
/// as a sentinel value which indicates an empty slot.
pub fn ArrayListMap(comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        // We store V directly, using 0 as a sentinel value meaning "empty"
        entries: []V,

        pub fn init(allocator: std.mem.Allocator, capacity: usize) !Self {
            // Allocate zeroed memory
            const entries = try allocator.alloc(V, capacity);
            @memset(entries, std.mem.zeroes(V));

            return .{
                .allocator = allocator,
                .entries = entries,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.entries);
        }

        pub fn put(self: *Self, key: K, value: V) !void {
            const index = @as(usize, @intFromEnum(key));
            std.debug.assert(index != 0); // 0 is reserved as sentinel

            // Ensure we have capacity for this index
            if (index >= self.entries.len) {
                const new_capacity = index + 1;
                const new_entries = try self.allocator.alloc(V, new_capacity);

                // Copy old entries
                @memcpy(new_entries[0..self.entries.len], self.entries);

                // Zero out the new portion
                @memset(new_entries[self.entries.len..], std.mem.zeroes(V));

                // Free old allocation and update
                self.allocator.free(self.entries);
                self.entries = new_entries;
            }

            self.entries[index] = value;
        }

        pub fn get(self: Self, key: K) ?V {
            const index = @as(usize, @intFromEnum(key));

            if (index >= self.entries.len) {
                return null;
            }

            const value = self.entries[index];
            // Check if value is the zero/sentinel value
            // For packed structs, we check if all bytes are zero
            const zero_value = std.mem.zeroes(V);
            if (std.meta.eql(value, zero_value)) {
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

            const zero_value = std.mem.zeroes(V);
            return !std.meta.eql(self.entries[index], zero_value);
        }

        pub fn remove(self: *Self, key: K) bool {
            const index = @as(usize, @intFromEnum(key));

            if (index >= self.entries.len) {
                return false;
            }

            const zero_value = std.mem.zeroes(V);
            if (!std.meta.eql(self.entries[index], zero_value)) {
                self.entries[index] = zero_value;
                return true;
            } else {
                return false;
            }
        }

        pub fn count(self: Self) usize {
            var total: usize = 0;
            const zero_value = std.mem.zeroes(V);
            for (self.entries) |value| {
                if (!std.meta.eql(value, zero_value)) total += 1;
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
                    const zero_value = std.mem.zeroes(V);
                    if (!std.meta.eql(value, zero_value)) {
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
