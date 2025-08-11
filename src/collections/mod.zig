//! Collection utilities and memory alignment constants for the Roc compiler.
//!
//! This module provides utilities for managing collections and defines
//! memory alignment constants used throughout the compiler, particularly
//! for stack allocations in the interpreter.

const std = @import("std");

/// The highest alignment any Roc type can have.
/// This is used as the base alignment for the allocation used
/// in the interpreter for stack allocations.
pub const max_roc_alignment: std.mem.Alignment = .@"16";

pub const SafeList = @import("safe_list.zig").SafeList;
pub const SafeRange = @import("safe_list.zig").SafeRange;
pub const SafeMultiList = @import("safe_list.zig").SafeMultiList;

pub const SafeStringHashMap = @import("safe_hash_map.zig").SafeStringHashMap;

pub const SortedArrayBuilder = @import("SortedArrayBuilder.zig").SortedArrayBuilder;
pub const ExposedItems = @import("ExposedItems.zig").ExposedItems;
pub const CompactWriter = @import("CompactWriter.zig");

pub const SERIALIZATION_ALIGNMENT = CompactWriter.SERIALIZATION_ALIGNMENT;

/// A range that must have at least one element
pub const NonEmptyRange = struct {
    /// Starting index (inclusive)
    start: u32,
    /// Number of elements (must be > 0)
    count: u32,

    /// Convert to a SafeMultiList range
    pub fn toRange(self: NonEmptyRange, comptime Idx: type) SafeRange(Idx) {
        std.debug.assert(self.count > 0);
        return .{
            .start = @enumFromInt(self.start),
            .count = self.count,
        };
    }
};

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

            return .{ .entries = entries };
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            allocator.free(self.entries);
        }

        pub fn get(self: Self, key: K) ?V {
            const idx = @intFromEnum(key);
            if (idx >= self.entries.len) return null;

            const value = self.entries[idx];
            // Check if this is an empty slot (zero value)
            if (std.meta.eql(value, std.mem.zeroes(V))) {
                return null;
            }
            return value;
        }

        pub fn put(self: *Self, allocator: std.mem.Allocator, key: K, value: V) !void {
            const idx = @intFromEnum(key);

            // Grow if necessary
            if (idx >= self.entries.len) {
                const new_size = idx + 1;
                const new_entries = try allocator.realloc(self.entries, new_size);
                // Zero out the new memory
                @memset(new_entries[self.entries.len..], std.mem.zeroes(V));
                self.entries = new_entries;
            }

            self.entries[idx] = value;
        }

        pub fn contains(self: Self, key: K) bool {
            return self.get(key) != null;
        }
    };
}

test "collections tests" {
    std.testing.refAllDecls(@import("ExposedItems.zig"));
    std.testing.refAllDecls(@import("safe_hash_map.zig"));
    std.testing.refAllDecls(@import("safe_list.zig"));
    std.testing.refAllDecls(@import("SortedArrayBuilder.zig"));
}
