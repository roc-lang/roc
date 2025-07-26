//! A specialized collection for tracking exposed items in modules using string keys.
//!
//! This combines the functionality of exposed_by_str and exposed_nodes into a single
//! SortedArrayBuilder to save memory and simplify the API. It uses string keys
//! (the exposed item names) and u16 values (the node indices).
//!
//! A value of 0 means "exposed but not yet defined", while non-zero values
//! are actual node indices.

const std = @import("std");
const Allocator = std.mem.Allocator;
const SortedArrayBuilder = @import("SortedArrayBuilder.zig").SortedArrayBuilder;

/// A collection that tracks exposed items by their names and associated CIR node indices
pub const ExposedItems = struct {
    /// Maps item name -> CIR node index (0 means exposed but not defined)
    items: SortedArrayBuilder([]const u8, u16),

    const Self = @This();

    pub fn init() Self {
        return .{
            .items = SortedArrayBuilder([]const u8, u16).init(),
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.items.deinit(allocator);
    }

    /// Add an exposed item (equivalent to exposed_by_str.put)
    /// If the item already exists, this is a no-op (keeping the existing value)
    pub fn addExposed(self: *Self, allocator: Allocator, name: []const u8) !void {
        // Add with value 0 to indicate "exposed but not yet defined"
        // The SortedArrayBuilder will handle duplicates by keeping the last value,
        // but we don't want to overwrite an existing node index with 0
        if (self.items.sorted) {
            // If already sorted, check if it exists first
            if (self.items.get(allocator, name) == null) {
                try self.items.put(allocator, name, 0);
            }
        } else {
            // If not sorted yet, just add it - duplicates will be handled later
            try self.items.put(allocator, name, 0);
        }
    }

    /// Set the node index for an exposed item (equivalent to exposed_nodes.put)
    pub fn setNodeIndex(self: *Self, allocator: Allocator, name: []const u8, node_idx: u16) !void {
        // This will overwrite any existing value (including 0)
        try self.items.put(allocator, name, node_idx);
    }

    /// Check if an item is exposed (equivalent to exposed_by_str.contains)
    pub fn contains(self: *const Self, allocator: Allocator, name: []const u8) bool {
        // Cast away const for the lookup since SortedArrayBuilder.get may sort the array
        var mutable_self = @constCast(self);
        return mutable_self.items.get(allocator, name) != null;
    }

    /// Get the node index for an exposed item (equivalent to exposed_nodes.get)
    /// Returns null if not exposed, or the node index (which may be 0)
    pub fn getNodeIndex(self: *const Self, allocator: Allocator, name: []const u8) ?u16 {
        // Cast away const for the lookup since SortedArrayBuilder.get may sort the array
        var mutable_self = @constCast(self);
        return mutable_self.items.get(allocator, name);
    }

    /// Get the number of exposed items
    pub fn count(self: *const Self) usize {
        return self.items.count();
    }

    /// Ensure the array is sorted and deduplicated
    /// This should be called at the end of canonicalization
    pub fn ensureSorted(self: *Self, allocator: Allocator) void {
        self.items.ensureSorted(allocator);
    }

    /// Detect duplicate exposed items for error reporting
    pub fn detectDuplicates(self: *Self, allocator: Allocator) ![][]const u8 {
        return self.items.detectDuplicates(allocator);
    }

    /// Relocate pointers after memory movement
    pub fn relocate(self: *Self, offset: isize) void {
        self.items.relocate(offset);
    }

    /// Append to iovec writer for serialization
    pub fn appendToIovecs(self: *const Self, writer: anytype) !usize {
        return self.items.appendToIovecs(writer);
    }

    /// Iterator for all exposed items
    pub const Iterator = struct {
        items: []const SortedArrayBuilder([]const u8, u16).Entry,
        index: usize,

        pub fn next(self: *Iterator) ?struct { name: []const u8, node_idx: u16 } {
            if (self.index < self.items.len) {
                const entry = self.items[self.index];
                self.index += 1;
                return .{ .name = entry.key, .node_idx = entry.value };
            }
            return null;
        }
    };

    /// Get an iterator over all exposed items
    pub fn iterator(self: *const Self) Iterator {
        return .{ .items = self.items.entries.items, .index = 0 };
    }
};

test "ExposedItems basic operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var exposed = ExposedItems.init();
    defer exposed.deinit(allocator);

    // Add exposed items (like exposed_by_str.put)
    try exposed.addExposed(allocator, "foo");
    try exposed.addExposed(allocator, "bar");
    try exposed.addExposed(allocator, "MyType");

    // Test count
    try testing.expectEqual(@as(usize, 3), exposed.count());

    // Test contains (like exposed_by_str.contains)
    try testing.expect(exposed.contains(allocator, "foo"));
    try testing.expect(exposed.contains(allocator, "bar"));
    try testing.expect(exposed.contains(allocator, "MyType"));
    try testing.expect(!exposed.contains(allocator, "missing"));

    // Test getNodeIndex before setting (should be 0)
    try testing.expectEqual(@as(?u16, 0), exposed.getNodeIndex(allocator, "foo"));

    // Set node indices (like exposed_nodes.put)
    try exposed.setNodeIndex(allocator, "foo", 42);
    try exposed.setNodeIndex(allocator, "bar", 84);

    // Test getNodeIndex after setting
    try testing.expectEqual(@as(?u16, 42), exposed.getNodeIndex(allocator, "foo"));
    try testing.expectEqual(@as(?u16, 84), exposed.getNodeIndex(allocator, "bar"));
    try testing.expectEqual(@as(?u16, 0), exposed.getNodeIndex(allocator, "MyType")); // Not set
    try testing.expectEqual(@as(?u16, null), exposed.getNodeIndex(allocator, "missing")); // Not exposed
}

test "ExposedItems handles duplicates" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var exposed = ExposedItems.init();
    defer exposed.deinit(allocator);

    // Add items with duplicates
    try exposed.addExposed(allocator, "foo");
    try exposed.setNodeIndex(allocator, "foo", 42);
    try exposed.addExposed(allocator, "foo"); // Duplicate - should not overwrite node index

    // Force sorting
    exposed.ensureSorted(allocator);

    // Should still have the node index
    try testing.expectEqual(@as(?u16, 42), exposed.getNodeIndex(allocator, "foo"));
}

test "ExposedItems detectDuplicates" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var exposed = ExposedItems.init();
    defer exposed.deinit(allocator);

    // Add some duplicates
    try exposed.addExposed(allocator, "foo");
    try exposed.addExposed(allocator, "bar");
    try exposed.addExposed(allocator, "foo"); // duplicate
    try exposed.setNodeIndex(allocator, "bar", 42);
    try exposed.addExposed(allocator, "bar"); // duplicate

    // Detect duplicates
    const duplicates = try exposed.detectDuplicates(allocator);
    defer allocator.free(duplicates);

    // Should report both duplicates
    try testing.expectEqual(@as(usize, 2), duplicates.len);
}
