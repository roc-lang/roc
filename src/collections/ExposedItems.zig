//! A specialized collection for tracking exposed items in modules using interned IDs.
//!
//! This combines the functionality of exposed_by_str and exposed_nodes into a single
//! SortedArrayBuilder to save memory and simplify the API. It uses interned identifier
//! indices (u32 matching Ident.Idx) as keys and u16 values (the node indices).
//!
//! A value of 0 means "exposed but not yet defined", while non-zero values
//! are actual node indices.
//!
//! Note: This module only provides ID-based methods. String-to-ID conversion must be
//! handled by the caller who has access to the Ident.Store.

const std = @import("std");
const Allocator = std.mem.Allocator;
const SortedArrayBuilder = @import("SortedArrayBuilder.zig").SortedArrayBuilder;

// We use u32 for the identifier index type to match Ident.Idx
// This avoids needing to import base module but maintains type compatibility
const IdentIdx = u32;

/// A collection that tracks exposed items by their names and associated CIR node indices
pub const ExposedItems = struct {
    /// Maps item name (as interned ID) -> CIR node index (0 means exposed but not defined)
    items: SortedArrayBuilder(IdentIdx, u16),

    const Self = @This();

    pub fn init() Self {
        return .{
            .items = SortedArrayBuilder(IdentIdx, u16).init(),
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.items.deinit(allocator);
    }

    /// Add an exposed item by its interned ID
    pub fn addExposedById(self: *Self, allocator: Allocator, ident_idx: IdentIdx) !void {
        // Add with value 0 to indicate "exposed but not yet defined"
        // The SortedArrayBuilder will handle duplicates by keeping the last value,
        // but we don't want to overwrite an existing node index with 0
        if (self.items.sorted) {
            // If already sorted, check if it exists first
            if (self.items.get(allocator, ident_idx) == null) {
                try self.items.put(allocator, ident_idx, 0);
            }
        } else {
            // If not sorted yet, just add it - duplicates will be handled later
            try self.items.put(allocator, ident_idx, 0);
        }
    }

    /// Set the node index for an exposed item by its interned ID
    pub fn setNodeIndexById(self: *Self, allocator: Allocator, ident_idx: IdentIdx, node_idx: u16) !void {
        // First ensure the array is sorted so we can search
        self.items.ensureSorted(allocator);
        
        // Find the existing entry and update its value
        const entries = self.items.entries.items;
        for (entries) |*entry| {
            if (entry.key == ident_idx) {
                entry.value = node_idx;
                return;
            }
        }
        
        // If not found, add a new entry
        try self.items.put(allocator, ident_idx, node_idx);
    }

    /// Check if an item is exposed by its interned ID
    pub fn containsById(self: *const Self, allocator: Allocator, ident_idx: IdentIdx) bool {
        var mutable_self = @constCast(self);
        return mutable_self.items.get(allocator, ident_idx) != null;
    }

    /// Get the node index for an exposed item by its interned ID
    pub fn getNodeIndexById(self: *const Self, allocator: Allocator, ident_idx: IdentIdx) ?u16 {
        var mutable_self = @constCast(self);
        return mutable_self.items.get(allocator, ident_idx);
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
    /// Returns the interned IDs of duplicates (caller must convert to strings)
    pub fn detectDuplicates(self: *Self, allocator: Allocator) ![]IdentIdx {
        return self.items.detectDuplicates(allocator);
    }

    /// Relocate pointers after memory movement
    pub fn relocate(self: *Self, offset: isize) void {
        self.items.relocate(offset);
    }

    /// Calculate the size needed to serialize this ExposedItems
    pub fn serializedSize(self: *const Self) usize {
        // We need to serialize:
        // 1. The count of items (u32)
        // 2. For each item: key (u32) + value (u16)
        var size: usize = @sizeOf(u32); // count
        
        for (self.items.entries.items) |_| {
            size += @sizeOf(u32); // key (interned ID)
            size += @sizeOf(u16); // value
        }
        
        // Align to SERIALIZATION_ALIGNMENT
        const SERIALIZATION_ALIGNMENT = 16;
        return std.mem.alignForward(usize, size, SERIALIZATION_ALIGNMENT);
    }
    
    /// Serialize this ExposedItems into the provided buffer
    pub fn serializeInto(self: *const Self, buffer: []u8) ![]u8 {
        const size = self.serializedSize();
        if (buffer.len < size) return error.BufferTooSmall;
        
        var offset: usize = 0;
        
        // Write count
        std.mem.writeInt(u32, buffer[offset..][0..4], @intCast(self.items.entries.items.len), .little);
        offset += @sizeOf(u32);
        
        // Write entries
        for (self.items.entries.items) |entry| {
            // Write key (interned ID)
            std.mem.writeInt(u32, buffer[offset..][0..4], entry.key, .little);
            offset += @sizeOf(u32);
            
            // Write value
            std.mem.writeInt(u16, buffer[offset..][0..2], entry.value, .little);
            offset += @sizeOf(u16);
        }
        
        // Zero padding
        if (offset < size) {
            @memset(buffer[offset..size], 0);
        }
        
        return buffer[0..size];
    }
    
    /// Deserialize ExposedItems from the provided buffer
    pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) !Self {
        if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;
        
        var offset: usize = 0;
        
        // Read count
        const entry_count = std.mem.readInt(u32, buffer[offset..][0..4], .little);
        offset += @sizeOf(u32);
        
        var result = Self.init();
        errdefer result.deinit(allocator);
        
        // Read entries
        for (0..entry_count) |_| {
            // Read key (interned ID)
            if (offset + @sizeOf(u32) > buffer.len) return error.BufferTooSmall;
            const key = std.mem.readInt(u32, buffer[offset..][0..4], .little);
            offset += @sizeOf(u32);
            
            // Read value
            if (offset + @sizeOf(u16) > buffer.len) return error.BufferTooSmall;
            const value = std.mem.readInt(u16, buffer[offset..][0..2], .little);
            offset += @sizeOf(u16);
            
            // Add to builder
            try result.items.put(allocator, key, value);
        }
        
        return result;
    }
    
    /// Append to iovec writer for serialization
    pub fn appendToIovecs(self: *const Self, writer: anytype) !usize {
        return self.items.appendToIovecs(writer);
    }

    /// Iterator for all exposed items
    pub const Iterator = struct {
        items: []const SortedArrayBuilder(IdentIdx, u16).Entry,
        index: usize,

        pub fn next(self: *Iterator) ?struct { ident_idx: IdentIdx, node_idx: u16 } {
            if (self.index < self.items.len) {
                const entry = self.items[self.index];
                self.index += 1;
                return .{ .ident_idx = entry.key, .node_idx = entry.value };
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
