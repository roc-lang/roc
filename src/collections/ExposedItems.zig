//! A specialized collection for tracking exposed items in modules.
//!
//! This combines the functionality of exposed_by_str and exposed_nodes into a single
//! SortedArrayBuilder to save memory and simplify the API. Instead of storing intern
//! indices twice (once as keys in each map), we store them once with their node indices.

const std = @import("std");
const Allocator = std.mem.Allocator;
const collections = @import("mod.zig");

/// A collection that tracks exposed items by their intern indices and associated CIR node indices
pub const ExposedItems = struct {
    /// Maps intern index -> CIR node index for exposed items
    items: collections.SortedArrayBuilder(u32, u16),

    const Self = @This();

    pub fn init() Self {
        return .{
            .items = collections.SortedArrayBuilder(u32, u16).init(),
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.items.deinit(allocator);
    }

    /// Add an exposed item with its intern index and CIR node index
    pub fn put(self: *Self, allocator: Allocator, intern_idx: u32, node_idx: u16) !void {
        try self.items.put(allocator, intern_idx, node_idx);
    }

    /// Check if an intern index is exposed (equivalent to the old exposed_by_str.get() != null check)
    pub fn isExposed(self: *Self, allocator: Allocator, intern_idx: u32) bool {
        return self.items.get(allocator, intern_idx) != null;
    }

    /// Get the CIR node index for an exposed intern index (equivalent to exposed_nodes.get())
    pub fn getNodeIndex(self: *Self, allocator: Allocator, intern_idx: u32) ?u16 {
        return self.items.get(allocator, intern_idx);
    }

    /// Get the number of exposed items
    pub fn count(self: *const Self) usize {
        return self.items.count();
    }

    /// Ensure the array is sorted and deduplicated
    pub fn ensureSorted(self: *Self, allocator: Allocator) void {
        self.items.ensureSorted(allocator);
    }

    /// Relocate pointers after memory movement
    pub fn relocate(self: *Self, offset: isize) void {
        self.items.relocate(offset);
    }

    /// Get serialized size
    pub fn serializedSize(self: *const Self) usize {
        return self.items.serializedSize();
    }

    /// Append to iovec writer for serialization
    pub fn appendToIovecs(self: *const Self, writer: anytype) !usize {
        return self.items.appendToIovecs(writer);
    }

    /// Serialize into buffer
    pub fn serializeInto(self: *const Self, allocator: Allocator, buffer: []u8) ![]u8 {
        return self.items.serializeInto(allocator, buffer);
    }

    /// Deserialize from buffer
    pub fn deserializeFrom(buffer: []const u8, allocator: Allocator) !Self {
        const items = try collections.SortedArrayBuilder(u32, u16).deserializeFrom(buffer, allocator);
        return Self{ .items = items };
    }
};

test "ExposedItems basic operations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var exposed = ExposedItems.init();
    defer exposed.deinit(allocator);

    // Add items
    try exposed.put(allocator, 100, 42);
    try exposed.put(allocator, 200, 84);
    try exposed.put(allocator, 150, 63);

    // Test count
    try testing.expectEqual(@as(usize, 3), exposed.count());

    // Test isExposed (forces sorting)
    try testing.expect(exposed.isExposed(allocator, 100));
    try testing.expect(exposed.isExposed(allocator, 200));
    try testing.expect(exposed.isExposed(allocator, 150));
    try testing.expect(!exposed.isExposed(allocator, 999));

    // Test getNodeIndex
    try testing.expectEqual(@as(?u16, 42), exposed.getNodeIndex(allocator, 100));
    try testing.expectEqual(@as(?u16, 84), exposed.getNodeIndex(allocator, 200));
    try testing.expectEqual(@as(?u16, 63), exposed.getNodeIndex(allocator, 150));
    try testing.expectEqual(@as(?u16, null), exposed.getNodeIndex(allocator, 999));
}

test "ExposedItems handles duplicates" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var exposed = ExposedItems.init();
    defer exposed.deinit(allocator);

    // Add duplicate keys with different values
    try exposed.put(allocator, 100, 42);
    try exposed.put(allocator, 200, 84);
    try exposed.put(allocator, 100, 99); // duplicate key, should keep last value

    // Initial count includes duplicates
    try testing.expectEqual(@as(usize, 3), exposed.count());

    // Force sorting and deduplication
    exposed.ensureSorted(allocator);

    // Count should be reduced after deduplication
    try testing.expectEqual(@as(usize, 2), exposed.count());

    // Should return the last value for the duplicate key
    try testing.expectEqual(@as(?u16, 99), exposed.getNodeIndex(allocator, 100));
    try testing.expectEqual(@as(?u16, 84), exposed.getNodeIndex(allocator, 200));
}

test "ExposedItems serialization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var exposed = ExposedItems.init();
    defer exposed.deinit(allocator);

    // Add items
    try exposed.put(allocator, 300, 1);
    try exposed.put(allocator, 100, 2);
    try exposed.put(allocator, 200, 3);

    // Ensure sorted before serialization
    exposed.ensureSorted(allocator);

    // Test serialization
    const size = exposed.serializedSize();
    const buffer = try allocator.alloc(u8, size);
    defer allocator.free(buffer);

    const serialized = try exposed.serializeInto(allocator, buffer);
    try testing.expectEqual(size, serialized.len);

    // Test deserialization
    var deserialized = try ExposedItems.deserializeFrom(serialized, allocator);
    defer deserialized.deinit(allocator);

    try testing.expectEqual(@as(usize, 3), deserialized.count());
    try testing.expectEqual(@as(?u16, 2), deserialized.getNodeIndex(allocator, 100));
    try testing.expectEqual(@as(?u16, 3), deserialized.getNodeIndex(allocator, 200));
    try testing.expectEqual(@as(?u16, 1), deserialized.getNodeIndex(allocator, 300));
}

test "ExposedItems serialization parity" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var exposed = ExposedItems.init();
    defer exposed.deinit(allocator);

    // Add test data
    try exposed.put(allocator, 500, 10);
    try exposed.put(allocator, 100, 20);
    try exposed.put(allocator, 300, 30);
    try exposed.put(allocator, 200, 40);
    
    // Ensure sorted
    exposed.ensureSorted(allocator);

    // Test old method
    const old_size = exposed.serializedSize();
    const old_buffer = try allocator.alloc(u8, old_size);
    defer allocator.free(old_buffer);
    const old_serialized = try exposed.serializeInto(allocator, old_buffer);

    // Test new method
    const base = @import("base");
    var writer = base.iovec_serialize.IovecWriter.init(allocator);
    defer writer.deinit();
    _ = try exposed.appendToIovecs(&writer);
    const new_serialized = try writer.serialize(allocator);
    defer allocator.free(new_serialized);

    // Both methods should produce identical output
    try testing.expectEqual(old_serialized.len, new_serialized.len);
    try testing.expectEqualSlices(u8, old_serialized, new_serialized);
}

test "ExposedItems round-trip parity" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var original = ExposedItems.init();
    defer original.deinit(allocator);

    // Add test data
    try original.put(allocator, 0, 0); // min values
    try original.put(allocator, 0xFFFFFFFF, 0xFFFF); // max values
    try original.put(allocator, 12345, 999);
    
    // Ensure sorted
    original.ensureSorted(allocator);

    // Serialize with old method, deserialize
    const old_size = original.serializedSize();
    const old_buffer = try allocator.alloc(u8, old_size);
    defer allocator.free(old_buffer);
    const old_serialized = try original.serializeInto(allocator, old_buffer);
    
    var from_old = try ExposedItems.deserializeFrom(old_serialized, allocator);
    defer from_old.deinit(allocator);

    // Serialize with new method, deserialize
    const base = @import("base");
    var writer = base.iovec_serialize.IovecWriter.init(allocator);
    defer writer.deinit();
    _ = try original.appendToIovecs(&writer);
    const new_serialized = try writer.serialize(allocator);
    defer allocator.free(new_serialized);
    
    var from_new = try ExposedItems.deserializeFrom(new_serialized, allocator);
    defer from_new.deinit(allocator);

    // Verify both deserialized versions are identical
    try testing.expectEqual(from_old.count(), from_new.count());
    try testing.expectEqual(@as(?u16, 0), from_old.getNodeIndex(allocator, 0));
    try testing.expectEqual(@as(?u16, 0), from_new.getNodeIndex(allocator, 0));
    try testing.expectEqual(@as(?u16, 0xFFFF), from_old.getNodeIndex(allocator, 0xFFFFFFFF));
    try testing.expectEqual(@as(?u16, 0xFFFF), from_new.getNodeIndex(allocator, 0xFFFFFFFF));
    try testing.expectEqual(@as(?u16, 999), from_old.getNodeIndex(allocator, 12345));
    try testing.expectEqual(@as(?u16, 999), from_new.getNodeIndex(allocator, 12345));
}
