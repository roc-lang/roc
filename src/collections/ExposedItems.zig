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
const CompactWriter = @import("CompactWriter.zig");

// We use u32 which is the bit representation of base.Ident.Idx
// This includes both the 29-bit index AND the 3-bit attributes (effectful, ignored, reassignable)
// This is critical because foo, foo!, and _foo are different identifiers
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

    /// Add an exposed item by its interned ID (pass @bitCast(base.Ident.Idx) to u32)
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

    /// Set the node index for an exposed item by its interned ID (pass @bitCast(base.Ident.Idx) to u32)
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

    /// Check if an item is exposed by its interned ID (pass @bitCast(base.Ident.Idx) to u32)
    pub fn containsById(self: *const Self, allocator: Allocator, ident_idx: IdentIdx) bool {
        var mutable_self = @constCast(self);
        return mutable_self.items.get(allocator, ident_idx) != null;
    }

    /// Get the node index for an exposed item by its interned ID (pass @bitCast(base.Ident.Idx) to u32)
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

    /// Serialized representation of ExposedItems
    pub const Serialized = struct {
        items: SortedArrayBuilder(IdentIdx, u16).Serialized,

        /// Serialize an ExposedItems into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            exposed_items: *const ExposedItems,
            allocator: Allocator,
            writer: *CompactWriter,
        ) Allocator.Error!void {
            try self.items.serialize(&exposed_items.items, allocator, writer);
        }

        /// Deserialize this Serialized struct into an ExposedItems
        pub fn deserialize(self: *Serialized, offset: i64) *ExposedItems {
            // ExposedItems.Serialized should be at least as big as ExposedItems
            std.debug.assert(@sizeOf(Serialized) >= @sizeOf(ExposedItems));

            // Overwrite ourself with the deserialized version, and return our pointer after casting it to Self.
            const exposed_items = @as(*ExposedItems, @ptrFromInt(@intFromPtr(self)));

            exposed_items.* = ExposedItems{
                .items = self.items.deserialize(offset).*,
            };

            return exposed_items;
        }
    };

    /// Serialize this ExposedItems to the given CompactWriter. The resulting ExposedItems
    /// in the writer's buffer will have offsets instead of pointers. Calling any
    /// methods on it or dereferencing its internal "pointers" (which are now
    /// offsets) is illegal behavior!
    pub fn serialize(
        self: *const Self,
        allocator: Allocator,
        writer: *CompactWriter,
    ) Allocator.Error!*const Self {
        // Items must be sorted and deduplicated before serialization
        std.debug.assert(self.items.sorted);
        std.debug.assert(self.items.isDeduplicated());

        // First, write the ExposedItems struct itself
        const offset_self = try writer.appendAlloc(allocator, Self);

        // Serialize the SortedArrayBuilder's entries
        const entries = self.items.entries.items;
        const entries_offset = if (entries.len > 0) blk: {
            // Write the entries array
            const offset = try writer.appendSlice(allocator, entries);
            break :blk offset;
        } else null;

        // Update the struct with serialized data
        offset_self.* = .{
            .items = .{
                .entries = .{
                    .items = if (entries_offset) |offset| offset else entries[0..0],
                    .capacity = entries.len,
                },
                .sorted = self.items.sorted,
            },
        };

        return @constCast(offset_self);
    }

    /// Iterator for all exposed items
    pub const Iterator = struct {
        // items: []const SortedArrayBuilder(IdentIdx, u16).Entry,
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

    // Simulate some interned IDs (in real usage these would come from Ident.Store)
    const foo_id: IdentIdx = 100;
    const bar_id: IdentIdx = 200;
    const my_type_id: IdentIdx = 300;

    // Add exposed items
    try exposed.addExposedById(allocator, foo_id);
    try exposed.addExposedById(allocator, bar_id);
    try exposed.addExposedById(allocator, my_type_id);

    // Test count
    try testing.expectEqual(@as(usize, 3), exposed.count());

    // Test contains
    try testing.expect(exposed.containsById(allocator, foo_id));
    try testing.expect(exposed.containsById(allocator, bar_id));
    try testing.expect(exposed.containsById(allocator, my_type_id));
    try testing.expect(!exposed.containsById(allocator, 999)); // missing

    // Test getNodeIndexById before setting (should be 0)
    try testing.expectEqual(@as(?u16, 0), exposed.getNodeIndexById(allocator, foo_id));

    // Set node indices
    try exposed.setNodeIndexById(allocator, foo_id, 42);
    try exposed.setNodeIndexById(allocator, bar_id, 84);

    // Test getNodeIndexById after setting
    try testing.expectEqual(@as(?u16, 42), exposed.getNodeIndexById(allocator, foo_id));
    try testing.expectEqual(@as(?u16, 84), exposed.getNodeIndexById(allocator, bar_id));
    try testing.expectEqual(@as(?u16, 0), exposed.getNodeIndexById(allocator, my_type_id)); // Not set
    try testing.expectEqual(@as(?u16, null), exposed.getNodeIndexById(allocator, 999)); // Not exposed
}

test "ExposedItems empty CompactWriter roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create an empty ExposedItems
    var original = ExposedItems.init();
    defer original.deinit(allocator);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_exposed.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    _ = try original.serialize(allocator, &writer);

    // Write to file
    try writer.writeGather(allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(file_size));
    defer allocator.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*ExposedItems, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(ExposedItems))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.count());
}

test "ExposedItems basic CompactWriter roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create original ExposedItems and add some items
    var original = ExposedItems.init();
    defer original.deinit(allocator);

    // Add exposed items with various IDs
    const id1: IdentIdx = 100;
    const id2: IdentIdx = 200;
    const id3: IdentIdx = 300;

    try original.addExposedById(allocator, id1);
    try original.addExposedById(allocator, id2);
    try original.addExposedById(allocator, id3);

    // Set node indices
    try original.setNodeIndexById(allocator, id1, 42);
    try original.setNodeIndexById(allocator, id2, 84);
    // id3 left as 0 (exposed but not defined)

    // Ensure sorted before serialization
    original.ensureSorted(allocator);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_exposed.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    _ = try original.serialize(allocator, &writer);

    // Write to file
    try writer.writeGather(allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ExposedItems.Serialized)), @intCast(file_size));
    defer allocator.free(buffer);

    _ = try file.read(buffer);

    // The serialized ExposedItems.Serialized struct is at the beginning of the buffer
    // (appendAlloc is called first in serialize)
    const serialized_ptr = @as(*ExposedItems.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the items are accessible
    try testing.expectEqual(@as(usize, 3), deserialized.count());
    try testing.expectEqual(@as(?u16, 42), deserialized.getNodeIndexById(allocator, id1));
    try testing.expectEqual(@as(?u16, 84), deserialized.getNodeIndexById(allocator, id2));
    try testing.expectEqual(@as(?u16, 0), deserialized.getNodeIndexById(allocator, id3));
}

test "ExposedItems with duplicates CompactWriter roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var original = ExposedItems.init();
    defer original.deinit(allocator);

    // Add items with duplicates
    const id1: IdentIdx = 100;
    const id2: IdentIdx = 200;

    try original.addExposedById(allocator, id1);
    try original.setNodeIndexById(allocator, id1, 42);
    try original.addExposedById(allocator, id2);
    try original.addExposedById(allocator, id1); // duplicate
    try original.setNodeIndexById(allocator, id2, 84);
    try original.addExposedById(allocator, id2); // duplicate

    // Ensure sorted before serialization (this will also deduplicate)
    original.ensureSorted(allocator);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_duplicates_exposed.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    _ = try original.serialize(allocator, &writer);

    // Write to file
    try writer.writeGather(allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ExposedItems.Serialized)), @intCast(file_size));
    defer allocator.free(buffer);

    _ = try file.read(buffer);

    // The serialized ExposedItems.Serialized struct is at the beginning of the buffer
    // (appendAlloc is called first in serialize)
    const serialized_ptr: *ExposedItems.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

    // After deduplication, should have only 2 items
    try testing.expectEqual(@as(usize, 2), deserialized.count());
    try testing.expectEqual(@as(?u16, 42), deserialized.getNodeIndexById(allocator, id1));
    try testing.expectEqual(@as(?u16, 84), deserialized.getNodeIndexById(allocator, id2));
}

test "ExposedItems comprehensive CompactWriter roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var original = ExposedItems.init();
    defer original.deinit(allocator);

    // Test with many items including edge cases
    const test_items = [_]struct { id: IdentIdx, node_idx: u16 }{
        .{ .id = 0, .node_idx = 0 }, // minimum ID
        .{ .id = 1, .node_idx = 100 },
        .{ .id = 42, .node_idx = 200 },
        .{ .id = 100, .node_idx = 300 },
        .{ .id = 1000, .node_idx = 400 },
        .{ .id = 10000, .node_idx = 500 },
        .{ .id = 65535, .node_idx = 600 }, // near max u16 node index
        .{ .id = 100000, .node_idx = 0 }, // exposed but not defined
        .{ .id = std.math.maxInt(u32) - 1, .node_idx = 999 }, // near max ID
    };

    // Add all items
    for (test_items) |item| {
        try original.addExposedById(allocator, item.id);
        if (item.node_idx > 0) {
            try original.setNodeIndexById(allocator, item.id, item.node_idx);
        }
    }

    // Ensure sorted before serialization
    original.ensureSorted(allocator);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_comprehensive_exposed.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    _ = try original.serialize(allocator, &writer);

    // Write to file
    try writer.writeGather(allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const serialized_align = @alignOf(ExposedItems);
    const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(serialized_align), @intCast(file_size));
    defer allocator.free(buffer);

    _ = try file.read(buffer);

    // Cast to Serialized type and deserialize
    const serialized_ptr: *ExposedItems.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all items
    try testing.expectEqual(@as(usize, test_items.len), deserialized.count());
    for (test_items) |item| {
        const actual = deserialized.getNodeIndexById(allocator, item.id);
        try testing.expectEqual(@as(?u16, item.node_idx), actual);
    }
}

test "ExposedItems edge cases CompactWriter roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Test empty ExposedItems
    {
        var exposed = ExposedItems.init();
        defer exposed.deinit(allocator);

        // Empty collections should serialize and deserialize correctly
        var writer = CompactWriter.init();
        defer writer.deinit(allocator);

        _ = try exposed.serialize(allocator, &writer);

        const buffer = try allocator.alloc(u8, writer.total_bytes);
        defer allocator.free(buffer);
        _ = try writer.writeToBuffer(buffer);

        const serialized_ptr = @as(*ExposedItems.Serialized, @ptrCast(@alignCast(buffer.ptr)));
        const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

        try testing.expectEqual(@as(usize, 0), deserialized.count());
    }

    // Test single item with file I/O to ensure writeGather works correctly
    {
        var exposed = ExposedItems.init();
        defer exposed.deinit(allocator);

        try exposed.addExposedById(allocator, 100);
        try exposed.setNodeIndexById(allocator, 100, 42);
        exposed.ensureSorted(allocator);

        // Create a temp file
        var tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const file = try tmp_dir.dir.createFile("test_single.dat", .{ .read = true });
        defer file.close();

        var writer = CompactWriter.init();
        defer writer.deinit(allocator);

        _ = try exposed.serialize(allocator, &writer);

        // Test writeGather
        try writer.writeGather(allocator, file);

        // Read back and verify
        try file.seekTo(0);
        const file_size = try file.getEndPos();
        const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ExposedItems.Serialized)), @intCast(file_size));
        defer allocator.free(buffer);

        _ = try file.read(buffer);

        const serialized_ptr = @as(*ExposedItems.Serialized, @ptrCast(@alignCast(buffer.ptr)));
        const deserialized = serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

        try testing.expectEqual(@as(usize, 1), deserialized.count());
        try testing.expectEqual(@as(?u16, 42), deserialized.getNodeIndexById(allocator, 100));
    }
}
