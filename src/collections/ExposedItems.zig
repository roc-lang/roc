//! A specialized collection for tracking exposed items in modules using interned IDs.
//!
//! This combines the functionality of exposed_by_str and exposed_nodes into a single
//! SortedArrayBuilder to save memory and simplify the API. It uses interned identifier
//! indices (u32 matching Ident.Idx) as keys and explicit export targets as values.
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

/// The resolved target for an exposed item.
pub const ExposedItemTarget = extern struct {
    kind: u32,
    node_idx: u32,

    pub const Kind = enum(u32) {
        unresolved,
        value_def,
        type_decl,
    };

    pub fn unresolved() ExposedItemTarget {
        return .{
            .kind = @intFromEnum(Kind.unresolved),
            .node_idx = 0,
        };
    }

    pub fn valueDef(node_idx: u32) ExposedItemTarget {
        return .{
            .kind = @intFromEnum(Kind.value_def),
            .node_idx = node_idx,
        };
    }

    pub fn typeDecl(node_idx: u32) ExposedItemTarget {
        return .{
            .kind = @intFromEnum(Kind.type_decl),
            .node_idx = node_idx,
        };
    }

    pub fn tag(self: ExposedItemTarget) Kind {
        return @enumFromInt(self.kind);
    }

    pub fn isResolved(self: ExposedItemTarget) bool {
        return self.tag() != .unresolved;
    }

    pub fn valueDefNode(self: ExposedItemTarget) ?u32 {
        return if (self.tag() == .value_def) self.node_idx else null;
    }

    pub fn typeDeclNode(self: ExposedItemTarget) ?u32 {
        return if (self.tag() == .type_decl) self.node_idx else null;
    }
};

/// A collection that tracks exposed items by their names and explicit targets.
pub const ExposedItems = struct {
    /// Maps item name (as interned ID) -> resolved or unresolved exposure target.
    items: SortedArrayBuilder(IdentIdx, ExposedItemTarget),

    const Self = @This();

    pub fn init() Self {
        return .{
            .items = SortedArrayBuilder(IdentIdx, ExposedItemTarget).init(),
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.items.deinit(allocator);
    }

    pub fn clone(self: *const Self, allocator: Allocator) Allocator.Error!Self {
        return .{
            .items = try self.items.clone(allocator),
        };
    }

    fn findIndex(self: *Self, allocator: Allocator, ident_idx: IdentIdx) struct { index: usize, found: bool } {
        self.items.ensureSorted(allocator);

        const entries = self.items.entries.items;
        var left: usize = 0;
        var right: usize = entries.len;

        while (left < right) {
            const mid = left + (right - left) / 2;
            const mid_key = entries[mid].key;
            if (mid_key == ident_idx) {
                return .{ .index = mid, .found = true };
            } else if (mid_key < ident_idx) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }

        return .{ .index = left, .found = false };
    }

    /// Add an exposed item by its interned ID (pass @bitCast(base.Ident.Idx) to u32)
    pub fn addExposedById(self: *Self, allocator: Allocator, ident_idx: IdentIdx) Allocator.Error!void {
        const found = self.findIndex(allocator, ident_idx);
        if (found.found) return;

        try self.items.entries.insert(allocator, found.index, .{
            .key = ident_idx,
            .value = ExposedItemTarget.unresolved(),
        });
    }

    fn setTargetById(self: *Self, allocator: Allocator, ident_idx: IdentIdx, target: ExposedItemTarget) Allocator.Error!void {
        const found = self.findIndex(allocator, ident_idx);
        if (found.found) {
            self.items.entries.items[found.index].value = target;
            return;
        }

        try self.items.entries.insert(allocator, found.index, .{
            .key = ident_idx,
            .value = target,
        });
    }

    /// Set the value definition target for an exposed item by its interned ID.
    pub fn setValueNodeIndexById(self: *Self, allocator: Allocator, ident_idx: IdentIdx, node_idx: u32) Allocator.Error!void {
        try self.setTargetById(allocator, ident_idx, ExposedItemTarget.valueDef(node_idx));
    }

    /// Set the type declaration target for an exposed item by its interned ID.
    pub fn setTypeNodeIndexById(self: *Self, allocator: Allocator, ident_idx: IdentIdx, node_idx: u32) Allocator.Error!void {
        try self.setTargetById(allocator, ident_idx, ExposedItemTarget.typeDecl(node_idx));
    }

    /// Check if an item is exposed by its interned ID (pass @bitCast(base.Ident.Idx) to u32)
    pub fn containsById(self: *const Self, allocator: Allocator, ident_idx: IdentIdx) bool {
        var mutable_self = @constCast(self);
        return mutable_self.items.get(allocator, ident_idx) != null;
    }

    /// Get the explicit target for an exposed item by its interned ID.
    pub fn getTargetById(self: *const Self, allocator: Allocator, ident_idx: IdentIdx) ?ExposedItemTarget {
        var mutable_self = @constCast(self);
        return mutable_self.items.get(allocator, ident_idx);
    }

    /// Get the value definition node for an exposed item by its interned ID.
    pub fn getValueNodeIndexById(self: *const Self, allocator: Allocator, ident_idx: IdentIdx) ?u32 {
        const target = self.getTargetById(allocator, ident_idx) orelse return null;
        return target.valueDefNode();
    }

    /// Get the type declaration node for an exposed item by its interned ID.
    pub fn getTypeNodeIndexById(self: *const Self, allocator: Allocator, ident_idx: IdentIdx) ?u32 {
        const target = self.getTargetById(allocator, ident_idx) orelse return null;
        return target.typeDeclNode();
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
    pub fn detectDuplicates(self: *Self, allocator: Allocator) Allocator.Error![]IdentIdx {
        return self.items.detectDuplicates(allocator);
    }

    /// Relocate pointers after memory movement
    pub fn relocate(self: *Self, offset: isize) void {
        self.items.relocate(offset);
    }

    /// Serialized representation of ExposedItems
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        items: SortedArrayBuilder(IdentIdx, ExposedItemTarget).Serialized,

        /// Serialize an ExposedItems into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            exposed_items: *const ExposedItems,
            allocator: Allocator,
            writer: *CompactWriter,
        ) Allocator.Error!void {
            try self.items.serialize(&exposed_items.items, allocator, writer);
        }

        /// Deserialize into an ExposedItems value (no in-place modification of cache buffer).
        /// The base_addr parameter is the base address of the serialized buffer in memory.
        pub fn deserializeInto(self: *const Serialized, base_addr: usize) ExposedItems {
            return ExposedItems{
                .items = self.items.deserializeInto(base_addr),
            };
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
        items: []const SortedArrayBuilder(IdentIdx, ExposedItemTarget).Entry,
        index: usize,

        pub fn next(self: *Iterator) ?struct { ident_idx: IdentIdx, target: ExposedItemTarget } {
            if (self.index < self.items.len) {
                const entry = self.items[self.index];
                self.index += 1;
                return .{ .ident_idx = entry.key, .target = entry.value };
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

    // Test unresolved exposure requests before setting.
    try testing.expectEqual(@as(?u32, null), exposed.getValueNodeIndexById(allocator, foo_id));
    try testing.expectEqual(@as(?u32, null), exposed.getTypeNodeIndexById(allocator, foo_id));
    try testing.expectEqual(ExposedItemTarget.Kind.unresolved, exposed.getTargetById(allocator, foo_id).?.tag());

    // Set node indices
    try exposed.setValueNodeIndexById(allocator, foo_id, 42);
    try exposed.setTypeNodeIndexById(allocator, bar_id, 84);

    // Test typed lookups after setting
    try testing.expectEqual(@as(?u32, 42), exposed.getValueNodeIndexById(allocator, foo_id));
    try testing.expectEqual(@as(?u32, 84), exposed.getTypeNodeIndexById(allocator, bar_id));
    try testing.expectEqual(@as(?u32, null), exposed.getTypeNodeIndexById(allocator, foo_id));
    try testing.expectEqual(@as(?u32, null), exposed.getValueNodeIndexById(allocator, bar_id));
    try testing.expectEqual(@as(?u32, null), exposed.getValueNodeIndexById(allocator, my_type_id)); // Not set
    try testing.expectEqual(@as(?u32, null), exposed.getValueNodeIndexById(allocator, 999)); // Not exposed
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

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_empty_exposed.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    const serialized = try original.serialize(allocator, &writer);
    try testing.expectEqual(original.items.entries.items.len, serialized.items.entries.capacity);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer allocator.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

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
    try original.setValueNodeIndexById(allocator, id1, 42);
    try original.setTypeNodeIndexById(allocator, id2, 84);
    // id3 left unresolved

    // Ensure sorted before serialization
    original.ensureSorted(allocator);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_basic_exposed.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    const serialized = try original.serialize(allocator, &writer);
    try testing.expectEqual(original.items.entries.items.len, serialized.items.entries.capacity);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ExposedItems.Serialized)), writer.total_bytes);
    defer allocator.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // The serialized ExposedItems.Serialized struct is at the beginning of the buffer
    // (appendAlloc is called first in serialize)
    const serialized_ptr = @as(*ExposedItems.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Verify the items are accessible
    try testing.expectEqual(@as(usize, 3), deserialized.count());
    try testing.expectEqual(@as(?u32, 42), deserialized.getValueNodeIndexById(allocator, id1));
    try testing.expectEqual(@as(?u32, 84), deserialized.getTypeNodeIndexById(allocator, id2));
    try testing.expectEqual(@as(?u32, null), deserialized.getValueNodeIndexById(allocator, id3));
    try testing.expectEqual(ExposedItemTarget.Kind.value_def, deserialized.getTargetById(allocator, id1).?.tag());
    try testing.expectEqual(ExposedItemTarget.Kind.type_decl, deserialized.getTargetById(allocator, id2).?.tag());
    try testing.expectEqual(ExposedItemTarget.Kind.unresolved, deserialized.getTargetById(allocator, id3).?.tag());
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
    try original.setValueNodeIndexById(allocator, id1, 42);
    try original.addExposedById(allocator, id2);
    try original.addExposedById(allocator, id1); // duplicate
    try original.setValueNodeIndexById(allocator, id2, 84);
    try original.addExposedById(allocator, id2); // duplicate

    // Ensure sorted before serialization (this will also deduplicate)
    original.ensureSorted(allocator);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_duplicates_exposed.dat", .{ .read = true });
    defer file.close(io);

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    const serialized = try original.serialize(allocator, &writer);
    try testing.expectEqual(original.items.entries.items.len, serialized.items.entries.capacity);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ExposedItems.Serialized)), writer.total_bytes);
    defer allocator.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // The serialized ExposedItems.Serialized struct is at the beginning of the buffer
    // (appendAlloc is called first in serialize)
    const serialized_ptr: *ExposedItems.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // After deduplication, should have only 2 items
    try testing.expectEqual(@as(usize, 2), deserialized.count());
    try testing.expectEqual(@as(?u32, 42), deserialized.getValueNodeIndexById(allocator, id1));
    try testing.expectEqual(@as(?u32, 84), deserialized.getValueNodeIndexById(allocator, id2));
}

test "ExposedItems comprehensive CompactWriter roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var original = ExposedItems.init();
    defer original.deinit(allocator);

    // Test with many items including edge cases
    const test_items = [_]struct { id: IdentIdx, node_idx: ?u32 }{
        .{ .id = 0, .node_idx = null }, // minimum ID, unresolved
        .{ .id = 1, .node_idx = 100 },
        .{ .id = 42, .node_idx = 200 },
        .{ .id = 100, .node_idx = 300 },
        .{ .id = 1000, .node_idx = 400 },
        .{ .id = 10000, .node_idx = 500 },
        .{ .id = 65535, .node_idx = 600 },
        .{ .id = 100000, .node_idx = null }, // exposed but not defined
        .{ .id = std.math.maxInt(u32) - 1, .node_idx = 999 }, // near max ID
    };

    // Add all items
    for (test_items) |item| {
        try original.addExposedById(allocator, item.id);
        if (item.node_idx) |node_idx| {
            try original.setValueNodeIndexById(allocator, item.id, node_idx);
        }
    }

    // Ensure sorted before serialization
    original.ensureSorted(allocator);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_comprehensive_exposed.dat", .{ .read = true });
    defer file.close(io);

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(allocator);

    const serialized = try original.serialize(allocator, &writer);
    try testing.expectEqual(original.items.entries.items.len, serialized.items.entries.capacity);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const serialized_align = @alignOf(ExposedItems);
    const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(serialized_align), writer.total_bytes);
    defer allocator.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast to Serialized type and deserialize
    const serialized_ptr: *ExposedItems.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Verify all items
    try testing.expectEqual(@as(usize, test_items.len), deserialized.count());
    for (test_items) |item| {
        const actual = deserialized.getValueNodeIndexById(allocator, item.id);
        try testing.expectEqual(item.node_idx, actual);
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

        const serialized = try exposed.serialize(allocator, &writer);
        try testing.expectEqual(exposed.items.entries.items.len, serialized.items.entries.capacity);

        const buffer = try allocator.alloc(u8, writer.total_bytes);
        defer allocator.free(buffer);
        const written = try writer.writeToBuffer(buffer);
        try testing.expectEqual(buffer.len, written.len);

        const serialized_ptr = @as(*ExposedItems.Serialized, @ptrCast(@alignCast(buffer.ptr)));
        const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

        try testing.expectEqual(@as(usize, 0), deserialized.count());
    }

    // Test single item with file I/O to ensure writeGather works correctly
    {
        var exposed = ExposedItems.init();
        defer exposed.deinit(allocator);

        try exposed.addExposedById(allocator, 100);
        try exposed.setValueNodeIndexById(allocator, 100, 42);
        exposed.ensureSorted(allocator);

        // Create a temp file
        const io = std.testing.io;
        var tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const file = try tmp_dir.dir.createFile(io, "test_single.dat", .{ .read = true });
        defer file.close(io);

        var writer = CompactWriter.init();
        defer writer.deinit(allocator);

        const serialized = try exposed.serialize(allocator, &writer);
        try testing.expectEqual(exposed.items.entries.items.len, serialized.items.entries.capacity);

        // Test writeGather
        try writer.writeGather(file, io);

        // Read back and verify
        const buffer = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ExposedItems.Serialized)), writer.total_bytes);
        defer allocator.free(buffer);

        _ = try file.readPositionalAll(io, buffer, 0);

        const serialized_ptr = @as(*ExposedItems.Serialized, @ptrCast(@alignCast(buffer.ptr)));
        const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

        try testing.expectEqual(@as(usize, 1), deserialized.count());
        try testing.expectEqual(@as(?u32, 42), deserialized.getValueNodeIndexById(allocator, 100));
    }
}
