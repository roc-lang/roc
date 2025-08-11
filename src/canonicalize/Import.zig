//! Manages module imports and their metadata

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const CompactWriter = collections.CompactWriter;
const StringLiteral = base.StringLiteral;

const Import = @This();

/// Index into the Import Store
pub const Idx = enum(u16) { _ };

/// A store for interning imported module names
pub const Store = struct {
    /// Map from module name string to Import.Idx
    map: std.StringHashMapUnmanaged(Import.Idx) = .{},
    /// List of imports indexed by Import.Idx
    imports: std.ArrayListUnmanaged([]u8) = .{},
    /// Storage for module name strings
    strings: std.ArrayListUnmanaged(u8) = .{},

    pub fn init() Store {
        return .{};
    }

    pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
        self.map.deinit(allocator);
        for (self.imports.items) |import| {
            allocator.free(import);
        }
        self.imports.deinit(allocator);
        self.strings.deinit(allocator);
    }

    pub fn intern(self: *Store, allocator: std.mem.Allocator, module_name: []const u8) !Import.Idx {
        if (self.map.get(module_name)) |idx| {
            return idx;
        }

        const idx = @as(Import.Idx, @enumFromInt(self.imports.items.len));
        const owned_name = try allocator.dupe(u8, module_name);
        try self.imports.append(allocator, owned_name);
        try self.map.put(allocator, owned_name, idx);
        return idx;
    }

    pub fn get(self: *const Store, idx: Import.Idx) []const u8 {
        return self.imports.items[@intFromEnum(idx)];
    }
};

test "Import.Store uses interned string deduplication" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create two separate string stores that will intern the same string differently
    var string_store1 = try base.StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store1.deinit(gpa);

    var string_store2 = try base.StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store2.deinit(gpa);

    // Intern some strings in the first store
    _ = try string_store1.insert(gpa, "foo");
    _ = try string_store1.insert(gpa, "bar");

    // Create import store
    var store = Import.Store.init();
    defer store.deinit(gpa);

    // Add the same module name multiple times - should deduplicate based on interned string
    const idx1 = try store.getOrPut(gpa, &string_store1, "test.Module");
    const idx2 = try store.getOrPut(gpa, &string_store1, "test.Module");

    // Should get the same index
    try testing.expectEqual(idx1, idx2);
    try testing.expectEqual(@as(usize, 1), store.imports.len());

    // Now if we use a different string store, we might get a different interned string index
    // but the Import.Store should still deduplicate based on the string content
    const idx3 = try store.getOrPut(gpa, &string_store2, "test.Module");

    // Should still get the same import index because the string content is the same
    try testing.expectEqual(idx1, idx3);
    try testing.expectEqual(@as(usize, 1), store.imports.len());

    // But if we add a different module, it should create a new entry
    const idx4 = try store.getOrPut(gpa, &string_store1, "other.Module");
    try testing.expect(idx4 != idx1);
    try testing.expectEqual(@as(usize, 2), store.imports.len());
}

test "Import.Store.Serialized roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create original store and add some imports
    var original = Import.Store.init();
    defer original.deinit(gpa);

    // Create a string store for interning module names
    var string_store = try StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store.deinit(gpa);

    // Add some imports
    const idx1 = try original.getOrPut(gpa, &string_store, "Std.List");
    const idx2 = try original.getOrPut(gpa, &string_store, "Std.Dict");
    const idx3 = try original.getOrPut(gpa, &string_store, "App.Model");

    // Create a CompactWriter and arena
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test_import_store_serialized.dat", .{ .read = true });
    defer tmp_file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    // Allocate and serialize using the Serialized struct
    const serialized_ptr = try writer.appendAlloc(arena_alloc, Import.Store.Serialized);
    try serialized_ptr.serialize(&original, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(arena_alloc, tmp_file);

    // Read back
    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alloc(u8, file_size);
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

    // Deserialize
    const deserialized_ptr = @as(*Import.Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const store = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the imports are accessible
    try testing.expectEqual(@as(usize, 3), store.imports.len());

    // The map should be empty after deserialization
    try testing.expectEqual(@as(usize, 0), store.map.count());

    // Verify the import indices match
    try testing.expectEqual(string_store.get(original.imports.getAssume(@intFromEnum(idx1))), string_store.get(store.imports.getAssume(@intFromEnum(idx1))));
    try testing.expectEqual(string_store.get(original.imports.getAssume(@intFromEnum(idx2))), string_store.get(store.imports.getAssume(@intFromEnum(idx2))));
    try testing.expectEqual(string_store.get(original.imports.getAssume(@intFromEnum(idx3))), string_store.get(store.imports.getAssume(@intFromEnum(idx3))));
}
