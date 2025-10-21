//! This file contains tests for the Import.Store module.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const CIR = @import("../CIR.zig");

const Import = CIR.Import;
const StringLiteral = base.StringLiteral;
const CompactWriter = collections.CompactWriter;

test "Import.Store deduplicates module names" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create a string store for interning module names
    var string_store = try StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store.deinit(gpa);

    // Create import store
    var store = Import.Store.init();
    defer store.deinit(gpa);

    // Add the same module name multiple times - should deduplicate
    const idx1 = try store.getOrPut(gpa, &string_store, "test.Module");
    const idx2 = try store.getOrPut(gpa, &string_store, "test.Module");

    // Should get the same index
    try testing.expectEqual(idx1, idx2);
    try testing.expectEqual(@as(usize, 1), store.imports.len());

    // Add a different module - should create a new entry
    const idx3 = try store.getOrPut(gpa, &string_store, "other.Module");
    try testing.expect(idx3 != idx1);
    try testing.expectEqual(@as(usize, 2), store.imports.len());

    // Add the first module name again - should still return original index
    const idx4 = try store.getOrPut(gpa, &string_store, "test.Module");
    try testing.expectEqual(idx1, idx4);
    try testing.expectEqual(@as(usize, 2), store.imports.len());

    // Verify we can retrieve the module names through the string store
    const str_idx1 = store.imports.items.items[@intFromEnum(idx1)];
    const str_idx3 = store.imports.items.items[@intFromEnum(idx3)];
    try testing.expectEqualStrings("test.Module", string_store.get(str_idx1));
    try testing.expectEqualStrings("other.Module", string_store.get(str_idx3));
}

test "Import.Store empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create an empty Store
    var original = Import.Store.init();

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    // Cast to Serialized and deserialize
    const serialized_ptr = @as(*Import.Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = try serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.imports.len());
    try testing.expectEqual(@as(usize, 0), deserialized.map.count());
}

test "Import.Store basic CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create a mock module env with string store
    var string_store = try StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store.deinit(gpa);

    const MockEnv = struct { strings: *StringLiteral.Store };
    const mock_env = MockEnv{ .strings = &string_store };

    // Create original store and add some imports
    var original = Import.Store.init();
    defer original.deinit(gpa);

    const idx1 = try original.getOrPut(gpa, mock_env.strings, "json.Json");
    const idx2 = try original.getOrPut(gpa, mock_env.strings, "core.List");
    const idx3 = try original.getOrPut(gpa, mock_env.strings, "my.Module");

    // Verify indices
    try testing.expectEqual(@as(u32, 0), @intFromEnum(idx1));
    try testing.expectEqual(@as(u32, 1), @intFromEnum(idx2));
    try testing.expectEqual(@as(u32, 2), @intFromEnum(idx3));

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    // Cast to Serialized and deserialize
    const serialized_ptr: *Import.Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized = try serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);
    defer deserialized.map.deinit(gpa);

    // Verify the imports are accessible
    try testing.expectEqual(@as(usize, 3), deserialized.imports.len());

    // Verify the interned string IDs are stored correctly
    const str_idx1 = deserialized.imports.items.items[0];
    const str_idx2 = deserialized.imports.items.items[1];
    const str_idx3 = deserialized.imports.items.items[2];

    try testing.expectEqualStrings("json.Json", string_store.get(str_idx1));
    try testing.expectEqualStrings("core.List", string_store.get(str_idx2));
    try testing.expectEqualStrings("my.Module", string_store.get(str_idx3));

    // Verify the map is repopulated correctly
    try testing.expectEqual(@as(usize, 3), deserialized.map.count());
}

test "Import.Store duplicate imports CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create a mock module env with string store
    var string_store = try StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store.deinit(gpa);

    const MockEnv = struct { strings: *StringLiteral.Store };
    const mock_env = MockEnv{ .strings = &string_store };

    // Create store with duplicate imports
    var original = Import.Store.init();
    defer original.deinit(gpa);

    const idx1 = try original.getOrPut(gpa, mock_env.strings, "test.Module");
    const idx2 = try original.getOrPut(gpa, mock_env.strings, "another.Module");
    const idx3 = try original.getOrPut(gpa, mock_env.strings, "test.Module"); // duplicate

    // Verify deduplication worked
    try testing.expectEqual(idx1, idx3);
    try testing.expectEqual(@as(usize, 2), original.imports.len());

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_duplicate_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    // Cast to Serialized and deserialize
    const serialized_ptr: *Import.Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized = try serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);
    defer deserialized.map.deinit(gpa);

    // Verify correct number of imports
    try testing.expectEqual(@as(usize, 2), deserialized.imports.len());

    // Get the string IDs and verify the strings
    const str_idx1 = deserialized.imports.items.items[@intFromEnum(idx1)];
    const str_idx2 = deserialized.imports.items.items[@intFromEnum(idx2)];

    try testing.expectEqualStrings("test.Module", string_store.get(str_idx1));
    try testing.expectEqualStrings("another.Module", string_store.get(str_idx2));

    // Verify the map was repopulated correctly
    try testing.expectEqual(@as(usize, 2), deserialized.map.count());

    // Check that the map has correct entries for the string indices that were deserialized
    const str_idx_0 = deserialized.imports.items.items[0];
    const str_idx_1 = deserialized.imports.items.items[1];

    try testing.expect(deserialized.map.contains(str_idx_0));
    try testing.expect(deserialized.map.contains(str_idx_1));
    try testing.expectEqual(@as(Import.Idx, @enumFromInt(0)), deserialized.map.get(str_idx_0).?);
    try testing.expectEqual(@as(Import.Idx, @enumFromInt(1)), deserialized.map.get(str_idx_1).?);
}
