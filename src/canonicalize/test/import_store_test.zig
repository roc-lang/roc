//! This file contains tests for the Import.Store module.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const CIR = @import("../CIR.zig");

const Import = CIR.Import;
const StringLiteral = base.StringLiteral;
const CompactWriter = collections.CompactWriter;

fn storeContainsModule(store: *const Import.Store, string_store: *const StringLiteral.Store, module_name: []const u8) bool {
    for (store.imports.items.items) |string_idx| {
        if (std.mem.eql(u8, string_store.get(string_idx), module_name)) {
            return true;
        }
    }
    return false;
}

test "Import.Store deduplicates module names" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var string_store = try StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store.deinit(gpa);

    var store = Import.Store.init();
    defer store.deinit(gpa);

    // Add the same module name multiple times - should deduplicate
    const idx1 = try store.getOrPut(gpa, &string_store, "test.Module");
    const idx2 = try store.getOrPut(gpa, &string_store, "test.Module");

    // Should get the same index back (deduplication)
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

    // Verify both module names are present
    try testing.expect(storeContainsModule(&store, &string_store, "test.Module"));
    try testing.expect(storeContainsModule(&store, &string_store, "other.Module"));
}

test "Import.Store empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var original = Import.Store.init();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    try writer.writeGather(gpa, file);

    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    const serialized_ptr = @as(*Import.Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = try serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.imports.len());
    try testing.expectEqual(@as(usize, 0), deserialized.map.count());
}

test "Import.Store basic CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var string_store = try StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store.deinit(gpa);

    var original = Import.Store.init();
    defer original.deinit(gpa);

    _ = try original.getOrPut(gpa, &string_store, "json.Json");
    _ = try original.getOrPut(gpa, &string_store, "core.List");
    _ = try original.getOrPut(gpa, &string_store, "my.Module");

    try testing.expectEqual(@as(usize, 3), original.imports.len());

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    try writer.writeGather(gpa, file);

    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    const serialized_ptr: *Import.Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized = try serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);
    defer deserialized.map.deinit(gpa);

    // Verify the correct number of imports
    try testing.expectEqual(@as(usize, 3), deserialized.imports.len());

    // Verify all expected module names are present by iterating
    try testing.expect(storeContainsModule(deserialized, &string_store, "json.Json"));
    try testing.expect(storeContainsModule(deserialized, &string_store, "core.List"));
    try testing.expect(storeContainsModule(deserialized, &string_store, "my.Module"));

    // Verify the map is repopulated correctly
    try testing.expectEqual(@as(usize, 3), deserialized.map.count());
}

test "Import.Store duplicate imports CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var string_store = try StringLiteral.Store.initCapacityBytes(gpa, 1024);
    defer string_store.deinit(gpa);

    var original = Import.Store.init();
    defer original.deinit(gpa);

    const idx1 = try original.getOrPut(gpa, &string_store, "test.Module");
    _ = try original.getOrPut(gpa, &string_store, "another.Module");
    const idx3 = try original.getOrPut(gpa, &string_store, "test.Module"); // duplicate

    // Verify deduplication worked
    try testing.expectEqual(idx1, idx3);
    try testing.expectEqual(@as(usize, 2), original.imports.len());

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_duplicate_import_store.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    try writer.writeGather(gpa, file);

    try file.seekTo(0);
    const buffer = try file.readToEndAlloc(gpa, 1024 * 1024);
    defer gpa.free(buffer);

    const serialized_ptr: *Import.Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized = try serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa);
    defer deserialized.map.deinit(gpa);

    // Verify correct number of imports (duplicates deduplicated)
    try testing.expectEqual(@as(usize, 2), deserialized.imports.len());

    // Verify expected module names are present
    try testing.expect(storeContainsModule(deserialized, &string_store, "test.Module"));
    try testing.expect(storeContainsModule(deserialized, &string_store, "another.Module"));

    // Verify the map was repopulated correctly
    try testing.expectEqual(@as(usize, 2), deserialized.map.count());
}
