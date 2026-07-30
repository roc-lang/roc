//! This file contains tests for the Import.Store module.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const CIR = @import("../CIR.zig");

const Import = CIR.Import;
const CommonEnv = base.CommonEnv;
const CompactWriter = collections.CompactWriter;

fn storeContainsModule(store: *const Import.Store, common: *const CommonEnv, module_name: []const u8) bool {
    for (store.imports.items.items) |string_idx| {
        if (std.mem.eql(u8, common.getString(string_idx), module_name)) {
            return true;
        }
    }
    return false;
}

test "Import.Store deduplicates module names" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var common = try CommonEnv.init(gpa, "");
    defer common.deinit(gpa);

    var store = Import.Store.init();
    defer store.deinit(gpa);

    // Add the same module name multiple times - should deduplicate
    const idx1 = try store.getOrPut(gpa, &common, "test.Module");
    const idx2 = try store.getOrPut(gpa, &common, "test.Module");

    // Should get the same index back (deduplication)
    try testing.expectEqual(idx1, idx2);
    try testing.expectEqual(@as(usize, 1), store.imports.len());

    // Add a different module - should create a new entry
    const idx3 = try store.getOrPut(gpa, &common, "other.Module");
    try testing.expect(idx3 != idx1);
    try testing.expectEqual(@as(usize, 2), store.imports.len());

    // Add the first module name again - should still return original index
    const idx4 = try store.getOrPut(gpa, &common, "test.Module");
    try testing.expectEqual(idx1, idx4);
    try testing.expectEqual(@as(usize, 2), store.imports.len());

    // Verify both module names are present
    try testing.expect(storeContainsModule(&store, &common, "test.Module"));
    try testing.expect(storeContainsModule(&store, &common, "other.Module"));
}

test "Import.Store empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var original = Import.Store.init();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(std.testing.io, "test_empty_import_store.dat", .{ .read = true });
    defer file.close(std.testing.io);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    try writer.writeGather(file, std.testing.io);

    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(writer.total_bytes));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(std.testing.io, buffer, 0);

    const serialized_ptr = @as(*Import.Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = try serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa);

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.imports.len());
    try testing.expectEqual(@as(usize, 0), deserialized.map.count());
}

test "Import.Store basic CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var common = try CommonEnv.init(gpa, "");
    defer common.deinit(gpa);

    var original = Import.Store.init();
    defer original.deinit(gpa);

    _ = try original.getOrPut(gpa, &common, "json.Json");
    _ = try original.getOrPut(gpa, &common, "core.List");
    _ = try original.getOrPut(gpa, &common, "my.Module");

    try testing.expectEqual(@as(usize, 3), original.imports.len());

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(std.testing.io, "test_basic_import_store.dat", .{ .read = true });
    defer file.close(std.testing.io);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    try writer.writeGather(file, std.testing.io);

    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(writer.total_bytes));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(std.testing.io, buffer, 0);

    const serialized_ptr: *Import.Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized = try serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa);
    defer deserialized.map.deinit(gpa);

    // Verify the correct number of imports
    try testing.expectEqual(@as(usize, 3), deserialized.imports.len());

    // Verify all expected module names are present by iterating
    try testing.expect(storeContainsModule(&deserialized, &common, "json.Json"));
    try testing.expect(storeContainsModule(&deserialized, &common, "core.List"));
    try testing.expect(storeContainsModule(&deserialized, &common, "my.Module"));

    // Verify the map is repopulated correctly
    try testing.expectEqual(@as(usize, 3), deserialized.map.count());
}

test "Import.Store duplicate imports CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var common = try CommonEnv.init(gpa, "");
    defer common.deinit(gpa);

    var original = Import.Store.init();
    defer original.deinit(gpa);

    const idx1 = try original.getOrPut(gpa, &common, "test.Module");
    _ = try original.getOrPut(gpa, &common, "another.Module");
    const idx3 = try original.getOrPut(gpa, &common, "test.Module"); // duplicate

    // Verify deduplication worked
    try testing.expectEqual(idx1, idx3);
    try testing.expectEqual(@as(usize, 2), original.imports.len());

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(std.testing.io, "test_duplicate_import_store.dat", .{ .read = true });
    defer file.close(std.testing.io);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, Import.Store.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    try writer.writeGather(file, std.testing.io);

    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(writer.total_bytes));
    defer gpa.free(buffer);
    _ = try file.readPositionalAll(std.testing.io, buffer, 0);

    const serialized_ptr: *Import.Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized = try serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa);
    defer deserialized.map.deinit(gpa);

    // Verify correct number of imports (duplicates deduplicated)
    try testing.expectEqual(@as(usize, 2), deserialized.imports.len());

    // Verify expected module names are present
    try testing.expect(storeContainsModule(&deserialized, &common, "test.Module"));
    try testing.expect(storeContainsModule(&deserialized, &common, "another.Module"));

    // Verify the map was repopulated correctly
    try testing.expectEqual(@as(usize, 2), deserialized.map.count());
}
