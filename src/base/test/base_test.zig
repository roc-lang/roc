const std = @import("std");
const base = @import("base");
const serialization = @import("serialization");
const collections = @import("collections");

const Ident = base.Ident;
const CompactWriter = serialization.CompactWriter;
const DataSpan = base.DataSpan;
const PackedDataSpan = base.PackedDataSpan;
const FunctionArgs = base.FunctionArgs;
const SmallCollections = base.SmallCollections;
const SafeList = collections.SafeList;
const RegionInfo = base.RegionInfo;
const SmallStringInterner = base.SmallStringInterner;
const StringLiteral = base.StringLiteral;

test "from_bytes validates empty text" {
    const result = Ident.from_bytes("");
    try std.testing.expectError(Ident.Error.EmptyText, result);
}

test "from_bytes validates null bytes" {
    const text_with_null = "hello\x00world";
    const result = Ident.from_bytes(text_with_null);
    try std.testing.expectError(Ident.Error.ContainsNullByte, result);
}

test "from_bytes validates control characters" {
    const text_with_control = "hello\x01world";
    const result = Ident.from_bytes(text_with_control);
    try std.testing.expectError(Ident.Error.ContainsControlCharacters, result);
}

test "from_bytes disallows common whitespace" {
    const text_with_space = "hello world";
    const result = Ident.from_bytes(text_with_space);
    try std.testing.expect(result == Ident.Error.ContainsControlCharacters);

    const text_with_tab = "hello\tworld";
    const result2 = Ident.from_bytes(text_with_tab);
    try std.testing.expect(result2 == Ident.Error.ContainsControlCharacters);

    const text_with_newline = "hello\nworld";
    const result3 = Ident.from_bytes(text_with_newline);
    try std.testing.expect(result3 == Ident.Error.ContainsControlCharacters);

    const text_with_cr = "hello\rworld";
    const result4 = Ident.from_bytes(text_with_cr);
    try std.testing.expect(result4 == Ident.Error.ContainsControlCharacters);
}

test "from_bytes creates valid identifier" {
    const result = try Ident.from_bytes("valid_name!");
    try std.testing.expectEqualStrings("valid_name!", result.raw_text);
    try std.testing.expect(result.attributes.effectful == true);
    try std.testing.expect(result.attributes.ignored == false);
    try std.testing.expect(result.attributes.reassignable == false);
}

test "from_bytes creates ignored identifier" {
    const result = try Ident.from_bytes("_ignored");
    try std.testing.expectEqualStrings("_ignored", result.raw_text);
    try std.testing.expect(result.attributes.effectful == false);
    try std.testing.expect(result.attributes.ignored == true);
    try std.testing.expect(result.attributes.reassignable == false);
}

test "Ident.Store empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an empty Store
    var original = try Ident.Store.initCapacity(gpa, 0);
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Ident.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try std.testing.expectEqual(@as(usize, 0), deserialized.interner.bytes.len());
    try std.testing.expectEqual(@as(usize, 0), deserialized.interner.entry_count);
    try std.testing.expectEqual(@as(usize, 0), deserialized.attributes.len());
    try std.testing.expectEqual(@as(u32, 0), deserialized.next_unique_name);
}

test "Ident.Store basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create original store and add some identifiers
    var original = try Ident.Store.initCapacity(gpa, 16);
    defer original.deinit(gpa);

    const ident1 = Ident.for_text("hello");
    const ident2 = Ident.for_text("world!");
    const ident3 = Ident.for_text("_ignored");

    const idx1 = try original.insert(gpa, ident1);
    const idx2 = try original.insert(gpa, ident2);
    const idx3 = try original.insert(gpa, ident3);

    // Verify the attributes in the indices
    try std.testing.expect(!idx1.attributes.effectful);
    try std.testing.expect(!idx1.attributes.ignored);
    try std.testing.expect(idx2.attributes.effectful);
    try std.testing.expect(!idx2.attributes.ignored);
    try std.testing.expect(!idx3.attributes.effectful);
    try std.testing.expect(idx3.attributes.ignored);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Ident.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the identifiers are accessible
    try std.testing.expectEqualStrings("hello", deserialized.getText(idx1));
    try std.testing.expectEqualStrings("world!", deserialized.getText(idx2));
    try std.testing.expectEqualStrings("_ignored", deserialized.getText(idx3));

    // Verify next_unique_name is preserved
    try std.testing.expectEqual(original.next_unique_name, deserialized.next_unique_name);

    // Verify the interner's hash map is empty after deserialization
    try std.testing.expectEqual(@as(usize, 0), deserialized.interner.entry_count);
}

test "Ident.Store with genUnique CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create store and generate unique identifiers
    var original = try Ident.Store.initCapacity(gpa, 10);
    defer original.deinit(gpa);

    // Add some regular identifiers
    const ident1 = Ident.for_text("regular");
    const idx1 = try original.insert(gpa, ident1);

    // Generate unique identifiers
    const unique1 = try original.genUnique(gpa);
    const unique2 = try original.genUnique(gpa);
    const unique3 = try original.genUnique(gpa);

    // Verify unique names are correct
    try std.testing.expectEqualStrings("0", original.getText(unique1));
    try std.testing.expectEqualStrings("1", original.getText(unique2));
    try std.testing.expectEqualStrings("2", original.getText(unique3));

    // Verify next_unique_name was incremented
    try std.testing.expectEqual(@as(u32, 3), original.next_unique_name);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_unique_store.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Ident.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all identifiers
    try std.testing.expectEqualStrings("regular", deserialized.getText(idx1));
    try std.testing.expectEqualStrings("0", deserialized.getText(unique1));
    try std.testing.expectEqualStrings("1", deserialized.getText(unique2));
    try std.testing.expectEqualStrings("2", deserialized.getText(unique3));

    // Verify next_unique_name is preserved
    try std.testing.expectEqual(@as(u32, 3), deserialized.next_unique_name);
}

test "Ident.Store frozen state CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate store
    var original = try Ident.Store.initCapacity(gpa, 5);
    defer original.deinit(gpa);

    _ = try original.insert(gpa, Ident.for_text("test1"));
    _ = try original.insert(gpa, Ident.for_text("test2"));

    // Freeze the store
    original.freeze();

    // Verify interner is frozen
    if (std.debug.runtime_safety) {
        try std.testing.expect(original.interner.frozen);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_store.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Ident.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify frozen state is preserved
    if (std.debug.runtime_safety) {
        try std.testing.expect(deserialized.interner.frozen);
    }
}

test "Ident.Store comprehensive CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create store with various identifiers
    var original = try Ident.Store.initCapacity(gpa, 20);
    defer original.deinit(gpa);

    // Test various identifier types and edge cases
    const test_idents = [_]struct { text: []const u8, expected_idx: u32 }{
        .{ .text = "hello", .expected_idx = 0 },
        .{ .text = "world!", .expected_idx = 6 },
        .{ .text = "_ignored", .expected_idx = 13 },
        .{ .text = "a", .expected_idx = 22 }, // single character
        .{ .text = "very_long_identifier_name_that_might_cause_issues", .expected_idx = 24 },
        .{ .text = "effectful!", .expected_idx = 75 },
        .{ .text = "_", .expected_idx = 86 }, // Just underscore
        .{ .text = "CamelCase", .expected_idx = 88 },
        .{ .text = "snake_case", .expected_idx = 98 },
        .{ .text = "SCREAMING_CASE", .expected_idx = 109 },
        .{ .text = "hello", .expected_idx = 0 }, // duplicate, should reuse
    };

    var indices = std.ArrayList(Ident.Idx).init(gpa);
    defer indices.deinit();

    for (test_idents) |test_ident| {
        const ident = Ident.for_text(test_ident.text);
        const idx = try original.insert(gpa, ident);
        try indices.append(idx);
        try std.testing.expectEqual(test_ident.expected_idx, idx.idx);
    }

    // Add some unique names
    const unique1 = try original.genUnique(gpa);
    const unique2 = try original.genUnique(gpa);

    // Verify the interner's hash map is populated
    try std.testing.expect(original.interner.entry_count > 0);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_comprehensive_store.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Ident.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all identifiers (skip duplicate at end)
    for (test_idents[0..10], 0..) |test_ident, i| {
        const idx = indices.items[i];
        const text = deserialized.getText(idx);
        try std.testing.expectEqualStrings(test_ident.text, text);
    }

    // Verify unique names
    try std.testing.expectEqualStrings("0", deserialized.getText(unique1));
    try std.testing.expectEqualStrings("1", deserialized.getText(unique2));

    // Verify the interner's hash map is empty after deserialization
    try std.testing.expectEqual(@as(usize, 0), deserialized.interner.entry_count);

    // Verify next_unique_name
    try std.testing.expectEqual(@as(u32, 2), deserialized.next_unique_name);
}

test "Ident.Store multiple stores CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create multiple stores to test alignment
    var store1 = try Ident.Store.initCapacity(gpa, 5);
    defer store1.deinit(gpa);

    var store2 = try Ident.Store.initCapacity(gpa, 5);
    defer store2.deinit(gpa);

    var store3 = try Ident.Store.initCapacity(gpa, 5);
    defer store3.deinit(gpa);

    // Populate stores differently
    const idx1_1 = try store1.insert(gpa, Ident.for_text("store1_ident"));
    _ = try store1.genUnique(gpa);

    const idx2_1 = try store2.insert(gpa, Ident.for_text("store2_ident!"));
    const idx2_2 = try store2.insert(gpa, Ident.for_text("_store2_ignored"));
    store2.freeze();

    const idx3_1 = try store3.insert(gpa, Ident.for_text("store3"));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_multiple_stores.dat", .{ .read = true });
    defer file.close();

    // Serialize all three
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try store1.serialize(gpa, &writer);
    const offset1 = writer.total_bytes - @sizeOf(Ident.Store);

    _ = try store2.serialize(gpa, &writer);
    const offset2 = writer.total_bytes - @sizeOf(Ident.Store);

    _ = try store3.serialize(gpa, &writer);
    const offset3 = writer.total_bytes - @sizeOf(Ident.Store);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate all three
    const deserialized1 = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr + offset1)));
    deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized2 = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr + offset2)));
    deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized3 = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr + offset3)));
    deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify store 1
    try std.testing.expectEqualStrings("store1_ident", deserialized1.getText(idx1_1));
    try std.testing.expectEqual(@as(u32, 1), deserialized1.next_unique_name);

    // Verify store 2 (frozen)
    try std.testing.expectEqualStrings("store2_ident!", deserialized2.getText(idx2_1));
    try std.testing.expectEqualStrings("_store2_ignored", deserialized2.getText(idx2_2));
    if (std.debug.runtime_safety) {
        try std.testing.expect(deserialized2.interner.frozen);
    }

    // Verify store 3
    try std.testing.expectEqualStrings("store3", deserialized3.getText(idx3_1));

    // Verify all have empty hash maps
    try std.testing.expectEqual(@as(usize, 0), deserialized1.interner.entry_count);
    try std.testing.expectEqual(@as(usize, 0), deserialized2.interner.entry_count);
    try std.testing.expectEqual(@as(usize, 0), deserialized3.interner.entry_count);
}

test "PackedDataSpan basic functionality" {
    const Packed = PackedDataSpan(16, 16);

    // Test creation and conversion
    const original = DataSpan{ .start = 1000, .len = 50 };
    const packed_span = try Packed.fromDataSpan(original);
    const restored = packed_span.toDataSpan();

    try std.testing.expectEqual(original.start, restored.start);
    try std.testing.expectEqual(original.len, restored.len);
}

test "PackedDataSpan limits" {
    const Packed = PackedDataSpan(16, 16);

    // Test max values work
    try std.testing.expectEqual(@as(u32, 65535), Packed.MAX_START);
    try std.testing.expectEqual(@as(u32, 65535), Packed.MAX_LENGTH);

    const max_span = DataSpan{ .start = 65535, .len = 65535 };
    const packed_span = try Packed.fromDataSpan(max_span);
    const restored = packed_span.toDataSpan();

    try std.testing.expectEqual(max_span.start, restored.start);
    try std.testing.expectEqual(max_span.len, restored.len);
}

test "PackedDataSpan overflow detection" {
    const Packed = PackedDataSpan(16, 16);

    // Test start overflow
    const start_overflow = DataSpan{ .start = 65536, .len = 0 };
    try std.testing.expectError(error.StartTooLarge, Packed.fromDataSpan(start_overflow));

    // Test length overflow
    const len_overflow = DataSpan{ .start = 0, .len = 65536 };
    try std.testing.expectError(error.LengthTooLarge, Packed.fromDataSpan(len_overflow));
}

test "PackedDataSpan canFit" {
    const Packed = PackedDataSpan(16, 16);

    try std.testing.expect(Packed.canFit(DataSpan{ .start = 1000, .len = 50 }));
    try std.testing.expect(Packed.canFit(DataSpan{ .start = 65535, .len = 65535 }));
    try std.testing.expect(!Packed.canFit(DataSpan{ .start = 65536, .len = 0 }));
    try std.testing.expect(!Packed.canFit(DataSpan{ .start = 0, .len = 65536 }));
}

test "PackedDataSpan u32 conversion" {
    const Packed = PackedDataSpan(16, 16);

    const original = DataSpan{ .start = 1000, .len = 50 };
    const packed_span = try Packed.fromDataSpan(original);
    const raw = packed_span.toU32();
    const restored_packed = Packed.fromU32(raw);
    const restored = restored_packed.toDataSpan();

    try std.testing.expectEqual(original.start, restored.start);
    try std.testing.expectEqual(original.len, restored.len);
}

test "PackedDataSpan different configurations" {
    // Test FunctionArgs configuration (20, 12)
    const func_span = DataSpan{ .start = 1000000, .len = 100 };
    try std.testing.expect(FunctionArgs.canFit(func_span));

    const packed_func = try FunctionArgs.fromDataSpan(func_span);
    const restored_func = packed_func.toDataSpan();
    try std.testing.expectEqual(func_span.start, restored_func.start);
    try std.testing.expectEqual(func_span.len, restored_func.len);

    // Test SmallCollections configuration (24, 8)
    const small_span = DataSpan{ .start = 10000000, .len = 50 };
    try std.testing.expect(SmallCollections.canFit(small_span));

    const packed_small = try SmallCollections.fromDataSpan(small_span);
    const restored_small = packed_small.toDataSpan();
    try std.testing.expectEqual(small_span.start, restored_small.start);
    try std.testing.expectEqual(small_span.len, restored_small.len);
}

test "PackedDataSpan compile-time validation" {
    // These should compile fine
    _ = PackedDataSpan(16, 16);
    _ = PackedDataSpan(20, 12);
    _ = PackedDataSpan(24, 8);
    _ = PackedDataSpan(1, 31);
    _ = PackedDataSpan(31, 1);

    // These would cause compile errors if uncommented:
    // _ = PackedDataSpan(16, 15); // doesn't sum to 32
    // _ = PackedDataSpan(0, 32);  // zero bits not allowed
    // _ = PackedDataSpan(33, 0);  // doesn't fit in u32
}

test "process basic functionality" {
    const gpa = std.testing.allocator;

    const MyContext = struct {
        items: []const i32,
        outputs: []i32,
    };

    const TestWorker = struct {
        fn worker(worker_allocator: std.mem.Allocator, item: *MyContext, item_id: usize) void {
            _ = worker_allocator; // unused in this test
            const value = item.items[item_id];
            if (value < 0) {
                item.outputs[item_id] = -1;
            } else {
                item.outputs[item_id] = value * value;
            }
        }
    };

    var outputs: [5]i32 = undefined; // Preallocate output array

    var context = MyContext{
        .items = &[_]i32{ 1, 2, -3, 4, 5 },
        .outputs = &outputs,
    };

    try base.parallel.process(
        MyContext,
        &context,
        TestWorker.worker,
        gpa,
        outputs.len,
        .{ .max_threads = 1, .use_per_thread_arenas = false },
    );
    try std.testing.expectEqual(
        outputs,
        [_]i32{ 1, 4, -1, 16, 25 },
    );
}

test "lineIdx" {
    const gpa = std.testing.allocator;
    var line_starts = try SafeList(u32).initCapacity(gpa, 256);
    defer line_starts.deinit(gpa);

    // Simple test case with lines at positions 0, 10, 20
    _ = try line_starts.append(gpa, 0);
    _ = try line_starts.append(gpa, 10);
    _ = try line_starts.append(gpa, 20);
    _ = try line_starts.append(gpa, 30);

    try std.testing.expectEqual(0, RegionInfo.lineIdx(line_starts.items.items, 0));
    try std.testing.expectEqual(0, RegionInfo.lineIdx(line_starts.items.items, 5));
    try std.testing.expectEqual(0, RegionInfo.lineIdx(line_starts.items.items, 9));
    try std.testing.expectEqual(1, RegionInfo.lineIdx(line_starts.items.items, 10));
    try std.testing.expectEqual(1, RegionInfo.lineIdx(line_starts.items.items, 15));
    try std.testing.expectEqual(1, RegionInfo.lineIdx(line_starts.items.items, 19));
    try std.testing.expectEqual(2, RegionInfo.lineIdx(line_starts.items.items, 20));
    try std.testing.expectEqual(2, RegionInfo.lineIdx(line_starts.items.items, 25));
    try std.testing.expectEqual(2, RegionInfo.lineIdx(line_starts.items.items, 29));
    try std.testing.expectEqual(3, RegionInfo.lineIdx(line_starts.items.items, 30));
    try std.testing.expectEqual(3, RegionInfo.lineIdx(line_starts.items.items, 35));
}

test "columnIdx" {
    const gpa = std.testing.allocator;
    var line_starts = try SafeList(u32).initCapacity(gpa, 256);
    defer line_starts.deinit(gpa);

    _ = try line_starts.append(gpa, 0);
    _ = try line_starts.append(gpa, 10);
    _ = try line_starts.append(gpa, 20);

    try std.testing.expectEqual(0, RegionInfo.columnIdx(line_starts.items.items, 0, 0));
    try std.testing.expectEqual(5, RegionInfo.columnIdx(line_starts.items.items, 0, 5));
    try std.testing.expectEqual(9, RegionInfo.columnIdx(line_starts.items.items, 0, 9));

    try std.testing.expectEqual(0, RegionInfo.columnIdx(line_starts.items.items, 1, 10));
    try std.testing.expectEqual(5, RegionInfo.columnIdx(line_starts.items.items, 1, 15));
}

test "getLineText" {
    const gpa = std.testing.allocator;
    var line_starts = try SafeList(u32).initCapacity(gpa, 256);
    defer line_starts.deinit(gpa);

    const source = "line0\nline1\nline2";

    _ = try line_starts.append(gpa, 0);
    _ = try line_starts.append(gpa, 6);
    _ = try line_starts.append(gpa, 12);

    try std.testing.expectEqualStrings("line0", RegionInfo.getLineText(source, line_starts.items.items, 0, 0));
    try std.testing.expectEqualStrings("line1", RegionInfo.getLineText(source, line_starts.items.items, 1, 1));
    try std.testing.expectEqualStrings("line0\nline1", RegionInfo.getLineText(source, line_starts.items.items, 0, 1));
    try std.testing.expectEqualStrings("line2", RegionInfo.getLineText(source, line_starts.items.items, 2, 2));
}

test "get" {
    const gpa = std.testing.allocator;
    var line_starts = try SafeList(u32).initCapacity(gpa, 256);
    defer line_starts.deinit(gpa);

    const source = "line0\nline1\nline2";

    _ = try line_starts.append(gpa, 0);
    _ = try line_starts.append(gpa, 6);
    _ = try line_starts.append(gpa, 12);

    const info1 = try RegionInfo.position(source, line_starts.items.items, 2, 4);
    try std.testing.expectEqual(0, info1.start_line_idx);
    try std.testing.expectEqual(2, info1.start_col_idx);
    try std.testing.expectEqual(0, info1.end_line_idx);
    try std.testing.expectEqual(4, info1.end_col_idx);
    try std.testing.expectEqualStrings("line0", info1.calculateLineText(source, line_starts.items.items));

    const info2 = try RegionInfo.position(source, line_starts.items.items, 8, 10);
    try std.testing.expectEqual(1, info2.start_line_idx);
    try std.testing.expectEqual(2, info2.start_col_idx);
    try std.testing.expectEqual(1, info2.end_line_idx);
    try std.testing.expectEqual(4, info2.end_col_idx);
    try std.testing.expectEqualStrings("line1", info2.calculateLineText(source, line_starts.items.items));
}

test "SmallStringInterner empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an empty SmallStringInterner
    var original = SmallStringInterner{};
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate - empty interner should still work
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SmallStringInterner))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty - bytes starts with one zero byte, hash_table should be empty
    try std.testing.expectEqual(@as(usize, 1), deserialized.bytes.len());
    try std.testing.expectEqual(@as(u32, 0), deserialized.entry_count);
}

test "SmallStringInterner basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an interner with some strings
    var original = try SmallStringInterner.initCapacity(gpa, 10);
    defer original.deinit(gpa);

    // Insert test strings
    const test_strings = [_][]const u8{
        "hello",
        "world",
        "foo",
        "bar",
        "baz",
        "test string",
        "another test",
        "", // empty string
        "duplicate",
        "duplicate", // Should reuse the same index
    };

    var indices = std.ArrayList(SmallStringInterner.Idx).init(gpa);
    defer indices.deinit();

    for (test_strings) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(idx);
    }

    // Verify duplicate detection worked
    try std.testing.expectEqual(indices.items[8], indices.items[9]);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SmallStringInterner))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings are accessible and correct
    for (test_strings[0..9], 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getText(idx);
        try std.testing.expectEqualStrings(expected_str, actual_str);
    }

    // Verify the entry count is preserved after deserialization
    try std.testing.expectEqual(@as(u32, 9), deserialized.entry_count);
}

test "SmallStringInterner with populated hashmap CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create interner and populate it
    var original = try SmallStringInterner.initCapacity(gpa, 20);
    defer original.deinit(gpa);

    // Insert many strings to ensure the hash map is well populated
    const test_data = [_]struct { str: []const u8, expected_idx: u32 }{
        .{ .str = "first", .expected_idx = 0 },
        .{ .str = "second", .expected_idx = 6 },
        .{ .str = "third", .expected_idx = 13 },
        .{ .str = "first", .expected_idx = 0 }, // duplicate
        .{ .str = "fourth", .expected_idx = 19 },
        .{ .str = "fifth", .expected_idx = 26 },
        .{ .str = "second", .expected_idx = 6 }, // duplicate
        .{ .str = "sixth", .expected_idx = 32 },
        .{ .str = "seventh", .expected_idx = 38 },
        .{ .str = "eighth", .expected_idx = 46 },
    };

    for (test_data) |data| {
        const idx = try original.insert(gpa, data.str);
        try std.testing.expectEqual(@as(u32, data.expected_idx), @intFromEnum(idx));
    }

    // Verify the hash table is populated
    try std.testing.expect(original.entry_count > 0);
    const original_entry_count = original.entry_count;

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_hashmap_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SmallStringInterner))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the entry count is preserved after deserialization
    try std.testing.expectEqual(original_entry_count, deserialized.entry_count);

    // But all strings should still be accessible
    try std.testing.expectEqualStrings("first", deserialized.getText(@enumFromInt(0)));
    try std.testing.expectEqualStrings("second", deserialized.getText(@enumFromInt(6)));
    try std.testing.expectEqualStrings("third", deserialized.getText(@enumFromInt(13)));
    try std.testing.expectEqualStrings("fourth", deserialized.getText(@enumFromInt(19)));
    try std.testing.expectEqualStrings("fifth", deserialized.getText(@enumFromInt(26)));
    try std.testing.expectEqualStrings("sixth", deserialized.getText(@enumFromInt(32)));
    try std.testing.expectEqualStrings("seventh", deserialized.getText(@enumFromInt(38)));
    try std.testing.expectEqualStrings("eighth", deserialized.getText(@enumFromInt(46)));

    // Log to confirm the original had entries
    if (original_entry_count == 0) {
        std.debug.panic("Test failed: original should have had entries\n", .{});
    }
}

test "SmallStringInterner frozen state CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate interner
    var original = try SmallStringInterner.initCapacity(gpa, 5);
    defer original.deinit(gpa);

    _ = try original.insert(gpa, "test1");
    _ = try original.insert(gpa, "test2");

    // Freeze the interner
    original.freeze();

    // Verify it's frozen
    if (std.debug.runtime_safety) {
        try std.testing.expect(original.frozen);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SmallStringInterner))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify frozen state is preserved
    if (std.debug.runtime_safety) {
        try std.testing.expect(deserialized.frozen);
    }

    // Verify strings are still accessible
    try std.testing.expectEqualStrings("test1", deserialized.getText(@enumFromInt(0)));
    try std.testing.expectEqualStrings("test2", deserialized.getText(@enumFromInt(6)));
}

test "SmallStringInterner edge cases CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Test with strings of various lengths and special characters
    var original = try SmallStringInterner.initCapacity(gpa, 15);
    defer original.deinit(gpa);

    const edge_cases = [_][]const u8{
        "", // empty string
        "a", // single char
        "ab", // two chars
        "hello world with spaces",
        "special\ncharacters\ttabs",
        "unicode: 你好世界", // UTF-8
        "very_long_string_that_is_much_longer_than_average_to_test_capacity_handling",
        "\x00embedded", // string starting with null (though this might not work)
        "end_with_space ",
        " start_with_space",
    };

    var indices = std.ArrayList(SmallStringInterner.Idx).init(gpa);
    defer indices.deinit();

    for (edge_cases) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(idx);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_edge_interner.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SmallStringInterner))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all edge cases
    for (edge_cases, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getText(idx);
        try std.testing.expectEqualStrings(expected_str, actual_str);
    }
}

test "SmallStringInterner multiple interners CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create multiple interners to test alignment and offset handling
    var interner1 = try SmallStringInterner.initCapacity(gpa, 5);
    defer interner1.deinit(gpa);

    var interner2 = try SmallStringInterner.initCapacity(gpa, 5);
    defer interner2.deinit(gpa);

    var interner3 = try SmallStringInterner.initCapacity(gpa, 5);
    defer interner3.deinit(gpa);

    // Populate with different strings
    const idx1_1 = try interner1.insert(gpa, "interner1_string1");
    const idx1_2 = try interner1.insert(gpa, "interner1_string2");

    const idx2_1 = try interner2.insert(gpa, "interner2_string1");
    const idx2_2 = try interner2.insert(gpa, "interner2_string2");
    const idx2_3 = try interner2.insert(gpa, "interner2_string3");

    const idx3_1 = try interner3.insert(gpa, "interner3_string1");

    // Freeze the second one
    interner2.freeze();

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_multiple_interners.dat", .{ .read = true });
    defer file.close();

    // Serialize all three
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try interner1.serialize(gpa, &writer);
    const offset1 = writer.total_bytes - @sizeOf(SmallStringInterner);

    _ = try interner2.serialize(gpa, &writer);
    const offset2 = writer.total_bytes - @sizeOf(SmallStringInterner);

    _ = try interner3.serialize(gpa, &writer);
    const offset3 = writer.total_bytes - @sizeOf(SmallStringInterner);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate all three
    const deserialized1 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset1)));
    deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized2 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset2)));
    deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized3 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset3)));
    deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify interner 1
    try std.testing.expectEqualStrings("interner1_string1", deserialized1.getText(idx1_1));
    try std.testing.expectEqualStrings("interner1_string2", deserialized1.getText(idx1_2));
    try std.testing.expectEqual(@as(u32, 2), deserialized1.entry_count);

    // Verify interner 2 (frozen)
    try std.testing.expectEqualStrings("interner2_string1", deserialized2.getText(idx2_1));
    try std.testing.expectEqualStrings("interner2_string2", deserialized2.getText(idx2_2));
    try std.testing.expectEqualStrings("interner2_string3", deserialized2.getText(idx2_3));
    try std.testing.expectEqual(@as(u32, 3), deserialized2.entry_count);
    if (std.debug.runtime_safety) {
        try std.testing.expect(deserialized2.frozen);
    }

    // Verify interner 3
    try std.testing.expectEqualStrings("interner3_string1", deserialized3.getText(idx3_1));
    try std.testing.expectEqual(@as(u32, 1), deserialized3.entry_count);
}

test "insert" {
    const gpa = std.testing.allocator;

    var interner = StringLiteral.Store{};
    defer interner.deinit(gpa);

    const str_1 = "abc".*;
    const str_2 = "defg".*;
    const idx_1 = try interner.insert(gpa, &str_1);
    const idx_2 = try interner.insert(gpa, &str_2);

    try std.testing.expectEqualStrings("abc", interner.get(idx_1));
    try std.testing.expectEqualStrings("defg", interner.get(idx_2));
}

test "StringLiteral.Store serialization comprehensive" {
    const gpa = std.testing.allocator;

    var store = StringLiteral.Store{};
    defer store.deinit(gpa);

    // Add various test strings including edge cases
    _ = try store.insert(gpa, "hello");
    _ = try store.insert(gpa, "world");
    _ = try store.insert(gpa, "test string with 🦎 unicode");
    _ = try store.insert(gpa, ""); // empty string
    _ = try store.insert(gpa, "\x00\x01\x02"); // binary data
    _ = try store.insert(gpa, "🦎🚀✨"); // emoji
    _ = try store.insert(gpa, "日本語"); // non-latin script
    _ = try store.insert(gpa, "test\n\r\t"); // control characters
    _ = try store.insert(gpa, "very very very very very very long string that exceeds normal buffer sizes and might cause issues with memory management");

    // Test serialization
    try serialization.testing.testSerialization(StringLiteral.Store, &store, gpa);
}

test "StringLiteral.Store empty store serialization" {
    const gpa = std.testing.allocator;

    var empty_store = StringLiteral.Store{};
    defer empty_store.deinit(gpa);

    try serialization.testing.testSerialization(StringLiteral.Store, &empty_store, gpa);
}

test "StringLiteral.Store empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an empty StringLiteral.Store
    var original = StringLiteral.Store{};
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_stringlit.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*StringLiteral.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(StringLiteral.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try std.testing.expectEqual(@as(usize, 0), deserialized.buffer.len());
}

test "StringLiteral.Store basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create original store and add some strings
    var original = StringLiteral.Store{};
    defer original.deinit(gpa);

    const idx1 = try original.insert(gpa, "hello");
    const idx2 = try original.insert(gpa, "world");
    const idx3 = try original.insert(gpa, "foo bar baz");

    // Verify original values
    try std.testing.expectEqualStrings("hello", original.get(idx1));
    try std.testing.expectEqualStrings("world", original.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", original.get(idx3));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_stringlit.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*StringLiteral.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(StringLiteral.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the strings are accessible
    try std.testing.expectEqualStrings("hello", deserialized.get(idx1));
    try std.testing.expectEqualStrings("world", deserialized.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", deserialized.get(idx3));
}

test "StringLiteral.Store comprehensive CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    var original = StringLiteral.Store{};
    defer original.deinit(gpa);

    // Test various string types
    const test_strings = [_][]const u8{
        "", // empty string
        "a", // single character
        "hello world", // simple string
        "🦎🚀✨", // emojis
        "日本語テキスト", // non-Latin script
        "\x00\x01\x02\x03", // binary data
        "line1\nline2\r\nline3", // line breaks
        "tab\tseparated\tvalues", // tabs
        "quotes: 'single' and \"double\"", // quotes
        "very long string " ** 50, // long string
    };

    var indices = std.ArrayList(StringLiteral.Idx).init(gpa);
    defer indices.deinit();

    for (test_strings) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(idx);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_comprehensive_stringlit.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*StringLiteral.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(StringLiteral.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings
    for (test_strings, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.get(idx);
        try std.testing.expectEqualStrings(expected_str, actual_str);
    }
}

test "StringLiteral.Store frozen state CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate store
    var original = StringLiteral.Store{};
    defer original.deinit(gpa);

    _ = try original.insert(gpa, "test1");
    _ = try original.insert(gpa, "test2");

    // Freeze the store
    original.freeze();

    // Verify store is frozen
    if (std.debug.runtime_safety) {
        try std.testing.expect(original.frozen);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_stringlit.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*StringLiteral.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(StringLiteral.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify frozen state is preserved
    if (std.debug.runtime_safety) {
        try std.testing.expect(deserialized.frozen);
    }
}

test "StringLiteral.Store multiple stores CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create multiple stores
    var store1 = StringLiteral.Store{};
    defer store1.deinit(gpa);

    var store2 = StringLiteral.Store{};
    defer store2.deinit(gpa);

    var store3 = StringLiteral.Store{};
    defer store3.deinit(gpa);

    // Populate differently
    const idx1_1 = try store1.insert(gpa, "store1_string1");
    const idx1_2 = try store1.insert(gpa, "store1_string2");

    const idx2_1 = try store2.insert(gpa, "store2_string1");
    store2.freeze();

    // store3 left empty

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_multiple_stringlit.dat", .{ .read = true });
    defer file.close();

    // Serialize all three
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try store1.serialize(gpa, &writer);
    const offset1 = writer.total_bytes - @sizeOf(StringLiteral.Store);

    _ = try store2.serialize(gpa, &writer);
    const offset2 = writer.total_bytes - @sizeOf(StringLiteral.Store);

    _ = try store3.serialize(gpa, &writer);
    const offset3 = writer.total_bytes - @sizeOf(StringLiteral.Store);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate all three
    const deserialized1 = @as(*StringLiteral.Store, @ptrCast(@alignCast(buffer.ptr + offset1)));
    deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized2 = @as(*StringLiteral.Store, @ptrCast(@alignCast(buffer.ptr + offset2)));
    deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized3 = @as(*StringLiteral.Store, @ptrCast(@alignCast(buffer.ptr + offset3)));
    deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify store 1
    try std.testing.expectEqualStrings("store1_string1", deserialized1.get(idx1_1));
    try std.testing.expectEqualStrings("store1_string2", deserialized1.get(idx1_2));

    // Verify store 2 (frozen)
    try std.testing.expectEqualStrings("store2_string1", deserialized2.get(idx2_1));
    if (std.debug.runtime_safety) {
        try std.testing.expect(deserialized2.frozen);
    }

    // Verify store 3 (empty)
    try std.testing.expectEqual(@as(usize, 0), deserialized3.buffer.len());
}

test "StringLiteral.Store edge case indices CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    var original = StringLiteral.Store{};
    defer original.deinit(gpa);

    // The index returned points to the first byte of the string content,
    // with the length stored in the previous 4 bytes.
    // Test various scenarios that might stress the index calculation.

    // First string starts at index 4 (after its length)
    const idx1 = try original.insert(gpa, "first");
    try std.testing.expectEqual(@as(u32, 4), @intFromEnum(idx1));

    // Second string starts at 4 + 5 + 4 = 13
    const idx2 = try original.insert(gpa, "second");
    try std.testing.expectEqual(@as(u32, 13), @intFromEnum(idx2));

    // Empty string
    const idx3 = try original.insert(gpa, "");
    try std.testing.expectEqual(@as(u32, 23), @intFromEnum(idx3));

    // Very long string
    const long_str = "x" ** 1000;
    const idx4 = try original.insert(gpa, long_str);
    try std.testing.expectEqual(@as(u32, 27), @intFromEnum(idx4));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_edge_indices_stringlit.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*StringLiteral.Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(StringLiteral.Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings with their exact indices
    try std.testing.expectEqualStrings("first", deserialized.get(idx1));
    try std.testing.expectEqualStrings("second", deserialized.get(idx2));
    try std.testing.expectEqualStrings("", deserialized.get(idx3));
    try std.testing.expectEqualStrings(long_str, deserialized.get(idx4));
}
