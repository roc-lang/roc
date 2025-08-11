//! Tests for ModuleEnv
const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");

const ModuleEnv = can.ModuleEnv;
const CompactWriter = collections.CompactWriter;
const Ident = base.Ident;
const Expr = can.CIR.Expr;

// test "ModuleEnv.Serialized roundtrip" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     var common_env = try base.CommonEnv.init(gpa, "");
//     defer common_env.deinit(gpa);

//     // Add some test data
//     const hello_idx = try common_env.insertIdent(gpa, Ident.for_text("hello"));
//     const world_idx = try common_env.insertIdent(gpa, Ident.for_text("world"));

//     _ = try common_env.insertString(gpa, "test string");

//     try common_env.addExposedById(gpa, hello_idx);

//     // Create original ModuleEnv with some data
//     var original = try ModuleEnv.init(gpa, &common_env);
//     defer original.deinit();

//     // Initialize CIR fields to ensure imports are available
//     try original.initCIRFields(gpa, "TestModule");

//     try original.exposed_items.setNodeIndexById(gpa, @as(u32, @bitCast(hello_idx)), 42);
//     original.exposed_items.ensureSorted(gpa);

//     _ = try original.line_starts.append(gpa, 0);
//     _ = try original.line_starts.append(gpa, 10);
//     _ = try original.line_starts.append(gpa, 20);

//     const source = "hello world\ntest line 2\n";
//     original.source = source;
//     original.module_name = "TestModule";

//     // Create a CompactWriter and arena
//     var arena = std.heap.ArenaAllocator.init(gpa);
//     defer arena.deinit();
//     const arena_alloc = arena.allocator();

//     var tmp_dir = testing.tmpDir(.{});
//     defer tmp_dir.cleanup();
//     const tmp_file = try tmp_dir.dir.createFile("test.compact", .{ .read = true });
//     defer tmp_file.close();

//     var writer = CompactWriter.init();
//     defer writer.deinit(arena_alloc);

//     // Allocate space for ModuleEnv (not Serialized) since deserialize requires enough space
//     const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv);
//     const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv);
//     const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
//     try serialized_ptr.serialize(&original, arena_alloc, &writer);

//     // Write to file
//     try writer.writeGather(arena_alloc, tmp_file);

//     // Read back
//     const file_size = try tmp_file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, @alignOf(ModuleEnv), @intCast(file_size));
//     defer gpa.free(buffer);
//     _ = try tmp_file.pread(buffer, 0);

//     // Find the ModuleEnv at the tracked offset
//     const deserialized_ptr: *ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr + env_start_offset));
//     const env = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa, "TestModule");

//     // Verify the data was preserved
//     // try testing.expectEqual(@as(usize, 2), env.ident_ids_for_slicing.len());
//     try testing.expectEqualStrings("hello", env.getIdent(hello_idx));
//     try testing.expectEqualStrings("world", env.getIdent(world_idx));

//     try testing.expectEqual(@as(usize, 1), env.exposed_items.count());
//     try testing.expectEqual(@as(?u16, 42), env.exposed_items.getNodeIndexById(gpa, @as(u32, @bitCast(hello_idx))));

//     try testing.expectEqual(@as(usize, 3), env.line_starts.len());
//     try testing.expectEqual(@as(u32, 0), env.line_starts.items.items[0]);
//     try testing.expectEqual(@as(u32, 10), env.line_starts.items.items[1]);
//     try testing.expectEqual(@as(u32, 20), env.line_starts.items.items[2]);

//     // TODO restore source using CommonEnv
//     // try testing.expectEqualStrings(source, env.source);
//     try testing.expectEqualStrings("TestModule", env.module_name);
// }

test "ModuleEnv with types CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var common_env = try base.CommonEnv.init(gpa, "");
    defer common_env.deinit(gpa);

    // Create ModuleEnv with some types
    var original = try ModuleEnv.init(gpa, &common_env);
    defer original.deinit();

    // Initialize CIR fields
    try original.initCIRFields(gpa, "test.Types");

    // Add some type variables
    const var1 = try original.types.freshFromContent(.err);
    const var2 = try original.types.freshFromContent(.{ .flex_var = null });

    _ = var1;
    _ = var2;

    // Create arena allocator for serialization
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_types_module_env.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    // Allocate space for ModuleEnv (not Serialized) since deserialize requires enough space
    const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv);
    const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv);
    const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
    try serialized_ptr.serialize(&original, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(arena_alloc, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, @alignOf(ModuleEnv), @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Find the ModuleEnv at the tracked offset
    const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + env_start_offset)));
    const deserialized = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa, "", "test.Types");

    // Verify module name
    try testing.expectEqualStrings("test.Types", deserialized.module_name);

    // Can't verify types directly as the internal structure is complex
    // but we can at least verify the module env is intact
    try testing.expect(deserialized.types.len() >= 2);
}

// test "ModuleEnv empty CompactWriter roundtrip" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     var common_env = try base.CommonEnv.init(gpa, "");
//     defer common_env.deinit(gpa);

//     // Create empty ModuleEnv
//     var original = try ModuleEnv.init(gpa, &common_env);
//     defer original.deinit();

//     // Don't initialize CIR fields to keep it truly empty

//     // Create arena allocator for serialization
//     var arena = std.heap.ArenaAllocator.init(gpa);
//     defer arena.deinit();
//     const arena_alloc = arena.allocator();

//     // Create a temp file
//     var tmp_dir = testing.tmpDir(.{});
//     defer tmp_dir.cleanup();

//     const file = try tmp_dir.dir.createFile("test_empty_module_env.dat", .{ .read = true });
//     defer file.close();

//     // Serialize
//     var writer = CompactWriter.init();
//     defer writer.deinit(arena_alloc);

//     // Allocate space for ModuleEnv (not Serialized) since deserialize requires enough space
//     const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv);
//     const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv);
//     const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
//     try serialized_ptr.serialize(&original, arena_alloc, &writer);

//     // Write to file
//     try writer.writeGather(arena_alloc, file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, @alignOf(ModuleEnv), @intCast(file_size));
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // Find the ModuleEnv at the tracked offset
//     const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + env_start_offset)));
//     const deserialized = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa, "", "test.Empty");

//     // Verify module name
//     try testing.expectEqualStrings("test.Empty", deserialized.module_name);

//     // Verify the deserialized ModuleEnv has the expected state
//     // Note: Even an "empty" ModuleEnv may have some initialized state
//     try testing.expect(deserialized.idents.interner.bytes.len() >= 0);
//     try testing.expect(deserialized.strings.buffer.len() >= 0);
// }

// test "ModuleEnv with source code CompactWriter roundtrip" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     const source =
//         \\app [main] {
//         \\    main = \{} ->
//         \\        "Hello, World!"
//         \\}
//     ;

//     var common_env = try base.CommonEnv.init(gpa, source);
//     defer common_env.deinit(gpa);

//     // Calculate line starts
//     try common_env.calcLineStarts(gpa);

//     // Create ModuleEnv with source
//     var original = try ModuleEnv.init(gpa, &common_env);
//     defer original.deinit();

//     // Initialize CIR fields
//     try original.initCIRFields(gpa, "test.Hello");

//     // Create arena allocator for serialization
//     var arena = std.heap.ArenaAllocator.init(gpa);
//     defer arena.deinit();
//     const arena_alloc = arena.allocator();

//     // Create a temp file
//     var tmp_dir = testing.tmpDir(.{});
//     defer tmp_dir.cleanup();

//     const file = try tmp_dir.dir.createFile("test_source_module_env.dat", .{ .read = true });
//     defer file.close();

//     // Serialize
//     var writer = CompactWriter.init();
//     defer writer.deinit(arena_alloc);

//     // Allocate space for ModuleEnv (not Serialized) since deserialize requires enough space
//     const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv);
//     const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv);
//     const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
//     try serialized_ptr.serialize(&original, arena_alloc, &writer);

//     // Write to file
//     try writer.writeGather(arena_alloc, file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, @alignOf(ModuleEnv), @intCast(file_size));
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // Find the ModuleEnv at the tracked offset
//     const deserialized_ptr: *ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr + env_start_offset));
//     const deserialized = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa, source, "test.Hello");

//     // Verify source and module name
//     try testing.expectEqualStrings(source, deserialized.source);
//     try testing.expectEqualStrings("test.Hello", deserialized.module_name);

//     // Verify line starts were preserved
//     try testing.expectEqual(original.line_starts.items.items.len, deserialized.line_starts.items.items.len);
// }

test "ModuleEnv pushExprTypesToSExprTree extracts and formats types" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var common_env = try base.CommonEnv.init(gpa, "");
    defer common_env.deinit(gpa);

    // First add a string literal
    const str_literal_idx = try common_env.insertString(gpa, "hello");

    // Create a simple ModuleEnv
    var env = try ModuleEnv.init(gpa, &common_env);
    defer env.deinit();

    // Add a string segment expression
    const segment_idx = try env.addExprAndTypeVar(.{ .e_str_segment = .{ .literal = str_literal_idx } }, .{ .structure = .str }, base.Region.from_raw_offsets(0, 5));

    // Now create a string expression that references the segment
    const expr_idx = try env.addExprAndTypeVar(.{ .e_str = .{ .span = Expr.Span{ .span = base.DataSpan{ .start = @intFromEnum(segment_idx), .len = 1 } } } }, .{ .structure = .str }, base.Region.from_raw_offsets(0, 5));

    // Create an S-expression tree
    var tree = base.SExprTree.init(gpa);
    defer tree.deinit();

    // Call pushExprTypesToSExprTree (which is called by pushTypesToSExprTree)
    try env.pushTypesToSExprTree(expr_idx, &tree);

    // Convert tree to string
    var result = std.ArrayList(u8).init(gpa);
    defer result.deinit();
    try tree.toStringPretty(result.writer().any());

    // Verify the output contains the type information
    const result_str = result.items;

    // Uncomment to debug:
    // std.debug.print("\nType extraction result:\n{s}\n", .{result_str});

    try testing.expect(std.mem.indexOf(u8, result_str, "(expr") != null);
    try testing.expect(std.mem.indexOf(u8, result_str, "(type") != null);
    try testing.expect(std.mem.indexOf(u8, result_str, "Str") != null);
}
