//! Tests for ModuleEnv serialization and type S-expression rendering.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const types = @import("types");

const CIR = can.CIR;
const CompactWriter = collections.CompactWriter;
const Expr = CIR.Expr;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;

test "ModuleEnv.Serialized roundtrip" {
    const gpa = std.testing.allocator;
    const source = "hello world\ntest line 2\n";

    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();

    try original.initCIRFields("TestModule");

    const hello_idx = try original.insertIdent(Ident.for_text("hello"));
    const world_idx = try original.insertIdent(Ident.for_text("world"));
    _ = try original.insertString("test string");

    try original.addExposedById(hello_idx);
    try original.setExposedNodeIndexById(hello_idx, 42);
    original.ensureExposedSorted(gpa);

    try original.common.calcLineStarts(gpa);

    const import_json = try original.imports.getOrPut(gpa, &original.common.strings, "json.Json");
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(try original.imports.getOrPut(gpa, &original.common.strings, "core.List")));
    const import_json_duplicate = try original.imports.getOrPut(gpa, &original.common.strings, "json.Json");
    try std.testing.expectEqual(import_json, import_json_duplicate);
    try std.testing.expectEqual(@as(usize, 2), original.imports.imports.len());

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile(std.testing.io, "test.compact", .{ .read = true });
    defer tmp_file.close(std.testing.io);

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(&original, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(tmp_file, std.testing.io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try tmp_file.readPositionalAll(std.testing.io, buffer, 0);

    const deserialized_ptr: *ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const env = try deserialized_ptr.deserializeWithMutableTypes(@intFromPtr(buffer.ptr), gpa, source, "TestModule");
    defer {
        env.deinitCachedModule();
        gpa.destroy(env);
    }

    try std.testing.expectEqual(original.common.idents.interner.entry_count, env.common.idents.interner.entry_count);
    try std.testing.expectEqualStrings("hello", original.getIdent(hello_idx));
    try std.testing.expectEqualStrings("world", original.getIdent(world_idx));
    try std.testing.expectEqualStrings("hello", env.getIdent(hello_idx));
    try std.testing.expectEqualStrings("world", env.getIdent(world_idx));

    try std.testing.expectEqual(@as(usize, 1), env.common.exposed_items.count());
    try std.testing.expectEqual(@as(?u16, 42), env.common.exposed_items.getNodeIndexById(gpa, @as(u32, @bitCast(hello_idx))));

    try std.testing.expectEqual(original.common.line_starts.len(), env.common.line_starts.len());
    for (original.common.line_starts.items.items, env.common.line_starts.items.items) |expected, actual| {
        try std.testing.expectEqual(expected, actual);
    }
    try std.testing.expectEqualStrings("TestModule", env.module_name);

    try std.testing.expectEqual(@as(usize, 2), env.imports.imports.len());
    try std.testing.expectEqualStrings("json.Json", env.common.strings.get(env.imports.imports.items.items[0]));
    try std.testing.expectEqualStrings("core.List", env.common.strings.get(env.imports.imports.items.items[1]));
    try std.testing.expectEqual(@as(usize, 2), env.imports.map.count());

    const import_json_after = try env.imports.getOrPut(gpa, &env.common.strings, "json.Json");
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(import_json_after));
    try std.testing.expectEqual(@as(usize, 2), env.imports.imports.len());
}

// test "ModuleEnv with types CompactWriter roundtrip" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     var common_env = try base.CommonEnv.init(gpa, "");
//     // Module env takes ownership of Common env -- no need to deinit here

//     // Create ModuleEnv with some types
//     var original = try ModuleEnv.init(gpa, &common_env);
//     defer original.deinit();

//     // Initialize CIR fields
//     try original.initCIRFields("test.Types");

//     // Add some type variables
//     const var1 = try original.types.freshFromContent(.err);
//     const var2 = try original.types.freshFromContent(.{ .flex_var = null });

//     _ = var1;
//     _ = var2;

//     // Create arena allocator for serialization
//     var arena = std.heap.ArenaAllocator.init(gpa);
//     defer arena.deinit();
//     const arena_alloc = arena.allocator();

//     // Create a temp file
//     var tmp_dir = testing.tmpDir(.{});
//     defer tmp_dir.cleanup();

//     const file = try tmp_dir.dir.createFile("test_types_module_env.dat", .{ .read = true });
//     defer file.close();

//     // Serialize
//     var writer = CompactWriter.init();
//     defer writer.deinit(arena_alloc);

//     // First, allocate and serialize the CommonEnv separately using the working pattern
//     const common_serialized = try writer.appendAlloc(arena_alloc, base.CommonEnv.Serialized);
//     try common_serialized.serialize(original.common, arena_alloc, &writer);

//     // Now serialize the ModuleEnv, but we'll need to handle the common field specially
//     const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
//     try serialized.serialize(&original, arena_alloc, &writer);

//     // Update the ModuleEnv.Serialized to point to our separately serialized CommonEnv
//     serialized.common = common_serialized.*;

//     // Write to file
//     try writer.writeGather(file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // The CommonEnv.Serialized is at the beginning of the buffer
//     const common_serialized_ptr = @as(*base.CommonEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));
//     const deserialized_common = common_serialized_ptr.deserialize(@intFromPtr(buffer.ptr), "");

//     // The ModuleEnv.Serialized follows after the CommonEnv.Serialized
//     const module_env_offset = @sizeOf(base.CommonEnv.Serialized);
//     const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + module_env_offset)));

//     // Now manually construct the ModuleEnv using the deserialized CommonEnv
//     const deserialized = @as(*ModuleEnv, @ptrCast(@alignCast(deserialized_ptr)));
//     deserialized.* = ModuleEnv{
//         .gpa = gpa,
//         .common = deserialized_common,
//         .types = deserialized_ptr.types.deserialize(@intFromPtr(buffer.ptr)).*,
//         .all_defs = deserialized_ptr.all_defs,
//         .all_statements = deserialized_ptr.all_statements,
//         .external_decls = deserialized_ptr.external_decls.deserialize(@intFromPtr(buffer.ptr)).*,
//         .imports = (try deserialized_ptr.imports.deserialize(@intFromPtr(buffer.ptr), gpa)).*,
//         .module_name = "test.Types",
//         .diagnostics = deserialized_ptr.diagnostics,
//         .store = deserialized_ptr.store.deserialize(@intFromPtr(buffer.ptr), gpa).*,
//     };

//     // Verify module name
//     try testing.expectEqualStrings("test.Types", deserialized.module_name);

//     // Debug: Check the length of types before and after serialization
//     std.debug.print("\nOriginal types.len(): {}\n", .{original.types.len()});
//     std.debug.print("Types serialized field - slots.len(): {}\n", .{deserialized_ptr.types.slots.len});
//     std.debug.print("Types serialized field - descs.len(): {}\n", .{deserialized_ptr.types.descs.len});
//     std.debug.print("Deserialized types.len(): {}\n", .{deserialized.types.len()});

//     // Can't verify types directly as the internal structure is complex
//     // but we can at least verify the module env is intact
//     try testing.expect(deserialized.types.len() >= 2);
// }

// test "ModuleEnv empty CompactWriter roundtrip" {
//     const testing = std.testing;
//     const gpa = testing.allocator;

//     var common_env = try base.CommonEnv.init(gpa, "");
//     // Module env takes ownership of Common env -- no need to deinit here

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
//     try writer.writeGather(file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, @alignOf(ModuleEnv), @intCast(file_size));
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // Find the ModuleEnv at the tracked offset
//     const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + env_start_offset)));
//     const deserialized = deserialized_ptr.deserialize(@intFromPtr(buffer.ptr), gpa, "", "test.Empty");

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
//     // Module env takes ownership of Common env -- no need to deinit here

//     // Calculate line starts
//     try common_env.calcLineStarts(gpa);

//     // Create ModuleEnv with source
//     var original = try ModuleEnv.init(gpa, &common_env);
//     defer original.deinit();

//     // Initialize CIR fields
//     try original.initCIRFields("test.Hello");

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
//     try writer.writeGather(file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, @alignOf(ModuleEnv), @intCast(file_size));
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // Find the ModuleEnv at the tracked offset
//     const deserialized_ptr: *ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr + env_start_offset));
//     const deserialized = deserialized_ptr.deserialize(@intFromPtr(buffer.ptr), gpa, source, "test.Hello");

//     // Verify source and module name
//     try testing.expectEqualStrings(source, deserialized.source);
//     try testing.expectEqualStrings("test.Hello", deserialized.module_name);

//     // Verify line starts were preserved
//     try testing.expectEqual(original.line_starts.items.items.len, deserialized.line_starts.items.items.len);
// }

test "ModuleEnv pushExprTypesToSExprTree extracts and formats types" {
    const gpa = std.testing.allocator;

    var env = try ModuleEnv.init(gpa, "hello");
    defer env.deinit();

    const str_literal_idx = try env.insertString("hello");
    const str_ident = try env.insertIdent(Ident.for_text("Str"));
    const builtin_ident = try env.insertIdent(Ident.for_text("Builtin"));

    const segment_idx = try env.addExpr(.{ .e_str_segment = .{ .literal = str_literal_idx } }, base.Region.from_raw_offsets(0, 5));
    const expr_idx = try env.addExpr(.{ .e_str = .{ .span = Expr.Span{ .span = base.DataSpan{ .start = @intFromEnum(segment_idx), .len = 1 } } } }, base.Region.from_raw_offsets(0, 5));

    const segment_var = try env.types.freshFromContent(.err);
    try std.testing.expectEqual(ModuleEnv.varFrom(segment_idx), segment_var);

    const expr_var = try env.types.freshFromContent(.err);
    try std.testing.expectEqual(ModuleEnv.varFrom(expr_idx), expr_var);

    const str_backing_var = try env.types.freshFromContent(.{ .structure = .empty_record });
    const str_content = try env.types.mkNominal(
        types.TypeIdent{ .ident_idx = str_ident },
        str_backing_var,
        &.{},
        builtin_ident,
        false,
    );
    try env.types.setVarContent(expr_var, str_content);

    var tree = base.SExprTree.init(gpa);
    defer tree.deinit();

    try env.pushTypesToSExprTree(expr_idx, &tree);

    var result = std.ArrayList(u8).empty;
    defer result.deinit(gpa);
    var aw: std.Io.Writer.Allocating = .fromArrayList(gpa, &result);
    defer result = aw.toArrayList();
    try tree.toStringPretty(&aw.writer, .include_linecol);

    // Verify the output contains the type information
    const result_str = result.items;

    try testing.expect(std.mem.find(u8, result_str, "(expr") != null);
    try testing.expect(std.mem.find(u8, result_str, "(type") != null);
    try testing.expect(std.mem.find(u8, result_str, "Str") != null);
}
