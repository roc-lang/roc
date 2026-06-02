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
    const tmp_file = try tmp_dir.dir.createFile("module_env.compact", .{ .read = true });
    defer tmp_file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(&original, arena_alloc, &writer);
    try writer.writeGather(arena_alloc, tmp_file);

    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

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
    try std.testing.expectEqual(@as(?u32, 42), env.common.exposed_items.getNodeIndexById(gpa, @as(u32, @bitCast(hello_idx))));

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

test "ModuleEnv.Serialized finalizes method metadata tables before writing" {
    const gpa = std.testing.allocator;
    const source = "module []\n";

    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();

    try original.initCIRFields("Test");
    try original.common.calcLineStarts(gpa);

    const type_ident = try original.insertIdent(Ident.for_text("Local"));
    const method_ident = try original.insertIdent(Ident.for_text("get"));
    const first_qualified = try original.insertIdent(Ident.for_text("First.Local.get"));
    const second_qualified = try original.insertIdent(Ident.for_text("Second.Local.get"));

    try original.registerMethod(type_ident, method_ident, first_qualified, .{
        .type_node_idx = @enumFromInt(1),
        .def_idx = @enumFromInt(1),
    });
    try original.registerMethod(type_ident, method_ident, second_qualified, .{
        .type_node_idx = @enumFromInt(2),
        .def_idx = @enumFromInt(2),
    });

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(&original, arena_alloc, &writer);

    try std.testing.expect(serialized.method_idents.sorted);
    try std.testing.expect(serialized.method_idents.deduplicated);
    try std.testing.expectEqual(@as(u64, 1), serialized.method_idents.entries_len);

    try std.testing.expect(serialized.method_defs.sorted);
    try std.testing.expect(serialized.method_defs.deduplicated);
    try std.testing.expectEqual(@as(u64, 1), serialized.method_defs.entries_len);
}

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
    try tree.toStringPretty(result.writer(gpa).any(), .include_linecol);

    try std.testing.expect(std.mem.indexOf(u8, result.items, "(expr") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.items, "(type") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.items, "Str") != null);
}
