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
const testing = std.testing;

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
    try original.setExposedValueNodeIndexById(hello_idx, 42);
    original.ensureExposedSorted(gpa);

    try original.common.calcLineStarts(gpa);

    const import_json = try original.imports.getOrPut(gpa, &original.common, "json.Json");
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(try original.imports.getOrPut(gpa, &original.common, "core.List")));
    const import_json_duplicate = try original.imports.getOrPut(gpa, &original.common, "json.Json");
    try std.testing.expectEqual(import_json, import_json_duplicate);
    try std.testing.expectEqual(@as(usize, 2), original.imports.imports.len());

    var arena = collections.SingleThreadArena.init(gpa);
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
    try std.testing.expectEqual(@as(?u32, 42), env.common.exposed_items.getValueNodeIndexById(gpa, @as(u32, @bitCast(hello_idx))));

    try std.testing.expectEqual(original.common.line_starts.len(), env.common.line_starts.len());
    for (original.common.line_starts.items.items, env.common.line_starts.items.items) |expected, actual| {
        try std.testing.expectEqual(expected, actual);
    }
    try std.testing.expectEqualStrings("TestModule", env.module_name);

    try std.testing.expectEqual(@as(usize, 2), env.imports.imports.len());
    try std.testing.expectEqualStrings("json.Json", env.common.strings.get(env.imports.imports.items.items[0]));
    try std.testing.expectEqualStrings("core.List", env.common.strings.get(env.imports.imports.items.items[1]));
    try std.testing.expectEqual(@as(usize, 2), env.imports.map.count());

    // Verify original data before serialization was correct
    // initCIRFields inserts the module name ("TestModule") into the interner, so we have 3 total: hello, world, TestModule
    // ModuleEnv.init() also interns 16 well-known identifiers: Try, OutOfRange, Builtin, plus, minus, times, div_by, div_trunc_by, rem_by, negate, not, is_lt, is_lte, is_gt, is_gte, is_eq
    // Plus 19 type identifiers: Str, Builtin.Try, Builtin.Num.Numeral, Builtin.Str, List, Box, Builtin.Num.{U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, F32, F64, Dec}
    // Plus 3 field/tag identifiers: before_dot, after_dot, ProvidedByCompiler
    // Plus 7 more identifiers: tag, payload, is_negative, digits_before_pt, digits_after_pt, box, unbox
    // Plus 2 Try tag identifiers: Ok, Err
    // Plus 1 method identifier: from_numeral
    // Plus 1 interpolation method identifier: from_interpolation
    // Plus 2 Bool tag identifiers: True, False
    // Plus 6 from_utf8 identifiers: byte_index, string, is_ok, problem_code, problem, index
    // Plus 2 synthetic identifiers for ? operator desugaring: #ok, #err
    // Plus 1 synthetic identifier for .. implicit rigids in open tag unions or records
    // Plus 2 numeric method identifiers: abs, abs_diff
    // Plus 1 inspect method identifier: to_inspect
    // Plus 15 unqualified builtin type names: Num, Bool, U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec
    // Plus 2 fully qualified builtin type names: Builtin.List, Builtin.Box
    // Plus 2 fully qualified Box intrinsic method names: Builtin.Box.box, Builtin.Box.unbox
    // Plus 1 fully qualified Bool type name: Builtin.Bool
    // Plus 4 fully qualified Crypto builtin type names: SHA256/BLAKE3 Digest and Hasher
    // Count reflects the merged builtin set, including structural parser/encoder
    // method identifiers, Builtin.Json.Encoding's parse/encode helpers, and Crypto.
    try testing.expectEqual(@as(u32, 106), original.common.idents.interner.entry_count);
    try testing.expectEqualStrings("hello", original.getIdent(hello_idx));
    try testing.expectEqualStrings("world", original.getIdent(world_idx));

    // Verify imports before serialization
    try testing.expectEqual(import_json, import_json_duplicate); // Deduplication should work
    try testing.expectEqual(@as(usize, 2), original.imports.imports.len()); // Should have 2 unique imports

    // First verify that the CommonEnv data was preserved after deserialization
    // Should have same identifiers as original, including the builtin structural method identifiers.
    // (Note: "Try" is now shared with well-known identifiers, reducing total by 1)
    try testing.expectEqual(@as(u32, 106), env.common.idents.interner.entry_count);

    try testing.expectEqual(@as(usize, 1), env.common.exposed_items.count());
    try testing.expectEqual(@as(?u32, 42), env.common.exposed_items.getValueNodeIndexById(gpa, @as(u32, @bitCast(hello_idx))));

    try testing.expectEqual(@as(usize, 3), env.common.line_starts.len());
    try testing.expectEqual(@as(u32, 0), env.common.line_starts.items.items[0]);
    try testing.expectEqual(@as(u32, 12), env.common.line_starts.items.items[1]);
    try testing.expectEqual(@as(u32, 24), env.common.line_starts.items.items[2]);

    try testing.expectEqualStrings(source, env.common.source);
    try testing.expectEqualStrings("TestModule", env.module_name);

    // Verify imports were preserved after deserialization
    try testing.expectEqual(@as(usize, 2), env.imports.imports.len());

    // Verify the import strings are correct (they reference string indices in the string store)
    const import_str1 = env.common.strings.get(env.imports.imports.items.items[0]);
    const import_str2 = env.common.strings.get(env.imports.imports.items.items[1]);

    try testing.expectEqualStrings("json.Json", import_str1);
    try testing.expectEqualStrings("core.List", import_str2);

    // Verify that the map was repopulated correctly
    try testing.expectEqual(@as(usize, 2), env.imports.map.count());

    // Test that deduplication still works after deserialization for existing keys.
    // Note: the deserialized StringLiteral.Store points into the cache buffer and
    // cannot be grown (SafeList.deserializeInto contract), so we only test lookup
    // of already-serialized strings here.
    var test_arena = collections.SingleThreadArena.init(gpa);
    defer test_arena.deinit();
    const test_alloc = test_arena.allocator();

    const import4 = try env.imports.getOrPut(test_alloc, &env.common, "json.Json");

    // Should find existing json.Json (deduplication)
    try testing.expectEqual(@as(u32, 0), @intFromEnum(import4));
    try testing.expectEqual(@as(usize, 2), env.imports.imports.len());
}

test "ModuleEnv.Serialized finalizes method metadata tables before writing" {
    const gpa = std.testing.allocator;
    const source = "";

    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();

    try original.initCIRFields("Test");
    try original.common.calcLineStarts(gpa);

    const get_ident = try original.insertIdent(Ident.for_text("get"));
    const set_ident = try original.insertIdent(Ident.for_text("set"));
    const get_qualified = try original.insertIdent(Ident.for_text("Local.get"));
    const set_qualified = try original.insertIdent(Ident.for_text("Local.set"));
    const owner_stmt: CIR.Statement.Idx = @enumFromInt(1);

    try original.registerMethodIdentForOwner(owner_stmt, set_ident, set_qualified);
    try original.registerMethodDefForOwner(owner_stmt, set_ident, .{
        .type_node_idx = @enumFromInt(2),
        .def_idx = @enumFromInt(2),
    });
    try original.registerMethodIdentForOwner(owner_stmt, get_ident, get_qualified);
    try original.registerMethodDefForOwner(owner_stmt, get_ident, .{
        .type_node_idx = @enumFromInt(1),
        .def_idx = @enumFromInt(1),
    });
    original.finalizeMethodTables();

    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(&original, arena_alloc, &writer);

    try std.testing.expect(serialized.method_idents.sorted);
    try std.testing.expect(serialized.method_idents.deduplicated);
    try std.testing.expectEqual(@as(u64, 2), serialized.method_idents.entries_len);

    try std.testing.expect(serialized.method_defs.sorted);
    try std.testing.expect(serialized.method_defs.deduplicated);
    try std.testing.expectEqual(@as(u64, 2), serialized.method_defs.entries_len);
}

test "ModuleEnv.Serialized roundtrip preserves file dependency states" {
    const gpa = std.testing.allocator;
    const source = "";

    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();

    try original.initCIRFields("Test");

    const present_idx = try original.recordFileDependency("data.txt");
    const present_hash = [_]u8{0x11} ** 32;
    original.setFileDependencyContentHash(present_idx, present_hash);

    const missing_idx = try original.recordFileDependency("missing.txt");
    original.setFileDependencyMissing(missing_idx);

    const unreadable_idx = try original.recordFileDependency("denied.txt");
    original.setFileDependencyUnreadable(unreadable_idx);

    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(&original, arena_alloc, &writer);

    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const deserialized_ptr: *ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const env = try deserialized_ptr.deserializeWithMutableTypes(@intFromPtr(buffer.ptr), gpa, source, "Test");
    defer {
        env.deinitCachedModule();
        gpa.destroy(env);
    }

    const deps = env.file_dependencies.items.items;
    try testing.expectEqual(@as(usize, 3), deps.len);

    try testing.expectEqual(ModuleEnv.FileDependencyState.present, deps[0].state);
    try testing.expectEqualStrings("data.txt", env.fileDependencyRelativePath(deps[0]));
    try testing.expectEqualSlices(u8, &present_hash, &deps[0].content_hash);

    try testing.expectEqual(ModuleEnv.FileDependencyState.missing, deps[1].state);
    try testing.expectEqualStrings("missing.txt", env.fileDependencyRelativePath(deps[1]));

    try testing.expectEqual(ModuleEnv.FileDependencyState.unreadable, deps[2].state);
    try testing.expectEqualStrings("denied.txt", env.fileDependencyRelativePath(deps[2]));
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

    // Convert tree to string.
    // fromArrayList takes ownership of the ArrayList buffer immediately, so
    // we must call toArrayList() explicitly before inspecting the result.
    var result = std.ArrayList(u8).empty;
    defer result.deinit(gpa);
    {
        var aw: std.Io.Writer.Allocating = .fromArrayList(gpa, &result);
        try tree.toStringPretty(&aw.writer, .include_linecol);
        result = aw.toArrayList();
    }

    // Verify the output contains the type information
    const result_str = result.items;

    try testing.expect(std.mem.find(u8, result_str, "(expr") != null);
    try testing.expect(std.mem.find(u8, result_str, "(type") != null);
    try testing.expect(std.mem.find(u8, result_str, "Str") != null);
}
