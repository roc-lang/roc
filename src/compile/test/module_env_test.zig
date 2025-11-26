//! Tests for ModuleEnv
const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const collections = @import("collections");

const ModuleEnv = can.ModuleEnv;
const CompactWriter = collections.CompactWriter;
const Ident = base.Ident;
const Expr = can.CIR.Expr;
const CIR = can.CIR;

test "ModuleEnv.Serialized roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const source = "hello world\ntest line 2\n";

    // Create original ModuleEnv with some data
    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();

    // Add some test data
    const hello_idx = try original.insertIdent(Ident.for_text("hello"));
    const world_idx = try original.insertIdent(Ident.for_text("world"));

    _ = try original.insertString("test string");

    try original.addExposedById(hello_idx);

    _ = try original.common.line_starts.append(gpa, 0);
    _ = try original.common.line_starts.append(gpa, 10);
    _ = try original.common.line_starts.append(gpa, 20);

    // Initialize CIR fields to ensure imports are available
    try original.initCIRFields(gpa, "TestModule");

    // Add some imports to test serialization/deserialization
    const import1 = try original.imports.getOrPut(gpa, &original.common.strings, "json.Json");
    const import2 = try original.imports.getOrPut(gpa, &original.common.strings, "core.List");
    const import3 = try original.imports.getOrPut(gpa, &original.common.strings, "json.Json"); // duplicate - should return same as import1

    _ = import2; // Mark as used

    try original.setExposedNodeIndexById(hello_idx, 42);
    original.ensureExposedSorted(gpa);
    original.module_name = "TestModule";

    // Create a CompactWriter and arena
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test.compact", .{ .read = true });
    defer tmp_file.close();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    // Now serialize the ModuleEnv, but we'll need to handle the common field specially
    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(&original, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(arena_alloc, tmp_file);

    // Read back
    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

    const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));

    // Create an arena for deserialization to avoid memory leaks
    var deser_arena = std.heap.ArenaAllocator.init(gpa);
    defer deser_arena.deinit();
    const deser_alloc = deser_arena.allocator();

    // Now manually construct the ModuleEnv using the deserialized CommonEnv
    const env = @as(*ModuleEnv, @ptrCast(@alignCast(deserialized_ptr)));

    // Deserialize common env first so we can look up identifiers
    const common = deserialized_ptr.common.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), source).*;

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = deserialized_ptr.types.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa).*,
        .module_kind = deserialized_ptr.module_kind.decode(),
        .all_defs = deserialized_ptr.all_defs,
        .all_statements = deserialized_ptr.all_statements,
        .exports = deserialized_ptr.exports,
        .requires_types = deserialized_ptr.requires_types.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr)))).*,
        .builtin_statements = deserialized_ptr.builtin_statements,
        .external_decls = deserialized_ptr.external_decls.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr)))).*,
        .imports = (try deserialized_ptr.imports.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), deser_alloc)).*,
        .module_name = "TestModule",
        .module_name_idx = undefined, // Not used for deserialized modules (only needed during fresh canonicalization)
        .diagnostics = deserialized_ptr.diagnostics,
        .store = deserialized_ptr.store.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), deser_alloc).*,
        .evaluation_order = null,
        .from_int_digits_ident = common.findIdent(Ident.FROM_INT_DIGITS_METHOD_NAME) orelse unreachable,
        .from_dec_digits_ident = common.findIdent(Ident.FROM_DEC_DIGITS_METHOD_NAME) orelse unreachable,
        .try_ident = common.findIdent("Try") orelse unreachable,
        .out_of_range_ident = common.findIdent("OutOfRange") orelse unreachable,
        .builtin_module_ident = common.findIdent("Builtin") orelse unreachable,
        .plus_ident = common.findIdent(Ident.PLUS_METHOD_NAME) orelse unreachable,
        .minus_ident = common.findIdent("minus") orelse unreachable,
        .times_ident = common.findIdent("times") orelse unreachable,
        .div_by_ident = common.findIdent("div_by") orelse unreachable,
        .div_trunc_by_ident = common.findIdent("div_trunc_by") orelse unreachable,
        .rem_by_ident = common.findIdent("rem_by") orelse unreachable,
        .negate_ident = common.findIdent(Ident.NEGATE_METHOD_NAME) orelse unreachable,
        .not_ident = common.findIdent("not") orelse unreachable,
        .is_lt_ident = common.findIdent("is_lt") orelse unreachable,
        .is_lte_ident = common.findIdent("is_lte") orelse unreachable,
        .is_gt_ident = common.findIdent("is_gt") orelse unreachable,
        .is_gte_ident = common.findIdent("is_gte") orelse unreachable,
        .is_eq_ident = common.findIdent("is_eq") orelse unreachable,
        .is_ne_ident = common.findIdent("is_ne") orelse unreachable,
        // Fully-qualified type identifiers for type checking and layout generation
        .builtin_try_ident = common.findIdent("Builtin.Try") orelse unreachable,
        .builtin_numeral_ident = common.findIdent("Builtin.Num.Numeral") orelse unreachable,
        .builtin_str_ident = common.findIdent("Builtin.Str") orelse unreachable,
        .list_type_ident = common.findIdent("List") orelse unreachable,
        .box_type_ident = common.findIdent("Box") orelse unreachable,
        .u8_type_ident = common.findIdent("Builtin.Num.U8") orelse unreachable,
        .i8_type_ident = common.findIdent("Builtin.Num.I8") orelse unreachable,
        .u16_type_ident = common.findIdent("Builtin.Num.U16") orelse unreachable,
        .i16_type_ident = common.findIdent("Builtin.Num.I16") orelse unreachable,
        .u32_type_ident = common.findIdent("Builtin.Num.U32") orelse unreachable,
        .i32_type_ident = common.findIdent("Builtin.Num.I32") orelse unreachable,
        .u64_type_ident = common.findIdent("Builtin.Num.U64") orelse unreachable,
        .i64_type_ident = common.findIdent("Builtin.Num.I64") orelse unreachable,
        .u128_type_ident = common.findIdent("Builtin.Num.U128") orelse unreachable,
        .i128_type_ident = common.findIdent("Builtin.Num.I128") orelse unreachable,
        .f32_type_ident = common.findIdent("Builtin.Num.F32") orelse unreachable,
        .f64_type_ident = common.findIdent("Builtin.Num.F64") orelse unreachable,
        .dec_type_ident = common.findIdent("Builtin.Num.Dec") orelse unreachable,
        .before_dot_ident = common.findIdent("before_dot") orelse unreachable,
        .after_dot_ident = common.findIdent("after_dot") orelse unreachable,
        .provided_by_compiler_ident = common.findIdent("ProvidedByCompiler") orelse unreachable,
        .tag_ident = common.findIdent("tag") orelse unreachable,
        .payload_ident = common.findIdent("payload") orelse unreachable,
        .is_negative_ident = common.findIdent("is_negative") orelse unreachable,
        .digits_before_pt_ident = common.findIdent("digits_before_pt") orelse unreachable,
        .digits_after_pt_ident = common.findIdent("digits_after_pt") orelse unreachable,
        .box_method_ident = common.findIdent("box") orelse unreachable,
        .unbox_method_ident = common.findIdent("unbox") orelse unreachable,
        .ok_ident = common.findIdent("Ok") orelse unreachable,
        .err_ident = common.findIdent("Err") orelse unreachable,
        .numeral_relative_ident = common.findIdent("Num.Numeral") orelse unreachable,
        .true_tag_ident = common.findIdent("True") orelse unreachable,
        .false_tag_ident = common.findIdent("False") orelse unreachable,
        .deferred_numeric_literals = try ModuleEnv.DeferredNumericLiteral.SafeList.initCapacity(deser_alloc, 0),
        .import_mapping = types.import_mapping.ImportMapping.init(deser_alloc),
    };

    // Verify the data was preserved
    // try testing.expectEqual(@as(usize, 2), env.ident_ids_for_slicing.len());

    // Verify original data before serialization was correct
    // initCIRFields inserts the module name ("TestModule") into the interner, so we have 3 total: hello, world, TestModule
    // ModuleEnv.init() also interns 19 well-known identifiers: from_int_digits, from_dec_digits, Try, OutOfRange, Builtin, plus, minus, times, div_by, div_trunc_by, rem_by, negate, not, is_lt, is_lte, is_gt, is_gte, is_eq, is_ne
    // Plus 18 type identifiers: Builtin.Try, Builtin.Num.Numeral, Builtin.Str, List, Box, Builtin.Num.{U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, F32, F64, Dec}
    // Plus 3 field/tag identifiers: before_dot, after_dot, ProvidedByCompiler
    // Plus 7 more identifiers: tag, payload, is_negative, digits_before_pt, digits_after_pt, box, unbox
    // Plus 2 Try tag identifiers: Ok, Err
    // Plus 3 more identifiers: Num.Numeral (relative), True, False
    try testing.expectEqual(@as(u32, 55), original.common.idents.interner.entry_count);
    try testing.expectEqualStrings("hello", original.getIdent(hello_idx));
    try testing.expectEqualStrings("world", original.getIdent(world_idx));

    // Verify imports before serialization
    try testing.expectEqual(import1, import3); // Deduplication should work
    try testing.expectEqual(@as(usize, 2), original.imports.imports.len()); // Should have 2 unique imports

    // First verify that the CommonEnv data was preserved after deserialization
    // Should have same 55 identifiers as original: hello, world, TestModule + 19 well-known identifiers + 18 type identifiers + 3 field/tag identifiers + 7 more identifiers + 2 Try tag identifiers + 3 additional (Num.Numeral relative, True, False) from ModuleEnv.init()
    try testing.expectEqual(@as(u32, 55), env.common.idents.interner.entry_count);

    try testing.expectEqual(@as(usize, 1), env.common.exposed_items.count());
    try testing.expectEqual(@as(?u16, 42), env.common.exposed_items.getNodeIndexById(gpa, @as(u32, @bitCast(hello_idx))));

    try testing.expectEqual(@as(usize, 3), env.common.line_starts.len());
    try testing.expectEqual(@as(u32, 0), env.common.line_starts.items.items[0]);
    try testing.expectEqual(@as(u32, 10), env.common.line_starts.items.items[1]);
    try testing.expectEqual(@as(u32, 20), env.common.line_starts.items.items[2]);

    // TODO restore source using CommonEnv
    // try testing.expectEqualStrings(source, env.source);
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

    // Test that deduplication still works after deserialization
    // Use arena allocator for these operations to avoid memory issues
    var test_arena = std.heap.ArenaAllocator.init(gpa);
    defer test_arena.deinit();
    const test_alloc = test_arena.allocator();

    const import4 = try env.imports.getOrPut(test_alloc, &env.common.strings, "json.Json");
    const import5 = try env.imports.getOrPut(test_alloc, &env.common.strings, "new.Module");

    // Should find existing json.Json
    try testing.expectEqual(@as(u32, 0), @intFromEnum(import4));
    // Should create new entry for new.Module
    try testing.expectEqual(@as(u32, 2), @intFromEnum(import5));
    try testing.expectEqual(@as(usize, 3), env.imports.imports.len());
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
//     try original.initCIRFields(gpa, "test.Types");

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
//     try writer.writeGather(arena_alloc, file);

//     // Read back
//     try file.seekTo(0);
//     const file_size = try file.getEndPos();
//     const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
//     defer gpa.free(buffer);

//     _ = try file.read(buffer);

//     // The CommonEnv.Serialized is at the beginning of the buffer
//     const common_serialized_ptr = @as(*base.CommonEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));
//     const deserialized_common = common_serialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), "");

//     // The ModuleEnv.Serialized follows after the CommonEnv.Serialized
//     const module_env_offset = @sizeOf(base.CommonEnv.Serialized);
//     const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + module_env_offset)));

//     // Now manually construct the ModuleEnv using the deserialized CommonEnv
//     const deserialized = @as(*ModuleEnv, @ptrCast(@alignCast(deserialized_ptr)));
//     deserialized.* = ModuleEnv{
//         .gpa = gpa,
//         .common = deserialized_common,
//         .types = deserialized_ptr.types.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr)))).*,
//         .all_defs = deserialized_ptr.all_defs,
//         .all_statements = deserialized_ptr.all_statements,
//         .external_decls = deserialized_ptr.external_decls.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr)))).*,
//         .imports = (try deserialized_ptr.imports.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa)).*,
//         .module_name = "test.Types",
//         .diagnostics = deserialized_ptr.diagnostics,
//         .store = deserialized_ptr.store.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa).*,
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
//     // Module env takes ownership of Common env -- no need to deinit here

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

    // Create a simple ModuleEnv
    var env = try ModuleEnv.init(gpa, "hello");
    defer env.deinit();

    // First add a string literal
    const str_literal_idx = try env.insertString("hello");

    // Create a nominal Str type
    const str_ident = try env.insertIdent(base.Ident.for_text("Str"));
    const builtin_ident = try env.insertIdent(base.Ident.for_text("Builtin"));

    // Create backing type for Str (empty_record as placeholder for the tag union)
    const str_backing_var = try env.types.freshFromContent(.{ .structure = .empty_record });
    const str_vars = [_]types.Var{str_backing_var};
    const str_vars_range = try env.types.appendVars(&str_vars);

    const str_nominal = types.NominalType{
        .ident = types.TypeIdent{ .ident_idx = str_ident },
        .vars = .{ .nonempty = str_vars_range },
        .origin_module = builtin_ident,
    };
    const str_type = try env.types.freshFromContent(.{ .structure = .{ .nominal_type = str_nominal } });

    // Add a string segment expression
    const segment_idx = try env.addExpr(.{ .e_str_segment = .{ .literal = str_literal_idx } }, base.Region.from_raw_offsets(0, 5));
    _ = str_type;

    // Now create a string expression that references the segment
    const expr_idx = try env.addExpr(.{ .e_str = .{ .span = Expr.Span{ .span = base.DataSpan{ .start = @intFromEnum(segment_idx), .len = 1 } } } }, base.Region.from_raw_offsets(0, 5));
    _ = try env.types.freshFromContent(.{ .structure = .{ .nominal_type = str_nominal } });

    // Create an S-expression tree
    var tree = base.SExprTree.init(gpa);
    defer tree.deinit();

    // Call pushExprTypesToSExprTree (which is called by pushTypesToSExprTree)
    try env.pushTypesToSExprTree(expr_idx, &tree);

    // Convert tree to string
    var result = std.ArrayList(u8).empty;
    defer result.deinit(gpa);
    try tree.toStringPretty(result.writer(gpa).any(), .include_linecol);

    // Verify the output contains the type information
    const result_str = result.items;

    try testing.expect(std.mem.indexOf(u8, result_str, "(expr") != null);
    try testing.expect(std.mem.indexOf(u8, result_str, "(type") != null);
    try testing.expect(std.mem.indexOf(u8, result_str, "Str") != null);
}
