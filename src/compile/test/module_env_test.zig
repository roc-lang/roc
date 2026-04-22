//! Tests for ModuleEnv
const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const types = @import("types");
const builtins = @import("builtins");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");
const eval = @import("eval");

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
    try original.initCIRFields("TestModule");

    // Add some imports to test serialization/deserialization
    const import1 = try original.imports.getOrPut(gpa, &original.common.strings, "json.Json");
    const import2 = try original.imports.getOrPut(gpa, &original.common.strings, "core.List");
    const import3 = try original.imports.getOrPut(gpa, &original.common.strings, "json.Json"); // duplicate - should return same as import1

    try testing.expect(import2 != import1);

    // First add to exposed items, then set node index
    try original.addExposedById(hello_idx);
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
    const bytes_read = try tmp_file.pread(buffer, 0);
    try std.testing.expectEqual(@as(usize, @intCast(file_size)), bytes_read);

    const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));

    // Create an arena for deserialization to avoid memory leaks
    var deser_arena = std.heap.ArenaAllocator.init(gpa);
    defer deser_arena.deinit();
    const deser_alloc = deser_arena.allocator();

    // Now manually construct the ModuleEnv using the deserialized CommonEnv
    const env = @as(*ModuleEnv, @ptrCast(@alignCast(deserialized_ptr)));

    // Deserialize common env first so we can look up identifiers
    const common = deserialized_ptr.common.deserializeInto(@intFromPtr(buffer.ptr), source);

    env.* = ModuleEnv{
        .gpa = gpa,
        .common = common,
        .types = deserialized_ptr.types.deserializeInto(@intFromPtr(buffer.ptr), gpa),
        .module_kind = deserialized_ptr.module_kind.decode(),
        .all_defs = deserialized_ptr.all_defs,
        .all_statements = deserialized_ptr.all_statements,
        .exports = deserialized_ptr.exports,
        .requires_types = deserialized_ptr.requires_types.deserializeInto(@intFromPtr(buffer.ptr)),
        .for_clause_aliases = deserialized_ptr.for_clause_aliases.deserializeInto(@intFromPtr(buffer.ptr)),
        .provides_entries = deserialized_ptr.provides_entries.deserializeInto(@intFromPtr(buffer.ptr)),
        .builtin_statements = deserialized_ptr.builtin_statements,
        .external_decls = deserialized_ptr.external_decls.deserializeInto(@intFromPtr(buffer.ptr)),
        .imports = try deserialized_ptr.imports.deserializeInto(@intFromPtr(buffer.ptr), deser_alloc),
        .module_name = "TestModule",
        .display_module_name_idx = base.Ident.Idx.NONE, // Not used for deserialized modules (only needed during fresh canonicalization)
        .qualified_module_ident = base.Ident.Idx.NONE,
        .diagnostics = deserialized_ptr.diagnostics,
        .store = deserialized_ptr.store.deserializeInto(@intFromPtr(buffer.ptr), deser_alloc),
        .evaluation_order = null,
        .idents = ModuleEnv.CommonIdents.find(&common),
        .import_mapping = types.import_mapping.ImportMapping.init(deser_alloc),
        .method_idents = deserialized_ptr.method_idents.deserializeInto(@intFromPtr(buffer.ptr)),
        .rigid_vars = std.AutoHashMapUnmanaged(base.Ident.Idx, types.Var){},
    };

    // Verify original data before serialization was correct
    // initCIRFields inserts the module name ("TestModule") into the interner, so we have 3 total: hello, world, TestModule
    // ModuleEnv.init() also interns 16 well-known identifiers: Try, OutOfRange, Builtin, plus, minus, times, div_by, div_trunc_by, rem_by, negate, not, is_lt, is_lte, is_gt, is_gte, is_eq
    // Plus 19 type identifiers: Str, Builtin.Try, Builtin.Num.Numeral, Builtin.Str, List, Box, Builtin.Num.{U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, F32, F64, Dec}
    // Plus 3 field/tag identifiers: before_dot, after_dot, ProvidedByCompiler
    // Plus 7 more identifiers: tag, payload, is_negative, digits_before_pt, digits_after_pt, box, unbox
    // Plus 2 Try tag identifiers: Ok, Err
    // Plus 1 method identifier: from_numeral
    // Plus 2 Bool tag identifiers: True, False
    // Plus 6 from_utf8 identifiers: byte_index, string, is_ok, problem_code, problem, index
    // Plus 2 synthetic identifiers for ? operator desugaring: #ok, #err
    // Plus 1 synthetic identifier for .. implicit rigids in open tag unions or records
    // Plus 2 numeric method identifiers: abs, abs_diff
    // Plus 1 inspect method identifier: to_inspect
    // Plus 15 unqualified builtin type names: Num, Bool, U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec
    // Plus 2 fully qualified Box intrinsic method names: Builtin.Box.box, Builtin.Box.unbox
    // Plus 1 fully qualified Bool type name: Builtin.Bool
    const expected_ident_count = original.common.idents.interner.entry_count;
    try testing.expectEqualStrings("hello", original.getIdent(hello_idx));
    try testing.expectEqualStrings("world", original.getIdent(world_idx));

    // Verify imports before serialization
    try testing.expectEqual(import1, import3); // Deduplication should work
    try testing.expectEqual(@as(usize, 2), original.imports.imports.len()); // Should have 2 unique imports

    // First verify that the CommonEnv data was preserved after deserialization
    // Should have same 81 identifiers as original: hello, world, TestModule + 16 well-known identifiers + 19 type identifiers + 3 field/tag identifiers + 7 more identifiers + 2 Try tag identifiers + 1 method identifier + 2 Bool tag identifiers + 6 from_utf8 identifiers + 2 synthetic identifiers for ? operator desugaring + 2 numeric method identifiers (abs, abs_diff) + 1 inspect method identifier (to_inspect) + 15 unqualified builtin type names from ModuleEnv.init() (Num, Bool, U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec) + 2 fully qualified Box intrinsic method names (Builtin.Box.box, Builtin.Box.unbox) + 1 fully qualified Bool type name (Builtin.Bool)
    // (Note: "Try" is now shared with well-known identifiers, reducing total by 1)
    try testing.expectEqual(expected_ident_count, env.common.idents.interner.entry_count);

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
//     try writer.writeGather(arena_alloc, file);

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
//     try writer.writeGather(arena_alloc, file);

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
//     try writer.writeGather(arena_alloc, file);

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
        .is_opaque = false,
    };
    // Add a string segment expression
    const segment_idx = try env.addExpr(.{ .e_str_segment = .{ .literal = str_literal_idx } }, base.Region.from_raw_offsets(0, 5));

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

test "ModuleEnv serialization and interpreter evaluation" {
    // This test demonstrates that a ModuleEnv can be successfully:
    // 1. Created and used with the Interpreter to evaluate expressions
    // 2. Serialized to bytes and written to disk
    // 3. Deserialized from those bytes read back from disk
    // 4. Used with a new Interpreter to evaluate the same expressions with identical results
    //
    // This verifies the complete round-trip of compilation state preservation
    // through serialization, which is critical for incremental compilation
    // and distributed build systems.
    //
    const testing = std.testing;
    const gpa = std.heap.smp_allocator;
    const source = "5 + 8";
    const module_source = try std.fmt.allocPrint(gpa, "main = {s}", .{source});
    defer gpa.free(module_source);
    const builtin_loading = eval.builtin_loading;
    const EvalInterpreter = eval.Interpreter;
    const EvalTestEnv = eval.TestEnv;
    const eval_pipeline = eval.pipeline;

    // Load builtin module
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source);
    defer builtin_module.deinit();

    const checked_module = try eval_pipeline.parseCheckModule(
        gpa,
        "TestModule",
        .expr,
        source,
        false,
        true,
        builtin_module.env,
        builtin_indices,
        &.{},
    );
    defer eval_pipeline.cleanupCheckedModule(gpa, checked_module);

    const original_env = checked_module.module_env;

    // Test 1: Evaluate with the original ModuleEnv via LIR pipeline
    {
        const all_module_envs = [_]*const ModuleEnv{ original_env, builtin_module.env };
        const source_modules = [_]check.TypedCIR.Modules.SourceModule{
            .{ .precompiled = original_env },
            .{ .precompiled = builtin_module.env },
        };
        var typed_cir_modules = try check.TypedCIR.Modules.init(gpa, &source_modules);
        defer typed_cir_modules.deinit();
        var lowered = try eval_pipeline.lowerTypedCIRToLir(gpa, &typed_cir_modules, &all_module_envs);
        defer lowered.deinit();
        var test_env = EvalTestEnv.init(gpa);
        defer test_env.deinit();

        var interp = try EvalInterpreter.init(gpa, &lowered.lir_result.store, &lowered.lir_result.layouts, test_env.get_ops());
        defer interp.deinit();
        const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
        const eval_result = try interp.eval(.{
            .proc_id = lowered.main_proc,
            .arg_layouts = &.{},
            .ret_layout = proc.ret_layout,
        });
        const value = switch (eval_result) {
            .value => |v| v,
        };

        // Read result — `5 + 8` produces a Dec (i128) via the default Num type
        const int_value = blk: {
            const lay = lowered.lir_result.layouts.getLayout(proc.ret_layout);
            if (lay.tag == .scalar and lay.data.scalar.tag == .int) {
                break :blk value.read(i128);
            } else {
                // Dec: raw i128 divided by one_point_zero
                const raw = value.read(i128);
                break :blk @divTrunc(raw, builtins.dec.RocDec.one_point_zero_i128);
            }
        };

        interp.dropValue(value, proc.ret_layout);
        try test_env.checkForLeaks();

        try testing.expectEqual(@as(i128, 13), int_value);
    }

    // Test 2: Full serialization and deserialization with interpreter evaluation
    {
        var serialization_arena = std.heap.ArenaAllocator.init(gpa);
        defer serialization_arena.deinit();
        const arena_alloc = serialization_arena.allocator();

        var tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const tmp_file = try tmp_dir.dir.createFile("test_module_env.compact", .{ .read = true });
        defer tmp_file.close();

        var writer = CompactWriter.init();
        defer writer.deinit(arena_alloc);

        // Allocate space for ModuleEnv.Serialized (NOT ModuleEnv!) and serialize
        // IMPORTANT: ModuleEnv.Serialized may be larger than ModuleEnv. Allocating only
        // @sizeOf(ModuleEnv) bytes causes a buffer overflow that corrupts subsequent data.
        const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
        const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv.Serialized);
        const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
        try serialized_ptr.serialize(original_env, arena_alloc, &writer);

        // Write to file
        try writer.writeGather(arena_alloc, tmp_file);

        // Read back from file
        const file_size = try tmp_file.getEndPos();
        const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ModuleEnv)), @intCast(file_size));
        defer gpa.free(buffer);
        const read_len = try tmp_file.pread(buffer, 0);
        try testing.expectEqual(buffer.len, read_len);

        // Deserialize the ModuleEnv
        const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + env_start_offset)));
        const deserialized_env = try deserialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa, module_source, "TestModule");
        // Free the heap-allocated ModuleEnv and its imports map
        defer {
            deserialized_env.imports.deinitMapOnly(gpa);
            deserialized_env.import_mapping.deinit();
            deserialized_env.rigid_vars.deinit(gpa);
            deserialized_env.common.idents.interner.deinit(gpa);
            gpa.destroy(deserialized_env);
        }

        // Verify basic deserialization worked
        try testing.expectEqualStrings("TestModule", deserialized_env.module_name);
        try testing.expectEqualStrings(module_source, deserialized_env.common.source);

        // Test 3: Verify the deserialized ModuleEnv has the correct structure
        try testing.expect(deserialized_env.types.len() > 0);
        try testing.expect(deserialized_env.store.nodes.items.len > 0);

        // Verify that the deserialized data matches the original data
        try testing.expectEqual(original_env.types.len(), deserialized_env.types.len());
        try testing.expectEqual(original_env.store.nodes.items.len, deserialized_env.store.nodes.items.len);
        try testing.expectEqual(original_env.common.idents.interner.bytes.len(), deserialized_env.common.idents.interner.bytes.len());

        // Test 4: Evaluate the same expression using the deserialized ModuleEnv
        // The original expression index should still be valid since the NodeStore structure is preserved
        {
            // Enable runtime inserts on all deserialized interners so the interpreter can add new idents.
            // Both the test module and the builtin module were deserialized (via loadCompiledModule).
            try deserialized_env.common.idents.interner.enableRuntimeInserts(gpa);
            try @constCast(builtin_module.env).common.idents.interner.enableRuntimeInserts(gpa);

            // Fix up display_module_name_idx and qualified_module_ident for deserialized modules (critical for method dispatch).
            // Deserialized modules have display_module_name_idx set to NONE - we need to re-intern the name.
            if (deserialized_env.display_module_name_idx.isNone() and deserialized_env.module_name.len > 0) {
                deserialized_env.display_module_name_idx = try deserialized_env.insertIdent(base.Ident.for_text(deserialized_env.module_name));
                deserialized_env.qualified_module_ident = deserialized_env.display_module_name_idx;
            }
            if (builtin_module.env.display_module_name_idx.isNone() and builtin_module.env.module_name.len > 0) {
                @constCast(builtin_module.env).display_module_name_idx = try @constCast(builtin_module.env).insertIdent(base.Ident.for_text(builtin_module.env.module_name));
                @constCast(builtin_module.env).qualified_module_ident = builtin_module.env.display_module_name_idx;
            }

            const all_module_envs2 = [_]*const ModuleEnv{ deserialized_env, builtin_module.env };
            const source_modules2 = [_]check.TypedCIR.Modules.SourceModule{
                .{ .precompiled = deserialized_env },
                .{ .precompiled = builtin_module.env },
            };
            var typed_cir_modules2 = try check.TypedCIR.Modules.init(gpa, &source_modules2);
            defer typed_cir_modules2.deinit();
            var lowered2 = try eval_pipeline.lowerTypedCIRToLir(gpa, &typed_cir_modules2, &all_module_envs2);
            defer lowered2.deinit();
            var test_env2 = EvalTestEnv.init(gpa);
            defer test_env2.deinit();

            var interp2 = try EvalInterpreter.init(gpa, &lowered2.lir_result.store, &lowered2.lir_result.layouts, test_env2.get_ops());
            defer interp2.deinit();
            const proc2 = lowered2.lir_result.store.getProcSpec(lowered2.main_proc);
            const eval_result2 = try interp2.eval(.{
                .proc_id = lowered2.main_proc,
                .arg_layouts = &.{},
                .ret_layout = proc2.ret_layout,
            });
            const value2 = switch (eval_result2) {
                .value => |v| v,
            };

            // Verify we get the same result from the deserialized ModuleEnv
            const int_value = blk: {
                const lay = lowered2.lir_result.layouts.getLayout(proc2.ret_layout);
                if (lay.tag == .scalar and lay.data.scalar.tag == .int) {
                    break :blk value2.read(i128);
                } else {
                    const raw = value2.read(i128);
                    break :blk @divTrunc(raw, builtins.dec.RocDec.one_point_zero_i128);
                }
            };

            interp2.dropValue(value2, proc2.ret_layout);
            try test_env2.checkForLeaks();

            try testing.expectEqual(@as(i128, 13), int_value);
        }
    }
}
