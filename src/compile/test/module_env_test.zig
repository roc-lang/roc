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

const relocated_numeral_digit_bytes = [_]u8{ 0x01, 0x64, 0x14, 0x32, 0xfe, 0xdc };
const relocated_numeral_literals = [_]ModuleEnv.NumeralLiteral{
    .{
        .node_idx = 7,
        .digits_start = 0,
        .before_len = 2,
        .after_len = 2,
        .after_decimal_digit_count = 4,
        .flags = ModuleEnv.NumeralLiteral.fractional_flag |
            ModuleEnv.NumeralLiteral.decimal_point_flag,
    },
    .{
        .node_idx = 11,
        .digits_start = 4,
        .before_len = 2,
        .after_len = 0,
        .after_decimal_digit_count = 0,
        .flags = ModuleEnv.NumeralLiteral.negative_flag |
            ModuleEnv.NumeralLiteral.materialized_flag,
    },
};
const relocated_numeric_suffix_targets = [_]ModuleEnv.NumericSuffixTarget{
    .{
        .node_idx = 7,
        .kind = @intFromEnum(ModuleEnv.NumericSuffixTarget.Kind.builtin),
        .data1 = @intFromEnum(CIR.NumKind.u64),
        .data2 = 0,
    },
    .{
        .node_idx = 11,
        .kind = @intFromEnum(ModuleEnv.NumericSuffixTarget.Kind.external),
        .data1 = 3,
        .data2 = 47,
    },
};
const relocated_generated_codec_calls = [_]ModuleEnv.GeneratedCodecCall{
    .{
        .method_ident = 101,
        .dispatcher_var = 102,
        .callable_var = 103,
        .evidence_var = 104,
        .subject_var = 105,
    },
    .{
        .method_ident = 201,
        .dispatcher_var = 202,
        .callable_var = 203,
        .evidence_var = 204,
        .subject_var = ModuleEnv.GeneratedCodecCall.no_subject_var,
    },
};
const relocated_generated_codec_derivations = [_]ModuleEnv.GeneratedCodecDerivation{
    .{
        .kind = @intFromEnum(ModuleEnv.GeneratedCodecDerivation.Kind.parser),
        .source_constraint_fn_var = 301,
        .source_runtime_fn_var = 302,
        .source_shape_var = 303,
        .source_encoding_var = 304,
        .source_state_var = 305,
        .source_error_var = 306,
        .constraint_fn_var = 311,
        .runtime_fn_var = 312,
        .shape_var = 313,
        .encoding_var = 314,
        .state_var = 315,
        .error_var = 316,
        .calls_start = 0,
        .calls_len = 1,
    },
    .{
        .kind = @intFromEnum(ModuleEnv.GeneratedCodecDerivation.Kind.encoder),
        .source_constraint_fn_var = 401,
        .source_runtime_fn_var = 402,
        .source_shape_var = 403,
        .source_encoding_var = 404,
        .source_state_var = 405,
        .source_error_var = 406,
        .constraint_fn_var = 411,
        .runtime_fn_var = 412,
        .shape_var = 413,
        .encoding_var = 414,
        .state_var = 415,
        .error_var = 416,
        .calls_start = 1,
        .calls_len = 1,
    },
};

fn populateRelocatedNumericAndCodecTables(env: *ModuleEnv, gpa: std.mem.Allocator) !void {
    _ = try env.numeral_digit_bytes.appendSlice(gpa, &relocated_numeral_digit_bytes);
    _ = try env.numeral_literals.appendSlice(gpa, &relocated_numeral_literals);
    _ = try env.numeric_suffix_targets.appendSlice(gpa, &relocated_numeric_suffix_targets);
    _ = try env.generated_codec_derivations.appendSlice(gpa, &relocated_generated_codec_derivations);
    _ = try env.generated_codec_calls.appendSlice(gpa, &relocated_generated_codec_calls);
}

fn expectRelocatedNumericAndCodecTables(env: *const ModuleEnv) !void {
    try testing.expectEqualSlices(u8, &relocated_numeral_digit_bytes, env.numeral_digit_bytes.items.items);
    try testing.expectEqualSlices(ModuleEnv.NumeralLiteral, &relocated_numeral_literals, env.numeral_literals.items.items);
    try testing.expectEqualSlices(ModuleEnv.NumericSuffixTarget, &relocated_numeric_suffix_targets, env.numeric_suffix_targets.items.items);
    try testing.expectEqualSlices(ModuleEnv.GeneratedCodecDerivation, &relocated_generated_codec_derivations, env.generated_codec_derivations.items.items);
    try testing.expectEqualSlices(ModuleEnv.GeneratedCodecCall, &relocated_generated_codec_calls, env.generated_codec_calls.items.items);
}

test "ModuleEnv relocation moves every numeric and generated-codec serialized list" {
    const gpa = testing.allocator;
    const source = "";

    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();
    try original.initCIRFields("Test");
    try populateRelocatedNumericAndCodecTables(&original, gpa);

    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();
    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);
    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(&original, arena_alloc, &writer);

    const original_buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        writer.total_bytes,
    );
    defer gpa.free(original_buffer);
    _ = try writer.writeToBuffer(original_buffer);
    const original_base = @intFromPtr(original_buffer.ptr);
    const original_serialized: *const ModuleEnv.Serialized = @ptrCast(@alignCast(original_buffer.ptr));
    try original_serialized.validate(original_buffer.len);

    var relocated_env = try original_serialized.viewStatic(original_base, gpa, source, "");
    try expectRelocatedNumericAndCodecTables(&relocated_env);

    const relocated_buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        original_buffer.len,
    );
    defer gpa.free(relocated_buffer);
    @memcpy(relocated_buffer, original_buffer);
    const relocated_base = @intFromPtr(relocated_buffer.ptr);
    const offset = @as(isize, @intCast(relocated_base)) - @as(isize, @intCast(original_base));
    relocated_env.relocate(offset);

    const relocated_serialized: *const ModuleEnv.Serialized = @ptrCast(@alignCast(relocated_buffer.ptr));
    try testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.numeral_digit_bytes.offset)),
        @intFromPtr(relocated_env.numeral_digit_bytes.items.items.ptr),
    );
    try testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.numeral_literals.offset)),
        @intFromPtr(relocated_env.numeral_literals.items.items.ptr),
    );
    try testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.numeric_suffix_targets.offset)),
        @intFromPtr(relocated_env.numeric_suffix_targets.items.items.ptr),
    );
    try testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.generated_codec_derivations.offset)),
        @intFromPtr(relocated_env.generated_codec_derivations.items.items.ptr),
    );
    try testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.generated_codec_calls.offset)),
        @intFromPtr(relocated_env.generated_codec_calls.items.items.ptr),
    );
    try expectRelocatedNumericAndCodecTables(&relocated_env);
}

test "ModuleEnv numeric and generated-codec tables survive static and mutable roundtrips" {
    const gpa = testing.allocator;
    const source = "";

    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();
    try original.initCIRFields("Test");
    try populateRelocatedNumericAndCodecTables(&original, gpa);

    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();
    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);
    const serialized = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    try serialized.serialize(&original, arena_alloc, &writer);

    const buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        writer.total_bytes,
    );
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const base_addr = @intFromPtr(buffer.ptr);
    const serialized_ptr: *const ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized_ptr.validate(buffer.len);

    const static_env = try serialized_ptr.viewStatic(base_addr, gpa, source, "Test");
    try expectRelocatedNumericAndCodecTables(&static_env);

    const mutable_env = try serialized_ptr.deserializeWithMutableTypes(
        base_addr,
        gpa,
        source,
        "Test",
    );
    defer {
        mutable_env.deinitCachedModule();
        gpa.destroy(mutable_env);
    }
    try expectRelocatedNumericAndCodecTables(mutable_env);
}

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
    try original.recordRejectedStaticDispatch(@enumFromInt(1234));
    try original.recordBindingScheme(@enumFromInt(42));
    try original.recordBindingScheme(@enumFromInt(7));
    try original.recordBindingScheme(@enumFromInt(42));
    try original.recordBindingSchemeCodecRequirement(@enumFromInt(42), @enumFromInt(90), @enumFromInt(12), 34);
    try original.recordBindingSchemeCodecRequirement(@enumFromInt(7), @enumFromInt(91), @enumFromInt(56), 78);
    try original.recordBindingSchemeCodecRequirement(@enumFromInt(42), @enumFromInt(90), @enumFromInt(13), 35);
    try original.recordBindingSchemeCodecRequirement(@enumFromInt(42), @enumFromInt(90), @enumFromInt(12), 34);
    _ = try original.where_marker_constraint_move_offsets.append(gpa, 1);
    _ = try original.where_marker_constraint_move_offsets.append(gpa, 0);
    _ = try original.where_marker_constraint_moves.append(gpa, .{
        .source_constraint_index = 12,
        .destination_constraint_index = 34,
        .offsets_start = 0,
        .offsets_len = 2,
    });
    original.top_level_value_defs = .{ .span = .{ .start = 17, .len = 2 } };
    original.value_binding_defs = .{ .span = .{ .start = 23, .len = 4 } };
    _ = try original.provided_low_level_defs.append(gpa, .{
        .def_idx = 7,
        .op = .num_int_add_wrap,
    });
    _ = try original.provided_low_level_defs.append(gpa, .{
        .def_idx = 11,
        .op = .num_bitwise_xor,
    });
    const body_annotation_attachments = [_]ModuleEnv.BodyAnnotationAttachment{
        .{
            .attachment_kind = @intFromEnum(ModuleEnv.BodyAnnotationAttachment.AttachmentKind.top_level_def),
            .attachment_node = 21,
            .annotation_root = 22,
            .body_expr = 23,
        },
        .{
            .attachment_kind = @intFromEnum(ModuleEnv.BodyAnnotationAttachment.AttachmentKind.local_decl),
            .attachment_node = 31,
            .annotation_root = 32,
            .body_expr = 33,
        },
    };
    for (body_annotation_attachments) |attachment| {
        _ = try original.body_annotation_attachments.append(gpa, attachment);
    }
    const body_annotation_malformed_type_publications = [_]ModuleEnv.BodyAnnotationMalformedTypePublication{
        .{
            .attachment_kind = @intFromEnum(ModuleEnv.BodyAnnotationMalformedTypePublication.AttachmentKind.top_level_def),
            .attachment_node = 21,
            .annotation_root = 22,
            .body_expr = 23,
            .malformed_type_publication_index = 0,
        },
        .{
            .attachment_kind = @intFromEnum(ModuleEnv.BodyAnnotationMalformedTypePublication.AttachmentKind.local_decl),
            .attachment_node = 31,
            .annotation_root = 32,
            .body_expr = 33,
            .malformed_type_publication_index = 1,
        },
    };
    for (body_annotation_malformed_type_publications) |publication| {
        _ = try original.body_annotation_malformed_type_publications.append(gpa, publication);
    }

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
    try deserialized_ptr.validate(buffer.len);
    try std.testing.expectEqual(
        @intFromEnum(ModuleEnv.TypecheckState.canonical_unchecked),
        deserialized_ptr.typecheck_state,
    );
    var corrupt_serialized = deserialized_ptr.*;
    corrupt_serialized.common.idents.interner.bytes.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt_serialized.validate(buffer.len));
    corrupt_serialized = deserialized_ptr.*;
    corrupt_serialized.typecheck_state = std.math.maxInt(u8);
    try std.testing.expectError(error.CorruptArtifact, corrupt_serialized.validate(buffer.len));
    corrupt_serialized = deserialized_ptr.*;
    corrupt_serialized.typecheck_state_padding[2] = 1;
    try std.testing.expectError(error.CorruptArtifact, corrupt_serialized.validate(buffer.len));
    corrupt_serialized = deserialized_ptr.*;
    corrupt_serialized.body_annotation_attachments.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt_serialized.validate(buffer.len));
    corrupt_serialized = deserialized_ptr.*;
    corrupt_serialized.body_annotation_malformed_type_publications.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt_serialized.validate(buffer.len));

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
    try std.testing.expectEqual(ModuleEnv.TypecheckState.canonical_unchecked, env.typecheck_state);

    try std.testing.expectEqual(@as(usize, 2), env.imports.imports.len());
    try std.testing.expectEqualStrings("json.Json", env.common.strings.get(env.imports.imports.items.items[0]));
    try std.testing.expectEqualStrings("core.List", env.common.strings.get(env.imports.imports.items.items[1]));
    try std.testing.expectEqual(@as(usize, 2), env.imports.map.count());
    try std.testing.expectEqual(@as(usize, 1), env.rejectedStaticDispatches().len);
    try std.testing.expectEqual(@as(types.Var, @enumFromInt(1234)), env.rejectedStaticDispatches()[0].fnVar());
    try std.testing.expect(env.nodeIsBindingScheme(@enumFromInt(7)));
    try std.testing.expect(env.nodeIsBindingScheme(@enumFromInt(42)));
    try std.testing.expect(!env.nodeIsBindingScheme(@enumFromInt(41)));
    try std.testing.expectEqual(@as(usize, 2), env.binding_schemes.items.items.len);
    const codec_requirements_7 = env.bindingSchemeCodecRequirementsForNode(@enumFromInt(7));
    try std.testing.expectEqual(@as(usize, 1), codec_requirements_7.len);
    try std.testing.expectEqual(@as(u32, 91), codec_requirements_7[0].scheme_root);
    try std.testing.expectEqual(@as(u32, 56), codec_requirements_7[0].receiver_var);
    try std.testing.expectEqual(@as(u32, 78), codec_requirements_7[0].constraint_index);
    const codec_requirements_42 = env.bindingSchemeCodecRequirementsForNode(@enumFromInt(42));
    try std.testing.expectEqual(@as(usize, 2), codec_requirements_42.len);
    try std.testing.expectEqual(@as(u32, 90), codec_requirements_42[0].scheme_root);
    try std.testing.expectEqual(@as(u32, 12), codec_requirements_42[0].receiver_var);
    try std.testing.expectEqual(@as(u32, 34), codec_requirements_42[0].constraint_index);
    try std.testing.expectEqual(@as(u32, 13), codec_requirements_42[1].receiver_var);
    try std.testing.expectEqual(@as(u32, 35), codec_requirements_42[1].constraint_index);
    try std.testing.expectEqual(@as(usize, 0), env.bindingSchemeCodecRequirementsForNode(@enumFromInt(41)).len);
    try std.testing.expectEqualSlices(
        ModuleEnv.WhereMarkerConstraintMove,
        original.where_marker_constraint_moves.items.items,
        env.where_marker_constraint_moves.items.items,
    );
    try std.testing.expectEqualSlices(
        u32,
        original.where_marker_constraint_move_offsets.items.items,
        env.where_marker_constraint_move_offsets.items.items,
    );
    try std.testing.expectEqual(original.top_level_value_defs.span, env.top_level_value_defs.span);
    try std.testing.expectEqual(original.value_binding_defs.span, env.value_binding_defs.span);
    try std.testing.expectEqual(base.LowLevel.num_int_add_wrap, env.providedLowLevelForDef(@enumFromInt(7)).?);
    try std.testing.expectEqual(base.LowLevel.num_bitwise_xor, env.providedLowLevelForDef(@enumFromInt(11)).?);
    try std.testing.expect(env.providedLowLevelForDef(@enumFromInt(9)) == null);

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
    // Plus 1 synthetic identifier for rigid presence vars of ?: optional fields
    // Plus 1 error tag for optional field access on an absent field: MissingField
    // Plus 2 numeric method identifiers: abs, abs_diff
    // Plus 1 inspect method identifier: to_inspect
    // Plus 23 unqualified builtin type names: Num, Bool, U8, U16, U32, U64, U128, I8, I16, I32, I64, I128, F32, F64, Dec,
    // and the eight integer SIMD vector types.
    // Plus 2 fully qualified builtin type names: Builtin.List, Builtin.Box
    // Plus 2 fully qualified Box intrinsic method names: Builtin.Box.box, Builtin.Box.unbox
    // Plus 1 fully qualified Bool type name: Builtin.Bool
    // Plus 4 fully qualified Crypto builtin type names: SHA256/BLAKE3 Digest and Hasher
    // Plus the fully qualified Range type name and 2 range method identifiers:
    // Builtin.Num.Range, range_exclusive_to, range_inclusive_to
    // Count reflects the merged builtin set, including structural parser/encoder
    // method identifiers, Builtin.Json.Encoding's parse/encode helpers, and Crypto.
    try testing.expectEqual(@as(u32, 122), original.common.idents.interner.entry_count);
    try testing.expectEqualStrings("hello", original.getIdent(hello_idx));
    try testing.expectEqualStrings("world", original.getIdent(world_idx));

    // Verify imports before serialization
    try testing.expectEqual(import_json, import_json_duplicate); // Deduplication should work
    try testing.expectEqual(@as(usize, 2), original.imports.imports.len()); // Should have 2 unique imports

    // First verify that the CommonEnv data was preserved after deserialization
    // Should have same identifiers as original, including the builtin structural method identifiers.
    // (Note: "Try" is now shared with well-known identifiers, reducing total by 1)
    try testing.expectEqual(@as(u32, 122), env.common.idents.interner.entry_count);

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
    try testing.expectEqualSlices(
        ModuleEnv.BodyAnnotationAttachment,
        &body_annotation_attachments,
        env.body_annotation_attachments.items.items,
    );
    try testing.expectEqualSlices(
        ModuleEnv.BodyAnnotationMalformedTypePublication,
        &body_annotation_malformed_type_publications,
        env.body_annotation_malformed_type_publications.items.items,
    );

    // The deserialized StringLiteral.Store is immutable, so cached import lookup
    // uses the serialized string index rather than reinterning its bytes.
    const json_string_idx = env.imports.imports.items.items[0];
    const import_json_cached = env.imports.map.get(json_string_idx).?;
    try testing.expectEqual(@as(u32, 0), @intFromEnum(import_json_cached));
}

test "ModuleEnv generalized dispatch target shares survive static and mutable roundtrips" {
    const gpa = std.testing.allocator;
    const source = "";

    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();
    try original.initCIRFields("Test");
    const method = try original.insertIdent(Ident.for_text("render"));

    const shares = [_]ModuleEnv.GeneralizedDispatchTargetShare{
        .{
            .receiver_var = 11,
            .method_ident = @bitCast(method),
            .omitted_fn_var = 21,
            .retained_fn_var = 22,
            .proof_fn_var = 21,
            .proof_kind = @intFromEnum(ModuleEnv.GeneralizedDispatchTargetShare.ProofKind.shape_only),
        },
        .{
            .receiver_var = 31,
            .method_ident = @bitCast(method),
            .omitted_fn_var = 41,
            .retained_fn_var = 42,
            .proof_fn_var = 43,
            .proof_kind = @intFromEnum(ModuleEnv.GeneralizedDispatchTargetShare.ProofKind.where_method_use),
        },
    };
    for (shares) |share| {
        _ = try original.generalized_dispatch_target_shares.append(gpa, share);
    }

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
    const serialized_ptr: *const ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized_ptr.validate(buffer.len);

    var corrupt = serialized_ptr.*;
    corrupt.generalized_dispatch_target_shares.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));

    // The static Builtin-style view aliases the serialized bytes and preserves
    // every raw witness without allocating a replacement list.
    const static_env = try serialized_ptr.viewStatic(
        @intFromPtr(buffer.ptr),
        gpa,
        source,
        "Test",
    );
    try std.testing.expectEqualSlices(
        ModuleEnv.GeneralizedDispatchTargetShare,
        &shares,
        static_env.generalized_dispatch_target_shares.items.items,
    );

    const mutable_env = try serialized_ptr.deserializeWithMutableTypes(
        @intFromPtr(buffer.ptr),
        gpa,
        source,
        "Test",
    );
    defer {
        mutable_env.deinitCachedModule();
        gpa.destroy(mutable_env);
    }
    try std.testing.expectEqualSlices(
        ModuleEnv.GeneralizedDispatchTargetShare,
        &shares,
        mutable_env.generalized_dispatch_target_shares.items.items,
    );

    // Mutable materialization owns a copied table: runtime preparation can
    // mutate it without changing either the frozen buffer or its static view.
    _ = try mutable_env.generalized_dispatch_target_shares.append(gpa, .{
        .receiver_var = 51,
        .method_ident = @bitCast(method),
        .omitted_fn_var = 61,
        .retained_fn_var = 62,
        .proof_fn_var = 61,
        .proof_kind = @intFromEnum(ModuleEnv.GeneralizedDispatchTargetShare.ProofKind.shape_only),
    });
    try std.testing.expectEqual(@as(usize, 3), mutable_env.generalized_dispatch_target_shares.items.items.len);
    try std.testing.expectEqual(@as(usize, 2), static_env.generalized_dispatch_target_shares.items.items.len);
    try std.testing.expectEqualSlices(
        ModuleEnv.GeneralizedDispatchTargetShare,
        &shares,
        static_env.generalized_dispatch_target_shares.items.items,
    );
}

test "ModuleEnv W6b method output and widening tables survive static and mutable roundtrips" {
    const gpa = std.testing.allocator;
    const source = "";

    var original = try ModuleEnv.init(gpa, source);
    defer original.deinit();
    try original.initCIRFields("Test");

    const paths = [_]ModuleEnv.WhereMethodMarkerPathStep{
        .{ .kind = @intFromEnum(ModuleEnv.WhereMethodMarkerPathStep.Kind.fn_ret), .index = 0, .arity = 1, .name = 0, .origin_module = 0, .source_decl = 0 },
        .{ .kind = @intFromEnum(ModuleEnv.WhereMethodMarkerPathStep.Kind.fn_ret), .index = 0, .arity = 1, .name = 0, .origin_module = 0, .source_decl = 0 },
        .{ .kind = @intFromEnum(ModuleEnv.WhereMethodMarkerPathStep.Kind.nominal_arg), .index = 0, .arity = 2, .name = @bitCast(original.idents.builtin_try), .origin_module = 1, .source_decl = 1 },
    };
    for (paths) |path| _ = try original.method_output_row_path_steps.append(gpa, path);
    const where_alias_expansions = [_]ModuleEnv.WhereAliasExpansion{
        .{ .alias_decl_node = 1, .source_where_node = 2, .method_ident = 3, .retained_constraint_index = 4 },
    };
    for (where_alias_expansions) |expansion| _ = try original.where_alias_expansions.append(gpa, expansion);
    const where_alias_declaration_publications = [_]ModuleEnv.WhereAliasDeclarationPublication{
        .{ .decl_node = 1, .dependency_rank = 0, .expansions_start = 0, .expansions_len = 1, .outcome = @intFromEnum(ModuleEnv.WhereAliasDeclarationPublication.Outcome.ready) },
        .{ .decl_node = 5, .dependency_rank = 0, .expansions_start = 1, .expansions_len = 0, .outcome = @intFromEnum(ModuleEnv.WhereAliasDeclarationPublication.Outcome.ready) },
    };
    for (where_alias_declaration_publications) |publication| {
        _ = try original.where_alias_declaration_publications.append(gpa, publication);
    }
    const where_method_sources = [_]ModuleEnv.WhereMethodSource{
        .{ .owner_node = 1, .where_node = 2, .method_ident = 3, .source_ordinal = 0, .retained_constraint_index = 4, .source_contracts_start = 5, .source_contracts_len = 1 },
    };
    for (where_method_sources) |where_method_source| _ = try original.where_method_sources.append(gpa, where_method_source);
    const rows = [_]ModuleEnv.MethodOutputRow{
        .{ .row_var = 7, .tail_var = 8, .position = 0, .path_start = 0, .path_len = 1 },
        .{ .row_var = 9, .tail_var = 10, .position = 1, .path_start = 1, .path_len = 2 },
    };
    for (rows) |row| _ = try original.method_output_rows.append(gpa, row);
    const publications = [_]ModuleEnv.MethodOutputPublication{
        .{ .type_node_idx = 3, .rows_start = 0, .rows_len = 0 },
        .{ .type_node_idx = 4, .rows_start = 0, .rows_len = 2 },
    };
    for (publications) |publication| _ = try original.method_output_publications.append(gpa, publication);
    const authorizations = [_]ModuleEnv.ResultRowWideningUse{
        .{ .constraint_fn_var = 12, .source_where_fn_var = 13, .widening = (ModuleEnv.ResultRowWidening{ .direct = true }).bits() },
        .{ .constraint_fn_var = 14, .source_where_fn_var = 15, .widening = (ModuleEnv.ResultRowWidening{ .try_ok = true, .try_err = true }).bits() },
    };
    for (authorizations) |authorization| _ = try original.result_row_widening_uses.append(gpa, authorization);
    const external_lookup_tokens = [_]ModuleEnv.ExternalLookupToken{
        .{
            .key = .{ .resolved_module_idx = 1, .target_node = 41 },
            .import_idx = 0,
            .origin_node = 17,
            .site_kind = @intFromEnum(ModuleEnv.ExternalLookupSiteKind.external_lookup_expr),
            .parameter_ordinal = ModuleEnv.ExternalLookupToken.none,
        },
        .{
            .key = .{ .resolved_module_idx = 1, .target_node = 41 },
            .import_idx = 0,
            .origin_node = 19,
            .site_kind = @intFromEnum(ModuleEnv.ExternalLookupSiteKind.external_lookup_expr),
            .parameter_ordinal = ModuleEnv.ExternalLookupToken.none,
        },
    };
    for (external_lookup_tokens) |token| _ = try original.external_lookup_tokens.append(gpa, token);
    const external_cache_seeds = [_]ModuleEnv.ExternalCacheSeed{.{
        .key = external_lookup_tokens[0].key,
        .seed_token = 0,
        .seed_node = external_lookup_tokens[0].origin_node,
        .support_step = 23,
    }};
    for (external_cache_seeds) |seed| _ = try original.external_cache_seeds.append(gpa, seed);
    const selected_method_decisions = [_]ModuleEnv.SelectedMethodDecision{.{
        .state = @intFromEnum(ModuleEnv.SelectedMethodDecision.State.complete),
        .scheme_use_index = 31,
        .constraint_index = 32,
        .receiver_var = 33,
        .provider_dependency_index = 34,
        .provider_type_node = 35,
        .provider_def = 36,
        .constraint_evidence_start = 37,
        .constraint_evidence_len = 2,
        .constraint_moves_start = 39,
        .constraint_moves_len = 3,
        .root_copy_step = 42,
        .use_copy_step = 43,
        .receiver_owner_origin_module = 44,
        .receiver_owner_source_decl = 45,
        .provider_method_entry_index = 46,
    }};
    for (selected_method_decisions) |decision| {
        _ = try original.selected_method_decisions.append(gpa, decision);
    }

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
    const serialized_ptr: *const ModuleEnv.Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized_ptr.validate(buffer.len);

    var corrupt = serialized_ptr.*;
    corrupt.method_output_publications.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized_ptr.*;
    corrupt.method_output_row_path_steps.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized_ptr.*;
    corrupt.method_output_rows.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized_ptr.*;
    corrupt.where_alias_expansions.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized_ptr.*;
    corrupt.where_alias_declaration_publications.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized_ptr.*;
    corrupt.external_lookup_tokens.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized_ptr.*;
    corrupt.external_cache_seeds.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized_ptr.*;
    corrupt.selected_method_decisions.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));

    const MutableDeserialize = struct {
        fn run(
            allocator: std.mem.Allocator,
            serialized_env: *const ModuleEnv.Serialized,
            base_addr: usize,
            source_bytes: []const u8,
        ) !void {
            const env = try serialized_env.deserializeWithMutableTypes(base_addr, allocator, source_bytes, "Test");
            defer {
                env.deinitCachedModule();
                allocator.destroy(env);
            }
            try std.testing.expect(env.method_output_publications.items.items.len != 0);
            try std.testing.expect(env.method_output_row_path_steps.items.items.len != 0);
            try std.testing.expect(env.method_output_rows.items.items.len != 0);
            try std.testing.expect(env.result_row_widening_uses.items.items.len != 0);
            try std.testing.expect(env.where_alias_expansions.items.items.len != 0);
            try std.testing.expect(env.where_alias_declaration_publications.items.items.len != 0);
            try std.testing.expect(env.where_method_sources.items.items.len != 0);
            try std.testing.expect(env.external_lookup_tokens.items.items.len != 0);
            try std.testing.expect(env.external_cache_seeds.items.items.len != 0);
            try std.testing.expect(env.selected_method_decisions.items.items.len != 0);
        }
    };
    try std.testing.checkAllAllocationFailures(
        gpa,
        MutableDeserialize.run,
        .{ serialized_ptr, @intFromPtr(buffer.ptr), source },
    );

    const static_env = try serialized_ptr.viewStatic(@intFromPtr(buffer.ptr), gpa, source, "Test");
    try std.testing.expectEqualSlices(ModuleEnv.MethodOutputPublication, &publications, static_env.method_output_publications.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.WhereMethodMarkerPathStep, &paths, static_env.method_output_row_path_steps.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.MethodOutputRow, &rows, static_env.method_output_rows.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.ResultRowWideningUse, &authorizations, static_env.result_row_widening_uses.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.WhereAliasExpansion, &where_alias_expansions, static_env.where_alias_expansions.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.WhereAliasDeclarationPublication, &where_alias_declaration_publications, static_env.where_alias_declaration_publications.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.WhereMethodSource, &where_method_sources, static_env.where_method_sources.items.items);
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(&external_lookup_tokens), std.mem.sliceAsBytes(static_env.external_lookup_tokens.items.items));
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(&external_cache_seeds), std.mem.sliceAsBytes(static_env.external_cache_seeds.items.items));
    try std.testing.expectEqualSlices(ModuleEnv.SelectedMethodDecision, &selected_method_decisions, static_env.selected_method_decisions.items.items);

    const mutable_env = try serialized_ptr.deserializeWithMutableTypes(@intFromPtr(buffer.ptr), gpa, source, "Test");
    defer {
        mutable_env.deinitCachedModule();
        gpa.destroy(mutable_env);
    }
    try std.testing.expectEqualSlices(ModuleEnv.MethodOutputPublication, &publications, mutable_env.method_output_publications.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.WhereMethodMarkerPathStep, &paths, mutable_env.method_output_row_path_steps.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.MethodOutputRow, &rows, mutable_env.method_output_rows.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.ResultRowWideningUse, &authorizations, mutable_env.result_row_widening_uses.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.WhereAliasExpansion, &where_alias_expansions, mutable_env.where_alias_expansions.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.WhereAliasDeclarationPublication, &where_alias_declaration_publications, mutable_env.where_alias_declaration_publications.items.items);
    try std.testing.expectEqualSlices(ModuleEnv.WhereMethodSource, &where_method_sources, mutable_env.where_method_sources.items.items);
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(&external_lookup_tokens), std.mem.sliceAsBytes(mutable_env.external_lookup_tokens.items.items));
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(&external_cache_seeds), std.mem.sliceAsBytes(mutable_env.external_cache_seeds.items.items));
    try std.testing.expectEqualSlices(ModuleEnv.SelectedMethodDecision, &selected_method_decisions, mutable_env.selected_method_decisions.items.items);
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

    const present_idx = try original.recordFileDependency("data.txt", 0, 0);
    const present_hash = [_]u8{0x11} ** 32;
    original.setFileDependencyContentHash(present_idx, present_hash);

    const missing_idx = try original.recordFileDependency("missing.txt", 0, 0);
    original.setFileDependencyMissing(missing_idx);

    const unreadable_idx = try original.recordFileDependency("denied.txt", 0, 0);
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
    const builtin_ident = try env.internModuleIdentity(&([_]u8{0x66} ** 32), Ident.Idx.NONE);

    const segment_idx = try env.addExpr(.{ .e_str_segment = .{ .literal = str_literal_idx } }, base.Region.from_raw_offsets(0, 5));
    const expr_idx = try env.addExpr(.{ .e_str = .{ .span = Expr.Span{ .span = base.DataSpan{ .start = @intFromEnum(segment_idx), .len = 1 } } } }, base.Region.from_raw_offsets(0, 5));

    const segment_var = try env.types.freshFromContent(.err);
    try std.testing.expectEqual(ModuleEnv.varFrom(segment_idx), segment_var);

    const expr_var = try env.types.freshFromContent(.err);
    try std.testing.expectEqual(ModuleEnv.varFrom(expr_idx), expr_var);

    const str_content = try env.types.mkNominal(
        types.TypeIdent{ .ident_idx = str_ident },
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
