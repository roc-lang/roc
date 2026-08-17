//! Canonicalization tests for optional record fields and optional field paths.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");

const BuiltinTestContext = @import("BuiltinTestContext.zig").BuiltinTestContext;
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");
const ModuleEnv = @import("../ModuleEnv.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;
const CoreCtx = @import("ctx").CoreCtx;

test "consecutive optional field accesses canonicalize to one source-ordered path" {
    const source = "{}.?outer.?inner";
    var env = try TestEnv.init(source);
    defer env.deinit();

    const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
    const access = blk_access: {
        const scrutinee = env.getCanonicalExpr(result.idx);
        if (scrutinee != .e_field_access) return error.ExpectedFieldAccess;
        break :blk_access scrutinee.e_field_access;
    };

    try std.testing.expectEqualDeep(
        base.Region.from_raw_offsets(0, source.len),
        env.module_env.store.getExprRegion(result.idx),
    );
    try std.testing.expect(env.getCanonicalExpr(access.receiver) == .e_empty_record);

    try std.testing.expectEqual(@as(u32, 2), access.segments.len);
    const outer_idx = env.module_env.store.fieldAccessSegmentAt(access.segments, 0);
    const inner_idx = env.module_env.store.fieldAccessSegmentAt(access.segments, 1);

    const outer = env.module_env.store.getFieldAccessSegment(outer_idx);
    try std.testing.expectEqualStrings("outer", env.getIdent(outer.name));
    try std.testing.expectEqual(.optional, outer.mode);
    try std.testing.expectEqualDeep(
        base.Region.from_raw_offsets(2, 9),
        env.module_env.store.getFieldAccessSegmentRegion(outer_idx),
    );

    const inner = env.module_env.store.getFieldAccessSegment(inner_idx);
    try std.testing.expectEqualStrings("inner", env.getIdent(inner.name));
    try std.testing.expectEqual(.optional, inner.mode);
    try std.testing.expectEqualDeep(
        base.Region.from_raw_offsets(9, 16),
        env.module_env.store.getFieldAccessSegmentRegion(inner_idx),
    );
}

test "mixed optional and required accesses preserve source order, modes, and regions" {
    const Case = struct {
        source: []const u8,
        names: [3][]const u8,
        modes: [3]CIR.Expr.FieldAccessMode,
        regions: [3]base.Region,
    };
    const cases = [_]Case{
        .{
            .source = "{}.?b.c.?d",
            .names = .{ "b", "c", "d" },
            .modes = .{ .optional, .required, .optional },
            .regions = .{
                base.Region.from_raw_offsets(2, 5),
                base.Region.from_raw_offsets(5, 7),
                base.Region.from_raw_offsets(7, 10),
            },
        },
        .{
            .source = "{}.b.?c.d",
            .names = .{ "b", "c", "d" },
            .modes = .{ .required, .optional, .required },
            .regions = .{
                base.Region.from_raw_offsets(2, 4),
                base.Region.from_raw_offsets(4, 7),
                base.Region.from_raw_offsets(7, 9),
            },
        },
    };

    for (cases) |case| {
        var env = try TestEnv.init(case.source);
        defer env.deinit();

        const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
        const access = blk_access: {
            const scrutinee = env.getCanonicalExpr(result.idx);
            if (scrutinee != .e_field_access) return error.ExpectedFieldAccess;
            break :blk_access scrutinee.e_field_access;
        };
        try std.testing.expect(env.getCanonicalExpr(access.receiver) == .e_empty_record);
        try std.testing.expectEqualDeep(
            base.Region.from_raw_offsets(0, @intCast(case.source.len)),
            env.module_env.store.getExprRegion(result.idx),
        );

        try std.testing.expectEqual(@as(u32, @intCast(case.names.len)), access.segments.len);
        for (case.names, case.modes, case.regions, 0..) |name, mode, region, position| {
            const segment_idx = env.module_env.store.fieldAccessSegmentAt(access.segments, @intCast(position));
            const segment = env.module_env.store.getFieldAccessSegment(segment_idx);
            try std.testing.expectEqualStrings(name, env.getIdent(segment.name));
            try std.testing.expectEqual(mode, segment.mode);
            try std.testing.expectEqualDeep(region, env.module_env.store.getFieldAccessSegmentRegion(segment_idx));
        }
    }
}

test "required-only accesses canonicalize to the same flat path representation" {
    var env = try TestEnv.init("{}.a.b.c");
    defer env.deinit();

    const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
    const access = blk_access: {
        const scrutinee = env.getCanonicalExpr(result.idx);
        if (scrutinee != .e_field_access) return error.ExpectedFieldAccess;
        break :blk_access scrutinee.e_field_access;
    };

    try std.testing.expect(env.getCanonicalExpr(access.receiver) == .e_empty_record);
    try std.testing.expectEqual(@as(u32, 3), access.segments.len);

    const expected_names = [_][]const u8{ "a", "b", "c" };
    for (expected_names, 0..) |expected_name, position| {
        const segment_idx = env.module_env.store.fieldAccessSegmentAt(access.segments, @intCast(position));
        try std.testing.expectEqual(
            @intFromEnum(access.segments.start) + @as(u32, @intCast(position)),
            @intFromEnum(segment_idx),
        );
        const segment = env.module_env.store.getFieldAccessSegment(segment_idx);
        try std.testing.expectEqualStrings(expected_name, env.getIdent(segment.name));
        try std.testing.expectEqual(.required, segment.mode);
    }
}

test "mixed field-access S-expression preserves required and optional modes in source order" {
    var env = try TestEnv.init("{}.a.?b.c");
    defer env.deinit();

    const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
    const expr_value = env.getCanonicalExpr(result.idx);

    var tree = base.SExprTree.init(std.testing.allocator);
    defer tree.deinit();
    try expr_value.pushToSExprTree(env.module_env, &tree, result.idx);

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();
    try tree.toStringPretty(&output.writer, .skip_linecol);

    const text = output.written();
    try std.testing.expect(std.mem.find(u8, text, "segments") != null);
    const first_required = std.mem.find(u8, text, "\"required\"") orelse return error.MissingRequiredMode;
    const optional_offset = std.mem.find(u8, text[first_required + 1 ..], "\"optional\"") orelse return error.MissingOptionalMode;
    const optional = first_required + 1 + optional_offset;
    const second_required_offset = std.mem.find(u8, text[optional + 1 ..], "\"required\"") orelse return error.MissingSecondRequiredMode;
    const second_required = optional + 1 + second_required_offset;
    try std.testing.expect(first_required < optional);
    try std.testing.expect(optional < second_required);
}

test "parentheses delimit canonical optional field paths" {
    const source = "({}.?outer).?inner";
    var env = try TestEnv.init(source);
    defer env.deinit();

    const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
    try std.testing.expectEqualDeep(
        base.Region.from_raw_offsets(0, source.len),
        env.module_env.store.getExprRegion(result.idx),
    );

    const root_access = blk_root_access: {
        const scrutinee = env.getCanonicalExpr(result.idx);
        if (scrutinee != .e_field_access) return error.ExpectedOuterFieldAccess;
        break :blk_root_access scrutinee.e_field_access;
    };
    try std.testing.expectEqual(@as(u32, 1), root_access.segments.len);
    const root_segment = env.module_env.store.fieldAccessSegmentAt(root_access.segments, 0);
    try std.testing.expectEqualStrings(
        "inner",
        env.getIdent(env.module_env.store.getFieldAccessSegment(root_segment).name),
    );
    try std.testing.expectEqualDeep(
        base.Region.from_raw_offsets(11, 18),
        env.module_env.store.getFieldAccessSegmentRegion(root_segment),
    );

    const parenthesized_access_idx = root_access.receiver;
    try std.testing.expectEqualDeep(
        base.Region.from_raw_offsets(1, 10),
        env.module_env.store.getExprRegion(parenthesized_access_idx),
    );
    const parenthesized_access = blk_parenthesized_access: {
        const scrutinee = env.getCanonicalExpr(parenthesized_access_idx);
        if (scrutinee != .e_field_access) return error.ExpectedInnerFieldAccess;
        break :blk_parenthesized_access scrutinee.e_field_access;
    };
    try std.testing.expectEqual(@as(u32, 1), parenthesized_access.segments.len);
    const parenthesized_segment = env.module_env.store.fieldAccessSegmentAt(parenthesized_access.segments, 0);
    try std.testing.expectEqualStrings(
        "outer",
        env.getIdent(env.module_env.store.getFieldAccessSegment(parenthesized_segment).name),
    );
    try std.testing.expectEqualDeep(
        base.Region.from_raw_offsets(3, 10),
        env.module_env.store.getFieldAccessSegmentRegion(parenthesized_segment),
    );
}

test "parentheses delimit optional-containing paths in both access directions" {
    {
        var env = try TestEnv.init("({}.?a).b");
        defer env.deinit();

        const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
        const outer = blk_outer: {
            const scrutinee = env.getCanonicalExpr(result.idx);
            if (scrutinee != .e_field_access) return error.ExpectedRequiredFieldAccess;
            break :blk_outer scrutinee.e_field_access;
        };
        try std.testing.expectEqual(@as(u32, 1), outer.segments.len);
        const outer_segment_idx = env.module_env.store.fieldAccessSegmentAt(outer.segments, 0);
        const outer_segment = env.module_env.store.getFieldAccessSegment(outer_segment_idx);
        try std.testing.expectEqualStrings("b", env.getIdent(outer_segment.name));
        try std.testing.expectEqual(.required, outer_segment.mode);
        const inner = blk_inner: {
            const scrutinee = env.getCanonicalExpr(outer.receiver);
            if (scrutinee != .e_field_access) return error.ExpectedFieldAccess;
            break :blk_inner scrutinee.e_field_access;
        };
        try std.testing.expectEqual(@as(u32, 1), inner.segments.len);
        const inner_segment_idx = env.module_env.store.fieldAccessSegmentAt(inner.segments, 0);
        try std.testing.expectEqual(.optional, env.module_env.store.getFieldAccessSegment(inner_segment_idx).mode);
    }

    {
        var env = try TestEnv.init("({}.a).?b");
        defer env.deinit();

        const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
        const outer = blk_outer: {
            const scrutinee = env.getCanonicalExpr(result.idx);
            if (scrutinee != .e_field_access) return error.ExpectedFieldAccess;
            break :blk_outer scrutinee.e_field_access;
        };
        try std.testing.expectEqual(@as(u32, 1), outer.segments.len);
        const outer_segment_idx = env.module_env.store.fieldAccessSegmentAt(outer.segments, 0);
        try std.testing.expectEqual(.optional, env.module_env.store.getFieldAccessSegment(outer_segment_idx).mode);
        const inner = blk_inner: {
            const scrutinee = env.getCanonicalExpr(outer.receiver);
            if (scrutinee != .e_field_access) return error.ExpectedRequiredFieldAccess;
            break :blk_inner scrutinee.e_field_access;
        };
        try std.testing.expectEqual(@as(u32, 1), inner.segments.len);
        const inner_segment_idx = env.module_env.store.fieldAccessSegmentAt(inner.segments, 0);
        const inner_segment = env.module_env.store.getFieldAccessSegment(inner_segment_idx);
        try std.testing.expectEqualStrings("a", env.getIdent(inner_segment.name));
        try std.testing.expectEqual(.required, inner_segment.mode);
    }
}

test "optional field paths preserve receiver free variables" {
    var env = try TestEnv.init("|record| |_| record.required.?outer.required.?inner");
    defer env.deinit();

    const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
    const outer_lambda = blk_outer_lambda: {
        const scrutinee = env.getCanonicalExpr(result.idx);
        if (scrutinee != .e_lambda) return error.ExpectedOuterLambda;
        break :blk_outer_lambda scrutinee.e_lambda;
    };
    const inner_closure = blk_inner_closure: {
        const scrutinee = env.getCanonicalExpr(outer_lambda.body);
        if (scrutinee != .e_closure) return error.ExpectedInnerClosure;
        break :blk_inner_closure scrutinee.e_closure;
    };

    const captures = env.module_env.store.sliceCaptures(inner_closure.captures);
    try std.testing.expectEqual(@as(usize, 1), captures.len);
    const capture = env.module_env.store.getCapture(captures[0]);
    try std.testing.expectEqualStrings("record", env.getIdent(capture.name));

    const inner_lambda = blk_inner_lambda: {
        const scrutinee = env.getCanonicalExpr(inner_closure.lambda_idx);
        if (scrutinee != .e_lambda) return error.ExpectedInnerLambda;
        break :blk_inner_lambda scrutinee.e_lambda;
    };
    const access = blk_access: {
        const scrutinee = env.getCanonicalExpr(inner_lambda.body);
        if (scrutinee != .e_field_access) return error.ExpectedFieldAccess;
        break :blk_access scrutinee.e_field_access;
    };
    try std.testing.expectEqual(
        @as(u32, 4),
        access.segments.len,
    );
}

test "optional record annotation fields retain their presence mode" {
    const allocator = std.testing.allocator;
    const source = "Example : { x : U32, y ?: U32, z : U32 }";

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();
    try std.testing.expectEqual(@as(u32, 1), env.type_decls.span.len);

    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const anno_idx = blk_anno_idx: {
        const scrutinee = env.store.getStatement(statement_idx);
        if (scrutinee != .s_alias_decl) return error.ExpectedAliasDeclaration;
        const alias = scrutinee.s_alias_decl;
        break :blk_anno_idx alias.anno;
    };
    const record = blk_record: {
        const scrutinee = env.store.getTypeAnno(anno_idx);
        if (scrutinee != .record) return error.ExpectedRecordAnnotation;
        break :blk_record scrutinee.record;
    };
    const fields = env.store.sliceAnnoRecordFields(record.fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);

    const expected_names = [_][]const u8{ "x", "y", "z" };
    const expected_optional = [_]bool{ false, true, false };
    for (fields, expected_names, expected_optional) |field_idx, expected_name, is_optional| {
        const field = env.store.getAnnoRecordField(field_idx);
        try std.testing.expectEqualStrings(expected_name, env.getIdent(field.name));
        try std.testing.expectEqual(is_optional, field.is_optional);
    }
}

test "long alternating field-access chains canonicalize without recursive path nodes" {
    const segment_count = 16_384;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(std.testing.allocator);
    try source.ensureTotalCapacity(std.testing.allocator, 2 + 3 * segment_count + 7);
    try source.appendSlice(std.testing.allocator, "{}");
    for (0..segment_count) |i| {
        if (i == 0) {
            try source.appendSlice(std.testing.allocator, ".?first");
        } else if (i + 1 == segment_count) {
            try source.appendSlice(std.testing.allocator, ".last");
        } else if (i % 2 == 0) {
            try source.appendSlice(std.testing.allocator, ".?f");
        } else {
            try source.appendSlice(std.testing.allocator, ".f");
        }
    }

    var env = try TestEnv.init(source.items);
    defer env.deinit();
    const result = (try env.canonicalizeExpr()) orelse return error.CanonicalizeError;
    const access = blk_access: {
        const scrutinee = env.getCanonicalExpr(result.idx);
        if (scrutinee != .e_field_access) return error.ExpectedFieldAccess;
        break :blk_access scrutinee.e_field_access;
    };

    try std.testing.expect(env.getCanonicalExpr(access.receiver) == .e_empty_record);
    try std.testing.expectEqual(@as(u32, segment_count), access.segments.len);
    const first_segment = env.module_env.store.fieldAccessSegmentAt(access.segments, 0);
    const last_segment = env.module_env.store.fieldAccessSegmentAt(access.segments, segment_count - 1);
    try std.testing.expectEqualStrings(
        "first",
        env.getIdent(env.module_env.store.getFieldAccessSegment(first_segment).name),
    );
    try std.testing.expectEqual(.optional, env.module_env.store.getFieldAccessSegment(first_segment).mode);
    try std.testing.expectEqualStrings(
        "last",
        env.getIdent(env.module_env.store.getFieldAccessSegment(last_segment).name),
    );
    try std.testing.expectEqual(.required, env.module_env.store.getFieldAccessSegment(last_segment).mode);
    for (0..segment_count) |i| {
        const segment_idx = env.module_env.store.fieldAccessSegmentAt(access.segments, @intCast(i));
        const expected_mode: CIR.Expr.FieldAccessMode = if (i % 2 == 0) .optional else .required;
        try std.testing.expectEqual(expected_mode, env.module_env.store.getFieldAccessSegment(segment_idx).mode);
    }
}

test "defaulted record annotation field canonicalizes its default expression" {
    const allocator = std.testing.allocator;
    const source = "Example := { x : U32, y : U32 ?? 10, z : U32 }";

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();
    try std.testing.expectEqual(@as(u32, 1), env.type_decls.span.len);

    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const anno_idx = blk_anno_idx: {
        const scrutinee = env.store.getStatement(statement_idx);
        if (scrutinee != .s_nominal_decl) return error.ExpectedNominalDeclaration;
        const nominal = scrutinee.s_nominal_decl;
        break :blk_anno_idx nominal.anno;
    };
    const record = blk_record: {
        const scrutinee = env.store.getTypeAnno(anno_idx);
        if (scrutinee != .record) return error.ExpectedRecordAnnotation;
        break :blk_record scrutinee.record;
    };
    const fields = env.store.sliceAnnoRecordFields(record.fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);

    const x = env.store.getAnnoRecordField(fields[0]);
    try std.testing.expectEqual(@as(?CIR.Expr.Idx, null), x.default_value);
    const y = env.store.getAnnoRecordField(fields[1]);
    const default_idx = y.default_value orelse return error.ExpectedDefaultValue;
    // The default canonicalized as an ordinary expression.
    if (env.store.getExpr(default_idx) != .e_num) return error.ExpectedNumericDefault;
    const z = env.store.getAnnoRecordField(fields[2]);
    try std.testing.expectEqual(@as(?CIR.Expr.Idx, null), z.default_value);
}

test "optional field with default is rejected at canonicalization" {
    const allocator = std.testing.allocator;
    const source = "Example : { y ?: U32 ?? 10 }";

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    // The `?:` + `??` combination is rejected and the default dropped
    // (design.md "Defaulted Fields"). PRECEDENCE PIN: even though this alias
    // is also an illegal position for a default, the more specific shape
    // conflict wins and exactly one diagnostic fires per offending field.
    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found = false;
    for (diagnostics) |diag| {
        if (diag == .optional_field_cannot_have_default) found = true;
        try std.testing.expect(diag != .default_not_allowed_in_structural_record);
    }
    try std.testing.expect(found);

    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const anno_idx = blk_anno_idx: {
        const scrutinee = env.store.getStatement(statement_idx);
        if (scrutinee != .s_alias_decl) return error.ExpectedAliasDeclaration;
        const alias = scrutinee.s_alias_decl;
        break :blk_anno_idx alias.anno;
    };
    const record = blk_record: {
        const scrutinee = env.store.getTypeAnno(anno_idx);
        if (scrutinee != .record) return error.ExpectedRecordAnnotation;
        break :blk_record scrutinee.record;
    };
    const fields = env.store.sliceAnnoRecordFields(record.fields);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    const y = env.store.getAnnoRecordField(fields[0]);
    try std.testing.expect(y.is_optional);
    try std.testing.expectEqual(@as(?CIR.Expr.Idx, null), y.default_value);
}

test "unnamed nominal field with a default reports the padding restriction" {
    const allocator = std.testing.allocator;
    const source = "Opt7 := { a : U8, _pad : U8 ?? 3 }";

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    var report = try env.diagnosticToReport(diagnostics[0], allocator, "Test.roc");
    defer report.deinit();
    try std.testing.expectEqualStrings("Unnamed Field Cannot Have A Default", report.title);
}

test "default in an inline annotation is rejected as structural" {
    const allocator = std.testing.allocator;
    // Formerly the local-capture non-literal test: under the nominal-only
    // rule an inline annotation can never carry a `??` at all, so the
    // local-capture scenario (a default referencing a lambda binding) is
    // structurally unreachable—nominal declarations are top-level only.
    const source =
        \\f = |n| {
        \\    y : { a : U8 ?? n }
        \\    y = {}
        \\    y.a
        \\}
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    // The position error fires; the default is dropped before it is
    // canonicalized.
    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found = false;
    for (diagnostics) |diag| {
        if (diag == .default_not_allowed_in_structural_record) found = true;
    }
    try std.testing.expect(found);
}

test "default in a type alias is rejected as structural" {
    const allocator = std.testing.allocator;
    const source = "Cfg : { a : U8 ?? 1 }";

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    var report = try env.diagnosticToReport(diagnostics[0], allocator, "Test.roc");
    defer report.deinit();
    try std.testing.expectEqualStrings("Default Not Allowed In Structural Record", report.title);

    // The field survives as plain required with no default.
    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const statement = env.store.getStatement(statement_idx);
    if (statement != .s_alias_decl) return error.ExpectedAliasDeclaration;
    const anno = env.store.getTypeAnno(statement.s_alias_decl.anno);
    if (anno != .record) return error.ExpectedRecordAnnotation;
    const fields = env.store.sliceAnnoRecordFields(anno.record.fields);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    const field = env.store.getAnnoRecordField(fields[0]);
    try std.testing.expectEqual(@as(?CIR.Expr.Idx, null), field.default_value);
}

test "default on a nested record inside a nominal backing is rejected" {
    const allocator = std.testing.allocator;
    // Direct fields only: the nested record type is structural even though
    // it appears inside a nominal declaration's backing.
    const source = "A := { inner : { x : U8 ?? 1 } }";

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 1), diagnostics.len);
    var report = try env.diagnosticToReport(diagnostics[0], allocator, "Test.roc");
    defer report.deinit();
    try std.testing.expectEqualStrings("Default Not Allowed In Structural Record", report.title);
}

test "self-referential default is rejected by the cycle pass" {
    const allocator = std.testing.allocator;
    // default(a) -> x (reference) -> X.{} (omission of a) -> default(a).
    const source =
        \\X := { a : U8 ?? x.a }
        \\
        \\x : X
        \\x = X.{}
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    // The end-of-module cycle pass reports once and drops the default so
    // check and lowering never see it (design.md "Defaulted Fields").
    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .record_default_reference_cycle) found += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), found);

    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const statement = env.store.getStatement(statement_idx);
    if (statement != .s_nominal_decl) return error.ExpectedNominalDeclaration;
    const anno = env.store.getTypeAnno(statement.s_nominal_decl.anno);
    if (anno != .record) return error.ExpectedRecordAnnotation;
    const fields = env.store.sliceAnnoRecordFields(anno.record.fields);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    const field = env.store.getAnnoRecordField(fields[0]);
    try std.testing.expectEqual(@as(?CIR.Expr.Idx, null), field.default_value);
}

test "type-declaration default referencing a def is accepted as a pure expression" {
    const allocator = std.testing.allocator;
    const source =
        \\Cfg := { a : U8 ?? foo }
        \\
        \\foo : U8
        \\foo = 10
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    // A reference with no path back to the default is not a cycle: the
    // default survives with its expression (design.md "Defaulted Fields").
    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);

    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const statement = env.store.getStatement(statement_idx);
    if (statement != .s_nominal_decl) return error.ExpectedNominalDeclaration;
    const anno = env.store.getTypeAnno(statement.s_nominal_decl.anno);
    if (anno != .record) return error.ExpectedRecordAnnotation;
    const fields = env.store.sliceAnnoRecordFields(anno.record.fields);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    const field = env.store.getAnnoRecordField(fields[0]);
    try std.testing.expect(field.default_value != null);
}

test "construction omitting a defaulted field records an explicit demand edge on the default's references" {
    const allocator = std.testing.allocator;
    // `cfg` omits the defaulted field `n`, whose default references `base`,
    // declared AFTER `cfg`. Materializing `cfg` materializes the default, so
    // the top-level demand graph must carry the explicit edge cfg -> base
    // and the evaluation order must place `base` before `cfg` (design.md
    // "Defaulted Fields": omission edges are explicit, not recovered later).
    const source =
        \\Cfg := { n : U8 ?? base }
        \\
        \\cfg : Cfg
        \\cfg = Cfg.{}
        \\
        \\base : U8
        \\base = 40 + 2
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);

    var cfg_def: ?CIR.Def.Idx = null;
    var base_def: ?CIR.Def.Idx = null;
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        if (env.store.getExpr(def.expr) == .e_anno_only) continue;
        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;
        const name = env.common.getIdent(pattern.assign.ident);
        if (std.mem.eql(u8, name, "cfg")) cfg_def = def_idx;
        if (std.mem.eql(u8, name, "base")) base_def = def_idx;
    }
    const cfg = cfg_def orelse return error.MissingCfgDef;
    const base_idx = base_def orelse return error.MissingBaseDef;

    const DependencyGraph = @import("../DependencyGraph.zig");
    try std.testing.expect(DependencyGraph.hasDependency(
        env.top_level_demand_dependencies.items.items,
        cfg,
        base_idx,
    ));

    // The SCC order must schedule `base` strictly before `cfg`.
    const eval_order = env.evaluation_order orelse return error.MissingEvaluationOrder;
    var cfg_group: ?usize = null;
    var base_group: ?usize = null;
    for (eval_order.sccs, 0..) |scc, group_index| {
        for (scc.defs) |def_idx| {
            if (def_idx == cfg) cfg_group = group_index;
            if (def_idx == base_idx) base_group = group_index;
        }
    }
    try std.testing.expect(base_group.? < cfg_group.?);
}

test "def-mediated default cycle is rejected by the cycle pass" {
    const allocator = std.testing.allocator;
    // The heir of the old alias-mediated gap: the default references `foo`,
    // whose body reads a value built by a construction that omits the
    // field. default(a) -> foo -> cfg_val -> Cfg.{} (omission) -> default(a).
    const source =
        \\Cfg := { a : U8 ?? foo }
        \\
        \\foo : U8
        \\foo = cfg_val.a
        \\
        \\cfg_val : Cfg
        \\cfg_val = Cfg.{}
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .record_default_reference_cycle) found += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), found);
}

test "mutually cyclic defaults report once and both drop" {
    const allocator = std.testing.allocator;
    // default(A.a) -> B.{} omission -> default(B.b) -> A.{} omission ->
    // default(A.a): one SCC, one report, both defaults dropped.
    const source =
        \\A := { a : U8 ?? B.{}.b }
        \\B := { b : U8 ?? A.{}.a }
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .record_default_reference_cycle) found += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), found);

    var decl_index: usize = 0;
    while (decl_index < 2) : (decl_index += 1) {
        const statement_idx = env.store.statementAt(env.type_decls, decl_index);
        const statement = env.store.getStatement(statement_idx);
        if (statement != .s_nominal_decl) return error.ExpectedNominalDeclaration;
        const anno = env.store.getTypeAnno(statement.s_nominal_decl.anno);
        if (anno != .record) return error.ExpectedRecordAnnotation;
        const fields = env.store.sliceAnnoRecordFields(anno.record.fields);
        try std.testing.expectEqual(@as(usize, 1), fields.len);
        const field = env.store.getAnnoRecordField(fields[0]);
        try std.testing.expectEqual(@as(?CIR.Expr.Idx, null), field.default_value);
    }
}

test "literal defaults of every accepted shape canonicalize with their defaults kept" {
    const allocator = std.testing.allocator;
    // The full accepted literal set (design.md "Defaulted Fields"): numeral,
    // negated numeral, interpolation-free string, empty list, tag application
    // of literals, and a record literal of literals.
    const source =
        \\Example := {
        \\    a : U8 ?? 10,
        \\    b : Str ?? "hi",
        \\    c : I8 ?? -1,
        \\    d : List(U8) ?? [],
        \\    e : [None, Some(U8)] ?? Some(1),
        \\    f : { x : U8 } ?? { x: 1 },
        \\}
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);

    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const statement = env.store.getStatement(statement_idx);
    if (statement != .s_nominal_decl) return error.ExpectedNominalDeclaration;
    const anno_idx = statement.s_nominal_decl.anno;
    const anno = env.store.getTypeAnno(anno_idx);
    if (anno != .record) return error.ExpectedRecordAnnotation;
    const record = anno.record;
    const fields = env.store.sliceAnnoRecordFields(record.fields);
    try std.testing.expectEqual(@as(usize, 6), fields.len);
    for (fields) |field_idx| {
        const field = env.store.getAnnoRecordField(field_idx);
        try std.testing.expect(field.default_value != null);
    }
}

test "default on a block-local nominal declaration is rejected" {
    const allocator = std.testing.allocator;
    // A block-local `:=` declaration's default would canonicalize in
    // function scope and capture `n`, which no other construction site can
    // supply, and the end-of-module cycle pass only scans top-level
    // declarations—so the default is rejected outright and dropped.
    // PRECEDENCE PIN: the declaration IS a nominal backing, so the
    // structural-record diagnostic must NOT fire; the dedicated local-decl
    // diagnostic does.
    const source =
        \\f = |n| {
        \\    Cfg := { a : U8 ?? n }
        \\    g = |{}| Cfg.{}
        \\    g({})
        \\}
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .default_not_allowed_on_local_type_decl) found += 1;
        try std.testing.expect(diag != .default_not_allowed_in_structural_record);
    }
    try std.testing.expectEqual(@as(usize, 1), found);
}

test "default on a block-local opaque declaration is rejected" {
    const allocator = std.testing.allocator;
    // Opaque (`::`) declarations share the nominal backing route, so the
    // block-local restriction applies identically.
    const source =
        \\f = |n| {
        \\    Cfg :: { a : U8 ?? n }
        \\    g = |{}| Cfg.{}
        \\    g({})
        \\}
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found: usize = 0;
    for (diagnostics) |diag| {
        if (diag != .default_not_allowed_on_local_type_decl) continue;
        found += 1;
        var report = try env.diagnosticToReport(diag, allocator, "Test.roc");
        defer report.deinit();
        try std.testing.expectEqualStrings("Default Not Allowed On Local Type Declaration", report.title);
    }
    try std.testing.expectEqual(@as(usize, 1), found);
}

test "self-referential default behind parens is rejected by the cycle pass" {
    const allocator = std.testing.allocator;
    // canonicalizeNominalBackingAnno unwraps a parenthesized backing when
    // ACCEPTING the default, so the cycle pass must unwrap identically when
    // COLLECTING it—otherwise this cycle would silently leak to check.
    const source =
        \\X := ({ a : U8 ?? x.a })
        \\
        \\x : X
        \\x = X.{}
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .record_default_reference_cycle) found += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), found);

    // The default is dropped through the parens wrapping.
    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const statement = env.store.getStatement(statement_idx);
    if (statement != .s_nominal_decl) return error.ExpectedNominalDeclaration;
    var anno = env.store.getTypeAnno(statement.s_nominal_decl.anno);
    while (anno == .parens) anno = env.store.getTypeAnno(anno.parens.anno);
    if (anno != .record) return error.ExpectedRecordAnnotation;
    const fields = env.store.sliceAnnoRecordFields(anno.record.fields);
    try std.testing.expectEqual(@as(usize, 1), fields.len);
    const field = env.store.getAnnoRecordField(fields[0]);
    try std.testing.expectEqual(@as(?CIR.Expr.Idx, null), field.default_value);
}

test "function-valued default referencing a def is not a cycle" {
    const allocator = std.testing.allocator;
    // Materializing `mk`'s default only creates the `make` closure—`make`'s
    // body is NOT evaluated at materialization, so the omission of `mk`
    // inside that body is not an edge and there is no cycle. The walk must
    // not descend lambda bodies reached as values.
    const source =
        \\Cfg := { n : U8 ?? 0, mk : (U8 -> Cfg) ?? make }
        \\
        \\make : U8 -> Cfg
        \\make = |x| Cfg.{ n: x }
    ;

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    for (diagnostics) |diag| {
        try std.testing.expect(diag != .record_default_reference_cycle);
    }

    // Both defaults survive.
    const statement_idx = env.store.statementAt(env.type_decls, 0);
    const statement = env.store.getStatement(statement_idx);
    if (statement != .s_nominal_decl) return error.ExpectedNominalDeclaration;
    const anno = env.store.getTypeAnno(statement.s_nominal_decl.anno);
    if (anno != .record) return error.ExpectedRecordAnnotation;
    const fields = env.store.sliceAnnoRecordFields(anno.record.fields);
    try std.testing.expectEqual(@as(usize, 2), fields.len);
    for (fields) |field_idx| {
        const field = env.store.getAnnoRecordField(field_idx);
        try std.testing.expect(field.default_value != null);
    }
}

test "immediately-invoked lambda default cycle is still rejected" {
    const allocator = std.testing.allocator;
    // A lambda in direct callee position IS evaluated at materialization,
    // so its body walks: the construction inside omits `n`, a self-edge.
    const source = "Cfg := { n : U8 ?? (|{}| Cfg.{}.n)({}) }";

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .record_default_reference_cycle) found += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), found);
}

test "immediately-invoked closure default cycle is still rejected" {
    const allocator = std.testing.allocator;
    // Same rule through the closure form: the inner lambda captures `k`, so
    // it canonicalizes as a closure, and it still sits in direct callee
    // position—its body walks and the omission self-edge is found.
    const source = "Cfg := { n : U8 ?? (|k| (|{}| Cfg.{}.n + k)({}))(1) }";

    var builtin_ctx = try BuiltinTestContext.init(allocator);
    defer builtin_ctx.deinit();
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();
    try env.initCIRFields("Test");
    const ast = try parse.file(allocator, &env.common);
    defer ast.deinit();
    var can = try Can.initModule(
        CoreCtx.testing(allocator, allocator),
        &env,
        ast,
        builtin_ctx.canInitContext(),
    );
    defer can.deinit();

    try can.canonicalizeFile();

    const diagnostics = try env.getDiagnostics();
    defer allocator.free(diagnostics);
    var found: usize = 0;
    for (diagnostics) |diag| {
        if (diag == .record_default_reference_cycle) found += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), found);
}
