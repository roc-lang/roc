//! Tests for integer literal canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of integer literals and integer expressions from parsed AST into the
//! compiler's canonical internal representation (CIR).

const std = @import("std");
const build_options = @import("build_options");
const testing = std.testing;
const parse = @import("parse");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");
const TestEnv = @import("TestEnv.zig").TestEnv;
const BuiltinTestContext = @import("./BuiltinTestContext.zig").BuiltinTestContext;
const ModuleEnv = @import("../ModuleEnv.zig");
const CoreCtx = @import("ctx").CoreCtx;

fn getIntValue(module_env: *ModuleEnv, expr_idx: CIR.Expr.Idx) error{NotAnInteger}!i128 {
    const expr = module_env.store.getExpr(expr_idx);
    switch (expr) {
        .e_num => |int_expr| {
            return @bitCast(int_expr.value.bytes);
        },
        else => return error.NotAnInteger,
    }
}

test "canonicalize simple positive integer" {
    const source = "42";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, 42), value);
}

test "canonicalize simple negative integer" {
    const source = "-42";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, -42), value);
}

test "canonicalize very large integer" {
    const source = "170141183460469231731687303715884105727"; // i128 max
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, 170141183460469231731687303715884105727), value);
}

test "canonicalize very large negative integer" {
    const source = "-170141183460469231731687303715884105728"; // i128 min
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
    try testing.expectEqual(@as(i128, -170141183460469231731687303715884105728), value);
}

test "canonicalize builtin typed integer suffix without caller setup" {
    var test_env = try TestEnv.init("0.I64");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_typed_int => |typed| {
            try testing.expectEqual(@as(i128, 0), typed.value.toI128());
            try testing.expectEqualStrings("I64", test_env.getIdent(typed.type_name));
        },
        else => return error.NotATypedInteger,
    }
}

test "canonicalize builtin typed fractional suffix without caller setup" {
    var test_env = try TestEnv.init("3.14.Dec");
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());

    switch (expr) {
        .e_typed_frac => |typed| {
            try testing.expectEqual(@as(i128, 3_140_000_000_000_000_000), typed.value.toI128());
            try testing.expectEqualStrings("Dec", test_env.getIdent(typed.type_name));
        },
        else => return error.NotATypedFraction,
    }
}

test "typed numeric suffix still uses ordinary scope lookup" {
    var test_env = try TestEnv.init("123.UnknownType");
    defer test_env.deinit();

    _ = try test_env.canonicalizeExpr();

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var found_undeclared_type = false;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .undeclared_type => |data| {
                if (std.mem.eql(u8, test_env.getIdent(data.name), "UnknownType")) {
                    found_undeclared_type = true;
                }
            },
            else => {},
        }
    }

    try testing.expect(found_undeclared_type);
}

test "typed numeric suffix uses local shadow of builtin numeric type" {
    const source =
        \\{
        \\    U64 := {}
        \\    123.U64
        \\}
    ;
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
    try testing.expectEqual(.e_block, std.meta.activeTag(expr));

    const final_expr_idx = expr.e_block.final_expr;
    const suffix_target = test_env.module_env.numericSuffixTargetForNode(ModuleEnv.nodeIdxFrom(final_expr_idx)) orelse return error.MissingSuffixTarget;

    switch (suffix_target.target()) {
        .local => {},
        else => return error.NotLocalSuffixTarget,
    }

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);

    var found_shadowing_warning = false;
    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .builtin_type_shadowed_warning => found_shadowing_warning = true,
            else => {},
        }
    }

    try testing.expect(found_shadowing_warning);
}

test "canonicalize integer literals with underscores" {
    const test_cases = [_]struct { source: []const u8, expected: i128 }{
        .{ .source = "1_000", .expected = 1000 },
        .{ .source = "1_000_000", .expected = 1000000 },
        .{ .source = "-1_234_567", .expected = -1234567 },
        .{ .source = "123_456_789", .expected = 123456789 },
        .{ .source = "1_2_3_4_5", .expected = 12345 },
    };

    for (test_cases) |tc| {
        var test_env = try TestEnv.init(tc.source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        const value = try getIntValue(test_env.module_env, canonical_expr.get_idx());
        try testing.expectEqual(tc.expected, value);
    }
}

test "canonicalize integer literals outside supported range" {
    // Exact integer literals that do not fit the compact payload stay available
    // for `from_numeral`; checking decides whether a concrete target accepts them.
    const test_cases = [_][]const u8{
        // Negative number slightly lower than i128 min
        "-170141183460469231731687303715884105729",
        // Number too big for u128 max (340282366920938463463374607431768211455)
        "340282366920938463463374607431768211456",
        // Way too big
        "999999999999999999999999999999999999999999999999999",
    };

    for (test_cases) |source| {
        var test_env = try TestEnv.init(source);
        defer test_env.deinit();

        const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
        const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
        try testing.expect(expr == .e_num_from_numeral);
        const literal = test_env.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(canonical_expr.get_idx())) orelse return error.MissingNumeralLiteral;
        try testing.expect(!literal.isFractional());
    }
}

test "integer literal one above u128 max records exact digits without canon diagnostic" {
    // 2^128 does not fit any builtin integer type. Canonicalization must not
    // report anything for it: the literal defers to `from_numeral`, and
    // checking decides whether the target type accepts the value (defaulted
    // commits report INVALID NUMBER there, covered by
    // src/check/test/num_type_inference_test.zig and
    // test/snapshots/expr_int_invalid.md). What canonicalization owns is
    // recording the exact magnitude so no truncation can happen downstream.
    const source = "340282366920938463463374607431768211456";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
    try testing.expect(expr == .e_num_from_numeral);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);
    try testing.expectEqual(@as(usize, 0), diagnostics.len);

    const literal = test_env.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(canonical_expr.get_idx())) orelse return error.MissingNumeralLiteral;
    try testing.expect(!literal.isFractional());
    try testing.expect(!literal.isNegative());

    // 2^128 in big-endian base-256 digits: a leading 1 followed by 16 zero bytes.
    const expected_digits = [_]u8{1} ++ [_]u8{0} ** 16;
    try testing.expectEqualSlices(u8, &expected_digits, test_env.module_env.numeralDigitsBefore(literal));
}

test "integer literal one below i128 min records exact digits without canon diagnostic" {
    // -(2^127 + 1) is one below i128 min. As with the positive overflow case,
    // canonicalization defers the fit decision to `from_numeral` and must
    // record the exact magnitude and sign.
    const source = "-170141183460469231731687303715884105729";
    var test_env = try TestEnv.init(source);
    defer test_env.deinit();

    const canonical_expr = try test_env.canonicalizeExpr() orelse unreachable;
    const expr = test_env.getCanonicalExpr(canonical_expr.get_idx());
    try testing.expect(expr == .e_num_from_numeral);

    const diagnostics = try test_env.getDiagnostics();
    defer testing.allocator.free(diagnostics);
    try testing.expectEqual(@as(usize, 0), diagnostics.len);

    const literal = test_env.module_env.numeralLiteralForNode(ModuleEnv.nodeIdxFrom(canonical_expr.get_idx())) orelse return error.MissingNumeralLiteral;
    try testing.expect(!literal.isFractional());
    try testing.expect(literal.isNegative());

    // 2^127 + 1 in big-endian base-256 digits: 0x80, fourteen zero bytes, then 1.
    const expected_digits = [_]u8{0x80} ++ [_]u8{0} ** 14 ++ [_]u8{1};
    try testing.expectEqualSlices(u8, &expected_digits, test_env.module_env.numeralDigitsBefore(literal));
}

test "hexadecimal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
    }{
        // Basic hex literals
        .{ .literal = "0x0", .expected_value = 0 },
        .{ .literal = "0x1", .expected_value = 1 },
        .{ .literal = "0xFF", .expected_value = 255 },
        .{ .literal = "0x100", .expected_value = 256 },
        .{ .literal = "0xFFFF", .expected_value = 65535 },
        .{ .literal = "0x10000", .expected_value = 65536 },
        .{ .literal = "0xFFFFFFFF", .expected_value = 4294967295 },
        .{ .literal = "0x100000000", .expected_value = 4294967296 },
        .{ .literal = "0xFFFFFFFFFFFFFFFF", .expected_value = @as(i128, @bitCast(@as(u128, 18446744073709551615))) },

        // Hex with underscores
        .{ .literal = "0x1_000", .expected_value = 4096 },
        .{ .literal = "0xFF_FF", .expected_value = 65535 },
        .{ .literal = "0x1234_5678_9ABC_DEF0", .expected_value = @as(i128, @bitCast(@as(u128, 0x123456789ABCDEF0))) },

        // Negative hex literals
        .{ .literal = "-0x1", .expected_value = -1 },
        .{ .literal = "-0x80", .expected_value = -128 },
        .{ .literal = "-0x81", .expected_value = -129 },
        .{ .literal = "-0x8000", .expected_value = -32768 },
        .{ .literal = "-0x8001", .expected_value = -32769 },
        .{ .literal = "-0x80000000", .expected_value = -2147483648 },
        .{ .literal = "-0x80000001", .expected_value = -2147483649 },
        .{ .literal = "-0x8000000000000000", .expected_value = -9223372036854775808 },
        .{ .literal = "-0x8000000000000001", .expected_value = @as(i128, -9223372036854775809) },
    };

    var gpa_state = std.heap.DebugAllocator(.{ .safety = true, .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){};
    defer std.debug.assert(build_options.debugGpaOk(gpa_state.deinit()));
    const gpa = gpa_state.allocator();
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        try env.initCIRFields("test");

        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const ast = try parse.expr(gpa, &env.common);
        defer ast.deinit();

        var czer = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx.get_idx());
        try std.testing.expect(expr == .e_num);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_num.value.bytes)));
    }
}

test "binary integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
    }{
        // Basic binary literals
        .{ .literal = "0b0", .expected_value = 0 },
        .{ .literal = "0b1", .expected_value = 1 },
        .{ .literal = "0b10", .expected_value = 2 },
        .{ .literal = "0b11111111", .expected_value = 255 },
        .{ .literal = "0b100000000", .expected_value = 256 },
        .{ .literal = "0b1111111111111111", .expected_value = 65535 },
        .{ .literal = "0b10000000000000000", .expected_value = 65536 },

        // Binary with underscores
        .{ .literal = "0b11_11", .expected_value = 15 },
        .{ .literal = "0b1111_1111", .expected_value = 255 },
        .{ .literal = "0b1_0000_0000", .expected_value = 256 },
        .{ .literal = "0b1010_1010_1010_1010", .expected_value = 43690 },

        // Negative binary
        .{ .literal = "-0b1", .expected_value = -1 },
        .{ .literal = "-0b10000000", .expected_value = -128 },
        .{ .literal = "-0b10000001", .expected_value = -129 },
        .{ .literal = "-0b1000000000000000", .expected_value = -32768 },
        .{ .literal = "-0b1000000000000001", .expected_value = -32769 },
    };

    var gpa_state = std.heap.DebugAllocator(.{ .safety = true, .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){};
    defer std.debug.assert(build_options.debugGpaOk(gpa_state.deinit()));
    const gpa = gpa_state.allocator();
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        try env.initCIRFields("test");

        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const ast = try parse.expr(gpa, &env.common);
        defer ast.deinit();

        var czer = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx.get_idx());
        try std.testing.expect(expr == .e_num);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_num.value.bytes)));
    }
}

test "octal integer literals" {
    const test_cases = [_]struct {
        literal: []const u8,
        expected_value: i128,
    }{
        // Basic octal literals
        .{ .literal = "0o0", .expected_value = 0 },
        .{ .literal = "0o1", .expected_value = 1 },
        .{ .literal = "0o7", .expected_value = 7 },
        .{ .literal = "0o10", .expected_value = 8 },
        .{ .literal = "0o377", .expected_value = 255 },
        .{ .literal = "0o400", .expected_value = 256 },
        .{ .literal = "0o177777", .expected_value = 65535 },
        .{ .literal = "0o200000", .expected_value = 65536 },

        // Octal with underscores
        .{ .literal = "0o377_377", .expected_value = 130815 },
        .{ .literal = "0o1_234_567", .expected_value = 342391 },

        // Negative octal literals
        .{ .literal = "-0o1", .expected_value = -1 },
        .{ .literal = "-0o100", .expected_value = -64 },
        .{ .literal = "-0o200", .expected_value = -128 },
        .{ .literal = "-0o201", .expected_value = -129 },
        .{ .literal = "-0o100000", .expected_value = -32768 },
        .{ .literal = "-0o100001", .expected_value = -32769 },
    };

    var gpa_state = std.heap.DebugAllocator(.{ .safety = true, .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){};
    defer std.debug.assert(build_options.debugGpaOk(gpa_state.deinit()));
    const gpa = gpa_state.allocator();
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();

    for (test_cases) |tc| {
        var env = try ModuleEnv.init(gpa, tc.literal);
        defer env.deinit();

        try env.initCIRFields("test");

        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const ast = try parse.expr(gpa, &env.common);
        defer ast.deinit();

        var czer = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
        defer czer.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
            std.debug.print("Failed to canonicalize: {s}\n", .{tc.literal});
            try std.testing.expect(false);
            continue;
        };

        const expr = env.store.getExpr(canonical_expr_idx.get_idx());
        try std.testing.expect(expr == .e_num);

        // Check the value
        try std.testing.expectEqual(tc.expected_value, @as(i128, @bitCast(expr.e_num.value.bytes)));
    }
}
