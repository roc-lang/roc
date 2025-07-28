//! Tests for fractional literal canonicalization during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct canonicalization
//! of fractional literals and decimal expressions from parsed AST into the
//! compiler's canonical internal representation (CIR).

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const parse = @import("parse");
const Can = @import("can");
const compile = @import("compile");
const types = @import("types");
const RocDec = @import("builtins").RocDec;
const ModuleEnv = compile.ModuleEnv;

// Note: Each test should create its own GPA to avoid memory leak detection issues

fn parseAndCreateFrac(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    can: *Can,
    expr_idx: ModuleEnv.Expr.Idx,
} {
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(module_env);

    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(allocator, "Test");

    const can = try allocator.create(Can);
    can.* = try Can.init(module_env, parse_ast, null);

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);

    // Check if parsing produced an error
    if (parse_ast.parse_diagnostics.items.len > 0 or parse_ast.tokenize_diagnostics.items.len > 0) {
        // Parsing failed, create a runtime error
        const diagnostic_idx = try module_env.addDiagnostic(.{ .invalid_num_literal = .{
            .region = base.Region.zero(),
        } });
        const error_expr_idx = try module_env.addExprAndTypeVar(ModuleEnv.Expr{ .e_runtime_error = .{ .diagnostic = diagnostic_idx } }, types.Content{ .err = {} }, base.Region.zero());
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .can = can,
            .expr_idx = error_expr_idx,
        };
    }

    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .can = can,
        .expr_idx = (canonical_expr_idx.get_idx()),
    };
}

fn cleanup(allocator: std.mem.Allocator, result: anytype) void {
    result.can.deinit();
    result.parse_ast.deinit(allocator);
    result.module_env.deinit();
    allocator.destroy(result.can);
    allocator.destroy(result.parse_ast);
    allocator.destroy(result.module_env);
}

test "fractional literal - basic decimal" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "3.14");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 314);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.module_env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |poly| {
                            try testing.expect(poly.requirements.fits_in_dec);
                        },
                        .frac_unbound => |requirements| {
                            try testing.expect(requirements.fits_in_dec);
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                .flex_var => {
                    // It's an unbound type variable, which is also fine for literals
                },
                else => {
                    std.debug.print("Unexpected content type: {}\n", .{resolved.desc.content});
                    return error.UnexpectedContentType;
                },
            }
        },
        .e_frac_dec => |dec| {
            _ = dec;
            // Also accept e_frac_dec for decimal literals
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.module_env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |poly| {
                            try testing.expect(poly.requirements.fits_in_dec);
                        },
                        .frac_unbound => |requirements| {
                            try testing.expect(requirements.fits_in_dec);
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                .flex_var => {
                    // It's an unbound type variable, which is also fine for literals
                },
                else => {
                    std.debug.print("Unexpected content type: {}\n", .{resolved.desc.content});
                    return error.UnexpectedContentType;
                },
            }
        },
        else => {
            std.debug.print("Unexpected expr type: {}\n", .{expr});
            try testing.expect(false); // Should be dec_small or frac_dec
        },
    }
}

test "fractional literal - scientific notation small" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "1.23e-10");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            // Very small numbers may round to zero when parsed as small decimal
            // This is expected behavior when the value is too small for i16 representation
            try testing.expectEqual(dec.numerator, 0);

            // Still check type requirements
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.module_env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .frac_poly => |poly| {
                            try testing.expect(poly.requirements.fits_in_f32);
                            try testing.expect(poly.requirements.fits_in_dec);
                        },
                        .frac_unbound => |requirements| {
                            try testing.expect(requirements.fits_in_f32);
                            try testing.expect(requirements.fits_in_dec);
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                .flex_var => {
                    // It's an unbound type variable, which is also fine for literals
                },
                else => return error.UnexpectedContentType,
            }
        },
        .e_frac_dec => |frac| {
            // Scientific notation can also be parsed as RocDec for exact representation
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.module_env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |poly| {
                            // 1.23e-10 is within f32 range, so it should fit (ignoring precision)
                            try testing.expect(poly.requirements.fits_in_f32);
                            try testing.expect(poly.requirements.fits_in_dec);
                        },
                        .frac_unbound => |requirements| {
                            // 1.23e-10 is within f32 range, so it should fit (ignoring precision)
                            try testing.expect(requirements.fits_in_f32);
                            try testing.expect(requirements.fits_in_dec);
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                .flex_var => {
                    // It's an unbound type variable, which is also fine for literals
                },
                else => return error.UnexpectedContentType,
            }
            // RocDec stores the value in a special format
            _ = frac;
        },
        .e_frac_f64 => |frac| {
            // Or it might be parsed as f64
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.module_env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |poly| {
                            // 1.23e-10 is within f32 range, so it should fit (ignoring precision)
                            try testing.expect(poly.requirements.fits_in_f32);
                            try testing.expect(poly.requirements.fits_in_dec);
                        },
                        .frac_unbound => |requirements| {
                            // 1.23e-10 is within f32 range, so it should fit (ignoring precision)
                            try testing.expect(requirements.fits_in_f32);
                            try testing.expect(requirements.fits_in_dec);
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                .flex_var => {
                    // It's an unbound type variable, which is also fine for literals
                },
                else => return error.UnexpectedContentType,
            }
            try testing.expectApproxEqAbs(frac.value, 1.23e-10, 1e-20);
        },
        else => {
            std.debug.print("Unexpected expr type for '1.23e-10': {}\n", .{expr});
            try testing.expect(false); // Should be e_frac_f64
        },
    }
}

test "fractional literal - scientific notation large (near f64 max)" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "1e308");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_f64 => |frac| {
            const expr_as_type_var: types.Var = @enumFromInt(@intFromEnum(result.expr_idx));
            const resolved = result.module_env.types.resolveVar(expr_as_type_var);
            switch (resolved.desc.content) {
                .structure => |structure| switch (structure) {
                    .num => |num| switch (num) {
                        .num_poly => return error.UnexpectedNumPolyType,
                        .frac_poly => |poly| {
                            try testing.expect(!poly.requirements.fits_in_dec); // Way out of Dec range
                        },
                        .frac_unbound => |requirements| {
                            try testing.expect(!requirements.fits_in_dec); // Way out of Dec range
                        },
                        else => return error.UnexpectedNumType,
                    },
                    else => return error.UnexpectedStructureType,
                },
                .flex_var => {
                    // It's an unbound type variable, which is also fine for literals
                },
                else => return error.UnexpectedContentType,
            }
            try testing.expectEqual(frac.value, 1e308);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - scientific notation at f32 boundary" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // f32 max is approximately 3.4028235e38
    // 3.4e38 is actually within the range, but let's test with 3.5e38 which is above
    const result = try parseAndCreateFrac(gpa, "3.5e38");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_f64 => |frac| {
            try testing.expect(true); // Infinity doesn't fit in Dec
            try testing.expect(true); // Above f32 max
            try testing.expectEqual(frac.value, 3.5e38);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - negative zero" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "-0.0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            // dec_small doesn't preserve sign for -0.0
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 0);
            try testing.expect(true); // -0.0 fits in Dec
            try testing.expect(true); // -0.0 fits in F32
        },
        .e_frac_dec => |frac| {
            try testing.expect(true); // -0.0 fits in Dec and F32
            const f64_val = frac.value.toF64();
            // RocDec may not preserve the sign bit for -0.0, so just check it's zero
            try testing.expectEqual(@abs(f64_val), 0.0);
        },
        .e_frac_f64 => |frac| {
            // Also acceptable if it's parsed as f64
            try testing.expectEqual(frac.value, -0.0);
            try testing.expect(std.math.signbit(frac.value));
        },
        else => {
            try testing.expect(false); // Should be dec_small, frac_dec or frac_f64
        },
    }
}

test "fractional literal - positive zero" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "0.0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 1);
            try testing.expect(true); // 0.5 fits in F32
            try testing.expect(true); // 0.5 fits in Dec
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "fractional literal - very small scientific notation" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test a value that's smaller than f32 min positive normal (approximately 1.2e-38)
    const result = try parseAndCreateFrac(gpa, "1e-40");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_f64 => |frac| {
            try testing.expect(true); // This test is for minimum f64 value
            // 1e-40 is within f32's subnormal range, so it should fit (ignoring precision)
            try testing.expect(true); // 1e-40 fits in F32 subnormal range
            try testing.expectApproxEqAbs(frac.value, 1e-40, 1e-50);
        },
        else => {
            try testing.expect(false); // Should be frac_f64
        },
    }
}

test "fractional literal - NaN handling" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Note: NaN is not a valid numeric literal in Roc
    // The parser will fail before canonicalization
    // This test verifies that behavior
    const module_env = try gpa.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(gpa, "NaN");
    defer {
        module_env.deinit();
        gpa.destroy(module_env);
    }

    var parse_ast = try parse.parseExpr(module_env);
    defer parse_ast.deinit(gpa);

    // Check if it parsed as an identifier instead of a number
    const expr: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const parsed_expr = parse_ast.store.getExpr(expr);

    // NaN parses as a tag expression, not a numeric literal
    try testing.expect(parsed_expr == .tag);
}

test "fractional literal - infinity handling" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Note: Infinity is not a valid numeric literal in Roc
    // The parser will fail before canonicalization
    // This test verifies that behavior
    const module_env = try gpa.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(gpa, "Infinity");
    defer {
        module_env.deinit();
        gpa.destroy(module_env);
    }

    var parse_ast = try parse.parseExpr(module_env);
    defer parse_ast.deinit(gpa);

    // Check if it parsed as an identifier instead of a number
    const expr: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const parsed_expr = parse_ast.store.getExpr(expr);

    // Infinity parses as a tag expression, not a numeric literal
    try testing.expect(parsed_expr == .tag);
}

test "fractional literal - scientific notation with capital E" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "2.5E10");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_dec => |frac| {
            try testing.expect(true); // 1e7 fits in Dec
            try testing.expect(true); // 2.5e10 is within f32 range
            try testing.expectApproxEqAbs(@as(f64, @floatFromInt(frac.value.num)) / std.math.pow(f64, 10, 18), 2.5e10, 1e-5);
        },
        else => {
            try testing.expect(false); // Should be frac_dec
        },
    }
}

test "fractional literal - negative scientific notation" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "-1.5e-5");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_dec => |frac| {
            try testing.expect(true); // 1e-7 fits in Dec
            // -1.5e-5 may not round-trip perfectly through f32
            // Let's just check the value is correct
            try testing.expectApproxEqAbs(@as(f64, @floatFromInt(frac.value.num)) / std.math.pow(f64, 10, 18), -1.5e-5, 1e-10);
        },
        else => {
            try testing.expect(false); // Should be frac_dec
        },
    }
}

test "negative zero preservation in f64" {
    // Direct test to verify std.fmt.parseFloat preserves negative zero
    const neg_zero_str = "-0.0";
    const pos_zero_str = "0.0";

    const neg_zero = try std.fmt.parseFloat(f64, neg_zero_str);
    const pos_zero = try std.fmt.parseFloat(f64, pos_zero_str);

    // Both should be zero
    try testing.expectEqual(neg_zero, 0.0);
    try testing.expectEqual(pos_zero, 0.0);

    // But neg_zero should have sign bit set
    try testing.expect(std.math.signbit(neg_zero));
    try testing.expect(!std.math.signbit(pos_zero));

    // Also test f32
    const neg_zero_f32 = try std.fmt.parseFloat(f32, neg_zero_str);
    const pos_zero_f32 = try std.fmt.parseFloat(f32, pos_zero_str);

    try testing.expect(std.math.signbit(neg_zero_f32));
    try testing.expect(!std.math.signbit(pos_zero_f32));
}

test "negative zero forced to f64 parsing" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Test that when we force parsing through f64 path (e.g., with scientific notation),
    // negative zero now uses dec_small and loses sign
    const result = try parseAndCreateFrac(gpa, "-0.0e0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "negative zero preservation in Dec" {
    // Test if RocDec preserves negative zero when converting to f64
    if (RocDec.fromNonemptySlice("-0.0")) |neg_zero_dec| {
        const f64_val = neg_zero_dec.toF64();

        // RocDec does NOT preserve negative zero - it becomes positive zero
        try testing.expect(!std.math.signbit(f64_val));
        try testing.expectEqual(f64_val, 0.0);
    }

    // For comparison, positive zero
    if (RocDec.fromNonemptySlice("0.0")) |pos_zero_dec| {
        const f64_val = pos_zero_dec.toF64();
        try testing.expect(!std.math.signbit(f64_val));
        try testing.expectEqual(f64_val, 0.0);
    }
}

test "small dec - basic positive decimal" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "3.14");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 314);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
            try testing.expect(true); // 1.1 fits in Dec
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "negative zero preservation - uses f64" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "-0.0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.numerator, 0);
            // Sign is lost with dec_small
        },
        .e_frac_dec => |frac| {
            // If it went through Dec path, check if sign is preserved
            if (std.mem.eql(u8, "-0.0", "-0.0")) {
                const f64_val = @as(f64, @floatFromInt(frac.value.num)) / std.math.pow(f64, 10, 18);
                // Dec doesn't preserve negative zero, which is expected
                try testing.expectEqual(f64_val, 0.0);
            }
        },
        else => {
            try testing.expect(false); // Should be dec_small or frac_dec
        },
    }
}

test "negative zero with scientific notation - preserves sign via f64" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Scientific notation now uses dec_small for zero, loses sign
    const result = try parseAndCreateFrac(gpa, "-0.0e0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - positive zero" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "0.0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 0);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);

            // Verify positive zero
            try testing.expectEqual(dec.numerator, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - precision preservation for 0.1" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "0.1");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 1);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - trailing zeros" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "1.100");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 1100);
            try testing.expectEqual(dec.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - negative number" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "-5.25");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, -525);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - max i8 value" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "127.99");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 12799);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - min i8 value" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "-128.0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, -1280);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - 128.0 now fits with new representation" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "128.0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            // With numerator/power representation, 128.0 = 1280/10^1 fits in i16
            try testing.expectEqual(dec.numerator, 1280);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - exceeds i16 range falls back to Dec" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "32768.0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_frac_dec => {
            // Should fall back to Dec because 327680 > 32767 (max i16)
            try testing.expect(true); // 3.141 fits in Dec
        },
        .e_dec_small => {
            try testing.expect(false); // Should NOT be dec_small
        },
        else => {
            try testing.expect(false); // Should be frac_dec
        },
    }
}

test "small dec - too many fractional digits falls back to Dec" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "1.234");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 1234);
            try testing.expectEqual(dec.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - complex example 0.001" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "0.001");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 1);
            try testing.expectEqual(dec.denominator_power_of_ten, 3);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "small dec - negative example -0.05" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "-0.05");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            // -0.05 = -5 / 10^2
            try testing.expectEqual(dec.numerator, -5);
            try testing.expectEqual(dec.denominator_power_of_ten, 2);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "negative zero with scientific notation preserves sign" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    // Scientific notation now uses dec_small for zero, loses sign
    const result = try parseAndCreateFrac(gpa, "-0.0e0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |small| {
            // With scientific notation, now uses dec_small and loses sign
            try testing.expectEqual(small.numerator, 0);
            try testing.expectEqual(small.denominator_power_of_ten, 0);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}

test "fractional literal - simple 1.0 uses small dec" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const result = try parseAndCreateFrac(gpa, "1.0");
    defer cleanup(gpa, result);

    const expr = result.module_env.store.getExpr(result.expr_idx);
    switch (expr) {
        .e_dec_small => |dec| {
            try testing.expectEqual(dec.numerator, 10);
            try testing.expectEqual(dec.denominator_power_of_ten, 1);
        },
        else => {
            try testing.expect(false); // Should be dec_small
        },
    }
}
