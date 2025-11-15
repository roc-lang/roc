//! Tests for integer literal type inference
//!
//! This module contains unit tests that verify the correct type inferenece
//! of integer literals and integer expressions from CIR into the types store

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const builtins = @import("builtins");
const Can = @import("can").Can;
const CIR = @import("can").CIR;
const ModuleEnv = @import("can").ModuleEnv;
const RocDec = builtins.dec.RocDec;
const TestEnv = @import("TestEnv.zig");
const parseIntWithUnderscores = Can.parseIntWithUnderscores;
const Content = types.Content;

test "infers type for small nums" {
    const test_cases = [_]struct { source: []const u8, expected: []const u8 }{
        .{ .source = "1", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "-1", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "10", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "-10", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "255", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "-128", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "256", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "-129", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "32767", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "-32768", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "65535", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
        .{ .source = "-32769", .expected = "_size where [_a.from_int_digits : _arg -> _ret]" },
    };

    inline for (test_cases) |tc| {
        var test_env = try TestEnv.initExpr("Test", tc.source);
        defer test_env.deinit();

        try test_env.assertLastDefType(tc.expected);
    }
}

test "infers type for nums with specific requirements" {
    const test_cases = [_]struct {
        source: []const u8,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        .{ .source = "127", .expected_sign_needed = false, .expected_bits_needed = .@"7" },
        .{ .source = "128", .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .source = "255", .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        .{ .source = "256", .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .source = "-128", .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        .{ .source = "-129", .expected_sign_needed = true, .expected_bits_needed = .@"8" },
        .{ .source = "32767", .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        .{ .source = "32768", .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .source = "65535", .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        .{ .source = "65536", .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },
    };

    inline for (test_cases) |tc| {
        var test_env = try TestEnv.initExpr("Test", tc.source);
        defer test_env.deinit();

        const typ = (try test_env.getLastExprType()).content;

        try testing.expect(typ == .structure);
        try testing.expect(typ.structure == .num);
        try testing.expect(typ.structure.num == .num_unbound);

        const reqs = typ.structure.num.num_unbound;

        try testing.expectEqual(tc.expected_sign_needed, reqs.int_requirements.sign_needed);
        try testing.expectEqual(
            tc.expected_bits_needed.toBits(),
            reqs.int_requirements.bits_needed,
        );

        try testing.expectEqual(true, reqs.frac_requirements.fits_in_f32);
        try testing.expectEqual(true, reqs.frac_requirements.fits_in_dec);
    }
}

test "infers num requirements correctly" {
    const test_cases = [_]struct {
        source: []const u8,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // 255 needs 8 bits and no sign
        .{ .source = "255", .expected_sign_needed = false, .expected_bits_needed = .@"8" },
        // 256 needs 9-15 bits and no sign
        .{ .source = "256", .expected_sign_needed = false, .expected_bits_needed = .@"9_to_15" },
        // -1 needs sign and 7 bits
        .{ .source = "-1", .expected_sign_needed = true, .expected_bits_needed = .@"7" },
        // 65535 needs 16 bits and no sign
        .{ .source = "65535", .expected_sign_needed = false, .expected_bits_needed = .@"16" },
        // 65536 needs 17-31 bits and no sign
        .{ .source = "65536", .expected_sign_needed = false, .expected_bits_needed = .@"17_to_31" },
    };

    inline for (test_cases) |tc| {
        var test_env = try TestEnv.initExpr("Test", tc.source);
        defer test_env.deinit();

        const typ = (try test_env.getLastExprType()).content;

        try testing.expect(typ == .structure);
        try testing.expect(typ.structure == .num);
        try testing.expect(typ.structure.num == .num_unbound);

        const reqs = typ.structure.num.num_unbound;

        try testing.expectEqual(tc.expected_sign_needed, reqs.int_requirements.sign_needed);
        try testing.expectEqual(
            tc.expected_bits_needed.toBits(),
            reqs.int_requirements.bits_needed,
        );

        try testing.expectEqual(true, reqs.frac_requirements.fits_in_f32);
        try testing.expectEqual(true, reqs.frac_requirements.fits_in_dec);
    }
}

test "fail to infer num literals outside supported range" {
    // Test integer literals that are too big to be represented
    const test_cases = [_][]const u8{
        // Negative number slightly lower than i128 min
        "-170141183460469231731687303715884105729",
        // Number too big for u128 max (340282366920938463463374607431768211455)
        "340282366920938463463374607431768211456",
        // Way too big
        "999999999999999999999999999999999999999999999999999",
        // Way, way too big
        "999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999",
        // Way, way too big
        "-999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999",
    };

    inline for (test_cases) |source| {
        var test_env = try TestEnv.initExpr("Test", source);
        defer test_env.deinit();

        const typ = (try test_env.getLastExprType()).content;
        try testing.expect(typ == .err);
    }
}

test "edge case: negative 0" {
    const source = "-0";
    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    const typ = (try test_env.getLastExprType()).content;

    try testing.expect(typ == .structure);
    try testing.expect(typ.structure == .num);
    try testing.expect(typ.structure.num == .num_unbound);

    const reqs = typ.structure.num.num_unbound;

    try testing.expectEqual(false, reqs.int_requirements.sign_needed);
    try testing.expectEqual(7, reqs.int_requirements.bits_needed);

    try testing.expectEqual(true, reqs.frac_requirements.fits_in_f32);
    try testing.expectEqual(true, reqs.frac_requirements.fits_in_dec);
}

test "edge case: positive 0" {
    const source = "0";
    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    const typ = (try test_env.getLastExprType()).content;

    try testing.expect(typ == .structure);
    try testing.expect(typ.structure == .num);
    try testing.expect(typ.structure.num == .num_unbound);

    const reqs = typ.structure.num.num_unbound;

    try testing.expectEqual(false, reqs.int_requirements.sign_needed);
    try testing.expectEqual(7, reqs.int_requirements.bits_needed);

    try testing.expectEqual(true, reqs.frac_requirements.fits_in_f32);
    try testing.expectEqual(true, reqs.frac_requirements.fits_in_dec);
}

test "infer hexadecimal literals as unbound num" {
    const test_cases = [_]struct {
        source: []const u8,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // Basic hex literals
        .{ .source = "0x0", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0x1", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0xFF", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(1) },
        .{ .source = "0x100", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(2) },
        .{ .source = "0xFFFF", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(3) },
        .{ .source = "0x10000", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(4) },
        .{ .source = "0xFFFFFFFF", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(5) },
        .{ .source = "0x100000000", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(6) },
        .{ .source = "0xFFFFFFFFFFFFFFFF", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(7) },

        // Hex with underscores
        .{ .source = "0x1_000", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(2) },
        .{ .source = "0xFF_FF", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(3) },
        .{ .source = "0x1234_5678_9ABC_DEF0", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(6) },

        // Negative hex literals
        .{ .source = "-0x1", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "-0x80", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "-0x81", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(1) },
        .{ .source = "-0x8000", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(2) },
        .{ .source = "-0x8001", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(3) },
        .{ .source = "-0x80000000", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(4) },
        .{ .source = "-0x80000001", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(5) },
        .{ .source = "-0x8000000000000000", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(6) },
        .{ .source = "-0x8000000000000001", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(7) },
    };

    inline for (test_cases) |tc| {
        var test_env = try TestEnv.initExpr("Test", tc.source);
        defer test_env.deinit();

        const typ = (try test_env.getLastExprType()).content;

        try testing.expect(typ == .structure);
        try testing.expect(typ.structure == .num);
        try testing.expect(typ.structure.num == .num_unbound);

        const reqs = typ.structure.num.num_unbound;

        try testing.expectEqual(tc.expected_sign_needed, reqs.int_requirements.sign_needed);
        try testing.expectEqual(tc.expected_bits_needed.toBits(), reqs.int_requirements.bits_needed);
    }
}

test "infer binary literals as unbound num" {
    const test_cases = [_]struct {
        source: []const u8,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // Basic binary literals
        .{ .source = "0b0", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0b1", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0b10", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0b11111111", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(1) },
        .{ .source = "0b100000000", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(2) },
        .{ .source = "0b1111111111111111", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(3) },
        .{ .source = "0b10000000000000000", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(4) },

        // Binary with underscores
        .{ .source = "0b11_11", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0b1111_1111", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(1) },
        .{ .source = "0b1_0000_0000", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(2) },
        .{ .source = "0b1010_1010_1010_1010", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(3) },

        // Negative binary
        .{ .source = "-0b1", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "-0b10000000", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "-0b10000001", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(1) },
        .{ .source = "-0b1000000000000000", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(2) },
        .{ .source = "-0b1000000000000001", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(3) },
    };

    inline for (test_cases) |tc| {
        var test_env = try TestEnv.initExpr("Test", tc.source);
        defer test_env.deinit();

        const typ = (try test_env.getLastExprType()).content;

        try testing.expect(typ == .structure);
        try testing.expect(typ.structure == .num);
        try testing.expect(typ.structure.num == .num_unbound);

        const reqs = typ.structure.num.num_unbound;

        try testing.expectEqual(tc.expected_sign_needed, reqs.int_requirements.sign_needed);
        try testing.expectEqual(tc.expected_bits_needed.toBits(), reqs.int_requirements.bits_needed);
    }
}

test "infer octal literals as unbound num" {
    const test_cases = [_]struct {
        source: []const u8,
        expected_sign_needed: bool,
        expected_bits_needed: types.Num.Int.BitsNeeded,
    }{
        // Basic octal literals
        .{ .source = "0o0", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0o1", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0o7", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0o10", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "0o377", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(1) },
        .{ .source = "0o400", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(2) },
        .{ .source = "0o177777", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(3) },
        .{ .source = "0o200000", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(4) },

        // Octal with underscores
        .{ .source = "0o377_377", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(4) },
        .{ .source = "0o1_234_567", .expected_sign_needed = false, .expected_bits_needed = @enumFromInt(4) },

        // Negative octal literals
        .{ .source = "-0o1", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "-0o100", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "-0o200", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(0) },
        .{ .source = "-0o201", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(1) },
        .{ .source = "-0o100000", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(2) },
        .{ .source = "-0o100001", .expected_sign_needed = true, .expected_bits_needed = @enumFromInt(3) },
    };

    inline for (test_cases) |tc| {
        var test_env = try TestEnv.initExpr("Test", tc.source);
        defer test_env.deinit();

        const typ = (try test_env.getLastExprType()).content;

        try testing.expect(typ == .structure);
        try testing.expect(typ.structure == .num);
        try testing.expect(typ.structure.num == .num_unbound);

        const reqs = typ.structure.num.num_unbound;

        try testing.expectEqual(tc.expected_sign_needed, reqs.int_requirements.sign_needed);
        try testing.expectEqual(tc.expected_bits_needed.toBits(), reqs.int_requirements.bits_needed);
    }
}

test "type annotation I128 applied to literal in block" {
    // This is the exact pattern from the failing eval test
    const source =
        \\{
        \\    x : I128
        \\    x = 42
        \\    x
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    // Check that the block type is I128
    try test_env.assertLastDefType("I128");

    // Also try to get the type of the literal `42` expression
    // It should be I128, not .err
    const defs = test_env.module_env.store.sliceDefs(test_env.module_env.all_defs);
    const last_def = test_env.module_env.store.getDef(defs[defs.len - 1]);
    const block_expr = test_env.module_env.store.getExpr(last_def.expr);

    if (block_expr == .e_block) {
        const stmts = test_env.module_env.store.sliceStatements(block_expr.e_block.stmts);
        // Find the `x = 42` statement
        for (stmts) |stmt_idx| {
            const stmt = test_env.module_env.store.getStatement(stmt_idx);
            if (stmt == .s_decl) {
                const decl = stmt.s_decl;
                // Get the expr (42)
                const expr_idx = decl.expr;
                const expr_var = ModuleEnv.varFrom(expr_idx);

                // Resolve the type of the literal
                const resolved = test_env.module_env.types.resolveVar(expr_var);

                // It should NOT be .err
                if (resolved.desc.content == .err) {
                    std.debug.print("\n❌ LITERAL 42 HAS .err TYPE!\n", .{});
                    return error.TestFailed;
                }

                // Use TypeWriter to print what type it actually is
                try test_env.type_writer.write(expr_var);
                const type_str = test_env.type_writer.get();
                std.debug.print("\n✅ Literal 42 type: {s}\n", .{type_str});

                // It should be I128
                try testing.expectEqualStrings("I128", type_str);
            }
        }
    } else {
        return error.TestFailed;
    }
}

test "type annotation I128 applied to literal with plus method call" {
    // This tests the ACTUAL failing eval test pattern - WITH the .plus(7) call
    // This should now PASS with the fixed Builtin wiring
    const source =
        \\{
        \\    x : I128
        \\    x = 42
        \\    x.plus(7)
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();

    // Check if there are type problems first
    if (test_env.checker.problems.problems.items.len > 0) {
        std.debug.print("\n❌ Still have {d} type problems:\n", .{test_env.checker.problems.problems.items.len});
        for (test_env.checker.problems.problems.items) |problem| {
            std.debug.print("  - {s}\n", .{@tagName(problem)});
        }
    }

    // This should succeed now that numeric types are properly wired in
    try test_env.assertLastDefType("I128");
}
