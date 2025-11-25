//! Tests for integer literal type inference
//!
//! This module contains unit tests that verify the correct type inference
//! of integer literals and integer expressions from CIR into the types store.
//!
//! Number literals in the current type system are represented as flex variables
//! with static dispatch constraints for the `from_numeral` method.

const std = @import("std");
const testing = std.testing;
const types = @import("types");
const TestEnv = @import("TestEnv.zig");
const Content = types.Content;

test "infers type for small nums" {
    const test_cases = [_][]const u8{
        "1",
        "-1",
        "10",
        "-10",
        "255",
        "-128",
        "256",
        "-129",
        "32767",
        "-32768",
        "65535",
        "-32769",
    };

    inline for (test_cases) |source| {
        var test_env = try TestEnv.initExpr("Test", source);
        defer test_env.deinit();

        // Number literals produce flex variables with from_numeral constraints
        try test_env.assertLastDefTypeContains("from_numeral");
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

test "infers type for zero" {
    const test_cases = [_][]const u8{
        "0",
        "-0",
    };

    inline for (test_cases) |source| {
        var test_env = try TestEnv.initExpr("Test", source);
        defer test_env.deinit();

        // Number literals produce flex variables with from_numeral constraints
        try test_env.assertLastDefTypeContains("from_numeral");
    }
}

test "infers type for hex literals" {
    const test_cases = [_][]const u8{
        "0x0",
        "0x1",
        "0xFF",
        "0x100",
        "0xFFFF",
        "-0x1",
        "0x1_000",
    };

    inline for (test_cases) |source| {
        var test_env = try TestEnv.initExpr("Test", source);
        defer test_env.deinit();

        // Number literals produce flex variables with from_numeral constraints
        try test_env.assertLastDefTypeContains("from_numeral");
    }
}

test "infers type for binary literals" {
    const test_cases = [_][]const u8{
        "0b0",
        "0b1",
        "0b10",
        "0b11111111",
        "-0b1",
        "0b11_11",
    };

    inline for (test_cases) |source| {
        var test_env = try TestEnv.initExpr("Test", source);
        defer test_env.deinit();

        // Number literals produce flex variables with from_numeral constraints
        try test_env.assertLastDefTypeContains("from_numeral");
    }
}

test "infers type for octal literals" {
    const test_cases = [_][]const u8{
        "0o0",
        "0o1",
        "0o7",
        "0o377",
        "-0o1",
        "0o1_000",
    };

    inline for (test_cases) |source| {
        var test_env = try TestEnv.initExpr("Test", source);
        defer test_env.deinit();

        // Number literals produce flex variables with from_numeral constraints
        try test_env.assertLastDefTypeContains("from_numeral");
    }
}

test "numeric literal in comparison unifies with typed operand" {
    // When comparing a typed variable with a numeric literal,
    // the literal should unify to match the variable's type.
    // `answer == 42` desugars to `answer.is_eq(42)`, which dispatches to I64.is_eq(answer, 42),
    // which should unify 42's flex var with I64.
    const source =
        \\answer : I64
        \\answer = 42
        \\
        \\result = answer == 42
    ;

    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // First verify no type errors
    try test_env.assertNoErrors();

    // Verify that `answer` has type I64
    try test_env.assertDefType("answer", "I64");

    // Verify that `result` has type Bool (the result of ==)
    try test_env.assertDefType("result", "Bool");

    // Now verify that the binop expression's operands both have I64 type
    // Find the `result` definition and check the binop's operand types
    const ModuleEnv = @import("can").ModuleEnv;
    const defs_slice = test_env.module_env.store.sliceDefs(test_env.module_env.all_defs);
    var found_result = false;
    for (defs_slice) |def_idx| {
        const def = test_env.module_env.store.getDef(def_idx);
        const ptrn = test_env.module_env.store.getPattern(def.pattern);
        if (ptrn == .assign) {
            const def_name = test_env.module_env.getIdentStoreConst().getText(ptrn.assign.ident);
            if (std.mem.eql(u8, def_name, "result")) {
                found_result = true;
                // Get the expression - should be a binop
                const expr = test_env.module_env.store.getExpr(def.expr);
                try testing.expect(expr == .e_binop);
                const binop = expr.e_binop;

                // Check LHS type (should be I64)
                const lhs_var = ModuleEnv.varFrom(binop.lhs);
                try test_env.type_writer.write(lhs_var);
                const lhs_type = test_env.type_writer.get();
                try testing.expectEqualStrings("I64", lhs_type);

                // Check RHS type (the literal 42 - should also be I64 after unification)
                const rhs_var = ModuleEnv.varFrom(binop.rhs);
                try test_env.type_writer.write(rhs_var);
                const rhs_type = test_env.type_writer.get();
                try testing.expectEqualStrings("I64", rhs_type);

                // Verify that the RHS type var is actually resolved to a nominal type, not flex
                // This is what the interpreter's translateTypeVar should see
                const rhs_resolved = test_env.module_env.types.resolveVar(rhs_var);
                // After type checking, the RHS (numeric literal) should be unified to I64,
                // which is a nominal type (structure.nominal_type), NOT a flex var
                try testing.expect(rhs_resolved.desc.content == .structure);
                try testing.expect(rhs_resolved.desc.content.structure == .nominal_type);
                break;
            }
        }
    }
    try testing.expect(found_result);
}
