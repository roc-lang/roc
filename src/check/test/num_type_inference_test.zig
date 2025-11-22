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
