//! Tests for the expression evaluator
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const layout = @import("layout");
const builtins = @import("builtins");
const collections = @import("collections");
const serialization = @import("serialization");
const build_options = @import("build_options");

const helpers = @import("helpers.zig");
const TestEnv = @import("TestEnv.zig");
const eval = @import("../interpreter.zig");
const stack = @import("../stack.zig");

const LayoutStore = layout.Store;
const Can = can.Can;
const CIR = can.CIR;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const CompactWriter = collections.CompactWriter;
const testing = std.testing;
const test_allocator = testing.allocator;

const EvalError = eval.EvalError;
const runExpectInt = helpers.runExpectInt;
const runExpectBool = helpers.runExpectBool;
const runExpectError = helpers.runExpectError;
const runExpectStr = helpers.runExpectStr;

test "eval simple number" {
    try runExpectInt("1", 1, .no_trace);
    try runExpectInt("42", 42, .no_trace);
    try runExpectInt("-1234", -1234, .no_trace);
}

test "eval boolean literals" {
    try runExpectBool("True", true, .no_trace);
    try runExpectBool("False", false, .no_trace);
    try runExpectBool("Bool.True", true, .no_trace);
    try runExpectBool("Bool.False", false, .no_trace);
}

test "eval unary not operator" {
    try runExpectBool("!True", false, .no_trace);
    try runExpectBool("!False", true, .no_trace);
    try runExpectBool("!Bool.True", false, .no_trace);
    try runExpectBool("!Bool.False", true, .no_trace);
}

test "eval double negation" {
    try runExpectBool("!!True", true, .no_trace);
    try runExpectBool("!!False", false, .no_trace);
    try runExpectBool("!!!True", false, .no_trace);
    try runExpectBool("!!!False", true, .no_trace);
}

test "eval boolean in lambda expressions" {
    try runExpectBool("(|x| !x)(True)", false, .no_trace);
    try runExpectBool("(|x| !x)(False)", true, .no_trace);
    // Not implemented yet -- the closure return type is still flex var
    // try runExpectBool("(|x, y| x and y)(True, False)", false, .no_trace);
    // try runExpectBool("(|x, y| x or y)(False, True)", true, .no_trace);
    // try runExpectBool("(|x| x and !x)(True)", false, .no_trace);
    // try runExpectBool("(|x| x or !x)(False)", true, .no_trace);
}

test "eval unary not in conditional expressions" {
    try runExpectInt("if !True 42 else 99", 99, .no_trace);
    try runExpectInt("if !False 42 else 99", 42, .no_trace);
    try runExpectInt("if !!True 42 else 99", 42, .no_trace);
    try runExpectInt("if !!False 42 else 99", 99, .no_trace);
}

test "if-else" {
    try runExpectInt("if (1 == 1) 42 else 99", 42, .no_trace);
    try runExpectInt("if (1 == 2) 42 else 99", 99, .no_trace);
    try runExpectInt("if (5 > 3) 100 else 200", 100, .no_trace);
    try runExpectInt("if (3 > 5) 100 else 200", 200, .no_trace);
}

test "nested if-else" {
    try runExpectInt("if (1 == 1) (if (2 == 2) 100 else 200) else 300", 100, .no_trace);
    try runExpectInt("if (1 == 1) (if (2 == 3) 100 else 200) else 300", 200, .no_trace);
    try runExpectInt("if (1 == 2) (if (2 == 2) 100 else 200) else 300", 300, .no_trace);
}

test "eval single element record" {
    try runExpectInt("{x: 42}.x", 42, .no_trace);
    try runExpectInt("{foo: 100}.foo", 100, .no_trace);
    try runExpectInt("{bar: 1 + 2}.bar", 3, .no_trace);
}

test "eval multi-field record" {
    try runExpectInt("{x: 10, y: 20}.x", 10, .no_trace);
    try runExpectInt("{x: 10, y: 20}.y", 20, .no_trace);
    try runExpectInt("{a: 1, b: 2, c: 3}.a", 1, .no_trace);
    try runExpectInt("{a: 1, b: 2, c: 3}.b", 2, .no_trace);
    try runExpectInt("{a: 1, b: 2, c: 3}.c", 3, .no_trace);
}

test "nested record access" {
    try runExpectInt("{outer: {inner: 42}}.outer.inner", 42, .no_trace);
    try runExpectInt("{a: {b: {c: 100}}}.a.b.c", 100, .no_trace);
}

test "record field order independence" {
    try runExpectInt("{x: 1, y: 2}.x + {y: 2, x: 1}.x", 2, .no_trace);
    try runExpectInt("{a: 10, b: 20, c: 30}.b", 20, .no_trace);
    try runExpectInt("{c: 30, a: 10, b: 20}.b", 20, .no_trace);
}

test "arithmetic binops" {
    try runExpectInt("1 + 2", 3, .no_trace);
    try runExpectInt("5 - 3", 2, .no_trace);
    try runExpectInt("4 * 5", 20, .no_trace);
    try runExpectInt("10 // 2", 5, .no_trace);
    try runExpectInt("7 % 3", 1, .no_trace);
}

test "comparison binops" {
    try runExpectInt("if 1 < 2 100 else 200", 100, .no_trace);
    try runExpectInt("if 2 < 1 100 else 200", 200, .no_trace);
    try runExpectInt("if 5 > 3 100 else 200", 100, .no_trace);
    try runExpectInt("if 3 > 5 100 else 200", 200, .no_trace);
    try runExpectInt("if 10 <= 10 100 else 200", 100, .no_trace);
    try runExpectInt("if 10 <= 9 100 else 200", 200, .no_trace);
    try runExpectInt("if 10 >= 10 100 else 200", 100, .no_trace);
    try runExpectInt("if 9 >= 10 100 else 200", 200, .no_trace);
    try runExpectInt("if 5 == 5 100 else 200", 100, .no_trace);
    try runExpectInt("if 5 == 6 100 else 200", 200, .no_trace);
    try runExpectInt("if 5 != 6 100 else 200", 100, .no_trace);
    try runExpectInt("if 5 != 5 100 else 200", 200, .no_trace);
}

test "logical binops" {
    try runExpectInt("if True and True 1 else 0", 1, .no_trace);
    try runExpectInt("if True and False 1 else 0", 0, .no_trace);
    try runExpectInt("if False and True 1 else 0", 0, .no_trace);
    try runExpectInt("if False and False 1 else 0", 0, .no_trace);
    try runExpectInt("if True or True 1 else 0", 1, .no_trace);
    try runExpectInt("if True or False 1 else 0", 1, .no_trace);
    try runExpectInt("if False or True 1 else 0", 1, .no_trace);
    try runExpectInt("if False or False 1 else 0", 0, .no_trace);
}

test "unary minus" {
    try runExpectInt("-5", -5, .no_trace);
    try runExpectInt("-(-10)", 10, .no_trace);
    try runExpectInt("-(3 + 4)", -7, .no_trace);
    try runExpectInt("-0", 0, .no_trace);
}

test "parentheses and precedence" {
    try runExpectInt("2 + 3 * 4", 14, .no_trace);
    try runExpectInt("(2 + 3) * 4", 20, .no_trace);
    try runExpectInt("100 - 20 - 10", 70, .no_trace);
    try runExpectInt("100 - (20 - 10)", 90, .no_trace);
}

test "operator associativity - addition" {
    // Left associative: a + b + c should parse as (a + b) + c
    try runExpectInt("100 + 20 + 10", 130, .no_trace); // (100 + 20) + 10 = 130
    try runExpectInt("100 + (20 + 10)", 130, .no_trace); // Same result, but explicitly grouped

    // More complex case
    try runExpectInt("10 + 20 + 30 + 40", 100, .no_trace); // ((10 + 20) + 30) + 40 = 100
}

test "operator associativity - subtraction" {
    // Left associative: a - b - c should parse as (a - b) - c
    try runExpectInt("100 - 20 - 10", 70, .no_trace); // (100 - 20) - 10 = 70
    try runExpectInt("100 - (20 - 10)", 90, .no_trace); // Different result with explicit grouping

    // More complex case showing the difference
    try runExpectInt("100 - 50 - 25 - 5", 20, .no_trace); // ((100 - 50) - 25) - 5 = 20
    try runExpectInt("100 - (50 - (25 - 5))", 70, .no_trace); // Right associative would give 70
}

test "operator associativity - multiplication" {
    // Left associative: a * b * c should parse as (a * b) * c
    try runExpectInt("2 * 3 * 4", 24, .no_trace); // (2 * 3) * 4 = 24
    try runExpectInt("2 * (3 * 4)", 24, .no_trace); // Same result for multiplication

    // Chain of multiplications
    try runExpectInt("2 * 3 * 4 * 5", 120, .no_trace); // ((2 * 3) * 4) * 5 = 120
}

test "operator associativity - division" {
    // Left associative: a / b / c should parse as (a / b) / c
    // Note: Using integer division (//) for predictable integer results
    try runExpectInt("100 // 20 // 2", 2, .no_trace); // (100 // 20) // 2 = 5 // 2 = 2
    try runExpectInt("100 // (20 // 2)", 10, .no_trace); // Different result: 100 // 10 = 10

    // More complex case showing the difference
    try runExpectInt("1000 // 10 // 5 // 2", 10, .no_trace); // ((1000 // 10) // 5) // 2 = 10
    try runExpectInt("1000 // (10 // (5 // 2))", 200, .no_trace); // Right associative would give 200
}

test "operator associativity - modulo" {
    // Left associative: a % b % c should parse as (a % b) % c
    try runExpectInt("100 % 30 % 7", 3, .no_trace); // (100 % 30) % 7 = 10 % 7 = 3
    try runExpectInt("100 % (30 % 7)", 0, .no_trace); // Different result: 100 % 2 = 0

    // Another example
    try runExpectInt("50 % 20 % 6", 4, .no_trace); // (50 % 20) % 6 = 10 % 6 = 4
    try runExpectInt("50 % (20 % 6)", 0, .no_trace); // Right associative: 50 % 2 = 0
}

test "operator associativity - mixed precedence" {
    // Verify that precedence still works correctly with fixed associativity
    try runExpectInt("2 + 3 * 4", 14, .no_trace); // 2 + (3 * 4) = 14
    try runExpectInt("2 * 3 + 4", 10, .no_trace); // (2 * 3) + 4 = 10

    // More complex mixed operations
    try runExpectInt("10 - 2 * 3", 4, .no_trace); // 10 - (2 * 3) = 4
    try runExpectInt("100 // 5 + 10", 30, .no_trace); // (100 // 5) + 10 = 30
    try runExpectInt("100 // 5 % 3", 2, .no_trace); // (100 // 5) % 3 = 20 % 3 = 2
}

test "operator associativity - edge cases" {
    // Very long chains to ensure associativity is consistent
    try runExpectInt("1000 - 100 - 50 - 25 - 10 - 5", 810, .no_trace);
    // ((((1000 - 100) - 50) - 25) - 10) - 5 = 810

    // Complex nested expressions
    try runExpectInt("(100 - 50) - (30 - 10)", 30, .no_trace); // 50 - 20 = 30
    try runExpectInt("100 - (50 - 30) - 10", 70, .no_trace); // 100 - 20 - 10 = 70

    // Division chains that would overflow if right-associative
    try runExpectInt("1000000 // 1000 // 100 // 10", 1, .no_trace);
    // (((1000000 // 1000) // 100) // 10) = 1

    // Modulo chains
    try runExpectInt("1000 % 300 % 40 % 7", 6, .no_trace);
    // ((1000 % 300) % 40) % 7 = (100 % 40) % 7 = 20 % 7 = 6
}

test "comparison operators - non-associative" {
    // Comparison operators should be non-associative
    // These should work with parentheses
    try runExpectBool("(5 > 3)", true, .no_trace); // true
    try runExpectBool("(10 < 20)", true, .no_trace); // true
    try runExpectBool("(5 >= 5)", true, .no_trace); // true
    try runExpectBool("(10 <= 9)", false, .no_trace); // false

    // But chaining without parentheses should fail to parse
    // We can't test parse errors in eval tests, so we just verify the operators work
}

test "operator associativity - documentation" {
    // This test documents the expected associativity behavior after fixes

    // LEFT ASSOCIATIVE (most arithmetic operators)
    // a op b op c = (a op b) op c
    try runExpectInt("8 - 4 - 2", 2, .no_trace); // (8-4)-2 = 2, NOT 8-(4-2) = 6
    try runExpectInt("16 // 4 // 2", 2, .no_trace); // (16//4)//2 = 2, NOT 16//(4//2) = 8

    // NON-ASSOCIATIVE (comparison operators)
    // Can't chain without parentheses
    try runExpectBool("(5 > 3) and (3 > 1)", true, .no_trace); // Must use parentheses

    // RIGHT ASSOCIATIVE (logical operators)
    // a op b op c = a op (b op c)
    // Note: && and || are right associative in Roc
    // This is mostly relevant for short-circuiting behavior
}

test "error test - divide by zero" {
    try runExpectError("5 // 0", EvalError.DivisionByZero, .no_trace);
    try runExpectError("10 % 0", EvalError.DivisionByZero, .no_trace);
}

test "error test - crash statement" {
    // Test crash statement in a block (crash is a statement, not an expression)
    try runExpectError(
        \\{
        \\    crash "test"
        \\    0
        \\}
    , EvalError.Crash, .no_trace);

    // Test crash in block with final expression
    try runExpectError(
        \\{
        \\    crash "This is a crash statement"
        \\    42
        \\}
    , EvalError.Crash, .no_trace);
}

test "crash message storage and retrieval - direct API test" {
    // Test the getCrashMsg() API directly by simulating what happens during a crash
    const test_message = "Direct API test message";

    const resources = try helpers.parseAndCanonicalizeExpr(testing.allocator, "42");
    defer helpers.cleanupParseAndCanonical(testing.allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(testing.allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = TestEnv.init(testing.allocator);
    defer test_env_instance.deinit();

    var interpreter = try eval.Interpreter.init(
        testing.allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());
    test_env_instance.setInterpreter(&interpreter);

    // Test that crash functionality works through RocOps
    // Before crash, getCrashMsg should return null
    try testing.expect(interpreter.getCrashMsg() == null);

    // Simulate what happens when roc_ops.crash() is called
    const crash_args = builtins.host_abi.RocCrashed{
        .utf8_bytes = @constCast(test_message.ptr),
        .len = test_message.len,
    };

    // Call the crash function directly through test RocOps
    test_env_instance.get_ops().roc_crashed(&crash_args, test_env_instance.get_ops().env);

    // After crash, getCrashMsg should return the crash message
    const crash_msg = interpreter.getCrashMsg();
    try testing.expect(crash_msg != null);
    try testing.expectEqualStrings(test_message, crash_msg.?);
}

test "tuples" {
    // 2-tuple
    const expected_elements1 = &[_]helpers.ExpectedElement{
        .{ .index = 0, .value = 10 },
        .{ .index = 1, .value = 20 },
    };
    try helpers.runExpectTuple("(10, 20)", expected_elements1, .no_trace);

    // Tuple with elements from arithmetic expressions
    const expected_elements3 = &[_]helpers.ExpectedElement{
        .{ .index = 0, .value = 6 },
        .{ .index = 1, .value = 15 },
    };
    try helpers.runExpectTuple("(5 + 1, 5 * 3)", expected_elements3, .no_trace);
}

test "simple lambdas" {
    try runExpectInt("(|x| x + 1)(5)", 6, .no_trace);
    try runExpectInt("(|x| x * 2 + 1)(10)", 21, .no_trace);
    try runExpectInt("(|x| x - 3)(8)", 5, .no_trace);
    try runExpectInt("(|x| 100 - x)(25)", 75, .no_trace);
    try runExpectInt("(|x| 5)(99)", 5, .no_trace);
    try runExpectInt("(|x| x + x)(7)", 14, .no_trace);
}

test "multi-parameter lambdas" {
    try runExpectInt("(|x, y| x + y)(3, 4)", 7, .no_trace);
    try runExpectInt("(|x, y| x * y)(10, 20)", 200, .no_trace);
    try runExpectInt("(|a, b, c| a + b + c)(1, 2, 3)", 6, .no_trace);
}

test "lambdas with if-then bodies" {
    try runExpectInt("(|x| if x > 0 x else 0)(5)", 5, .no_trace);
    try runExpectInt("(|x| if x > 0 x else 0)(-3)", 0, .no_trace);
    try runExpectInt("(|x| if x == 0 1 else x)(0)", 1, .no_trace);
    try runExpectInt("(|x| if x == 0 1 else x)(42)", 42, .no_trace);
}

test "lambdas with unary minus" {
    try runExpectInt("(|x| -x)(5)", -5, .no_trace);
    try runExpectInt("(|x| -x)(0)", 0, .no_trace);
    try runExpectInt("(|x| -x)(-3)", 3, .no_trace);
    try runExpectInt("(|x| -5)(999)", -5, .no_trace);
    try runExpectInt("(|x| if True -x else 0)(5)", -5, .no_trace);
    try runExpectInt("(|x| if True -10 else x)(999)", -10, .no_trace);
}

test "lambdas closures" {
    // Curried functions still have interpreter issues with TypeMismatch
    return error.SkipZigTest;
    // try runExpectInt("(|a| |b| a * b)(5)(10)", 50, .no_trace);
    // try runExpectInt("(((|a| |b| |c| a + b + c)(100))(20))(3)", 123, .no_trace);
    // try runExpectInt("(|a, b, c| |d| a + b + c + d)(10, 20, 5)(7)", 42, .no_trace);
    // try runExpectInt("(|y| (|x| (|z| x + y + z)(3))(2))(1)", 6, .no_trace);
}

test "lambdas with capture" {
    try runExpectInt(
        \\{
        \\    x = 10
        \\    f = |y| x + y
        \\    f(5)
        \\}
    , 15, .no_trace);

    try runExpectInt(
        \\{
        \\    x = 20
        \\    y = 30
        \\    f = |z| x + y + z
        \\    f(10)
        \\}
    , 60, .no_trace);
}

test "lambdas nested closures" {
    // Nested closures still have interpreter issues with TypeMismatch
    return error.SkipZigTest;
    // try runExpectInt(
    //     \\(((|a| {
    //     \\    a_loc = a * 2
    //     \\    |b| {
    //     \\        b_loc = a_loc + b
    //     \\        |c| b_loc + c
    //     \\    }
    //     \\})(100))(20))(3)
    // , 223, .no_trace);
}

// Helper function to test that evaluation succeeds without checking specific values
fn runExpectSuccess(src: []const u8, should_trace: enum { trace, no_trace }) !void {
    var test_env_instance = TestEnv.init(testing.allocator);
    defer test_env_instance.deinit();

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(std.testing.allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var interpreter = try eval.Interpreter.init(
        std.testing.allocator,
        resources.module_env,
        &eval_stack,
        &layout_cache,
        &resources.module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    if (should_trace == .trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }

    const result = interpreter.eval(resources.expr_idx, test_env_instance.get_ops());

    if (should_trace == .trace) {
        interpreter.endTrace();
    }

    // Just verify that evaluation succeeded
    _ = try result;
}

test "integer type evaluation" {
    // Test integer types to verify basic evaluation works
    // This should help us debug why 255u8 shows as 42 in REPL
    try runExpectInt("255u8", 255, .no_trace);
    try runExpectInt("42i32", 42, .no_trace);
    try runExpectInt("123i64", 123, .no_trace);
}

test "decimal literal evaluation" {
    // Test basic decimal literals - these should be parsed and evaluated correctly
    try runExpectSuccess("1.5dec", .no_trace);
    try runExpectSuccess("0.0dec", .no_trace);
    try runExpectSuccess("123.456dec", .no_trace);
    try runExpectSuccess("-1.5dec", .no_trace);
}

test "float literal evaluation" {
    // Test float literals - these should work correctly
    try runExpectSuccess("3.14f64", .no_trace);
    try runExpectSuccess("2.5f32", .no_trace);
    try runExpectSuccess("-3.14f64", .no_trace);
    try runExpectSuccess("0.0f32", .no_trace);
}

test "comprehensive integer literal formats" {
    // Test various integer literal formats and precisions

    // Unsigned integers
    try runExpectInt("0u8", 0, .no_trace);
    try runExpectInt("255u8", 255, .no_trace);
    try runExpectInt("1000u16", 1000, .no_trace);
    try runExpectInt("65535u16", 65535, .no_trace);
    try runExpectInt("100000u32", 100000, .no_trace);
    try runExpectInt("999999999u64", 999999999, .no_trace);

    // Signed integers
    try runExpectInt("-128i8", -128, .no_trace);
    try runExpectInt("127i8", 127, .no_trace);
    try runExpectInt("-32768i16", -32768, .no_trace);
    try runExpectInt("32767i16", 32767, .no_trace);
    try runExpectInt("-2147483648i32", -2147483648, .no_trace);
    try runExpectInt("2147483647i32", 2147483647, .no_trace);
    try runExpectInt("-999999999i64", -999999999, .no_trace);
    try runExpectInt("999999999i64", 999999999, .no_trace);

    // Default integer type (i64)
    try runExpectInt("42", 42, .no_trace);
    try runExpectInt("-1234", -1234, .no_trace);
    try runExpectInt("0", 0, .no_trace);
}

test "hexadecimal and binary integer literals" {
    // Test alternative number bases
    try runExpectInt("0xFF", 255, .no_trace);
    try runExpectInt("0x10", 16, .no_trace);
    try runExpectInt("0xDEADBEEF", 3735928559, .no_trace);
    try runExpectInt("0b1010", 10, .no_trace);
    try runExpectInt("0b11111111", 255, .no_trace);
    try runExpectInt("0b0", 0, .no_trace);
}

test "scientific notation literals" {
    // Test scientific notation - these get parsed as decimals or floats
    try runExpectSuccess("1e5", .no_trace);
    try runExpectSuccess("2.5e10", .no_trace);
    try runExpectSuccess("1.5e-5", .no_trace);
    try runExpectSuccess("-1.5e-5", .no_trace);
}

test "string literals and interpolation" {
    // Test basic string literals
    try runExpectSuccess("\"Hello, World!\"", .no_trace);
    try runExpectSuccess("\"\"", .no_trace);
    try runExpectSuccess("\"Roc\"", .no_trace);

    // Test string interpolation
    try runExpectSuccess(
        \\{
        \\    hello = "Hello"
        \\    world = "World"
        \\    "${hello} ${world}"
        \\}
    , .no_trace);
}

test "string refcount - basic literal" {
    // Test basic string literal creation and cleanup
    try runExpectStr("\"Hello, World!\"", "Hello, World!", .no_trace);
}

test "polymorphic identity function" {
    // Test the identity function with different types
    const code =
        \\{
        \\    identity = |val| val
        \\    num = identity(5)
        \\    str = identity("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;
    try runExpectStr(code, "Hello", .no_trace);
}

test "direct polymorphic function usage" {
    // Test that polymorphic functions work correctly when used directly
    // This is valid in rank-1 Hindley-Milner type systems
    const code =
        \\{
        \\    id = |x| x
        \\
        \\    # Direct calls to identity with different types
        \\    num1 = id(10)
        \\    str1 = id("Test")
        \\    num2 = id(20)
        \\
        \\    # Verify all values are correct
        \\    if (num1 == 10)
        \\        if (num2 == 20)
        \\            str1
        \\        else
        \\            "Failed2"
        \\    else
        \\        "Failed1"
        \\}
    ;
    try runExpectStr(code, "Test", .no_trace);
}

test "multiple polymorphic instantiations" {
    // Test that let-bound polymorphic values can be instantiated multiple times
    // This tests valid rank-1 polymorphism patterns
    const code =
        \\{
        \\    id = |x| x
        \\
        \\    # Test polymorphic identity with different types
        \\    num1 = id(42)
        \\    str1 = id("Hello")
        \\    num2 = id(100)
        \\
        \\    # Verify all results
        \\    if (num1 == 42)
        \\        if (num2 == 100)
        \\            str1
        \\        else
        \\            "Failed2"
        \\    else
        \\        "Failed1"
        \\}
    ;
    try runExpectStr(code, "Hello", .no_trace);
}

test "string refcount - large string literal" {
    // Test large string that requires heap allocation and reference counting
    // This string is longer than SMALL_STR_MAX_LENGTH to trigger heap allocation
    const large_str = "This is a very long string that definitely exceeds the small string optimization limit in RocStr and will require heap allocation with reference counting";
    try runExpectStr("\"This is a very long string that definitely exceeds the small string optimization limit in RocStr and will require heap allocation with reference counting\"", large_str, .no_trace);
}

test "string refcount - heap allocated string" {
    // Test another large string to exercise reference counting with heap allocation
    const large_str = "This is a very long string that definitely exceeds the small string optimization limit and requires heap allocation";

    // Test the large string without trace since it's working
    try runExpectStr("\"This is a very long string that definitely exceeds the small string optimization limit and requires heap allocation\"", large_str, .no_trace);
}

test "string refcount - small string optimization" {
    // Test small string (≤23 bytes) that uses inline storage instead of heap allocation
    // This should show different behavior in the trace (no heap allocation)
    try runExpectStr("\"Small string test\"", "Small string test", .no_trace);
}

test "string refcount - empty string" {
    // Test empty string as a special case for reference counting
    // Empty strings are typically optimized differently
    try runExpectStr("\"\"", "", .no_trace);
}

test "string refcount - boundary case 25 bytes" {
    // Test string that's 25 bytes - should trigger heap allocation (>23 bytes)
    const boundary_str = "1234567890123456789012345"; // 25 bytes - should be big
    try runExpectStr("\"1234567890123456789012345\"", boundary_str, .no_trace);
}

test "string refcount - max small string 23 bytes" {
    // Test string that's exactly 23 bytes - should still use small string optimization
    const max_small_str = "12345678901234567890123"; // 23 bytes - should be small
    try runExpectStr("\"12345678901234567890123\"", max_small_str, .no_trace);
}

test "string refcount - conditional strings" {
    // Test string reference counting with conditional expressions
    // This exercises reference counting when strings are used in if-else branches
    try runExpectStr("if True \"This is a large string that exceeds small string optimization\" else \"Short\"", "This is a large string that exceeds small string optimization", .no_trace);
}

test "string refcount - simpler record test" {
    // Test record containing integers first to see if the issue is record-specific or string-specific
    try runExpectInt("{foo: 42}.foo", 42, .no_trace);
}

test "string refcount - mixed string sizes" {
    // Test mixture of small and large strings in conditional expressions
    // Exercise reference counting across different string storage types
    try runExpectStr("if False \"Small\" else \"This is a very long string that definitely exceeds the small string optimization limit and requires heap allocation\"", "This is a very long string that definitely exceeds the small string optimization limit and requires heap allocation", .no_trace);
}

test "string refcount - nested conditionals with strings" {
    // Test nested conditional expressions with strings to exercise complex control flow
    // This tests reference counting when strings are created and destroyed in nested scopes
    try runExpectStr("if True (if False \"Inner small\" else \"Inner large string that exceeds small string optimization\") else \"Outer\"", "Inner large string that exceeds small string optimization", .no_trace);
}

test "string refcount - record field access small string" {
    // Test record field access with small strings (uses inline storage)
    try runExpectStr("{foo: \"Hello\"}.foo", "Hello", .no_trace);
}

test "string refcount - record field access large string" {
    // Test record field access with large strings (uses heap allocation)
    const large_str = "This is a very long string that definitely exceeds the small string optimization limit";
    try runExpectStr("{foo: \"This is a very long string that definitely exceeds the small string optimization limit\"}.foo", large_str, .no_trace);
}

test "string refcount - record with empty string" {
    // Test record field access with empty string (special case)
    try runExpectStr("{empty: \"\"}.empty", "", .no_trace);
}

test "string refcount - simple integer closure" {
    // Test basic closure with integer first to see if the issue is closure-specific
    try runExpectInt("(|x| x)(42)", 42, .no_trace);
}

test "string refcount - simple string closure" {
    try runExpectStr("(|s| s)(\"Test\")", "Test", .no_trace);
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
    const source = "5 + 8";

    const gpa = test_allocator;
    var test_env_instance = TestEnv.init(gpa);
    defer test_env_instance.deinit();

    // Create original ModuleEnv
    var original_env = try ModuleEnv.init(gpa, source);
    defer original_env.deinit();

    original_env.common.source = source;
    original_env.module_name = "TestModule";
    try original_env.common.calcLineStarts(original_env.gpa);

    // Parse the source code
    var parse_ast = try parse.parseExpr(&original_env.common, original_env.gpa);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try original_env.initCIRFields(gpa, "test");

    // Create canonicalizer
    var czer = try Can.init(&original_env, &parse_ast, null);
    defer czer.deinit();

    // Canonicalize the expression
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonicalized_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeFailure;
    };

    // Type check the expression
    var checker = try Check.init(gpa, &original_env.types, &original_env, &.{}, &original_env.store.regions);
    defer checker.deinit();

    _ = try checker.checkExpr(canonicalized_expr_idx.get_idx());

    // Test 1: Evaluate with the original ModuleEnv
    {
        var eval_stack = try stack.Stack.initCapacity(gpa, 1024);
        defer eval_stack.deinit();

        var layout_cache = try LayoutStore.init(&original_env, &original_env.types);
        defer layout_cache.deinit();

        var interpreter = try eval.Interpreter.init(
            gpa,
            &original_env,
            &eval_stack,
            &layout_cache,
            &original_env.types,
        );
        defer interpreter.deinit(test_env_instance.get_ops());

        const result = try interpreter.eval(canonicalized_expr_idx.get_idx(), test_env_instance.get_ops());

        try testing.expectEqual(@as(i128, 13), result.asI128());
    }

    // Test 2: Full serialization and deserialization with interpreter evaluation
    {
        var arena = std.heap.ArenaAllocator.init(gpa);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        var tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();
        const tmp_file = try tmp_dir.dir.createFile("test_module_env.compact", .{ .read = true });
        defer tmp_file.close();

        var writer = CompactWriter{
            .iovecs = .{},
            .total_bytes = 0,
            .allocated_memory = .{},
        };
        defer writer.deinit(arena_alloc);

        // Allocate space for ModuleEnv and serialize
        const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv);
        const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv);
        const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
        try serialized_ptr.serialize(&original_env, arena_alloc, &writer);

        // Write to file
        try writer.writeGather(arena_alloc, tmp_file);

        // Read back from file
        const file_size = try tmp_file.getEndPos();
        const buffer = try gpa.alignedAlloc(u8, @alignOf(ModuleEnv), @intCast(file_size));
        defer gpa.free(buffer);
        _ = try tmp_file.pread(buffer, 0);

        // Deserialize the ModuleEnv
        const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + env_start_offset)));
        const deserialized_env = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), gpa, source, "TestModule");
        // Note: deserialized_env points into buffer and doesn't own its memory, so we don't call deinit()
        // The buffer will be freed by the defer on line 807

        // Verify basic deserialization worked
        try testing.expectEqualStrings("TestModule", deserialized_env.module_name);
        try testing.expectEqualStrings(source, deserialized_env.common.source);

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
            var eval_stack = try stack.Stack.initCapacity(gpa, 1024);
            defer eval_stack.deinit();

            var layout_cache = try LayoutStore.init(deserialized_env, &deserialized_env.types);
            defer layout_cache.deinit();

            var interpreter = try eval.Interpreter.init(
                gpa,
                deserialized_env,
                &eval_stack,
                &layout_cache,
                &deserialized_env.types,
            );
            defer interpreter.deinit(test_env_instance.get_ops());

            const result = try interpreter.eval(canonicalized_expr_idx.get_idx(), test_env_instance.get_ops());

            // Verify we get the same result from the deserialized ModuleEnv
            try testing.expectEqual(@as(i128, 13), result.asI128());
        }
    }
}

test "eval static dispatch - basic syntax and name lookup" {
    // This test verifies that static dispatch syntax (x.method()) is parsed
    // and that the interpreter can do basic name lookup for methods.
    // Note: Full static dispatch on nominal types requires more work.
    const allocator = testing.allocator;

    // Test that static dispatch syntax is accepted and tries to find the method
    // This tests basic method lookup by name in the current module
    const source =
        \\module []
        \\
        \\area = |x| 75
        \\
        \\obj = { value: 42 }
        \\
        \\main = obj.area()
    ;

    // Create module environment
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    module_env.module_name = "Test";
    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var parse_ast = try parse.parse(&module_env.common, allocator);
    defer parse_ast.deinit(allocator);

    // Initialize CIR fields
    try module_env.initCIRFields(allocator, "Test");

    // Canonicalize
    var czer = try Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();
    try czer.canonicalizeFile();

    // Type check
    var checker = try Check.init(allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();
    try checker.checkDefs();

    // Verify no type errors
    try testing.expectEqual(@as(usize, 0), checker.problems.problems.items.len);

    // Find the main expression
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    var main_expr_idx: ?CIR.Expr.Idx = null;

    for (defs) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        const pattern = module_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = module_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "main")) {
                main_expr_idx = def.expr;
                break;
            }
        }
    }

    try testing.expect(main_expr_idx != null);

    // Set up the interpreter
    var eval_stack = try stack.Stack.initCapacity(allocator, 4096);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    // Create interpreter
    var interpreter = try eval.Interpreter.init(
        allocator,
        &module_env,
        &eval_stack,
        &layout_cache,
        &module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    // Evaluate the main expression
    const result = try interpreter.eval(main_expr_idx.?, test_env_instance.get_ops());

    // Verify the result, which should be 75
    try testing.expectEqual(@as(i128, 75), result.asI128());
}

test "eval static dispatch - method call with arguments" {
    // Test static dispatch with arguments passed to the method
    const allocator = testing.allocator;

    const source =
        \\module []
        \\
        \\add = |x, y| y + 32
        \\
        \\obj = { value: 10 }
        \\
        \\main = obj.add(10)
    ;

    // Create module environment
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    module_env.module_name = "Test";
    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    // Parse the module
    var parse_ast = try parse.parse(&module_env.common, allocator);
    defer parse_ast.deinit(allocator);

    // Initialize CIR fields
    try module_env.initCIRFields(allocator, "Test");

    // Canonicalize
    var czer = try Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();
    try czer.canonicalizeFile();

    // Type check
    var checker = try Check.init(allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();
    try checker.checkDefs();

    // Verify no type errors
    try testing.expectEqual(@as(usize, 0), checker.problems.problems.items.len);

    // Find the main expression
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    var main_expr_idx: ?CIR.Expr.Idx = null;

    for (defs) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        const pattern = module_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = module_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "main")) {
                main_expr_idx = def.expr;
                break;
            }
        }
    }

    try testing.expect(main_expr_idx != null);

    // Set up the interpreter
    var eval_stack = try stack.Stack.initCapacity(allocator, 4096);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    // Create interpreter
    var interpreter = try eval.Interpreter.init(
        allocator,
        &module_env,
        &eval_stack,
        &layout_cache,
        &module_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    // Evaluate the main expression
    const result = try interpreter.eval(main_expr_idx.?, test_env_instance.get_ops());

    // Verify the result, which should be 42 (10 + 32)
    try testing.expectEqual(@as(i128, 42), result.asI128());
}

test "eval static dispatch - simple import test" {
    // First understand how imports work before tackling cross-module static dispatch
    const allocator = testing.allocator;

    // Math module with simple functions
    const math_source =
        \\module [addFive]
        \\
        \\addFive = |x| x + 5
    ;

    // Main module that imports and uses the function
    const main_source =
        \\module []
        \\
        \\import Math exposing [addFive]
        \\
        \\main = addFive(10)
    ;

    // Create module environments
    var math_env = try ModuleEnv.init(allocator, math_source);
    defer math_env.deinit();

    math_env.module_name = "Math";
    math_env.common.source = math_source;
    try math_env.common.calcLineStarts(allocator);

    // Parse the Math module
    var math_parse = try parse.parse(&math_env.common, allocator);
    defer math_parse.deinit(allocator);

    // Initialize CIR fields for Math
    try math_env.initCIRFields(allocator, "Math");

    // Canonicalize Math module
    var math_czer = try Can.init(&math_env, &math_parse, null);
    defer math_czer.deinit();
    try math_czer.canonicalizeFile();

    // Type check Math module
    var math_checker = try Check.init(allocator, &math_env.types, &math_env, &.{}, &math_env.store.regions);
    defer math_checker.deinit();
    try math_checker.checkDefs();

    // Create Main module environment
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    // Parse the Main module
    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    // Initialize CIR fields for Main
    try main_env.initCIRFields(allocator, "Main");

    // Canonicalize Main module with Math module available
    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Math", &math_env);
    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    // Find the main expression
    const defs = main_env.store.sliceDefs(main_env.all_defs);
    var main_expr_idx: ?CIR.Expr.Idx = null;

    std.debug.print("\n=== Simple import test definitions ===\n", .{});
    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = main_env.getIdent(pattern.assign.ident);
            const expr = main_env.store.getExpr(def.expr);
            std.debug.print("  {s}: {s}\n", .{ ident_text, @tagName(expr) });
            if (std.mem.eql(u8, ident_text, "main")) {
                main_expr_idx = def.expr;

                // Print more info about the expression
                switch (expr) {
                    .e_call => |call| {
                        const args = main_env.store.sliceExpr(call.args);
                        if (args.len > 0) {
                            const fn_expr = main_env.store.getExpr(args[0]);
                            std.debug.print("    -> e_call with function: {s}\n", .{@tagName(fn_expr)});
                        }
                    },
                    else => {},
                }
            }
        }
    }
    std.debug.print("================================\n\n", .{});

    // For now just check that canonicalization worked
    try testing.expect(main_expr_idx != null);
}

test "eval - minimal import definition issue" {
    // Minimal test to reproduce the import definition issue
    const allocator = testing.allocator;

    // Math module with a simple value
    const math_source =
        \\module [myValue]
        \\
        \\myValue = 42
    ;

    // Main module that imports the value
    const main_source =
        \\module []
        \\
        \\import Math exposing [myValue]
        \\
        \\main = myValue
    ;

    // Create and set up Math module
    var math_env = try ModuleEnv.init(allocator, math_source);
    defer math_env.deinit();
    math_env.module_name = "Math";
    math_env.common.source = math_source;
    try math_env.common.calcLineStarts(allocator);

    var math_parse = try parse.parse(&math_env.common, allocator);
    defer math_parse.deinit(allocator);

    try math_env.initCIRFields(allocator, "Math");

    var math_czer = try Can.init(&math_env, &math_parse, null);
    defer math_czer.deinit();
    try math_czer.canonicalizeFile();

    // Create and set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();
    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Math", &math_env);

    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    // Check what definitions were created in Main
    const defs = main_env.store.sliceDefs(main_env.all_defs);

    std.debug.print("\n=== Minimal import test - Main module definitions ===\n", .{});
    for (defs, 0..) |def_idx, i| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = main_env.getIdent(pattern.assign.ident);
            const expr = main_env.store.getExpr(def.expr);
            std.debug.print("  Def[{}]: '{s}' -> {s} (pattern_idx={}, expr_idx={})\n", .{
                i, ident_text, @tagName(expr),
                @intFromEnum(def.pattern), @intFromEnum(def.expr)
            });
        }
    }
    std.debug.print("======================================================\n\n", .{});

    // The issue: We expect TWO definitions in Main:
    // 1. myValue -> e_lookup_external (from the import)
    // 2. main -> e_lookup_local (referencing myValue)
    // But we probably only get one (main)

    var found_myValue = false;
    var found_main = false;

    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "myValue")) {
                found_myValue = true;
                // Check that it's an e_lookup_external
                const expr = main_env.store.getExpr(def.expr);
                try testing.expect(expr == .e_lookup_external);
            } else if (std.mem.eql(u8, ident_text, "main")) {
                found_main = true;
            }
        }
    }

    try testing.expect(found_main);

    // This is the key test - does importing create a definition?
    if (!found_myValue) {
        std.debug.print("ERROR: Import did not create a definition for 'myValue' in Main module!\n", .{});
        std.debug.print("This is the root cause of the cross-module static dispatch issue.\n", .{});
    }
    try testing.expect(found_myValue);
}

test "eval static dispatch - cross-module method call" {
    // Record layout partially fixed but still has TypeMismatch in handleLambdaReturn
    // TODO: Fix type handling for cross-module function returns
    if (true) return error.SkipZigTest;

    std.debug.print("\n=== STARTING 3-MODULE STATIC DISPATCH TEST ===\n", .{});
    // Test static dispatch across 3 modules where:
    // 1. Types module defines a nominal type with a method
    // 2. Factory module imports the type and has a function that returns it
    // 3. Main module imports Factory (NOT Types) and does static dispatch on the returned value
    const allocator = testing.allocator;

    // Module 1: Defines methods that work on records
    const types_source =
        \\module [getX, getY]
        \\
        \\getX = |point| point.x
        \\
        \\getY = |point| point.y
    ;

    // Module 2: Creates records and re-exports them
    const factory_source =
        \\module [makePoint]
        \\
        \\makePoint = |xVal, yVal| { x: xVal, y: yVal }
    ;

    // Module 3: Only imports Factory, gets a record, and calls static dispatch
    // Main does NOT import Types, but should still be able to call getX!
    const main_source =
        \\module []
        \\
        \\import Factory exposing [makePoint]
        \\
        \\myPoint = makePoint(10, 20)
        \\
        \\main = myPoint.getX()
    ;

    // Create and set up Types module
    var types_env = try ModuleEnv.init(allocator, types_source);
    defer types_env.deinit();

    types_env.module_name = "Types";
    types_env.common.source = types_source;
    try types_env.common.calcLineStarts(allocator);

    var types_parse = try parse.parse(&types_env.common, allocator);
    defer types_parse.deinit(allocator);

    try types_env.initCIRFields(allocator, "Types");

    var types_czer = try Can.init(&types_env, &types_parse, null);
    defer types_czer.deinit();
    try types_czer.canonicalizeFile();

    var types_checker = try Check.init(allocator, &types_env.types, &types_env, &.{}, &types_env.store.regions);
    defer types_checker.deinit();
    try types_checker.checkDefs();

    std.debug.print("Types module problems: {}\n", .{types_checker.problems.problems.items.len});
    for (types_checker.problems.problems.items) |problem| {
        std.debug.print("  Problem: {}\n", .{problem});
    }

    try testing.expectEqual(@as(usize, 0), types_checker.problems.problems.items.len);

    // Create and set up Factory module
    var factory_env = try ModuleEnv.init(allocator, factory_source);
    defer factory_env.deinit();

    factory_env.module_name = "Factory";
    factory_env.common.source = factory_source;
    try factory_env.common.calcLineStarts(allocator);

    // Factory depends on Types
    var factory_deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer factory_deps.deinit();
    try factory_deps.put("Types", &types_env);

    var factory_parse = try parse.parse(&factory_env.common, allocator);
    defer factory_parse.deinit(allocator);

    try factory_env.initCIRFields(allocator, "Factory");

    var factory_czer = try Can.init(&factory_env, &factory_parse, &factory_deps);
    defer factory_czer.deinit();
    try factory_czer.canonicalizeFile();

    const factory_other_modules = [_]*ModuleEnv{&types_env};
    var factory_checker = try Check.init(allocator, &factory_env.types, &factory_env, &factory_other_modules, &factory_env.store.regions);
    defer factory_checker.deinit();
    try factory_checker.checkDefs();

    std.debug.print("Factory module problems: {}\n", .{factory_checker.problems.problems.items.len});
    try testing.expectEqual(@as(usize, 0), factory_checker.problems.problems.items.len);

    // Create and set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    // Main only depends on Factory, NOT on Types!
    var main_deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer main_deps.deinit();
    try main_deps.put("Factory", &factory_env);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    var main_czer = try Can.init(&main_env, &main_parse, &main_deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    // Main can see Factory and Types (transitively through Factory)
    const main_other_modules = [_]*ModuleEnv{ &factory_env, &types_env };
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &main_other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    std.debug.print("Main module problems: {}\n", .{main_checker.problems.problems.items.len});
    for (main_checker.problems.problems.items) |problem| {
        std.debug.print("  Problem: {}\n", .{problem});
    }
    // Should have no problems if static dispatch works correctly
    try testing.expectEqual(@as(usize, 0), main_checker.problems.problems.items.len);

    // Find the main expression
    const defs = main_env.store.sliceDefs(main_env.all_defs);
    var main_expr_idx: ?CIR.Expr.Idx = null;

    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "main")) {
                main_expr_idx = def.expr;
            }
        }
    }

    try testing.expect(main_expr_idx != null);

    // Debug: Print what we're about to evaluate
    std.debug.print("\nAbout to evaluate main expression\n", .{});
    const main_expr = main_env.store.getExpr(main_expr_idx.?);
    std.debug.print("Main expr type: {s}\n", .{@tagName(main_expr)});
    if (main_expr == .e_dot_access) {
        std.debug.print("  field: {s}\n", .{main_env.getIdent(main_expr.e_dot_access.field_name)});
        const receiver_expr = main_env.store.getExpr(main_expr.e_dot_access.receiver);
        std.debug.print("  receiver type: {s}\n", .{@tagName(receiver_expr)});
    } else if (main_expr == .e_call) {
        const args = main_env.store.sliceExpr(main_expr.e_call.args);
        if (args.len > 0) {
            const func_expr = main_env.store.getExpr(args[0]);
            std.debug.print("  function expr type: {s}\n", .{@tagName(func_expr)});
            if (func_expr == .e_lookup_external) {
                std.debug.print("    lookup_external module_idx={}, target_node_idx={}\n", .{
                    @intFromEnum(func_expr.e_lookup_external.module_idx),
                    func_expr.e_lookup_external.target_node_idx
                });
            }
        }
    }

    // Set up interpreter
    var eval_stack = try stack.Stack.initCapacity(allocator, 4096);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(&main_env, &main_env.types);
    defer layout_cache.deinit();

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    // Interpreter needs all modules: Factory and Types
    const other_envs = [_]*const ModuleEnv{ &factory_env, &types_env };
    var interpreter = try eval.Interpreter.initWithModules(
        allocator,
        &main_env,
        &other_envs,
        &eval_stack,
        &layout_cache,
        &main_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    // Enable tracing for debugging
    if (build_options.trace_eval) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
        defer interpreter.endTrace();
    }

    // Evaluate the main expression
    const result = try interpreter.eval(main_expr_idx.?, test_env_instance.get_ops());

    // Verify the result: myPoint.getX() should return 10
    // This tests that static dispatch can find methods from a module
    // that was never directly imported!
    try testing.expectEqual(@as(i128, 10), result.asI128());

    std.debug.print("\n✅ 3-MODULE STATIC DISPATCH TEST PASSED!\n", .{});
    std.debug.print("Main successfully called getX() on a record from Factory,\n", .{});
    std.debug.print("where getX is defined in Types module!\n", .{});
    std.debug.print("Main never imported Types, demonstrating true static dispatch.\n", .{});
}

test "cross-module record field access bug" {
    // This test reproduces the issue where records returned from cross-module functions
    // lose their field layout information, causing a panic when trying to access fields
    //
    // The panic happens at: src/eval/interpreter.zig:2832 in handleRecordFields
    // when record_data.getFields() returns an empty range (count = 0)
    //
    // Partially fixed: Record layout preservation works but there's a type mismatch issue
    // TODO: Fix TypeMismatch in handleLambdaReturn for cross-module records
    if (true) return error.SkipZigTest;

    const allocator = testing.allocator;

    // Module that creates and returns records
    const factory_source =
        \\module [makePoint]
        \\
        \\makePoint = |x, y| { x: x, y: y }
    ;

    // Main module that uses the record
    const main_source =
        \\module []
        \\
        \\import Factory exposing [makePoint]
        \\
        \\point = makePoint(10, 20)
        \\
        \\main = point.x
    ;

    // Set up Factory module
    var factory_env = try ModuleEnv.init(allocator, factory_source);
    defer factory_env.deinit();

    factory_env.module_name = "Factory";
    factory_env.common.source = factory_source;
    try factory_env.common.calcLineStarts(allocator);

    var factory_parse = try parse.parse(&factory_env.common, allocator);
    defer factory_parse.deinit(allocator);

    try factory_env.initCIRFields(allocator, "Factory");

    var factory_czer = try Can.init(&factory_env, &factory_parse, null);
    defer factory_czer.deinit();
    try factory_czer.canonicalizeFile();

    var factory_checker = try Check.init(allocator, &factory_env.types, &factory_env, &.{}, &factory_env.store.regions);
    defer factory_checker.deinit();
    try factory_checker.checkDefs();

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Factory", &factory_env);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    const other_modules = [_]*ModuleEnv{&factory_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    // Find main expression
    const defs = main_env.store.sliceDefs(main_env.all_defs);
    var main_expr_idx: ?CIR.Expr.Idx = null;
    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "main")) {
                main_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(main_expr_idx != null);

    // Set up interpreter
    var eval_stack = try stack.Stack.initCapacity(allocator, 4096);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(&main_env, &main_env.types);
    defer layout_cache.deinit();

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const other_envs = [_]*const ModuleEnv{&factory_env};
    var interpreter = try eval.Interpreter.initWithModules(
        allocator,
        &main_env,
        &other_envs,
        &eval_stack,
        &layout_cache,
        &main_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    // This should evaluate to 10, but currently panics because the record
    // returned from makePoint() has no fields in its layout
    const result = try interpreter.eval(main_expr_idx.?, test_env_instance.get_ops());
    try testing.expectEqual(@as(i128, 10), result.asI128());
}

test "record return type becomes Bool - bug reproduction" {
    // This test reproduces the bug where a lambda returning a record
    // gets incorrectly typed as returning Bool
    const allocator = testing.allocator;

    // Single module test - no cross-module complexity
    const source =
        \\module []
        \\
        \\makeRec = |n| { value: n }
        \\
        \\rec = makeRec(42)
        \\
        \\main = rec.value
    ;

    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();

    env.module_name = "Test";
    env.common.source = source;
    try env.common.calcLineStarts(allocator);

    var parsed = try parse.parse(&env.common, allocator);
    defer parsed.deinit(allocator);

    try env.initCIRFields(allocator, "Test");

    var czer = try Can.init(&env, &parsed, null);
    defer czer.deinit();
    try czer.canonicalizeFile();

    var checker = try Check.init(allocator, &env.types, &env, &.{}, &env.store.regions);
    defer checker.deinit();
    try checker.checkDefs();

    // Check the type of makeRec
    const defs = env.store.sliceDefs(env.all_defs);
    for (defs) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "makeRec")) {
                // Get the type of makeRec
                const makeRec_var = ModuleEnv.varFrom(def.expr);
                const makeRec_resolved = env.types.resolveVar(makeRec_var);

                std.debug.print("\n=== Type of makeRec ===\n", .{});
                std.debug.print("  Content: {s}\n", .{@tagName(makeRec_resolved.desc.content)});

                if (makeRec_resolved.desc.content == .structure) {
                    const structure = makeRec_resolved.desc.content.structure;
                    std.debug.print("  Structure type: {s}\n", .{@tagName(structure)});

                    // Check the return type of the function
                    switch (structure) {
                        .fn_pure, .fn_effectful, .fn_unbound => |func| {
                            const ret_var = func.ret;
                            const ret_resolved = env.types.resolveVar(ret_var);
                            std.debug.print("  Return type content: {s}\n", .{@tagName(ret_resolved.desc.content)});

                            if (ret_resolved.desc.content == .structure) {
                                const ret_structure = ret_resolved.desc.content.structure;
                                std.debug.print("  Return structure: {s}\n", .{@tagName(ret_structure)});

                                // This should be 'record' but we're getting 'record_unbound'
                                // Let's check what record_unbound means
                                if (ret_structure == .record_unbound) {
                                    std.debug.print("  Got record_unbound - this might be the issue!\n", .{});

                                    // Let's see what happens when we try to get the layout
                                    var stack_memory = try stack.Stack.initCapacity(allocator, 4096);
                                    defer stack_memory.deinit();

                                    var layout_cache = try LayoutStore.init(&env, &env.types);
                                    defer layout_cache.deinit();

                                    // Try to get the layout for this return type
                                    var type_scope = types.TypeScope.init(allocator);
                                    defer type_scope.deinit();

                                    const layout_idx = layout_cache.addTypeVar(ret_var, &type_scope) catch |err| {
                                        std.debug.print("  ERROR getting layout: {}\n", .{err});
                                        return err;
                                    };

                                    const ret_layout = layout_cache.getLayout(layout_idx);
                                    std.debug.print("  Layout for record_unbound: {s}\n", .{@tagName(ret_layout.tag)});

                                    // This is the bug - record_unbound should produce a record layout
                                    try testing.expect(ret_layout.tag == .record);
                                }
                            }
                        },
                        else => {},
                    }
                }
            }
        }
    }
}

test "cross-module record_unbound becomes wrong layout" {
    // This test reproduces the bug where record_unbound produces wrong layout
    // when crossing module boundaries
    const allocator = testing.allocator;

    // Module that returns a record through a function
    const lib_source =
        \\module [makeRec]
        \\
        \\makeRec = |n| { value: n }
    ;

    // Main module that uses the function
    const main_source =
        \\module []
        \\
        \\import Lib exposing [makeRec]
        \\
        \\rec = makeRec(42)
        \\
        \\main = rec.value
    ;

    // Set up Lib module
    var lib_env = try ModuleEnv.init(allocator, lib_source);
    defer lib_env.deinit();

    lib_env.module_name = "Lib";
    lib_env.common.source = lib_source;
    try lib_env.common.calcLineStarts(allocator);

    var lib_parse = try parse.parse(&lib_env.common, allocator);
    defer lib_parse.deinit(allocator);

    try lib_env.initCIRFields(allocator, "Lib");

    var lib_czer = try Can.init(&lib_env, &lib_parse, null);
    defer lib_czer.deinit();
    try lib_czer.canonicalizeFile();

    var lib_checker = try Check.init(allocator, &lib_env.types, &lib_env, &.{}, &lib_env.store.regions);
    defer lib_checker.deinit();
    try lib_checker.checkDefs();

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Lib", &lib_env);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    // Check the type of makeRec in Main module
    const defs = main_env.store.sliceDefs(main_env.all_defs);
    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "rec")) {
                // rec = makeRec(42), so get its type
                const rec_var = ModuleEnv.varFrom(def.expr);
                const rec_resolved = main_env.types.resolveVar(rec_var);

                std.debug.print("\n=== Type of rec (result of makeRec(42)) ===\n", .{});
                std.debug.print("  Content: {s}\n", .{@tagName(rec_resolved.desc.content)});

                // rec should be a record value, not a function
                if (rec_resolved.desc.content == .structure) {
                    const structure = rec_resolved.desc.content.structure;
                    std.debug.print("  Structure type: {s}\n", .{@tagName(structure)});

                    // Try to get the layout for rec
                    var layout_cache = try LayoutStore.init(&main_env, &main_env.types);
                    defer layout_cache.deinit();

                    var type_scope = types.TypeScope.init(allocator);
                    defer type_scope.deinit();

                    const rec_type_var = rec_var;
                    std.debug.print("  Calling addTypeVar on rec var {}\n", .{rec_type_var});
                    const layout_idx = layout_cache.addTypeVar(rec_type_var, &type_scope) catch |err| {
                        std.debug.print("  ERROR getting layout: {}\n", .{err});
                        return err;
                    };

                    const rec_layout = layout_cache.getLayout(layout_idx);
                    std.debug.print("  Layout for rec: {s}\n", .{@tagName(rec_layout.tag)});

                    // This should be 'record' but might be something else
                    try testing.expect(rec_layout.tag == .record);
                }
            }
        }
    }
}

test "minimal handleLambdaReturn type mismatch" {
    // TODO: Fix cross-module record returns - the record layout field data is not being preserved
    // correctly when a function from another module returns a record value
    // if (true) return error.SkipZigTest;

    // Minimal reproduction of TypeMismatch in handleLambdaReturn for cross-module returns
    const allocator = testing.allocator;

    // Module that returns a record through a function
    const lib_source =
        \\module [makeRec]
        \\
        \\makeRec = |n| { value: n }
    ;

    // Main module that uses the function to get a record
    const main_source =
        \\module []
        \\
        \\import Lib exposing [makeRec]
        \\
        \\rec = makeRec(42)
        \\
        \\main = rec.value
    ;

    // Set up Lib module
    var lib_env = try ModuleEnv.init(allocator, lib_source);
    defer lib_env.deinit();

    lib_env.module_name = "Lib";
    lib_env.common.source = lib_source;
    try lib_env.common.calcLineStarts(allocator);

    var lib_parse = try parse.parse(&lib_env.common, allocator);
    defer lib_parse.deinit(allocator);

    try lib_env.initCIRFields(allocator, "Lib");

    var lib_czer = try Can.init(&lib_env, &lib_parse, null);
    defer lib_czer.deinit();
    try lib_czer.canonicalizeFile();

    var lib_checker = try Check.init(allocator, &lib_env.types, &lib_env, &.{}, &lib_env.store.regions);
    defer lib_checker.deinit();
    try lib_checker.checkDefs();

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Lib", &lib_env);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    // Find main expression
    const defs = main_env.store.sliceDefs(main_env.all_defs);
    var main_expr_idx: ?CIR.Expr.Idx = null;
    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "main")) {
                main_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(main_expr_idx != null);

    // Set up interpreter
    var eval_stack = try stack.Stack.initCapacity(allocator, 4096);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(&main_env, &main_env.types);
    defer layout_cache.deinit();

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const other_envs = [_]*const ModuleEnv{&lib_env};
    var interpreter = try eval.Interpreter.initWithModules(
        allocator,
        &main_env,
        &other_envs,
        &eval_stack,
        &layout_cache,
        &main_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    // This should evaluate to 42 but may hit TypeMismatch in handleLambdaReturn
    const result = try interpreter.eval(main_expr_idx.?, test_env_instance.get_ops());
    try testing.expectEqual(@as(i128, 42), result.asI128());
}

test "minimal cross-module typecheck bug" {
    const allocator = testing.allocator;

    // Math module with a simple double method
    const math_source =
        \\module [double]
        \\
        \\double = |obj| obj.value * 2
    ;

    // Main module using static dispatch
    const main_source =
        \\module []
        \\
        \\import Math exposing [double]
        \\
        \\obj = { value: 7 }
        \\
        \\main = obj.double()
    ;

    // Set up Math module
    var math_env = try ModuleEnv.init(allocator, math_source);
    defer math_env.deinit();

    math_env.module_name = "Math";
    math_env.common.source = math_source;
    try math_env.common.calcLineStarts(allocator);

    var math_parse = try parse.parse(&math_env.common, allocator);
    defer math_parse.deinit(allocator);

    try math_env.initCIRFields(allocator, "Math");

    var math_czer = try Can.init(&math_env, &math_parse, null);
    defer math_czer.deinit();
    try math_czer.canonicalizeFile();

    var math_checker = try Check.init(allocator, &math_env.types, &math_env, &.{}, &math_env.store.regions);
    defer math_checker.deinit();
    try math_checker.checkDefs();

    std.debug.print("\nMath module type-checked OK\n", .{});

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    // Set up imports through deps
    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Math", &math_env);

    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    std.debug.print("Main module canonicalized OK\n", .{});

    // Debug: Print what was canonicalized
    const defs = main_env.store.sliceDefs(main_env.all_defs);
    std.debug.print("Main module has {} defs\n", .{defs.len});
    for (defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident = main_env.getIdent(pattern.assign.ident);
            const expr = main_env.store.getExpr(def.expr);
            std.debug.print("  {s}: {s}\n", .{ ident, @tagName(expr) });

            if (expr == .e_dot_access) {
                const field = main_env.getIdent(expr.e_dot_access.field_name);
                std.debug.print("    -> dot access field: {s}\n", .{field});
                if (expr.e_dot_access.args) |args| {
                    const arg_exprs = main_env.store.sliceExpr(args);
                    std.debug.print("    -> args count: {}\n", .{arg_exprs.len});
                } else {
                    std.debug.print("    -> no args (field access, not method call)\n", .{});
                }
            }
        }
    }

    // Type check Main module - THIS SHOULD FAIL
    const other_modules = [_]*ModuleEnv{&math_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();

    std.debug.print("About to type-check Main module...\n", .{});
    try main_checker.checkDefs();
    std.debug.print("Main module type-checked OK!\n", .{});
}

test "minimal import lookup" {
    const allocator = testing.allocator;

    // Simple module with one function - using CORRECT Roc lambda syntax
    const lib_source =
        \\module [add]
        \\
        \\add = |x, y| x + y
    ;

    // Main module that imports and calls the function
    const main_source =
        \\module []
        \\
        \\import Lib exposing [add]
        \\
        \\main = add(3, 4)
    ;

    // Set up Lib module
    var lib_env = try ModuleEnv.init(allocator, lib_source);
    defer lib_env.deinit();

    lib_env.module_name = "Lib";
    lib_env.common.source = lib_source;
    try lib_env.common.calcLineStarts(allocator);

    var lib_parse = try parse.parse(&lib_env.common, allocator);
    defer lib_parse.deinit(allocator);

    try lib_env.initCIRFields(allocator, "Lib");

    var lib_czer = try Can.init(&lib_env, &lib_parse, null);
    defer lib_czer.deinit();
    try lib_czer.canonicalizeFile();

    // Type check Lib module
    var lib_checker = try Check.init(allocator, &lib_env.types, &lib_env, &.{}, &lib_env.store.regions);
    defer lib_checker.deinit();
    try lib_checker.checkDefs();

    // Debug: Print Lib module definitions
    std.debug.print("\n=== MINIMAL IMPORT TEST - Lib module ===\n", .{});
    const lib_defs = lib_env.store.sliceDefs(lib_env.all_defs);
    for (lib_defs) |def_idx| {
        const def = lib_env.store.getDef(def_idx);
        const pattern = lib_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = lib_env.getIdent(pattern.assign.ident);
            const expr = lib_env.store.getExpr(def.expr);
            std.debug.print("  {s}: pattern_idx={}, expr_idx={}, type={s}\n", .{
                name,
                @intFromEnum(def.pattern),
                @intFromEnum(def.expr),
                @tagName(expr)
            });
        }
    }

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Lib", &lib_env);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    // Debug: Print Main module definitions
    std.debug.print("\n=== MINIMAL IMPORT TEST - Main module ===\n", .{});
    const main_defs = main_env.store.sliceDefs(main_env.all_defs);
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            const expr = main_env.store.getExpr(def.expr);
            std.debug.print("  {s}: expr_idx={}, type={s}\n", .{
                name,
                @intFromEnum(def.expr),
                @tagName(expr)
            });

            // If it's a call, inspect it
            if (expr == .e_call) {
                const args = main_env.store.sliceExpr(expr.e_call.args);
                std.debug.print("    call has {} args\n", .{args.len});
                if (args.len > 0) {
                    const func_expr = main_env.store.getExpr(args[0]);
                    std.debug.print("    function: {s}\n", .{@tagName(func_expr)});
                    if (func_expr == .e_lookup_external) {
                        std.debug.print("      lookup_external: module_idx={}, target_node_idx={}\n", .{
                            @intFromEnum(func_expr.e_lookup_external.module_idx),
                            func_expr.e_lookup_external.target_node_idx
                        });
                    }
                }
            }
        }
    }
    std.debug.print("=========================================\n", .{});

    // Type check Main module
    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    std.debug.print("Main module type-checked successfully\n", .{});

    // Find main expression
    var main_expr_idx: ?CIR.Expr.Idx = null;
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "main")) {
                main_expr_idx = def.expr;
                break;
            }
        }
    }

    try testing.expect(main_expr_idx != null);

    // Set up interpreter
    var eval_stack = try stack.Stack.initCapacity(allocator, 4096);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(&main_env, &main_env.types);
    defer layout_cache.deinit();

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const other_envs = [_]*const ModuleEnv{&lib_env};
    var interpreter = try eval.Interpreter.initWithModules(
        allocator,
        &main_env,
        &other_envs,
        &eval_stack,
        &layout_cache,
        &main_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    // Evaluate main expression
    const result = try interpreter.eval(main_expr_idx.?, test_env_instance.get_ops());

    // Verify: 3 + 4 = 7
    try testing.expectEqual(@as(i128, 7), result.asI128());
}

test "minimal closure import - return closure from module" {
    const allocator = testing.allocator;

    // Module that returns a closure - using simplest possible closure
    const lib_source =
        \\module [getFunc]
        \\
        \\getFunc = |x| |y| y
    ;

    // Main module that imports and calls the function
    const main_source =
        \\module []
        \\
        \\import Lib exposing [getFunc]
        \\
        \\main = getFunc(5)
    ;

    // Set up Lib module
    var lib_env = try ModuleEnv.init(allocator, lib_source);
    defer lib_env.deinit();

    lib_env.module_name = "Lib";
    lib_env.common.source = lib_source;
    try lib_env.common.calcLineStarts(allocator);

    var lib_parse = try parse.parse(&lib_env.common, allocator);
    defer lib_parse.deinit(allocator);

    try lib_env.initCIRFields(allocator, "Lib");

    var lib_czer = try Can.init(&lib_env, &lib_parse, null);
    defer lib_czer.deinit();
    try lib_czer.canonicalizeFile();

    // Type check Lib module
    var lib_checker = try Check.init(allocator, &lib_env.types, &lib_env, &.{}, &lib_env.store.regions);
    defer lib_checker.deinit();
    try lib_checker.checkDefs();

    // Set up Main module
    var main_env = try ModuleEnv.init(allocator, main_source);
    defer main_env.deinit();

    main_env.module_name = "Main";
    main_env.common.source = main_source;
    try main_env.common.calcLineStarts(allocator);

    var deps = std.StringHashMap(*ModuleEnv).init(allocator);
    defer deps.deinit();
    try deps.put("Lib", &lib_env);

    var main_parse = try parse.parse(&main_env.common, allocator);
    defer main_parse.deinit(allocator);

    try main_env.initCIRFields(allocator, "Main");

    var main_czer = try Can.init(&main_env, &main_parse, &deps);
    defer main_czer.deinit();
    try main_czer.canonicalizeFile();

    // Type check Main module
    const other_modules = [_]*ModuleEnv{&lib_env};
    var main_checker = try Check.init(allocator, &main_env.types, &main_env, &other_modules, &main_env.store.regions);
    defer main_checker.deinit();
    try main_checker.checkDefs();

    // Find main expression
    var main_expr_idx: ?CIR.Expr.Idx = null;
    const main_defs = main_env.store.sliceDefs(main_env.all_defs);
    for (main_defs) |def_idx| {
        const def = main_env.store.getDef(def_idx);
        const pattern = main_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const name = main_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, name, "main")) {
                main_expr_idx = def.expr;
                break;
            }
        }
    }

    try testing.expect(main_expr_idx != null);

    // Set up interpreter
    var eval_stack = try stack.Stack.initCapacity(allocator, 4096);
    defer eval_stack.deinit();

    var layout_cache = try LayoutStore.init(&main_env, &main_env.types);
    defer layout_cache.deinit();

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const other_envs = [_]*const ModuleEnv{&lib_env};
    var interpreter = try eval.Interpreter.initWithModules(
        allocator,
        &main_env,
        &other_envs,
        &eval_stack,
        &layout_cache,
        &main_env.types,
    );
    defer interpreter.deinit(test_env_instance.get_ops());

    // Debug: Print what we're evaluating
    const main_expr = main_env.store.getExpr(main_expr_idx.?);
    std.debug.print("\n=== Closure import test ===\n", .{});
    std.debug.print("Main expr type: {s}\n", .{@tagName(main_expr)});

    // Evaluate main expression
    const result = try interpreter.eval(main_expr_idx.?, test_env_instance.get_ops());

    // The result should be a closure (a partially applied function)
    std.debug.print("Result layout tag: {s}\n", .{@tagName(result.layout.tag)});
    if (result.layout.tag == .closure) {
        const closure_size = layout_cache.layoutSize(result.layout);
        std.debug.print("Closure size: {} bytes\n", .{closure_size});
    }
    std.debug.print("===========================\n\n", .{});

    try testing.expect(result.layout.tag == .closure);
}

test "SKIP: original cross-module test" {
    return error.SkipZigTest;
}

// Import comprehensive test suite
test {
    _ = @import("static_dispatch_comprehensive_test.zig");
}
