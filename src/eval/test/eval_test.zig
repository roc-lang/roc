//! Tests for the expression evaluator
const std = @import("std");
const helpers = @import("helpers.zig");
const eval = @import("../interpreter.zig");
const test_env = @import("../test_env.zig");
const compile = @import("compile");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const Can = @import("can");
const Check = @import("check");
const stack = @import("../stack.zig");
const layout_store = @import("../../layout/store.zig");
const collections = @import("collections");
const serialization = @import("serialization");
const builtins = @import("builtins");

const ModuleEnv = compile.ModuleEnv;
const CompactWriter = serialization.CompactWriter;
const testing = std.testing;
const test_allocator = testing.allocator;

const EvalError = eval.EvalError;
const runExpectInt = helpers.runExpectInt;
const runExpectError = helpers.runExpectError;
const runExpectStr = helpers.runExpectStr;

test "eval simple number" {
    try runExpectInt("1", 1, .no_trace);
    try runExpectInt("42", 42, .no_trace);
    try runExpectInt("-1234", -1234, .no_trace);
}

test "eval boolean literals" {
    try runExpectInt("True", 1, .no_trace);
    try runExpectInt("False", 0, .no_trace);
    try runExpectInt("Bool.True", 1, .no_trace);
    try runExpectInt("Bool.False", 0, .no_trace);
}

test "eval unary not operator" {
    try runExpectInt("!True", 0, .no_trace);
    try runExpectInt("!False", 1, .no_trace);
    try runExpectInt("!Bool.True", 0, .no_trace);
    try runExpectInt("!Bool.False", 1, .no_trace);
}

test "eval double negation" {
    try runExpectInt("!!True", 1, .no_trace);
    try runExpectInt("!!False", 0, .no_trace);
    try runExpectInt("!!!True", 0, .no_trace);
    try runExpectInt("!!!False", 1, .no_trace);
}

test "eval boolean in lambda expressions" {
    try runExpectInt("(|x| !x)(True)", 0, .no_trace);
    try runExpectInt("(|x| !x)(False)", 1, .no_trace);
    try runExpectInt("(|x, y| x and y)(True, False)", 0, .no_trace);
    try runExpectInt("(|x, y| x or y)(False, True)", 1, .no_trace);
    try runExpectInt("(|x| x and !x)(True)", 0, .no_trace);
    try runExpectInt("(|x| x or !x)(False)", 1, .no_trace);
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
    try runExpectInt("(5 > 3) == 1", 1, .no_trace); // true == true
    try runExpectInt("(10 < 20) == 1", 1, .no_trace); // true == true
    try runExpectInt("(5 >= 5) == 1", 1, .no_trace); // true == true
    try runExpectInt("(10 <= 9) == 0", 1, .no_trace); // false == false

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
    try runExpectInt("(5 > 3) && (3 > 1)", 1, .no_trace); // Must use parentheses

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
    // Test simple crash statement
    try runExpectError("crash \"test\"", EvalError.Crash, .no_trace);

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

    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var test_env_instance = test_env.TestEnv.init(testing.allocator);
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
    try runExpectInt("(|a| |b| a * b)(5)(10)", 50, .no_trace);
    try runExpectInt("(((|a| |b| |c| a + b + c)(100))(20))(3)", 123, .no_trace);
    try runExpectInt("(|a, b, c| |d| a + b + c + d)(10, 20, 5)(7)", 42, .no_trace);
    try runExpectInt("(|y| (|x| (|z| x + y + z)(3))(2))(1)", 6, .no_trace);
}

test "lambdas with capture" {
    try runExpectInt(
        \\{
        \\    x = 10;
        \\    f = |y| x + y;
        \\    f(5)
        \\}
    , 15, .no_trace);

    try runExpectInt(
        \\{
        \\    x = 20;
        \\    y = 30;
        \\    f = |z| x + y + z;
        \\    f(10)
        \\}
    , 60, .no_trace);
}

test "lambdas nested closures" {
    try runExpectInt(
        \\(((|a| {
        \\    a_loc = a * 2;
        \\    |b| {
        \\        b_loc = a_loc + b;
        \\        |c| b_loc + c
        \\    }
        \\})(100))(20))(3)
    , 223, .no_trace);
}

// Helper function to test that evaluation succeeds without checking specific values
fn runExpectSuccess(src: []const u8, should_trace: enum { trace, no_trace }) !void {
    var test_env_instance = test_env.TestEnv.init(testing.allocator);
    defer test_env_instance.deinit();

    const resources = try helpers.parseAndCanonicalizeExpr(std.testing.allocator, src);
    defer helpers.cleanupParseAndCanonical(std.testing.allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(std.testing.allocator, 1024);
    defer eval_stack.deinit();

    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
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
        \\    hello = "Hello";
        \\    world = "World";
        \\    "${hello} ${world}"
        \\}
    , .no_trace);
}

test "string refcount - basic literal" {
    // Test basic string literal creation and cleanup
    try runExpectStr("\"Hello, World!\"", "Hello, World!", .no_trace);
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
    try runExpectStr("(|s| s)(\"Test\")", "Test", .trace);
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
    var test_env_instance = test_env.TestEnv.init(gpa);
    defer test_env_instance.deinit();

    // Create original ModuleEnv
    var original_env = try ModuleEnv.init(gpa, source);
    defer original_env.deinit();

    original_env.source = source;
    original_env.module_name = "TestModule";
    try original_env.calcLineStarts();

    // Parse the source code
    var parse_ast = try parse.parseExpr(&original_env);
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try original_env.initCIRFields(gpa, "test");

    // Create canonicalizer
    var can = try Can.init(&original_env, &parse_ast, null);
    defer can.deinit();

    // Canonicalize the expression
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonicalized_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
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

        var layout_cache = try layout_store.Store.init(&original_env, &original_env.types);
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

        // Verify basic deserialization worked
        try testing.expectEqualStrings("TestModule", deserialized_env.module_name);
        try testing.expectEqualStrings(source, deserialized_env.source);

        // Test 3: Verify the deserialized ModuleEnv has the correct structure
        try testing.expect(deserialized_env.types.len() > 0);
        try testing.expect(deserialized_env.store.nodes.items.len > 0);

        // Verify that the deserialized data matches the original data
        try testing.expectEqual(original_env.types.len(), deserialized_env.types.len());
        try testing.expectEqual(original_env.store.nodes.items.len, deserialized_env.store.nodes.items.len);
        try testing.expectEqual(original_env.idents.interner.bytes.len(), deserialized_env.idents.interner.bytes.len());

        // Test 4: Evaluate the same expression using the deserialized ModuleEnv
        // The original expression index should still be valid since the NodeStore structure is preserved
        {
            var eval_stack = try stack.Stack.initCapacity(gpa, 1024);
            defer eval_stack.deinit();

            var layout_cache = try layout_store.Store.init(deserialized_env, &deserialized_env.types);
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
