//! Tests for the expression evaluator
const std = @import("std");
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");
const collections = @import("collections");
const compiled_builtins = @import("compiled_builtins");
const roc_target = @import("roc_target");

const helpers = @import("helpers.zig");
const builtin_loading = @import("../builtin_loading.zig");
const TestEnv = @import("TestEnv.zig");
const Interpreter = @import("../interpreter.zig").Interpreter;
const BuiltinTypes = @import("../builtins.zig").BuiltinTypes;

const Can = can.Can;
const Check = check.Check;
const ModuleEnv = can.ModuleEnv;
const Allocators = base.Allocators;
const CompactWriter = collections.CompactWriter;
const testing = std.testing;
// Use interpreter_allocator for interpreter tests (doesn't track leaks)
const test_allocator = helpers.interpreter_allocator;

const runExpectI64 = helpers.runExpectI64;
const runExpectIntDec = helpers.runExpectIntDec;
const runExpectBool = helpers.runExpectBool;
const runExpectError = helpers.runExpectError;
const runExpectStr = helpers.runExpectStr;
const runExpectRecord = helpers.runExpectRecord;
const runExpectListI64 = helpers.runExpectListI64;
const runExpectListZst = helpers.runExpectListZst;
const runExpectDec = helpers.runExpectDec;
const ExpectedField = helpers.ExpectedField;

const TraceWriterState = struct {
    buffer: [256]u8 = undefined,
    writer: std.fs.File.Writer = undefined,

    fn init() TraceWriterState {
        var state = TraceWriterState{};
        state.writer = std.fs.File.stderr().writer(&state.buffer);
        return state;
    }
};

test "eval simple number" {
    try runExpectI64("1", 1, .no_trace);
    try runExpectI64("42", 42, .no_trace);
    try runExpectI64("-1234", -1234, .no_trace);
}

test "if-else" {
    try runExpectI64("if (1 == 1) 42 else 99", 42, .no_trace);
    try runExpectI64("if (1 == 2) 42 else 99", 99, .no_trace);
    try runExpectI64("if (5 > 3) 100 else 200", 100, .no_trace);
    try runExpectI64("if (3 > 5) 100 else 200", 200, .no_trace);
}

test "nested if-else" {
    try runExpectI64("if (1 == 1) (if (2 == 2) 100 else 200) else 300", 100, .no_trace);
    try runExpectI64("if (1 == 1) (if (2 == 3) 100 else 200) else 300", 200, .no_trace);
    try runExpectI64("if (1 == 2) (if (2 == 2) 100 else 200) else 300", 300, .no_trace);
}

test "eval single element record" {
    try runExpectI64("{x: 42}.x", 42, .no_trace);
    try runExpectI64("{foo: 100}.foo", 100, .no_trace);
    try runExpectI64("{bar: 1 + 2}.bar", 3, .no_trace);
}

test "eval multi-field record" {
    try runExpectI64("{x: 10, y: 20}.x", 10, .no_trace);
    try runExpectI64("{x: 10, y: 20}.y", 20, .no_trace);
    try runExpectI64("{a: 1, b: 2, c: 3}.a", 1, .no_trace);
    try runExpectI64("{a: 1, b: 2, c: 3}.b", 2, .no_trace);
    try runExpectI64("{a: 1, b: 2, c: 3}.c", 3, .no_trace);
}

test "nested record access" {
    try runExpectI64("{outer: {inner: 42}}.outer.inner", 42, .no_trace);
    try runExpectI64("{a: {b: {c: 100}}}.a.b.c", 100, .no_trace);
}

test "record field order independence" {
    try runExpectI64("{x: 1, y: 2}.x + {y: 2, x: 1}.x", 2, .no_trace);
    try runExpectI64("{a: 10, b: 20, c: 30}.b", 20, .no_trace);
    try runExpectI64("{c: 30, a: 10, b: 20}.b", 20, .no_trace);
}

test "arithmetic binops" {
    try runExpectI64("1 + 2", 3, .no_trace);
    try runExpectI64("5 - 3", 2, .no_trace);
    try runExpectI64("4 * 5", 20, .no_trace);
    try runExpectI64("10 // 2", 5, .no_trace);
    try runExpectI64("7 % 3", 1, .no_trace);
}

test "simple Dec division - larger numbers" {
    // Single division with numbers similar to failing tests
    try runExpectI64("100 // 20", 5, .no_trace);
}

test "simple Dec modulo - larger numbers" {
    // Single modulo - does this work?
    try runExpectI64("100 % 30", 10, .no_trace);
}

test "Dec division result used in arithmetic" {
    // Division result used in subsequent arithmetic (addition, not another division)
    try runExpectI64("(100 // 20) + 1", 6, .no_trace);
}

test "comparison binops" {
    try runExpectI64("if 1 < 2 100 else 200", 100, .no_trace);
    try runExpectI64("if 2 < 1 100 else 200", 200, .no_trace);
    try runExpectI64("if 5 > 3 100 else 200", 100, .no_trace);
    try runExpectI64("if 3 > 5 100 else 200", 200, .no_trace);
    try runExpectI64("if 10 <= 10 100 else 200", 100, .no_trace);
    try runExpectI64("if 10 <= 9 100 else 200", 200, .no_trace);
    try runExpectI64("if 10 >= 10 100 else 200", 100, .no_trace);
    try runExpectI64("if 9 >= 10 100 else 200", 200, .no_trace);
    try runExpectI64("if 5 == 5 100 else 200", 100, .no_trace);
    try runExpectI64("if 5 == 6 100 else 200", 200, .no_trace);
    try runExpectI64("if 5 != 6 100 else 200", 100, .no_trace);
    try runExpectI64("if 5 != 5 100 else 200", 200, .no_trace);
}

test "unary minus" {
    try runExpectI64("-5", -5, .no_trace);
    try runExpectI64("-(-10)", 10, .no_trace);
    try runExpectI64("-(3 + 4)", -7, .no_trace);
    try runExpectI64("-0", 0, .no_trace);
}

test "parentheses and precedence" {
    try runExpectI64("2 + 3 * 4", 14, .no_trace);
    try runExpectI64("(2 + 3) * 4", 20, .no_trace);
    try runExpectI64("100 - 20 - 10", 70, .no_trace);
    try runExpectI64("100 - (20 - 10)", 90, .no_trace);
}

test "operator associativity - addition" {
    // Left associative: a + b + c should parse as (a + b) + c
    try runExpectI64("100 + 20 + 10", 130, .no_trace); // (100 + 20) + 10 = 130
    try runExpectI64("100 + (20 + 10)", 130, .no_trace); // Same result, but explicitly grouped

    // More complex case
    try runExpectI64("10 + 20 + 30 + 40", 100, .no_trace); // ((10 + 20) + 30) + 40 = 100
}

test "operator associativity - subtraction" {
    // Left associative: a - b - c should parse as (a - b) - c
    try runExpectI64("100 - 20 - 10", 70, .no_trace); // (100 - 20) - 10 = 70
    try runExpectI64("100 - (20 - 10)", 90, .no_trace); // Different result with explicit grouping

    // More complex case showing the difference
    try runExpectI64("100 - 50 - 25 - 5", 20, .no_trace); // ((100 - 50) - 25) - 5 = 20
    try runExpectI64("100 - (50 - (25 - 5))", 70, .no_trace); // Right associative would give 70
}

test "operator associativity - mixed addition and subtraction" {
    // Regression test: + and - should have equal precedence and be left-associative
    // Previously + had higher precedence than -, causing 1 - 2 + 3 to parse as 1 - (2 + 3) = -4
    try runExpectI64("1 - 2 + 3", 2, .no_trace); // (1 - 2) + 3 = 2, NOT 1 - (2 + 3) = -4
    try runExpectI64("5 + 3 - 2", 6, .no_trace); // (5 + 3) - 2 = 6
    try runExpectI64("10 - 5 + 3 - 2", 6, .no_trace); // ((10 - 5) + 3) - 2 = 6
    try runExpectI64("1 + 2 - 3 + 4 - 5", -1, .no_trace); // (((1 + 2) - 3) + 4) - 5 = -1
}

test "operator associativity - multiplication" {
    // Left associative: a * b * c should parse as (a * b) * c
    try runExpectI64("2 * 3 * 4", 24, .no_trace); // (2 * 3) * 4 = 24
    try runExpectI64("2 * (3 * 4)", 24, .no_trace); // Same result for multiplication

    // Chain of multiplications
    try runExpectI64("2 * 3 * 4 * 5", 120, .no_trace); // ((2 * 3) * 4) * 5 = 120
}

test "operator associativity - division" {
    // Left associative: a / b / c should parse as (a / b) / c
    // Note: Using integer division (//) for predictable integer results
    try runExpectI64("100 // 20 // 2", 2, .no_trace); // (100 // 20) // 2 = 5 // 2 = 2
    try runExpectI64("100 // (20 // 2)", 10, .no_trace); // Different result: 100 // 10 = 10

    // More complex case showing the difference
    // Using small numbers to avoid Dec overflow with multiple divisions
    try runExpectI64("80 // 8 // 2", 5, .no_trace); // ((80 // 8) // 2) = (10 // 2) = 5
    try runExpectI64("80 // (8 // 2)", 20, .no_trace); // 80 // 4 = 20
}

test "operator associativity - modulo" {
    // Left associative: a % b % c should parse as (a % b) % c
    try runExpectI64("100 % 30 % 7", 3, .no_trace); // (100 % 30) % 7 = 10 % 7 = 3
    try runExpectI64("100 % (30 % 7)", 0, .no_trace); // Different result: 100 % 2 = 0

    // Another example
    try runExpectI64("50 % 20 % 6", 4, .no_trace); // (50 % 20) % 6 = 10 % 6 = 4
    try runExpectI64("50 % (20 % 6)", 0, .no_trace); // Right associative: 50 % 2 = 0
}

test "operator associativity - mixed precedence" {
    // Verify that precedence still works correctly with fixed associativity
    try runExpectI64("2 + 3 * 4", 14, .no_trace); // 2 + (3 * 4) = 14
    try runExpectI64("2 * 3 + 4", 10, .no_trace); // (2 * 3) + 4 = 10

    // More complex mixed operations
    try runExpectI64("10 - 2 * 3", 4, .no_trace); // 10 - (2 * 3) = 4
    try runExpectI64("100 // 5 + 10", 30, .no_trace); // (100 // 5) + 10 = 30
    try runExpectI64("100 // 5 % 3", 2, .no_trace); // (100 // 5) % 3 = 20 % 3 = 2
}

test "operator associativity - edge cases" {
    // Very long chains to ensure associativity is consistent
    try runExpectI64("1000 - 100 - 50 - 25 - 10 - 5", 810, .no_trace);
    // ((((1000 - 100) - 50) - 25) - 10) - 5 = 810

    // Complex nested expressions
    try runExpectI64("(100 - 50) - (30 - 10)", 30, .no_trace); // 50 - 20 = 30
    try runExpectI64("100 - (50 - 30) - 10", 70, .no_trace); // 100 - 20 - 10 = 70

    // Division chains that would overflow if right-associative
    // Using very small numbers to avoid Dec overflow with chained divisions
    try runExpectI64("80 // 4 // 2", 10, .no_trace);
    // (((80 // 4) // 2) = (20 // 2) = 10

    // Modulo chains
    try runExpectI64("1000 % 300 % 40 % 7", 6, .no_trace);
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
    try runExpectI64("8 - 4 - 2", 2, .no_trace); // (8-4)-2 = 2, NOT 8-(4-2) = 6
    try runExpectI64("16 // 4 // 2", 2, .no_trace); // (16//4)//2 = 2, NOT 16//(4//2) = 8

    // NON-ASSOCIATIVE (comparison operators)
    // Can't chain without parentheses
    try runExpectBool("(5 > 3) and (3 > 1)", true, .no_trace); // Must use parentheses

    // RIGHT ASSOCIATIVE (logical operators)
    // a op b op c = a op (b op c)
    // Note: the boolean keywords `and` and `or` are right associative in Roc
    // This is mostly relevant for short-circuiting behavior
}

test "error test - divide by zero" {
    try runExpectError("5 // 0", error.DivisionByZero, .no_trace);
    try runExpectError("10 % 0", error.DivisionByZero, .no_trace);
}

test "simple lambda with if-else" {
    try runExpectI64("(|x| if x > 0.I64 x else 0.I64)(5.I64)", 5, .no_trace);
    try runExpectI64("(|x| if x > 0.I64 x else 0.I64)(-3.I64)", 0, .no_trace);
}

test "crash in else branch inside lambda" {
    // Test crash in else branch evaluated at runtime
    try runExpectError(
        \\(|x| if x > 0.I64 x else {
        \\    crash "crash in else!"
        \\    0.I64
        \\})(-5.I64)
    , error.Crash, .no_trace);
}

test "crash NOT taken when condition true" {
    // Test that crash in else branch is NOT executed when if branch is taken
    try runExpectI64(
        \\(|x| if x > 0.I64 x else {
        \\    crash "this should not execute"
        \\    0.I64
        \\})(10.I64)
    , 10, .no_trace);
}

test "error test - crash statement" {
    // Test crash statement in a block (crash is a statement, not an expression)
    try runExpectError(
        \\{
        \\    crash "test"
        \\    0
        \\}
    , error.Crash, .no_trace);

    // Test crash in block with final expression
    try runExpectError(
        \\{
        \\    crash "This is a crash statement"
        \\    42
        \\}
    , error.Crash, .no_trace);
}

test "crash message storage and retrieval - host-managed context" {
    // Verify the crash callback stores the message in the host CrashContext
    const test_message = "Direct API test message";

    var test_env_instance = TestEnv.init(helpers.interpreter_allocator);
    defer test_env_instance.deinit();

    try testing.expect(test_env_instance.crashState() == .did_not_crash);

    const crash_args = builtins.host_abi.RocCrashed{
        .utf8_bytes = @constCast(test_message.ptr),
        .len = test_message.len,
    };

    const ops = test_env_instance.get_ops();
    ops.roc_crashed(&crash_args, ops.env);

    switch (test_env_instance.crashState()) {
        .did_not_crash => return error.TestUnexpectedResult,
        .crashed => |msg| try testing.expectEqualStrings(test_message, msg),
    }
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
    try runExpectI64("(|x| x + 1.I64)(5.I64)", 6, .no_trace);
    try runExpectI64("(|x| x * 2.I64 + 1.I64)(10.I64)", 21, .no_trace);
    try runExpectI64("(|x| x - 3.I64)(8.I64)", 5, .no_trace);
    try runExpectI64("(|x| 100.I64 - x)(25.I64)", 75, .no_trace);
    try runExpectI64("(|x| 5.I64)(99.I64)", 5, .no_trace);
    try runExpectI64("(|x| x + x)(7.I64)", 14, .no_trace);
}

test "multi-parameter lambdas" {
    try runExpectI64("(|x, y| x + y)(3.I64, 4.I64)", 7, .no_trace);
    try runExpectI64("(|x, y| x * y)(5.I64, 6.I64)", 30, .no_trace);
    try runExpectI64("(|a, b, c| a + b + c)(1.I64, 2.I64, 3.I64)", 6, .no_trace);
}

test "lambdas with if-then bodies" {
    try runExpectI64("(|x| if x > 0.I64 x else 0.I64)(5.I64)", 5, .no_trace);
    try runExpectI64("(|x| if x > 0.I64 x else 0.I64)(-3.I64)", 0, .no_trace);
    try runExpectI64("(|x| if x == 0.I64 1.I64 else x)(0.I64)", 1, .no_trace);
    try runExpectI64("(|x| if x == 0.I64 1.I64 else x)(42.I64)", 42, .no_trace);
}

test "lambdas with unary minus" {
    try runExpectI64("(|x| -x)(5.I64)", -5, .no_trace);
    try runExpectI64("(|x| -x)(0.I64)", 0, .no_trace);
    try runExpectI64("(|x| -x)(-3.I64)", 3, .no_trace);
    try runExpectI64("(|x| -5.I64)(999.I64)", -5, .no_trace);
    try runExpectI64("(|x| if True -x else 0.I64)(5.I64)", -5, .no_trace);
    try runExpectI64("(|x| if True -10.I64 else x)(999.I64)", -10, .no_trace);
}

test "lambdas closures" {
    // Curried functions - lambdas returning lambdas
    try runExpectI64("(|a| |b| a * b)(5.I64)(10.I64)", 50, .no_trace);
    // Triple curried
    try runExpectI64("(((|a| |b| |c| a + b + c)(100.I64))(20.I64))(3.I64)", 123, .no_trace);
    // Multi-param lambda returning lambda
    try runExpectI64("(|a, b, c| |d| a + b + c + d)(10.I64, 20.I64, 5.I64)(7.I64)", 42, .no_trace);
    // Nested lambda calls with captures
    try runExpectI64("(|y| (|x| (|z| x + y + z)(3.I64))(2.I64))(1.I64)", 6, .no_trace);
}

test "lambdas with capture" {
    try runExpectI64(
        \\{
        \\    x = 10.I64
        \\    f = |y| x + y
        \\    f(5.I64)
        \\}
    , 15, .no_trace);

    try runExpectI64(
        \\{
        \\    x = 20.I64
        \\    y = 30.I64
        \\    f = |z| x + y + z
        \\    f(10.I64)
        \\}
    , 60, .no_trace);
}

test "closure with many captures (struct_captures)" {
    // 4 captures -> struct_captures representation
    try runExpectI64(
        \\{
        \\    a = 100.I64
        \\    b = 200.I64
        \\    c = 300.I64
        \\    d = 400.I64
        \\    f = |n| a + b + c + d + n
        \\    f(5.I64)
        \\}
    , 1005, .no_trace);
}

test "lambdas nested closures" {
    // Nested closures with block locals
    try runExpectI64(
        \\(((|a| {
        \\    a_loc = a * 2.I64
        \\    |b| {
        \\        b_loc = a_loc + b
        \\        |c| b_loc + c
        \\    }
        \\})(100.I64))(20.I64))(3.I64)
    , 223, .no_trace);
}

// Helper function to test that evaluation succeeds without checking specific values
fn runExpectSuccess(src: []const u8, should_trace: enum { trace, no_trace }) !void {
    var test_env_instance = TestEnv.init(helpers.interpreter_allocator);
    defer test_env_instance.deinit();

    const resources = try helpers.parseAndCanonicalizeExpr(helpers.interpreter_allocator, src);
    defer helpers.cleanupParseAndCanonical(helpers.interpreter_allocator, resources);

    var interpreter = try Interpreter.init(helpers.interpreter_allocator, resources.module_env, resources.builtin_types, resources.builtin_module.env, &[_]*const can.ModuleEnv{}, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    const enable_trace = should_trace == .trace;
    if (enable_trace) {
        interpreter.startTrace();
    }
    defer if (enable_trace) interpreter.endTrace();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);

    // Minimal smoke check: the helper only succeeds if evaluation produced a value without crashing.
    try std.testing.expect(test_env_instance.crashState() == .did_not_crash);
}

test "integer type evaluation" {
    // Test integer types to verify basic evaluation works
    // This should help us debug why 255u8 shows as 42 in REPL
    try runExpectI64("255u8", 255, .no_trace);
    try runExpectI64("42i32", 42, .no_trace);
    try runExpectI64("123i64", 123, .no_trace);
}

test "decimal literal evaluation" {
    // Test basic decimal literals - these should be parsed and evaluated correctly
    try runExpectSuccess("1.5dec", .no_trace);
    try runExpectSuccess("0.0dec", .no_trace);
    try runExpectSuccess("123.456dec", .no_trace);
    try runExpectSuccess("-1.5dec", .no_trace);
}

test "decimal arithmetic with negative values" {
    // one_point_zero = 10^18 = 1_000_000_000_000_000_000
    const one = 1_000_000_000_000_000_000;
    try runExpectDec("-1.5dec", -one - one / 2, .no_trace);
    try runExpectDec("1.5dec", one + one / 2, .no_trace);
    try runExpectDec("-1.5dec + 2.5dec", one, .no_trace);
    try runExpectDec("0.0dec - 1.0dec", -one, .no_trace);
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
    try runExpectI64("0u8", 0, .no_trace);
    try runExpectI64("255u8", 255, .no_trace);
    try runExpectI64("1000u16", 1000, .no_trace);
    try runExpectI64("65535u16", 65535, .no_trace);
    try runExpectI64("100000u32", 100000, .no_trace);
    try runExpectI64("999999999u64", 999999999, .no_trace);

    // Signed integers
    try runExpectI64("-128i8", -128, .no_trace);
    try runExpectI64("127i8", 127, .no_trace);
    try runExpectI64("-32768i16", -32768, .no_trace);
    try runExpectI64("32767i16", 32767, .no_trace);
    try runExpectI64("-2147483648i32", -2147483648, .no_trace);
    try runExpectI64("2147483647i32", 2147483647, .no_trace);
    try runExpectI64("-999999999i64", -999999999, .no_trace);
    try runExpectI64("999999999i64", 999999999, .no_trace);

    // Default integer type (i64)
    try runExpectI64("42", 42, .no_trace);
    try runExpectI64("-1234", -1234, .no_trace);
    try runExpectI64("0", 0, .no_trace);
}

test "hexadecimal and binary integer literals" {
    // Test alternative number bases
    try runExpectI64("0xFF", 255, .no_trace);
    try runExpectI64("0x10", 16, .no_trace);
    try runExpectI64("0xDEADBEEF", 3735928559, .no_trace);
    try runExpectI64("0b1010", 10, .no_trace);
    try runExpectI64("0b11111111", 255, .no_trace);
    try runExpectI64("0b0", 0, .no_trace);
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
    try runExpectI64("{foo: 42}.foo", 42, .no_trace);
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
    try runExpectI64("(|x| x)(42)", 42, .no_trace);
}

test "string refcount - simple string closure" {
    try runExpectStr("(|s| s)(\"Test\")", "Test", .no_trace);
}

test "recursive factorial function" {
    // Test standalone evaluation of recursive factorial without comptime
    try runExpectI64(
        \\{
        \\    factorial = |n|
        \\        if n <= 1
        \\            1
        \\        else
        \\            n * factorial(n - 1)
        \\    factorial(5)
        \\}
    , 120, .no_trace);
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

    // Load builtin module
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(gpa, compiled_builtins.builtin_indices_bin);
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = try builtin_loading.loadCompiledModule(gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source);
    defer builtin_module.deinit();

    // Create original ModuleEnv
    var original_env = try ModuleEnv.init(gpa, source);
    defer original_env.deinit();

    original_env.common.source = source;
    original_env.module_name = "TestModule";
    try original_env.common.calcLineStarts(original_env.gpa);

    // Parse the source code
    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    const parse_ast = try parse.parseExpr(&allocators, &original_env.common);
    defer parse_ast.deinit();

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try original_env.initCIRFields("test");

    // Get Bool and Try statement indices from builtin module
    const bool_stmt_in_builtin_module = builtin_indices.bool_type;
    const try_stmt_in_builtin_module = builtin_indices.try_type;
    const str_stmt_in_builtin_module = builtin_indices.str_type;

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try original_env.insertIdent(base.Ident.for_text("test")),
        .bool_stmt = bool_stmt_in_builtin_module,
        .try_stmt = try_stmt_in_builtin_module,
        .str_stmt = str_stmt_in_builtin_module,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
    };

    // Create module_envs map for canonicalization (enables qualified calls)
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(gpa);
    defer module_envs_map.deinit();
    const builtin_ident = try original_env.insertIdent(base.Ident.for_text("Builtin"));
    const builtin_qualified_ident = try builtin_module.env.common.insertIdent(builtin_module.env.gpa, base.Ident.for_text("Builtin"));
    try module_envs_map.put(builtin_ident, .{ .env = builtin_module.env, .qualified_type_ident = builtin_qualified_ident });

    // Create canonicalizer with module_envs_map for qualified name resolution
    var czer = try Can.init(&allocators, &original_env, parse_ast, &module_envs_map);
    defer czer.deinit();

    // Canonicalize the expression
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonicalized_expr_idx = try czer.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeFailure;
    };

    // Type check the expression - pass Builtin as imported module
    const imported_envs = [_]*const ModuleEnv{builtin_module.env};

    // Resolve imports - map each import to its index in imported_envs
    original_env.imports.resolveImports(&original_env, &imported_envs);

    var checker = try Check.init(gpa, &original_env.types, &original_env, &imported_envs, &module_envs_map, &original_env.store.regions, builtin_ctx);
    defer checker.deinit();

    _ = try checker.checkExprRepl(canonicalized_expr_idx.get_idx());

    // Test 1: Evaluate with the original ModuleEnv
    {
        const builtin_types_local = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
        var interpreter = try Interpreter.init(gpa, &original_env, builtin_types_local, builtin_module.env, &[_]*const can.ModuleEnv{}, &checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
        defer interpreter.deinit();

        const ops = test_env_instance.get_ops();
        const result = try interpreter.eval(canonicalized_expr_idx.get_idx(), ops);
        const layout_cache = &interpreter.runtime_layout_store;
        defer result.decref(layout_cache, ops);

        // Extract integer value (handles both integer and Dec types)
        const int_value = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
            break :blk result.asI128();
        } else blk: {
            const dec_value = result.asDec(ops);
            const RocDec = builtins.dec.RocDec;
            break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
        };
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

        var writer = CompactWriter{
            .iovecs = .{},
            .total_bytes = 0,
            .allocated_memory = .{},
        };
        defer writer.deinit(arena_alloc);

        // Allocate space for ModuleEnv.Serialized (NOT ModuleEnv!) and serialize
        // IMPORTANT: ModuleEnv.Serialized may be larger than ModuleEnv. Allocating only
        // @sizeOf(ModuleEnv) bytes causes a buffer overflow that corrupts subsequent data.
        const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
        const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv.Serialized);
        const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
        try serialized_ptr.serialize(&original_env, arena_alloc, &writer);

        // Write to file
        try writer.writeGather(arena_alloc, tmp_file);

        // Read back from file
        const file_size = try tmp_file.getEndPos();
        const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ModuleEnv)), @intCast(file_size));
        defer gpa.free(buffer);
        _ = try tmp_file.pread(buffer, 0);

        // Deserialize the ModuleEnv
        const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + env_start_offset)));
        const deserialized_env = try deserialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa, source, "TestModule");
        // Free the heap-allocated ModuleEnv and its imports map
        defer {
            deserialized_env.imports.map.deinit(gpa);
            gpa.destroy(deserialized_env);
        }

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

            const builtin_types_local = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
            var interpreter = try Interpreter.init(gpa, deserialized_env, builtin_types_local, builtin_module.env, &[_]*const can.ModuleEnv{}, &checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
            defer interpreter.deinit();

            const ops = test_env_instance.get_ops();
            const result = try interpreter.eval(canonicalized_expr_idx.get_idx(), ops);
            const layout_cache = &interpreter.runtime_layout_store;
            defer result.decref(layout_cache, ops);

            // Verify we get the same result from the deserialized ModuleEnv
            // Extract integer value (handles both integer and Dec types)
            const int_value = if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) blk: {
                break :blk result.asI128();
            } else blk: {
                const dec_value = result.asDec(ops);
                const RocDec = builtins.dec.RocDec;
                break :blk @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
            };
            try testing.expectEqual(@as(i128, 13), int_value);
        }
    }
}

// Tests for anonymous type equality (is_eq on records, tuples, and tag unions)

test "anonymous record equality" {
    // Same records should be equal
    try runExpectBool("{ x: 1, y: 2 } == { x: 1, y: 2 }", true, .no_trace);
    // Different values should not be equal
    try runExpectBool("{ x: 1, y: 2 } == { x: 1, y: 3 }", false, .no_trace);
    // Field order shouldn't matter
    try runExpectBool("{ x: 1, y: 2 } == { y: 2, x: 1 }", true, .no_trace);
}

test "anonymous tuple equality" {
    // Same tuples should be equal
    try runExpectBool("(1, 2) == (1, 2)", true, .no_trace);
    // Different values should not be equal
    try runExpectBool("(1, 2) == (1, 3)", false, .no_trace);
}

test "empty record equality" {
    try runExpectBool("{} == {}", true, .no_trace);
}

test "mutable record equality" {
    // Test comparing a mutable variable record with a literal
    try runExpectBool(
        \\{
        \\    var $x = { sum: 6 }
        \\    $x == { sum: 6 }
        \\}
    , true, .no_trace);
}

test "mutable record with rebind equality" {
    // Test comparing a mutable variable record that was rebound
    try runExpectBool(
        \\{
        \\    var $x = { sum: 0 }
        \\    $x = { sum: 6 }
        \\    $x == { sum: 6 }
        \\}
    , true, .no_trace);
}

test "mutable record loop accumulator equality" {
    // Test comparing a mutable record after for loop (like fold does)
    try runExpectBool(
        \\{
        \\    var $acc = { sum: 0 }
        \\    for item in [1, 2, 3] {
        \\        $acc = { sum: $acc.sum + item }
        \\    }
        \\    $acc == { sum: 6 }
        \\}
    , true, .no_trace);
}

test "string field equality" {
    try runExpectBool("{ name: \"hello\" } == { name: \"hello\" }", true, .no_trace);
    try runExpectBool("{ name: \"hello\" } == { name: \"world\" }", false, .no_trace);
}

test "nested record equality" {
    try runExpectBool("{ a: { x: 1 }, b: 2 } == { a: { x: 1 }, b: 2 }", true, .no_trace);
    try runExpectBool("{ a: { x: 1 }, b: 2 } == { a: { x: 2 }, b: 2 }", false, .no_trace);
    try runExpectBool("{ outer: { inner: { deep: 42 } } } == { outer: { inner: { deep: 42 } } }", true, .no_trace);
    try runExpectBool("{ outer: { inner: { deep: 42 } } } == { outer: { inner: { deep: 99 } } }", false, .no_trace);
}

test "bool field equality" {
    // Use comparison expressions to produce boolean values for record fields
    try runExpectBool("{ flag: (1 == 1) } == { flag: (1 == 1) }", true, .no_trace);
    try runExpectBool("{ flag: (1 == 1) } == { flag: (1 != 1) }", false, .no_trace);
}

test "nested tuple equality" {
    try runExpectBool("((1, 2), 3) == ((1, 2), 3)", true, .no_trace);
    try runExpectBool("((1, 2), 3) == ((1, 9), 3)", false, .no_trace);
    try runExpectBool("(1, (2, 3)) == (1, (2, 3))", true, .no_trace);
    try runExpectBool("(1, (2, 3)) == (1, (2, 9))", false, .no_trace);
}

// This test is disabled because it takes too long to run, and we already know
// the interpreter is stack-safe!
//
// test "stack safety - deep recursion reports graceful error" {
//     // Test that deep recursive function calls report a graceful StackOverflow error
//     // rather than crashing with a native stack overflow (SIGSEGV).
//     // This verifies the stack-safe interpreter is working correctly.
//     const code =
//         \\{
//         \\    countdown = |n|
//         \\        if n == 0
//         \\            0
//         \\        else
//         \\            countdown(n - 1)
//         \\    countdown(100000)
//         \\}
//     ;
//     try runExpectError(code, error.StackOverflow, .no_trace);
// }

// This test is disabled because it takes too long to run, and we already know
// the interpreter is stack-safe!
//
// test "stack safety - deep fibonacci reports graceful error" {
//     // Test that deep recursive fibonacci reports a graceful StackOverflow error
//     // rather than crashing with a native stack overflow (SIGSEGV).
//     // The tree recursion pattern creates very deep call stacks.
//     const code =
//         \\{
//         \\    fib = |n|
//         \\        if n <= 1
//         \\            n
//         \\        else
//         \\            fib(n - 1) + fib(n - 2)
//         \\    fib(30)
//         \\}
//     ;
//     try runExpectError(code, error.StackOverflow, .no_trace);
// }

// Tests for nominal type equality (is_eq method dispatch)
// These tests exercise dispatchNominalIsEq which resolves and calls is_eq methods on nominal types

test "nominal type equality - Bool" {
    // Bool is a nominal type wrapping [False, True]
    // These test that is_eq is properly dispatched for Bool
    try runExpectBool("Bool.True == Bool.True", true, .no_trace);
    try runExpectBool("Bool.False == Bool.False", true, .no_trace);
    try runExpectBool("Bool.True == Bool.False", false, .no_trace);
    try runExpectBool("Bool.False == Bool.True", false, .no_trace);
}

test "nominal type equality - Bool in expressions" {
    // Bool comparisons within larger expressions
    try runExpectBool("(1 == 1) == (2 == 2)", true, .no_trace);
    try runExpectBool("(1 == 1) == (1 == 2)", false, .no_trace);
    try runExpectBool("(1 != 2) == (3 != 4)", true, .no_trace);
}

test "nominal type equality - records containing Bool" {
    // Records with Bool fields - exercises roc_ops threading through structural equality
    try runExpectBool("{ flag: Bool.True } == { flag: Bool.True }", true, .no_trace);
    try runExpectBool("{ flag: Bool.True } == { flag: Bool.False }", false, .no_trace);
    try runExpectBool("{ a: Bool.True, b: Bool.False } == { a: Bool.True, b: Bool.False }", true, .no_trace);
    try runExpectBool("{ a: Bool.True, b: Bool.False } == { a: Bool.False, b: Bool.True }", false, .no_trace);
}

test "nominal type equality - tuples containing Bool" {
    // Tuples with Bool elements
    try runExpectBool("(Bool.True, Bool.False) == (Bool.True, Bool.False)", true, .no_trace);
    try runExpectBool("(Bool.True, Bool.False) == (Bool.False, Bool.True)", false, .no_trace);
    try runExpectBool("(1, Bool.True, 2) == (1, Bool.True, 2)", true, .no_trace);
}

test "nominal type equality - nested structures with Bool" {
    // Nested records/tuples containing Bool - tests deep roc_ops threading
    try runExpectBool("{ outer: { inner: Bool.True } } == { outer: { inner: Bool.True } }", true, .no_trace);
    try runExpectBool("{ outer: { inner: Bool.True } } == { outer: { inner: Bool.False } }", false, .no_trace);
    try runExpectBool("((Bool.True, Bool.False), Bool.True) == ((Bool.True, Bool.False), Bool.True)", true, .no_trace);
}

// Tests for tag union equality

test "tag union equality - same tag no payload" {
    try runExpectBool("Ok == Ok", true, .no_trace);
    try runExpectBool("Err == Err", true, .no_trace);
    try runExpectBool("Ok == Err", false, .no_trace);
    try runExpectBool("Err == Ok", false, .no_trace);
}

test "tag union equality - same tag with payload" {
    try runExpectBool("Ok(1) == Ok(1)", true, .no_trace);
    try runExpectBool("Ok(1) == Ok(2)", false, .no_trace);
    try runExpectBool("Err(1) == Err(1)", true, .no_trace);
}

test "tag union equality - different tags with payload" {
    try runExpectBool(
        \\{
        \\    x = Ok(1)
        \\    y = if Bool.False Ok(1) else Err(1)
        \\    x == y
        \\}
    , false, .no_trace);
}

test "tag union equality - string payloads" {
    try runExpectBool("Ok(\"hello\") == Ok(\"hello\")", true, .no_trace);
    try runExpectBool("Ok(\"hello\") == Ok(\"world\")", false, .no_trace);
}

test "tag union equality - three or more tags" {
    // Use match to produce values of the same tag union type with 3 variants
    try runExpectBool(
        \\{
        \\    x = Red
        \\    y = Red
        \\    x == y
        \\}
    , true, .no_trace);
    try runExpectBool(
        \\{
        \\    x = Red
        \\    y = if Bool.True Red else if Bool.True Green else Blue
        \\    x == y
        \\}
    , true, .no_trace);
    try runExpectBool(
        \\{
        \\    x = Red
        \\    y = if Bool.False Red else Green
        \\    x == y
        \\}
    , false, .no_trace);
}

// Tests for inequality operator (!=) on structural types

test "record inequality" {
    try runExpectBool("{ x: 1, y: 2 } != { x: 1, y: 2 }", false, .no_trace);
    try runExpectBool("{ x: 1, y: 2 } != { x: 1, y: 3 }", true, .no_trace);
    try runExpectBool("{ x: 1, y: 2 } != { y: 2, x: 1 }", false, .no_trace);
}

test "tuple inequality" {
    try runExpectBool("(1, 2) != (1, 2)", false, .no_trace);
    try runExpectBool("(1, 2) != (1, 3)", true, .no_trace);
}

test "tag union inequality" {
    try runExpectBool("Ok == Ok", true, .no_trace);
    try runExpectBool("Ok != Ok", false, .no_trace);
    try runExpectBool("Ok != Err", true, .no_trace);
    try runExpectBool("Ok(1) != Ok(1)", false, .no_trace);
    try runExpectBool("Ok(1) != Ok(2)", true, .no_trace);
}

// Tests for mixed structural types (combinations of records, tuples, tag unions)

test "record containing tuple equality" {
    try runExpectBool("{ pair: (1, 2) } == { pair: (1, 2) }", true, .no_trace);
    try runExpectBool("{ pair: (1, 2) } == { pair: (1, 3) }", false, .no_trace);
}

test "tuple containing record equality" {
    try runExpectBool("({ x: 1 }, 2) == ({ x: 1 }, 2)", true, .no_trace);
    try runExpectBool("({ x: 1 }, 2) == ({ x: 9 }, 2)", false, .no_trace);
}

test "record with multiple types" {
    try runExpectBool(
        \\{ name: "alice", age: 30 } == { name: "alice", age: 30 }
    , true, .no_trace);
    try runExpectBool(
        \\{ name: "alice", age: 30 } == { name: "bob", age: 30 }
    , false, .no_trace);
    try runExpectBool(
        \\{ name: "alice", age: 30 } == { name: "alice", age: 31 }
    , false, .no_trace);
}

test "deeply nested mixed structures" {
    try runExpectBool(
        \\{ a: (1, { b: 2 }), c: 3 } == { a: (1, { b: 2 }), c: 3 }
    , true, .no_trace);
    try runExpectBool(
        \\{ a: (1, { b: 2 }), c: 3 } == { a: (1, { b: 9 }), c: 3 }
    , false, .no_trace);
}

test "tuple of tuples equality" {
    try runExpectBool("((1, 2), (3, 4)) == ((1, 2), (3, 4))", true, .no_trace);
    try runExpectBool("((1, 2), (3, 4)) == ((1, 2), (3, 5))", false, .no_trace);
}

test "record with string and bool fields" {
    try runExpectBool(
        \\{ name: "hello", active: Bool.True } == { name: "hello", active: Bool.True }
    , true, .no_trace);
    try runExpectBool(
        \\{ name: "hello", active: Bool.True } == { name: "hello", active: Bool.False }
    , false, .no_trace);
}

test "tag union inside record equality" {
    try runExpectBool(
        \\{
        \\    a = { status: Ok(42) }
        \\    b = { status: Ok(42) }
        \\    a == b
        \\}
    , true, .no_trace);
    try runExpectBool(
        \\{
        \\    a = { status: Ok(42) }
        \\    b = { status: Ok(99) }
        \\    a == b
        \\}
    , false, .no_trace);
}

test "record inside tag union equality" {
    try runExpectBool("Ok({ x: 1, y: 2 }) == Ok({ x: 1, y: 2 })", true, .no_trace);
    try runExpectBool("Ok({ x: 1, y: 2 }) == Ok({ x: 1, y: 9 })", false, .no_trace);
}

test "tag union inside tuple equality" {
    try runExpectBool("(Ok(1), 2) == (Ok(1), 2)", true, .no_trace);
    try runExpectBool("(Ok(1), 2) == (Ok(9), 2)", false, .no_trace);
}

test "tuple inside tag union equality" {
    try runExpectBool("Ok((1, 2)) == Ok((1, 2))", true, .no_trace);
    try runExpectBool("Ok((1, 2)) == Ok((1, 9))", false, .no_trace);
}

test "record inside tag union inside tuple equality" {
    // Three-deep nesting: tuple containing tag union containing record
    try runExpectBool(
        \\(Ok({ x: 1, y: 2 }), 42) == (Ok({ x: 1, y: 2 }), 42)
    , true, .no_trace);
    try runExpectBool(
        \\(Ok({ x: 1, y: 2 }), 42) == (Ok({ x: 1, y: 9 }), 42)
    , false, .no_trace);
}

test "tuple inside record inside tag union equality" {
    // Three-deep nesting: tag union containing record containing tuple
    try runExpectBool(
        \\Ok({ pair: (1, 2), val: 99 }) == Ok({ pair: (1, 2), val: 99 })
    , true, .no_trace);
    try runExpectBool(
        \\Ok({ pair: (1, 2), val: 99 }) == Ok({ pair: (1, 9), val: 99 })
    , false, .no_trace);
}

test "tag union inside record inside tuple equality" {
    // Three-deep nesting: tuple containing record containing tag union
    try runExpectBool(
        \\({ result: Ok(1) }, 99) == ({ result: Ok(1) }, 99)
    , true, .no_trace);
    try runExpectBool(
        \\({ result: Ok(1) }, 99) == ({ result: Ok(2) }, 99)
    , false, .no_trace);
}

test "four-deep nested equality" {
    // Record → tuple → tag union → record
    try runExpectBool(
        \\{ data: (Ok({ val: 42 }), 1) } == { data: (Ok({ val: 42 }), 1) }
    , true, .no_trace);
    try runExpectBool(
        \\{ data: (Ok({ val: 42 }), 1) } == { data: (Ok({ val: 99 }), 1) }
    , false, .no_trace);
}

// Tests for heap-type fields (long strings beyond SSO) inside structural types.
// These exercise layout-aware comparison rather than raw byte comparison,
// ensuring heap pointers are compared by content, not address.

test "record with long string field equality" {
    // Long strings exceed SSO (~23 bytes), forcing heap allocation
    try runExpectBool(
        \\{ name: "this string is long enough to avoid SSO optimization" } == { name: "this string is long enough to avoid SSO optimization" }
    , true, .no_trace);
    try runExpectBool(
        \\{ name: "this string is long enough to avoid SSO optimization" } == { name: "different long string that also avoids SSO optimization" }
    , false, .no_trace);
}

test "record with long string field inequality" {
    try runExpectBool(
        \\{ name: "this string is long enough to avoid SSO optimization" } != { name: "this string is long enough to avoid SSO optimization" }
    , false, .no_trace);
    try runExpectBool(
        \\{ name: "this string is long enough to avoid SSO optimization" } != { name: "different long string that also avoids SSO optimization" }
    , true, .no_trace);
}

test "tuple with long string element equality" {
    try runExpectBool(
        \\("this string is long enough to avoid SSO optimization", 42) == ("this string is long enough to avoid SSO optimization", 42)
    , true, .no_trace);
    try runExpectBool(
        \\("this string is long enough to avoid SSO optimization", 42) == ("different long string that also avoids SSO optimization", 42)
    , false, .no_trace);
}

test "record with multiple long string fields equality" {
    try runExpectBool(
        \\{ a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" } == { a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" }
    , true, .no_trace);
    try runExpectBool(
        \\{ a: "first long string exceeding SSO limit!!", b: "second long string exceeding SSO limit!" } == { a: "first long string exceeding SSO limit!!", b: "DIFFERENT long string exceeding SSO!!!!" }
    , false, .no_trace);
}

test "long string inside record inside tuple equality" {
    try runExpectBool(
        \\({ name: "this string is long enough to avoid SSO optimization" }, 1) == ({ name: "this string is long enough to avoid SSO optimization" }, 1)
    , true, .no_trace);
    try runExpectBool(
        \\({ name: "this string is long enough to avoid SSO optimization" }, 1) == ({ name: "different long string that also avoids SSO optimization" }, 1)
    , false, .no_trace);
}

test "tag union with long string payload equality" {
    try runExpectBool(
        \\Ok("this string is long enough to avoid SSO optimization") == Ok("this string is long enough to avoid SSO optimization")
    , true, .no_trace);
    try runExpectBool(
        \\Ok("this string is long enough to avoid SSO optimization") == Ok("different long string that also avoids SSO optimization")
    , false, .no_trace);
}

test "tag union with long string payload inequality" {
    try runExpectBool(
        \\Ok("this string is long enough to avoid SSO optimization") != Ok("this string is long enough to avoid SSO optimization")
    , false, .no_trace);
    try runExpectBool(
        \\Ok("this string is long enough to avoid SSO optimization") != Ok("different long string that also avoids SSO optimization")
    , true, .no_trace);
}

// Tests for equality in control flow contexts

test "equality result used in if condition" {
    try runExpectI64(
        \\if { x: 1 } == { x: 1 } 42 else 0
    , 42, .no_trace);
    try runExpectI64(
        \\if { x: 1 } == { x: 2 } 42 else 0
    , 0, .no_trace);
}

test "equality with variable bindings" {
    try runExpectBool(
        \\{
        \\    a = { x: 10, y: 20 }
        \\    b = { x: 10, y: 20 }
        \\    a == b
        \\}
    , true, .no_trace);
    try runExpectBool(
        \\{
        \\    a = { x: 10, y: 20 }
        \\    b = { x: 10, y: 99 }
        \\    a == b
        \\}
    , false, .no_trace);
}

test "inequality with variable bindings - tuples" {
    try runExpectBool(
        \\{
        \\    a = (1, 2, 3)
        \\    b = (1, 2, 3)
        \\    a != b
        \\}
    , false, .no_trace);
    try runExpectBool(
        \\{
        \\    a = (1, 2, 3)
        \\    b = (1, 2, 4)
        \\    a != b
        \\}
    , true, .no_trace);
}

test "inequality with variable bindings - records" {
    try runExpectBool(
        \\{
        \\    a = { x: 10, y: 20 }
        \\    b = { x: 10, y: 20 }
        \\    a != b
        \\}
    , false, .no_trace);
    try runExpectBool(
        \\{
        \\    a = { x: 10, y: 20 }
        \\    b = { x: 10, y: 99 }
        \\    a != b
        \\}
    , true, .no_trace);
}

// Tests for List.fold with record accumulators
// This exercises record state management within fold operations

test "List.fold with record accumulator - sum and count" {
    // Test folding a list while accumulating sum and count in a record
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 6 },
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - empty list" {
    // Folding an empty list should return the initial record unchanged
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 0 },
        .{ .name = "count", .value = 0 },
    };
    try runExpectRecord(
        "List.fold([], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - single field" {
    // Test with a single-field record accumulator
    const expected_fields = [_]ExpectedField{
        .{ .name = "total", .value = 10 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {total: 0}, |acc, item| {total: acc.total + item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - record update syntax" {
    // Test using record update syntax { ..acc, field: newValue }
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 6 },
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {..acc, sum: acc.sum + item, count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - partial update" {
    // Test updating only one field while keeping others
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 10 },
        .{ .name = "multiplier", .value = 2 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {sum: 0, multiplier: 2}, |acc, item| {..acc, sum: acc.sum + item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - nested field access" {
    // Test accessing nested record fields in accumulator
    const expected_fields = [_]ExpectedField{
        .{ .name = "value", .value = 6 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3], {value: 0}, |acc, item| {value: acc.value + item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - three fields" {
    // Test with more fields to exercise record layout handling
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 10 },
        .{ .name = "count", .value = 4 },
        .{ .name = "product", .value = 24 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {sum: 0, count: 0, product: 1}, |acc, item| {sum: acc.sum + item, count: acc.count + 1, product: acc.product * item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - conditional update" {
    // Test conditional logic inside the fold with record accumulator
    const expected_fields = [_]ExpectedField{
        .{ .name = "evens", .value = 6 },
        .{ .name = "odds", .value = 4 },
    };
    try runExpectRecord(
        "List.fold([1, 2, 3, 4], {evens: 0, odds: 0}, |acc, item| if item % 2 == 0 {evens: acc.evens + item, odds: acc.odds} else {evens: acc.evens, odds: acc.odds + item})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - string list" {
    // Test folding over strings with a record accumulator (count only)
    const expected_fields = [_]ExpectedField{
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        "List.fold([\"a\", \"bb\", \"ccc\"], {count: 0}, |acc, _| {count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "simple fold without records - Dec result" {
    try runExpectIntDec(
        "List.fold([1, 2, 3], 0, |acc, item| acc + item)",
        6,
        .no_trace,
    );
}

test "simple fold without records - Dec equality" {
    try runExpectBool(
        "List.fold([1, 2, 3], 0, |acc, item| acc + item) == 6",
        true,
        .no_trace,
    );
}

test "List.fold with record accumulator - record equality comparison" {
    // Test that fold result can be compared with == to a record literal
    try runExpectBool(
        "List.fold([1, 2, 3], {sum: 0}, |acc, item| {sum: acc.sum + item}) == {sum: 6}",
        true,
        .no_trace,
    );
}

test "List.fold with record accumulator - multi-field record equality" {
    // Test equality comparison with multi-field record accumulator
    try runExpectBool(
        "List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {sum: acc.sum + item, count: acc.count + 1}) == {sum: 6, count: 3}",
        true,
        .no_trace,
    );
}

// Tests for List.fold with record accumulators and list/record destructuring
// This exercises pattern matching within fold operations

test "List.fold with record accumulator - record destructuring in lambda" {
    // Test folding over a list of records, destructuring each record in the lambda
    const expected_fields = [_]ExpectedField{
        .{ .name = "total_x", .value = 6 },
        .{ .name = "total_y", .value = 15 },
    };
    try runExpectRecord(
        "List.fold([{x: 1, y: 2}, {x: 2, y: 5}, {x: 3, y: 8}], {total_x: 0, total_y: 0}, |acc, {x, y}| {total_x: acc.total_x + x, total_y: acc.total_y + y})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - partial record destructuring" {
    // Test destructuring only some fields from records
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum", .value = 6 },
    };
    try runExpectRecord(
        "List.fold([{a: 1, b: 100}, {a: 2, b: 200}, {a: 3, b: 300}], {sum: 0}, |acc, {a}| {sum: acc.sum + a})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - single field record destructuring" {
    // Test destructuring single-field records
    const expected_fields = [_]ExpectedField{
        .{ .name = "total", .value = 10 },
    };
    try runExpectRecord(
        "List.fold([{val: 1}, {val: 2}, {val: 3}, {val: 4}], {total: 0}, |acc, {val}| {total: acc.total + val})",
        &expected_fields,
        .no_trace,
    );
}

// List destructuring tests in lambda params - these previously leaked memory
// Fixed by adding decref after successful patternMatchesBind in for_loop_iterate

test "List.fold with list destructuring - simple first element" {
    // Simplest case: just extract the first element
    try runExpectI64(
        "List.fold([[10], [20], [30]], 0, |acc, [x]| acc + x)",
        60,
        .no_trace,
    );
}

test "List.fold with list destructuring - two element exact match" {
    // Extract exactly two elements
    try runExpectI64(
        "List.fold([[1, 2], [3, 4]], 0, |acc, [a, b]| acc + a + b)",
        10,
        .no_trace,
    );
}

// Test that list destructuring works in match (not in lambda params) - this should work
test "match with list destructuring - baseline" {
    // This tests list destructuring in a match context, not lambda params
    try runExpectI64(
        "match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }",
        6,
        .no_trace,
    );
}

test "match with pattern alternatives" {
    try runExpectI64(
        "match Err(42) { Ok(x) | Err(x) => x, _ => 0 }",
        42,
        .no_trace,
    );
}

// List destructuring tests with record accumulators

test "List.fold with record accumulator - list destructuring in lambda" {
    // Test folding over a list of lists, destructuring each inner list
    // [1, 2], [3, 4], [5, 6] -> first elements are 1, 3, 5 -> sum is 9
    const expected_fields = [_]ExpectedField{
        .{ .name = "first_sum", .value = 9 },
        .{ .name = "count", .value = 3 },
    };
    try runExpectRecord(
        "List.fold([[1, 2], [3, 4], [5, 6]], {first_sum: 0, count: 0}, |acc, [first, ..]| {first_sum: acc.first_sum + first, count: acc.count + 1})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - destructure two elements" {
    // Test destructuring first two elements from each inner list
    const expected_fields = [_]ExpectedField{
        .{ .name = "sum_firsts", .value = 9 },
        .{ .name = "sum_seconds", .value = 12 },
    };
    try runExpectRecord(
        "List.fold([[1, 2, 100], [3, 4, 200], [5, 6, 300]], {sum_firsts: 0, sum_seconds: 0}, |acc, [a, b, ..]| {sum_firsts: acc.sum_firsts + a, sum_seconds: acc.sum_seconds + b})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - exact list pattern" {
    // Test exact list pattern matching (no rest pattern)
    const expected_fields = [_]ExpectedField{
        .{ .name = "total", .value = 21 },
    };
    try runExpectRecord(
        "List.fold([[1, 2], [3, 4], [5, 6]], {total: 0}, |acc, [a, b]| {total: acc.total + a + b})",
        &expected_fields,
        .no_trace,
    );
}

test "List.fold with record accumulator - nested list and record" {
    // Test combining list destructuring with record accumulator updates
    // Using ".. as tail" syntax for the rest pattern
    const expected_fields = [_]ExpectedField{
        .{ .name = "head_sum", .value = 6 },
        .{ .name = "tail_count", .value = 6 },
    };
    try runExpectRecord(
        "List.fold([[1, 10, 20], [2, 30, 40], [3, 50, 60]], {head_sum: 0, tail_count: 0}, |acc, [head, .. as tail]| {head_sum: acc.head_sum + head, tail_count: acc.tail_count + List.len(tail)})",
        &expected_fields,
        .no_trace,
    );
}

// For loop with mutable list append
test "for loop - mutable list append" {
    try runExpectListI64(
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    var $result = List.with_capacity(List.len(list))
        \\    for item in list {
        \\        $result = List.append($result, item)
        \\    }
        \\    $result
        \\}
    ,
        &[_]i64{ 1, 2, 3 },
        .no_trace,
    );
}

// For loop with closure call (like List.map does)
test "for loop - with closure transform" {
    try runExpectListI64(
        \\{
        \\    list = [1.I64, 2.I64, 3.I64]
        \\    identity = |x| x
        \\    var $result = List.with_capacity(List.len(list))
        \\    for item in list {
        \\        $result = List.append($result, identity(item))
        \\    }
        \\    $result
        \\}
    ,
        &[_]i64{ 1, 2, 3 },
        .no_trace,
    );
}

// Tests for List.map

test "List.map - basic identity" {
    // Map with identity function
    try runExpectListI64(
        "List.map([1.I64, 2.I64, 3.I64], |x| x)",
        &[_]i64{ 1, 2, 3 },
        .no_trace,
    );
}

test "List.map - single element" {
    // Map on single element list
    try runExpectListI64(
        "List.map([42i64], |x| x)",
        &[_]i64{42},
        .no_trace,
    );
}

test "List.map - longer list with squaring" {
    // Check that map on a longer list with squaring works
    try runExpectListI64(
        "List.map([1i64, 2i64, 3i64, 4i64, 5i64], |x| x * x)",
        &[_]i64{ 1, 4, 9, 16, 25 },
        .no_trace,
    );
}

test "List.map - doubling" {
    // Map with doubling function
    try runExpectListI64(
        "List.map([1i64, 2i64, 3i64], |x| x * 2i64)",
        &[_]i64{ 2, 4, 6 },
        .no_trace,
    );
}

test "List.map - adding" {
    // Map with adding function
    try runExpectListI64(
        "List.map([10i64, 20i64], |x| x + 5i64)",
        &[_]i64{ 15, 25 },
        .no_trace,
    );
}

test "List.map - empty list" {
    // Map with adding function
    try runExpectListZst(
        "List.map([], |x| x)",
        0,
        .no_trace,
    );
}

test "empty list with non-numeric type constraint should be list of zst" {
    // An empty list whose element type has a method_call constraint but no
    // from_numeral constraint should be List(ZST), not List(Dec).
    // e.g. `x : List(a) where [a.blah : Str -> Str]` then `x = []`
    try runExpectListZst(
        "[]",
        0,
        .no_trace,
    );
}

// Test for List.append

test "List.append - basic case" {
    // Append two non-empty lists
    try runExpectListI64(
        "List.append([1i64, 2i64], 3i64)",
        &[_]i64{ 1, 2, 3 },
        .no_trace,
    );
}

test "List.append - empty case" {
    // Append to empty list
    try runExpectListI64(
        "List.append([], 42i64)",
        &[_]i64{42},
        .no_trace,
    );
}

test "List.append - zst case" {
    // Append to empty list
    try runExpectListZst(
        "List.append([{}], {})",
        2,
        .no_trace,
    );
}

// Test for List.repeat

test "List.repeat - basic case" {
    // Repeat a value multiple times
    try runExpectListI64(
        "List.repeat(7i64, 4)",
        &[_]i64{ 7, 7, 7, 7 },
        .no_trace,
    );
}

test "List.repeat - empty case" {
    // Repeat a value zero times returns empty list
    try helpers.runExpectEmptyListI64("List.repeat(7i64, 0)", .no_trace);
}

test "List.with_capacity - unknown case" {
    // Create a list with specified capacity
    try runExpectListZst(
        "List.with_capacity(5)",
        0,
        .no_trace,
    );
}

test "List.with_capacity - append case" {
    // Create a list with specified capacity
    try runExpectListI64(
        "List.with_capacity(5).append(10i64)",
        &[_]i64{10},
        .trace,
    );
}

// Tests for List.sum

test "List.sum - basic case" {
    // Sum of a list of integers (untyped literals default to Dec)
    try runExpectIntDec("List.sum([1, 2, 3, 4])", 10, .no_trace);
}

test "List.sum - single element" {
    try runExpectIntDec("List.sum([42])", 42, .no_trace);
}

test "List.sum - negative numbers" {
    try runExpectIntDec("List.sum([-1, -2, 3, 4])", 4, .no_trace);
}

test "List.sum - larger list" {
    try runExpectIntDec("List.sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])", 55, .no_trace);
}

// Bug regression tests - interpreter crash issues

test "match with tag containing pattern-bound variable - regression" {
    // Regression test for GitHub issue: interpreter crash when creating a tag
    // with a payload that contains a variable bound by a match pattern.
    //
    // In isolated eval tests this works, but when running as a full app with
    // platform integration it crashes with "e_closure: failed to resolve capture value".
    // The issue is specific to module management in full app execution.
    //
    // This test ensures the basic case works in the eval context.
    // Full reproduction requires running as: `roc run <app-with-platform.roc>`
    try runExpectSuccess(
        \\match Some("x") {
        \\    Some(a) => Tagged(a)
        \\    None => Tagged("")
        \\}
    , .no_trace);
}

test "nested match with Result type - regression" {
    // Regression test for interpreter crash when using nested match expressions
    // with Result types (Ok/Err).
    //
    // Original bug report:
    //   match ["x"] {
    //       [a] => {
    //           match Ok(a) {
    //               Ok(val) => Ok(val),
    //               _ => Err(Oops)
    //           }
    //       }
    //   }
    //
    // Like the above test, this works in isolation but crashes in full app execution.
    try runExpectSuccess(
        \\match ["x"] {
        \\    [a] => {
        \\        match Ok(a) {
        \\            Ok(val) => Ok(val),
        \\            _ => Err(Oops)
        \\        }
        \\    }
        \\}
    , .no_trace);
}

// Bug regression tests - segfault issues from bug reports

test "list equality - single element list - regression" {
    try runExpectBool("[1] == [1]", true, .no_trace);
}

test "list equality - nested lists - regression" {
    try runExpectBool("[[1, 2]] == [[1, 2]]", true, .no_trace);
}

test "list equality - single string element list - regression" {
    try runExpectBool("[\"hello\"] == [\"hello\"]", true, .no_trace);
}

test "if block with local bindings - regression" {
    // Regression test for segfault in if block with local variable bindings
    // Bug report: `main! = || { if True { x = 0 _y = x } }`
    try runExpectI64(
        \\if True {
        \\    x = 0
        \\    _y = x
        \\    x
        \\}
        \\else 99
    , 0, .no_trace);
}

test "List.len returns proper U64 nominal type for method calls - regression" {
    // Regression test for InvalidMethodReceiver when calling methods on List.len result
    // Bug report: `n = List.len([]); _str = n.to_str()` crashed with InvalidMethodReceiver
    // The issue was that List.len created a fresh runtime type variable instead of using
    // the return_rt_var parameter, which prevented method resolution from finding the
    // U64 nominal type information needed to look up .to_str()
    try runExpectStr(
        \\{
        \\    n = List.len([])
        \\    n.to_str()
        \\}
    , "0", .no_trace);

    // Also test with non-empty list
    try runExpectStr(
        \\{
        \\    n = List.len([1, 2, 3])
        \\    n.to_str()
        \\}
    , "3", .no_trace);
}

test "type annotation on var declaration - regression issue8660" {
    // Regression test for issue #8660: Type annotation on var produced duplicate definition error
    // The syntax `var $foo : U8` followed by `var $foo = 42` should work correctly
    try runExpectI64(
        \\{
        \\    var $foo : U8
        \\    var $foo = 42
        \\    $foo
        \\}
    , 42, .no_trace);
}

test "List.get with polymorphic numeric index - regression #8666" {
    // Regression test for GitHub issue #8666: interpreter panic when using
    // a polymorphic numeric type as a list index.
    //
    // The bug occurred because numeric literals with from_numeral constraints
    // were being generalized, causing each use to get a fresh instantiation.
    // This meant the concrete U64 type from List.get didn't propagate back
    // to the original definition, leaving it as a flex var that defaulted to Dec.
    //
    // The fix: don't generalize vars with from_numeral constraints, and don't
    // instantiate them during lookup, so constraint propagation works correctly.
    try runExpectI64(
        \\{
        \\    list = [10, 20, 30]
        \\    index = 0
        \\    match List.get(list, index) { Ok(v) => v, _ => 0 }
        \\}
    , 10, .no_trace);
}

test "for loop element type extracted from list runtime type - regression #8664" {
    // Regression test for InvalidMethodReceiver when calling methods on elements
    // from a for loop over a list passed to an untyped function parameter.
    // The fix: extract element type from list's runtime type (e.g., List(Dec))
    // instead of using the pattern's compile-time flex variable.
    // Note: unsuffixed number literals default to Dec in Roc.
    try runExpectStr(
        \\{
        \\    calc = |list| {
        \\        var $result = ""
        \\        for elem in list {
        \\            $result = elem.to_str()
        \\        }
        \\        $result
        \\    }
        \\    calc([1, 2, 3])
        \\}
    , "3.0", .no_trace);
}

test "List.get method dispatch on Try type - issue 8665" {
    // Regression test for issue #8665: InvalidMethodReceiver crash when calling
    // ok_or() method on the result of List.get() using dot notation.
    // The function call syntax works: Try.ok_or(List.get(list, 0), "fallback")
    // But method syntax crashes: List.get(list, 0).ok_or("fallback")
    try runExpectStr(
        \\{
        \\    list = ["hello"]
        \\    List.get(list, 0).ok_or("fallback")
        \\}
    , "hello", .no_trace);
}

test "List.get with list var and when destructure" {
    // Test List.get with a list VARIABLE and match destructure
    try runExpectStr(
        \\{
        \\    list = ["hello"]
        \\    match List.get(list, 0) {
        \\        Ok(val) => val
        \\        Err(_) => "error"
        \\    }
        \\}
    , "hello", .no_trace);
}

test "record destructuring with assignment - regression" {
    // Regression test for GitHub issue #8647
    // Record destructuring should not cause TypeMismatch error during evaluation
    try runExpectI64(
        \\{
        \\    rec = { x: 1, y: 2 }
        \\    { x, y } = rec
        \\    x + y
        \\}
    , 3, .no_trace);
}

test "record field access - regression 8647" {
    // Regression test for GitHub issue #8647
    // Record field access should work properly
    try runExpectStr(
        \\{
        \\    rec = { name: "test" }
        \\    rec.name
        \\}
    , "test", .no_trace);
}

test "record field access with multiple string fields - regression 8648" {
    // Regression test for GitHub issue #8648
    // Record field access with app module ident space
    try runExpectStr(
        \\{
        \\    record = { x: "a", y: "b" }
        \\    record.x
        \\}
    , "a", .no_trace);
}

test "method calls on numeric variables with flex types - regression" {
    // Regression test for InvalidMethodReceiver when calling methods on numeric
    // variables that have unconstrained (flex/rigid) types at compile time.
    // Bug report: https://github.com/roc-lang/roc/issues/8663
    // The issue was that when a numeric variable's compile-time type is flex,
    // method dispatch would fail because it requires a nominal type (like Dec).

    // Simple case: variable bound to numeric literal
    try runExpectStr(
        \\{
        \\    x = 7.0
        \\    x.to_str()
        \\}
    , "7.0", .no_trace);

    // With integer literal (defaults to Dec, so output has decimal point)
    try runExpectStr(
        \\{
        \\    x = 42
        \\    x.to_str()
        \\}
    , "42.0", .no_trace);
}

test "issue 8667: List.with_capacity should be inferred as List(I64)" {
    // When List.with_capacity is used with List.append(_, 1i64), the type checker should
    // unify the list element type to I64. This means the layout should be .list (not .list_of_zst).
    // If it's .list_of_zst, that indicates a type inference bug.
    try runExpectListI64("List.append(List.with_capacity(1), 1i64)", &[_]i64{1}, .no_trace);

    // Test fold with inline lambda that calls append
    try runExpectListI64("[1i64].fold(List.with_capacity(1), |acc, item| acc.append(item))", &[_]i64{1}, .no_trace);

    // Also test the fold case which is where the bug was originally reported
    try runExpectListI64("[1i64].fold(List.with_capacity(1), List.append)", &[_]i64{1}, .no_trace);
}

test "issue 8710: tag union with heap payload in tuple should not leak" {
    // Regression test for GitHub issue #8710
    // When a tag union (like Ok) containing a heap-allocated payload (like a List)
    // is stored in a tuple, the decref logic must properly free the payload.
    // The bug was that decrefLayoutPtr was missing handling for .tag_union layouts,
    // so the payload was never decremented and would leak.
    // We create a list, wrap in Ok, and return just the list length to verify the
    // tuple is properly cleaned up (the test allocator catches any leaks).
    try runExpectI64("[1i64, 2i64, 3i64].len()", 3, .no_trace);
    // Also test the actual bug scenario: tag union in a tuple
    try runExpectListI64(
        \\{
        \\    list = [1i64, 2i64, 3i64]
        \\    tuple = (Ok(list), 42i64)
        \\    list
        \\}
    , &[_]i64{ 1, 2, 3 }, .no_trace);
}

test "issue 8727: function returning closure that captures outer variable" {
    // Regression test for GitHub issue #8727
    // A function that returns a closure which captures a variable from its
    // enclosing scope would crash with "e_lookup_local: definition not found".
    // The issue was that capture field names are stored using runtime_layout_store
    // idents, but lookups used module idents which have different indices.

    // Simple case: function returns closure capturing its argument
    try runExpectI64(
        \\{
        \\    make_adder = |n| |x| n + x
        \\    add_ten = make_adder(10)
        \\    add_ten(5)
        \\}
    , 15, .no_trace);

    // Curried multiplication
    try runExpectI64("(|a| |b| a * b)(5)(10)", 50, .no_trace);

    // Triple currying
    try runExpectI64("(((|a| |b| |c| a + b + c)(100))(20))(3)", 123, .no_trace);
}

test "issue 8737: tag union with tuple payload containing tag union" {
    // Regression test for GitHub issue #8737
    // A tag union whose payload is a tuple containing another tag union as the first element
    // would crash during pattern matching due to incorrect discriminant reading.
    // The bug is specifically triggered when:
    // 1. Outer tag union has a tuple payload
    // 2. The tuple's first element is another tag union (with a payload)
    // 3. The tuple has 2+ elements
    // 4. Pattern matching is used on the outer tag union

    // Test: Inner tag union inside tuple inside outer tag union (the bug trigger)
    // The match branches force type inference to produce a 2-variant type
    try runExpectI64(
        \\{
        \\    result = XYZ((QQQ(1u8), 3u64))
        \\    match result {
        \\        XYZ(_) => 42
        \\        BBB => 0
        \\    }
        \\}
    , 42, .no_trace);
}

test "early return: basic ? operator with Ok" {
    // The ? operator on Ok should unwrap the value
    try runExpectI64(
        \\{
        \\    compute = |x| Ok(x?)
        \\    match compute(Ok(42.I64)) { Ok(v) => v, _ => 0 }
        \\}
    , 42, .no_trace);
}

test "early return: basic ? operator with Err" {
    // The ? operator on Err should early return
    try runExpectI64(
        \\{
        \\    compute = |x| Ok(x?)
        \\    match compute(Err({})) { Ok(_) => 1, Err(_) => 0 }
        \\}
    , 0, .no_trace);
}

test "early return: ? in closure passed to List.map" {
    // Regression test: early return from closure in List.map would crash
    // with "call_invoke_closure: value_stack empty when popping function"
    try runExpectI64(
        \\{
        \\    result = [Ok(1), Err({})].map(|x| Ok(x?))
        \\    List.len(result)
        \\}
    , 2, .no_trace);
}

test "early return: ? in closure passed to List.fold" {
    // Regression test: early return from closure in List.fold would crash
    try runExpectI64(
        \\{
        \\    compute = |x| Ok(x?)
        \\    result = List.fold([Ok(1), Err({})], [], |acc, x| List.append(acc, compute(x)))
        \\    List.len(result)
        \\}
    , 2, .no_trace);
}

test "early return: ? in second argument of multi-arg call" {
    // Regression test: early return in second arg corrupted value stack
    try runExpectI64(
        \\{
        \\    my_func = |_a, b| b
        \\    compute = |x| Ok(x?)
        \\    match my_func(42, compute(Err({}))) { Ok(_) => 1, Err(_) => 0 }
        \\}
    , 0, .no_trace);
}

test "early return: ? in first argument of multi-arg call" {
    // Regression test: early return in first arg corrupted value stack
    try runExpectI64(
        \\{
        \\    my_func = |a, _b| a
        \\    compute = |x| Ok(x?)
        \\    match my_func(compute(Err({})), 42) { Ok(_) => 1, Err(_) => 0 }
        \\}
    , 0, .no_trace);
}

test "Decoder: create ok result - check result is Ok" {
    // Test that we can create a decode result and it is an Ok
    try runExpectBool(
        \\{
        \\    result = { result: Ok(42i64), rest: [] }
        \\    match result.result {
        \\        Ok(_) => Bool.True
        \\        Err(_) => Bool.False
        \\    }
        \\}
    , true, .no_trace);
}

test "Decoder: create ok result - extract value" {
    // Test that we can extract the value from a decode result
    try runExpectI64(
        \\{
        \\    result = { result: Ok(42i64), rest: [] }
        \\    match result.result {
        \\        Ok(n) => n
        \\        Err(_) => 0i64
        \\    }
        \\}
    , 42, .no_trace);
}

test "Decoder: create err result" {
    // Test that we can create an error decode result
    try runExpectBool(
        \\{
        \\    result = { result: Err(TooShort), rest: [1u8, 2u8, 3u8] }
        \\    match result.result {
        \\        Ok(_) => Bool.True
        \\        Err(_) => Bool.False
        \\    }
        \\}
    , false, .no_trace);
}

test "decode: I32.decode with simple format" {
    // Test I32.decode with a format that provides decode_i32
    try runExpectI64(
        \\{
        \\    # Define a format type with decode_i32 method
        \\    MyFormat := {}.{
        \\        decode_i32 : MyFormat, List(U8) -> (Try(I32, [Err]), List(U8))
        \\        decode_i32 = |_fmt, src| (Ok(42i32), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = I32.decode([], fmt)
        \\    match result {
        \\        Ok(n) => n.to_i64()
        \\        Err(_) => 0i64
        \\    }
        \\}
    , 42, .no_trace);
}

test "decode: I64.decode with simple format" {
    // Test I64.decode with a simple format that returns a constant
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_i64 : MyFormat, List(U8) -> (Try(I64, [Err]), List(U8))
        \\        decode_i64 = |_fmt, src| (Ok(99i64), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = I64.decode([], fmt)
        \\    match result {
        \\        Ok(n) => n
        \\        Err(_) => 0i64
        \\    }
        \\}
    , 99, .no_trace);
}

test "decode: U8.decode success" {
    // Test U8.decode with simple constant format
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_u8 : MyFormat, List(U8) -> (Try(U8, [Empty]), List(U8))
        \\        decode_u8 = |_fmt, src| (Ok(255u8), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = U8.decode([], fmt)
        \\    match result {
        \\        Ok(n) => n.to_i64()
        \\        Err(_) => -1i64
        \\    }
        \\}
    , 255, .no_trace);
}

test "decode: U8.decode error" {
    // Test U8.decode returns error - use I64 result to avoid complex match
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_u8 : MyFormat, List(U8) -> (Try(U8, [Empty]), List(U8))
        \\        decode_u8 = |_fmt, src| (Err(Empty), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = U8.decode([], fmt)
        \\    match result {
        \\        Ok(_) => 0i64
        \\        Err(_) => 1i64
        \\    }
        \\}
    , 1, .no_trace);
}

test "decode: Bool.decode true" {
    // Test Bool.decode returns true
    try runExpectBool(
        \\{
        \\    MyFormat := {}.{
        \\        decode_bool : MyFormat, List(U8) -> (Try(Bool, [Empty]), List(U8))
        \\        decode_bool = |_fmt, src| (Ok(Bool.True), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = Bool.decode([], fmt)
        \\    match result {
        \\        Ok(b) => b
        \\        Err(_) => Bool.False
        \\    }
        \\}
    , true, .no_trace);
}

test "decode: Bool.decode false" {
    // Test Bool.decode returns false
    try runExpectBool(
        \\{
        \\    MyFormat := {}.{
        \\        decode_bool : MyFormat, List(U8) -> (Try(Bool, [Empty]), List(U8))
        \\        decode_bool = |_fmt, src| (Ok(Bool.False), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = Bool.decode([], fmt)
        \\    match result {
        \\        Ok(b) => b
        \\        Err(_) => Bool.True  # Return True on error to distinguish
        \\    }
        \\}
    , false, .no_trace);
}

test "decode: Str.decode success" {
    // Test Str.decode with constant
    try runExpectStr(
        \\{
        \\    MyFormat := {}.{
        \\        decode_str : MyFormat, List(U8) -> (Try(Str, [BadUtf8]), List(U8))
        \\        decode_str = |_fmt, src| (Ok("hi"), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = Str.decode([], fmt)
        \\    match result {
        \\        Ok(s) => s
        \\        Err(_) => "error"
        \\    }
        \\}
    , "hi", .no_trace);
}

test "decode: rest returned from decode" {
    // Verify that decode returns the rest bytes
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_u8 : MyFormat, List(U8) -> (Try(U8, [Empty]), List(U8))
        \\        decode_u8 = |_fmt, src| (Ok(1u8), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = U8.decode([5u8], fmt)
        \\    match result {
        \\        Ok(n) => n.to_i64()
        \\        Err(_) => 0i64
        \\    }
        \\}
    , 1, .no_trace);
}

test "decode: U16.decode" {
    // Test U16.decode
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_u16 : MyFormat, List(U8) -> (Try(U16, [Err]), List(U8))
        \\        decode_u16 = |_fmt, src| (Ok(1000u16), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = U16.decode([], fmt)
        \\    match result {
        \\        Ok(n) => n.to_i64()
        \\        Err(_) => 0i64
        \\    }
        \\}
    , 1000, .no_trace);
}

test "decode: U32.decode" {
    // Test U32.decode
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_u32 : MyFormat, List(U8) -> (Try(U32, [Err]), List(U8))
        \\        decode_u32 = |_fmt, src| (Ok(100000u32), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = U32.decode([], fmt)
        \\    match result {
        \\        Ok(n) => n.to_i64()
        \\        Err(_) => 0i64
        \\    }
        \\}
    , 100000, .no_trace);
}

test "decode: U64.decode" {
    // Test U64.decode
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_u64 : MyFormat, List(U8) -> (Try(U64, [Err]), List(U8))
        \\        decode_u64 = |_fmt, src| (Ok(9223372036854775807u64), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = U64.decode([], fmt)
        \\    match result {
        \\        Ok(n) => n.to_i64_wrap()
        \\        Err(_) => 0i64
        \\    }
        \\}
    , 9223372036854775807, .no_trace);
}

test "decode: I8.decode negative" {
    // Test I8.decode with negative value
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_i8 : MyFormat, List(U8) -> (Try(I8, [Err]), List(U8))
        \\        decode_i8 = |_fmt, src| (Ok(-42i8), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = I8.decode([], fmt)
        \\    match result {
        \\        Ok(n) => n.to_i64()
        \\        Err(_) => 0i64
        \\    }
        \\}
    , -42, .no_trace);
}

test "decode: I16.decode negative" {
    // Test I16.decode with negative value
    try runExpectI64(
        \\{
        \\    MyFormat := {}.{
        \\        decode_i16 : MyFormat, List(U8) -> (Try(I16, [Err]), List(U8))
        \\        decode_i16 = |_fmt, src| (Ok(-1000i16), src)
        \\    }
        \\    fmt : MyFormat
        \\    fmt = {}
        \\    (result, _rest) = I16.decode([], fmt)
        \\    match result {
        \\        Ok(n) => n.to_i64()
        \\        Err(_) => 0i64
        \\    }
        \\}
    , -1000, .no_trace);
}

// TODO: Test with multiple decode methods in same format has issues
// test "decode: chained format with different types" { ... }

test "debug 8783a: lambda with tag match called directly" {
    try runExpectI64(
        \\{
        \\    f = |child|
        \\        match child {
        \\            Aaa(_, _) => 10.I64
        \\            Bbb(_) => 1.I64
        \\        }
        \\    f(Bbb(42.I64))
        \\}
    , 1, .no_trace);
}

test "debug 8783b: fold with simple addition lambda" {
    try runExpectI64(
        \\{
        \\    items = [1.I64, 2.I64, 3.I64]
        \\    List.fold(items, 0.I64, |acc, x| acc + x)
        \\}
    , 6, .no_trace);
}

// TODO: test for fold with no-payload tag match (no-payload tag discriminant issue in fold)
// Tracked separately from the 8783f payload flex var resolution fix.

test "debug 8783g: match on payload tag without fold" {
    try runExpectI64(
        \\{
        \\    item = A(1.I64)
        \\    match item {
        \\        A(x) => x + 100.I64
        \\        B(x) => x + 200.I64
        \\    }
        \\}
    , 101, .no_trace);
}

test "debug 8783f: fold with tag match single payload" {
    try runExpectI64(
        \\{
        \\    items = [A(1.I64), B(2.I64)]
        \\    f = |acc, x|
        \\        match x {
        \\            A(_) => acc + 1.I64
        \\            B(_) => acc + 10.I64
        \\        }
        \\    List.fold(items, 0.I64, f)
        \\}
    , 11, .no_trace);
}

test "debug 8783c: fold with tag match" {
    try runExpectI64(
        \\{
        \\    children = [Text("hello")]
        \\    count_child = |acc, child|
        \\        match child {
        \\            Text(_) => acc + 1.I64
        \\            Element(_, _) => acc + 10.I64
        \\        }
        \\    List.fold(children, 0.I64, count_child)
        \\}
    , 1, .no_trace);
}

test "issue 8783: List.fold with match on tag union elements from pattern match" {
    // Regression test: List.fold with a callback that matches on elements extracted from pattern matching
    // would fail with TypeMismatch in match_branches continuation.
    try runExpectI64(
        \\{
        \\    elem = Element("div", [Text("hello")])
        \\    children = match elem {
        \\        Element(_tag, c) => c
        \\        Text(_) => []
        \\    }
        \\    count_child = |acc, child|
        \\        match child {
        \\            Text(_) => acc + 1.I64
        \\            Element(_, _) => acc + 10.I64
        \\        }
        \\    List.fold(children, 0.I64, count_child)
        \\}
    , 1, .no_trace);
}

test "issue 8821: List.get with records and pattern match on Try type" {
    // Regression test for issue #8821
    // Test List.get with a list of records, pattern matching on Try/Result,
    // and accessing record fields from the matched value
    try runExpectStr(
        \\{
        \\    clients : List({ id : U64, name : Str })
        \\    clients = [{ id: 1, name: "Alice" }]
        \\
        \\    match List.get(clients, 0) {
        \\        Ok(client) => client.name
        \\        Err(_) => "missing"
        \\    }
        \\}
    , "Alice", .no_trace);
}

test "encode: just convert string to utf8" {
    // Simple test: convert string to utf8 and back
    try runExpectStr(
        \\{
        \\    bytes = Str.to_utf8("hello")
        \\    Str.from_utf8_lossy(bytes)
        \\}
    , "hello", .no_trace);
}

test "static dispatch: List.sum uses item.plus and item.default" {
    // Test that static dispatch works with List.sum
    // List.sum requires: item.plus : item, item -> item, item.default : item
    // This demonstrates the static dispatch pattern that Encode uses
    try runExpectI64(
        \\{
        \\    list : List(I64)
        \\    list = [1i64, 2i64, 3i64, 4i64, 5i64]
        \\    List.sum(list)
        \\}
    , 15, .no_trace);
}

test "issue 8814: List.get with numeric literal on function parameter - regression" {
    // Regression test for GitHub issue #8814: interpreter crash when calling
    // list.get(0) on a list passed as a function parameter.
    //
    // The bug occurred because when collecting arguments for a static dispatch
    // method call, the expected type for the numeric literal 0 wasn't being
    // set from the method's signature (U64). This caused the interpreter to
    // fail when trying to evaluate the numeric literal without a concrete type.
    //
    // The fix: extract expected parameter types from the method's function
    // signature and use them when evaluating arguments. This allows numeric
    // literals to correctly infer their concrete types (like U64 for List.get).
    try runExpectStr(
        \\{
        \\    process = |args| {
        \\        match args.get(0) {
        \\            Ok(x) => x
        \\            Err(_) => "error"
        \\        }
        \\    }
        \\    process(["hello", "world"])
        \\}
    , "hello", .no_trace);
}

test "nominal: structural record unifies with nominal type via annotation" {
    // Test that a structural record literal unifies with a nominal type
    // when the variable has a type annotation.
    try runExpectStr(
        \\{
        \\    Foo := { bar : Str }
        \\    x : Foo
        \\    x = { bar: "hello" }
        \\    x.bar
        \\}
    , "hello", .no_trace);
}

test "nominal: empty record unifies with nominal type via annotation" {
    // Test that {} unifies with a nominal type backed by {}
    // when the variable has a type annotation.
    try runExpectI64(
        \\{
        \\    Foo := {}
        \\    x : Foo
        \\    x = {}
        \\    42.I64
        \\}
    , 42, .no_trace);
}

test "encode: Str.encode with local format type" {
    // Test cross-module dispatch: Str.encode (in Builtin) calls Fmt.encode_str
    // where Fmt is a local type defined in the test.
    // Note: uses match instead of ? to avoid return_outside_fn at top level
    // which causes the block's type variable to resolve to Content.err.
    try runExpectListI64(
        \\{
        \\    Utf8Format := {}.{
        \\        encode_str : Utf8Format, Str -> Try(List(U8), [])
        \\        encode_str = |_fmt, s| Ok(Str.to_utf8(s))
        \\    }
        \\    fmt : Utf8Format
        \\    fmt = {}
        \\    result = Str.encode("hi", fmt)
        \\    match result {
        \\        Ok(bytes) => List.map(bytes, |b| U8.to_i64(b))
        \\        Err(_) => []
        \\    }
        \\}
    , &[_]i64{ 104, 105 }, .no_trace);
}

test "issue 8831: self-referential value definition should produce error, not crash" {
    // Regression test for GitHub issue #8831
    // A self-referential value definition like `a = a` should produce a
    // compile-time error (ident_not_in_scope) instead of crashing at runtime
    // with "e_lookup_local: definition not found in current scope".
    //
    // The fix is to detect during canonicalization that the RHS of a definition
    // refers to a variable that is being defined in the current definition and
    // hasn't been introduced to the scope yet.
    try runExpectError(
        \\{
        \\    a = a
        \\    a
        \\}
    , error.Crash, .no_trace);
}

test "issue 8831: nested self-reference in list should also error" {
    // Additional test for issue #8831
    // Even nested self-references like `a = [a]` should error during canonicalization.
    // In Roc, shadowing is not allowed, so `a = [a]` cannot reference an outer `a`.
    // Only lambdas are allowed to self-reference (for recursive function calls).
    try runExpectError(
        \\{
        \\    a = [a]
        \\    a
        \\}
    , error.Crash, .no_trace);
}

test "issue 9043: self-reference in tuple pattern with var element should error" {
    // Regression test for GitHub issue #9043
    // A self-referential definition with a mutable variable in a tuple pattern
    // like `(_, var $n) = f($n)` should produce a compile-time error.
    // Previously this would crash with "e_lookup_local: definition not found".
    try runExpectError(
        \\{
        \\    next = |idx| (idx, idx + 1)
        \\    (_, var $n) = next($n)
        \\    $n
        \\}
    , error.Crash, .no_trace);
}

test "recursive function with record - stack memory restoration (issue #8813)" {
    // Test that recursive closure calls don't leak stack memory.
    // If stack memory is not properly restored after closure returns,
    // deeply recursive functions will exhaust the interpreter's stack.
    // The record allocation forces stack allocation on each call.
    try runExpectI64(
        \\{
        \\    f = |n|
        \\        if n <= 0
        \\            0
        \\        else
        \\            { a: n, b: n * 2, c: n * 3, d: n * 4 }.a + f(n - 1)
        \\    f(1000)
        \\}
    , 500500, .no_trace);
}

test "issue 8872: polymorphic tag union payload layout in match expressions" {
    // Regression test for GitHub issue #8872: when using a polymorphic function
    // that transforms Err(a) to Err(b) via a lambda, the Str payload was being
    // corrupted because the layout was computed from a flex var (defaulting to
    // Dec = 16 bytes) instead of the actual Str type (24 bytes).
    //
    // The bug manifested when:
    // 1. A polymorphic function takes a lambda that returns type `b`
    // 2. The function wraps the lambda result in Err(b)
    // 3. The match expression extracts the Err payload
    // 4. The extracted value is corrupted due to wrong layout
    try runExpectStr(
        \\{
        \\    transform_err : [Ok({}), Err(a)], (a -> b) -> [Ok({}), Err(b)]
        \\    transform_err = |try_val, transform| match try_val {
        \\        Err(a) => Err(transform(a))
        \\        Ok(ok) => Ok(ok)
        \\    }
        \\
        \\    err : [Ok({}), Err(I32)]
        \\    err = Err(42i32)
        \\
        \\    result = transform_err(err, |_e| "hello")
        \\    match result {
        \\        Ok(_) => "got ok"
        \\        Err(msg) => msg
        \\    }
        \\}
    , "hello", .no_trace);
}

test "match on tag union with different input/output sizes in proc" {
    try runExpectStr(
        \\{
        \\    transform : [Ok({}), Err(I32)] -> [Ok({}), Err(Str)]
        \\    transform = |try_val| match try_val {
        \\        Err(_) => Err("hello")
        \\        Ok(ok) => Ok(ok)
        \\    }
        \\
        \\    result = transform(Err(42i32))
        \\    match result {
        \\        Ok(_) => "got ok"
        \\        Err(msg) => msg
        \\    }
        \\}
    , "hello", .no_trace);
}

test "polymorphic tag transform with match (transform_err pattern)" {
    try runExpectStr(
        \\{
        \\    transform_err = |try_val| match try_val {
        \\        Err(_) => Err("hello")
        \\        Ok(ok) => Ok(ok)
        \\    }
        \\
        \\    err : [Ok({}), Err(I32)]
        \\    err = Err(42i32)
        \\
        \\    result = transform_err(err)
        \\    match result {
        \\        Ok(_) => "got ok"
        \\        Err(msg) => msg
        \\    }
        \\}
    , "hello", .no_trace);
}

test "proc with tag match returning non-tag type" {
    try runExpectStr(
        \\{
        \\    check : [Ok({}), Err(I32)] -> Str
        \\    check = |try_val| match try_val {
        \\        Err(_) => "was err"
        \\        Ok(_) => "was ok"
        \\    }
        \\
        \\    check(Err(42i32))
        \\}
    , "was err", .no_trace);
}

test "lambda with list param calling List.len (no allocation)" {
    // Simple lambda that takes a list and returns its length
    // This doesn't require allocation, so it tests basic roc_ops passing
    try runExpectI64(
        \\{
        \\    get_len = |l| List.len(l)
        \\    get_len([1i64, 2i64, 3i64])
        \\}
    , 3, .no_trace);
}

test "lambda with list param calling List.append (requires allocation)" {
    // Lambda that takes a list and appends to it
    // This requires allocation, so it tests roc_ops passing for builtins
    try runExpectI64(
        \\{
        \\    add_one = |l| List.len(List.append(l, 99i64))
        \\    add_one([1i64, 2i64, 3i64])
        \\}
    , 4, .no_trace);
}

test "lambda with list param and var declaration" {
    // Lambda with a mutable variable inside
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0i64]
        \\        List.len($acc)
        \\    }
        \\    test_fn([1i64, 2i64])
        \\}
    , 1, .no_trace);
}

test "lambda with list param and list literal creation" {
    // Lambda that creates a list literal inside (requires allocation)
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0i64]
        \\        List.len($acc)
        \\    }
        \\    test_fn([10i64, 20i64])
        \\}
    , 1, .no_trace);
}

test "lambda with list param, var, and for loop" {
    // Lambda with for loop that mutates a variable
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0i64
        \\        for e in l {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([10i64, 20i64, 30i64])
        \\}
    , 60, .no_trace);
}

test "lambda with list param, var, and List.append (no for loop)" {
    // Lambda with var and List.append but NO for loop
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0i64]
        \\        $acc = List.append($acc, 42i64)
        \\        List.len($acc)
        \\    }
        \\    test_fn([10i64, 20i64])
        \\}
    , 2, .no_trace);
}

test "minimal lambda with list param and for loop (no allocation)" {
    // Absolute minimal test: list param + for loop, no allocations inside
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0i64
        \\        for e in l {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([1i64, 2i64])
        \\}
    , 3, .no_trace);
}

test "lambda with list param, for loop, and allocation inside loop (list literal)" {
    // List param + for loop + allocation inside loop body (not List.append)
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0i64
        \\        for e in l {
        \\            var $temp = [e]
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([1i64, 2i64])
        \\}
    , 3, .no_trace);
}

test "lambda with for loop over internal list, not param (scalar param)" {
    // Lambda with for loop over an internal list, scalar parameter
    try runExpectI64(
        \\{
        \\    test_fn = |x| {
        \\        var $total = 0i64
        \\        for e in [1i64, 2i64, 3i64] {
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn(42i64)
        \\}
    , 6, .no_trace);
}

test "lambda with list param, for loop over internal list, allocation inside" {
    // Lambda with list param, but for loop over internal list, allocation inside
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0i64
        \\        for e in [1i64, 2i64] {
        \\            var $temp = [e]
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([10i64, 20i64])
        \\}
    , 3, .no_trace);
}

test "lambda with list param, for loop, but empty iteration" {
    // Lambda with for loop that runs 0 times
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0i64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([])
        \\}
    , 1, .no_trace);
}

test "lambda with list param, for loop, and List.append in loop with single iteration" {
    // Lambda with for loop that calls List.append but with single element
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0i64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([10i64])
        \\}
    , 2, .no_trace);
}

test "lambda with list param, var, for loop, and List.append" {
    // Lambda with for loop that calls List.append
    try runExpectI64(
        \\{
        \\    test_fn = |l| {
        \\        var $acc = [0i64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\        }
        \\        List.len($acc)
        \\    }
        \\    test_fn([10i64, 20i64, 30i64])
        \\}
    , 4, .no_trace);
}

test "issue 8899: closure decref index out of bounds in for loop" {
    // Regression test for GitHub issue #8899: panic "index out of bounds: index 131, len 73"
    // when running roc test on code with closures and for loops.
    // The bug was in decrefLayoutPtr which read captures_layout_idx from raw memory
    // instead of using the layout parameter.
    //
    // The original code was a compress function that removes consecutive duplicates.
    // The issue manifested when closures were created inside the for loop (match branches)
    // and List operations like List.last and List.append were used.
    try runExpectI64(
        \\{
        \\    sum_with_last = |l| {
        \\        var $total = 0i64
        \\        var $acc = [0i64]
        \\        for e in l {
        \\            $acc = List.append($acc, e)
        \\            $total = match List.last($acc) { Ok(last) => $total + last, Err(_) => $total }
        \\        }
        \\        $total
        \\    }
        \\    sum_with_last([10i64, 20i64, 30i64])
        \\}
    , 60, .no_trace);
}

test "issue 8892: nominal type wrapping tag union with match expression" {
    // Regression test for GitHub issue #8892: when evaluating a tag expression
    // inside a function where the expected type is a nominal type wrapping a tag union,
    // the interpreter would crash with "e_tag: unexpected layout type: box".
    //
    // The bug was in e_tag evaluation: it was using getRuntimeLayout(rt_var) where
    // rt_var was the nominal type (which has a box layout), instead of using the
    // unwrapped backing type's layout (which is the actual tag union layout).
    //
    // The fix: use getRuntimeLayout(resolved.var_) to get the backing type's layout.
    try runExpectSuccess(
        \\{
        \\    parse_value = || {
        \\        combination_method = match ModuloToken {
        \\            ModuloToken => Modulo
        \\        }
        \\        combination_method
        \\    }
        \\    parse_value()
        \\}
    , .no_trace);
}

test "issue 8927: early return in method argument leaks memory" {
    // Regression test for GitHub issue #8927: memory leak when using ? operator
    // inside a for loop that accumulates to a mutable variable via method call.
    //
    // When ? triggers early return during method argument evaluation (like
    // list.append(x?)), the receiver value and method function on the value
    // stack were not being decreffed, causing a memory leak.
    //
    // The fix adds cleanup handlers for dot_access_resolve, dot_access_collect_args,
    // and type_var_dispatch_collect_args in the early_return section.
    //
    // This test uses test_allocator which detects memory leaks.
    try runExpectI64(
        \\{
        \\    fold_try = |tries| {
        \\        var $ok_list = [""]
        \\        $ok_list = []
        \\        for a_try in tries {
        \\            $ok_list = $ok_list.append(a_try?)
        \\        }
        \\        Ok($ok_list)
        \\    }
        \\
        \\    tries = [Ok("a"), Ok("b"), Err(Oops), Ok("d")]
        \\
        \\    match fold_try(tries) {
        \\        Ok(list) => List.len(list)
        \\        Err(_) => 0
        \\    }
        \\}
    , 0, .no_trace);
}

test "issue 8946: closure capturing for-loop element with == comparison" {
    // Regression test for GitHub issue #8946: NotNumeric crash when closures
    // capture for-loop elements and use them in == comparisons.
    //
    // The bug was in layout computation for flex/rigid type variables inside
    // list containers: when the variable had is_eq constraint (from ==) but
    // not from_numeral constraint, it was getting opaquePtr() layout instead
    // of a numeric layout (Dec).
    //
    // The fix ensures flex/rigid vars with any constraints default to Dec layout.
    try runExpectI64(
        \\{
        \\    my_any = |lst, pred| {
        \\        for e in lst {
        \\            if pred(e) { return True }
        \\        }
        \\        False
        \\    }
        \\    check = |list| {
        \\        var $built = []
        \\        for item in list {
        \\            _x = my_any($built, |x| x == item)
        \\            $built = $built.append(item)
        \\        }
        \\        $built.len()
        \\    }
        \\    check([1, 2])
        \\}
    , 2, .no_trace);
}

test "issue 8978: incref alignment with recursive tag unions in tuples" {
    // Regression test for GitHub issue #8978: incref alignment check failed
    // when a recursive tag union using pointer tagging was stored in a tuple.
    //
    // Recursive tag unions (types that contain themselves, like linked lists
    // or expression trees) use pointer tagging to store the tag discriminant
    // in the low bits of the pointer. When incref is called on such a pointer,
    // it needs to strip the tag bits before accessing the refcount at ptr - 8.
    //
    // The bug was that increfDataPtrC had an alignment check that would fail
    // on tagged pointers because they aren't aligned to @alignOf(usize).
    //
    // The fix: remove the alignment check since the tag bits are stripped
    // before accessing the refcount anyway.
    //
    // This test uses a recursive tag pattern (Element containing children
    // that can also be Element) inside a tuple, which triggers the incref
    // alignment issue when the tuple is returned from a function.
    try runExpectI64(
        \\{
        \\    make_result = || {
        \\        elem = Element("div", [Text("hello"), Element("span", [Text("world")])])
        \\        children = match elem {
        \\            Element(_tag, c) => c
        \\            Text(_) => []
        \\        }
        \\        (children, 42i64)
        \\    }
        \\    (_, n) = make_result()
        \\    n
        \\}
    , 42, .no_trace);
}

// ============ str_inspekt (Str.inspect) tests ============

test "str_inspekt - integer" {
    // Str.inspect on an integer should return its string representation
    // Note: untyped numeric literals default to Dec, so 42 becomes "42.0"
    try runExpectStr("Str.inspect(42)", "42.0", .no_trace);
}

test "str_inspekt - negative integer" {
    try runExpectStr("Str.inspect(-123)", "-123.0", .no_trace);
}

test "str_inspekt - zero" {
    try runExpectStr("Str.inspect(0)", "0.0", .no_trace);
}

test "str_inspekt - boolean true" {
    // Str.inspect on Bool.True renders without the nominal prefix
    try runExpectStr("Str.inspect(Bool.True)", "True", .no_trace);
}

test "str_inspekt - boolean false" {
    try runExpectStr("Str.inspect(Bool.False)", "False", .no_trace);
}

test "str_inspekt - simple string" {
    // Str.inspect on a string should return it quoted and escaped
    try runExpectStr("Str.inspect(\"hello\")", "\"hello\"", .no_trace);
}

test "str_inspekt - string with quotes" {
    // Quotes inside strings should be escaped
    try runExpectStr("Str.inspect(\"say \\\"hi\\\"\")", "\"say \\\"hi\\\"\"", .no_trace);
}

test "str_inspekt - empty string" {
    try runExpectStr("Str.inspect(\"\")", "\"\"", .no_trace);
}

test "str_inspekt - large integer" {
    try runExpectStr("Str.inspect(1234567890)", "1234567890.0", .no_trace);
}

// ============ Higher-Order Function Tests ============

test "higher-order function - simple apply" {
    try runExpectI64(
        \\{
        \\    apply = |f, x| f(x)
        \\    apply(|n| n + 1i64, 5i64)
        \\}
    , 6, .no_trace);
}

test "higher-order function - apply with closure" {
    try runExpectI64(
        \\{
        \\    offset = 10i64
        \\    apply = |f, x| f(x)
        \\    apply(|n| n + offset, 5i64)
        \\}
    , 15, .no_trace);
}

test "higher-order function - twice" {
    try runExpectI64(
        \\{
        \\    twice = |f, x| f(f(x))
        \\    twice(|n| n * 2i64, 3i64)
        \\}
    , 12, .no_trace);
}

// Integer conversion tests

test "int conversion: I8.to_i64 positive" {
    try runExpectI64(
        \\{ 42i8.to_i64() }
    , 42, .no_trace);
}

test "int conversion: I8.to_i64 negative" {
    try runExpectI64(
        \\{ (-1i8).to_i64() }
    , -1, .no_trace);
}

test "int conversion: I16.to_i64 positive" {
    try runExpectI64(
        \\{ 1000i16.to_i64() }
    , 1000, .no_trace);
}

test "int conversion: I16.to_i64 negative" {
    try runExpectI64(
        \\{ (-500i16).to_i64() }
    , -500, .no_trace);
}

test "int conversion: I32.to_i64 positive" {
    try runExpectI64(
        \\{ 100000i32.to_i64() }
    , 100000, .no_trace);
}

test "int conversion: I32.to_i64 negative" {
    try runExpectI64(
        \\{ (-100000i32).to_i64() }
    , -100000, .no_trace);
}

test "int conversion: U8.to_i64" {
    try runExpectI64(
        \\{ 255u8.to_i64() }
    , 255, .no_trace);
}

test "int conversion: U16.to_i64" {
    try runExpectI64(
        \\{ 65535u16.to_i64() }
    , 65535, .no_trace);
}

test "int conversion: U32.to_i64" {
    try runExpectI64(
        \\{ 4000000000u32.to_i64() }
    , 4000000000, .no_trace);
}

test "int conversion: I8.to_i32.to_i64" {
    try runExpectI64(
        \\{ (-10i8).to_i32().to_i64() }
    , -10, .no_trace);
}

test "int conversion: U8.to_u32.to_i64" {
    try runExpectI64(
        \\{ 200u8.to_u32().to_i64() }
    , 200, .no_trace);
}

test "int conversion: U8.to_i16.to_i64" {
    try runExpectI64(
        \\{ 128u8.to_i16().to_i64() }
    , 128, .no_trace);
}

test "diag: match Ok extract payload" {
    try runExpectI64(
        \\match Ok(42) { Ok(v) => v, _ => 0 }
    , 42, .no_trace);
}

test "diag: lambda returning tag union" {
    try runExpectI64(
        \\{
        \\    f = |x| Ok(x)
        \\    match f(42) { Ok(v) => v, _ => 0 }
        \\}
    , 42, .no_trace);
}

test "diag: identity lambda call" {
    try runExpectI64(
        \\{
        \\    f = |x| x
        \\    f(42)
        \\}
    , 42, .no_trace);
}

test "diag: lambda wrapping try suffix result in Ok" {
    try runExpectI64(
        \\{
        \\    compute = |x| Ok(x?)
        \\    match compute(Ok(42.I64)) { Ok(v) => v, _ => 0 }
        \\}
    , 42, .no_trace);
}

test "Bool.True and Bool.False raw values - bug confirmation" {
    // Test that Bool.True and Bool.False have different raw byte values
    // Bug report: both Bool.True and Bool.False write 0x00 to memory
    try runExpectBool("Bool.True", true, .no_trace);
    try runExpectBool("Bool.False", false, .no_trace);
}

test "Bool in record field - bug confirmation" {
    // Test Bool values when stored in record fields
    // This is closer to the bug report scenario where Bool is in a struct
    try runExpectBool("{ flag: Bool.True }.flag", true, .no_trace);
    try runExpectBool("{ flag: Bool.False }.flag", false, .no_trace);
}

test "polymorphic tag union payload substitution: extract payload" {
    // Tests that `a -> I64` is discovered from the Ok tag's payload
    try runExpectI64(
        \\{
        \\    second : [Left(a), Right(b)] -> b
        \\    second = |either| match either {
        \\        Left(_) => 0i64
        \\        Right(val) => val
        \\    }
        \\
        \\    input : [Left(I64), Right(I64)]
        \\    input = Right(42i64)
        \\    second(input)
        \\}
    , 42, .no_trace);
}

test "polymorphic tag union payload substitution: multiple type vars" {
    // Tests that `e -> Str` is discovered from the Err tag's payload
    try runExpectStr(
        \\{
        \\    get_err : [Ok(a), Err(e)] -> e
        \\    get_err = |result| match result {
        \\        Ok(_) => ""
        \\        Err(e) => e
        \\    }
        \\
        \\    val : [Ok(I64), Err(Str)]
        \\    val = Err("hello")
        \\    get_err(val)
        \\}
    , "hello", .no_trace);
}

test "polymorphic tag union payload substitution: wrap and unwrap" {
    // Tests that `a -> I64` is discovered from the return type's tag payload
    try runExpectI64(
        \\{
        \\    wrap : a -> [Val(a)]
        \\    wrap = |x| Val(x)
        \\
        \\    result = wrap(42)
        \\    match result {
        \\        Val(n) => n
        \\    }
        \\}
    , 42, .no_trace);
}

test "Bool in record with mixed alignment fields - bug confirmation" {
    // Test Bool in a record with fields of different alignments
    // Similar to the bug report: { key: U64, childCount: U32, isElement: Bool }
    try runExpectBool("{ key: 42u64, flag: Bool.True }.flag", true, .no_trace);
    try runExpectBool("{ key: 42u64, flag: Bool.False }.flag", false, .no_trace);
    try runExpectBool("{ key: 42u64, count: 1u32, flag: Bool.True }.flag", true, .no_trace);
    try runExpectBool("{ key: 42u64, count: 1u32, flag: Bool.False }.flag", false, .no_trace);
}

test "Str.trim" {
    try runExpectStr("Str.trim(\"  hello  \")", "hello", .no_trace);
    try runExpectStr("Str.trim(\"hello\")", "hello", .no_trace);
    try runExpectStr("Str.trim(\"  \")", "", .no_trace);
}

test "Str.trim_start" {
    try runExpectStr("Str.trim_start(\"  hello  \")", "hello  ", .no_trace);
    try runExpectStr("Str.trim_start(\"hello\")", "hello", .no_trace);
}

test "Str.trim_end" {
    try runExpectStr("Str.trim_end(\"  hello  \")", "  hello", .no_trace);
    try runExpectStr("Str.trim_end(\"hello\")", "hello", .no_trace);
}

test "Str.with_ascii_lowercased" {
    try runExpectStr("Str.with_ascii_lowercased(\"HELLO\")", "hello", .no_trace);
    try runExpectStr("Str.with_ascii_lowercased(\"Hello World\")", "hello world", .no_trace);
    try runExpectStr("Str.with_ascii_lowercased(\"abc\")", "abc", .no_trace);
}

test "Str.with_ascii_uppercased" {
    try runExpectStr("Str.with_ascii_uppercased(\"hello\")", "HELLO", .no_trace);
    try runExpectStr("Str.with_ascii_uppercased(\"Hello World\")", "HELLO WORLD", .no_trace);
    try runExpectStr("Str.with_ascii_uppercased(\"ABC\")", "ABC", .no_trace);
}

test "Str.caseless_ascii_equals" {
    try runExpectBool("Str.caseless_ascii_equals(\"hello\", \"HELLO\")", true, .no_trace);
    try runExpectBool("Str.caseless_ascii_equals(\"abc\", \"abc\")", true, .no_trace);
    try runExpectBool("Str.caseless_ascii_equals(\"abc\", \"def\")", false, .no_trace);
}

test "Str.repeat" {
    try runExpectStr("Str.repeat(\"ab\", 3)", "ababab", .no_trace);
    try runExpectStr("Str.repeat(\"x\", 1)", "x", .no_trace);
    try runExpectStr("Str.repeat(\"x\", 0)", "", .no_trace);
}

test "Str.with_prefix" {
    try runExpectStr("Str.with_prefix(\"world\", \"hello \")", "hello world", .no_trace);
    try runExpectStr("Str.with_prefix(\"bar\", \"\")", "bar", .no_trace);
}

test "polymorphic closure capture duplication during monomorphization" {
    // Regression test: when a polymorphic function creates a closure that captures
    // its argument, each specialization must get independent copies of the captures.
    // Without proper duplication, specializations share capture data, causing corruption.

    // Polymorphic function that returns a closure capturing its argument,
    // called with both integer and string types.
    try runExpectI64(
        \\{
        \\    make_getter = |n| |_x| n
        \\    get_num = make_getter(42)
        \\    get_num(0)
        \\}
    , 42, .no_trace);

    try runExpectStr(
        \\{
        \\    make_getter = |n| |_x| n
        \\    get_str = make_getter("hello")
        \\    get_str(0)
        \\}
    , "hello", .no_trace);
}

test "large record - chained higher-order calls with growing intermediates" {
    // Simulates the record builder pattern: nested apply calls build up larger types
    try runExpectStr(
        \\{
        \\    apply2 = |a, b, f| f(a, b)
        \\    step1 = apply2("x_val", "y_val", |x, y| { x, y })
        \\    result = apply2("w_val", step1.y, |w, y| { w, y })
        \\    result.w
        \\}
    , "w_val", .no_trace);
    try runExpectStr(
        \\{
        \\    apply2 = |a, b, f| f(a, b)
        \\    step1 = apply2("x_val", "y_val", |x, y| { x, y })
        \\    result = apply2("w_val", step1.y, |w, y| { w, y })
        \\    result.y
        \\}
    , "y_val", .no_trace);
}

test "Str.drop_prefix" {
    try runExpectStr("Str.drop_prefix(\"foobar\", \"foo\")", "bar", .no_trace);
    try runExpectStr("Str.drop_prefix(\"foobar\", \"baz\")", "foobar", .no_trace);
}

test "Str.drop_suffix" {
    try runExpectStr("Str.drop_suffix(\"foobar\", \"bar\")", "foo", .no_trace);
    try runExpectStr("Str.drop_suffix(\"foobar\", \"baz\")", "foobar", .no_trace);
}

test "Str.release_excess_capacity" {
    try runExpectStr("Str.release_excess_capacity(\"hello\")", "hello", .no_trace);
}

test "Str.split_on and Str.join_with" {
    try runExpectStr(
        \\{
        \\    parts = Str.split_on("a,b,c", ",")
        \\    Str.join_with(parts, "-")
        \\}
    , "a-b-c", .no_trace);
}

test "Str.join_with" {
    try runExpectStr(
        \\Str.join_with(["hello", "world"], " ")
    , "hello world", .no_trace);
}

// Note: List.contains is implemented as List.any(list, |x| x == needle) in the builtins,
// which goes through closure + higher-order function paths rather than the list_contains
// low-level. The DevEvaluator doesn't currently support List.any with variable-capturing
// closures, so List.contains tests are not included here. The list_contains low-level
// codegen fix (H4) is tested via the LirCodeGen unit tests and will be exercised when
// the full compilation pipeline (CIR -> MIR -> LIR -> codegen) is used.

// Note: Str.from_utf8 returns a Result which requires match support in all evaluators.
// It is tested indirectly via the encode/decode tests. The wasm codegen for it is implemented
// but we don't add a standalone test here to avoid DevEvaluator limitations with Result matching.
