//! Tests for the expression evaluator
const std = @import("std");
const helpers = @import("helpers.zig");
const eval = @import("../interpreter.zig");
const stack = @import("../stack.zig");
const layout_store = @import("../../layout/store.zig");

const EvalError = eval.EvalError;
const runExpectInt = helpers.runExpectInt;
const runExpectError = helpers.runExpectError;

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
    try runExpectInt("if True (if True 10 else 20) else 30", 10, .no_trace);
    try runExpectInt("if True (if False 10 else 20) else 30", 20, .no_trace);
    try runExpectInt("if False (if True 10 else 20) else 30", 30, .no_trace);
    try runExpectInt("if False 99 else (if True 40 else 50)", 40, .no_trace);
    try runExpectInt("if False 99 else (if False 40 else 50)", 50, .no_trace);
}

test "chained if-else" {
    try runExpectInt("if True 10 else if True 20 else 30", 10, .no_trace);
    try runExpectInt("if False 10 else if True 20 else 30", 20, .no_trace);
    try runExpectInt("if False 10 else if False 20 else 30", 30, .no_trace);
}

test "if-else arithmetic" {
    try runExpectInt("if True (1 + 2) else (3 + 4)", 3, .no_trace);
    try runExpectInt("if False (1 + 2) else (3 + 4)", 7, .no_trace);
    try runExpectInt("if True (10 * 5) else (20 / 4)", 50, .no_trace);
    try runExpectInt("if (2 > 1) (100 - 50) else (200 - 100)", 50, .no_trace);
}

test "eval if expression with non-boolean condition" {
    // TypeContainedMismatch error because condition must be a boolean tag union
    try runExpectError("if 42 1 else 0", EvalError.TypeContainedMismatch, .no_trace);
}

test "list literal" {
    // List literals are not yet implemented
    try runExpectError("[1, 2, 3]", EvalError.LayoutError, .no_trace);
}
test "record literal" {
    // Empty record literal is a zero-sized type
    try runExpectError("{}", EvalError.ZeroSizedType, .no_trace);

    // Record with integer fields
    const expected_fields1 = &[_]helpers.ExpectedField{
        .{ .name = "x", .value = 10 },
        .{ .name = "y", .value = 20 },
    };
    try helpers.runExpectRecord("{ x: 10, y: 20 }", expected_fields1, .no_trace);

    // Record with a single field
    const expected_fields2 = &[_]helpers.ExpectedField{
        .{ .name = "a", .value = 42 },
    };
    try helpers.runExpectRecord("{ a: 42 }", expected_fields2, .no_trace);

    // Record with fields in a different order
    const expected_fields3 = &[_]helpers.ExpectedField{
        .{ .name = "x", .value = 1 },
        .{ .name = "y", .value = 2 },
    };
    try helpers.runExpectRecord("{ y: 2, x: 1 }", expected_fields3, .no_trace);

    // Record with field values from arithmetic expressions and lambdas
    const expected_fields4 = &[_]helpers.ExpectedField{
        .{ .name = "sum", .value = 6 },
        .{ .name = "product", .value = 15 },
    };
    try helpers.runExpectRecord("{ sum: (|x| x + 1)(5), product: 5 * 3 }", expected_fields4, .no_trace);

    // TODO: Add support for non-integer fields in tests
    // Record with a string field
    // const expected_fields5 = &[_]helpers.ExpectedField{
    //     .{ .name = "name", .value = "roc" },
    // };
    // try helpers.runExpectRecord("{ name: \"roc\" }", expected_fields5, .no_trace);

    // TODO: Add support for nested records in tests
    // Record with a nested record
    // const expected_fields6 = &[_]helpers.ExpectedField{
    //     .{ .name = "p", .value = ... },
    // };
    // try helpers.runExpectRecord("{ p: { x: 1, y: 2 } }", expected_fields6, .no_trace);
}

test "record destructure patterns" {
    try helpers.runExpectInt("(|x| x)(42)", 42, .no_trace);
    // try helpers.runExpectInt("(|{ x }| x )({ x: -10 })", -10, .no_trace);
    try helpers.runExpectInt("(|{ x, y }| x * y)({ x: 10, y: 20 })", 200, .no_trace);
    try helpers.runExpectInt("(|{ x, y, z }| x * y * z)({ x: 10, y: 20, z: 30 })", 6000, .no_trace);
}

test "tuple destructure patterns" {
    try helpers.runExpectInt("(|(x,y)| x * y )((10,2))", 20, .no_trace);
    try helpers.runExpectInt("(|(x,(y,z))| x * y * z )((10,(20,30)))", 6000, .no_trace);
}

test "mixed destructure patterns" {
    try helpers.runExpectInt("(|{ a, x: (b, c), y: { d, e } }| a + b + c + d + e)({ a: 1, x: (2, 3), y: { d: 4, e: 5 } })", 15, .no_trace);
}

test "tuple literal" {
    // Tuple with integer elements
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
    try runExpectInt("(|y, z| (|x, w| (|a| a + w + x + y + z)(5))(2, 4))(1, 3)", 15, .no_trace);

    try runExpectInt(
        \\(((|a| {
        \\    a_loc = a * 2;
        \\    |b| {
        \\        b_loc = a_loc + b;
        \\        |c| b_loc + c
        \\    }
        \\})(100))(20))(3)
    , 223, .trace);
}

// Helper function to test that evaluation succeeds without checking specific values
fn runExpectSuccess(src: []const u8, should_trace: enum { trace, no_trace }) !void {
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
    defer interpreter.deinit();

    if (should_trace == .trace) {
        interpreter.startTrace(std.io.getStdErr().writer().any());
    }

    const result = interpreter.eval(resources.expr_idx);

    if (should_trace == .trace) {
        interpreter.endTrace();
    }

    // Just verify that evaluation succeeded
    _ = try result;
}

test "integer type evaluation" {
    // Test integer types to verify basic evaluation works
    // This should help us debug why 255u8 shows as 42 in REPL
    try runExpectInt("255u8", 255, .trace);
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
