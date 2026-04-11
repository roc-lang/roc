//! End-to-end tests for emitting monomorphic Roc code
//!
//! These tests verify that we can:
//! 1. Parse Roc source code
//! 2. Canonicalize it
//! 3. Type check it
//! 4. Emit it as valid Roc source code using the RocEmitter
//!
//! This is the foundation for the monomorphization pipeline testing.

const std = @import("std");
const can = @import("can");
const helpers = @import("helpers.zig");

const Emitter = can.RocEmitter;

const testing = std.testing;
// Use interpreter_allocator for interpreter tests (doesn't track leaks)
const test_allocator = helpers.interpreter_allocator;

/// Helper to parse, canonicalize, type check, and emit Roc code
fn emitFromSource(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    const resources = try helpers.parseAndCanonicalizeExpr(allocator, source);
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    var emitter = Emitter.init(allocator, resources.module_env);
    defer emitter.deinit();

    try emitter.emitExpr(resources.expr_idx);

    // Return a copy of the output since emitter will be deinitialized
    return try allocator.dupe(u8, emitter.getOutput());
}

test "end-to-end: emit integer literal" {
    const output = try emitFromSource(test_allocator, "42");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("42", output);
}

test "end-to-end: emit arithmetic expression" {
    const output = try emitFromSource(test_allocator, "1 + 2");
    defer test_allocator.free(output);

    // After parsing, the expression becomes a binop (no parens needed)
    try testing.expectEqualStrings("1 + 2", output);
}

test "end-to-end: emit True tag" {
    const output = try emitFromSource(test_allocator, "True");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("True", output);
}

test "end-to-end: emit False tag" {
    const output = try emitFromSource(test_allocator, "False");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("False", output);
}

test "end-to-end: emit empty list" {
    const output = try emitFromSource(test_allocator, "[]");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("[]", output);
}

test "end-to-end: emit list with elements" {
    const output = try emitFromSource(test_allocator, "[1, 2, 3]");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("[1, 2, 3]", output);
}

test "end-to-end: emit empty record" {
    const output = try emitFromSource(test_allocator, "{}");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("{}", output);
}

test "end-to-end: emit identity lambda" {
    const output = try emitFromSource(test_allocator, "|x| x");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("|x| x", output);
}

test "end-to-end: emit lambda with body" {
    const output = try emitFromSource(test_allocator, "|x| x + 1");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("|x| x + 1", output);
}

test "end-to-end: emit if expression" {
    const output = try emitFromSource(test_allocator, "if True 1 else 2");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("if (True) 1 else 2", output);
}

test "end-to-end: emit tuple" {
    const output = try emitFromSource(test_allocator, "(1, 2)");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("(1, 2)", output);
}

test "end-to-end: emit block with let binding" {
    const source =
        \\{
        \\    x = 42
        \\    x
        \\}
    ;
    const output = try emitFromSource(test_allocator, source);
    defer test_allocator.free(output);

    // The emitter will output the block structure
    try testing.expect(std.mem.indexOf(u8, output, "x = 42") != null);
    try testing.expect(std.mem.indexOf(u8, output, "x") != null);
}

// Emitter tests

test "emitter: identity function is polymorphic before type checking" {
    // This test parses an identity lambda and checks it can be emitted
    const output = try emitFromSource(test_allocator, "|x| x");
    defer test_allocator.free(output);

    // The identity function emits as expected
    try testing.expectEqualStrings("|x| x", output);
}

test "emitter: can emit identity function applied to integer" {
    // Test that we can parse and emit a block with identity function application
    const source =
        \\{
        \\    identity = |x| x
        \\    identity(42)
        \\}
    ;
    const output = try emitFromSource(test_allocator, source);
    defer test_allocator.free(output);

    // Verify the output contains the identity function and application
    try testing.expect(std.mem.indexOf(u8, output, "identity = |x| x") != null);
    try testing.expect(std.mem.indexOf(u8, output, "identity(42)") != null);
}

// Roundtrip verification tests
// These tests verify that emitted code produces the same result as the original

/// Helper to evaluate an expression via Str.inspect and return its output.
fn evalToInspect(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    const inspected = try helpers.allocInspectedExprSource(allocator, source);
    defer allocator.free(inspected);

    var compiled = try helpers.compileInspectedExpr(allocator, inspected);
    defer compiled.deinit(allocator);

    return helpers.lirInterpreterInspectedStr(allocator, &compiled.lowered);
}

/// Helper to verify an expression round-trips correctly through emitter
fn testRoundTrip(source: []const u8) !void {
    const original = try evalToInspect(test_allocator, source);
    defer test_allocator.free(original);

    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    const roundtrip = try evalToInspect(test_allocator, emitted);
    defer test_allocator.free(roundtrip);

    try testing.expectEqualStrings(original, roundtrip);
}

test "roundtrip: integer literal produces same result" {
    try testRoundTrip("42");
}

test "roundtrip: arithmetic expression produces same result" {
    try testRoundTrip("10 + 32");
}

test "roundtrip: if expression produces same result" {
    try testRoundTrip("if True 1 else 2");
}

test "roundtrip: boolean True produces same result" {
    try testRoundTrip("if True 1 else 0");
}

test "roundtrip: boolean False produces same result" {
    try testRoundTrip("if False 1 else 0");
}

test "roundtrip: complex arithmetic produces same result" {
    try testRoundTrip("(5 + 3) * 2");
}

test "roundtrip: tuple literal produces same result" {
    try testRoundTrip("(10, 20)");
}

test "roundtrip: computed tuple produces same result" {
    try testRoundTrip("(5 + 5, 10 * 2)");
}

test "roundtrip: arithmetic tuple produces same result" {
    try testRoundTrip("(1 + 2, 3 * 4)");
}

test "detect closure with single capture" {
    // Test that we can parse a lambda with a captured variable
    const source =
        \\{
        \\    y = 10
        \\    f = |x| x + y
        \\    f(5)
        \\}
    ;
    try testRoundTrip(source);
}

test "detect closure with multiple captures" {
    // Test that we can parse a lambda with multiple captured variables
    const source =
        \\{
        \\    y = 10
        \\    z = 20
        \\    f = |x| x + y + z
        \\    f(5)
        \\}
    ;
    try testRoundTrip(source);
}

test "detect pure lambda (no captures)" {
    // Test that we can parse a lambda with no captures
    const source =
        \\{
        \\    f = |x| x + 1
        \\    f(5)
        \\}
    ;
    try testRoundTrip(source);
}
