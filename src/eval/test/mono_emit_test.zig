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
const parse = @import("parse");
const types = @import("types");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const builtins = @import("builtins");

const helpers = @import("helpers.zig");
const eval_mod = @import("../mod.zig");
const TestEnv = @import("TestEnv.zig");
const Interpreter = eval_mod.Interpreter;
const BuiltinTypes = eval_mod.BuiltinTypes;

const Emitter = can.RocEmitter;
const Monomorphizer = can.Monomorphizer;

const testing = std.testing;
const test_allocator = testing.allocator;

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

    // After parsing, the expression becomes a binop
    try testing.expectEqualStrings("(1 + 2)", output);
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

    try testing.expectEqualStrings("|x| (x + 1)", output);
}

test "end-to-end: emit if expression" {
    const output = try emitFromSource(test_allocator, "if True 1 else 2");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("if True 1 else 2", output);
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

// Monomorphization infrastructure tests

test "monomorphizer: can be initialized with parsed/type-checked code" {
    const resources = try helpers.parseAndCanonicalizeExpr(test_allocator, "42");
    defer helpers.cleanupParseAndCanonical(test_allocator, resources);

    // Initialize monomorphizer with the module environment and types
    var mono = Monomorphizer.init(test_allocator, resources.module_env, &resources.module_env.types);
    defer mono.deinit();

    // Basic sanity check - monomorphizer should be ready
    try testing.expectEqual(@as(u32, 0), mono.specialization_counter);
}

test "monomorphizer: identity function is polymorphic before type checking" {
    // This test parses an identity lambda and checks it can be emitted
    const output = try emitFromSource(test_allocator, "|x| x");
    defer test_allocator.free(output);

    // The identity function emits as expected
    try testing.expectEqualStrings("|x| x", output);
}

test "monomorphizer: can emit identity function applied to integer" {
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

/// Helper to evaluate an expression and get its integer result
fn evalToInt(allocator: std.mem.Allocator, source: []const u8) !i128 {
    const resources = try helpers.parseAndCanonicalizeExpr(allocator, source);
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);
    defer interpreter.cleanupBindings(ops);

    // Check if this is an integer or Dec
    if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) {
        return result.asI128();
    } else if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .frac) {
        // Unsuffixed numeric literals default to Dec
        const dec_value = result.asDec(ops);
        const RocDec = builtins.dec.RocDec;
        return @divTrunc(dec_value.num, RocDec.one_point_zero_i128);
    }
    return error.NotAnInteger;
}

/// Helper to evaluate an expression and get its boolean result
fn evalToBool(allocator: std.mem.Allocator, source: []const u8) !bool {
    const resources = try helpers.parseAndCanonicalizeExpr(allocator, source);
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{resources.builtin_module.env};
    var interpreter = try Interpreter.init(allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null);
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);
    defer interpreter.cleanupBindings(ops);

    // Boolean represented as integer (discriminant)
    if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) {
        const int_val = result.asI128();
        return int_val != 0;
    } else if (result.ptr != null) {
        // Try reading as raw byte (for boolean tag values)
        const bool_ptr: *const u8 = @ptrCast(@alignCast(result.ptr.?));
        return bool_ptr.* != 0;
    }
    return error.NotABool;
}

test "roundtrip: integer literal produces same result" {
    const source = "42";

    // Get original result
    const original_result = try evalToInt(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalToInt(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(@as(i128, 42), emitted_result);
}

test "roundtrip: arithmetic expression produces same result" {
    const source = "10 + 32";

    // Get original result
    const original_result = try evalToInt(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalToInt(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(@as(i128, 42), emitted_result);
}

test "roundtrip: if expression produces same result" {
    const source = "if True 1 else 2";

    // Get original result
    const original_result = try evalToInt(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalToInt(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(@as(i128, 1), emitted_result);
}

test "roundtrip: boolean True produces same result" {
    const source = "True";

    // Get original result
    const original_result = try evalToBool(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalToBool(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(true, emitted_result);
}

test "roundtrip: boolean False produces same result" {
    const source = "False";

    // Get original result
    const original_result = try evalToBool(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalToBool(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(false, emitted_result);
}

test "roundtrip: complex arithmetic produces same result" {
    const source = "(5 + 3) * 2";

    // Get original result
    const original_result = try evalToInt(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalToInt(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(@as(i128, 16), emitted_result);
}
