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
const base = @import("base");
const can = @import("can");
const builtins = @import("builtins");
const i128h = builtins.compiler_rt_128;

const helpers = @import("helpers.zig");
const eval_mod = @import("../mod.zig");
const roc_target = @import("roc_target");
const TestEnv = @import("TestEnv.zig");
const Interpreter = eval_mod.Interpreter;
const BuiltinTypes = eval_mod.BuiltinTypes;

const Emitter = can.RocEmitter;

const testing = std.testing;
// Use interpreter_allocator for interpreter tests (doesn't track leaks)
const test_allocator = helpers.interpreter_allocator;

/// Helper to check if output contains a closure tag (format: C followed by digit)
/// Closure tags use internal format #N_hint which RocEmitter transforms to CN_hint
fn hasClosureTag(output: []const u8) bool {
    var i: usize = 0;
    while (i < output.len) : (i += 1) {
        if (output[i] == 'C' and i + 1 < output.len and output[i + 1] >= '0' and output[i + 1] <= '9') {
            return true;
        }
    }
    return false;
}

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

/// Helper to evaluate an expression and get its integer result
fn evalToInt(allocator: std.mem.Allocator, source: []const u8) !i128 {
    const resources = try helpers.parseAndCanonicalizeExpr(allocator, source);
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);
    defer interpreter.bindings.items.len = 0;

    // Check if this is an integer or Dec
    if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .int) {
        return result.asI128();
    } else if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .frac) {
        // Unsuffixed numeric literals default to Dec
        const dec_value = result.asDec(ops);
        const RocDec = builtins.dec.RocDec;
        return i128h.divTrunc_i128(dec_value.num, RocDec.one_point_zero_i128);
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
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);
    defer interpreter.bindings.items.len = 0;

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

/// Helper to check if source code contains a closure with captures
fn hasClosureWithCaptures(allocator: std.mem.Allocator, source: []const u8) !bool {
    const resources = try helpers.parseAndCanonicalizeExpr(allocator, source);
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    // Recursively check if any expression is a closure with captures
    return checkForCapturesRecursive(resources.module_env, resources.expr_idx);
}

fn checkForCapturesRecursive(module_env: *can.ModuleEnv, expr_idx: can.CIR.Expr.Idx) bool {
    const expr = module_env.store.getExpr(expr_idx);
    switch (expr) {
        .e_closure => |closure| {
            if (closure.captures.span.len > 0) {
                return true;
            }
            // Also check the lambda body
            return checkForCapturesRecursive(module_env, closure.lambda_idx);
        },
        .e_lambda => |lambda| {
            return checkForCapturesRecursive(module_env, lambda.body);
        },
        .e_block => |block| {
            // Check statements
            const stmts = module_env.store.sliceStatements(block.stmts);
            for (stmts) |stmt_idx| {
                const stmt = module_env.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        if (checkForCapturesRecursive(module_env, decl.expr)) {
                            return true;
                        }
                    },
                    else => {},
                }
            }
            // Check final expression
            return checkForCapturesRecursive(module_env, block.final_expr);
        },
        .e_call => |call| {
            if (checkForCapturesRecursive(module_env, call.func)) {
                return true;
            }
            const args = module_env.store.sliceExpr(call.args);
            for (args) |arg_idx| {
                if (checkForCapturesRecursive(module_env, arg_idx)) {
                    return true;
                }
            }
            return false;
        },
        .e_if => |if_expr| {
            const branches = module_env.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = module_env.store.getIfBranch(branch_idx);
                if (checkForCapturesRecursive(module_env, branch.cond) or
                    checkForCapturesRecursive(module_env, branch.body))
                {
                    return true;
                }
            }
            return checkForCapturesRecursive(module_env, if_expr.final_else);
        },
        .e_binop => |binop| {
            return checkForCapturesRecursive(module_env, binop.lhs) or
                checkForCapturesRecursive(module_env, binop.rhs);
        },
        else => return false,
    }
}

test "detect closure with single capture" {
    const source =
        \\{
        \\    x = 42
        \\    f = |y| x + y
        \\    f(10)
        \\}
    ;

    const has_captures = try hasClosureWithCaptures(test_allocator, source);
    try testing.expect(has_captures);
}

test "detect closure with multiple captures" {
    const source =
        \\{
        \\    a = 1
        \\    b = 2
        \\    f = |x| a + b + x
        \\    f(3)
        \\}
    ;

    const has_captures = try hasClosureWithCaptures(test_allocator, source);
    try testing.expect(has_captures);
}

test "detect pure lambda (no captures)" {
    const source =
        \\{
        \\    f = |x| x + 1
        \\    f(41)
        \\}
    ;

    const has_captures = try hasClosureWithCaptures(test_allocator, source);
    try testing.expect(!has_captures);
}

/// Helper to transform a single closure expression directly (not in a block)
fn transformClosureExpr(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    const resources = try helpers.parseAndCanonicalizeExpr(allocator, source);
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    // Create transformer
    var transformer = can.ClosureTransformer.init(allocator, resources.module_env);
    defer transformer.deinit();

    // Transform just the expression (not the block around it)
    const transformed_idx = try transformer.transformClosure(resources.expr_idx, null);

    // Emit the transformed expression
    var emitter = Emitter.init(allocator, resources.module_env);
    defer emitter.deinit();

    try emitter.emitExpr(transformed_idx);

    return try allocator.dupe(u8, emitter.getOutput());
}

test "pure lambda is left unchanged" {
    // Test a pure lambda (no captures) - should NOT be transformed
    const source = "|x| x + 1";

    const output = try transformClosureExpr(test_allocator, source);
    defer test_allocator.free(output);

    // Pure lambda should be left as-is, NOT transformed to a tag
    // Closure tags now use format C1_hint or C1 (# prefix transformed to C by RocEmitter)
    try testing.expect(!hasClosureTag(output));
    try testing.expectEqualStrings("|x| x + 1", output);
}

/// Helper to transform closures in a block and emit the result
fn transformBlockAndEmit(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    const resources = try helpers.parseAndCanonicalizeExpr(allocator, source);
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    // Create transformer
    var transformer = can.ClosureTransformer.init(allocator, resources.module_env);
    defer transformer.deinit();

    // Transform the entire expression tree
    const transformed_idx = try transformer.transformExpr(resources.expr_idx);

    // Emit the transformed expression
    var emitter = Emitter.init(allocator, resources.module_env);
    defer emitter.deinit();

    try emitter.emitExpr(transformed_idx);

    return try allocator.dupe(u8, emitter.getOutput());
}

test "transform closure with single capture to tag" {
    const source =
        \\{
        \\    x = 42
        \\    f = |y| x + y
        \\    f(10)
        \\}
    ;

    const output = try transformBlockAndEmit(test_allocator, source);
    defer test_allocator.free(output);

    // The closure should have been transformed to a # tag (emitted as C1_...)
    try testing.expect(hasClosureTag(output));

    // The capture 'x' should appear in the tag's record argument
    // RocEmitter uses shorthand syntax { x } for captures
    try testing.expect(std.mem.indexOf(u8, output, "{ x }") != null or
        std.mem.indexOf(u8, output, "x:") != null);

    // The call should have been transformed to a match expression
    try testing.expect(std.mem.indexOf(u8, output, "match") != null);
}

test "transform closure with multiple captures" {
    const source =
        \\{
        \\    a = 1
        \\    b = 2
        \\    f = |x| a + b + x
        \\    f(3)
        \\}
    ;

    const output = try transformBlockAndEmit(test_allocator, source);
    defer test_allocator.free(output);

    // The closure should have been transformed to a # tag (emitted as C1_...)
    try testing.expect(hasClosureTag(output));

    // Both captures 'a' and 'b' should appear in the tag's record
    // RocEmitter uses shorthand syntax { a, b } for captures
    try testing.expect(std.mem.indexOf(u8, output, "{ a") != null or
        std.mem.indexOf(u8, output, "a:") != null);
    try testing.expect(std.mem.indexOf(u8, output, " b }") != null or
        std.mem.indexOf(u8, output, ", b") != null or
        std.mem.indexOf(u8, output, "b:") != null);

    // The call should have been transformed to a match expression
    try testing.expect(std.mem.indexOf(u8, output, "match") != null);
}

test "verify: closure with single capture transforms correctly" {
    const source =
        \\{
        \\    x = 42
        \\    f = |y| x + y
        \\    f(10)
        \\}
    ;

    // Transform the code
    const transformed = try transformBlockAndEmit(test_allocator, source);
    defer test_allocator.free(transformed);

    // Verify transformation structure:
    // - Should have a # tag (internal closure tag, emitted as C1_...)
    // - Should have a match expression for the call site
    // - Should reference the captured variable x
    try testing.expect(hasClosureTag(transformed));
    try testing.expect(std.mem.indexOf(u8, transformed, "match") != null);
}

test "verify: closure with multiple captures transforms correctly" {
    const source =
        \\{
        \\    a = 1
        \\    b = 2
        \\    f = |x| a + b + x
        \\    f(3)
        \\}
    ;

    // Transform the code
    const transformed = try transformBlockAndEmit(test_allocator, source);
    defer test_allocator.free(transformed);

    // Verify transformation structure:
    // - Should have a # tag (internal closure tag, emitted as C1_...)
    // - Should have a match expression for the call site
    try testing.expect(hasClosureTag(transformed));
    try testing.expect(std.mem.indexOf(u8, transformed, "match") != null);
}

test "verify: pure lambda (no captures) is left unchanged" {
    const source =
        \\{
        \\    f = |x| x + 1
        \\    f(41)
        \\}
    ;

    // Transform the code
    const transformed = try transformBlockAndEmit(test_allocator, source);
    defer test_allocator.free(transformed);

    // Verify pure lambda is NOT transformed:
    // - Should NOT have a closure tag (C followed by digit)
    // - Should NOT have a match expression
    // - Should have the lambda preserved as-is
    try testing.expect(!hasClosureTag(transformed));
    try testing.expect(std.mem.indexOf(u8, transformed, "match") == null);
    try testing.expect(std.mem.indexOf(u8, transformed, "|x|") != null);
    try testing.expect(std.mem.indexOf(u8, transformed, "f(41)") != null);
}

test "verify: nested closures with captures transforms correctly" {
    // A closure that returns another closure, both with captures
    const source =
        \\{
        \\    x = 10
        \\    makeAdder = |y| |z| x + y + z
        \\    addFive = makeAdder(5)
        \\    addFive(3)
        \\}
    ;

    // Transform the code
    const transformed = try transformBlockAndEmit(test_allocator, source);
    defer test_allocator.free(transformed);

    // Verify transformation structure:
    // - Should have # tags (internal closure tags, emitted as C1_...)
    // - Should have match expressions for the call sites
    try testing.expect(hasClosureTag(transformed));
    try testing.expect(std.mem.indexOf(u8, transformed, "match") != null);
}

test "ClosureTransformer: can generate tag names" {
    // Test that the transformer can generate unique tag names
    const allocator = test_allocator;

    const module_env = try allocator.create(can.ModuleEnv);
    module_env.* = try can.ModuleEnv.init(allocator, "test");
    defer {
        module_env.deinit();
        allocator.destroy(module_env);
    }

    var transformer = can.ClosureTransformer.init(allocator, module_env);
    defer transformer.deinit();

    // Generate a tag name with a hint
    const hint = try module_env.insertIdent(base.Ident.for_text("myFunc"));
    const tag_name1 = try transformer.generateClosureTagName(hint);
    const tag_str1 = module_env.getIdent(tag_name1);

    try testing.expectEqualStrings("#1_myFunc", tag_str1);

    // Generate another tag name without hint
    const tag_name2 = try transformer.generateClosureTagName(null);
    const tag_str2 = module_env.getIdent(tag_name2);

    try testing.expectEqualStrings("#2", tag_str2);
}

// Constant folding tests
// These tests verify that compile-time evaluation correctly folds
// tuples, tags with payloads, and nested structures

test "end-to-end: emit tuple literal" {
    const output = try emitFromSource(test_allocator, "(1, 2, 3)");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("(1, 2, 3)", output);
}

test "end-to-end: emit nested tuple" {
    const output = try emitFromSource(test_allocator, "((1, 2), (3, 4))");
    defer test_allocator.free(output);

    try testing.expectEqualStrings("((1, 2), (3, 4))", output);
}

test "end-to-end: emit tag application with single integer payload" {
    // In Roc, `Some 42` is a tag call application, which emits as the tag name
    // and its argument separately: the tag function applied to the argument
    const output = try emitFromSource(test_allocator, "Some 42");
    defer test_allocator.free(output);

    // Tag applications are currently emitted as just the tag name for the tag part
    // and the arguments follow the syntax of the original expression
    try testing.expect(std.mem.indexOf(u8, output, "Some") != null);
}

test "end-to-end: emit tag application with multiple arguments" {
    // `Pair 1 2` is a tag applied to two arguments
    const output = try emitFromSource(test_allocator, "Pair 1 2");
    defer test_allocator.free(output);

    try testing.expect(std.mem.indexOf(u8, output, "Pair") != null);
}

test "end-to-end: emit nested tag application" {
    const output = try emitFromSource(test_allocator, "Outer (Inner 5)");
    defer test_allocator.free(output);

    // The outer tag should be present
    try testing.expect(std.mem.indexOf(u8, output, "Outer") != null);
}

/// Helper to evaluate an expression and get the first element of a tuple result
fn evalTupleFirst(allocator: std.mem.Allocator, source: []const u8) !i128 {
    const resources = try helpers.parseAndCanonicalizeExpr(allocator, source);
    defer helpers.cleanupParseAndCanonical(allocator, resources);

    var test_env_instance = TestEnv.init(allocator);
    defer test_env_instance.deinit();

    const builtin_types = BuiltinTypes.init(resources.builtin_indices, resources.builtin_module.env, resources.builtin_module.env, resources.builtin_module.env);
    const imported_envs = [_]*const can.ModuleEnv{ resources.module_env, resources.builtin_module.env };
    var interpreter = try Interpreter.init(allocator, resources.module_env, builtin_types, resources.builtin_module.env, &imported_envs, &resources.checker.import_mapping, null, null, roc_target.RocTarget.detectNative());
    defer interpreter.deinit();

    const ops = test_env_instance.get_ops();
    const result = try interpreter.eval(resources.expr_idx, ops);
    const layout_cache = &interpreter.runtime_layout_store;
    defer result.decref(layout_cache, ops);
    defer interpreter.bindings.items.len = 0;

    // Get the first element of the tuple
    if (result.layout.tag == .tuple) {
        const fresh_var = try interpreter.runtime_types.fresh();
        var accessor = try result.asTuple(layout_cache);
        const first_elem = try accessor.getElement(0, fresh_var);
        if (first_elem.layout.tag == .scalar and first_elem.layout.data.scalar.tag == .int) {
            const tmp_sv = eval_mod.StackValue{ .layout = first_elem.layout, .ptr = first_elem.ptr, .is_initialized = true, .rt_var = fresh_var };
            return tmp_sv.asI128();
        } else if (first_elem.layout.tag == .scalar and first_elem.layout.data.scalar.tag == .frac) {
            const tmp_sv = eval_mod.StackValue{ .layout = first_elem.layout, .ptr = first_elem.ptr, .is_initialized = true, .rt_var = fresh_var };
            const dec_value = tmp_sv.asDec(ops);
            const RocDec = builtins.dec.RocDec;
            return i128h.divTrunc_i128(dec_value.num, RocDec.one_point_zero_i128);
        }
    }
    return error.NotATuple;
}

test "roundtrip: tuple literal produces same result" {
    const source = "(10, 20)";

    // Get original result - first element
    const original_result = try evalTupleFirst(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalTupleFirst(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(@as(i128, 10), emitted_result);
}

test "roundtrip: computed tuple produces same result" {
    const source =
        \\{
        \\    x = 5
        \\    y = 10
        \\    (x, y)
        \\}
    ;

    // Get original result
    const original_result = try evalTupleFirst(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalTupleFirst(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(@as(i128, 5), emitted_result);
}

test "roundtrip: arithmetic tuple produces same result" {
    const source = "(1 + 2, 3 * 4)";

    // Get original result - first element should be 3
    const original_result = try evalTupleFirst(test_allocator, source);

    // Emit and re-parse
    const emitted = try emitFromSource(test_allocator, source);
    defer test_allocator.free(emitted);

    // Get result from emitted code
    const emitted_result = try evalTupleFirst(test_allocator, emitted);

    // Verify they match
    try testing.expectEqual(original_result, emitted_result);
    try testing.expectEqual(@as(i128, 3), emitted_result);
}
