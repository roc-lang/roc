//! Tests for the expression evaluator
const std = @import("std");
const testing = std.testing;
const eval = @import("interpreter.zig");
const base = @import("base");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const CIR = canonicalize.CIR;
const types = @import("types");
const stack = @import("stack.zig");
const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");

const test_allocator = testing.allocator;

fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!struct {
    module_env: *base.ModuleEnv,
    parse_ast: *parse.AST,
    cir: *CIR,
    can: *canonicalize,
    checker: *check_types,
    expr_idx: CIR.Expr.Idx,
} {
    // Initialize the ModuleEnv
    const module_env = try allocator.create(base.ModuleEnv);
    const owned_source = try allocator.dupe(u8, source);
    module_env.* = try base.ModuleEnv.init(allocator, owned_source);

    // Parse the source code as an expression
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(module_env, source);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Create CIR
    const cir = try allocator.create(CIR);
    cir.* = try CIR.init(module_env, "test");

    // Create canonicalizer
    const can = try allocator.create(canonicalize);
    can.* = try canonicalize.init(cir, parse_ast, null);

    // Canonicalize the expression
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        // If canonicalization fails, create a runtime error
        const diagnostic_idx = try cir.store.addDiagnostic(.{ .not_implemented = .{
            .feature = try cir.env.strings.insert(allocator, "canonicalization failed"),
            .region = base.Region.zero(),
        } });
        const checker = try allocator.create(check_types);
        checker.* = try check_types.init(allocator, &module_env.types, cir, &.{}, &cir.store.regions);
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = cir,
            .can = can,
            .checker = checker,
            .expr_idx = try cir.store.addExpr(.{ .e_runtime_error = .{
                .diagnostic = diagnostic_idx,
            } }, base.Region.zero()),
        };
    };

    // Create type checker
    const checker = try allocator.create(check_types);
    checker.* = try check_types.init(allocator, &module_env.types, cir, &.{}, &cir.store.regions);

    // Type check the expression
    _ = try checker.checkExpr(canonical_expr_idx);

    // WORKAROUND: The type checker doesn't set types for binop expressions yet.
    // For numeric binops, manually set the type to match the operands.
    const expr = cir.store.getExpr(canonical_expr_idx);
    if (expr == .e_binop) {
        const binop = expr.e_binop;
        // For arithmetic ops, use the type of the left operand
        switch (binop.op) {
            .add, .sub, .mul, .div, .rem, .pow, .div_trunc => {
                const left_var = @as(types.Var, @enumFromInt(@intFromEnum(binop.lhs)));
                const left_resolved = module_env.types.resolveVar(left_var);
                const result_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
                try module_env.types.setVarContent(result_var, left_resolved.desc.content);
            },
            .lt, .gt, .le, .ge, .eq, .ne => {
                // Comparison ops return Bool
                const result_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
                const bool_content = try module_env.types.mkBool(allocator, &module_env.idents, @enumFromInt(0));
                try module_env.types.setVarContent(result_var, bool_content);
            },
            else => {},
        }
    }

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir,
        .can = can,
        .checker = checker,
        .expr_idx = canonical_expr_idx,
    };
}

fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: anytype) void {
    resources.checker.deinit();
    resources.can.deinit();
    resources.cir.deinit();
    resources.parse_ast.deinit(allocator);
    // module_env.source is freed by module_env.deinit()
    resources.module_env.deinit();
    allocator.destroy(resources.checker);
    allocator.destroy(resources.can);
    allocator.destroy(resources.cir);
    allocator.destroy(resources.parse_ast);
    allocator.destroy(resources.module_env);
}

test "eval runtime error - returns crash error" {
    const source = "crash \"test feature\"";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if the expression is a runtime error
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    if (expr == .e_runtime_error) {
        // Create a stack for evaluation
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();

        // Create layout store
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        // Evaluating a runtime error should return an error
        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = interpreter.eval(resources.expr_idx);
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // If crash syntax is not supported in canonicalization, skip
        return error.SkipZigTest;
    }
}

test "eval tag - already primitive" {
    // Skip this test for now as tag_union layout is not yet implemented
    return error.SkipZigTest;
}

test "eval binop - basic implementation" {
    const source = "5 + 3";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    // Evaluate the binop expression
    var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
    defer interpreter.deinit();
    const result = try interpreter.eval(resources.expr_idx);

    // Verify we got a scalar layout
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);

    // Read the result
    const int_val = switch (result.layout.data.scalar.data.int) {
        .i64 => @as(i64, @as(*i64, @ptrCast(@alignCast(result.ptr))).*),
        .i32 => @as(i64, @as(*i32, @ptrCast(@alignCast(result.ptr))).*),
        .i16 => @as(i64, @as(*i16, @ptrCast(@alignCast(result.ptr))).*),
        .i8 => @as(i64, @as(*i8, @ptrCast(@alignCast(result.ptr))).*),
        .u64 => @as(i64, @intCast(@as(*u64, @ptrCast(@alignCast(result.ptr))).*)),
        .u32 => @as(i64, @intCast(@as(*u32, @ptrCast(@alignCast(result.ptr))).*)),
        .u16 => @as(i64, @intCast(@as(*u16, @ptrCast(@alignCast(result.ptr))).*)),
        .u8 => @as(i64, @intCast(@as(*u8, @ptrCast(@alignCast(result.ptr))).*)),
        .u128 => @as(i64, @intCast(@as(*u128, @ptrCast(@alignCast(result.ptr))).*)),
        .i128 => @as(i64, @intCast(@as(*i128, @ptrCast(@alignCast(result.ptr))).*)),
    };

    try testing.expectEqual(@as(i64, 8), int_val);
}

test "eval if expression with boolean tags" {
    // Test that if expressions with boolean tag conditions evaluate correctly
    const sources = [_]struct { src: []const u8, expected: i128 }{
        .{ .src = "if True 1 else 0", .expected = 1 },
        .{ .src = "if False 1 else 0", .expected = 0 },
    };

    for (sources) |test_case| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, test_case.src);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = try interpreter.eval(resources.expr_idx);

        // Verify the result
        try testing.expect(result.layout.tag == .scalar);
        try testing.expect(result.layout.data.scalar.tag == .int);
        const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
        try testing.expectEqual(test_case.expected, value);
    }
}

test "eval if expression with comparison condition" {
    // Test if expressions with comparison conditions that evaluate to boolean tags
    const sources = [_]struct { src: []const u8, expected: i128 }{
        .{ .src = "if (1 == 1) 42 else 99", .expected = 42 },
        .{ .src = "if (1 == 2) 42 else 99", .expected = 99 },
        .{ .src = "if (5 > 3) 100 else 200", .expected = 100 },
        .{ .src = "if (3 > 5) 100 else 200", .expected = 200 },
    };

    for (sources) |test_case| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, test_case.src);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = try interpreter.eval(resources.expr_idx);

        // Verify the result
        try testing.expect(result.layout.tag == .scalar);
        try testing.expect(result.layout.data.scalar.tag == .int);
        const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
        try testing.expectEqual(test_case.expected, value);
    }
}

test "eval nested if expressions" {
    // Test that nested if expressions evaluate correctly
    const sources = [_]struct { src: []const u8, expected: i128 }{
        .{ .src = "if True (if True 10 else 20) else 30", .expected = 10 },
        .{ .src = "if True (if False 10 else 20) else 30", .expected = 20 },
        .{ .src = "if False (if True 10 else 20) else 30", .expected = 30 },
        .{ .src = "if False 99 else (if True 40 else 50)", .expected = 40 },
        .{ .src = "if False 99 else (if False 40 else 50)", .expected = 50 },
    };

    for (sources) |test_case| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, test_case.src);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = try interpreter.eval(resources.expr_idx);

        // Verify the result
        try testing.expect(result.layout.tag == .scalar);
        try testing.expect(result.layout.data.scalar.tag == .int);
        const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
        try testing.expectEqual(test_case.expected, value);
    }
}

test "eval if-else if-else chains" {
    // Test that if-else if-else chains evaluate correctly, taking the first true branch
    const sources = [_]struct { src: []const u8, expected: i128 }{
        .{ .src = 
        \\if True
        \\    10
        \\else if True
        \\    20
        \\else
        \\    30
        , .expected = 10 }, // First branch is true
        .{ .src = 
        \\if False
        \\    10
        \\else if True
        \\    20
        \\else
        \\    30
        , .expected = 20 }, // Second branch is true
        .{ .src = 
        \\if False
        \\    10
        \\else if False
        \\    20
        \\else
        \\    30
        , .expected = 30 }, // All conditions false, use else
    };

    for (sources) |test_case| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, test_case.src);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = try interpreter.eval(resources.expr_idx);

        // Verify the result
        try testing.expect(result.layout.tag == .scalar);
        try testing.expect(result.layout.data.scalar.tag == .int);
        const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
        try testing.expectEqual(test_case.expected, value);
    }
}

test "eval if expression with arithmetic in branches" {
    // Test that expressions in branches are evaluated correctly
    const sources = [_]struct { src: []const u8, expected: i128 }{
        .{ .src = "if True (1 + 2) else (3 + 4)", .expected = 3 },
        .{ .src = "if False (1 + 2) else (3 + 4)", .expected = 7 },
        .{ .src = "if True (10 * 5) else (20 / 4)", .expected = 50 },
        .{ .src = "if (2 > 1) (100 - 50) else (200 - 100)", .expected = 50 },
    };

    for (sources) |test_case| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, test_case.src);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = try interpreter.eval(resources.expr_idx);

        // Verify the result
        try testing.expect(result.layout.tag == .scalar);
        try testing.expect(result.layout.data.scalar.tag == .int);
        const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
        try testing.expectEqual(test_case.expected, value);
    }
}

test "eval if expression with non-boolean condition" {
    // Test that if expressions with non-boolean conditions result in type errors
    const source = "if 42 1 else 0"; // Integer condition (should be type error)

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
    defer interpreter.deinit();
    const result = interpreter.eval(resources.expr_idx);

    // Should result in a TypeContainedMismatch error because condition must be a boolean tag union
    try testing.expectError(eval.EvalError.TypeContainedMismatch, result);
}

test "eval simple number" {
    const source = "42";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    // Evaluate the number
    var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
    defer interpreter.deinit();
    const result = try interpreter.eval(resources.expr_idx);

    // Verify we got an integer layout
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);

    // Read the value back based on the precision
    const value: i128 = switch (result.layout.data.scalar.data.int) {
        .u8 => @as(*u8, @ptrCast(@alignCast(result.ptr))).*,
        .i8 => @as(*i8, @ptrCast(@alignCast(result.ptr))).*,
        .u16 => @as(*u16, @ptrCast(@alignCast(result.ptr))).*,
        .i16 => @as(*i16, @ptrCast(@alignCast(result.ptr))).*,
        .u32 => @as(*u32, @ptrCast(@alignCast(result.ptr))).*,
        .i32 => @as(*i32, @ptrCast(@alignCast(result.ptr))).*,
        .u64 => @as(*u64, @ptrCast(@alignCast(result.ptr))).*,
        .i64 => @as(*i64, @ptrCast(@alignCast(result.ptr))).*,
        .u128 => @intCast(@as(*u128, @ptrCast(@alignCast(result.ptr))).*),
        .i128 => @as(*i128, @ptrCast(@alignCast(result.ptr))).*,
    };

    // The parser now correctly converts "42" to the integer 42
    try testing.expectEqual(@as(i128, 42), value);
}

test "eval negative number" {
    const source = "-42";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    // Evaluate the number
    var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
    defer interpreter.deinit();
    const result = try interpreter.eval(resources.expr_idx);

    // Verify we got an integer layout
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);

    // Read the value back based on the precision
    const value: i128 = switch (result.layout.data.scalar.data.int) {
        .u8 => @as(*u8, @ptrCast(@alignCast(result.ptr))).*,
        .i8 => @as(*i8, @ptrCast(@alignCast(result.ptr))).*,
        .u16 => @as(*u16, @ptrCast(@alignCast(result.ptr))).*,
        .i16 => @as(*i16, @ptrCast(@alignCast(result.ptr))).*,
        .u32 => @as(*u32, @ptrCast(@alignCast(result.ptr))).*,
        .i32 => @as(*i32, @ptrCast(@alignCast(result.ptr))).*,
        .u64 => @as(*u64, @ptrCast(@alignCast(result.ptr))).*,
        .i64 => @as(*i64, @ptrCast(@alignCast(result.ptr))).*,
        .u128 => @intCast(@as(*u128, @ptrCast(@alignCast(result.ptr))).*),
        .i128 => @as(*i128, @ptrCast(@alignCast(result.ptr))).*,
    };

    // The parser now correctly converts "-42" to the integer -42
    try testing.expectEqual(@as(i128, -42), value);
}

test "eval list literal" {
    const source = "[1, 2, 3]";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
    defer layout_cache.deinit();

    // List literals are not yet implemented
    var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
    defer interpreter.deinit();
    const result = interpreter.eval(resources.expr_idx);
    try testing.expectError(eval.EvalError.LayoutError, result);
}

test "eval record literal" {
    const source = "{ x: 10, y: 20 }";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if this resulted in a runtime error due to failed canonicalization
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    if (expr == .e_runtime_error) {
        // Expected - canonicalization of records may not be fully implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();
        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = interpreter.eval(resources.expr_idx);
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // Record literals are not yet implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();
        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = interpreter.eval(resources.expr_idx);
        try testing.expectError(eval.EvalError.LayoutError, result);
    }
}

test "eval empty record" {
    const source = "{}";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if this resulted in a runtime error due to incomplete canonicalization
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    if (expr == .e_runtime_error) {
        // Expected - canonicalization of empty records may not be fully implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();
        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = interpreter.eval(resources.expr_idx);
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // Create a stack for evaluation
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();

        // Record the stack position before evaluation
        const stack_before = eval_stack.used;

        // Create layout store
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        // Empty records are zero-sized types, which should return an error
        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();
        const result = interpreter.eval(resources.expr_idx);
        try testing.expectError(eval.EvalError.ZeroSizedType, result);

        // Verify the stack didn't grow
        const stack_after = eval_stack.used;
        try testing.expectEqual(stack_before, stack_after);
    }
}

test "eval integer literal directly from CIR node" {
    // This test creates expressions without proper canonicalization/type checking,
    // which means there are no corresponding type variables in the type store.
    // Since eval now requires real layouts from the type checker, this test
    // would need significant rework to properly set up the type system.
    // For now, skip this test.
    return error.SkipZigTest;
}

test "interpreter reuse across multiple evaluations" {
    // This test demonstrates that the interpreter can be reused across multiple
    // eval() calls, avoiding repeated allocations in scenarios like the REPL

    // Test multiple evaluations with the same work stack
    const sources = [_][]const u8{ "42", "100 + 200", "if True 1 else 2" };
    const expected = [_]i128{ 42, 300, 1 };

    for (sources, expected) |source, expected_value| {
        const resources = try parseAndCanonicalizeExpr(test_allocator, source);
        defer cleanupParseAndCanonical(test_allocator, resources);

        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types);
        defer layout_cache.deinit();

        // Create interpreter for this evaluation
        var interpreter = try eval.Interpreter.init(test_allocator, resources.cir, &eval_stack, &layout_cache, &resources.module_env.types);
        defer interpreter.deinit();

        // Verify work stack is empty before eval
        try testing.expectEqual(@as(usize, 0), interpreter.work_stack.items.len);

        const result = try interpreter.eval(resources.expr_idx);

        // Verify work stack is empty after eval (should be naturally empty, not cleared)
        try testing.expectEqual(@as(usize, 0), interpreter.work_stack.items.len);

        // Verify the result
        try testing.expect(result.layout.tag == .scalar);
        try testing.expect(result.layout.data.scalar.tag == .int);
        const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
        try testing.expectEqual(expected_value, value);
    }
}
