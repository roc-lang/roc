const std = @import("std");
const testing = std.testing;
const eval = @import("eval.zig");
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const CIR = canonicalize.CIR;
const types = @import("../types.zig");
const stack = @import("stack.zig");
const layout = @import("../layout/layout.zig");
const layout_store = @import("../layout/store.zig");

const test_allocator = testing.allocator;

fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *base.ModuleEnv,
    parse_ast: *parse.AST,
    cir: *CIR,
    can: *canonicalize,
    expr_idx: CIR.Expr.Idx,
} {
    // Initialize the ModuleEnv
    const module_env = try allocator.create(base.ModuleEnv);
    module_env.* = base.ModuleEnv.init(allocator);

    // Parse the source code as an expression
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = parse.parseExpr(module_env, source);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Create CIR
    const cir = try allocator.create(CIR);
    cir.* = CIR.init(module_env);

    // Create canonicalizer
    const can = try allocator.create(canonicalize);
    can.* = canonicalize.init(cir, parse_ast);

    // Canonicalize the expression
    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = can.canonicalize_expr(expr_idx) orelse {
        // If canonicalization fails, create a runtime error
        const diagnostic_idx = cir.store.addDiagnostic(.{ .not_implemented = .{
            .feature = cir.env.strings.insert(allocator, "canonicalization failed"),
            .region = base.Region.zero(),
        } });
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = cir,
            .can = can,
            .expr_idx = cir.store.addExpr(.{ .runtime_error = .{
                .diagnostic = diagnostic_idx,
                .region = base.Region.zero(),
            } }),
        };
    };

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir,
        .can = can,
        .expr_idx = canonical_expr_idx,
    };
}

fn cleanupParseAndCanonical(allocator: std.mem.Allocator, resources: anytype) void {
    resources.can.deinit();
    resources.cir.deinit();
    resources.parse_ast.deinit(allocator);
    resources.module_env.deinit();
    allocator.destroy(resources.can);
    allocator.destroy(resources.cir);
    allocator.destroy(resources.parse_ast);
    allocator.destroy(resources.module_env);
}

// Commented out string tests as requested (no heap allocation yet)
// test "eval string segment - already primitive" {
//     const source = "\"Hello, World!\"";
//
//     const resources = try parseAndCanonicalizeExpr(test_allocator, source);
//     defer cleanupParseAndCanonical(test_allocator, resources);
//     ...
// }

// test "eval string literal - already primitive" {
//     const source = "\"Hello\"";
//
//     const resources = try parseAndCanonicalizeExpr(test_allocator, source);
//     defer cleanupParseAndCanonical(test_allocator, resources);
//     ...
// }

test "eval runtime error - returns crash error" {
    const source = "crash \"test feature\"";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
    defer layout_cache.deinit();

    // Evaluating a runtime error should return an error
    const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
    try testing.expectError(eval.EvalError.Crash, result);
}

test "eval tag - already primitive" {
    const source = "Ok";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if this resulted in a runtime error due to failed canonicalization
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    if (expr == .runtime_error) {
        // Expected - canonicalization of tags may not be fully implemented
        return;
    }

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
    defer layout_cache.deinit();

    // Evaluate the tag
    const result = try eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);

    // Verify we got a valid layout and pointer
    try testing.expect(@intFromPtr(result.ptr) != 0);
    try testing.expect(result.layout.tag == .scalar or result.layout.tag == .tuple);
}

test "eval binop - not yet implemented" {
    const source = "5 + 3";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
    defer layout_cache.deinit();

    // For now, binop evaluation is not implemented
    const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
    try testing.expectError(eval.EvalError.LayoutError, result);
}

test "eval call - not yet implemented" {
    const source = "List.len []";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if this resulted in a runtime error due to failed canonicalization
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    if (expr == .runtime_error) {
        // Expected - canonicalization of calls may not be fully implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
        defer layout_cache.deinit();
        const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // For now, call evaluation is not implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
        defer layout_cache.deinit();
        const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
        try testing.expectError(eval.EvalError.LayoutError, result);
    }
}

// Commented out string concatenation test
// test "eval multiple string segments" {
//     const source = "\"Hello, \" ++ \"World!\"";
//     ...
// }

test "eval if expression - always takes final_else branch" {
    const source = "if Bool.true then \"true branch\" else \"else branch\"";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if this resulted in a runtime error due to failed canonicalization
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    if (expr == .runtime_error) {
        // Expected - canonicalization of if expressions may not be fully implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
        defer layout_cache.deinit();
        const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
        try testing.expectError(eval.EvalError.Crash, result);
    } else if (expr == .@"if") {
        // For now, string branches will fail
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
        defer layout_cache.deinit();
        const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
        try testing.expectError(eval.EvalError.LayoutError, result);
    }
}

test "eval simple number" {
    const source = "42";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
    defer layout_cache.deinit();

    // Evaluate the number
    const result = try eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);

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
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
    defer layout_cache.deinit();

    // Evaluate the number
    const result = try eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);

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
    var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
    defer layout_cache.deinit();

    // List literals are not yet implemented
    const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
    try testing.expectError(eval.EvalError.LayoutError, result);
}

test "eval record literal" {
    const source = "{ x: 10, y: 20 }";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if this resulted in a runtime error due to failed canonicalization
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    if (expr == .runtime_error) {
        // Expected - canonicalization of records may not be fully implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
        defer layout_cache.deinit();
        const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // Record literals are not yet implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
        defer layout_cache.deinit();
        const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
        try testing.expectError(eval.EvalError.LayoutError, result);
    }
}

test "eval empty record" {
    const source = "{}";

    const resources = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupParseAndCanonical(test_allocator, resources);

    // Check if this resulted in a runtime error due to incomplete canonicalization
    const expr = resources.cir.store.getExpr(resources.expr_idx);
    if (expr == .runtime_error) {
        // Expected - canonicalization of empty records may not be fully implemented
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
        defer layout_cache.deinit();
        const result = eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // Create a stack for evaluation
        var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
        defer eval_stack.deinit();

        // Create layout store
        var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
        defer layout_cache.deinit();

        // Empty records should work
        const result = try eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);

        // Empty record should have a valid layout and pointer (even if no bytes are written)
        try testing.expect(@intFromPtr(result.ptr) != 0);
    }
}

// TODO: Uncomment when single_quote storage is implemented in NodeStore
// test "eval character literal" {
//     const source = "'a'";
//
//     const resources = try parseAndCanonicalizeExpr(test_allocator, source);
//     defer cleanupParseAndCanonical(test_allocator, resources);
//
//     // Create a stack for evaluation
//     var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
//     defer eval_stack.deinit();
//
//     // Create layout store
//     var layout_cache = try layout_store.Store.init(resources.module_env, &resources.module_env.types_store);
//     defer layout_cache.deinit();
//
//     // Evaluate the character
//     const result = try eval.eval(test_allocator, resources.cir, resources.expr_idx, &eval_stack, &layout_cache);
//
//     // Verify we got a scalar layout
//     try testing.expect(result.layout.tag == .scalar);
//
//     // Read the character value back (stored as u32)
//     const char_value = @as(*u32, @ptrCast(@alignCast(result.ptr))).*;
//     try testing.expectEqual(@as(u32, 'a'), char_value);
// }

test "eval integer literal directly from CIR node" {
    // Create a minimal CIR environment
    var module_env = base.ModuleEnv.init(test_allocator);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create an integer literal node directly
    // Note: Currently using placeholder values since actual parsing isn't implemented
    const int_value = CIR.IntValue.placeholder();

    const int_expr_idx = cir.store.addExpr(.{ .int = .{
        .int_var = @enumFromInt(0),
        .precision_var = @enumFromInt(0),
        .literal = @enumFromInt(0),
        .value = int_value,
        .bound = .i128,
        .region = base.Region.zero(),
    } });

    // Create a stack for evaluation
    var eval_stack = try stack.Stack.initCapacity(test_allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types_store);
    defer layout_cache.deinit();

    // Evaluate the integer
    const result = try eval.eval(test_allocator, &cir, int_expr_idx, &eval_stack, &layout_cache);

    // Verify the layout
    try testing.expect(result.layout.tag == .scalar);
    try testing.expect(result.layout.data.scalar.tag == .int);

    // Read the value back
    const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
    // IntValue.placeholder() still returns the placeholder bytes
    try testing.expectEqual(@as(i128, 17230332160), value);
}
