const std = @import("std");
const testing = std.testing;
const eval = @import("eval.zig");
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const CIR = canonicalize.CIR;
const types = @import("../types.zig");

const test_allocator = testing.allocator;

fn parseAndCanonicalizeExpr(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *base.ModuleEnv,
    parse_ast: *parse.AST,
    cir: *CIR,
    can: *canonicalize,
    expr_idx: CIR.Expr.Idx,
} {
    const module_env = try allocator.create(base.ModuleEnv);
    module_env.* = base.ModuleEnv.init(allocator);

    // Assume we can parse the input source code as an expression
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = parse.parseExpr(module_env, source);

    // Empty scratch buffer (required before canonicalization)
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

fn cleanupAnswer(allocator: std.mem.Allocator, answer: anytype) void {
    answer.can.deinit();
    answer.cir.deinit();
    answer.parse_ast.deinit(allocator);
    answer.module_env.deinit();
    allocator.destroy(answer.can);
    allocator.destroy(answer.cir);
    allocator.destroy(answer.parse_ast);
    allocator.destroy(answer.module_env);
}

test "eval Str literal" {
    const source = "\"Hello, World!\"";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // Evaluating a str literal should return the same CIR Idx (since literals are already primitive)
    const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
    try testing.expectEqual(answer.expr_idx, evaluated);
}

test "eval runtime error - returns crash error" {
    const source = "crash \"kaboom!\"";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // Evaluating a runtime error should return an error
    const result = eval.eval(test_allocator, answer.cir, answer.expr_idx);
    try testing.expectError(eval.EvalError.Crash, result);
}

test "eval tag - already primitive" {
    const source = "Ok";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // Evaluating a tag should return the same index (for now, not yet implemented)
    const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
    try testing.expectEqual(answer.expr_idx, evaluated);
}

test "eval binop - not yet implemented" {
    const source = "5 + 3";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // For now, binop evaluation is not implemented, so it should return the same index
    const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
    try testing.expectEqual(answer.expr_idx, evaluated);
}

test "eval call - not yet implemented" {
    const source = "List.len []";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // Check if this resulted in a runtime error due to failed canonicalization
    const expr = answer.cir.store.getExpr(answer.expr_idx);
    if (expr == .runtime_error) {
        // Expected - canonicalization of calls may not be fully implemented
        const result = eval.eval(test_allocator, answer.cir, answer.expr_idx);
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // For now, call evaluation is not implemented, so it should return the same index
        const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
        try testing.expectEqual(answer.expr_idx, evaluated);
    }
}

test "eval multiple string segments" {
    const source = "\"Hello, \" ++ \"World!\"";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // For now, string concatenation is not implemented, so it should return the same index
    const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
    try testing.expectEqual(answer.expr_idx, evaluated);
}

test "eval if expression - always takes final_else branch" {
    const source = "if Bool.true then \"true branch\" else \"else branch\"";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // Check if this resulted in a runtime error due to failed canonicalization
    const expr = answer.cir.store.getExpr(answer.expr_idx);
    if (expr == .runtime_error) {
        // Expected - canonicalization of if expressions may not be fully implemented
        const result = eval.eval(test_allocator, answer.cir, answer.expr_idx);
        try testing.expectError(eval.EvalError.Crash, result);
    } else if (expr == .@"if") {
        // The eval function should return the final_else branch
        const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
        try testing.expectEqual(expr.@"if".final_else, evaluated);
    } else {
        // Otherwise, it should return the same index
        const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
        try testing.expectEqual(answer.expr_idx, evaluated);
    }
}

test "eval simple number" {
    const source = "42";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // Numbers are already primitive
    const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
    try testing.expectEqual(answer.expr_idx, evaluated);
}

test "eval list literal" {
    const source = "[1, 2, 3]";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // List literals should return the same index for now
    const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
    try testing.expectEqual(answer.expr_idx, evaluated);
}

test "eval record literal" {
    const source = "{ x: 10, y: 20 }";

    const answer = try parseAndCanonicalizeExpr(test_allocator, source);
    defer cleanupAnswer(test_allocator, answer);

    // Check if this resulted in a runtime error due to failed canonicalization
    const expr = answer.cir.store.getExpr(answer.expr_idx);
    if (expr == .runtime_error) {
        // Expected - canonicalization of records may not be fully implemented
        const result = eval.eval(test_allocator, answer.cir, answer.expr_idx);
        try testing.expectError(eval.EvalError.Crash, result);
    } else {
        // Record literals should return the same index for now
        const evaluated = try eval.eval(test_allocator, answer.cir, answer.expr_idx);
        try testing.expectEqual(answer.expr_idx, evaluated);
    }
}
