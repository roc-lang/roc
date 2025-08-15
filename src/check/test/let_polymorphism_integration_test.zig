//! Integration tests for let-polymorphism that parse and canonicalize
//! actual Roc code to ensure polymorphic values work correctly in practice.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const Can = can.Can;
const ModuleEnv = can.ModuleEnv;
const CanonicalizedExpr = can.Can.CanonicalizedExpr;
const testing = std.testing;
const test_allocator = testing.allocator;

/// Helper to run parsing, canonicalization, and type checking on an expression
fn typeCheckExpr(allocator: std.mem.Allocator, source: []const u8) !struct {
    module_env: *ModuleEnv,
    parse_ast: *parse.AST,
    cir: *ModuleEnv,
    can: *Can,
    checker: *Check,
    has_type_errors: bool,
} {
    // Set up module environment
    const module_env = try allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(allocator, source);

    // Parse
    const parse_ast = try allocator.create(parse.AST);
    parse_ast.* = try parse.parseExpr(&module_env.common, module_env.gpa);

    // Check for parse errors
    if (parse_ast.hasErrors()) {
        return .{
            .module_env = module_env,
            .parse_ast = parse_ast,
            .cir = undefined,
            .can = undefined,
            .checker = undefined,
            .has_type_errors = true, // Consider parse errors as errors
        };
    }

    // Canonicalize - CIR is an alias for ModuleEnv, so we use the same module_env
    const cir = module_env;

    const czer = try allocator.create(Can);
    czer.* = try Can.init(cir, parse_ast, null);

    // Run canonicalization - for expressions
    var canon_expr_idx: ?CanonicalizedExpr = null;
    if (parse_ast.root_node_idx != 0) {
        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
        canon_expr_idx = try czer.canonicalizeExpr(expr_idx);
    }

    // Type check - continue even if there are parse errors
    const checker = try allocator.create(Check);
    const empty_modules: []const *ModuleEnv = &.{};

    checker.* = try Check.init(allocator, &module_env.types, cir, empty_modules, &cir.store.regions);

    // For expressions, check the expression directly
    if (canon_expr_idx) |expr_idx| {
        _ = try checker.checkExpr(expr_idx.get_idx());
    }

    // Check if there are any type errors
    const has_type_errors = checker.problems.problems.items.len > 0;

    return .{
        .module_env = module_env,
        .parse_ast = parse_ast,
        .cir = cir,
        .can = czer,
        .checker = checker,
        .has_type_errors = has_type_errors,
    };
}

fn cleanup(result: anytype, allocator: std.mem.Allocator) void {
    result.checker.deinit();
    allocator.destroy(result.checker);
    result.can.deinit();
    allocator.destroy(result.can);
    result.parse_ast.deinit(allocator);
    allocator.destroy(result.parse_ast);
    result.module_env.deinit();
    allocator.destroy(result.module_env);
}

test "direct polymorphic identity usage" {
    const source =
        \\{
        \\    id = |x| x
        \\    a = id(1)
        \\    b = id("x")
        \\    { a, b }
        \\}
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    try testing.expect(!result.has_type_errors);
}

test "higher-order function with polymorphic identity" {
    const source =
        \\{
        \\    id = |x| x
        \\    f = |g, v| g(v)
        \\    a = f(id, 1)
        \\    b = f(id, "x")
        \\    { a, b }
        \\}
    ;

    const result = try typeCheckExpr(test_allocator, source);
    defer cleanup(result, test_allocator);

    try testing.expect(!result.has_type_errors);
}