//! Trace test for the apply pattern issue

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const testing = std.testing;
const test_allocator = testing.allocator;
const ModuleEnv = can.ModuleEnv;

test "minimal apply pattern trace" {
    // Minimal failing case
    const source =
        \\{
        \\    id = |x| x
        \\    f = |g, v| g(v)
        \\    a = f(id, 1)
        \\    a
        \\}
    ;

    // std.debug.print("\n=== MINIMAL APPLY TRACE ===\n", .{});

    var module_env = try ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();

    var parse_ast = try parse.parseExpr(&module_env.common, test_allocator);
    defer parse_ast.deinit(test_allocator);

    parse_ast.store.emptyScratch();
    try module_env.initCIRFields(test_allocator, "test");

    var czer = try can.Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_expr = try czer.canonicalizeExpr(expr_idx);

    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    if (canon_expr) |expr| {
        _ = try checker.checkExpr(expr.get_idx());
    }

    // std.debug.print("Problems: {}\n", .{checker.problems.problems.items.len});
    // for (checker.problems.problems.items, 0..) |problem, i| {
    //     std.debug.print("  Problem {}: ", .{i});
    //     switch (problem) {
    //         .type_mismatch => |tm| {
    //             std.debug.print("Type mismatch: expected var {} actual var {}\n", .{
    //                 @intFromEnum(tm.types.expected_var),
    //                 @intFromEnum(tm.types.actual_var),
    //             });
    //         },
    //         else => std.debug.print("{any}\n", .{problem}),
    //     }
    // }

    // This should pass
    try testing.expect(checker.problems.problems.items.len == 0);
}

test "double apply pattern trace" {
    // The actual failing case
    const source =
        \\{
        \\    id = |x| x
        \\    f = |g, v| g(v)
        \\    a = f(id, 1)
        \\    b = f(id, "x")
        \\    { a, b }
        \\}
    ;

    // std.debug.print("\n=== DOUBLE APPLY TRACE ===\n", .{});

    var module_env = try ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();

    var parse_ast = try parse.parseExpr(&module_env.common, test_allocator);
    defer parse_ast.deinit(test_allocator);

    parse_ast.store.emptyScratch();
    try module_env.initCIRFields(test_allocator, "test");

    var czer = try can.Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canon_expr = try czer.canonicalizeExpr(expr_idx);

    var checker = try Check.init(test_allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer checker.deinit();

    if (canon_expr) |expr| {
        _ = try checker.checkExpr(expr.get_idx());
    }

    // Debug output to see what's failing
    std.debug.print("Problems found: {}\n", .{checker.problems.problems.items.len});
    for (checker.problems.problems.items, 0..) |problem, i| {
        std.debug.print("  Problem {}: {any}\n", .{ i, problem });
    }

    try testing.expect(checker.problems.problems.items.len == 0);
}
