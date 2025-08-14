//! Test to verify heap allocation doesn't affect type checking

const std = @import("std");
const base = @import("base");
const types = @import("types");
const parse = @import("parse");
const can = @import("can");
const Check = @import("../Check.zig");

const testing = std.testing;
const test_allocator = testing.allocator;
const ModuleEnv = can.ModuleEnv;
const Can = can.Can;

test "apply used twice - production test" {
    const source =
        \\{
        \\    id = |x| x
        \\    app = |f, v| f(v)
        \\    a = app(id, 1)
        \\    b = app(id, "x")
        \\    { a, b }
        \\}
    ;

    // Try stack allocating ModuleEnv
    var module_env = try ModuleEnv.init(test_allocator, source);
    defer module_env.deinit();

    // Try stack allocating parse_ast and czer
    var parse_ast = try parse.parseExpr(&module_env.common, module_env.gpa);
    defer parse_ast.deinit(test_allocator);

    parse_ast.store.emptyScratch();
    try module_env.initCIRFields(test_allocator, "test");

    var czer = try Can.init(&module_env, &parse_ast, null);
    defer czer.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    // std.debug.print("\nCanonicalizing expr {}\n", .{@intFromEnum(expr_idx)});
    const canon_result = try czer.canonicalizeExpr(expr_idx);
    const canon_expr = canon_result orelse return error.CanonicalizeError;
    // std.debug.print("Canonicalized to expr {}\n", .{@intFromEnum(canon_expr.get_idx())});

    // Try creating checker on stack instead of heap
    const empty_modules: []const *ModuleEnv = &.{};

    // std.debug.print("Heap test - module_env pointer: {*}, types: {*}\n", .{&module_env, &module_env.types});

    var checker = try Check.init(test_allocator, &module_env.types, &module_env, empty_modules, &module_env.store.regions);
    defer checker.deinit();

    // Debug: Check the initial rank
    // std.debug.print("\nInitial rank before checking: {}\n", .{checker.current_rank});

    _ = try checker.checkExpr(canon_expr.get_idx());

    // Debug: Check the final rank
    // std.debug.print("Final rank after checking: {}\n", .{checker.current_rank});

    // if (checker.problems.problems.items.len > 0) {
    //     std.debug.print("\nTest has {} type errors\n", .{checker.problems.problems.items.len});
    //     for (checker.problems.problems.items, 0..) |problem, i| {
    //         std.debug.print("  Error {}: ", .{i});
    //         switch (problem) {
    //             .type_mismatch => |tm| {
    //                 const expected_resolved = module_env.types.resolveVar(tm.types.expected_var);
    //                 const actual_resolved = module_env.types.resolveVar(tm.types.actual_var);
    //                 std.debug.print("Type mismatch: expected var {} (content={}) vs actual var {} (content={})\n", .{
    //                     @intFromEnum(tm.types.expected_var),
    //                     expected_resolved.desc.content,
    //                     @intFromEnum(tm.types.actual_var),
    //                     actual_resolved.desc.content,
    //                 });
    //             },
    //             else => std.debug.print("{any}\n", .{problem}),
    //         }
    //     }
    // }

    // This should pass
    try testing.expect(checker.problems.problems.items.len == 0);
}
