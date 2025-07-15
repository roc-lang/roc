const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const CIR = canonicalize.CIR;
const types = @import("../types.zig");
const layout_store = @import("../layout/store.zig");

test "flex variable issue - numeric literal before type checking" {
    const allocator = testing.allocator;
    const source = "42";

    // Initialize ModuleEnv
    const owned_source = try allocator.dupe(u8, source);
    defer allocator.free(owned_source);
    var module_env = try base.ModuleEnv.init(allocator, owned_source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env, source);
    defer parse_ast.deinit(allocator);
    parse_ast.store.emptyScratch();

    // Create CIR
    var cir = try CIR.init(&module_env, "test");
    defer cir.deinit();

    // Canonicalize
    var can = try canonicalize.init(&cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    // Check the type variable BEFORE type checking
    const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    const resolved_before = module_env.types.resolveVar(expr_var);
    const var_content_before = resolved_before.desc.content;

    std.debug.print("\n=== BEFORE TYPE CHECKING ===\n", .{});
    std.debug.print("expr_var: {}\n", .{@intFromEnum(expr_var)});
    std.debug.print("var_content: {}\n", .{var_content_before});

    // Now run type checking
    var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
    defer checker.deinit();
    _ = try checker.checkExpr(canonical_expr_idx);

    // Check the type variable AFTER type checking
    const resolved_after = module_env.types.resolveVar(expr_var);
    const var_content_after = resolved_after.desc.content;
    std.debug.print("\n=== AFTER TYPE CHECKING ===\n", .{});
    std.debug.print("expr_var: {}\n", .{@intFromEnum(expr_var)});
    std.debug.print("var_content: {}\n", .{var_content_after});

    // Try to create layout store AFTER type checking
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // This should work now
    const layout_idx = try layout_cache.addTypeVar(expr_var);
    const expr_layout = layout_cache.getLayout(layout_idx);

    std.debug.print("\n=== LAYOUT ===\n", .{});
    std.debug.print("layout: {}\n", .{expr_layout});

    // Verify we got a numeric layout
    try testing.expect(expr_layout.tag == .scalar);
    try testing.expect(expr_layout.data.scalar.tag == .int);
}

test "flex variable issue - arithmetic expression" {
    const allocator = testing.allocator;
    const source = "5 + 3";

    // Initialize ModuleEnv
    const owned_source = try allocator.dupe(u8, source);
    defer allocator.free(owned_source);
    var module_env = try base.ModuleEnv.init(allocator, owned_source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env, source);
    defer parse_ast.deinit(allocator);
    parse_ast.store.emptyScratch();

    // Create CIR
    var cir = try CIR.init(&module_env, "test");
    defer cir.deinit();

    // Canonicalize
    var can = try canonicalize.init(&cir, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    // Check the type variable BEFORE type checking
    const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    const resolved_before = module_env.types.resolveVar(expr_var);
    const var_content_before = resolved_before.desc.content;

    std.debug.print("\n=== ARITHMETIC BEFORE TYPE CHECKING ===\n", .{});
    std.debug.print("expr_var: {}\n", .{@intFromEnum(expr_var)});
    std.debug.print("var_content: {}\n", .{var_content_before});

    // Also check the operands
    const binop_expr = cir.store.getExpr(canonical_expr_idx);
    if (binop_expr == .e_binop) {
        const left_var = @as(types.Var, @enumFromInt(@intFromEnum(binop_expr.e_binop.lhs)));
        const right_var = @as(types.Var, @enumFromInt(@intFromEnum(binop_expr.e_binop.rhs)));

        const left_resolved = module_env.types.resolveVar(left_var);
        const right_resolved = module_env.types.resolveVar(right_var);
        std.debug.print("left_var: {}, content: {}\n", .{ @intFromEnum(left_var), left_resolved.desc.content });
        std.debug.print("right_var: {}, content: {}\n", .{ @intFromEnum(right_var), right_resolved.desc.content });
    }

    // Now run type checking
    var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
    defer checker.deinit();
    _ = try checker.checkExpr(canonical_expr_idx);

    // Check the type variable AFTER type checking
    const resolved_after = module_env.types.resolveVar(expr_var);
    const var_content_after = resolved_after.desc.content;
    std.debug.print("\n=== ARITHMETIC AFTER TYPE CHECKING ===\n", .{});
    std.debug.print("expr_var: {}\n", .{@intFromEnum(expr_var)});
    std.debug.print("var_content: {}\n", .{var_content_after});

    // Try to create layout store AFTER type checking
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // This should work now
    const layout_idx = try layout_cache.addTypeVar(expr_var);
    const expr_layout = layout_cache.getLayout(layout_idx);

    std.debug.print("\n=== LAYOUT ===\n", .{});
    std.debug.print("layout: {}\n", .{expr_layout});

    // Verify we got a numeric layout
    try testing.expect(expr_layout.tag == .scalar);
    try testing.expect(expr_layout.data.scalar.tag == .int);
}
