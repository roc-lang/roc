//! Tests for canonicalizing boolean expressions
const std = @import("std");
const testing = std.testing;
const base = @import("base");
const parse = @import("./parse.zig");
const canonicalize = @import("./canonicalize.zig");
const types = @import("types");
const CIR = @import("./canonicalize/CIR.zig");

test "canonicalize True as Bool" {
    const allocator = testing.allocator;
    const source = "True";

    // Initialize ModuleEnv
    const owned_source = try allocator.dupe(u8, source);
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

    // Get the expression
    const expr = cir.store.getExpr(canonical_expr_idx);

    // Check if it's a nominal expression (Bool)
    try testing.expectEqual(.e_nominal, std.meta.activeTag(expr));

    // The backing expression should be a tag
    const backing_expr = cir.store.getExpr(expr.e_nominal.backing_expr);
    try testing.expectEqual(.e_tag, std.meta.activeTag(backing_expr));
    try testing.expectEqual(CIR.Expr.NominalBackingType.tag, expr.e_nominal.backing_type);

    // The tag should be "True"
    const tag_name = module_env.idents.getText(backing_expr.e_tag.name);
    try testing.expectEqualStrings("True", tag_name);
}

test "canonicalize False as Bool" {
    const allocator = testing.allocator;
    const source = "False";

    // Initialize ModuleEnv
    const owned_source = try allocator.dupe(u8, source);
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

    // Get the expression
    const expr = cir.store.getExpr(canonical_expr_idx);

    // Check if it's a nominal expression (Bool)
    try testing.expectEqual(.e_nominal, std.meta.activeTag(expr));

    // The backing expression should be a tag
    const backing_expr = cir.store.getExpr(expr.e_nominal.backing_expr);
    try testing.expectEqual(.e_tag, std.meta.activeTag(backing_expr));
    try testing.expectEqual(CIR.Expr.NominalBackingType.tag, expr.e_nominal.backing_type);

    // The tag should be "False"
    const tag_name = module_env.idents.getText(backing_expr.e_tag.name);
    try testing.expectEqualStrings("False", tag_name);
}

test "canonicalize random tag not as Bool" {
    const allocator = testing.allocator;
    const source = "SomeTag";

    // Initialize ModuleEnv
    const owned_source = try allocator.dupe(u8, source);
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

    // Get the expression
    const expr = cir.store.getExpr(canonical_expr_idx);

    // Check that it's NOT a nominal expression - just a plain tag
    try testing.expectEqual(.e_tag, std.meta.activeTag(expr));

    // The tag should be "SomeTag"
    const tag_name = module_env.idents.getText(expr.e_tag.name);
    try testing.expectEqualStrings("SomeTag", tag_name);
}
