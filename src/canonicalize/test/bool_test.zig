//! Tests for canonicalizing boolean expressions
const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types = @import("types");
const compile = @import("compile");
const parse = @import("parse");
const Can = @import("../Can.zig");

const ModuleEnv = compile.ModuleEnv;

test "canonicalize True as Bool" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    const source = "True";

    // Initialize ModuleEnv
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env);
    defer parse_ast.deinit(allocator);
    parse_ast.store.emptyScratch();

    // The tokenizer should have inserted the identifier "True" during parsing
    // No manual insertion needed anymore since we fixed the tokenizer to use a pointer

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(allocator, "test");

    // Canonicalize
    var can = try Can.init(&module_env, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    // Get the expression
    const expr = module_env.store.getExpr(canonical_expr_idx.get_idx());

    // Check if it's a nominal expression (Bool)
    try testing.expectEqual(.e_nominal, std.meta.activeTag(expr));

    // The backing expression should be a tag
    const backing_expr = module_env.store.getExpr(expr.e_nominal.backing_expr);
    try testing.expectEqual(.e_tag, std.meta.activeTag(backing_expr));
    try testing.expectEqual(ModuleEnv.Expr.NominalBackingType.tag, expr.e_nominal.backing_type);

    // The tag should be "True"
    const tag_name = module_env.getIdent(backing_expr.e_tag.name);
    try testing.expectEqualStrings("True", tag_name);
}

test "canonicalize False as Bool" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    const source = "False";

    // Initialize ModuleEnv
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env);
    defer parse_ast.deinit(allocator);
    parse_ast.store.emptyScratch();

    // The tokenizer should have inserted the identifier "False" during parsing
    // No manual insertion needed anymore since we fixed the tokenizer to use a pointer

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(allocator, "test");

    // Canonicalize
    var can = try Can.init(&module_env, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    // Get the expression
    const expr = module_env.store.getExpr(canonical_expr_idx.get_idx());

    // Check if it's a nominal expression (Bool)
    try testing.expectEqual(.e_nominal, std.meta.activeTag(expr));

    // The backing expression should be a tag
    const backing_expr = module_env.store.getExpr(expr.e_nominal.backing_expr);
    try testing.expectEqual(.e_tag, std.meta.activeTag(backing_expr));
    try testing.expectEqual(ModuleEnv.Expr.NominalBackingType.tag, expr.e_nominal.backing_type);

    // The tag should be "False"
    const tag_name = module_env.getIdent(backing_expr.e_tag.name);
    try testing.expectEqualStrings("False", tag_name);
}

test "canonicalize random tag not as Bool" {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer std.debug.assert(gpa_state.deinit() == .ok);
    const allocator = gpa_state.allocator();
    const source = "SomeTag";

    // Initialize ModuleEnv
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    // Parse
    var parse_ast = try parse.parseExpr(&module_env);
    defer parse_ast.deinit(allocator);
    parse_ast.store.emptyScratch();

    // The tokenizer should have inserted the identifier "SomeTag" during parsing
    // No manual insertion needed anymore since we fixed the tokenizer to use a pointer

    // Initialize CIR fields in ModuleEnv
    try module_env.initCIRFields(allocator, "test");

    // Canonicalize
    var can = try Can.init(&module_env, &parse_ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse unreachable;

    // Get the expression
    const expr = module_env.store.getExpr(canonical_expr_idx.get_idx());

    // Check that it's NOT a nominal expression - just a plain tag
    try testing.expectEqual(.e_tag, std.meta.activeTag(expr));

    // The tag should be "SomeTag"
    const tag_name = module_env.getIdent(expr.e_tag.name);
    try testing.expectEqualStrings("SomeTag", tag_name);
}
