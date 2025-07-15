const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const eval = @import("./eval.zig");
const stack = @import("./stack.zig");
const layout_store = @import("../layout/store.zig");
const layout = @import("../layout/layout.zig");
const types = @import("../types.zig");
const CIR = @import("../check/canonicalize/CIR.zig");

test "eval comparison returning Bool" {
    const allocator = testing.allocator;
    const source = "5 > 3";

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

    // Type check
    var checker = try check_types.init(allocator, &module_env.types, &cir, &.{}, &cir.store.regions);
    defer checker.deinit();
    _ = try checker.checkExpr(canonical_expr_idx);

    // WORKAROUND: Set the type of the comparison manually
    const comp_expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    const bool_content = try module_env.types.mkBool(allocator, &module_env.idents, @enumFromInt(0));
    try module_env.types.setVarContent(comp_expr_var, bool_content);

    // Create stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Evaluate
    const result = try eval.eval(allocator, &cir, canonical_expr_idx, &eval_stack, &layout_cache, &module_env.types);

    // The result should be a boolean scalar with value true (5 > 3)
    try testing.expectEqual(layout.LayoutTag.scalar, result.layout.tag);
    try testing.expectEqual(layout.ScalarTag.bool, result.layout.data.scalar.tag);

    // Read the boolean value
    const bool_ptr = @as(*const bool, @ptrCast(@alignCast(result.ptr)));
    try testing.expectEqual(true, bool_ptr.*);
}
