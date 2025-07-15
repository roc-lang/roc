const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const parse = @import("../check/parse.zig");
const canonicalize = @import("../check/canonicalize.zig");
const check_types = @import("../check/check_types.zig");
const CIR = canonicalize.CIR;
const types = @import("../types.zig");
const eval = @import("eval.zig");
const stack = @import("stack.zig");
const layout_store = @import("../layout/store.zig");

test "minimal segfault reproduction - integer literal" {
    const allocator = testing.allocator;
    const source = "42";

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

    // Create stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Check the type before eval
    const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    const resolved = module_env.types.resolveVar(expr_var);
    std.debug.print("\n=== Before eval ===\n", .{});
    std.debug.print("expr_idx: {}, var_content: {}\n", .{ @intFromEnum(canonical_expr_idx), resolved.desc.content });

    // Try to evaluate
    const result = try eval.eval(allocator, &cir, canonical_expr_idx, &eval_stack, &layout_cache, &module_env.types);

    std.debug.print("=== After eval ===\n", .{});
    std.debug.print("result.layout: {}\n", .{result.layout});
    std.debug.print("result.ptr: {*}\n", .{result.ptr});

    // Try to read the value
    const value: i128 = switch (result.layout.data.scalar.data.int) {
        .i128 => @as(*i128, @ptrCast(@alignCast(result.ptr))).*,
        else => unreachable,
    };

    std.debug.print("value: {}\n", .{value});
    try testing.expectEqual(@as(i128, 42), value);
}

test "minimal segfault reproduction - if expression" {
    const allocator = testing.allocator;
    const source = "if 5 > 3 1 else 2";

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

    // WORKAROUND: Set the type of the if expression manually since type checker doesn't set it
    const if_expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    const if_expr = cir.store.getExpr(canonical_expr_idx);
    if (if_expr == .e_if) {
        // For if expressions, use the type of the then branch
        const branches = cir.store.sliceIfBranches(if_expr.e_if.branches);
        if (branches.len > 0) {
            const branch_node_idx = CIR.nodeIdxFrom(branches[0]);
            const first_branch = cir.store.nodes.get(branch_node_idx);
            const then_body_idx = first_branch.data_2;
            const then_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(then_body_idx));
            const then_var = @as(types.Var, @enumFromInt(@intFromEnum(then_expr_idx)));
            const then_resolved = module_env.types.resolveVar(then_var);
            try module_env.types.setVarContent(if_expr_var, then_resolved.desc.content);
        }
    }

    // Create stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Create layout store
    var layout_cache = try layout_store.Store.init(&module_env, &module_env.types);
    defer layout_cache.deinit();

    // Check the type before eval
    const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(canonical_expr_idx)));
    const resolved = module_env.types.resolveVar(expr_var);
    std.debug.print("\n=== Before eval if ===\n", .{});
    std.debug.print("expr_idx: {}, var_content: {}\n", .{ @intFromEnum(canonical_expr_idx), resolved.desc.content });

    // Check if expression structure
    const expr = cir.store.getExpr(canonical_expr_idx);
    if (expr == .e_if) {
        std.debug.print("If expression branches: {}\n", .{expr.e_if});

        // Check each branch
        const branches_span = expr.e_if.branches;
        const branches = cir.store.sliceIfBranches(branches_span);
        for (branches, 0..) |branch_idx, i| {
            std.debug.print("Branch {}: idx={}\n", .{ i, branch_idx });
        }
    }

    // Try to evaluate
    const result = try eval.eval(allocator, &cir, canonical_expr_idx, &eval_stack, &layout_cache, &module_env.types);

    std.debug.print("=== After eval if ===\n", .{});
    std.debug.print("result.layout: {}\n", .{result.layout});
    std.debug.print("result.ptr: {*}\n", .{result.ptr});

    // Read the result
    const value = @as(*i128, @ptrCast(@alignCast(result.ptr))).*;
    std.debug.print("Result value: {}\n", .{value});
    try testing.expectEqual(@as(i128, 1), value);
}

test "debug stack allocation" {
    const allocator = testing.allocator;

    // Create a simple stack
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    std.debug.print("\n=== Stack test ===\n", .{});
    std.debug.print("Stack initial state: start={*}, capacity={}\n", .{ eval_stack.start, eval_stack.capacity });

    // Try allocating some memory
    const ptr1 = try eval_stack.alloca(16, .@"8");
    std.debug.print("Allocated 16 bytes: {*}\n", .{ptr1});

    const ptr2 = try eval_stack.alloca(32, .@"16");
    std.debug.print("Allocated 32 bytes: {*}\n", .{ptr2});

    // Write and read back
    const test_ptr = @as(*i128, @ptrCast(@alignCast(ptr1)));
    test_ptr.* = 42;
    const read_value = test_ptr.*;
    std.debug.print("Write/read test: wrote 42, read {}\n", .{read_value});
    try testing.expectEqual(@as(i128, 42), read_value);
}
