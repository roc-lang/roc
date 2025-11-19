//! Tests to verify that builtin_compiler correctly transforms e_anno_only nodes to e_low_level_lambda nodes

const std = @import("std");
const compiled_builtins = @import("compiled_builtins");
const builtin_loading = @import("../builtin_loading.zig");
const can = @import("can");
const testing = std.testing;

test "builtin_compiler transforms all e_anno_only to e_low_level_lambda" {
    const gpa = testing.allocator;

    // Load the compiled builtin module
    var builtin_module = try builtin_loading.loadCompiledModule(
        gpa,
        compiled_builtins.builtin_bin,
        "Builtin",
        compiled_builtins.builtin_source,
    );
    defer builtin_module.deinit();

    // Count e_anno_only and e_low_level_lambda nodes by iterating through all definitions
    var anno_only_count: usize = 0;
    var low_level_lambda_count: usize = 0;

    // Iterate through all definitions in the builtin module
    const defs = builtin_module.env.store.sliceDefs(builtin_module.env.all_defs);

    // Also count total expressions by tag
    var total_exprs: usize = 0;
    var expr_type_counts = std.AutoHashMap(std.meta.Tag(can.CIR.Expr), usize).init(gpa);
    defer expr_type_counts.deinit();

    for (defs) |def_idx| {
        const def = builtin_module.env.store.getDef(def_idx);
        const expr = builtin_module.env.store.getExpr(def.expr);

        total_exprs += 1;
        const tag = @as(std.meta.Tag(can.CIR.Expr), expr);
        const count = expr_type_counts.get(tag) orelse 0;
        try expr_type_counts.put(tag, count + 1);

        switch (expr) {
            .e_anno_only => {
                anno_only_count += 1;
            },
            .e_low_level_lambda => low_level_lambda_count += 1,
            else => {},
        }
    }

    // After builtin_compiler runs, there should be:
    // - Very few e_anno_only nodes (only those not yet implemented like from_num_literal)
    // - Many e_low_level_lambda nodes (the transformed builtins)
    try testing.expect(anno_only_count < 20); // Allow some unimplemented operations
    try testing.expect(low_level_lambda_count > 100); // Should have transformed most operations

    // Only print if test would fail
    if (anno_only_count >= 20 or low_level_lambda_count <= 100) {
        std.debug.print("\n=== Builtin module stats ===\n", .{});
        std.debug.print("e_anno_only nodes: {} (expected < 20)\n", .{anno_only_count});
        std.debug.print("e_low_level_lambda nodes: {} (expected > 100)\n", .{low_level_lambda_count});
    }
}

test "builtin deserialization produces valid CIR" {
    const gpa = testing.allocator;

    // Deserialize builtin indices
    const builtin_indices = try builtin_loading.deserializeBuiltinIndices(
        gpa,
        compiled_builtins.builtin_indices_bin,
    );

    // Load the compiled builtin module
    var builtin_module = try builtin_loading.loadCompiledModule(
        gpa,
        compiled_builtins.builtin_bin,
        "Builtin",
        compiled_builtins.builtin_source,
    );
    defer builtin_module.deinit();

    // Verify basic structure exists
    try testing.expect(builtin_module.env.store.nodes.len() > 0);

    // Verify indices point to valid statements
    const bool_stmt = builtin_module.env.store.getStatement(builtin_indices.bool_type);
    try testing.expect(bool_stmt == .s_nominal_decl);

    const try_stmt = builtin_module.env.store.getStatement(builtin_indices.try_type);
    try testing.expect(try_stmt == .s_nominal_decl);

    const list_stmt = builtin_module.env.store.getStatement(builtin_indices.list_type);
    try testing.expect(list_stmt == .s_nominal_decl);

    std.debug.print("\nBuiltin indices verification:\n", .{});
    std.debug.print("  bool_type: {}\n", .{@intFromEnum(builtin_indices.bool_type)});
    std.debug.print("  try_type: {}\n", .{@intFromEnum(builtin_indices.try_type)});
    std.debug.print("  list_type: {}\n", .{@intFromEnum(builtin_indices.list_type)});
}
