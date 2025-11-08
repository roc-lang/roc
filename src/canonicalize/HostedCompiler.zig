const std = @import("std");
const base = @import("base");
const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");

/// Replace all e_anno_only expressions in a Type Module with e_hosted_lambda operations (in-place).
/// This transforms standalone annotations into hosted lambda operations that will be
/// provided by the host application at runtime.
/// Returns a list of def indices that were modified.
pub fn replaceAnnoOnlyWithHosted(env: *ModuleEnv) !std.ArrayList(CIR.Def.Idx) {
    const gpa = env.gpa;
    var modified_def_indices = std.ArrayList(CIR.Def.Idx).empty;

    std.debug.print("=== REPLACE ANNO ONLY WITH HOSTED START ===\n", .{});

    // Ensure types array has entries for all existing nodes
    // This is necessary because varFrom(node_idx) assumes type_var index == node index
    const current_nodes = env.store.nodes.len();
    const current_types = env.types.len();
    std.debug.print("DEBUG replaceAnnoOnlyWithHosted: current_nodes={}, current_types={}\n", .{current_nodes, current_types});
    if (current_types < current_nodes) {
        // Fill the gap with fresh type variables
        var i: u64 = current_types;
        while (i < current_nodes) : (i += 1) {
            _ = env.types.fresh() catch unreachable;
        }
        std.debug.print("DEBUG replaceAnnoOnlyWithHosted: filled types array to {}\n", .{env.types.len()});
    }

    // Iterate through all defs and replace ALL anno-only defs with hosted implementations
    const all_defs = env.store.sliceDefs(env.all_defs);
    for (all_defs) |def_idx| {
        const def = env.store.getDef(def_idx);
        const expr = env.store.getExpr(def.expr);

        // Check if this is an anno-only def (e_anno_only expression)
        if (expr == .e_anno_only and def.annotation != null) {
            // Get the identifier from the pattern
            const pattern = env.store.getPattern(def.pattern);
            if (pattern == .assign) {
                const ident = pattern.assign.ident;

                // Create a dummy parameter pattern for the lambda
                // Use the identifier "_arg" for the parameter
                const arg_ident = env.common.findIdent("_arg") orelse try env.common.insertIdent(gpa, base.Ident.for_text("_arg"));
                const arg_pattern_idx = try env.addPattern(.{ .assign = .{ .ident = arg_ident } }, base.Region.zero());

                // Create a pattern span containing just this one parameter
                const patterns_start = env.store.scratchTop("patterns");
                try env.store.scratch.?.patterns.append(arg_pattern_idx);
                const args_span = CIR.Pattern.Span{ .span = .{ .start = @intCast(patterns_start), .len = 1 } };

                // Create an e_runtime_error body that crashes when the function is called in the interpreter
                const error_msg_lit = try env.insertString("Hosted functions cannot be called in the interpreter");
                const diagnostic_idx = try env.addDiagnostic(.{ .not_implemented = .{
                    .feature = error_msg_lit,
                    .region = base.Region.zero(),
                } });
                const body_idx = try env.addExpr(.{ .e_runtime_error = .{ .diagnostic = diagnostic_idx } }, base.Region.zero());

                // Ensure types array has entries for all new expressions
                const body_int = @intFromEnum(body_idx);
                while (env.types.len() <= body_int) {
                    _ = try env.types.fresh();
                }

                // Create e_hosted_lambda expression
                const expr_idx = try env.addExpr(.{ .e_hosted_lambda = .{
                    .symbol_name = ident,
                    .index = 0,  // Placeholder; will be assigned during sorting pass
                    .args = args_span,
                    .body = body_idx,
                } }, base.Region.zero());

                // Ensure types array has an entry for this new expression
                const expr_int = @intFromEnum(expr_idx);
                while (env.types.len() <= expr_int) {
                    _ = try env.types.fresh();
                }

                // Now replace the e_anno_only expression with the e_hosted_lambda
                // We need to modify the def's expr field in extra_data (NOT data_2!)
                // The expr is stored in extra_data[extra_start + 1]
                const def_node_idx = @as(@TypeOf(env.store.nodes).Idx, @enumFromInt(@intFromEnum(def_idx)));
                const def_node = env.store.nodes.get(def_node_idx);
                const extra_start = def_node.data_1;

                const old_expr = env.store.extra_data.items.items[extra_start + 1];
                env.store.extra_data.items.items[extra_start + 1] = @intFromEnum(expr_idx);

                std.debug.print("DEBUG HostedCompiler: Updated def_idx={}, old_expr={}, new_expr={}\n", .{def_idx, old_expr, @intFromEnum(expr_idx)});

                // Verify the update by reading it back via getDef
                const verify_def = env.store.getDef(def_idx);
                const verify_expr = env.store.getExpr(verify_def.expr);
                std.debug.print("DEBUG HostedCompiler: Verification - def_idx={}, expr type={s}\n", .{def_idx, @tagName(verify_expr)});

                // Track this modified def index
                try modified_def_indices.append(gpa, def_idx);
            }
        }
    }

    return modified_def_indices;
}

/// Information about a hosted function for sorting and indexing
pub const HostedFunctionInfo = struct {
    symbol_name: base.Ident.Idx,
    expr_idx: CIR.Expr.Idx,
    name_text: []const u8,  // For sorting
};

/// Collect all hosted functions from the module (transitively through imports)
/// and sort them alphabetically by fully-qualified name (with `!` stripped).
pub fn collectAndSortHostedFunctions(env: *ModuleEnv) !std.ArrayList(HostedFunctionInfo) {
    var hosted_fns = std.ArrayList(HostedFunctionInfo).empty;

    // Iterate through all defs to find e_hosted_lambda expressions
    const all_defs = env.store.sliceDefs(env.all_defs);
    for (all_defs) |def_idx| {
        const def = env.store.getDef(def_idx);
        const expr = env.store.getExpr(def.expr);

        if (expr == .e_hosted_lambda) {
            const hosted = expr.e_hosted_lambda;
            const local_name = env.getIdent(hosted.symbol_name);

            // Build fully-qualified name: "ModuleName.functionName"
            // Strip the .roc extension from module name (e.g., "Stdout.roc" -> "Stdout")
            var module_name = env.module_name;
            if (std.mem.endsWith(u8, module_name, ".roc")) {
                module_name = module_name[0 .. module_name.len - 4];
            }
            const qualified_name = try std.fmt.allocPrint(env.gpa, "{s}.{s}", .{module_name, local_name});
            defer env.gpa.free(qualified_name);

            // Strip the `!` suffix for sorting (e.g., "Stdout.line!" -> "Stdout.line")
            const stripped_name = if (std.mem.endsWith(u8, qualified_name, "!"))
                qualified_name[0 .. qualified_name.len - 1]
            else
                qualified_name;

            // Allocate a copy for storage
            const name_copy = try env.gpa.dupe(u8, stripped_name);

            std.debug.print("DEBUG collectAndSortHostedFunctions: module={s}, local={s}, qualified={s}, stripped={s}\n", .{module_name, local_name, qualified_name, stripped_name});

            try hosted_fns.append(env.gpa, .{
                .symbol_name = hosted.symbol_name,
                .expr_idx = def.expr,
                .name_text = name_copy,
            });
        }
    }

    // Sort alphabetically by stripped qualified name
    const SortContext = struct {
        pub fn lessThan(_: void, a: HostedFunctionInfo, b: HostedFunctionInfo) bool {
            return std.mem.order(u8, a.name_text, b.name_text) == .lt;
        }
    };
    std.mem.sort(HostedFunctionInfo, hosted_fns.items, {}, SortContext.lessThan);

    std.debug.print("DEBUG collectAndSortHostedFunctions: Sorted {} functions:\n", .{hosted_fns.items.len});
    for (hosted_fns.items, 0..) |fn_info, i| {
        std.debug.print("  [{d}] {s}\n", .{i, fn_info.name_text});
    }

    return hosted_fns;
}

/// Assign indices to e_hosted_lambda expressions based on sorted order
pub fn assignHostedIndices(env: *ModuleEnv, sorted_fns: []const HostedFunctionInfo) !void {
    for (sorted_fns, 0..) |fn_info, index| {
        // Get the expression node (Expr.Idx and Node.Idx have same underlying representation)
        const expr_node_idx = @as(@TypeOf(env.store.nodes).Idx, @enumFromInt(@intFromEnum(fn_info.expr_idx)));
        var expr_node = env.store.nodes.get(expr_node_idx);

        // For e_hosted_lambda nodes:
        // data_1 = symbol_name (Ident.Idx via @bitCast)
        // data_2 = index (u32) <- We set this here
        // data_3 = extra_data pointer (for args and body)
        expr_node.data_2 = @intCast(index);

        env.store.nodes.set(expr_node_idx, expr_node);
    }
}
