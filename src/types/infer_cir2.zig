//! Type inference for CIR2 nodes
//! This module provides type inference capabilities for the new CIR2 AST representation.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const Allocator = std.mem.Allocator;
const store_mod = @import("store.zig");
const types_mod = @import("types.zig");
const Store = store_mod.Store;
const Var = types_mod.Var;
const Content = types_mod.Content;
// Unification would be handled by the check module when it calls this inference

/// Type inference context for CIR2
/// The CIR2 type is passed as anytype to avoid circular dependencies
pub fn InferContext(comptime CIR2: type) type {
    return struct {
        allocator: Allocator,
        store: *Store,
        cir: *const CIR2,
        idents: *base.Ident.Store,

        const Self = @This();

        /// Create a new inference context
        pub fn init(allocator: Allocator, store: *Store, cir: *const CIR2, idents: *base.Ident.Store) Self {
            return .{
                .allocator = allocator,
                .store = store,
                .cir = cir,
                .idents = idents,
            };
        }

        /// Infer the type of a CIR2 expression
        pub fn inferExpr(self: *Self, expr_idx: CIR2.Expr.Idx) !Var {
            const expr = self.cir.getExpr(expr_idx);

            // Handle the common cases first to avoid accessing union fields incorrectly
            switch (expr.tag) {
                .num_literal_i32 => {
                    // Create a number type
                    const var_id = try self.store.fresh();
                    const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
                    try self.store.setVarContent(var_id, num_content);
                    return var_id;
                },
                .int_literal_i32 => {
                    // Create an integer type
                    const var_id = try self.store.fresh();
                    const int_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
                    try self.store.setVarContent(var_id, int_content);
                    return var_id;
                },
                .num_literal_big => {
                    // Big number literal - unbounded numeric type
                    const var_id = try self.store.fresh();
                    const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 128 } } } };
                    try self.store.setVarContent(var_id, num_content);
                    return var_id;
                },
                .int_literal_big => {
                    // Big integer literal - i128 precision
                    const var_id = try self.store.fresh();
                    const int_content = Content{ .structure = .{ .num = .{ .int_precision = .i128 } } };
                    try self.store.setVarContent(var_id, int_content);
                    return var_id;
                },
                .str_literal_small => {
                    // Create a string type
                    const var_id = try self.store.fresh();
                    const str_content = Content{ .structure = .{ .str = {} } };
                    try self.store.setVarContent(var_id, str_content);
                    return var_id;
                },
                .str_literal_big => {
                    // Create a string type
                    const var_id = try self.store.fresh();
                    const str_content = Content{ .structure = .{ .str = {} } };
                    try self.store.setVarContent(var_id, str_content);
                    return var_id;
                },
                .frac_literal_small => {
                    // Create a fractional type
                    const var_id = try self.store.fresh();
                    const frac_content = Content{ .structure = .{ .num = .{ .frac_precision = .f64 } } };
                    try self.store.setVarContent(var_id, frac_content);
                    return var_id;
                },
                .frac_literal_big => {
                    // Create a fractional type
                    const var_id = try self.store.fresh();
                    const frac_content = Content{ .structure = .{ .num = .{ .frac_precision = .f64 } } };
                    try self.store.setVarContent(var_id, frac_content);
                    return var_id;
                },
                .lookup => {
                    // For lookups, the type variable should already exist at the node index
                    // It was created during canonicalization
                    const lookup_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

                    // Get the identifier from the original node
                    const node = self.cir.getNode(@enumFromInt(@intFromEnum(expr_idx)));
                    const ident_idx = node.payload.ident;

                    // Look up the definition in the symbol table
                    if (self.cir.scope_state.symbol_table.get(ident_idx)) |def_node_idx| {
                        // The definition's type variable should also exist
                        const def_var = @as(Var, @enumFromInt(@intFromEnum(def_node_idx)));

                        // Return the definition's type variable
                        // The lookup and definition share the same type
                        return def_var;
                    }

                    // If not found in symbol table, return the lookup's own variable
                    return lookup_var;
                },
                .block => {
                    // Type check all expressions in the block
                    // TODO: Fix union field access
                    return try self.store.fresh();
                    // DISABLED DUE TO UNION FIELD ACCESS ISSUES:
                    // // Type check all expressions in the block
                    // // Return the type of the last expression
                    // // Check if this is actually a block by verifying it has nodes payload
                    // // Some expressions might be incorrectly tagged as blocks
                    // const nodes_idx = expr.payload.nodes;
                    //                     // // Get the iterator for block nodes
                    // var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);
                    // var last_type: Var = try self.store.fresh();
                    //                     // while (iter.next()) |node_idx| {
                    // const e_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(node_idx)));
                    // last_type = try self.inferExpr(e_idx);
                    // }
                    //                     // return last_type;
                },
                .record_literal => {
                    // Create a record type
                    const var_id = try self.store.fresh();
                    const record_content = Content{ .structure = .empty_record };
                    try self.store.setVarContent(var_id, record_content);
                    return var_id;
                    // DISABLED DUE TO UNION FIELD ACCESS ISSUES:
                    // // Create a record type - infer field types from the literal
                    // const var_id = try self.store.fresh();
                    //                     // // Record literals have fields in their nodes payload
                    // const nodes_idx = expr.payload.nodes;
                    // if (!nodes_idx.isNil()) {
                    // var field_vars = std.ArrayList(types_mod.RecordField).init(self.allocator);
                    // defer field_vars.deinit();
                    //                     // var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);
                    // while (iter.next()) |field_node| {
                    // // Each field is typically a binop_colon with name:value
                    // const field_tag = self.cir.ast.*.tag(field_node);
                    // if (field_tag == .binop_colon) {
                    // const binop = self.cir.ast.*.node_slices.binOp(self.cir.ast.*.payload(field_node).binop);
                    // // Left is field name, right is value
                    // const value_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));
                    // const field_type = try self.inferExpr(value_expr_idx);
                    //                     // // Get field name from left side
                    // const name_node = self.cir.ast.*.nodes.get(@enumFromInt(@intFromEnum(binop.lhs)));
                    // if (name_node.tag == .lc) {
                    // try field_vars.append(.{
                    // .name = name_node.payload.ident,
                    // .var_ = field_type,
                    // });
                    // }
                    // }
                    // }
                    //                     // if (field_vars.items.len > 0) {
                    // const fields_range = try self.store.appendRecordFields(field_vars.items);
                    // const ext_var = try self.store.fresh(); // Extension variable for open records
                    // const record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = ext_var } } };
                    // try self.store.setVarContent(var_id, record_content);
                    // return var_id;
                    // }
                    // }
                    //                     // // Empty record or couldn't parse fields
                    // const record_content = Content{ .structure = .empty_record };
                    // try self.store.setVarContent(var_id, record_content);
                    // return var_id;
                },
                .list_literal => {
                    // Create a list type
                    const var_id = try self.store.fresh();
                    const elem_var = try self.store.fresh();
                    const list_content = Content{ .structure = .{ .list = elem_var } };
                    try self.store.setVarContent(var_id, list_content);
                    return var_id;
                    // DISABLED DUE TO UNION FIELD ACCESS ISSUES:
                    // // Create a list type - infer element type from list elements
                    // const var_id = try self.store.fresh();
                    //                     // // List literals have elements in their nodes payload
                    // const nodes_idx = expr.payload.nodes;
                    // var elem_type = try self.store.fresh();
                    //                     // if (!nodes_idx.isNil()) {
                    // var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);
                    // // Infer type of first element as the base element type
                    // if (iter.next()) |first_elem| {
                    // const first_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(first_elem)));
                    // elem_type = try self.inferExpr(first_expr_idx);
                    //                     // // Infer types of remaining elements
                    // // In a full implementation, these would be unified with elem_type
                    // while (iter.next()) |elem| {
                    // const elem_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(elem)));
                    // _ = try self.inferExpr(elem_expr_idx);
                    // }
                    // }
                    // }
                    //                     // const list_content = Content{
                    // .structure = .{
                    // .list = elem_type,
                    //     },
                    // };
                    // try self.store.setVarContent(var_id, list_content);
                    // return var_id;
                },
                .binop_plus, .binop_minus, .binop_star, .binop_slash => {
                    // Type check arithmetic operations
                    // Due to CIR construction, binops might not have the expected payload
                    // For now, just return a numeric type
                    const var_id = try self.store.fresh();
                    const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
                    try self.store.setVarContent(var_id, num_content);
                    return var_id;

                    // TODO: Fix this once CIR payload is corrected
                    // const binop = self.cir.getBinOp(CIR2.Expr.Idx, expr.payload.binop);
                    // const lhs_type = try self.inferExpr(binop.lhs);
                    // const rhs_type = try self.inferExpr(binop.rhs);

                    // // For arithmetic, use the left operand's type as the result type
                    // // Both operands should be numeric
                    // _ = rhs_type; // Right operand type is also inferred for completeness
                    // return lhs_type;
                },
                .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte => {
                    // Comparison operators return Bool
                    // For now, just return Bool
                    const bool_content = try self.store.mkBool(self.allocator, self.idents, null);
                    return try self.store.freshFromContent(bool_content);

                    // TODO: Fix this once CIR payload is corrected
                    // const binop = self.cir.getBinOp(CIR2.Expr.Idx, expr.payload.binop);
                    // _ = try self.inferExpr(binop.lhs);
                    // _ = try self.inferExpr(binop.rhs);
                },
                .binop_colon => {
                    // Type annotation - infer the value's type
                    // For now, just return a fresh type
                    return try self.store.fresh();

                    // TODO: Fix this once CIR payload is corrected
                    // const binop = self.cir.getBinOp(CIR2.Expr.Idx, expr.payload.binop);
                    // return try self.inferExpr(binop.lhs);
                },
                .binop_and, .binop_or => {
                    // Boolean operators
                    // For now, just return Bool
                    const bool_content = try self.store.mkBool(self.allocator, self.idents, null);
                    return try self.store.freshFromContent(bool_content);

                    // TODO: Fix this once CIR payload is corrected
                    // const binop = self.cir.getBinOp(CIR2.Expr.Idx, expr.payload.binop);
                    // _ = try self.inferExpr(binop.lhs);
                    // _ = try self.inferExpr(binop.rhs);
                },
                .if_else => {
                    // If expression
                    return try self.store.fresh();
                    // DISABLED DUE TO UNION FIELD ACCESS ISSUES:
                    // // If expression - infer types of condition and branches
                    // const if_branches_u32 = expr.payload.if_branches;
                    // // Convert u32 to NodeSlices.Idx - same pattern as in CIR2
                    // const AST = @TypeOf(self.cir.ast.*);
                    // const nodes_idx = @as(collections.NodeSlices(AST.Node.Idx).Idx, @enumFromInt(if_branches_u32));
                    // var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);
                    //                     // // First node is the condition
                    // if (iter.next()) |cond| {
                    // const cond_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(cond)));
                    // _ = try self.inferExpr(cond_expr_idx);
                    // }
                    //                     // // Next is the then branch
                    // var result_type = try self.store.fresh();
                    // if (iter.next()) |then_branch| {
                    // const then_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(then_branch)));
                    // result_type = try self.inferExpr(then_expr_idx);
                    // }
                    //                     // // Finally the else branch
                    // if (iter.next()) |else_branch| {
                    // const else_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(else_branch)));
                    // _ = try self.inferExpr(else_expr_idx);
                    // }
                    //                     // return result_type;
                },
                .lambda => {
                    // Lambda expression
                    return try self.store.fresh();
                    // DISABLED DUE TO UNION FIELD ACCESS ISSUES:
                    // // Lambda expression - create function type
                    // // Lambda structure: binop_pipe with params on left, body on right
                    // const binop_idx = expr.payload.binop;
                    // const binop = self.cir.ast.*.node_slices.binOp(binop_idx);
                    //                     // // Infer body type (right side)
                    // const body_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));
                    // const body_type = try self.inferExpr(body_expr_idx);
                    //                     // // Count and create type variables for parameters on left side
                    // var param_types = std.ArrayList(Var).init(self.allocator);
                    // defer param_types.deinit();
                    //                     // // Left side contains the parameters - for now just count them
                    // const countParams = struct {
                    // fn count(ctx: *@This(), allocator: Allocator, cir: anytype, node_idx: anytype) !void {
                    // const node = cir.ast.*.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
                    // switch (node.tag) {
                    // .underscore, .lc, .var_lc => {
                    // // Single parameter
                    // const param_var = try ctx.store.fresh();
                    // try ctx.param_list.append(param_var);
                    //             },
                    //             .binop_pipe => {
                    //                 // Multiple parameters
                    //                 const inner_binop = cir.ast.*.node_slices.binOp(node.payload.binop);
                    //                 try count(ctx, allocator, cir, inner_binop.lhs);
                    //                 try count(ctx, allocator, cir, inner_binop.rhs);
                    //             },
                    //             else => {
                    //                 // Other pattern types - create a type variable
                    //                 const param_var = try ctx.store.fresh();
                    //                 try ctx.param_list.append(param_var);
                    //             },
                    //         }
                    //     }
                    //
                    //     store: *Store,
                    //     param_list: *std.ArrayList(Var),
                    // };
                    //
                    // var counter = countParams{ .store = self.store, .param_list = &param_types };
                    // try counter.count(self.allocator, self.cir, binop.lhs);
                    //
                    // // Create function type with parameters
                    // const func_var = try self.store.fresh();
                    //
                    // if (param_types.items.len > 0) {
                    //     const params_range = try self.store.appendVars(param_types.items);
                    //     const func_content = Content{ .structure = .{ .fn_unbound = .{
                    //         .args = params_range,
                    //         .ret = body_type,
                    //         .needs_instantiation = false,
                    //     } } };
                    //     try self.store.setVarContent(func_var, func_content);
                    // } else {
                    //     const func_content = Content{ .structure = .{ .fn_unbound = .{
                    //         .args = types_mod.Var.SafeList.Range.empty(),
                    //         .ret = body_type,
                    //         .needs_instantiation = false,
                    //     } } };
                    //     try self.store.setVarContent(func_var, func_content);
                    // }
                    //
                    // return func_var;
                },
                .match => {
                    // Match/when expression
                    // Structure: scrutinee followed by pattern-body pairs
                    // Get the original AST node to access the correct payload
                    const node_idx = @as(@TypeOf(self.cir.ast.*).Node.Idx, @enumFromInt(@intFromEnum(expr_idx)));
                    const ast_node = self.cir.ast.*.nodes.get(@enumFromInt(@intFromEnum(node_idx)));

                    // Check if the original node has nodes payload
                    if (ast_node.tag != .match) {
                        // Node was mutated but payload doesn't match - return fresh type var
                        return try self.store.fresh();
                    }

                    const nodes_idx = ast_node.payload.nodes;

                    if (!nodes_idx.isNil()) {
                        var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);

                        // First element is the scrutinee (expression being matched)
                        if (iter.next()) |scrutinee_idx| {
                            const scrutinee_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(scrutinee_idx)));
                            _ = try self.inferExpr(scrutinee_expr_idx);
                        }

                        // Remaining nodes are pattern-body pairs
                        var branch_type: ?Var = null;

                        while (iter.next()) |pattern_idx| {
                            // Pattern node - skip it for type inference
                            _ = pattern_idx;

                            // Next node is the body expression for this branch
                            if (iter.next()) |body_idx| {
                                const body_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(body_idx)));
                                const this_branch_type = try self.inferExpr(body_expr_idx);

                                if (branch_type) |existing_type| {
                                    // All branches should have the same type
                                    _ = existing_type;
                                } else {
                                    branch_type = this_branch_type;
                                }
                            }
                        }

                        return branch_type orelse try self.store.fresh();
                    }

                    // Fallback if we can't parse the match structure
                    return try self.store.fresh();
                },
                .apply_ident => {
                    // Function application
                    // Structure: first node is function, second node is args (often a tuple)
                    const nodes_idx = expr.payload.nodes;

                    var arg_vars = std.ArrayList(Var).init(self.allocator);
                    defer arg_vars.deinit();

                    // Create a fresh type variable for the result
                    const result_var = try self.store.fresh();

                    if (!nodes_idx.isNil()) {
                        var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);

                        // First node is the function being called
                        if (iter.next()) |func_node| {
                            const func_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(func_node)));
                            const func_var = try self.inferExpr(func_expr_idx);

                            // Second node contains the arguments (could be a tuple or single expr)
                            if (iter.next()) |args_node| {
                                const args_tag = self.cir.ast.*.tag(args_node);
                                if (args_tag == .tuple_literal) {
                                    // Arguments are in a tuple - infer each element's type
                                    const tuple_nodes_idx = self.cir.ast.*.payload(args_node).nodes;
                                    if (!tuple_nodes_idx.isNil()) {
                                        var tuple_iter = self.cir.ast.*.node_slices.nodes(&tuple_nodes_idx);
                                        while (tuple_iter.next()) |arg| {
                                            const arg_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(arg)));
                                            const arg_var = try self.inferExpr(arg_expr_idx);
                                            try arg_vars.append(arg_var);
                                        }
                                    }
                                } else {
                                    // Single argument - infer its type directly
                                    const arg_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(args_node)));
                                    const arg_var = try self.inferExpr(arg_expr_idx);
                                    try arg_vars.append(arg_var);
                                }
                            }

                            // Create a function type that expects the argument types and returns the result type
                            const expected_func_content = try self.store.mkFuncUnbound(arg_vars.items, result_var);
                            const expected_func_var = try self.store.freshFromContent(expected_func_content);

                            // Type checking would unify the function with the expected type
                            _ = func_var;
                            _ = expected_func_var;
                        }
                    }

                    return result_var;
                },
                .apply_tag => {
                    // Tag constructor application
                    // This could be from .uc (simple tag) or .apply_uc (tag with args)
                    // We need to check the original AST node to determine which

                    const result_var = try self.store.fresh();
                    var arg_vars = std.ArrayList(Var).init(self.allocator);
                    defer arg_vars.deinit();

                    // Get the original AST node to check its type
                    const ast_node_idx = @as(@TypeOf(self.cir.ast.*).Node.Idx, @enumFromInt(@intFromEnum(expr_idx)));
                    const original_tag = self.cir.ast.*.tag(ast_node_idx);

                    if (original_tag == .apply_uc) {
                        // Tag application with arguments
                        const nodes_idx = expr.payload.nodes;
                        if (!nodes_idx.isNil()) {
                            var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);

                            // Skip the tag name (first node)
                            _ = iter.next();

                            // Process arguments if present
                            if (iter.next()) |args_node| {
                                const args_tag = self.cir.ast.*.tag(args_node);
                                if (args_tag == .tuple_literal) {
                                    // Arguments in a tuple
                                    const tuple_nodes = self.cir.ast.*.payload(args_node).nodes;
                                    if (!tuple_nodes.isNil()) {
                                        var tuple_iter = self.cir.ast.*.node_slices.nodes(&tuple_nodes);
                                        while (tuple_iter.next()) |arg| {
                                            const arg_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(arg)));
                                            const arg_var = try self.inferExpr(arg_expr_idx);
                                            try arg_vars.append(arg_var);
                                        }
                                    }
                                } else {
                                    // Single argument
                                    const arg_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(args_node)));
                                    const arg_var = try self.inferExpr(arg_expr_idx);
                                    try arg_vars.append(arg_var);
                                }
                            }
                        }
                    }
                    // else: simple tag from .uc, no arguments

                    if (arg_vars.items.len > 0) {
                        // Tag with payload - create a tag union type
                        // The tag union contains this specific tag variant with its payload types
                        // Create a single tag with the payload types
                        var tags = std.ArrayList(types_mod.Tag).init(self.allocator);
                        defer tags.deinit();

                        const args_range = try self.store.appendVars(arg_vars.items);
                        try tags.append(.{
                            .name = .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 }, // Placeholder tag name
                            .args = args_range,
                        });

                        const tags_range = try self.store.appendTags(tags.items);
                        const tag_content = Content{
                            .structure = .{
                                .tag_union = .{
                                    .tags = tags_range,
                                    .ext = try self.store.fresh(), // Extension variable for open tag unions
                                },
                            },
                        };
                        try self.store.setVarContent(result_var, tag_content);
                    } else {
                        // Tag without payload - simple tag union
                        var tags = std.ArrayList(types_mod.Tag).init(self.allocator);
                        defer tags.deinit();

                        try tags.append(.{
                            .name = .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 0 }, // Placeholder tag name
                            .args = types_mod.Var.SafeList.Range.empty(),
                        });

                        const tags_range = try self.store.appendTags(tags.items);
                        const tag_content = Content{ .structure = .{ .tag_union = .{
                            .tags = tags_range,
                            .ext = try self.store.fresh(),
                        } } };
                        try self.store.setVarContent(result_var, tag_content);
                    }

                    return result_var;
                },
                .unary_neg => {
                    // Unary negation
                    const var_id = try self.store.fresh();
                    const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = true, .bits_needed = 0 } } } };
                    try self.store.setVarContent(var_id, num_content);
                    return var_id;
                    // DISABLED DUE TO UNION FIELD ACCESS ISSUES:
                    // // Unary negation - operand must be numeric
                    // // Just return a numeric type for now
                    // const var_id = try self.store.fresh();
                    // const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = true, .bits_needed = 0 } } } };
                    // try self.store.setVarContent(var_id, num_content);
                    // return var_id;
                },
                .unary_not => {
                    // Unary not - result is Bool
                    const bool_content = try self.store.mkBool(self.allocator, self.idents, null);
                    return try self.store.freshFromContent(bool_content);
                    // DISABLED DUE TO UNION FIELD ACCESS ISSUES:
                    // // Unary not - operand must be Bool, result is Bool
                    // const nodes_idx = expr.payload.nodes;
                    // if (!nodes_idx.isNil()) {
                    // var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);
                    // if (iter.next()) |operand_node| {
                    // const operand_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(operand_node)));
                    // const operand_type = try self.inferExpr(operand_expr_idx);
                    // // Type checking would verify operand is Bool
                    // const bool_content = try self.store.mkBool(self.allocator, self.idents, null);
                    // const bool_var = try self.store.freshFromContent(bool_content);
                    // _ = operand_type;
                    // _ = bool_var;
                    // }
                    // }
                    // // Return Bool type
                    // const bool_content = try self.store.mkBool(self.allocator, self.idents, null);
                    // return try self.store.freshFromContent(bool_content);
                },
                .malformed => {
                    // Malformed expression - create error type
                    const var_id = try self.store.fresh();
                    const err_content = Content{ .err = {} };
                    try self.store.setVarContent(var_id, err_content);
                    return var_id;
                },
                else => {
                    // For unhandled cases, create an unbound type variable
                    return try self.store.fresh();
                },
            }
        }

        /// Infer the type of a CIR2 pattern
        pub fn inferPattern(self: *Self, patt_idx: CIR2.Patt.Idx) !Var {
            const patt = self.cir.getPatt(patt_idx);

            switch (patt.tag) {
                .identifier => {
                    // Simple identifier binding - create fresh type variable
                    return try self.store.fresh();
                },
                .underscore => {
                    // Underscore pattern - matches anything
                    return try self.store.fresh();
                },
                .num_literal => {
                    // Number pattern
                    const var_id = try self.store.fresh();
                    const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
                    try self.store.setVarContent(var_id, num_content);
                    return var_id;
                },
                .str_literal => {
                    // String pattern
                    const var_id = try self.store.fresh();
                    const str_content = Content{ .structure = .{ .str = {} } };
                    try self.store.setVarContent(var_id, str_content);
                    return var_id;
                },
                .list => {
                    // List pattern
                    const var_id = try self.store.fresh();
                    const elem_type = try self.store.fresh();
                    const list_content = Content{
                        .structure = .{
                            .list = elem_type,
                        },
                    };
                    try self.store.setVarContent(var_id, list_content);
                    return var_id;
                },
                .record => {
                    // Record pattern - infer field types from the payload
                    const var_id = try self.store.fresh();

                    // Record patterns have fields in their nodes payload
                    const nodes_idx = patt.payload.nodes;
                    if (!nodes_idx.isNil()) {
                        var field_vars = std.ArrayList(types_mod.RecordField).init(self.allocator);
                        defer field_vars.deinit();

                        var iter = self.cir.ast.*.node_slices.nodes(&nodes_idx);
                        while (iter.next()) |field_node| {
                            // Each field is typically a binop_colon with name:pattern
                            const field_tag = self.cir.ast.*.tag(field_node);
                            if (field_tag == .binop_colon) {
                                const binop = self.cir.ast.*.node_slices.binOp(self.cir.ast.*.payload(field_node).binop);
                                // Right side is the pattern for the field value
                                const value_patt_idx = @as(CIR2.Patt.Idx, @enumFromInt(@intFromEnum(binop.rhs)));
                                const field_type = try self.inferPatt(value_patt_idx);

                                // Get field name from left side
                                const name_node = self.cir.ast.*.nodes.get(@enumFromInt(@intFromEnum(binop.lhs)));
                                if (name_node.tag == .lc) {
                                    try field_vars.append(.{
                                        .name = name_node.payload.ident,
                                        .var_ = field_type,
                                    });
                                }
                            }
                        }

                        if (field_vars.items.len > 0) {
                            const fields_range = try self.store.appendRecordFields(field_vars.items);
                            const ext_var = try self.store.fresh(); // Extension variable for open records
                            const record_content = Content{ .structure = .{ .record = .{ .fields = fields_range, .ext = ext_var } } };
                            try self.store.setVarContent(var_id, record_content);
                            return var_id;
                        }
                    }

                    // Empty record or couldn't parse fields
                    const record_content = Content{ .structure = .empty_record };
                    try self.store.setVarContent(var_id, record_content);
                    return var_id;
                },
                else => {
                    // For unhandled pattern types, create fresh variable
                    return try self.store.fresh();
                },
            }
        }

        /// Infer the type of a CIR2 statement
        pub fn inferStmt(self: *InferContext, stmt_idx: CIR2.Stmt.Idx) !void {
            const stmt = self.cir.getStmt(stmt_idx);

            switch (stmt.tag) {
                .assign, .init_var => {
                    // Assignment or variable initialization
                    // These statements still have binop payload with pattern/expr structure
                    const ast_node_idx = @as(@TypeOf(self.cir.ast.*).Node.Idx, @enumFromInt(@intFromEnum(stmt_idx)));
                    const ast_node = self.cir.ast.*.nodes.get(@enumFromInt(@intFromEnum(ast_node_idx)));

                    if (ast_node.tag == .binop_equals) {
                        const binop = self.cir.ast.*.node_slices.binOp(ast_node.payload.binop);

                        // Left side is pattern, right side is expression
                        const patt_idx = @as(CIR2.Patt.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
                        const expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));

                        const patt_type = try self.inferPatt(patt_idx);
                        const expr_type = try self.inferExpr(expr_idx);

                        // Type checking would unify pattern and expression types
                        _ = patt_type;
                        _ = expr_type;
                    }
                },
                .expr => {
                    // Expression statement - the statement itself is the expression
                    // Convert statement index to expression index
                    const expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(stmt_idx)));
                    _ = try self.inferExpr(expr_idx);
                },
                .ret => {
                    // Return statement with expression
                    // Get the expression from nodes payload
                    const ast_node_idx = @as(@TypeOf(self.cir.ast.*).Node.Idx, @enumFromInt(@intFromEnum(stmt_idx)));
                    const ast_node = self.cir.ast.*.nodes.get(@enumFromInt(@intFromEnum(ast_node_idx)));
                    const nodes_iter = self.cir.ast.*.node_slices.nodes(&ast_node.payload.nodes);
                    var iter = nodes_iter;

                    if (iter.next()) |expr_node_idx| {
                        // Infer type of the return expression
                        const expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(expr_node_idx)));
                        _ = try self.inferExpr(expr_idx);
                        // The return statement's type is handled by the function's return type
                        // We don't need to unify here, just ensure the expression is typed
                    }
                },
                .crash => {
                    // Crash statement with expression
                    // Get the expression from nodes payload
                    const ast_node_idx = @as(@TypeOf(self.cir.ast.*).Node.Idx, @enumFromInt(@intFromEnum(stmt_idx)));
                    const ast_node = self.cir.ast.*.nodes.get(@enumFromInt(@intFromEnum(ast_node_idx)));
                    const nodes_iter = self.cir.ast.*.node_slices.nodes(&ast_node.payload.nodes);
                    var iter = nodes_iter;

                    if (iter.next()) |expr_node_idx| {
                        // Infer type of the crash expression (should be a string)
                        const expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(expr_node_idx)));
                        _ = try self.inferExpr(expr_idx);
                        // Crash expressions should be strings, but we'll let unification handle that
                    }
                },
                else => {
                    // Unhandled statement type
                },
            }
        }
    };
}

/// Batch type inference for all expressions and statements in the CIR
/// Iterates through all nodes and infers types for expressions and typed statements
pub fn inferAll(comptime CIR2: type) fn (allocator: Allocator, cir: *const CIR2, store: *Store, idents: *base.Ident.Store) anyerror!void {
    return struct {
        pub fn inferAllImpl(allocator: Allocator, cir: *const CIR2, store: *Store, idents: *base.Ident.Store) !void {
            // Create inference context
            var context = InferContext(CIR2){
                .allocator = allocator,
                .cir = cir,
                .store = store,
                .idents = idents,
            };

            // Iterate through all expressions in the CIR
            const expr_count = cir.exprs().len();
            var expr_idx_num: usize = 0;
            while (expr_idx_num < expr_count) : (expr_idx_num += 1) {
                const expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@as(i32, @intCast(expr_idx_num))));
                _ = try context.inferExpr(expr_idx);
            }

            // Iterate through all statements in the CIR
            const stmt_count = cir.stmts().len();
            var stmt_idx_num: usize = 0;
            while (stmt_idx_num < stmt_count) : (stmt_idx_num += 1) {
                const stmt_idx = @as(CIR2.Stmt.Idx, @enumFromInt(@as(i32, @intCast(stmt_idx_num))));
                try context.inferStmt(stmt_idx);
            }
        }
    }.inferAllImpl;
}
