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
            // The type variable for this expression should already exist at its index
            // It was created and populated during type checking
            const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

            // Ensure the variable exists in the store
            const var_idx = @intFromEnum(expr_var);
            const current_len = self.store.len();
            if (var_idx >= current_len) {
                // Variable doesn't exist - create fresh variables up to this index
                var i = current_len;
                while (i <= var_idx) : (i += 1) {
                    _ = try self.store.fresh();
                }
            }

            // Check if this variable has content - if it does, return it
            const resolved = self.store.resolveVar(expr_var);
            if (resolved.desc.content != .flex_var) {
                // Already has a type from type checking, return it
                return expr_var;
            }

            // If no type was assigned during type checking, try to infer one
            const expr = self.cir.getExpr(expr_idx);

            // Handle the common cases first to avoid accessing union fields incorrectly
            switch (expr.tag) {
                .num_literal_i32 => {
                    // Create a number type
                    const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
                    try self.store.setVarContent(expr_var, num_content);
                    return expr_var;
                },
                .int_literal_i32 => {
                    // Create an integer type
                    const int_content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
                    try self.store.setVarContent(expr_var, int_content);
                    return expr_var;
                },
                .num_literal_big => {
                    // Big number literal - unbounded numeric type
                    const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 128 } } } };
                    try self.store.setVarContent(expr_var, num_content);
                    return expr_var;
                },
                .int_literal_big => {
                    // Big integer literal - i128 precision
                    const int_content = Content{ .structure = .{ .num = .{ .int_precision = .i128 } } };
                    try self.store.setVarContent(expr_var, int_content);
                    return expr_var;
                },
                .str_literal_small => {
                    // Create a string type
                    const str_content = Content{ .structure = .{ .str = {} } };
                    try self.store.setVarContent(expr_var, str_content);
                    return expr_var;
                },
                .str_literal_big => {
                    // Create a string type
                    const str_content = Content{ .structure = .{ .str = {} } };
                    try self.store.setVarContent(expr_var, str_content);
                    return expr_var;
                },
                .frac_literal_small => {
                    // Create a fractional type
                    const frac_content = Content{ .structure = .{ .num = .{ .frac_precision = .f64 } } };
                    try self.store.setVarContent(expr_var, frac_content);
                    return expr_var;
                },
                .frac_literal_big => {
                    // Create a fractional type
                    const frac_content = Content{ .structure = .{ .num = .{ .frac_precision = .f64 } } };
                    try self.store.setVarContent(expr_var, frac_content);
                    return expr_var;
                },
                .lookup => {
                    // For lookups, the type should have been populated during type checking
                    // Just return the expression's variable
                    return expr_var;
                },
                .block => {
                    // Block type should have been populated during type checking
                    // If not populated, return the variable as-is
                    return expr_var;
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
                    // Record type should have been populated during type checking
                    // If not, return the variable as-is (it's a flex var)
                    return expr_var;
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
                    // List type should have been populated during type checking
                    // If not, create a list with fresh element type
                    const elem_var = try self.store.fresh();
                    const list_content = Content{ .structure = .{ .list = elem_var } };
                    try self.store.setVarContent(expr_var, list_content);
                    return expr_var;
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
                    // For arithmetic operations, we need to infer the types of both operands
                    const binop_idx = expr.payload.binop;
                    const binop = self.cir.ast.node_slices.binOp(&binop_idx);

                    // Infer types of left and right operands
                    const lhs_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
                    const rhs_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));

                    const lhs_type = try self.inferExpr(lhs_expr_idx);
                    const rhs_type = try self.inferExpr(rhs_expr_idx);

                    // Unify the numeric types - both operands must have compatible numeric types
                    const unified_type = try self.unifyNumericTypes(lhs_type, rhs_type);

                    // Set the result type to the unified numeric type
                    try self.store.setVarContent(expr_var, try self.store.getVarContent(unified_type));
                    return expr_var;
                },
                .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte => {
                    // Comparison operators return Bool but we need to check operand types
                    const binop_idx = expr.payload.binop;
                    const binop = self.cir.ast.node_slices.binOp(&binop_idx);

                    // Infer types of both operands to ensure they're compatible
                    const lhs_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
                    const rhs_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));

                    _ = try self.inferExpr(lhs_expr_idx);
                    _ = try self.inferExpr(rhs_expr_idx);

                    // All comparison operators return Bool
                    const bool_content = try self.store.mkBool(self.allocator, self.idents, null);
                    try self.store.setVarContent(expr_var, bool_content);
                    return expr_var;
                },
                .binop_colon => {
                    // Type annotation - infer the type of the left operand and ensure it matches annotation
                    const binop_idx = expr.payload.binop;
                    const binop = self.cir.ast.node_slices.binOp(&binop_idx);

                    // The left operand is the expression, the right is the type annotation
                    const lhs_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
                    const lhs_type = try self.inferExpr(lhs_expr_idx);

                    // Parse the type annotation from the right operand
                    const annotation_type = try self.parseTypeAnnotation(binop.rhs);

                    // Unify the inferred type with the annotation
                    // This ensures the expression matches its type annotation
                    try self.unifyTypes(lhs_type, annotation_type);

                    // Return the annotated type (which should now match the inferred type)
                    try self.store.setVarContent(expr_var, try self.store.getVarContent(annotation_type));
                    return expr_var;
                },
                .binop_and, .binop_or => {
                    // Boolean operators - ensure both operands are boolean
                    const binop_idx = expr.payload.binop;
                    const binop = self.cir.ast.node_slices.binOp(&binop_idx);

                    // Infer types of both operands - they should both be boolean
                    const lhs_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
                    const rhs_expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));

                    _ = try self.inferExpr(lhs_expr_idx);
                    _ = try self.inferExpr(rhs_expr_idx);

                    // Boolean operators return Bool
                    const bool_content = try self.store.mkBool(self.allocator, self.idents, null);
                    try self.store.setVarContent(expr_var, bool_content);
                    return expr_var;
                },
                .if_else => {
                    // If expression - should have been populated during type checking
                    return expr_var;
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
                    // Lambda expression - should have been populated during type checking
                    return expr_var;
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
                    // Match/when expression - should have been populated during type checking
                    return expr_var;
                },
                .apply_ident => {
                    // Function application - should have been populated during type checking
                    return expr_var;
                },
                .apply_tag => {
                    // Tag constructor application - should have been populated during type checking
                    return expr_var;
                },
                .unary_neg => {
                    // Unary negation - should have been populated during type checking
                    return expr_var;
                    // DISABLED DUE TO UNION FIELD ACCESS ISSUES:
                    // // Unary negation - operand must be numeric
                    // // Just return a numeric type for now
                    // const var_id = try self.store.fresh();
                    // const num_content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = true, .bits_needed = 0 } } } };
                    // try self.store.setVarContent(var_id, num_content);
                    // return var_id;
                },
                .unary_not => {
                    // Unary not - should have been populated during type checking
                    return expr_var;
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
                    const err_content = Content{ .err = {} };
                    try self.store.setVarContent(expr_var, err_content);
                    return expr_var;
                },
                else => {
                    // For unhandled cases, return the expression's variable
                    // It should have been populated during type checking
                    return expr_var;
                },
            }
        }

        /// Parse a type annotation from an AST node and return its type variable
        fn parseTypeAnnotation(self: *Self, node_idx: CIR2.Node.Idx) !Var {
            const node = self.cir.ast.getNode(@enumFromInt(@intFromEnum(node_idx)));

            switch (node.tag) {
                .uc => {
                    // Uppercase identifier - could be a type name
                    const ident_idx = node.payload.ident;
                    const type_name = self.idents.get(ident_idx);

                    // Map common type names to their type representations
                    if (std.mem.eql(u8, type_name, "I8")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .i8 } });
                    } else if (std.mem.eql(u8, type_name, "I16")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .i16 } });
                    } else if (std.mem.eql(u8, type_name, "I32")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .i32 } });
                    } else if (std.mem.eql(u8, type_name, "I64")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .i64 } });
                    } else if (std.mem.eql(u8, type_name, "I128")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .i128 } });
                    } else if (std.mem.eql(u8, type_name, "U8")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .u8 } });
                    } else if (std.mem.eql(u8, type_name, "U16")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .u16 } });
                    } else if (std.mem.eql(u8, type_name, "U32")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .u32 } });
                    } else if (std.mem.eql(u8, type_name, "U64")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .u64 } });
                    } else if (std.mem.eql(u8, type_name, "U128")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .int = .u128 } });
                    } else if (std.mem.eql(u8, type_name, "F32")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .frac = .f32 } });
                    } else if (std.mem.eql(u8, type_name, "F64")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .frac = .f64 } });
                    } else if (std.mem.eql(u8, type_name, "Dec")) {
                        return try self.store.freshFromContent(.{ .structure = .{ .frac = .dec } });
                    } else if (std.mem.eql(u8, type_name, "Bool")) {
                        return try self.store.mkBool(self.allocator, self.idents, null);
                    } else if (std.mem.eql(u8, type_name, "Str")) {
                        return try self.store.mkStr(self.allocator, self.idents, null);
                    } else if (std.mem.eql(u8, type_name, "List")) {
                        // List needs a type parameter - for now create unbound list
                        const elem_var = try self.store.fresh();
                        return try self.store.mkList(self.allocator, self.idents, elem_var, null);
                    } else {
                        // Unknown type - create a fresh variable for now
                        // In a complete implementation, this would look up user-defined types
                        return try self.store.fresh();
                    }
                },
                .lc => {
                    // Lowercase identifier - type variable
                    // Create a fresh type variable
                    return try self.store.fresh();
                },
                .binop_thin_arrow => {
                    // Function type annotation: arg -> ret
                    const binop_idx = node.payload.binop;
                    const binop = self.cir.ast.node_slices.binOp(&binop_idx);

                    // Parse argument and return types
                    const arg_type = try self.parseTypeAnnotation(binop.lhs);
                    const ret_type = try self.parseTypeAnnotation(binop.rhs);

                    // Create function type
                    const func_var = try self.store.fresh();
                    const args_range = try self.store.appendVars(&[_]Var{arg_type});
                    const func_content = Content{ .structure = .{ .fn_unbound = .{
                        .args = args_range,
                        .ret = ret_type,
                        .needs_instantiation = false,
                    } } };
                    try self.store.setVarContent(func_var, func_content);

                    return func_var;
                },
                .apply_uc => {
                    // Type application like List a
                    // For now, just parse as the base type
                    const nodes_idx = node.payload.nodes;
                    var nodes_iter = self.cir.ast.node_slices.nodes(&nodes_idx);

                    if (nodes_iter.next()) |type_constructor_idx| {
                        return try self.parseTypeAnnotation(type_constructor_idx);
                    }

                    // No constructor - create fresh variable
                    return try self.store.fresh();
                },
                .record_literal => {
                    // Record type { field1: Type1, field2: Type2 }
                    // For now, create an empty record type
                    // A complete implementation would parse all fields
                    return try self.store.mkEmptyRecord(self.allocator, self.idents, null);
                },
                .tuple_literal => {
                    // Tuple type (Type1, Type2)
                    // For now, create a fresh variable
                    // A complete implementation would parse all elements
                    return try self.store.fresh();
                },
                else => {
                    // Unsupported annotation - create fresh variable
                    return try self.store.fresh();
                },
            }
        }

        /// Unify two types, ensuring they are compatible
        fn unifyTypes(self: *Self, type1: Var, type2: Var) !void {
            const resolved1 = self.store.resolveVar(type1);
            const resolved2 = self.store.resolveVar(type2);

            // If either is a flex variable, bind it to the other
            if (resolved1.desc.content == .flex_var) {
                try self.store.setVarContent(type1, resolved2.desc.content);
                return;
            }
            if (resolved2.desc.content == .flex_var) {
                try self.store.setVarContent(type2, resolved1.desc.content);
                return;
            }

            // Check if types are compatible
            switch (resolved1.desc.content) {
                .structure => |s1| switch (s1) {
                    .num => |n1| {
                        // Numeric types can unify with other numeric types
                        switch (resolved2.desc.content) {
                            .structure => |s2| switch (s2) {
                                .num => |n2| {
                                    // Unify numeric bounds
                                    const unified = try self.unifyNumBounds(n1, n2);
                                    try self.store.setVarContent(type1, .{ .structure = .{ .num = unified } });
                                    try self.store.setVarContent(type2, .{ .structure = .{ .num = unified } });
                                },
                                .int, .frac => {
                                    // Num can unify with specific int/frac
                                    try self.store.setVarContent(type1, resolved2.desc.content);
                                },
                                else => return error.TypeMismatch,
                            },
                            else => return error.TypeMismatch,
                        }
                    },
                    .int => |prec1| {
                        switch (resolved2.desc.content) {
                            .structure => |s2| switch (s2) {
                                .int => |prec2| {
                                    // Both are integers - must have same precision
                                    if (prec1 != prec2) {
                                        return error.TypeMismatch;
                                    }
                                },
                                .num => {
                                    // Int can unify with num - bind num to int
                                    try self.store.setVarContent(type2, resolved1.desc.content);
                                },
                                else => return error.TypeMismatch,
                            },
                            else => return error.TypeMismatch,
                        }
                    },
                    .frac => |prec1| {
                        switch (resolved2.desc.content) {
                            .structure => |s2| switch (s2) {
                                .frac => |prec2| {
                                    // Both are fractions - must have same precision
                                    if (prec1 != prec2) {
                                        return error.TypeMismatch;
                                    }
                                },
                                .num => {
                                    // Frac can unify with num - bind num to frac
                                    try self.store.setVarContent(type2, resolved1.desc.content);
                                },
                                else => return error.TypeMismatch,
                            },
                            else => return error.TypeMismatch,
                        }
                    },
                    .bool, .str => {
                        // Bool and Str must match exactly
                        if (!std.meta.eql(resolved1.desc.content, resolved2.desc.content)) {
                            return error.TypeMismatch;
                        }
                    },
                    .list => |elem1| {
                        switch (resolved2.desc.content) {
                            .structure => |s2| switch (s2) {
                                .list => |elem2| {
                                    // Recursively unify element types
                                    try self.unifyTypes(elem1, elem2);
                                },
                                else => return error.TypeMismatch,
                            },
                            else => return error.TypeMismatch,
                        }
                    },
                    else => {
                        // For other structural types, check exact equality for now
                        if (!std.meta.eql(resolved1.desc.content, resolved2.desc.content)) {
                            return error.TypeMismatch;
                        }
                    },
                },
                else => {
                    // For other types, check exact equality
                    if (!std.meta.eql(resolved1.desc.content, resolved2.desc.content)) {
                        return error.TypeMismatch;
                    }
                },
            }
        }

        /// Unify two numeric types to find their common type
        fn unifyNumericTypes(self: *Self, lhs_type: Var, rhs_type: Var) !Var {
            const lhs_resolved = self.store.resolveVar(lhs_type);
            const rhs_resolved = self.store.resolveVar(rhs_type);

            // Extract numeric content from both types
            const lhs_num = switch (lhs_resolved.desc.content) {
                .structure => |s| switch (s) {
                    .num => |n| n,
                    .int => |precision| types_mod.NumBound{ .int_exact = precision },
                    .frac => |precision| types_mod.NumBound{ .frac_exact = precision },
                    else => {
                        // Not a numeric type - return error
                        return error.TypeMismatch;
                    },
                },
                .flex_var => {
                    // If LHS is unbound, use RHS type
                    return rhs_type;
                },
                else => {
                    return error.TypeMismatch;
                },
            };

            const rhs_num = switch (rhs_resolved.desc.content) {
                .structure => |s| switch (s) {
                    .num => |n| n,
                    .int => |precision| types_mod.NumBound{ .int_exact = precision },
                    .frac => |precision| types_mod.NumBound{ .frac_exact = precision },
                    else => {
                        return error.TypeMismatch;
                    },
                },
                .flex_var => {
                    // If RHS is unbound, use LHS type
                    return lhs_type;
                },
                else => {
                    return error.TypeMismatch;
                },
            };

            // Unify the numeric bounds
            const unified_num = try self.unifyNumBounds(lhs_num, rhs_num);

            // Create a new variable with the unified type
            const unified_var = try self.store.fresh();
            const unified_content = Content{ .structure = .{ .num = unified_num } };
            try self.store.setVarContent(unified_var, unified_content);

            return unified_var;
        }

        /// Unify two numeric bounds to find their least upper bound
        fn unifyNumBounds(self: *Self, lhs: types_mod.NumBound, rhs: types_mod.NumBound) !types_mod.NumBound {
            _ = self;

            // Handle exact types first
            if (lhs == .int_exact and rhs == .int_exact) {
                // Both are exact integers - unify to the larger precision
                const lhs_prec = lhs.int_exact;
                const rhs_prec = rhs.int_exact;

                // Get the larger precision
                const unified_prec = blk: {
                    // Compare bit widths
                    const lhs_bits: u8 = switch (lhs_prec) {
                        .i8, .u8 => 8,
                        .i16, .u16 => 16,
                        .i32, .u32 => 32,
                        .i64, .u64 => 64,
                        .i128, .u128 => 128,
                    };
                    const rhs_bits: u8 = switch (rhs_prec) {
                        .i8, .u8 => 8,
                        .i16, .u16 => 16,
                        .i32, .u32 => 32,
                        .i64, .u64 => 64,
                        .i128, .u128 => 128,
                    };

                    // Check signedness compatibility
                    const lhs_signed = switch (lhs_prec) {
                        .i8, .i16, .i32, .i64, .i128 => true,
                        .u8, .u16, .u32, .u64, .u128 => false,
                    };
                    const rhs_signed = switch (rhs_prec) {
                        .i8, .i16, .i32, .i64, .i128 => true,
                        .u8, .u16, .u32, .u64, .u128 => false,
                    };

                    if (lhs_signed != rhs_signed) {
                        // Mixed signed/unsigned - promote to signed with larger width
                        const max_bits = @max(lhs_bits, rhs_bits);
                        break :blk switch (max_bits) {
                            8 => types_mod.IntPrecision.i16, // Promote to avoid overflow
                            16 => types_mod.IntPrecision.i32,
                            32 => types_mod.IntPrecision.i64,
                            64 => types_mod.IntPrecision.i128,
                            128 => types_mod.IntPrecision.i128,
                            else => unreachable,
                        };
                    } else if (lhs_bits >= rhs_bits) {
                        break :blk lhs_prec;
                    } else {
                        break :blk rhs_prec;
                    }
                };

                return .{ .int_exact = unified_prec };
            }

            if (lhs == .frac_exact and rhs == .frac_exact) {
                // Both are exact fractions - unify to the larger precision
                const lhs_prec = lhs.frac_exact;
                const rhs_prec = rhs.frac_exact;

                const unified_prec = switch (lhs_prec) {
                    .f32 => switch (rhs_prec) {
                        .f32 => types_mod.FracPrecision.f32,
                        .f64 => types_mod.FracPrecision.f64,
                        .dec => types_mod.FracPrecision.dec,
                    },
                    .f64 => switch (rhs_prec) {
                        .f32, .f64 => types_mod.FracPrecision.f64,
                        .dec => types_mod.FracPrecision.dec,
                    },
                    .dec => types_mod.FracPrecision.dec,
                };

                return .{ .frac_exact = unified_prec };
            }

            // Handle unbound types
            if (lhs == .num_unbound and rhs == .num_unbound) {
                // Both unbound - merge constraints
                const sign_needed = lhs.num_unbound.sign_needed or rhs.num_unbound.sign_needed;
                const bits_needed = @max(lhs.num_unbound.bits_needed, rhs.num_unbound.bits_needed);
                return .{ .num_unbound = .{ .sign_needed = sign_needed, .bits_needed = bits_needed } };
            }

            // If one is exact int and other is unbound num, use the exact int
            if (lhs == .int_exact and rhs == .num_unbound) {
                return lhs;
            }
            if (rhs == .int_exact and lhs == .num_unbound) {
                return rhs;
            }

            // If one is exact frac and other is unbound num, use the exact frac
            if (lhs == .frac_exact and rhs == .num_unbound) {
                return lhs;
            }
            if (rhs == .frac_exact and lhs == .num_unbound) {
                return rhs;
            }

            // Mixed int/frac - promote to frac
            if ((lhs == .int_exact and rhs == .frac_exact) or
                (lhs == .frac_exact and rhs == .int_exact))
            {
                // Default to f64 for mixed int/frac
                return .{ .frac_exact = .f64 };
            }

            // Default case - return unbound num
            return .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } };
        }

        /// Infer the type of a CIR2 pattern
        pub fn inferPattern(self: *Self, patt_idx: CIR2.Patt.Idx) !Var {
            // Patterns should also use their index as the type variable
            const patt_var = @as(Var, @enumFromInt(@intFromEnum(patt_idx)));

            // Ensure the variable exists in the store
            const var_idx = @intFromEnum(patt_var);
            const current_len = self.store.len();
            if (var_idx >= current_len) {
                // Variable doesn't exist - create fresh variables up to this index
                var i = current_len;
                while (i <= var_idx) : (i += 1) {
                    _ = try self.store.fresh();
                }
            }

            // Check if this variable has content - if it does, return it
            const resolved = self.store.resolveVar(patt_var);
            if (resolved.desc.content != .flex_var) {
                // Already has a type from type checking, return it
                return patt_var;
            }

            const patt = self.cir.getPatt(patt_idx);

            switch (patt.tag) {
                .identifier => {
                    // Simple identifier binding - return the pattern's variable
                    return patt_var;
                },
                .underscore => {
                    // Underscore pattern - matches anything
                    return patt_var;
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
                    // Record pattern - should have been populated during type checking
                    // Just return the pattern's variable
                    return patt_var;
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
                    // Type checking should have already handled these
                    // Skip inference to avoid union field access issues
                },
                .expr => {
                    // Expression statement - the statement itself is the expression
                    // Convert statement index to expression index
                    const expr_idx = @as(CIR2.Expr.Idx, @enumFromInt(@intFromEnum(stmt_idx)));
                    _ = try self.inferExpr(expr_idx);
                },
                .ret => {
                    // Return statement - type checking should have handled this
                    // Skip inference to avoid union field access issues
                },
                .crash => {
                    // Crash statement - type checking should have handled this
                    // Skip inference to avoid union field access issues
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
