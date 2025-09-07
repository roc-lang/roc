//! Performs Hindley-Milner type inference with constraint solving and unification on the Canonical Intermediate Representation (CIR).
//!
//! This module implements constraint-based type inference for the new CIR.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const tracy = @import("tracy");
const collections = @import("collections");
const types_mod = @import("types");
const can = @import("can");
const parse = @import("parse");

const unifier = @import("unify.zig");
const occurs = @import("occurs.zig");
const problem = @import("problem.zig");

const CIR = can.CIR;
const AST = parse.AST;
const CommonEnv = base.CommonEnv;
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const Instantiate = types_mod.instantiate.Instantiate;
const Var = types_mod.Var;
const Func = types_mod.Func;
const Content = types_mod.Content;
const RecordField = types_mod.RecordField;
const Record = types_mod.Record;
const testing = std.testing;
const SnapshotStore = @import("snapshot.zig").Store;
const ProblemStore = @import("problem.zig").Store;

const Self = @This();

gpa: std.mem.Allocator,
// not owned
types: *types_mod.Store,
regions: *Region.List,
// owned
snapshots: SnapshotStore,
problems: ProblemStore,
unify_scratch: unifier.Scratch,
occurs_scratch: occurs.Scratch,
var_map: std.AutoHashMap(Var, Var),
annotation_rigid_var_subs: Instantiate.RigidToFlexSubs,
anonymous_rigid_var_subs: Instantiate.RigidToFlexSubs,
/// Maps variables to the expressions that constrained them (for better error regions)
constraint_origins: std.AutoHashMap(Var, Var),

/// Initialize a Check instance for CIR type checking
pub fn initForCIR(
    gpa: std.mem.Allocator,
    types_store: *types_mod.Store,
    regions: *Region.List,
) std.mem.Allocator.Error!Self {
    return .{
        .gpa = gpa,
        .types = types_store,
        .regions = regions,
        .snapshots = try SnapshotStore.initCapacity(gpa, 1024),
        .problems = try ProblemStore.initCapacity(gpa, 1024),
        .unify_scratch = try unifier.Scratch.init(gpa),
        .occurs_scratch = try occurs.Scratch.init(gpa),
        .var_map = std.AutoHashMap(Var, Var).init(gpa),
        .annotation_rigid_var_subs = try Instantiate.RigidToFlexSubs.init(gpa),
        .anonymous_rigid_var_subs = try Instantiate.RigidToFlexSubs.init(gpa),
        .constraint_origins = std.AutoHashMap(Var, Var).init(gpa),
    };
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.problems.deinit(self.gpa);
    self.snapshots.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
    self.var_map.deinit();
    self.annotation_rigid_var_subs.deinit(self.gpa);
    self.anonymous_rigid_var_subs.deinit(self.gpa);
    self.constraint_origins.deinit();
}

/// Check types for a CIR expression
pub fn checkCIRExpr(self: *Self, comptime CIRType: type, cir: *const CIRType, expr_idx: CIRType.Expr.Idx) std.mem.Allocator.Error!Var {
    const expr = cir.getExpr(expr_idx);

    // Check for malformed expressions
    if (expr.tag == .malformed) {
        // Return a fresh type variable for malformed expressions
        return try self.types.fresh();
    }

    // For CIR, we use AST node indices as type variable indices
    // This is a design decision that requires us to ensure the variable exists
    const expr_idx_int = @intFromEnum(expr_idx);

    // Check for invalid indices
    if (expr_idx_int < 0 or expr_idx_int > 100000) {
        // Index is out of reasonable range - use a fresh variable
        return try self.types.fresh();
    }

    // Ensure the variable exists by allocating up to this index
    const needed_vars = @as(usize, @intCast(expr_idx_int + 1));
    while (self.types.len() < needed_vars) {
        _ = try self.types.fresh();
    }

    const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

    // Check if this expression has already been type-checked
    // If the variable already has content beyond flex_var, we've already processed it
    const resolved = self.types.resolveVar(expr_var);
    if (resolved.desc.content != .flex_var) {
        // Already has a concrete type, don't re-check
        // However, if it has an error type, that might mean it was created but never properly typed
        if (resolved.desc.content == .err) {
            // This shouldn't happen in normal flow - error types should only come from unification failures
            // For now, continue to type-check this expression
        } else {
            return expr_var;
        }
    }

    switch (expr.tag) {
        // Literals - these already have their types set in canonicalization
        .num_literal_i32, .int_literal_i32, .num_literal_big, .int_literal_big, .frac_literal_small, .frac_literal_big, .str_literal_small, .str_literal_big => {
            // Type already set via ensureTypeVarExists in CIR
            return expr_var;
        },

        // Lookups - connect to their definitions
        .lookup => {
            // The type was already connected to the definition in canonicalization
            return expr_var;
        },

        // Simple tags without arguments
        .tag_no_args => {
            // Simple tags are just values of their tag union type
            // The type will be inferred based on usage context
            return expr_var;
        },

        // Binary operations
        .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_lt, .binop_gt, .binop_lte, .binop_gte => {
            // Get the binary operation
            const binop = cir.getBinOp(CIRType.Expr.Idx, expr.payload.binop);

            // Recursively check both operands
            const lhs_var = try self.checkCIRExpr(CIRType, cir, binop.lhs);
            const rhs_var = try self.checkCIRExpr(CIRType, cir, binop.rhs);

            // For numeric operations, unify both sides and result
            _ = try self.unify(lhs_var, rhs_var);
            _ = try self.unify(lhs_var, expr_var);

            return expr_var;
        },

        // Equality operations
        .binop_double_equals, .binop_not_equals => {
            const binop = cir.getBinOp(CIRType.Expr.Idx, expr.payload.binop);

            // Check both operands
            const lhs_var = try self.checkCIRExpr(CIRType, cir, binop.lhs);
            const rhs_var = try self.checkCIRExpr(CIRType, cir, binop.rhs);

            // Operands must have the same type
            _ = try self.unify(lhs_var, rhs_var);

            // Result is Bool (for now, just use a fresh type variable)
            const bool_var = try self.types.fresh();
            _ = try self.unify(expr_var, bool_var);

            return expr_var;
        },

        // Boolean operations
        .binop_and, .binop_or => {
            const binop = cir.getBinOp(CIRType.Expr.Idx, expr.payload.binop);

            // Check both operands (they should be Bool)
            const lhs_var = try self.checkCIRExpr(CIRType, cir, binop.lhs);
            const rhs_var = try self.checkCIRExpr(CIRType, cir, binop.rhs);

            // For now, just unify them
            _ = try self.unify(lhs_var, rhs_var);
            _ = try self.unify(lhs_var, expr_var);

            return expr_var;
        },

        // Lists
        .list_literal => {
            // Type check all list elements and ensure they have the same type
            const nodes_idx = expr.payload.nodes;

            if (nodes_idx.isNil()) {
                // Empty list - create List a where a is a fresh type variable
                const elem_type = try self.types.fresh();
                const list_content = Content{ .structure = .{ .list = elem_type } };
                const list_type = try self.types.freshFromContent(list_content);
                _ = try self.unify(expr_var, list_type);
                return expr_var;
            }

            // Non-empty list - all elements must have the same type
            var elem_type: ?Var = null;
            var iter = cir.getNodeIndices(nodes_idx);
            while (iter.next()) |elem_node| {
                const elem_expr_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(elem_node)));
                const elem_var = try self.checkCIRExpr(CIRType, cir, elem_expr_idx);

                if (elem_type) |expected_type| {
                    // Unify with the expected element type
                    _ = try self.unify(elem_var, expected_type);
                } else {
                    // First element - set the expected type
                    elem_type = elem_var;
                }
            }

            // Create the list type
            const list_content = Content{ .structure = .{ .list = elem_type.? } };
            const list_type = try self.types.freshFromContent(list_content);
            _ = try self.unify(expr_var, list_type);

            return expr_var;
        },

        // Record field access
        .record_access => {
            // Handle record field access like record.field
            const binop_idx = expr.payload.binop;
            const binop = cir.getBinOp(CIRType.Expr.Idx, binop_idx);

            // Type-check the record expression (left side)
            const record_expr_idx = binop.lhs;
            const record_var = try self.checkCIRExpr(CIRType, cir, record_expr_idx);

            // For now, just propagate the record type and return a fresh variable for the field
            // This is a simplified approach that should work for basic cases
            _ = record_var;

            return expr_var;
        },

        // Records
        .record_literal => {
            // Create a proper record type by analyzing the fields
            const nodes_idx = expr.payload.nodes;

            var record_fields = std.ArrayList(RecordField).init(self.gpa);
            defer record_fields.deinit();

            if (!nodes_idx.isNil()) {
                var iter = cir.getNodeIndices(nodes_idx);
                while (iter.next()) |field_node| {
                    const field_expr_idx = @as(CIR.Expr.Idx, @enumFromInt(@intFromEnum(field_node)));
                    const field_expr = cir.getExpr(field_expr_idx);

                    if (field_expr.tag == .record_field) {
                        // Field with explicit value: { name: value }
                        const field_binop = cir.getBinOp(CIR.Expr.Idx, field_expr.payload.binop);

                        // Get field name from left side (should be an identifier)
                        // Field names are expressions after canonicalization
                        const field_name_expr = cir.getExpr(@enumFromInt(@intFromEnum(field_binop.lhs)));
                        const field_ident = blk: {
                            if (field_name_expr.tag == .lookup) {
                                break :blk field_name_expr.payload.ident;
                            } else if (field_name_expr.tag == .malformed) {
                                // The field name was converted to a pattern during canonicalization
                                // Extract the identifier from the pattern
                                const field_patt_idx = @as(CIRType.Patt.Idx, @enumFromInt(@intFromEnum(field_binop.lhs)));
                                const field_patt = cir.getPatt(field_patt_idx);
                                if (field_patt.tag == .ident) {
                                    break :blk field_patt.payload.ident;
                                }
                                // Can't extract field name - skip this field
                                continue;
                            } else {
                                // Field name is not an identifier - skip this field
                                continue;
                            }
                        };

                        // Type-check the field value (right side)
                        const field_value_var = try self.checkCIRExpr(CIRType, cir, field_binop.rhs);

                        // Create the record field
                        const record_field = RecordField{
                            .name = field_ident,
                            .var_ = field_value_var,
                        };
                        try record_fields.append(record_field);
                    } else if (field_expr.tag == .lookup) {
                        // Shorthand field: { x } means { x: x }
                        const field_ident = field_expr.payload.ident;

                        // The value is a lookup of the same identifier
                        const field_value_var = try self.checkCIRExpr(CIRType, cir, field_expr_idx);

                        // Create the record field
                        const record_field = RecordField{
                            .name = field_ident,
                            .var_ = field_value_var,
                        };
                        try record_fields.append(record_field);
                    }
                }
            }

            // Create the record type
            if (record_fields.items.len == 0) {
                // Empty record
                const empty_record_content = Content{ .structure = .empty_record };
                const empty_record_type = try self.types.freshFromContent(empty_record_content);
                _ = try self.unify(expr_var, empty_record_type);
                return expr_var;
            } else {
                // Non-empty record: create record with fields
                const fields_range = try self.types.appendRecordFields(record_fields.items);

                // Create extension variable (for open records)
                const ext_var = try self.types.freshFromContent(.{ .structure = .empty_record });

                // Create the record
                const record = Record{ .fields = fields_range, .ext = ext_var };
                const record_content = Content{ .structure = .{ .record = record } };
                const record_type = try self.types.freshFromContent(record_content);

                // Unify with the expression's type variable
                _ = try self.unify(expr_var, record_type);
                return expr_var;
            }
        },

        // Tuples
        .tuple_literal => {
            // Get the tuple elements
            const nodes_idx = expr.payload.nodes;

            if (nodes_idx.isNil()) {
                // Empty tuple (unit type)
                const empty_elems_range = try self.types.appendVars(&.{});
                const unit_content = Content{ .structure = .{ .tuple = .{ .elems = empty_elems_range } } };
                const unit_type = try self.types.freshFromContent(unit_content);
                _ = try self.unify(expr_var, unit_type);
                return expr_var;
            }

            // Type check each element
            var element_vars = std.ArrayList(Var).init(self.gpa);
            defer element_vars.deinit();

            var iter = cir.getNodeIndices(nodes_idx);
            while (iter.next()) |element_node| {
                const element_expr_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(element_node)));
                const element_var = try self.checkCIRExpr(CIRType, cir, element_expr_idx);
                try element_vars.append(element_var);
            }

            // Create tuple type
            const elems_range = try self.types.appendVars(element_vars.items);
            const tuple_content = Content{ .structure = .{ .tuple = .{ .elems = elems_range } } };
            const tuple_type = try self.types.freshFromContent(tuple_content);

            _ = try self.unify(expr_var, tuple_type);
            return expr_var;
        },

        // Blocks
        .block => {
            // Blocks contain statements - we need to type check them
            const nodes_idx = expr.payload.nodes;
            if (!nodes_idx.isNil()) {
                // Get the statements in the block
                var iter = cir.getNodeIndices(nodes_idx);

                // Type check each statement in the block
                while (iter.next()) |node_idx| {
                    // First check if this node is a statement or expression
                    const node_tag_value = cir.getNodeTag(node_idx);
                    const stmt_first = @intFromEnum(CIRType.StmtTag.assign);

                    if (node_tag_value >= stmt_first and node_tag_value < @intFromEnum(CIRType.ExprTag.lookup)) {
                        // It's a statement - handle it
                        const stmt_idx = @as(CIRType.Stmt.Idx, @enumFromInt(@intFromEnum(node_idx)));
                        const stmt = cir.getStmt(stmt_idx);
                        const stmt_tag: CIRType.StmtTag = @enumFromInt(node_tag_value);

                        switch (stmt_tag) {
                            .assign => {
                                // An assignment has a pattern and a value stored in a binop
                                // The payload is a binop with pattern on left and value on right
                                const binop = cir.getNodeBinOp(stmt.payload.binop);

                                // Get the pattern variable (left side)
                                const patt_idx = @as(CIRType.Patt.Idx, @enumFromInt(@intFromEnum(binop.lhs)));
                                const patt_var = @as(Var, @enumFromInt(@intFromEnum(patt_idx)));

                                // Get the value expression and check its type (right side)
                                const value_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(binop.rhs)));
                                const value_var = try self.checkCIRExpr(CIRType, cir, value_idx);

                                // Unify the pattern with the value!
                                _ = try self.unify(patt_var, value_var);
                            },
                            .expr => {
                                // Expression statement - just check it
                                const expr_stmt_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(node_idx)));
                                _ = try self.checkCIRExpr(CIRType, cir, expr_stmt_idx);
                            },
                            else => {
                                // Other statement types - skip for now
                            },
                        }
                    } else if (node_tag_value >= @intFromEnum(CIRType.ExprTag.lookup)) {
                        // It's an expression used as a statement - check it
                        const stmt_as_expr_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(node_idx)));
                        _ = try self.checkCIRExpr(CIRType, cir, stmt_as_expr_idx);
                    }
                }
            }

            // The block evaluates to unit type or the last expression
            // For now, just return the block's type variable
            return expr_var;
        },

        // If expressions
        .if_else => {
            // if condition then_branch else else_branch
            // The condition must be Bool
            // The then and else branches must have the same type
            // The whole expression has the type of the branches

            // Get the if expression components - if_branches is stored as u32
            const if_branches_u32 = expr.payload.if_branches;
            const nodes_idx = @as(collections.NodeSlices(AST.Node.Idx).Idx, @enumFromInt(if_branches_u32));
            if (!nodes_idx.isNil()) {
                var iter = cir.getNodeIndices(nodes_idx);

                // First node is the condition
                if (iter.next()) |cond_node| {
                    const cond_expr_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(cond_node)));
                    const cond_var = try self.checkCIRExpr(CIRType, cir, cond_expr_idx);

                    // Condition should be Bool (create a Bool constraint)
                    const bool_var = try self.types.fresh();
                    _ = try self.unify(cond_var, bool_var);
                }

                // Second node is the then branch
                var then_var: ?Var = null;
                if (iter.next()) |then_node| {
                    const then_expr_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(then_node)));
                    then_var = try self.checkCIRExpr(CIRType, cir, then_expr_idx);
                }

                // Third node is the else branch
                if (iter.next()) |else_node| {
                    const else_expr_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(else_node)));
                    const else_var = try self.checkCIRExpr(CIRType, cir, else_expr_idx);

                    if (then_var) |tv| {
                        // Both branches must have the same type
                        _ = try self.unify(tv, else_var);
                        // The whole expression has the branch type
                        _ = try self.unify(expr_var, tv);
                    }
                }
            }

            return expr_var;
        },

        // Lambda
        .lambda => {
            // Lambda expressions need their function type created during type checking
            // Multi-parameter lambdas in Roc are curried: |x, y| body becomes x -> (y -> body)

            // Get the lambda's body_then_args payload
            const body_then_args = expr.payload.body_then_args;
            if (body_then_args.isNil()) {
                // No body or parameters - shouldn't happen for well-formed lambdas
                return expr_var;
            }

            // Parse the lambda structure: [body, param1, param2, ...]
            var iter = cir.getNodeIndices(body_then_args);

            // First node is the body
            const body_node_idx = iter.next() orelse return expr_var;

            // Collect parameters
            var param_vars = std.ArrayList(Var).init(self.gpa);
            defer param_vars.deinit();

            while (iter.next()) |_| {
                // Create a fresh type variable for each parameter
                const param_var = try self.types.fresh();
                try param_vars.append(param_var);
            }

            // Type check the body
            const body_expr_idx = @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(body_node_idx)));
            const body_var = try self.checkCIRExpr(CIRType, cir, body_expr_idx);

            // Build the curried function type
            // For |x, y, z| body, we need: x -> (y -> (z -> body))
            var result_type = body_var;

            // Process parameters in reverse order to build the curried type properly
            var i = param_vars.items.len;
            while (i > 0) {
                i -= 1;
                const param_type = param_vars.items[i];

                // Create a function type: param -> result_type
                // Each function takes one parameter and returns either the body type
                // or another function
                const func_content = Content{ .structure = .{ .fn_pure = .{
                    .args = try self.types.appendVars(&[_]Var{param_type}),
                    .ret = result_type,
                    .needs_instantiation = self.types.needsInstantiation(param_type) or
                        self.types.needsInstantiation(result_type),
                } } };
                result_type = try self.types.freshFromContent(func_content);
            }

            // The lambda expression has the complete curried function type
            _ = try self.unify(expr_var, result_type);

            return expr_var;
        },

        .unary_not, .unary_neg => {
            // Unary operators - just check the operand and propagate its type
            // For unary_not, should be Bool -> Bool
            // For unary_neg, should be Num -> Num
            // For now, just check the operand and let type inference figure it out

            // The operand is stored in the nodes payload
            const nodes_idx = expr.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = cir.getNodeIndices(nodes_idx);
                if (iter.next()) |operand_node| {
                    const operand_var = try self.checkCIRExpr(CIRType, cir, @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(operand_node))));

                    // For both unary_not and unary_neg, the result type matches the operand type
                    // unary_not: Bool -> Bool
                    // unary_neg: Num a -> Num a
                    _ = try self.unify(expr_var, operand_var);
                }
            }
            return expr_var;
        },

        .fn_call, .tag_applied => {
            // Function/tag application with arguments
            // We need to type check both the function/tag and its arguments

            // Get the function and arguments nodes
            const nodes_idx = cir.getApplyNodes(expr_idx) orelse return expr_var;

            if (!nodes_idx.isNil()) {
                var iter = cir.getNodeIndices(nodes_idx);

                // First item is the function/tag being applied
                const func_node_idx = iter.next() orelse return expr_var;

                // Type check the function expression
                const func_var = try self.checkCIRExpr(CIRType, cir, @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(func_node_idx))));

                // Type check all arguments and collect their types
                var arg_vars = std.ArrayList(Var).init(self.gpa);
                defer arg_vars.deinit();

                while (iter.next()) |arg_node_idx| {
                    const arg_var = try self.checkCIRExpr(CIRType, cir, @as(CIRType.Expr.Idx, @enumFromInt(@intFromEnum(arg_node_idx))));
                    try arg_vars.append(arg_var);
                }

                // Create a function type constraint: func_var ~ (arg_types -> result_type)
                // The function being applied should have type (arg1, arg2, ...) -> result
                // where result unifies with expr_var

                // Create the expected function type
                const func_content = try self.types.mkFuncPure(arg_vars.items, // May be empty for zero-arg functions
                    expr_var // The result type is the expression's type
                );
                const expected_func_type = try self.types.freshFromContent(func_content);

                // Unify the actual function with the expected function type
                _ = try self.unify(func_var, expected_func_type);
            }

            return expr_var;
        },

        .binop_equals => {
            // Assignment expression (not a comparison) - treat as the RHS type
            const binop = cir.getBinOp(CIRType.Expr.Idx, expr.payload.binop);
            const rhs_var = try self.checkCIRExpr(CIRType, cir, binop.rhs);
            // The whole expression has the same type as the RHS
            _ = try self.unify(expr_var, rhs_var);
            return expr_var;
        },

        // Module access like Bool.True
        .module_access => {
            // Module-qualified access: Module.identifier
            // The type should come from the qualified identifier's definition
            // This requires looking up the module and then the identifier within it
            // For primitive types like Bool.True, the type should be Bool

            // Module access has a binop payload with module on left, identifier on right
            const binop = cir.getBinOp(CIRType.Expr.Idx, expr.payload.binop);

            // The right side is the identifier being accessed
            // Type check it to get its type
            const ident_var = try self.checkCIRExpr(CIRType, cir, binop.rhs);

            // The module access has the same type as the identifier
            _ = try self.unify(expr_var, ident_var);
            return expr_var;
        },

        // Crash expression
        .crash => {
            // Crash expressions never return - they have the "never" type
            // In our type system, we represent this as an error type since
            // it indicates the program will crash at this point
            const err_content = Content{ .err = {} };
            const err_type = try self.types.freshFromContent(err_content);
            _ = try self.unify(expr_var, err_type);
            return expr_var;
        },

        // Integer division
        .binop_double_slash => {
            // Integer division - similar to other numeric operations
            const binop = cir.getBinOp(CIRType.Expr.Idx, expr.payload.binop);
            const lhs_var = try self.checkCIRExpr(CIRType, cir, binop.lhs);
            const rhs_var = try self.checkCIRExpr(CIRType, cir, binop.rhs);
            _ = try self.unify(lhs_var, rhs_var);
            _ = try self.unify(lhs_var, expr_var);
            return expr_var;
        },

        // Unary not applied to a lookup
        .not_lookup => {
            // This is like !x where x is a lookup
            // The operand should be Bool and the result should be Bool
            // The payload contains the identifier being negated
            // For not_lookup, we need to look up the identifier's definition

            // Both the operand and result should be Bool
            // Create a Bool type variable that will be constrained to Bool
            const bool_var = try self.types.fresh();
            _ = try self.unify(expr_var, bool_var);

            return expr_var;
        },

        else => {
            // Unhandled expression type
            // This indicates a new expression type that needs implementation
            // Return the expression's type variable to allow inference to continue
            return expr_var;
        },
    }
}

/// Unify two type variables
pub fn unify(self: *Self, var1: Var, var2: Var) !Var {
    // We need an ident store for unification
    // For CIR checking, create a temporary empty ident store
    var idents = base.Ident.Store{
        .interner = try base.SmallStringInterner.initCapacity(self.gpa, 16),
    };
    defer idents.interner.deinit(self.gpa);

    const result = try unifier.unify(
        self.gpa,
        &idents,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.unify_scratch,
        &self.occurs_scratch,
        var1,
        var2,
    );

    return switch (result) {
        .ok => var1, // Unification succeeded
        .problem => var1, // Return var1 if unification fails
    };
}
