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
const Func = types_mod.Func;
const Var = types_mod.Var;
const Content = types_mod.Content;
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

    // Debug: Print the index to see what we're dealing with
    if (expr_idx_int > 10000) {
        // std.debug.print("WARNING: Large expr_idx_int: {}\n", .{expr_idx_int});
    }

    // Check for invalid indices
    if (expr_idx_int < 0 or expr_idx_int > 100000) {
        // Index is out of reasonable range - use a fresh variable
        // std.debug.print("Index out of range, using fresh var: {}\n", .{expr_idx_int});
        return try self.types.fresh();
    }

    // Ensure the variable exists by allocating up to this index
    const needed_vars = @as(usize, @intCast(expr_idx_int + 1));
    // std.debug.print("checkCIRExpr: ensuring {} variables exist (current: {})\n", .{ needed_vars, self.types.len() });
    while (self.types.len() < needed_vars) {
        _ = try self.types.fresh();
    }
    // std.debug.print("checkCIRExpr: allocated variables, now have {}\n", .{self.types.len()});

    const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
    // std.debug.print("checkCIRExpr: created expr_var from expr_idx {}\n", .{@intFromEnum(expr_idx)});

    // Check if this expression has already been type-checked
    // If the variable already has content beyond flex_var, we've already processed it
    // std.debug.print("checkCIRExpr: about to resolve var {}\n", .{@intFromEnum(expr_var)});
    const resolved = self.types.resolveVar(expr_var);
    // std.debug.print("checkCIRExpr: resolved var successfully\n", .{});
    if (resolved.desc.content != .flex_var) {
        // Already has a concrete type, don't re-check
        // std.debug.print("checkCIRExpr: var already has concrete type, returning\n", .{});
        return expr_var;
    }

    std.debug.print("checkCIRExpr: about to switch on expr.tag = {}\n", .{expr.tag});

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
            // For now, just return a fresh type variable
            return expr_var;
        },

        // Records
        .record_literal => {
            // For now, just return a fresh type variable
            return expr_var;
        },

        // Tuples
        .tuple_literal => {
            // For now, just return a fresh type variable
            return expr_var;
        },

        // Blocks
        .block => {
            // For now, just return a fresh type variable
            return expr_var;
        },

        // If expressions
        .if_else => {
            // For now, just return a fresh type variable
            return expr_var;
        },

        // Lambda
        .lambda => {
            // For now, just return a fresh type variable
            return expr_var;
        },

        .unary_not, .unary_neg => {
            // Unary operators - just check the operand and propagate its type
            // For unary_not, should be Bool -> Bool
            // For unary_neg, should be Num -> Num
            // For now, just check the operand and let type inference figure it out

            // The operand is stored in the nodes payload
            const node = cir.getNode(@as(AST.Node.Idx, @enumFromInt(@intFromEnum(expr_idx))));
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = cir.ast.node_slices.nodes(&nodes_idx);
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

        .apply_ident, .apply_tag => {
            // Function/tag application
            // For now, just return a fresh type variable
            // TODO: Implement proper function application type checking
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

        else => {
            // Unhandled expression type - just return the variable
            // std.debug.print("checkCIRExpr: unhandled expression tag: {}\n", .{expr.tag});
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
