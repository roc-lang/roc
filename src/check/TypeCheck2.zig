//! CIR2 traversal methods for the existing type checker
//!
//! This module extends the existing Check.zig with new traversal methods that work
//! with CIR2 nodes. We reuse all the existing type checking infrastructure
//! (unification, error collection, type store) and just add new traversal logic.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types");
const can = @import("can");
const parse = @import("parse");
const collections = @import("collections");

const CIR2 = can.CIR2;
const Check = @import("Check.zig");
const Var = types_mod.Var;
const Content = types_mod.Content;
const Region = base.Region;
const Position = base.Region.Position;
const AST2 = parse.AST2;
const Ident = base.Ident;

/// Scope tracking for CIR2 type checking
const CIR2Scope = struct {
    vars: std.AutoHashMap(Ident.Idx, Var),

    fn init(allocator: std.mem.Allocator) CIR2Scope {
        return .{
            .vars = std.AutoHashMap(Ident.Idx, Var).init(allocator),
        };
    }

    fn deinit(self: *CIR2Scope) void {
        self.vars.deinit();
    }
};

/// Context for CIR2 type checking that tracks scopes
const CIR2Context = struct {
    scopes: std.ArrayList(CIR2Scope),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) CIR2Context {
        return .{
            .scopes = std.ArrayList(CIR2Scope).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: *CIR2Context) void {
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit();
    }

    fn pushScope(self: *CIR2Context) !void {
        try self.scopes.append(CIR2Scope.init(self.allocator));
    }

    fn popScope(self: *CIR2Context) void {
        if (self.scopes.items.len > 0) {
            if (self.scopes.pop()) |scope| {
                var mut_scope = scope;
                mut_scope.deinit();
            }
        }
    }

    fn lookupVar(self: *CIR2Context, ident: Ident.Idx) ?Var {
        // Search from innermost to outermost scope
        var i = self.scopes.items.len;
        while (i > 0) : (i -= 1) {
            if (self.scopes.items[i - 1].vars.get(ident)) |var_type| {
                return var_type;
            }
        }
        return null;
    }

    fn addVar(self: *CIR2Context, ident: Ident.Idx, var_type: Var) !void {
        if (self.scopes.items.len == 0) {
            try self.pushScope();
        }
        try self.scopes.items[self.scopes.items.len - 1].vars.put(ident, var_type);
    }
};

/// Check a CIR2 expression using existing Check infrastructure
pub fn checkCIR2Expr(check: *Check, cir2: *const CIR2, expr_idx: CIR2.Expr.Idx, context: *CIR2Context) !Var {
    const expr = cir2.getExpr(expr_idx);

    // Create a type variable for this expression
    const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

    switch (expr.tag) {
        .str_literal_small => {
            // String literals have type Str
            const str_content = Content{ .structure = .{ .str = {} } };
            const str_var = try check.types.freshFromContent(str_content);
            _ = try check.unify(expr_var, str_var);
        },

        .num_literal_i32 => {
            // Create an i32 type
            const num_content = Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } };
            const num_var = try check.types.freshFromContent(num_content);
            _ = try check.unify(expr_var, num_var);
        },

        .int_literal_i32 => {
            // Integer literals with explicit type
            const int_content = Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } };
            const int_var = try check.types.freshFromContent(int_content);
            _ = try check.unify(expr_var, int_var);
        },

        .lookup => {
            // Variable lookup
            const ident = expr.payload.ident;
            if (context.lookupVar(ident)) |var_type| {
                _ = try check.unify(expr_var, var_type);
            } else {
                // Variable not in scope - create a flex var
                // The existing Check infrastructure will handle the error
                const flex_var = try check.types.fresh();
                _ = try check.unify(expr_var, flex_var);
            }
        },

        .binop_plus, .binop_minus, .binop_star, .binop_slash => {
            // Binary arithmetic operations - check both operands
            const binop = cir2.getBinOp(CIR2.Expr.Idx, expr.payload.binop);

            // Check left and right operands
            const lhs_var = try checkCIR2Expr(check, cir2, binop.lhs, context);
            const rhs_var = try checkCIR2Expr(check, cir2, binop.rhs, context);

            // For arithmetic operations, both operands and result must be numeric
            _ = try check.unify(lhs_var, rhs_var);
            _ = try check.unify(expr_var, lhs_var);
        },

        .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte => {
            // Comparison operations - check both operands
            const binop = cir2.getBinOp(CIR2.Expr.Idx, expr.payload.binop);

            // Check left and right operands
            const lhs_var = try checkCIR2Expr(check, cir2, binop.lhs, context);
            const rhs_var = try checkCIR2Expr(check, cir2, binop.rhs, context);

            // Operands must have the same type
            _ = try check.unify(lhs_var, rhs_var);

            // Result is Bool - use fresh var for now
            const bool_var = try check.types.fresh();
            _ = try check.unify(expr_var, bool_var);
        },

        .empty_list => {
            // Empty list - create List a where a is a flex var
            const elem_var = try check.types.fresh();
            const list_content = Content{ .structure = .{ .list = elem_var } };
            const list_var = try check.types.freshFromContent(list_content);
            _ = try check.unify(expr_var, list_var);
        },

        .list_literal => {
            // Non-empty list - all elements must have the same type
            // For now, we'll skip this until we implement proper node slice access
            @panic("TODO: Implement list literal type checking");
        },

        .block => {
            // Block expression - evaluate statements in order, return last expression
            // For now, we'll skip this until we implement proper node slice access
            @panic("TODO: Implement block expression type checking");
        },

        else => {
            // For unhandled expression types, panic with details
            std.debug.print("TODO: Unhandled CIR2 expression type: {}\n", .{expr.tag});
            @panic("TODO: Implement type checking for this CIR2 expression type");
        },
    }

    return expr_var;
}

/// Check a CIR2 statement using existing Check infrastructure
pub fn checkCIR2Stmt(_: *Check, cir2: *const CIR2, stmt_idx: CIR2.Stmt.Idx, _: *CIR2Context) !void {
    const stmt = cir2.getStmt(stmt_idx);

    switch (stmt.tag) {
        .assign => {
            // Immutable binding: pattern = expr
            // For now, we'll skip this until we understand the payload structure
            @panic("TODO: Implement assignment statement type checking");
        },

        .init_var => {
            // Mutable variable initialization: var pattern = expr
            // For now, we'll skip this until we understand the payload structure
            @panic("TODO: Implement init_var statement type checking");
        },

        .reassign => {
            // Reassignment to mutable variable
            // For now, we'll skip this until we understand the payload structure
            @panic("TODO: Implement reassign statement type checking");
        },

        else => {
            // Other statement types - add as needed
            std.debug.print("TODO: Unhandled CIR2 statement type: {}\n", .{stmt.tag});
        },
    }
}

/// Check a CIR2 pattern using existing Check infrastructure
pub fn checkCIR2Patt(check: *Check, cir2: *const CIR2, patt_idx: CIR2.Patt.Idx, expected_type: Var, context: *CIR2Context) !Var {
    const actual_idx = patt_idx.toNodeIdx();
    const patt = cir2.getPatt(patt_idx);

    // Create a type variable for this pattern
    const patt_var = @as(Var, @enumFromInt(@intFromEnum(actual_idx)));

    switch (patt.tag) {
        .ident, .var_ident => {
            // Identifier pattern - bind to expected type
            const ident = patt.payload.ident;
            try context.addVar(ident, expected_type);
            _ = try check.unify(patt_var, expected_type);
        },

        .underscore => {
            // Wildcard - matches anything, doesn't bind
            _ = try check.unify(patt_var, expected_type);
        },

        .num_literal_i32 => {
            // Number literal pattern
            const num_content = Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } };
            const num_var = try check.types.freshFromContent(num_content);
            _ = try check.unify(patt_var, num_var);
            _ = try check.unify(patt_var, expected_type);
        },

        else => {
            // Other pattern types - just unify with expected for now
            _ = try check.unify(patt_var, expected_type);
        },
    }

    return patt_var;
}

// Tests - using existing Check infrastructure

const testing = std.testing;

// Helper to create a properly initialized Check instance
fn createCheck(allocator: std.mem.Allocator) !struct {
    types: types_mod.Store,
    regions: Region.List,
    module_env: can.ModuleEnv,
    check: Check,
} {
    var types_store = try types_mod.Store.init(allocator);
    errdefer types_store.deinit();

    var regions = Region.List{};
    errdefer regions.deinit(allocator);

    var module_env = try can.ModuleEnv.init(allocator, "test");
    errdefer module_env.deinit();

    var check = try Check.init(allocator, &types_store, &module_env, &.{}, &regions);
    errdefer check.deinit();

    return .{
        .types = types_store,
        .regions = regions,
        .module_env = module_env,
        .check = check,
    };
}

// Helper to parse and canonicalize a program (simplified for now)
fn parseCanonicalizeProgram(allocator: std.mem.Allocator, _: []const u8) !struct {
    ast: AST2,
    cir: CIR2,
} {
    // For now, just create minimal AST and CIR for testing
    var ast = try AST2.initCapacity(allocator, 100);
    errdefer ast.deinit(allocator);

    // Create CIR from AST
    const cir = CIR2.init(&ast);

    return .{
        .ast = ast,
        .cir = cir,
    };
}

test "CIR2: basic infrastructure" {
    const allocator = testing.allocator;

    // Create Check infrastructure
    var test_env = try createCheck(allocator);
    defer test_env.types.deinit();
    defer test_env.regions.deinit(allocator);
    defer test_env.module_env.deinit();
    defer test_env.check.deinit();

    // Create CIR2 context
    var context = CIR2Context.init(allocator);
    defer context.deinit();

    try context.pushScope();

    // Test basic infrastructure is working
    try testing.expect(context.scopes.items.len == 1);
}

test "CIR2: scope management" {
    const allocator = testing.allocator;

    var context = CIR2Context.init(allocator);
    defer context.deinit();

    // Test nested scopes
    try context.pushScope();
    try context.pushScope();
    try context.pushScope();

    try testing.expect(context.scopes.items.len == 3);

    context.popScope();
    try testing.expect(context.scopes.items.len == 2);

    context.popScope();
    context.popScope();
    try testing.expect(context.scopes.items.len == 0);
}
