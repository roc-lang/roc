//! Type checking for CIR2 nodes
//! 
//! This module is responsible for traversing CIR2 nodes and connecting them to the
//! existing type-checking infrastructure. It does NOT handle unification or solving -
//! those are handled by the existing Check.zig infrastructure.
//!
//! The key design principle is that we traverse CIR2 nodes but delegate all actual
//! type inference, unification, and constraint solving to the existing system.

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
const Store = types_mod.Store;
const TypeChecker = Check;
const Region = base.Region;
const Position = base.Region.Position;
const AST2 = parse.AST2;
const Ident = base.Ident;

const Self = @This();

/// Reference to the main type checker
checker: *Check,

/// Reference to the CIR2 structure
cir2: *const CIR2,

/// Allocator
allocator: std.mem.Allocator,

/// Scope stack for tracking variable types
scopes: std.ArrayList(Scope),

const Scope = struct {
    vars: std.AutoHashMap(CIR2.Patt.Idx, Var),
    
    fn init(allocator: std.mem.Allocator) Scope {
        return .{
            .vars = std.AutoHashMap(CIR2.Patt.Idx, Var).init(allocator),
        };
    }
    
    fn deinit(self: *Scope) void {
        self.vars.deinit();
    }
};

/// Check a CIR2 expression and return its type variable
pub fn checkExpr(self: *Self, expr_idx: CIR2.Expr.Idx) std.mem.Allocator.Error!Var {
    const expr = self.cir2.getExpr(expr_idx);
    
    // Create a type variable for this expression
    // In the existing system, expression indices are cast to Vars
    const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
    
    switch (expr.tag) {
        .str_literal_small => {
            // String literals have type Str
            const str_content = Content{ .structure = .{ .str = {} } };
            const str_var = try self.checker.types.freshFromContent(str_content);
            
            // Unify the expression variable with the Str type
            _ = try self.checker.unify(expr_var, str_var);
        },
        
        .num_literal_i32 => {
            // Create an i32 type
            const num_content = Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } };
            const num_var = try self.checker.types.freshFromContent(num_content);
            
            _ = try self.checker.unify(expr_var, num_var);
        },
        
        .int_literal_i32 => {
            // Integer literals with explicit type
            const int_content = Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i32 } } } };
            const int_var = try self.checker.types.freshFromContent(int_content);
            
            _ = try self.checker.unify(expr_var, int_var);
        },
        
        .expr_binop_plus, .expr_binop_minus, .expr_binop_star, .expr_binop_slash => {
            // Binary arithmetic operations - check both operands
            const binop = self.cir2.getBinOp(CIR2.Expr.Idx, expr.payload.binop);
            
            // Check left and right operands
            const lhs_var = try self.checkExpr(binop.lhs);
            const rhs_var = try self.checkExpr(binop.rhs);
            
            // For arithmetic operations, we expect numeric types
            // Unify both operands with the same numeric type
            _ = try self.checker.unify(lhs_var, rhs_var);
            _ = try self.checker.unify(expr_var, lhs_var);
        },
        
        .expr_binop_double_equals, .expr_binop_not_equals, .expr_binop_gt, .expr_binop_gte, .expr_binop_lt, .expr_binop_lte => {
            // Comparison operations - check both operands
            const binop = self.cir2.getBinOp(CIR2.Expr.Idx, expr.payload.binop);
            
            // Check left and right operands
            const lhs_var = try self.checkExpr(binop.lhs);
            const rhs_var = try self.checkExpr(binop.rhs);
            
            // Operands must have the same type
            _ = try self.checker.unify(lhs_var, rhs_var);
            
            // Result is always Bool - for now use a flex var
            const bool_var = try self.checker.types.fresh();
            _ = try self.checker.unify(expr_var, bool_var);
        },
        
        .expr_empty_list => {
            // Empty list - create List a where a is a flex var
            const elem_var = try self.checker.types.fresh();
            const list_content = Content{ .structure = .{ .list = elem_var } };
            const list_var = try self.checker.types.freshFromContent(list_content);
            _ = try self.checker.unify(expr_var, list_var);
        },
        
        .expr_list_literal => {
            // Non-empty list - all elements must have the same type
            const elems_slice_idx = expr.payload.list_elems;
            const elems = self.cir2.getNodeSlice(CIR2.Expr.Idx, elems_slice_idx);
            
            // Check first element to get the element type
            var elem_type: ?Var = null;
            if (elems.len > 0) {
                elem_type = try self.checkExpr(elems[0]);
                
                // Check remaining elements and unify with first
                for (elems[1..]) |elem_idx| {
                    const elem_var = try self.checkExpr(elem_idx);
                    _ = try self.checker.unify(elem_type.?, elem_var);
                }
            } else {
                // Empty list - use flex var for element type
                elem_type = try self.checker.types.fresh();
            }
            
            const list_content = Content{ .structure = .{ .list = elem_type.? } };
            const list_var = try self.checker.types.freshFromContent(list_content);
            _ = try self.checker.unify(expr_var, list_var);
        },
        
        else => {
            // For unhandled expression types, panic with details
            @panic("TODO: Implement type checking for this expression type");
        },
    }
    
    return expr_var;
}

// Tests
const testing = std.testing;

// Helper to parse, canonicalize, and type-check a Roc expression
fn parseCanonicalizeAndCheck(allocator: std.mem.Allocator, source: []const u8) !struct {
    ast: AST2,
    cir: CIR2,
    checker: TypeChecker,
    result_var: Var,
} {
    // Parse the source code into an AST
    var ast = try AST2.initCapacity(allocator, 100);
    errdefer ast.deinit(allocator);

    // Create a CommonEnv for tokenization
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Create diagnostics buffer
    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Parse expression using Parser2
    var parser = try parse.Parser2.init(&env, allocator, source, msg_slice, &ast, &byte_slices);
    defer parser.deinit();

    const expr_idx = try parser.parseExpr();

    // Create CIR from AST
    var cir = CIR2.init(&ast);

    // Canonicalize the expression
    const cir_expr_idx = try cir.canonicalizeExpr(allocator, expr_idx);

    // Initialize type checker
    var types_store = Store.init();
    var checker = TypeChecker{
        .gpa = allocator,
        .pool = undefined,
        .types = &types_store,
        .delayed_is_equalish_checks = undefined,
        .abilities = undefined,
        .solved_abilities = undefined,
        .delayed_unify = undefined,
        .in_synth_expr = false,
    };

    // Type check the expression
    var type_check = Self{
        .cir2 = &cir,
        .checker = &checker,
        .allocator = allocator,
        .scopes = std.ArrayList(Scope).init(allocator),
    };
    defer type_check.scopes.deinit();

    try type_check.scopes.append(Scope.init(allocator));
    const result_var = try type_check.checkExpr(cir_expr_idx);

    return .{
        .ast = ast,
        .cir = cir,
        .checker = checker,
        .result_var = result_var,
    };
}

test "typecheck integer literal from source" {
    const allocator = std.testing.allocator;

    // Start with raw Roc syntax
    const source = "42";

    // Parse, canonicalize, and type-check
    var result = try parseCanonicalizeAndCheck(allocator, source);
    defer result.ast.deinit(allocator);
    defer result.checker.types.deinit();

    // Verify that the type is i32
    const content = result.checker.types.getContent(result.result_var);
    try std.testing.expect(content.structure.num.num_compact.int == .i32);
}

test "typecheck string literal from source" {
    const allocator = std.testing.allocator;

    // Start with raw Roc syntax
    const source = "\"hello\"";

    // Parse, canonicalize, and type-check
    var result = try parseCanonicalizeAndCheck(allocator, source);
    defer result.ast.deinit(allocator);
    defer result.checker.types.deinit();

    // Verify that the type is Str
    const content = result.checker.types.getContent(result.result_var);
    try std.testing.expect(content.structure == .str);
}

test "typecheck list literal from source" {
    const allocator = std.testing.allocator;

    // Start with raw Roc syntax
    const source = "[1, 2, 3]";

    // Parse, canonicalize, and type-check
    var result = try parseCanonicalizeAndCheck(allocator, source);
    defer result.ast.deinit(allocator);
    defer result.checker.types.deinit();

    // Verify that the type is List I32
    const content = result.checker.types.getContent(result.result_var);
    try std.testing.expect(content.structure == .list);
    const elem_content = result.checker.types.getContent(content.structure.list);
    try std.testing.expect(elem_content.structure.num.num_compact.int == .i32);
}

test "typecheck empty list from source" {
    const allocator = std.testing.allocator;

    // Start with raw Roc syntax
    const source = "[]";

    // Parse, canonicalize, and type-check
    var result = try parseCanonicalizeAndCheck(allocator, source);
    defer result.ast.deinit(allocator);
    defer result.checker.types.deinit();

    // Verify that the type is List with a flex var element
    const content = result.checker.types.getContent(result.result_var);
    try std.testing.expect(content.structure == .list);
    // The element type should be a flex var
    const elem_content = result.checker.types.getContent(content.structure.list);
    try std.testing.expect(elem_content == .flex_var or elem_content == .flex_able_var);
}

test "typecheck addition from source" {
    const allocator = std.testing.allocator;

    // Start with raw Roc syntax
    const source = "1 + 2";

    // Parse, canonicalize, and type-check
    var result = try parseCanonicalizeAndCheck(allocator, source);
    defer result.ast.deinit(allocator);
    defer result.checker.types.deinit();

    // Verify that the type is numeric (i32)
    const content = result.checker.types.getContent(result.result_var);
    try std.testing.expect(content.structure.num.num_compact.int == .i32);
}

test "typecheck comparison from source" {
    const allocator = std.testing.allocator;

    // Start with raw Roc syntax
    const source = "1 < 2";

    // Parse, canonicalize, and type-check
    var result = try parseCanonicalizeAndCheck(allocator, source);
    defer result.ast.deinit(allocator);
    defer result.checker.types.deinit();

    // The result should be a Bool (represented as a flex var for now)
    const content = result.checker.types.getContent(result.result_var);
    try std.testing.expect(content == .flex_var or content == .flex_able_var);
}