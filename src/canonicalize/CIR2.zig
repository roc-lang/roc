//! Canonical Intermediate Representation (CIR)

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("reporting");
const builtins = @import("builtins");
const parse = @import("parse");
const Diagnostic = parse.AST2.Diagnostic;

const CompactWriter = collections.CompactWriter;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const Position = Region.Position;
const SExprTree = base.SExprTree;
const SExpr = base.SExpr;
const TypeVar = types_mod.Var;
const TypeStore = types_mod.Store;
const Content = types_mod.Content;
const ByteSlices = collections.ByteSlices;
const AST2 = parse.AST2;
const Allocator = std.mem.Allocator;

const CIR = @This();

// Mutable reference to AST - we'll mutate tags in place during canonicalization
ast: *AST2,

// Type store for managing type variables - shared with ModuleEnv
types_store: *TypeStore,

// Diagnostics collected during canonicalization
diagnostics: std.ArrayListUnmanaged(CanDiagnostic),

// Scope state for tracking variable definitions and nested scopes
scope_state: ScopeState,

/// CIR Statement tags - start at 0
pub const StmtTag = enum(u8) {
    assign, // immutable assignment
    init_var, // mutable variable initialization
    reassign, // reassignment to existing var
    type_alias, // type alias definition
    type_anno, // type annotation
    nominal_type, // nominal type definition
    import, // import statement
    match, // match expression
    if_without_else, // if without else
    ret, // return statement
    for_loop, // for loop
    while_loop, // while loop
    crash, // crash statement
    expr, // standalone expression
    malformed, // error case
};

/// Calculate the starting offset for expression tags
/// We start at 100 to avoid collision with AST2.Node.Tag values (which go up to ~68)
const EXPR_TAG_START = 100;

/// CIR Expression tags - start after statement tags
pub const ExprTag = enum(u8) {
    lookup = EXPR_TAG_START, // First expr tag starts at calculated offset
    neg_lookup,
    not_lookup,
    num_literal_i32,
    int_literal_i32,
    num_literal_big,
    int_literal_big,
    frac_literal_small,
    frac_literal_big,
    str_literal_small,
    str_literal_big,
    str_interpolation, // String interpolation
    list_literal,
    empty_list,
    tuple_literal,
    record_literal,
    empty_record,
    apply_ident,
    apply_tag,
    lambda,
    if_else,
    binop_plus,
    binop_minus,
    binop_star,
    binop_slash,
    binop_double_equals,
    binop_not_equals,
    binop_gt,
    binop_gte,
    binop_lt,
    binop_lte,
    binop_and,
    binop_or,
    binop_double_question, // Null coalescing operator ??
    binop_double_slash, // Double slash division //
    binop_thick_arrow, // Thick arrow =>
    binop_thin_arrow, // Thin arrow ->
    binop_colon, // For record fields
    binop_equals, // For assignments in expression context
    block, // Block expression
    record_access,
    record_accessor, // Record accessor function (e.g., .foo)
    dot_num, // Tuple accessor (e.g., .0)
    match, // Match/when expression
    unary_neg, // Unary negation (e.g., -expr)
    unary_not, // Unary not (e.g., !expr)
    unary_double_dot, // Unary double dot (e.g., ..(expr))
    malformed,
};

/// Calculate the starting offset for pattern tags
/// We start at 150 (EXPR_TAG_START=100 + ~48 ExprTag values + buffer)
const PATT_TAG_START = 150;

/// CIR Pattern tags - start after expression tags
pub const PattTag = enum(u8) {
    ident = PATT_TAG_START, // First patt tag starts at calculated offset
    var_ident,
    underscore,
    num_literal_i32,
    malformed,
};

/// Diagnostic errors during canonicalization
pub const CanDiagnostic = struct {
    tag: Tag,
    region: Region,

    pub const Tag = enum {
        // Node placement errors
        pattern_in_expr_context, // Pattern node found where expression was expected
        expr_in_pattern_context, // Expression node found where pattern was expected
        stmt_in_expr_context, // Statement node found where expression was expected
        expr_in_stmt_context, // Expression node found where statement was expected (without semicolon)
        type_in_expr_context, // Type annotation node found where expression was expected

        // Scope errors
        ident_not_in_scope,
        ident_already_defined,

        // Type errors
        type_not_in_scope,

        // Other errors
        unsupported_node, // Node type not yet supported in canonicalization
        malformed_ast, // AST node was already malformed
    };
};

/// Initialize a new CIR that shares storage with the AST and TypeStore
pub fn init(ast: *AST2, type_store: *TypeStore) CIR {
    return .{
        .ast = ast,
        .types_store = type_store,
        .diagnostics = .{},
        .scope_state = .{},
    };
}

/// Initialize CIR with a new AST of the given capacity and a new TypeStore
pub fn initCapacity(allocator: Allocator, capacity: u32, byte_slices: *ByteSlices) !CIR {
    // Create a new AST with the specified capacity
    const ast = try AST2.initCapacity(allocator, capacity);
    // Transfer ownership to a heap-allocated AST so CIR can hold a pointer to it
    const ast_ptr = try allocator.create(AST2);
    ast_ptr.* = ast;

    // Create a new TypeStore
    const types_ptr = try allocator.create(TypeStore);
    types_ptr.* = try TypeStore.initCapacity(allocator, capacity, capacity / 4);

    _ = byte_slices; // byte_slices parameter kept for API compatibility but not used

    return .{
        .ast = ast_ptr,
        .types_store = types_ptr,
        .diagnostics = .{},
        .scope_state = .{},
    };
}

/// Deinitialize the CIR and free all memory
pub fn deinit(self: *CIR, allocator: Allocator) void {
    self.diagnostics.deinit(allocator);
    // Clean up scope state
    for (self.scope_state.scopes.items) |*scope| {
        scope.deinit(allocator);
    }
    self.scope_state.scopes.deinit(allocator);
    self.scope_state.function_regions.deinit(allocator);
    self.scope_state.var_function_regions.deinit(allocator);
    self.scope_state.symbol_table.deinit(allocator);
    // Note: We don't automatically free the AST here because we can't tell
    // if it was created with initCapacity() or passed in with init().
    // Tests using initCapacity() should call deinitWithAST() instead.
}

/// Deinitialize the CIR and also free the AST and TypeStore (for CIRs created with initCapacity)
pub fn deinitWithAST(self: *CIR, allocator: Allocator) void {
    self.diagnostics.deinit(allocator);
    // Clean up scope state
    for (self.scope_state.scopes.items) |*scope| {
        scope.deinit(allocator);
    }
    self.scope_state.scopes.deinit(allocator);
    self.scope_state.function_regions.deinit(allocator);
    self.scope_state.var_function_regions.deinit(allocator);
    self.scope_state.symbol_table.deinit(allocator);
    self.ast.deinit(allocator);
    allocator.destroy(self.ast);
    // Also free the TypeStore if created by initCapacity
    self.types_store.deinit();
    allocator.destroy(self.types_store);
}

// Interface to provide compatibility with tests that expect separate collections

/// Accessor for expressions - provides a view of the AST nodes that are expressions
pub const Exprs = struct {
    cir: *const CIR,

    pub fn len(self: Exprs) usize {
        var count: usize = 0;
        const slice = self.cir.ast.*.nodes.items.slice();
        const tags = slice.items(.tag);

        // Skip index 0 (sentinel node)
        for (tags[1..]) |tag| {
            const tag_value = @as(u8, @intFromEnum(tag));
            // Expression tags are in the range [EXPR_TAG_START, PATT_TAG_START)
            if (tag_value >= EXPR_TAG_START and tag_value < PATT_TAG_START) {
                count += 1;
            }
        }

        return count;
    }
};

/// Accessor for statements - provides a view of the AST nodes that are statements
pub const Stmts = struct {
    cir: *const CIR,

    pub fn len(self: Stmts) usize {
        var count: usize = 0;
        const slice = self.cir.ast.*.nodes.items.slice();
        const tags = slice.items(.tag);

        // Skip index 0 (sentinel node)
        for (tags[1..]) |tag| {
            const tag_value = @as(u8, @intFromEnum(tag));
            // Statement tags are in the range [0, EXPR_TAG_START)
            if (tag_value < EXPR_TAG_START) {
                count += 1;
            }
        }

        return count;
    }
};

/// Accessor for patterns - provides a view of the AST nodes that are patterns
pub const Patts = struct {
    cir: *const CIR,

    pub fn len(self: Patts) usize {
        var count: usize = 0;
        const slice = self.cir.ast.*.nodes.items.slice();
        const tags = slice.items(.tag);

        // Skip index 0 (sentinel node)
        for (tags[1..]) |tag| {
            const tag_value = @as(u8, @intFromEnum(tag));
            // Pattern tags start at PATT_TAG_START
            if (tag_value >= PATT_TAG_START) {
                count += 1;
            }
        }

        return count;
    }
};

/// Accessor for types - provides a view of type information
pub const Types = struct {
    cir: *const CIR,

    pub fn len(self: Types) usize {
        _ = self; // Types accessor doesn't need to examine CIR state
        // CIR doesn't store separate type information - types are handled
        // during type checking phase, not canonicalization
        return 0;
    }
};

/// Get the expressions accessor
pub fn exprs(self: *const CIR) Exprs {
    return Exprs{ .cir = self };
}

/// Get the statements accessor
pub fn stmts(self: *const CIR) Stmts {
    return Stmts{ .cir = self };
}

/// Get the patterns accessor
pub fn patts(self: *const CIR) Patts {
    return Patts{ .cir = self };
}

/// Get the types accessor
pub fn types(self: *const CIR) Types {
    return Types{ .cir = self };
}

/// Push a diagnostic error
pub fn pushDiagnostic(self: *CIR, allocator: Allocator, tag: CanDiagnostic.Tag, region: Region) !void {
    try self.diagnostics.append(allocator, .{
        .tag = tag,
        .region = region,
    });
}

/// Get a mutable pointer to a node's tag for in-place mutation
pub fn getNodeTagPtr(self: *CIR, idx: AST2.Node.Idx) *AST2.Node.Tag {
    // SafeMultiList stores fields separately, so we get pointer to the tag field
    const idx_raw = @as(collections.SafeMultiList(AST2.Node).Idx, @enumFromInt(@intFromEnum(idx)));
    var slice = self.ast.*.nodes.items.slice();
    const tags = slice.items(.tag);
    return &tags[@intFromEnum(idx_raw)];
}

/// Get an immutable node
pub fn getNode(self: *const CIR, idx: AST2.Node.Idx) AST2.Node {
    const node = self.ast.*.nodes.get(@enumFromInt(@intFromEnum(idx)));
    return node;
}

/// Mutate a node's tag in place to a CIR statement tag
pub fn mutateToStmt(self: *CIR, idx: AST2.Node.Idx, new_tag: StmtTag) void {
    const tag_ptr = self.getNodeTagPtr(idx);
    // Cast the tag field to u8 and overwrite it
    const tag_u8_ptr = @as(*u8, @ptrCast(tag_ptr));
    tag_u8_ptr.* = @intFromEnum(new_tag);
}

/// Mutate a node's tag in place to a CIR expression tag
pub fn mutateToExpr(self: *CIR, idx: AST2.Node.Idx, new_tag: ExprTag) void {
    const tag_ptr = self.getNodeTagPtr(idx);
    // Cast the tag field to u8 and overwrite it
    const tag_u8_ptr = @as(*u8, @ptrCast(tag_ptr));
    tag_u8_ptr.* = @intFromEnum(new_tag);
}

/// Mutate a node's tag in place to a CIR pattern tag
pub fn mutateToPatt(self: *CIR, idx: AST2.Node.Idx, new_tag: PattTag) void {
    const tag_ptr = self.getNodeTagPtr(idx);
    // Cast the tag field to u8 and overwrite it
    const tag_u8_ptr = @as(*u8, @ptrCast(tag_ptr));
    tag_u8_ptr.* = @intFromEnum(new_tag);
}

/// Cast an AST node index to a Stmt index (same underlying value)
pub fn asStmtIdx(idx: AST2.Node.Idx) Stmt.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

/// Cast an AST node index to an Expr index (same underlying value)
pub fn asExprIdx(idx: AST2.Node.Idx) Expr.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

/// Cast an AST node index to a Patt index (same underlying value)
/// Use this for immutable patterns. For mutable patterns, use asPattIdxMutable.
pub fn asPattIdx(idx: AST2.Node.Idx) Patt.Idx {
    return Patt.Idx.withMutability(idx, false);
}

/// Cast an AST node index to a mutable Patt index
pub fn asPattIdxMutable(idx: AST2.Node.Idx) Patt.Idx {
    return Patt.Idx.withMutability(idx, true);
}

/// Get a statement "view" - the node has been mutated to have a CIR tag
pub fn getStmt(self: *const CIR, idx: Stmt.Idx) struct {
    tag: Stmt.Tag,
    start: Position,
    payload: AST2.Node.Payload, // We reuse AST's payload
} {
    const node_idx = @as(AST2.Node.Idx, @enumFromInt(@intFromEnum(idx)));
    const node = self.getNode(node_idx);

    // Read the tag as a u8 and interpret it directly as a StmtTag
    const tag_value = @as(u8, @intFromEnum(node.tag));
    const stmt_tag = @as(StmtTag, @enumFromInt(tag_value));

    // Convert StmtTag to Stmt.Tag
    const tag: Stmt.Tag = switch (stmt_tag) {
        .assign => .assign,
        .init_var => .init_var,
        .reassign => .reassign,
        .expr => .expr,
        .malformed => .malformed,
        else => .malformed, // Fallback for unexpected tags
    };

    return .{
        .tag = tag,
        .start = node.region.start,
        .payload = node.payload,
    };
}

/// Get an expression "view" - the node has been mutated to have a CIR tag
pub fn getExpr(self: *const CIR, idx: Expr.Idx) struct {
    tag: Expr.Tag,
    start: Position,
    payload: AST2.Node.Payload, // We reuse AST's payload
} {
    const node_idx = @as(AST2.Node.Idx, @enumFromInt(@intFromEnum(idx)));
    const node = self.getNode(node_idx);

    // Read the tag as a u8 and interpret it directly as an ExprTag
    const tag_value = @as(u8, @intFromEnum(node.tag));

    // Check if this is a valid expression tag
    if (tag_value < EXPR_TAG_START or tag_value >= PATT_TAG_START) {
        // This node is not an expression - return a malformed expression view
        return .{
            .tag = .malformed,
            .start = node.region.start,
            .payload = node.payload,
        };
    }

    const expr_tag = @as(ExprTag, @enumFromInt(tag_value));

    // Convert ExprTag to Expr.Tag
    const tag: Expr.Tag = switch (expr_tag) {
        .lookup => .lookup,
        .neg_lookup => .neg_lookup,
        .not_lookup => .not_lookup,
        .num_literal_i32 => .num_literal_i32,
        .int_literal_i32 => .int_literal_i32,
        .num_literal_big => .num_literal_big,
        .int_literal_big => .int_literal_big,
        .frac_literal_small => .frac_literal_small,
        .frac_literal_big => .frac_literal_big,
        .str_literal_small => .str_literal_small,
        .str_literal_big => .str_literal_big,
        .str_interpolation => .str_interpolation,
        .list_literal => .list_literal,
        .empty_list => .empty_list,
        .tuple_literal => .tuple_literal,
        .record_literal => .record_literal,
        .empty_record => .empty_record,
        .apply_ident => .apply_ident,
        .apply_tag => .apply_tag,
        .lambda => .lambda,
        .if_else => .if_else,
        .binop_plus => .binop_plus,
        .binop_minus => .binop_minus,
        .binop_star => .binop_star,
        .binop_slash => .binop_slash,
        .binop_double_equals => .binop_double_equals,
        .binop_not_equals => .binop_not_equals,
        .binop_gt => .binop_gt,
        .binop_gte => .binop_gte,
        .binop_lt => .binop_lt,
        .binop_lte => .binop_lte,
        .binop_and => .binop_and,
        .binop_or => .binop_or,
        .binop_double_question => .binop_double_question,
        .binop_double_slash => .binop_double_slash,
        .binop_thick_arrow => .binop_thick_arrow,
        .binop_thin_arrow => .binop_thin_arrow,
        .binop_colon => .binop_colon,
        .binop_equals => .binop_equals,
        .block => .block,
        .record_access => .record_access,
        .record_accessor => .record_accessor,
        .dot_num => .dot_num,
        .match => .match,
        .unary_neg => .unary_neg,
        .unary_not => .unary_not,
        .unary_double_dot => .unary_double_dot,
        .malformed => .malformed,
    };

    return .{
        .tag = tag,
        .start = node.region.start,
        .payload = node.payload,
    };
}

/// Get a pattern "view" - the node has been mutated to have a CIR tag
pub fn getPatt(self: *const CIR, idx: Patt.Idx) struct {
    tag: Patt.Tag,
    start: Position,
    payload: AST2.Node.Payload, // We reuse AST's payload
    is_mutable: bool, // Mutability is encoded in the index
} {
    // Extract the actual node index from the pattern index (handles sign bit)
    const node_idx = idx.toNodeIdx();
    const node = self.getNode(node_idx);

    // Read the tag as a u8 and interpret it directly as a PattTag
    const tag_value = @as(u8, @intFromEnum(node.tag));
    const patt_tag = @as(PattTag, @enumFromInt(tag_value));

    // Convert PattTag to Patt.Tag
    const tag: Patt.Tag = switch (patt_tag) {
        .ident => .ident,
        .var_ident => .var_ident,
        .underscore => .underscore,
        .num_literal_i32 => .num_literal_i32,
        .malformed => .malformed,
    };

    return .{
        .tag = tag,
        .start = node.region.start,
        .payload = node.payload,
        .is_mutable = idx.isMutable(), // Extract mutability from the index
    };
}

/// Get a binop from the AST's NodeSlices
/// The payload already contains the correct index - we just cast the index types
pub fn getBinOp(self: *const CIR, comptime IdxType: type, binop_idx: collections.NodeSlices(AST2.Node.Idx).Idx) struct {
    lhs: IdxType,
    rhs: IdxType,
} {
    // Get the binop from AST's NodeSlices
    const ast_binop = self.ast.*.node_slices.binOp(binop_idx);

    // The indices are already correct, just need to cast them to the right type
    return .{
        .lhs = @as(IdxType, @enumFromInt(@intFromEnum(ast_binop.lhs))),
        .rhs = @as(IdxType, @enumFromInt(@intFromEnum(ast_binop.rhs))),
    };
}

// In AST, we have a Node type which represents uncategorized AST nodes.
// In CIR, we categorize each of these nodes so that we can work with more
// nicely typed subsets.
//
// We reuse the same node structure in memory as what
// we had for the AST, so that we don't have to copy the nodes and their
// region info over; instead, we just change the tags to apply the categorization.
//
// That said, each of the different CIR node types (Stmt, Expr, Patt, Type)
// do not need to have unique Tag integers because we can always tell from context
// which type of node we have. So Stmt and Expr and Patt and Type can each use a Tag
// of 1 in memory to mean something else, because we never use Tag number to distinguish
// between whether we have a Stmt, Expr, Patt, or Type; rather, we only use it to
// distinguish *within* one of those types.
//
// We use the same NodeSlices structure from collections, parameterized with the
// appropriate index type (e.g., Stmt.Idx, Expr.Idx, etc.). During canonicalization,
// we'll cast the AST's NodeSlices to the appropriate CIR NodeSlices type.

pub const Stmt = struct {
    tag: Stmt.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Stmt.Payload, // u32 union of extra information that varies based on tag

    /// Index to a Stmt in the CIR.
    /// Although this is an i32, indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing indices in collections like NodeSlices.
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        assign, // .binop_equals - immutable assignment
        init_var, // .binop_equals with var_lc - mutable variable initialization
        reassign, // .binop_equals to existing var - reassignment
        type_alias, // .binop_colon
        type_anno, // .binop_colon
        nominal_type, // .binop_colon_equals
        import, // .import
        match, // .match
        if_without_else, // .if_without_else
        ret, // .ret
        for_loop, // .for_loop
        while_loop, // .while_loop
        crash, // .crash
        expr, // standalone expression
        malformed, // error case
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.

        // For assignment statements (assign, init_var, reassign)
        assignment: struct {
            pattern_idx: Patt.Idx, // Pattern being assigned to (or existing pattern for reassign)
            expr_idx: Expr.Idx, // Expression being assigned
        },

        // For standalone expressions
        expr_idx: Expr.Idx,

        // For imports
        import_idx: u32, // Index into imports table

        // For crash statements
        crash_msg: ByteSlices.Idx, // String message for crash
    };
};

pub const Patt = struct {
    tag: Patt.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Patt.Payload, // u32 union of extra information that varies based on tag

    /// Index to a Patt in the CIR.
    /// Although this is an i32, indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing indices in collections like NodeSlices.
    ///
    /// Additionally, we can encode mutability information in the sign bit:
    /// - Positive values indicate immutable patterns (regular let bindings)
    /// - Negative values indicate mutable patterns (var bindings)
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }

        /// Create a pattern index with mutability encoded
        pub fn withMutability(idx: AST2.Node.Idx, is_mutable: bool) Idx {
            const base_value = @intFromEnum(idx);
            if (is_mutable) {
                // Set the sign bit to indicate mutability
                // We negate the value + 1 to avoid -0
                return @enumFromInt(-@as(i32, @intCast(base_value)) - 1);
            } else {
                return @enumFromInt(@as(i32, @intCast(base_value)));
            }
        }

        /// Check if this pattern index represents a mutable pattern
        pub fn isMutable(self: Idx) bool {
            return @intFromEnum(self) < 0;
        }

        /// Get the underlying AST node index, stripping mutability information
        pub fn toNodeIdx(self: Idx) AST2.Node.Idx {
            const value = @intFromEnum(self);
            if (value < 0) {
                // Undo the negation and -1
                return @enumFromInt(@as(u32, @intCast(-value - 1)));
            } else {
                return @enumFromInt(@as(u32, @intCast(value)));
            }
        }

        /// Set the mutability of this pattern index
        pub fn setMutability(self: Idx, is_mutable: bool) Idx {
            const node_idx = self.toNodeIdx();
            return withMutability(node_idx, is_mutable);
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        ident, // .lc
        var_ident, // .var_lc
        double_dot_ident, // .double_dot_lc (e.g. `..others`)
        as, // .as
        applied_tag, // .apply_uc
        nominal, // Nominal type patterns
        nominal_external, // External nominal type patterns
        record_destructure, // .record_literal
        tuple_destructure, // .tuple_literal
        list_destructure, // .list_literal
        num_literal_i32, // .num_literal_i32
        int_literal_i32, // .int_literal_i32
        int_literal_i64, // .int_literal_i64
        frac_literal_small, // .frac_literal_small
        str_literal_small, // .str_literal_small
        num_literal_big, // .num_literal_big
        int_literal_big, // .int_literal_big
        frac_literal_big, // .frac_literal_big
        str_literal_big, // .str_literal_big
        str_interpolation, // e.g. `"abc${def}ghi${jkl}mno"` - payload is list of nodes that will all get appended togther
        list_literal, // e.g. `[1, 2, 3]` - note that this is nonempty; .empty_list has its own variant
        tuple_literal, // e.g. `(foo, bar)` - we know it's a tuple literal because of the commas
        record_literal, // e.g. `{ foo, bar }` or `{ foo, }` - only records have commas; `{ foo }` is a block
        underscore, // .underscore
        empty_record, // e.g. `{}` - no data inside; we just store region and that's it.
        empty_list, // e.g. `[]` - no data inside; we just store region and that's it.
        malformed, // error case - used when a pattern is malformed
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.
        // Add other payload fields as needed
    };
};

pub const Expr = struct {
    tag: Expr.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Expr.Payload, // u32 union of extra information that varies based on tag

    /// Index to an Expr in the CIR.
    /// Although this is an i32, indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing indices in collections like NodeSlices.
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        lookup, // .var_lc (e.g. `foo`)
        neg_lookup, // .neg_lc (e.g. `-foo`)
        not_lookup, // .not_lc (e.g. `!foo`)
        record_accessor, // .dot_lc (e.g. `.foo`)
        double_dot_ident, // .double_dot_lc (e.g. `..others`)
        dot_num, // dot followed by number (e.g. `.0`) - this is a tuple accessor

        // Literals
        num_literal_i32, // e.g. `42`
        int_literal_i32, // e.g. `0x42`
        frac_literal_small, // e.g. `0.2` - fits in a 32-bit SmallDec
        str_literal_small, // Null-terminated ASCII with escapes resolved (if it contains '\0', must use .str_literal_big)
        num_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bignums
        int_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bigints
        frac_literal_big, // Like a bigint literal but stores 2 lengths first, for digits before/after decimal point
        str_literal_big, // Byte length followed by UTF-8 bytes (across multiple AstData entries) with all escapes resolved.
        str_interpolation, // e.g. `"abc${def}ghi${jkl}mno"` - payload is list of nodes that will all get appended togther
        list_literal, // e.g. `[1, 2, 3]` - note that this is nonempty; .empty_list has its own variant
        tuple_literal, // e.g. `(foo, bar)` - we know it's a tuple literal because of the commas
        record_literal, // e.g. `{ foo, bar }` or `{ foo, }` - only records have commas; `{ foo }` is a block

        // Binary operators
        binop_double_equals, // .binop_double_equals
        binop_not_equals, // .binop_not_equals
        binop_plus, // .binop_plus
        binop_minus, // .binop_minus
        binop_star, // .binop_star
        binop_slash, // .binop_slash
        binop_double_slash, // .binop_double_slash
        binop_double_question, // .binop_double_question
        binop_gt, // .binop_gt
        binop_gte, // .binop_gte
        binop_lt, // .binop_lt
        binop_lte, // .binop_lte
        binop_thick_arrow, // .binop_thick_arrow
        binop_thin_arrow, // .binop_thin_arrow
        binop_and, // .binop_and
        binop_or, // .binop_or
        binop_colon, // .binop_colon (for record fields)
        binop_equals, // .binop_equals (for assignments in expression context)
        record_access, // .binop_dot with .lc for rhs (e.g. `foo.bar`)
        method_call, // .binop_dot with .apply_lc for rhs (e.g. `foo.bar()`)

        // Other
        apply_ident, // e.g. `foo(bar, baz)`
        apply_tag, // e.g. `Foo(bar, baz)` or `(foo(bar, baz))(blah, etc)`
        apply_anon, // e.g. `(foo(bar, baz))(blah, etc)`
        block, // Block with curly braces, e.g. `{ expr1, expr2, ... }` - could end up being a record (expr or destructure)
        empty_record, // e.g. `{}` - no data inside; we just store region and that's it.
        empty_list, // e.g. `[]` - no data inside; we just store region and that's it.
        lambda, // e.g. `|x, y| x + y` - payload stores a slice of body_then_args
        match, // e.g. `match cond { Ok(a) => a Err(b) => b }` - needs to store cond as well as branches
        if_else, // e.g. `if cond then_branch else_branch` - needs to store cond as well as branches. if-exprs must have else.
        unary_not, // e.g. `!(foo())` - note that `!foo` is special-cased to .not_lc instead
        unary_neg, // e.g. `-(foo())` - note that `-foo` is special-cased to .neg_lc instead
        unary_double_dot, // e.g. `..(foo())` - note that `..foo` is special-cased to .double_dot_lc instead)
        malformed, // e.g. tokenization or parsing failed (stores a Diagnostic.Tag)
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.

        list_elems: u32, // Number of elements in the list literal
        block_nodes: collections.NodeSlices(Expr.Idx).Idx, // Number of nodes in a block (or fields in a record, if it turns out to be a record)
        body_then_args: collections.NodeSlices(Expr.Idx).Idx, // For lambdas, the Expr.Idx of the body followed by 0+ Expr.Idx entries for args.
        if_branches: u32, // Branches before the `else` - each branch begins with a conditional node
        binop: collections.NodeSlices(Expr.Idx).Idx, // Pass this to NodeSlices.binOp() to get lhs and rhs
        ident: Ident.Idx, // For both .uc and .lc tags

        // Number literals that are small enough to be stored inline right here - by far the most common case
        num_literal_i32: i32, // e.g. `42`
        int_literal_i32: i32, // e.g. `0x42`
        frac_literal_small_dec: i32, // e.g. `0.2`

        // Number literals that don't fit 4B, and must be instead stored in a side table.
        num_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bignums
        int_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bigints
        frac_literal_big: ByteSlices.Idx, // Like a bigint literal but stores 2 lengths first, for digits before and after decimal

        // String literals
        str_literal_small: [4]u8, // Null-terminated ASCII bytes (if there's a '\0' in it, then it must be .str_literal_big
        str_literal_big: ByteSlices.Idx, // Stores length followed by UTF-8 bytes (which can include \0 bytes).
        str_interpolated_nodes: collections.NodeSlices(Expr.Idx).Idx, // Stores length followed by node indices (some will be string literal nodes)

        import_nodes: collections.NodeSlices(Expr.Idx).Idx, // Stores imported module nodes for import statements

        malformed: Diagnostic.Tag, // Malformed nodes store the diagnostic tag
    };
};

pub const Type = struct {
    tag: Type.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Type.Payload, // u32 union of extra information that varies based on tag

    /// Index to a Type in the CIR.
    /// Although this is an i32, indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing indices in collections like NodeSlices.
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        var_ident, // .var_lc (e.g. `var foo`)
        neg_ident, // .neg_lc (e.g. `-foo`)
        not_ident, // .not_lc (e.g. `!foo`)
        dot_ident, // .dot_lc (e.g. `.foo`)
        double_dot_ident, // .double_dot_lc (e.g. `..others`)
        dot_num, // dot followed by number (e.g. `.0`) - this is a tuple accessor

        // Literals
        num_literal_i32, // e.g. `42`
        int_literal_i32, // e.g. `0x42`
        frac_literal_small, // e.g. `0.2` - fits in a 32-bit SmallDec
        str_literal_small, // Null-terminated ASCII with escapes resolved (if it contains '\0', must use .str_literal_big)
        num_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bignums
        int_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bigints
        frac_literal_big, // Like a bigint literal but stores 2 lengths first, for digits before/after decimal point
        str_literal_big, // Byte length followed by UTF-8 bytes (across multiple AstData entries) with all escapes resolved.
        str_interpolation, // e.g. `"abc${def}ghi${jkl}mno"` - payload is list of nodes that will all get appended togther
        list_literal, // e.g. `[1, 2, 3]` - note that this is nonempty; .empty_list has its own variant
        tuple_literal, // e.g. `(foo, bar)` - we know it's a tuple literal because of the commas
        record_literal, // e.g. `{ foo, bar }` or `{ foo, }` - only records have commas; `{ foo }` is a block

        // Binary operators
        binop_double_equals, // binop_double_equals,
        binop_not_equals, // binop_not_equals,
        binop_plus, // binop_plus,
        binop_minus, // binop_minus,
        binop_star, // binop_star,
        binop_slash, // binop_slash,
        binop_double_slash, // binop_double_slash,
        binop_double_question, // binop_double_question,
        binop_gt, // binop_gt,
        binop_gte, // binop_gte,
        binop_lt, // binop_lt,
        binop_lte, // binop_lte,
        binop_thick_arrow, // binop_thick_arrow,
        binop_thin_arrow, // binop_thin_arrow,
        binop_and, // binop_and,
        binop_or, // binop_or,

        // Other
        apply_ident, // e.g. `foo(bar, baz)`
        apply_tag, // e.g. `Foo(bar, baz)` or `(foo(bar, baz))(blah, etc)`
        apply_anon, // e.g. `(foo(bar, baz))(blah, etc)`
        block, // Block with curly braces, e.g. `{ expr1, expr2, ... }` - could end up being a record (expr or destructure)
        empty_record, // e.g. `{}` - no data inside; we just store region and that's it.
        empty_list, // e.g. `[]` - no data inside; we just store region and that's it.
        lambda, // e.g. `|x, y| x + y` - payload stores a slice of body_then_args
        match, // e.g. `match cond { Ok(a) => a Err(b) => b }` - needs to store cond as well as branches
        if_else, // e.g. `if cond then_branch else_branch` - needs to store cond as well as branches. if-exprs must have else.
        unary_not, // e.g. `!(foo())` - note that `!foo` is special-cased to .not_lc instead
        unary_neg, // e.g. `-(foo())` - note that `-foo` is special-cased to .neg_lc instead
        unary_double_dot, // e.g. `..(foo())` - note that `..foo` is special-cased to .double_dot_lc instead)
        malformed, // e.g. tokenization or parsing failed (stores a Diagnostic.Tag)
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.

        list_elems: u32, // Number of elements in the list literal
        block_nodes: collections.NodeSlices(Type.Idx).Idx, // Number of nodes in a block (or fields in a record, if it turns out to be a record)
        body_then_args: collections.NodeSlices(Type.Idx).Idx, // For lambdas, the Type.Idx of the body followed by 0+ Type.Idx entries for args.
        if_branches: u32, // Branches before the `else` - each branch begins with a conditional node
        binop: collections.NodeSlices(Type.Idx).Idx, // Pass this to NodeSlices.binOp() to get lhs and rhs
        ident: Ident.Idx, // For both .uc and .lc tags

        // Number literals that are small enough to be stored inline right here - by far the most common case
        num_literal_i32: i32, // e.g. `42`
        int_literal_i32: i32, // e.g. `0x42`
        frac_literal_small_dec: i32, // e.g. `0.2`

        // Number literals that don't fit 4B, and must be instead stored in a side table.
        num_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bignums
        int_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bigints
        frac_literal_big: ByteSlices.Idx, // Like a bigint literal but stores 2 lengths first, for digits before and after decimal

        // String literals
        str_literal_small: [4]u8, // Null-terminated ASCII bytes (if there's a '\0' in it, then it must be .str_literal_big
        str_literal_big: ByteSlices.Idx, // Stores length followed by UTF-8 bytes (which can include \0 bytes).
        str_interpolated_nodes: collections.NodeSlices(Type.Idx).Idx, // Stores length followed by node indices (some will be string literal nodes)

        import_nodes: collections.NodeSlices(Type.Idx).Idx, // Stores imported module nodes for import statements

        malformed: Diagnostic.Tag, // Malformed nodes store the diagnostic tag
    };
};

/// Ensure that a type variable exists at the given node index
/// Creates variables as needed to fill gaps
fn ensureTypeVarExists(self: *CIR, node_idx: AST2.Node.Idx) !void {
    const target_idx = @intFromEnum(node_idx);

    // Create variables up to the target index if needed
    while (self.types_store.len() <= target_idx) {
        _ = try self.types_store.fresh();
    }
}

/// Canonicalize an AST node that should be an expression
/// Mutates the node's tag in place to the appropriate CIR expression tag
/// Reports errors if the node is not valid in expression context
fn canonicalizeLambdaParams(self: *CIR, allocator: Allocator, node_idx: AST2.Node.Idx, raw_src: []const u8, idents: *const Ident.Store) error{OutOfMemory}!void {
    const node = self.getNode(node_idx);

    switch (node.tag) {
        .underscore => {
            // Underscore parameter - canonicalize as pattern
            _ = try self.canonicalizePatt(allocator, node_idx);
        },
        .lc, .var_lc => {
            // Identifier parameter - canonicalize as pattern
            const patt_idx = try self.canonicalizePatt(allocator, node_idx);
            // Register the parameter in scope
            try self.scope_state.addIdent(allocator, node.payload.ident, patt_idx);
            try self.scope_state.recordVarPattern(allocator, patt_idx);
        },
        .tuple_literal => {
            // Multiple parameters passed as a tuple: |a, b| -> parsed as tuple_literal(a, b)
            // Iterate through each element and canonicalize as a parameter
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                while (iter.next()) |param_node| {
                    try self.canonicalizeLambdaParams(allocator, param_node, raw_src, idents);
                }
            }
            // Don't mutate the tuple_literal node itself - it's just a container for params
        },
        .binop_pipe => {
            // Multiple parameters: |a| b | rest
            // This is nested binop_pipe for multiple parameters
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            // Recursively handle left parameter
            try self.canonicalizeLambdaParams(allocator, ast_binop.lhs, raw_src, idents);
            // Recursively handle right parameter(s)
            try self.canonicalizeLambdaParams(allocator, ast_binop.rhs, raw_src, idents);
        },
        else => {
            // Other patterns might be supported in the future
            _ = try self.canonicalizePatt(allocator, node_idx);
        },
    }
}

pub fn canonicalizeExpr(self: *CIR, allocator: Allocator, node_idx: AST2.Node.Idx, raw_src: []const u8, idents: *const Ident.Store) error{OutOfMemory}!Expr.Idx {
    const node = self.getNode(node_idx);

    // Check if this node has already been canonicalized (mutated)
    const tag_value = @as(u8, @intFromEnum(node.tag));
    if (tag_value >= EXPR_TAG_START and tag_value < PATT_TAG_START) {
        // This node has already been canonicalized as an expression
        return asExprIdx(node_idx);
    }

    // Calculate the proper region for this node BEFORE any mutations
    const node_region = self.ast.getRegion(node_idx);

    switch (node.tag) {
        // Expression nodes - mutate tag in place
        .num_literal_i32 => {
            self.mutateToExpr(node_idx, .num_literal_i32);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for numeric literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 0 } } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .int_literal_i32 => {
            self.mutateToExpr(node_idx, .int_literal_i32);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for integer literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .int_precision = .i32 } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .frac_literal_small => {
            self.mutateToExpr(node_idx, .frac_literal_small);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for fractional literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .frac_precision = .f64 } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .frac_literal_big => {
            self.mutateToExpr(node_idx, .frac_literal_big);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for big fractional literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .frac_precision = .f64 } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .num_literal_big => {
            self.mutateToExpr(node_idx, .num_literal_big);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for big numeric literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .num_unbound = .{ .sign_needed = false, .bits_needed = 128 } } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .int_literal_big => {
            self.mutateToExpr(node_idx, .int_literal_big);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for big integer literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .num = .{ .int_precision = .i128 } } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .lc, .var_lc => {
            // Identifiers become lookups in expression context
            self.mutateToExpr(node_idx, .lookup);

            const ident_idx = node.payload.ident;

            // Ensure type variable exists for the lookup
            try self.ensureTypeVarExists(node_idx);

            // Look up the definition in the symbol table and connect types
            if (self.scope_state.symbol_table.get(ident_idx)) |def_node_idx| {
                // Connect this lookup's type to its definition's type
                // Both the lookup and its definition share the same type variable
                // So we need to ensure the definition's type variable exists too
                try self.ensureTypeVarExists(def_node_idx);

                // The types are connected via shared type variables
                // Type unification happens during type checking phase
            }

            return asExprIdx(node_idx);
        },

        // String literals
        .str_literal_small => {
            self.mutateToExpr(node_idx, .str_literal_small);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for string literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .str = {} } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },
        .str_literal_big => {
            self.mutateToExpr(node_idx, .str_literal_big);
            // Ensure type variable exists
            try self.ensureTypeVarExists(node_idx);
            // Set type content for string literal
            const var_idx = @as(TypeVar, @enumFromInt(@intFromEnum(node_idx)));
            const content = Content{ .structure = .{ .str = {} } };
            try self.types_store.setVarContent(var_idx, content);
            return asExprIdx(node_idx);
        },

        // Binary operators
        .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_colon, .binop_equals => {
            // Get the binop data from AST's node slices
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // Recursively canonicalize left and right operands
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            // Map AST binop tag to CIR expr tag and mutate in place
            const expr_tag: ExprTag = switch (node.tag) {
                .binop_plus => .binop_plus,
                .binop_minus => .binop_minus,
                .binop_star => .binop_star,
                .binop_slash => .binop_slash,
                .binop_colon => .binop_colon,
                .binop_equals => .binop_equals,
                else => unreachable,
            };
            self.mutateToExpr(node_idx, expr_tag);
            return asExprIdx(node_idx);
        },

        // Pattern nodes - these are errors in expression context!
        .underscore => {
            // Underscore pattern found in expression context
            try self.pushDiagnostic(allocator, .pattern_in_expr_context, node_region);
            self.mutateToExpr(node_idx, .malformed);
            return asExprIdx(node_idx);
        },
        .uc => {
            // Uppercase identifier - this is a tag constructor without arguments
            // Store the tag name and mutate to an apply_tag expression
            self.mutateToExpr(node_idx, .apply_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Block - could be a single-field record or an actual block
        .block => {
            // Get the nodes in the block
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);

            // Count how many nodes we have
            var count: usize = 0;
            var first_node: ?AST2.Node.Idx = null;
            while (iter.next()) |n| {
                if (count == 0) first_node = n;
                count += 1;
            }

            // Check if this is a single-field record case:
            // { x: Foo } → single field record
            // { x } → block with single expression
            // { x: Foo, y: Bar } → would have been parsed as record_literal, not block
            // { x: Foo x = blah } → block with type annotation
            if (count == 1) {
                // Single node in block - check if it's a colon binop
                if (first_node) |fn_idx| {
                    const fn_node = self.getNode(fn_idx);
                    if (fn_node.tag == .binop_colon) {
                        // Single colon binop → treat as single-field record
                        self.mutateToExpr(node_idx, .record_literal);

                        // Canonicalize the field (the colon binop)
                        _ = try self.canonicalizeExpr(allocator, fn_idx, raw_src, idents);

                        return asExprIdx(node_idx);
                    }
                }
            }

            // Otherwise it's a regular block
            // Push a new scope for the block
            try self.scope_state.pushScope(allocator, false); // false = not a function boundary
            defer self.scope_state.popScope(allocator);

            // Canonicalize all nodes in the block with proper scope tracking
            iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                const n_node = self.getNode(n);
                // Check if this is a statement or expression
                if (n_node.tag == .binop_equals) {
                    // Assignment in a block - convert to expression
                    _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
                } else if (n_node.tag == .import) {
                    // Import statement in a block - not allowed, convert to malformed
                    const n_region = self.ast.getRegion(n);
                    try self.pushDiagnostic(allocator, .unsupported_node, n_region);
                    self.mutateToExpr(n, .malformed);
                    try self.ensureTypeVarExists(n);
                } else {
                    // This is an expression
                    _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .block);
            return asExprIdx(node_idx);
        },

        // Record literal - definitely a record
        .record_literal => {
            // Canonicalize all fields
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
            }

            self.mutateToExpr(node_idx, .record_literal);
            return asExprIdx(node_idx);
        },

        // Tuple literal
        .tuple_literal => {
            // Canonicalize all elements
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
            }
            self.mutateToExpr(node_idx, .tuple_literal);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // List literal
        .list_literal => {
            // Canonicalize all elements
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
            }
            self.mutateToExpr(node_idx, .list_literal);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Malformed nodes
        .malformed => {
            // Already malformed - just keep it that way
            self.mutateToExpr(node_idx, .malformed);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Unary operators
        .unary_neg => {
            // Unary negation: -expr
            // The operand is stored in .nodes as a single-element slice
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            if (iter.next()) |operand_idx| {
                _ = try self.canonicalizeExpr(allocator, operand_idx, raw_src, idents);
            }

            self.mutateToExpr(node_idx, .unary_neg);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .unary_not => {
            // Unary not: !expr
            // The operand is stored in .nodes as a single-element slice
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            if (iter.next()) |operand_idx| {
                _ = try self.canonicalizeExpr(allocator, operand_idx, raw_src, idents);
            }

            self.mutateToExpr(node_idx, .unary_not);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // If-else expressions
        .if_else => {
            // If-else uses .if_branches payload which is a u32 that can be cast to NodeSlices.Idx
            const if_branches_u32 = node.payload.if_branches;
            const nodes_idx = @as(collections.NodeSlices(AST2.Node.Idx).Idx, @enumFromInt(if_branches_u32));
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);

            // Canonicalize condition
            if (iter.next()) |condition| {
                _ = try self.canonicalizeExpr(allocator, condition, raw_src, idents);
            }

            // Canonicalize then branch
            if (iter.next()) |then_branch| {
                _ = try self.canonicalizeExpr(allocator, then_branch, raw_src, idents);
            }

            // Canonicalize else branch
            if (iter.next()) |else_branch| {
                _ = try self.canonicalizeExpr(allocator, else_branch, raw_src, idents);
            }

            // Mutate to if_else expression
            self.mutateToExpr(node_idx, .if_else);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Function and tag applications
        .apply_uc => {
            // Tag constructor with optional payload
            // Structure: first node is tag, second node is args (often a tuple)
            const nodes_idx = node.payload.nodes;

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the tag/constructor
                if (iter.next()) |tag_node| {
                    _ = try self.canonicalizeExpr(allocator, tag_node, raw_src, idents);
                }

                // Second node contains the payload arguments (could be a tuple or single expr)
                if (iter.next()) |args_node| {
                    const args_tag = self.ast.*.tag(args_node);
                    if (args_tag == .tuple_literal) {
                        // Arguments are in a tuple - canonicalize each element
                        const tuple_nodes_idx = self.ast.*.payload(args_node).nodes;
                        if (!tuple_nodes_idx.isNil()) {
                            var tuple_iter = self.ast.*.node_slices.nodes(&tuple_nodes_idx);
                            while (tuple_iter.next()) |arg| {
                                _ = try self.canonicalizeExpr(allocator, arg, raw_src, idents);
                            }
                        }
                    } else {
                        // Single argument - canonicalize it directly
                        _ = try self.canonicalizeExpr(allocator, args_node, raw_src, idents);
                    }
                }
            }

            self.mutateToExpr(node_idx, .apply_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .apply_lc => {
            // Function application - canonicalize function and arguments
            // Structure: first node is function, second node is args (often a tuple)
            const nodes_idx = node.payload.nodes;

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the function being called
                if (iter.next()) |func_node| {
                    _ = try self.canonicalizeExpr(allocator, func_node, raw_src, idents);
                }

                // Second node contains the arguments (could be a tuple or single expr)
                if (iter.next()) |args_node| {
                    const args_tag = self.ast.*.tag(args_node);
                    if (args_tag == .tuple_literal) {
                        // Arguments are in a tuple - canonicalize each element
                        const tuple_nodes_idx = self.ast.*.payload(args_node).nodes;
                        if (!tuple_nodes_idx.isNil()) {
                            var tuple_iter = self.ast.*.node_slices.nodes(&tuple_nodes_idx);
                            while (tuple_iter.next()) |arg| {
                                _ = try self.canonicalizeExpr(allocator, arg, raw_src, idents);
                            }
                        }
                    } else {
                        // Single argument - canonicalize it directly
                        _ = try self.canonicalizeExpr(allocator, args_node, raw_src, idents);
                    }
                }
            }

            self.mutateToExpr(node_idx, .apply_ident);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .apply_anon => {
            // Anonymous function application (e.g., (foo(bar))(baz))
            // Structure: first node is expr, second node is args (often a tuple)
            const nodes_idx = node.payload.nodes;

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the expression that evaluates to a function
                if (iter.next()) |func_expr| {
                    _ = try self.canonicalizeExpr(allocator, func_expr, raw_src, idents);
                }

                // Second node contains the arguments (could be a tuple or single expr)
                if (iter.next()) |args_node| {
                    const args_tag = self.ast.*.tag(args_node);
                    if (args_tag == .tuple_literal) {
                        // Arguments are in a tuple - canonicalize each element
                        const tuple_nodes_idx = self.ast.*.payload(args_node).nodes;
                        if (!tuple_nodes_idx.isNil()) {
                            var tuple_iter = self.ast.*.node_slices.nodes(&tuple_nodes_idx);
                            while (tuple_iter.next()) |arg| {
                                _ = try self.canonicalizeExpr(allocator, arg, raw_src, idents);
                            }
                        }
                    } else {
                        // Single argument - canonicalize it directly
                        _ = try self.canonicalizeExpr(allocator, args_node, raw_src, idents);
                    }
                }
            }

            self.mutateToExpr(node_idx, .apply_ident);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Accessor expressions
        .dot_lc => {
            // Record accessor function (e.g., .foo)
            self.mutateToExpr(node_idx, .record_accessor);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .dot_num => {
            // Tuple accessor (e.g., .0)
            self.mutateToExpr(node_idx, .dot_num);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Special identifier cases
        .neg_lc => {
            // Negated identifier (e.g., -foo) - this is unary minus on an identifier
            self.mutateToExpr(node_idx, .neg_lookup);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .not_lc => {
            // Bang identifier (e.g., !foo) - effectful function call
            self.mutateToExpr(node_idx, .not_lookup);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Other comparison operators
        .binop_double_equals, .binop_not_equals, .binop_gt, .binop_gte, .binop_lt, .binop_lte => {
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            const expr_tag: ExprTag = switch (node.tag) {
                .binop_double_equals => .binop_double_equals,
                .binop_not_equals => .binop_not_equals,
                .binop_gt => .binop_gt,
                .binop_gte => .binop_gte,
                .binop_lt => .binop_lt,
                .binop_lte => .binop_lte,
                else => unreachable,
            };
            self.mutateToExpr(node_idx, expr_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Logical operators
        .binop_and, .binop_or => {
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            const expr_tag: ExprTag = switch (node.tag) {
                .binop_and => .binop_and,
                .binop_or => .binop_or,
                else => unreachable,
            };
            self.mutateToExpr(node_idx, expr_tag);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Null coalescing operator
        .binop_double_question => {
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_double_question);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Match expressions
        .match => {
            // Match expressions have a scrutinee followed by pattern-body pairs
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);

            // First node is the scrutinee
            if (iter.next()) |scrutinee| {
                _ = try self.canonicalizeExpr(allocator, scrutinee, raw_src, idents);
            }

            // Remaining nodes are pattern-body pairs
            while (iter.next()) |pattern| {
                // Canonicalize the pattern
                _ = try self.canonicalizePatt(allocator, pattern);

                // Next node is the body for this branch
                if (iter.next()) |body| {
                    _ = try self.canonicalizeExpr(allocator, body, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .match);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Lambda expressions
        .lambda => {
            // Lambda expression with parameters and body
            const nodes_idx = node.payload.body_then_args;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First is body
                if (iter.next()) |body_node| {
                    _ = try self.canonicalizeExpr(allocator, body_node, raw_src, idents);
                }

                // Rest are parameters
                while (iter.next()) |param_node| {
                    _ = try self.canonicalizeLambdaParams(allocator, param_node, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .lambda);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Lambda expressions OR module access (binop_pipe is overloaded)
        .binop_pipe => {
            // binop_pipe can be:
            // 1. Lambda syntax: |params| body
            // 2. Module access: Module.Field

            // First check what kind of payload we have
            // binop_pipe should always have binop payload
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // Check if this is module access by looking at the left side
            const lhs_tag = self.ast.*.tag(ast_binop.lhs);
            const rhs_tag = self.ast.*.tag(ast_binop.rhs);

            if (lhs_tag == .uc and rhs_tag == .uc) {
                // This is module access like Bool.True
                // Treat it as a lookup for now
                self.mutateToExpr(node_idx, .lookup);
                try self.ensureTypeVarExists(node_idx);
                return asExprIdx(node_idx);
            } else {
                // This is lambda syntax: |params| body
                // Canonicalize the parameters (left side)
                // Parameters might be underscore, identifiers, or nested binop_pipe for multiple params
                _ = try self.canonicalizeLambdaParams(allocator, ast_binop.lhs, raw_src, idents);

                // Canonicalize the body (right side)
                _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                // Mutate to lambda expression
                self.mutateToExpr(node_idx, .lambda);
                try self.ensureTypeVarExists(node_idx);
                return asExprIdx(node_idx);
            }
        },

        // Statement nodes - error in expression context
        .import => {
            try self.pushDiagnostic(allocator, .stmt_in_expr_context, node_region);
            self.mutateToExpr(node_idx, .malformed);
            return asExprIdx(node_idx);
        },
        .expect => {
            // Expect statement - canonicalize the expression being tested
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                if (iter.next()) |expr_node| {
                    _ = try self.canonicalizeExpr(allocator, expr_node, raw_src, idents);
                }
            }
            self.mutateToExpr(node_idx, .malformed); // Expect is a statement, not an expression
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Record spread operator (e.g., ..person in { ..person, age: 31 })
        .double_dot_lc => {
            // This represents a record being spread - treat as a special lookup for now
            self.mutateToExpr(node_idx, .lookup);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Module/package qualified identifiers (e.g., Module.Type, pkg.Module.Type)
        .uc_dot_ucs => {
            // Module-qualified type or tag (e.g., Module.Type or Module.Tag)
            self.mutateToExpr(node_idx, .lookup);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .lc_dot_ucs => {
            // Package-qualified module access (e.g., pkg.Module.Type)
            self.mutateToExpr(node_idx, .lookup);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Special lambda without args (e.g., || expr)
        .lambda_no_args => {
            // Lambda with no arguments, just has body stored in nodes field
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                if (iter.next()) |body_node| {
                    _ = try self.canonicalizeExpr(allocator, body_node, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .lambda);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Loop constructs
        .for_loop => {
            // For loop: for pattern in iterable { body... }
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the pattern
                if (iter.next()) |pattern_node| {
                    _ = try self.canonicalizePatt(allocator, pattern_node);
                }

                // Second node is the iterable
                if (iter.next()) |iterable_node| {
                    _ = try self.canonicalizeExpr(allocator, iterable_node, raw_src, idents);
                }

                // Remaining nodes are the body
                while (iter.next()) |body_node| {
                    _ = try self.canonicalizeExpr(allocator, body_node, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .block); // For loops are blocks for now
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .while_loop => {
            // While loop: while condition { body... }
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the condition
                if (iter.next()) |cond_node| {
                    _ = try self.canonicalizeExpr(allocator, cond_node, raw_src, idents);
                }

                // Remaining nodes are the body
                while (iter.next()) |body_node| {
                    _ = try self.canonicalizeExpr(allocator, body_node, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .block); // While loops are blocks for now
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // If without else (e.g., if cond do_something!())
        .if_without_else => {
            // if_without_else has if_branches field, not nodes
            // The payload stores exactly 2 nodes: condition and body
            const if_branches_u32 = node.payload.if_branches;
            const nodes_idx = @as(collections.NodeSlices(AST2.Node.Idx).Idx, @enumFromInt(if_branches_u32));

            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);

                // First node is the condition
                if (iter.next()) |cond_node| {
                    _ = try self.canonicalizeExpr(allocator, cond_node, raw_src, idents);
                }

                // Second node is the body
                if (iter.next()) |body_node| {
                    _ = try self.canonicalizeExpr(allocator, body_node, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .if_else); // If without else is still an if
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Apply to module (used in where clauses)
        .apply_module => {
            // Module application in where clause (e.g., module(a))
            const nodes_idx = node.payload.nodes;
            if (!nodes_idx.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&nodes_idx);
                while (iter.next()) |arg_node| {
                    _ = try self.canonicalizeExpr(allocator, arg_node, raw_src, idents);
                }
            }

            self.mutateToExpr(node_idx, .apply_ident); // Module application is function application
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Type-level binops that might appear in expression context
        .binop_dot => {
            // Field access (e.g., foo.bar)
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .record_accessor); // Field access like foo.bar
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_colon_equals => {
            // Assignment operator :=
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_colon); // Assignment is just colon for now
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_as => {
            // Type ascription (e.g., expr as Type)
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            // The right side is a type, but we'll process it as an expression for now
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_colon); // Type ascription uses colon for now
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_thick_arrow => {
            // Pattern match arrow =>
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizePatt(allocator, ast_binop.lhs);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_thick_arrow);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_thin_arrow => {
            // Function type arrow ->
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_thin_arrow);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_where => {
            // Where clause (type constraints)
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_colon); // Where clause uses colon for now
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
        .binop_platform => {
            // Platform specification in app header
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            self.mutateToExpr(node_idx, .binop_colon); // Platform spec uses colon for now
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Placeholder for unimplemented features
        .ellipsis => {
            // Triple dot ... placeholder
            self.mutateToExpr(node_idx, .malformed); // Ellipsis is malformed
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // String interpolation
        .str_interpolation => {
            // String interpolation: "text ${expr} more text"
            const nodes_idx = node.payload.nodes;
            var iter = self.ast.*.node_slices.nodes(&nodes_idx);
            while (iter.next()) |n| {
                _ = try self.canonicalizeExpr(allocator, n, raw_src, idents);
            }
            self.mutateToExpr(node_idx, .str_interpolation);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        // Unary double dot
        .unary_double_dot => {
            // Unary double dot: ..(expr)
            if (!node.payload.nodes.isNil()) {
                var iter = self.ast.*.node_slices.nodes(&node.payload.nodes);
                if (iter.next()) |operand| {
                    _ = try self.canonicalizeExpr(allocator, operand, raw_src, idents);
                }
            }
            self.mutateToExpr(node_idx, .unary_double_dot);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },

        else => {
            // This should never happen if we've handled all cases
            std.log.err("Unhandled AST node tag in canonicalizeExpr: {}", .{node.tag});
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            self.mutateToExpr(node_idx, .malformed);
            try self.ensureTypeVarExists(node_idx);
            return asExprIdx(node_idx);
        },
    }
}

test "CIR2 basic initialization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a ByteSlices instance for the test
    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Initialize CIR with estimated capacity
    var cir = try CIR.initCapacity(allocator, 100, &byte_slices);
    defer cir.deinitWithAST(allocator);

    // Verify initial state
    // The accessors count actual CIR nodes (based on tag values), not raw AST nodes
    // Initially, no nodes have been canonicalized, so counts are 0
    try testing.expectEqual(@as(usize, 0), cir.stmts().len());
    try testing.expectEqual(@as(usize, 0), cir.exprs().len());
    try testing.expectEqual(@as(usize, 0), cir.patts().len());
    try testing.expectEqual(@as(usize, 0), cir.types().len());
}

test "CIR2 canonicalize simple number literal" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser2 = parse.Parser2;

    // Parse a simple number literal
    const source = "42";

    // Create heap-allocated AST for CIR to use
    const ast_ptr = try allocator.create(AST2);
    ast_ptr.* = try AST2.initCapacity(allocator, 100);
    defer {
        ast_ptr.deinit(allocator);
        allocator.destroy(ast_ptr);
    }

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];

    var parser = try Parser2.init(&env, allocator, source, msg_slice, ast_ptr, &byte_slices);
    defer parser.deinit();

    // Parse as an expression
    const node_idx = try parser.parseExpr();

    // Verify we got a num_literal_i32 node
    const node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(node.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 42), node.payload.num_literal_i32);

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Now canonicalize it to CIR2
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    const expr_idx = try cir.canonicalizeExpr(allocator, node_idx, source, &env.idents);

    // Verify the CIR2 expression
    const expr = cir.getExpr(expr_idx);
    try testing.expect(expr.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 42), expr.payload.num_literal_i32);

    // Verify we created exactly one expression
    try testing.expectEqual(@as(usize, 1), cir.exprs().len());
}

/// Canonicalize an AST node that should be a statement
//     const testing = std.testing;
//     const allocator = testing.allocator;
//     const Parser2 = parse.Parser2;

//     // Parse a simple identifier
//     const source = "foo";

//     var ast = try AST2.initCapacity(allocator, 100);
//     defer ast.deinit(allocator);

//     var env = try base.CommonEnv.init(allocator, source);
//     defer env.deinit(allocator);

//     var byte_slices = ByteSlices{ .entries = .{} };
//     defer byte_slices.entries.deinit(allocator);

//     var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
//     const msg_slice = messages[0..];

//     var parser = try Parser2.init(&env, allocator, source, msg_slice, &ast, &byte_slices);
//     defer parser.deinit();

//     // Parse as an expression
//     const node_idx = try parser.parseExpr();

//     // Verify we got an lc (lowercase identifier) node in AST
//     const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
//     try testing.expect(node.tag == .lc);

//     // Now canonicalize it to CIR2
//     var cir = CIR.init(&ast);
//     defer cir.deinit(allocator);

//     // Canonicalize using the actual CIR2 method - it should mutate the AST node in place
//     const expr_idx = try cir.canonicalizeExpr(allocator, node_idx, source, &env.idents);

//     // Verify the CIR2 expression is now a lookup (categorized as an expression)
//     const expr = cir.getExpr(expr_idx);
//     try testing.expect(expr.tag == .lookup);
//     try testing.expectEqual(node.start, expr.start); // Same position
//     const testing = std.testing;
//     const allocator = testing.allocator;

//     // Create an uppercase identifier node (pattern) in AST
//     var ast = try AST2.initCapacity(allocator, 10);
//     defer ast.deinit(allocator);

//     var byte_slices = ByteSlices{ .entries = .{} };
//     defer byte_slices.entries.deinit(allocator);

//     // Create identifier for "Foo"
//     const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 }; // Dummy identifier

//     // Manually create an uppercase identifier node
//     const node_idx = try ast.nodes.append(allocator, .{
//         .tag = .uc,
//         .start = Position{ .offset = 0 },
//         .payload = .{ .ident = ident_idx },
//     });

//     // Initialize CIR
//     var cir = try CIR.initCapacity(allocator, 10, &byte_slices);
//     defer cir.deinitWithAST(allocator);

//     // Try to canonicalize uppercase identifier as expression - should produce error
//     const expr_idx = try cir.canonicalizeExpr(allocator, @enumFromInt(@intFromEnum(node_idx)));

//     // Verify we got a malformed expression
//     const expr = cir.getExpr(expr_idx);
//     try testing.expect(expr.tag == .malformed);

//     // Verify we got a diagnostic error
//     try testing.expectEqual(@as(usize, 1), cir.diagnostics.items.len);
//     const diagnostic = cir.diagnostics.items[0];
//     try testing.expect(diagnostic.tag == .pattern_in_expr_context);
// }

/// Canonicalize an AST node that should be a statement
/// Reports errors if the node is not valid in statement context
pub fn canonicalizeStmt(self: *CIR, allocator: Allocator, node_idx: AST2.Node.Idx, raw_src: []const u8, idents: *const Ident.Store) error{OutOfMemory}!Stmt.Idx {
    const node = self.getNode(node_idx);

    // Check if this node has already been canonicalized (mutated)
    const tag_value = @as(u8, @intFromEnum(node.tag));
    if (tag_value < EXPR_TAG_START) {
        // This node has already been canonicalized as a statement
        return asStmtIdx(node_idx);
    }

    // Calculate the proper region for this node BEFORE any mutations
    const node_region = self.ast.getRegion(node_idx);

    switch (node.tag) {
        // Assignment-like statements
        .binop_equals => {
            // Get the binop data from AST's node slices
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // Check left-hand side to determine if this is a var declaration or reassignment
            const lhs_node = self.ast.*.nodes.get(@enumFromInt(@intFromEnum(ast_binop.lhs)));

            switch (lhs_node.tag) {
                .var_lc => {
                    // This is a mutable variable declaration: `var x = expr`
                    // Canonicalize the pattern (lhs) as mutable
                    const patt_idx = try self.canonicalizePattMutable(allocator, ast_binop.lhs);

                    // For var_lc nodes, the identifier is in the ident field of the payload
                    // Get the identifier from the var_lc node
                    const var_ident = lhs_node.payload.ident;

                    // Register this as a mutable variable (mutability is encoded in patt_idx)
                    try self.scope_state.addIdent(allocator, var_ident, patt_idx);
                    try self.scope_state.recordVarPattern(allocator, patt_idx);

                    // Add to symbol table so lookups can find this definition
                    try self.scope_state.symbol_table.put(allocator, var_ident, ast_binop.lhs);

                    // Canonicalize the expression (rhs)
                    _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                    // Mutate the AST node to have the CIR statement tag
                    self.mutateToStmt(node_idx, .init_var);

                    // The existing AST binop payload contains the pattern/expr structure
                    // patt_idx and expr_idx are used above in scope management

                    return asStmtIdx(node_idx);
                },
                .lc => {
                    // Could be immutable declaration or reassignment
                    const ident = lhs_node.payload.ident;

                    if (self.scope_state.lookupIdent(ident)) |existing_patt_idx| {
                        // Identifier already exists - check if it's a var
                        if (ScopeState.isVarPattern(existing_patt_idx)) {
                            // This is a var reassignment
                            // Check for cross-function boundary reassignment
                            if (self.scope_state.isVarReassignmentAcrossFunctionBoundary(existing_patt_idx)) {
                                try self.pushDiagnostic(allocator, .ident_already_defined, node_region);
                            }

                            // Canonicalize as reassignment
                            // Note: For reassignment, we don't canonicalize LHS as it should be a reference to existing var
                            // Canonicalize the RHS expression - this mutates it in place
                            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                            // Mutate AST node to reassignment statement
                            self.mutateToStmt(node_idx, .reassign);
                            // existing_patt_idx is used above in scope boundary check
                            return asStmtIdx(node_idx);
                        } else {
                            // Error: trying to reassign immutable variable
                            try self.pushDiagnostic(allocator, .ident_already_defined, node_region);

                            // Mutate to malformed statement - this AST node is semantically invalid
                            self.mutateToStmt(node_idx, .malformed);
                            return asStmtIdx(node_idx);
                        }
                    } else {
                        // This is an immutable variable declaration: `x = expr`
                        // Canonicalize the pattern (lhs)
                        const patt_idx = try self.canonicalizePatt(allocator, ast_binop.lhs);

                        // Register this as an immutable variable
                        try self.scope_state.addIdent(allocator, ident, patt_idx);

                        // Add to symbol table so lookups can find this definition
                        try self.scope_state.symbol_table.put(allocator, ident, ast_binop.lhs);

                        // Canonicalize the expression (rhs) - this mutates the expression nodes in place
                        _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                        // Mutate AST node to assignment statement
                        self.mutateToStmt(node_idx, .assign);
                        return asStmtIdx(node_idx);
                    }
                },
                else => {
                    // Complex pattern on LHS - treat as pattern match assignment
                    // Canonicalize both sides - this mutates the nodes in place
                    _ = try self.canonicalizePatt(allocator, ast_binop.lhs);
                    _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

                    // Mutate AST node to assignment statement
                    self.mutateToStmt(node_idx, .assign);
                    return asStmtIdx(node_idx);
                },
            }
        },

        // Return statement
        .ret => {
            // Parse return statement with expression
            // Get the expression from nodes payload
            const nodes_iter = self.ast.*.node_slices.nodes(&node.payload.nodes);
            var iter = nodes_iter;

            if (iter.next()) |expr_node_idx| {
                // Canonicalize the return expression
                _ = try self.canonicalizeExpr(allocator, expr_node_idx, raw_src, idents);
            }

            // Mutate to return statement
            self.mutateToStmt(node_idx, .ret);
            return asStmtIdx(node_idx);
        },

        // Crash statement
        .crash => {
            // Parse crash statement with expression
            // Get the expression from nodes payload
            const nodes_iter = self.ast.*.node_slices.nodes(&node.payload.nodes);
            var iter = nodes_iter;

            if (iter.next()) |expr_node_idx| {
                // Canonicalize the crash expression
                _ = try self.canonicalizeExpr(allocator, expr_node_idx, raw_src, idents);
            }

            // Mutate to crash statement
            self.mutateToStmt(node_idx, .crash);
            return asStmtIdx(node_idx);
        },

        // Type annotations
        .binop_colon => {
            // Type annotation: `name : Type`
            // Check if this actually has a binop payload
            // It should always have a binop payload for binop_colon
            const ast_binop = self.ast.*.node_slices.binOp(node.payload.binop);

            // The left side is the name being annotated
            // The right side is the type expression
            // For now, we'll just process both sides
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs, raw_src, idents);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs, raw_src, idents);

            // Mutate to a type annotation statement
            self.mutateToStmt(node_idx, .type_anno);
            return asStmtIdx(node_idx);
        },

        // Import statements
        .import => {
            // Import statement: import module.name
            // For now, just mark it as an import statement
            self.mutateToStmt(node_idx, .import);
            return asStmtIdx(node_idx);
        },

        // Expression nodes - error in statement context without assignment
        .num_literal_i32, .int_literal_i32, .lc => {
            try self.pushDiagnostic(allocator, .expr_in_stmt_context, node_region);
            // Mutate to malformed statement - expressions aren't valid in statement context
            self.mutateToStmt(node_idx, .malformed);
            return asStmtIdx(node_idx);
        },

        else => {
            // Unsupported statement type
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            // Mutate to malformed statement - this AST node type isn't supported in statement context
            self.mutateToStmt(node_idx, .malformed);
            return asStmtIdx(node_idx);
        },
    }
}

/// Canonicalize an AST node that should be a pattern (immutable)
pub fn canonicalizePatt(self: *CIR, allocator: Allocator, node_idx: AST2.Node.Idx) !Patt.Idx {
    return self.canonicalizePattWithMutability(allocator, self.ast, node_idx, false);
}

/// Canonicalize an AST node that should be a pattern (mutable)
pub fn canonicalizePattMutable(self: *CIR, allocator: Allocator, node_idx: AST2.Node.Idx) !Patt.Idx {
    return self.canonicalizePattWithMutability(allocator, self.ast, node_idx, true);
}

/// Canonicalize an AST node that should be a pattern with specified mutability
pub fn canonicalizePattWithMutability(self: *CIR, allocator: Allocator, ast_param: *const AST2, node_idx: AST2.Node.Idx, is_mutable: bool) !Patt.Idx {
    // Calculate the proper region for this node BEFORE any mutations
    const node_region = ast_param.getRegion(node_idx);
    const node = ast_param.nodes.get(@enumFromInt(@intFromEnum(node_idx)));

    switch (node.tag) {
        .lc => {
            // Lowercase identifier pattern
            // Mutate the AST node's tag in place
            self.mutateToPatt(node_idx, if (is_mutable) .var_ident else .ident);

            // Return the index with mutability encoded
            return Patt.Idx.withMutability(node_idx, is_mutable);
        },
        .var_lc => {
            // Mutable identifier pattern (var_lc always means mutable)
            // Mutate the AST node's tag in place
            self.mutateToPatt(node_idx, .var_ident);

            // Return the index with mutability encoded (always mutable for var_lc)
            return Patt.Idx.withMutability(node_idx, true);
        },
        .underscore => {
            // Underscore pattern
            // Mutate the AST node's tag in place
            self.mutateToPatt(node_idx, .underscore);

            // Return the index with mutability encoded (underscore can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },
        .num_literal_i32 => {
            // Number literal pattern
            // Mutate the AST node's tag in place
            self.mutateToPatt(node_idx, .num_literal_i32);

            // Return the index with mutability encoded (literals can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },

        // Expression nodes - error in pattern context
        .binop_plus, .binop_minus, .binop_star, .binop_slash => {
            try self.pushDiagnostic(allocator, .expr_in_pattern_context, node_region);
            // Mutate to malformed pattern tag
            self.mutateToPatt(node_idx, .malformed);
            // Return with mutability encoded (malformed patterns can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },

        else => {
            // Unsupported pattern type
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            // Mutate to malformed pattern tag
            self.mutateToPatt(node_idx, .malformed);
            // Return with mutability encoded (malformed patterns can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },
    }
}

/// Canonicalize a header - processes exposed items and generates type information
pub fn canonicalizeHeader(self: *CIR, allocator: Allocator, raw_src: []const u8, idents: *const Ident.Store) !void {
    // If there's no header, nothing to do
    if (self.ast.header == null) {
        return;
    }

    const header = self.ast.header.?;

    // Process the header based on its type
    switch (header) {
        .app => |app| {
            // For app headers, process the packages which now includes platform with provides
            var packages_iter = self.ast.node_slices.nodes(&app.packages);
            while (packages_iter.next()) |field_idx| {
                // Each field is a binop_colon
                const field_node = self.ast.nodes.get(@enumFromInt(@intFromEnum(field_idx)));
                if (field_node.tag == .binop_colon) {
                    const field_binop = self.ast.node_slices.binOp(field_node.payload.binop);
                    // Check if the RHS is a platform binop
                    const rhs_node = self.ast.nodes.get(@enumFromInt(@intFromEnum(field_binop.rhs)));
                    if (rhs_node.tag == .binop_platform) {
                        // This is the platform field with provides list
                        const platform_binop = self.ast.node_slices.binOp(rhs_node.payload.binop);
                        // The RHS of platform binop is the provides list (stored as a block)
                        const provides_block = self.ast.nodes.get(@enumFromInt(@intFromEnum(platform_binop.rhs)));
                        if (provides_block.tag == .block) {
                            var provides_iter = self.ast.node_slices.nodes(&provides_block.payload.nodes);
                            while (provides_iter.next()) |node_idx| {
                                // Each provided item needs to be canonicalized
                                const node = self.ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
                                switch (node.tag) {
                                    .lc => {
                                        // This is a value being provided - canonicalize as expression
                                        _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
                                    },
                                    .uc => {
                                        // This is a type being provided - would need type canonicalization
                                        // Currently types are handled separately
                                    },
                                    else => {
                                        // Other nodes in provides list - canonicalize as expressions
                                        _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
                                    },
                                }
                            }
                        }
                    }
                }
            }
        },
        .module => |mod| {
            // For module headers, process the exposes list
            var iter = self.ast.node_slices.nodes(&mod.exposes);
            while (iter.next()) |node_idx| {
                // Each exposed item needs to be canonicalized
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .package => |pkg| {
            // For package headers, process the exposes list
            var iter = self.ast.node_slices.nodes(&pkg.exposes);
            while (iter.next()) |node_idx| {
                // Each exposed item needs to be canonicalized
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .platform => |plat| {
            // For platform headers, process both exposes and provides
            var exposes_iter = self.ast.node_slices.nodes(&plat.exposes);
            while (exposes_iter.next()) |node_idx| {
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }

            var provides_iter = self.ast.node_slices.nodes(&plat.provides);
            while (provides_iter.next()) |node_idx| {
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .hosted => |host| {
            // For hosted headers, process the exposes list
            var iter = self.ast.node_slices.nodes(&host.exposes);
            while (iter.next()) |node_idx| {
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .interface => |iface| {
            // For interface headers, process the exposes list
            var iter = self.ast.node_slices.nodes(&iface.exposes);
            while (iter.next()) |node_idx| {
                _ = try self.canonicalizeExpr(allocator, node_idx, raw_src, idents);
            }
        },
        .malformed => {
            // Nothing to canonicalize for malformed headers
        },
    }
}

/// Scope management during canonicalization - follows the pattern from Can.zig
pub const ScopeState = struct {
    /// Stack of scopes for nested blocks
    scopes: std.ArrayListUnmanaged(Scope) = .{},

    /// Stack of function regions for tracking var reassignment across function boundaries
    function_regions: std.ArrayListUnmanaged(Region) = .{},

    /// Maps var patterns to the function region they were declared in
    /// We only need to track mutable vars here since immutable ones can't be reassigned across boundaries
    var_function_regions: std.AutoHashMapUnmanaged(Patt.Idx, Region) = .{},

    /// Symbol table mapping identifier indices to their definition node indices
    /// This allows lookups to find their corresponding definitions for type checking
    symbol_table: std.AutoHashMapUnmanaged(Ident.Idx, AST2.Node.Idx) = .{},

    /// Push a new scope onto the stack
    pub fn pushScope(self: *ScopeState, allocator: Allocator, is_function_boundary: bool) !void {
        try self.scopes.append(allocator, Scope.init(is_function_boundary));
    }

    /// Pop the current scope
    pub fn popScope(self: *ScopeState, allocator: Allocator) void {
        if (self.scopes.pop()) |scope| {
            var mut_scope = scope;
            mut_scope.deinit(allocator);
        }
    }

    /// Get the current scope (top of stack)
    pub fn currentScope(self: *ScopeState) ?*Scope {
        if (self.scopes.items.len > 0) {
            return &self.scopes.items[self.scopes.items.len - 1];
        }
        return null;
    }

    /// Enter a function (push function region)
    pub fn enterFunction(self: *ScopeState, allocator: Allocator, region: Region) !void {
        try self.function_regions.append(allocator, region);
    }

    /// Exit a function (pop function region)
    pub fn exitFunction(self: *ScopeState) void {
        _ = self.function_regions.popOrNull();
    }

    /// Get current function region
    pub fn getCurrentFunctionRegion(self: *const ScopeState) ?Region {
        if (self.function_regions.items.len > 0) {
            return self.function_regions.items[self.function_regions.items.len - 1];
        }
        return null;
    }

    /// Record a var pattern and its function region
    pub fn recordVarPattern(self: *ScopeState, allocator: Allocator, patt_idx: Patt.Idx) !void {
        // The mutability is already encoded in the sign bit of patt_idx
        // We just need to record the function region for mutable patterns
        if (patt_idx.isMutable()) {
            if (self.getCurrentFunctionRegion()) |function_region| {
                try self.var_function_regions.put(allocator, patt_idx, function_region);
            }
        }
    }

    /// Check if a pattern is a var
    pub fn isVarPattern(patt_idx: Patt.Idx) bool {
        // Use the sign bit encoding to check mutability
        return patt_idx.isMutable();
    }

    /// Check if a var reassignment crosses function boundaries
    pub fn isVarReassignmentAcrossFunctionBoundary(self: *const ScopeState, patt_idx: Patt.Idx) bool {
        if (self.var_function_regions.get(patt_idx)) |var_function_region| {
            if (self.getCurrentFunctionRegion()) |current_function_region| {
                return !var_function_region.eq(current_function_region);
            }
        }
        return false;
    }

    /// Lookup an identifier in the scope stack
    pub fn lookupIdent(self: *const ScopeState, ident: Ident.Idx) ?Patt.Idx {
        // Search from innermost to outermost scope
        var i = self.scopes.items.len;
        while (i > 0) : (i -= 1) {
            const scope = &self.scopes.items[i - 1];
            if (scope.idents.get(ident)) |patt_idx| {
                return patt_idx;
            }
        }
        return null;
    }

    /// Add an identifier to the current scope
    pub fn addIdent(self: *ScopeState, allocator: Allocator, ident: Ident.Idx, patt_idx: Patt.Idx) !void {
        if (self.currentScope()) |scope| {
            try scope.idents.put(allocator, ident, patt_idx);
        }
    }

    pub fn deinit(self: *ScopeState, allocator: Allocator) void {
        for (self.scopes.items) |*scope| {
            scope.deinit(allocator);
        }
        self.scopes.deinit(allocator);
        self.function_regions.deinit(allocator);
        self.var_function_regions.deinit(allocator);
    }
};

/// A single scope in the scope stack
pub const Scope = struct {
    /// Maps identifiers to their pattern indices in this scope
    idents: std.AutoHashMapUnmanaged(Ident.Idx, Patt.Idx) = .{},

    /// Whether this scope is a function boundary
    is_function_boundary: bool,

    pub fn init(is_function_boundary: bool) Scope {
        return .{
            .idents = .{},
            .is_function_boundary = is_function_boundary,
        };
    }

    pub fn deinit(self: *Scope, allocator: Allocator) void {
        self.idents.deinit(allocator);
    }
};

test "CIR2 canonicalize mutable variable declaration" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser2 = parse.Parser2;

    // Parse a mutable variable declaration
    const source = "var x = 10";

    // Create heap-allocated AST for CIR to use
    const ast_ptr = try allocator.create(AST2);
    ast_ptr.* = try AST2.initCapacity(allocator, 100);
    defer {
        ast_ptr.deinit(allocator);
        allocator.destroy(ast_ptr);
    }

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];

    var parser = try Parser2.init(&env, allocator, source, msg_slice, ast_ptr, &byte_slices);
    defer parser.deinit();

    // Parse the statement
    const node_idx = try parser.parseExpr();

    // Verify we got a binop_equals node with var_lc on the left
    const node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(node.tag == .binop_equals);

    // Check the left side is var_lc
    const ast_binop = ast_ptr.node_slices.binOp(node.payload.binop);
    const lhs_node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(ast_binop.lhs)));
    try testing.expect(lhs_node.tag == .var_lc);

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Now canonicalize it to CIR2
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    // Initialize with a root scope
    try cir.scope_state.pushScope(allocator, false);

    const stmt_idx = try cir.canonicalizeStmt(allocator, node_idx, source, &env.idents);

    // Verify the statement was created
    const stmt = cir.getStmt(stmt_idx);
    try testing.expect(stmt.tag == .init_var); // Mutable variable initialization

    // Verify we created one statement
    try testing.expectEqual(@as(usize, 1), cir.stmts().len());
}

test "CIR2 error: expression in statement context" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a simple source for testing
    const source = "42";
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Create heap-allocated AST
    const ast_ptr = try allocator.create(AST2);
    ast_ptr.* = try AST2.initCapacity(allocator, 10);
    defer {
        ast_ptr.deinit(allocator);
        allocator.destroy(ast_ptr);
    }

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create a number literal node (expression)
    const node_idx = try ast_ptr.appendNode(allocator, Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 2 } }, .num_literal_i32, .{ .num_literal_i32 = 42 });

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Initialize CIR
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    // Initialize with a root scope
    try cir.scope_state.pushScope(allocator, false);

    // Try to canonicalize the expression as a statement
    // This should create a malformed statement and add a diagnostic
    const stmt_idx = try cir.canonicalizeStmt(allocator, node_idx, source, &env.idents);

    // Verify we got a malformed statement
    const stmt = cir.getStmt(stmt_idx);
    try testing.expect(stmt.tag == .malformed);

    // Verify a diagnostic was added
    try testing.expectEqual(@as(usize, 1), cir.diagnostics.items.len);
    const diagnostic = cir.diagnostics.items[0];
    try testing.expect(diagnostic.tag == .expr_in_stmt_context);
}

test "CIR2 demonstrates in-place tag mutation" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a simple source for testing
    const source = "x";
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Create heap-allocated AST
    const ast_ptr = try allocator.create(AST2);
    ast_ptr.* = try AST2.initCapacity(allocator, 10);
    defer {
        ast_ptr.deinit(allocator);
        allocator.destroy(ast_ptr);
    }

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create an identifier node
    const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const node_idx = try ast_ptr.appendNode(allocator, Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 3 } }, .lc, .{ .ident = ident_idx });

    // Verify initial AST tag
    const initial_node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(initial_node.tag == .lc);

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Initialize CIR
    var cir = CIR.init(ast_ptr, &types_store);
    defer cir.deinit(allocator);

    // Canonicalize as expression - this should mutate the tag in-place
    const expr_idx = try cir.canonicalizeExpr(allocator, node_idx, source, &env.idents);

    // The returned index should be the same as the input (just cast to Expr.Idx)
    try testing.expectEqual(@intFromEnum(node_idx), @intFromEnum(expr_idx));

    // Verify the AST node's tag was mutated to expr_lookup
    const mutated_node = ast_ptr.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    const tag_value = @as(u8, @intFromEnum(mutated_node.tag));
    const expr_tag = @as(ExprTag, @enumFromInt(tag_value));
    try testing.expect(expr_tag == .lookup);

    // Verify the CIR can read it as an expression
    const expr = cir.getExpr(expr_idx);
    try testing.expect(expr.tag == .lookup);
}

// Note: Reassignment tracking and immutable variable reassignment error tests
// are better tested at the integration level with real Roc code parsing
// rather than manual AST construction

test "sign bit encoding: basic mutability encoding" {
    const testing = std.testing;

    // Test that we can encode and decode mutability correctly
    const node_idx = @as(AST2.Node.Idx, @enumFromInt(42));

    // Create immutable pattern index
    const immutable_idx = Patt.Idx.withMutability(node_idx, false);
    try testing.expect(!immutable_idx.isMutable());
    try testing.expectEqual(node_idx, immutable_idx.toNodeIdx());

    // Create mutable pattern index
    const mutable_idx = Patt.Idx.withMutability(node_idx, true);
    try testing.expect(mutable_idx.isMutable());
    try testing.expectEqual(node_idx, mutable_idx.toNodeIdx());

    // Test setMutability
    const changed_idx = immutable_idx.setMutability(true);
    try testing.expect(changed_idx.isMutable());
    try testing.expectEqual(node_idx, changed_idx.toNodeIdx());
}

test "sign bit encoding: edge cases" {
    const testing = std.testing;

    // Test with index 0
    const node_idx_0 = @as(AST2.Node.Idx, @enumFromInt(0));
    const mutable_0 = Patt.Idx.withMutability(node_idx_0, true);
    try testing.expect(mutable_0.isMutable());
    try testing.expectEqual(node_idx_0, mutable_0.toNodeIdx());

    // Test with large index
    const node_idx_large = @as(AST2.Node.Idx, @enumFromInt(1000000));
    const mutable_large = Patt.Idx.withMutability(node_idx_large, true);
    try testing.expect(mutable_large.isMutable());
    try testing.expectEqual(node_idx_large, mutable_large.toNodeIdx());

    // Test that mutability is preserved through multiple conversions
    const idx1 = Patt.Idx.withMutability(node_idx_large, true);
    const node_idx_extracted = idx1.toNodeIdx();
    const idx2 = Patt.Idx.withMutability(node_idx_extracted, idx1.isMutable());
    try testing.expectEqual(idx1, idx2);
}

test "sign bit encoding: mutable vs immutable pattern canonicalization" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a simple source for testing
    const source = "x y";
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    // Create a TypeStore for testing
    var types_store = try TypeStore.initCapacity(allocator, 100, 10);
    defer types_store.deinit();

    // Create CIR with shared AST storage
    var cir = CIR.init(&ast, &types_store);
    defer cir.deinit(allocator);

    // Create two identifier nodes
    const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };

    const node1_idx = try ast.appendNode(allocator, Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 3 } }, .lc, .{ .ident = ident_idx });

    const node2_idx = try ast.appendNode(allocator, Region{ .start = Position{ .offset = 10 }, .end = Position{ .offset = 13 } }, .lc, .{ .ident = ident_idx });

    // Canonicalize one as immutable, one as mutable
    const immutable_patt = try cir.canonicalizePatt(allocator, node1_idx);
    const mutable_patt = try cir.canonicalizePattMutable(allocator, node2_idx);

    // Verify mutability encoding
    try testing.expect(!immutable_patt.isMutable());
    try testing.expect(mutable_patt.isMutable());

    // Verify the underlying nodes are correct
    try testing.expectEqual(node1_idx, immutable_patt.toNodeIdx());
    try testing.expectEqual(node2_idx, mutable_patt.toNodeIdx());

    // Verify getPatt returns correct mutability info
    const patt1_view = cir.getPatt(immutable_patt);
    const patt2_view = cir.getPatt(mutable_patt);

    try testing.expect(!patt1_view.is_mutable);
    try testing.expect(patt2_view.is_mutable);
}
