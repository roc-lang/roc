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
const ByteSlices = collections.ByteSlices;
const AST2 = parse.AST2;
const Allocator = std.mem.Allocator;

const CIR = @This();

// Storage for canonicalized nodes - these share the same memory layout as AST nodes
// but have different tags after canonicalization
stmts: collections.SafeMultiList(Stmt),
exprs: collections.SafeMultiList(Expr),
patts: collections.SafeMultiList(Patt),
types: collections.SafeMultiList(Type),

// Shared slice storage - these can be cast from AST NodeSlices during canonicalization
stmt_slices: collections.NodeSlices(Stmt.Idx),
expr_slices: collections.NodeSlices(Expr.Idx),
patt_slices: collections.NodeSlices(Patt.Idx),
type_slices: collections.NodeSlices(Type.Idx),

// Shared byte storage - references the same ByteSlices from the AST
byte_slices: *ByteSlices,

// Diagnostics collected during canonicalization
diagnostics: std.ArrayListUnmanaged(CanDiagnostic),

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

/// Initialize a new CIR with pre-allocated capacity
pub fn initCapacity(allocator: Allocator, estimated_node_count: usize, byte_slices: *ByteSlices) Allocator.Error!CIR {
    var stmts = collections.SafeMultiList(Stmt){};
    try stmts.ensureTotalCapacity(allocator, estimated_node_count);
    
    var exprs = collections.SafeMultiList(Expr){};
    try exprs.ensureTotalCapacity(allocator, estimated_node_count);
    
    var patts = collections.SafeMultiList(Patt){};
    try patts.ensureTotalCapacity(allocator, estimated_node_count);
    
    var types = collections.SafeMultiList(Type){};
    try types.ensureTotalCapacity(allocator, estimated_node_count);

    return .{
        .stmts = stmts,
        .exprs = exprs,
        .patts = patts,
        .types = types,
        .stmt_slices = .{ .entries = .{} },
        .expr_slices = .{ .entries = .{} },
        .patt_slices = .{ .entries = .{} },
        .type_slices = .{ .entries = .{} },
        .byte_slices = byte_slices,
        .diagnostics = .{},
    };
}

/// Deinitialize the CIR and free all memory
pub fn deinit(self: *CIR, allocator: Allocator) void {
    self.stmts.deinit(allocator);
    self.exprs.deinit(allocator);
    self.patts.deinit(allocator);
    self.types.deinit(allocator);
    self.stmt_slices.entries.deinit(allocator);
    self.expr_slices.entries.deinit(allocator);
    self.patt_slices.entries.deinit(allocator);
    self.type_slices.entries.deinit(allocator);
    self.diagnostics.deinit(allocator);
}

/// Push a diagnostic error
pub fn pushDiagnostic(self: *CIR, allocator: Allocator, tag: CanDiagnostic.Tag, region: Region) !void {
    try self.diagnostics.append(allocator, .{
        .tag = tag,
        .region = region,
    });
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
        assign, // .binop_equals
        init_var, // .binop_equals
        reassign, // .binop_equals
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
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.
        // Add other payload fields as needed
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
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const Tag = enum {
        // The comment after each of these is the AST.Node.Tag it was converted from
        ident, // .lc
        var_ident, // .var_lc
        double_dot_ident, // .double_dot_lc (e.g. `..others`)
        as, // .as
        applied_tag, // .apply_uc
        nominal, // TODO not in AST.Node yet!
        nominal_external, // TODO not in AST.Node yet!
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
        match_branches: u32, // Total number of branches - each branch begins with an `if` (if there's a guard) or list (if multiple alternatives) or expr (normal pattern)
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
        match_branches: u32, // Total number of branches - each branch begins with an `if` (if there's a guard) or list (if multiple alternatives) or expr (normal pattern)
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

/// Canonicalize an AST node that should be an expression
/// Reports errors if the node is not valid in expression context
pub fn canonicalizeExpr(
    self: *CIR, 
    allocator: Allocator, 
    ast: *const AST2, 
    node_idx: AST2.Node.Idx
) !Expr.Idx {
    const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    const node_region = Region{ .start = node.start, .end = node.start }; // Simplified region for now
    
    switch (node.tag) {
        // Expression nodes
        .num_literal_i32 => {
            const expr_idx = try self.exprs.append(allocator, .{
                .tag = .num_literal_i32,
                .start = node.start,
                .payload = .{ .num_literal_i32 = node.payload.num_literal_i32 },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        },
        .int_literal_i32 => {
            const expr_idx = try self.exprs.append(allocator, .{
                .tag = .int_literal_i32,
                .start = node.start,
                .payload = .{ .int_literal_i32 = node.payload.int_literal_i32 },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        },
        .lc, .var_lc => {
            // Identifiers become lookups in expression context
            const expr_idx = try self.exprs.append(allocator, .{
                .tag = .lookup,
                .start = node.start,
                .payload = .{ .ident = node.payload.ident },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        },
        
        // Pattern nodes - these are errors in expression context!
        .underscore => {
            // Underscore pattern found in expression context
            try self.pushDiagnostic(allocator, .pattern_in_expr_context, node_region);
            const expr_idx = try self.exprs.append(allocator, .{
                .tag = .malformed,
                .start = node.start,
                .payload = .{ .malformed = .statement_unexpected_token },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        },
        .uc => {
            // Uppercase identifier (tag pattern) in expression context
            // This could be a tag constructor, but for now treat as error
            try self.pushDiagnostic(allocator, .pattern_in_expr_context, node_region);
            const expr_idx = try self.exprs.append(allocator, .{
                .tag = .malformed,
                .start = node.start,
                .payload = .{ .malformed = .statement_unexpected_token },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        },
        
        // Statement nodes - error in expression context
        .import => {
            try self.pushDiagnostic(allocator, .stmt_in_expr_context, node_region);
            const expr_idx = try self.exprs.append(allocator, .{
                .tag = .malformed,
                .start = node.start,
                .payload = .{ .malformed = .statement_unexpected_token },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        },
        
        else => {
            // Unsupported node type
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            const expr_idx = try self.exprs.append(allocator, .{
                .tag = .malformed,
                .start = node.start,
                .payload = .{ .malformed = .statement_unexpected_token },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        }
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
    defer cir.deinit(allocator);
    
    // Verify initial state
    try testing.expectEqual(@as(usize, 0), cir.stmts.len());
    try testing.expectEqual(@as(usize, 0), cir.exprs.len());
    try testing.expectEqual(@as(usize, 0), cir.patts.len());
    try testing.expectEqual(@as(usize, 0), cir.types.len());
}

test "CIR2 canonicalize simple number literal" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser2 = parse.Parser2;
    
    // Parse a simple number literal
    const source = "42";
    
    var ast = try AST2.initCapacity(allocator, 100);
    defer ast.deinit(allocator);
    
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);
    
    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);
    
    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];
    
    var parser = try Parser2.init(&env, allocator, source, msg_slice, &ast, &byte_slices);
    defer parser.deinit();
    
    // Parse as an expression
    const node_idx = try parser.parseExpr();
    
    // Verify we got a num_literal_i32 node
    const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(node.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 42), node.payload.num_literal_i32);
    
    // Now canonicalize it to CIR2
    var cir = try CIR.initCapacity(allocator, 100, &byte_slices);
    defer cir.deinit(allocator);
    
    const expr_idx = try cir.canonicalizeExpr(allocator, &ast, node_idx);
    
    // Verify the CIR2 expression
    const expr = cir.exprs.get(@enumFromInt(@intFromEnum(expr_idx)));
    try testing.expect(expr.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 42), expr.payload.num_literal_i32);
    
    // Verify we created exactly one expression
    try testing.expectEqual(@as(usize, 1), cir.exprs.len());
}

test "CIR2 canonicalize identifier to lookup" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser2 = parse.Parser2;
    
    // Parse a simple identifier
    const source = "foo";
    
    var ast = try AST2.initCapacity(allocator, 100);
    defer ast.deinit(allocator);
    
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);
    
    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);
    
    var messages: [128]parse.tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];
    
    var parser = try Parser2.init(&env, allocator, source, msg_slice, &ast, &byte_slices);
    defer parser.deinit();
    
    // Parse as an expression
    const node_idx = try parser.parseExpr();
    
    // Verify we got an lc (lowercase identifier) node in AST
    const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(node.tag == .lc);
    
    // Now canonicalize it to CIR2
    var cir = try CIR.initCapacity(allocator, 100, &byte_slices);
    defer cir.deinit(allocator);
    
    // For this test, extend canonicalizeSimpleExpr to handle identifiers
    const expr_idx = try canonicalizeIdentifier(&cir, allocator, &ast, node_idx);
    
    // Verify the CIR2 expression is now a lookup (categorized as an expression)
    const expr = cir.exprs.get(@enumFromInt(@intFromEnum(expr_idx)));
    try testing.expect(expr.tag == .lookup);
    try testing.expectEqual(node.start, expr.start); // Same position
    try testing.expectEqual(node.payload.ident, expr.payload.ident); // Same identifier
}

// Helper function to canonicalize identifiers
fn canonicalizeIdentifier(
    cir: *CIR,
    allocator: Allocator,
    ast: *const AST2,
    node_idx: AST2.Node.Idx
) !Expr.Idx {
    const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    
    switch (node.tag) {
        .lc => {
            // Lowercase identifier becomes a lookup expression
            const expr_idx = try cir.exprs.append(allocator, .{
                .tag = .lookup,
                .start = node.start,
                .payload = .{ .ident = node.payload.ident },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        },
        .var_lc => {
            // var identifier also becomes a lookup (we'll track mutability separately)
            const expr_idx = try cir.exprs.append(allocator, .{
                .tag = .lookup,
                .start = node.start,
                .payload = .{ .ident = node.payload.ident },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        },
        else => {
            // Unsupported for now
            const expr_idx = try cir.exprs.append(allocator, .{
                .tag = .malformed,
                .start = node.start,
                .payload = .{ .malformed = .statement_unexpected_token },
            });
            return @enumFromInt(@intFromEnum(expr_idx));
        }
    }
}

test "CIR2 error: pattern in expression context" {
    const testing = std.testing;
    const allocator = testing.allocator;
    
    // Create an underscore node (pattern) directly in AST
    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);
    
    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);
    
    // Manually create an underscore node
    const node_idx = try ast.nodes.append(allocator, .{
        .tag = .underscore,
        .start = Position{ .offset = 0 },
        .payload = .{ .src_bytes_end = Position{ .offset = 1 } },
    });
    
    // Initialize CIR
    var cir = try CIR.initCapacity(allocator, 10, &byte_slices);
    defer cir.deinit(allocator);
    
    // Try to canonicalize underscore as an expression - should produce error
    const expr_idx = try cir.canonicalizeExpr(allocator, &ast, @enumFromInt(@intFromEnum(node_idx)));
    
    // Verify we got a malformed expression
    const expr = cir.exprs.get(@enumFromInt(@intFromEnum(expr_idx)));
    try testing.expect(expr.tag == .malformed);
    
    // Verify we got a diagnostic error
    try testing.expectEqual(@as(usize, 1), cir.diagnostics.items.len);
    const diagnostic = cir.diagnostics.items[0];
    try testing.expect(diagnostic.tag == .pattern_in_expr_context);
    
    // Verify the region is correct
    try testing.expectEqual(Position{ .offset = 0 }, diagnostic.region.start);
}

test "CIR2 error: uppercase identifier in expression context" {
    const testing = std.testing;
    const allocator = testing.allocator;
    
    // Create an uppercase identifier node (pattern) in AST
    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);
    
    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);
    
    // Create identifier for "Foo" 
    const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 }; // Dummy identifier
    
    // Manually create an uppercase identifier node
    const node_idx = try ast.nodes.append(allocator, .{
        .tag = .uc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = ident_idx },
    });
    
    // Initialize CIR
    var cir = try CIR.initCapacity(allocator, 10, &byte_slices);
    defer cir.deinit(allocator);
    
    // Try to canonicalize uppercase identifier as expression - should produce error
    const expr_idx = try cir.canonicalizeExpr(allocator, &ast, @enumFromInt(@intFromEnum(node_idx)));
    
    // Verify we got a malformed expression
    const expr = cir.exprs.get(@enumFromInt(@intFromEnum(expr_idx)));
    try testing.expect(expr.tag == .malformed);
    
    // Verify we got a diagnostic error
    try testing.expectEqual(@as(usize, 1), cir.diagnostics.items.len);
    const diagnostic = cir.diagnostics.items[0];
    try testing.expect(diagnostic.tag == .pattern_in_expr_context);
}
