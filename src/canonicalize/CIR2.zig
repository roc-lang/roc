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

// Mutable reference to AST - we'll mutate tags in place during canonicalization
ast: *AST2,

// We don't need separate NodeSlices - the AST already has all the slices we need!
// The payloads in nodes already contain the correct NodeSlices indices.

// Diagnostics collected during canonicalization
diagnostics: std.ArrayListUnmanaged(CanDiagnostic),

/// Unified tag that can represent both AST tags and CIR categorized tags
/// This is what we store in the tag field after canonicalization
pub const UnifiedTag = enum(u8) {
    // === AST tags (before canonicalization) ===
    // Binops
    binop_equals, //           =
    binop_double_equals, //    ==
    binop_not_equals, //       !=
    binop_colon, //            :
    binop_colon_equals, //     :=
    binop_dot, //              .
    binop_plus, //             +
    binop_minus, //            -
    binop_star, //             *
    binop_slash, //            /
    binop_double_slash, //     //
    binop_double_question, //  ??
    binop_gt, //               >
    binop_gte, //              >=
    binop_lt, //               <
    binop_lte, //              <=
    binop_thick_arrow, //      =>
    binop_thin_arrow, //       ->
    binop_and, //              and
    binop_or, //               or

    // Identifiers
    lc, // lowercase identifier
    uc, // uppercase identifier
    var_lc, // var lowercase

    // Literals
    num_literal_i32,
    int_literal_i32,

    // Patterns
    underscore,

    // Statements
    import,

    // ... other AST tags can be added as needed ...

    // === CIR Statement tags (after canonicalization) ===
    stmt_assign = 128, // Start at 128 to distinguish from AST tags
    stmt_init_var,
    stmt_reassign,
    stmt_expr,
    stmt_malformed,

    // === CIR Expression tags ===
    expr_lookup = 160,
    expr_num_literal_i32,
    expr_int_literal_i32,
    expr_binop_plus,
    expr_binop_minus,
    expr_binop_star,
    expr_binop_slash,
    expr_malformed,

    // === CIR Pattern tags ===
    patt_ident = 192,
    patt_var_ident,
    patt_underscore,
    patt_num_literal_i32,
    patt_malformed,

    // We can add more as needed...
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

/// Initialize a new CIR that shares storage with the AST
pub fn init(ast: *AST2) CIR {
    return .{
        .ast = ast,
        .diagnostics = .{},
    };
}

/// Deinitialize the CIR and free all memory
pub fn deinit(self: *CIR, allocator: Allocator) void {
    // We don't own the AST nodes or their NodeSlices, so we don't free them
    self.diagnostics.deinit(allocator);
}

/// Push a diagnostic error
pub fn pushDiagnostic(self: *CIR, allocator: Allocator, tag: CanDiagnostic.Tag, region: Region) !void {
    try self.diagnostics.append(allocator, .{
        .tag = tag,
        .region = region,
    });
}

/// Get a mutable node for in-place tag mutation
pub fn getNodeMut(self: *CIR, idx: AST2.Node.Idx) *AST2.Node {
    return self.ast.nodes.getPtr(@enumFromInt(@intFromEnum(idx)));
}

/// Get an immutable node
pub fn getNode(self: *const CIR, idx: AST2.Node.Idx) AST2.Node {
    return self.ast.nodes.get(@enumFromInt(@intFromEnum(idx)));
}

/// Mutate a node's tag in place to a CIR tag
pub fn mutateNodeTag(self: *CIR, idx: AST2.Node.Idx, new_tag: UnifiedTag) void {
    const node = self.getNodeMut(idx);
    // Cast the tag field to u8 and overwrite it
    const tag_ptr = @as(*u8, @ptrCast(&node.tag));
    tag_ptr.* = @intFromEnum(new_tag);
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

    // Read the tag as a u8 and interpret it as a UnifiedTag
    const tag_value = @as(u8, @intFromEnum(node.tag));
    const unified_tag = @as(UnifiedTag, @enumFromInt(tag_value));

    // Convert UnifiedTag to Stmt.Tag
    const stmt_tag: Stmt.Tag = switch (unified_tag) {
        .stmt_assign => .assign,
        .stmt_init_var => .init_var,
        .stmt_reassign => .reassign,
        .stmt_expr => .expr,
        .stmt_malformed => .malformed,
        else => unreachable, // Should only be called on statement nodes
    };

    return .{
        .tag = stmt_tag,
        .start = node.start,
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

    // Read the tag as a u8 and interpret it as a UnifiedTag
    const tag_value = @as(u8, @intFromEnum(node.tag));
    const unified_tag = @as(UnifiedTag, @enumFromInt(tag_value));

    // Convert UnifiedTag to Expr.Tag
    const expr_tag: Expr.Tag = switch (unified_tag) {
        .expr_lookup => .lookup,
        .expr_num_literal_i32 => .num_literal_i32,
        .expr_int_literal_i32 => .int_literal_i32,
        .expr_binop_plus => .binop_plus,
        .expr_binop_minus => .binop_minus,
        .expr_binop_star => .binop_star,
        .expr_binop_slash => .binop_slash,
        .expr_malformed => .malformed,
        else => unreachable, // Should only be called on expression nodes
    };

    return .{
        .tag = expr_tag,
        .start = node.start,
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

    // Read the tag as a u8 and interpret it as a UnifiedTag
    const tag_value = @as(u8, @intFromEnum(node.tag));
    const unified_tag = @as(UnifiedTag, @enumFromInt(tag_value));

    // Convert UnifiedTag to Patt.Tag
    const patt_tag: Patt.Tag = switch (unified_tag) {
        .patt_ident => .ident,
        .patt_var_ident => .var_ident,
        .patt_underscore => .underscore,
        .patt_num_literal_i32 => .num_literal_i32,
        .patt_malformed => .malformed,
        else => unreachable, // Should only be called on pattern nodes
    };

    return .{
        .tag = patt_tag,
        .start = node.start,
        .payload = node.payload,
        .is_mutable = idx.isMutable(), // Extract mutability from the index
    };
}

/// Get a binop from the AST's NodeSlices
/// The payload already contains the correct index - we just cast the index types
pub fn getBinOp(self: *const CIR, comptime IdxType: type, binop_idx: IdxType) struct {
    lhs: IdxType,
    rhs: IdxType,
} {
    // Get the binop from AST's NodeSlices
    const ast_binop = self.ast.node_slices.binOp(binop_idx);

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
/// Mutates the node's tag in place to the appropriate CIR expression tag
/// Reports errors if the node is not valid in expression context
pub fn canonicalizeExpr(self: *CIR, allocator: Allocator, node_idx: AST2.Node.Idx) !Expr.Idx {
    const node = self.getNode(node_idx);
    const node_region = Region{ .start = node.start, .end = node.start }; // Simplified region for now

    switch (node.tag) {
        // Expression nodes - mutate tag in place
        .num_literal_i32 => {
            self.mutateNodeTag(node_idx, .expr_num_literal_i32);
            return self.asExprIdx(node_idx);
        },
        .int_literal_i32 => {
            self.mutateNodeTag(node_idx, .expr_int_literal_i32);
            return self.asExprIdx(node_idx);
        },
        .lc, .var_lc => {
            // Identifiers become lookups in expression context
            self.mutateNodeTag(node_idx, .expr_lookup);
            return self.asExprIdx(node_idx);
        },

        // Binary operators
        .binop_plus, .binop_minus, .binop_star, .binop_slash => {
            // Get the binop data from AST's node slices
            const ast_binop = self.ast.node_slices.binOp(node.payload.binop);

            // Recursively canonicalize left and right operands
            _ = try self.canonicalizeExpr(allocator, ast_binop.lhs);
            _ = try self.canonicalizeExpr(allocator, ast_binop.rhs);

            // Map AST binop tag to CIR expr tag and mutate in place
            const unified_tag: UnifiedTag = switch (node.tag) {
                .binop_plus => .expr_binop_plus,
                .binop_minus => .expr_binop_minus,
                .binop_star => .expr_binop_star,
                .binop_slash => .expr_binop_slash,
                else => unreachable,
            };

            self.mutateNodeTag(node_idx, unified_tag);
            return self.asExprIdx(node_idx);
        },

        // Pattern nodes - these are errors in expression context!
        .underscore => {
            // Underscore pattern found in expression context
            try self.pushDiagnostic(allocator, .pattern_in_expr_context, node_region);
            self.mutateNodeTag(node_idx, .expr_malformed);
            return self.asExprIdx(node_idx);
        },
        .uc => {
            // Uppercase identifier (tag pattern) in expression context
            // This could be a tag constructor, but for now treat as error
            try self.pushDiagnostic(allocator, .pattern_in_expr_context, node_region);
            self.mutateNodeTag(node_idx, .expr_malformed);
            return self.asExprIdx(node_idx);
        },

        // Statement nodes - error in expression context
        .import => {
            try self.pushDiagnostic(allocator, .stmt_in_expr_context, node_region);
            self.mutateNodeTag(node_idx, .expr_malformed);
            return self.asExprIdx(node_idx);
        },

        else => {
            // Unsupported node type
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            self.mutateNodeTag(node_idx, .expr_malformed);
            return self.asExprIdx(node_idx);
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
fn canonicalizeIdentifier(cir: *CIR, allocator: Allocator, ast: *const AST2, node_idx: AST2.Node.Idx) !Expr.Idx {
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
        },
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

test "CIR2 canonicalize binop with NodeSlices" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser2 = parse.Parser2;

    // Parse a simple addition expression
    const source = "1 + 2";

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

    // Parse the expression
    const node_idx = try parser.parseExpr();

    // Verify we got a binop_plus node
    const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(node.tag == .binop_plus);

    // Verify the AST binop structure
    const ast_binop = ast.node_slices.binOp(node.payload.binop);
    const lhs_node = ast.nodes.get(@enumFromInt(@intFromEnum(ast_binop.lhs)));
    const rhs_node = ast.nodes.get(@enumFromInt(@intFromEnum(ast_binop.rhs)));
    try testing.expect(lhs_node.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 1), lhs_node.payload.num_literal_i32);
    try testing.expect(rhs_node.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 2), rhs_node.payload.num_literal_i32);

    // Now canonicalize to CIR2
    var cir = try CIR.initCapacity(allocator, 100, &byte_slices);
    defer cir.deinit(allocator);

    const expr_idx = try cir.canonicalizeExpr(allocator, &ast, node_idx);

    // Verify the CIR2 expression
    const expr = cir.exprs.get(@enumFromInt(@intFromEnum(expr_idx)));
    try testing.expect(expr.tag == .binop_plus);

    // Verify the CIR binop structure uses the same NodeSlices mechanism
    const cir_binop = cir.expr_slices.binOp(expr.payload.binop);

    // Verify the operands were canonicalized correctly
    const lhs_expr = cir.exprs.get(@enumFromInt(@intFromEnum(cir_binop.lhs)));
    const rhs_expr = cir.exprs.get(@enumFromInt(@intFromEnum(cir_binop.rhs)));
    try testing.expect(lhs_expr.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 1), lhs_expr.payload.num_literal_i32);
    try testing.expect(rhs_expr.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 2), rhs_expr.payload.num_literal_i32);

    // Verify we created 3 expressions (the binop and its two operands)
    try testing.expectEqual(@as(usize, 3), cir.exprs.len());
}

test "CIR2 demonstrates NodeSlices memory sharing" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create AST with some nodes
    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create two number literal nodes
    const node1 = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 0 },
        .payload = .{ .num_literal_i32 = 10 },
    });

    const node2 = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 2 },
        .payload = .{ .num_literal_i32 = 20 },
    });

    // Store them in AST NodeSlices
    const ast_binop_idx = try ast.node_slices.appendBinOp(allocator, @enumFromInt(@intFromEnum(node1)), @enumFromInt(@intFromEnum(node2)));

    // Create a binop node using the slice
    _ = try ast.nodes.append(allocator, .{
        .tag = .binop_plus,
        .start = Position{ .offset = 1 },
        .payload = .{ .binop = ast_binop_idx },
    });

    // Initialize CIR - this will share the same byte_slices
    var cir = try CIR.initCapacity(allocator, 10, &byte_slices);
    defer cir.deinit(allocator);

    // IMPORTANT: This demonstrates the key insight - we can cast AST NodeSlices
    // indices directly to CIR NodeSlices indices because they share the same
    // underlying storage structure!

    // First, canonicalize the operands to get CIR expression indices
    const lhs_expr = try cir.canonicalizeExpr(allocator, &ast, @enumFromInt(@intFromEnum(node1)));
    const rhs_expr = try cir.canonicalizeExpr(allocator, &ast, @enumFromInt(@intFromEnum(node2)));

    // Store them in CIR expr_slices (this creates a new entry in CIR slices)
    const cir_binop_idx = try cir.expr_slices.appendBinOp(allocator, lhs_expr, rhs_expr);

    // Verify both AST and CIR are using similar slice structures
    const ast_binop = ast.node_slices.binOp(ast_binop_idx);
    const cir_binop = cir.expr_slices.binOp(cir_binop_idx);

    // The indices are different types (Node.Idx vs Expr.Idx) but the mechanism is the same
    try testing.expect(@intFromEnum(ast_binop.lhs) == @intFromEnum(node1));
    try testing.expect(@intFromEnum(ast_binop.rhs) == @intFromEnum(node2));
    try testing.expect(@intFromEnum(cir_binop.lhs) == @intFromEnum(lhs_expr));
    try testing.expect(@intFromEnum(cir_binop.rhs) == @intFromEnum(rhs_expr));
}

/// Canonicalize an AST node that should be a statement
/// Reports errors if the node is not valid in statement context
pub fn canonicalizeStmt(self: *CIR, allocator: Allocator, node_idx: AST2.Node.Idx, scope_state: *ScopeState) !Stmt.Idx {
    const node = self.getNode(node_idx);
    const node_region = Region{ .start = node.start, .end = node.start }; // Simplified region for now

    switch (node.tag) {
        // Assignment-like statements
        .binop_equals => {
            // Get the binop data from AST's node slices
            const ast_binop = self.ast.node_slices.binOp(node.payload.binop);

            // Check left-hand side to determine if this is a var declaration or reassignment
            const lhs_node = self.ast.nodes.get(@enumFromInt(@intFromEnum(ast_binop.lhs)));

            switch (lhs_node.tag) {
                .var_lc => {
                    // This is a mutable variable declaration: `var x = expr`
                    // Canonicalize the pattern (lhs) as mutable
                    const patt_idx = try self.canonicalizePattMutable(allocator, ast_binop.lhs);

                    // Register this as a mutable variable (mutability is encoded in patt_idx)
                    try scope_state.addIdent(allocator, lhs_node.payload.ident, patt_idx);
                    try scope_state.recordVarPattern(allocator, patt_idx);

                    // Canonicalize the expression (rhs)
                    const expr_idx = try self.canonicalizeExpr(allocator, self.ast, ast_binop.rhs);

                    // Create init_var statement
                    const stmt_idx = try self.stmts.append(allocator, .{
                        .tag = .init_var,
                        .start = node.start,
                        .payload = .{ .assignment = .{
                            .pattern_idx = patt_idx,
                            .expr_idx = expr_idx,
                        } },
                    });
                    return @enumFromInt(@intFromEnum(stmt_idx));
                },
                .lc => {
                    // Could be immutable declaration or reassignment
                    const ident = lhs_node.payload.ident;

                    if (scope_state.lookupIdent(ident)) |existing_patt_idx| {
                        // Identifier already exists - check if it's a var
                        if (ScopeState.isVarPattern(existing_patt_idx)) {
                            // This is a var reassignment
                            // Check for cross-function boundary reassignment
                            if (scope_state.isVarReassignmentAcrossFunctionBoundary(existing_patt_idx)) {
                                try self.pushDiagnostic(allocator, .ident_already_defined, node_region);
                            }

                            // Canonicalize as reassignment
                            // Note: For reassignment, we don't canonicalize LHS as it should be a reference to existing var
                            const expr_rhs = try self.canonicalizeExpr(allocator, self.ast, ast_binop.rhs);

                            const stmt_idx = try self.stmts.append(allocator, .{
                                .tag = .reassign,
                                .start = node.start,
                                .payload = .{ .assignment = .{
                                    .pattern_idx = existing_patt_idx,
                                    .expr_idx = expr_rhs,
                                } },
                            });
                            return @enumFromInt(@intFromEnum(stmt_idx));
                        } else {
                            // Error: trying to reassign immutable variable
                            try self.pushDiagnostic(allocator, .ident_already_defined, node_region);

                            // Create dummy statement
                            const stmt_idx = try self.stmts.append(allocator, .{
                                .tag = .malformed,
                                .start = node.start,
                                .payload = .{ .src_bytes_end = node.start },
                            });
                            return @enumFromInt(@intFromEnum(stmt_idx));
                        }
                    } else {
                        // This is an immutable variable declaration: `x = expr`
                        // Canonicalize the pattern (lhs)
                        const patt_idx = try self.canonicalizePatt(allocator, ast_binop.lhs);

                        // Register this as an immutable variable
                        try scope_state.addIdent(allocator, ident, patt_idx);

                        // Canonicalize the expression (rhs)
                        const expr_idx = try self.canonicalizeExpr(allocator, self.ast, ast_binop.rhs);

                        // Create assign statement (immutable assignment)
                        const stmt_idx = try self.stmts.append(allocator, .{
                            .tag = .assign,
                            .start = node.start,
                            .payload = .{ .assignment = .{
                                .pattern_idx = patt_idx,
                                .expr_idx = expr_idx,
                            } },
                        });
                        return @enumFromInt(@intFromEnum(stmt_idx));
                    }
                },
                else => {
                    // Complex pattern on LHS - treat as pattern match assignment
                    const patt_idx = try self.canonicalizePatt(allocator, ast_binop.lhs);
                    const expr_idx = try self.canonicalizeExpr(allocator, self.ast, ast_binop.rhs);

                    const stmt_idx = try self.stmts.append(allocator, .{
                        .tag = .assign,
                        .start = node.start,
                        .payload = .{ .assignment = .{
                            .pattern_idx = patt_idx,
                            .expr_idx = expr_idx,
                        } },
                    });
                    return @enumFromInt(@intFromEnum(stmt_idx));
                },
            }
        },

        // Expression nodes - error in statement context without assignment
        .num_literal_i32, .int_literal_i32, .lc => {
            try self.pushDiagnostic(allocator, .expr_in_stmt_context, node_region);
            const stmt_idx = try self.stmts.append(allocator, .{
                .tag = .malformed,
                .start = node.start,
                .payload = .{ .src_bytes_end = node.start },
            });
            return @enumFromInt(@intFromEnum(stmt_idx));
        },

        else => {
            // Unsupported statement type
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            const stmt_idx = try self.stmts.append(allocator, .{
                .tag = .malformed,
                .start = node.start,
                .payload = .{ .src_bytes_end = node.start },
            });
            return @enumFromInt(@intFromEnum(stmt_idx));
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
    const node = ast_param.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    const node_region = Region{ .start = node.start, .end = node.start }; // Simplified region for now

    switch (node.tag) {
        .lc => {
            // Lowercase identifier pattern
            // Mutate the AST node's tag in place
            self.mutateNodeTag(node_idx, if (is_mutable) .patt_var_ident else .patt_ident);

            // Return the index with mutability encoded
            return Patt.Idx.withMutability(node_idx, is_mutable);
        },
        .var_lc => {
            // Mutable identifier pattern (var_lc always means mutable)
            // Mutate the AST node's tag in place
            self.mutateNodeTag(node_idx, .patt_var_ident);

            // Return the index with mutability encoded (always mutable for var_lc)
            return Patt.Idx.withMutability(node_idx, true);
        },
        .underscore => {
            // Underscore pattern
            // Mutate the AST node's tag in place
            self.mutateNodeTag(node_idx, .patt_underscore);

            // Return the index with mutability encoded (underscore can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },
        .num_literal_i32 => {
            // Number literal pattern
            // Mutate the AST node's tag in place
            self.mutateNodeTag(node_idx, .patt_num_literal_i32);

            // Return the index with mutability encoded (literals can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },

        // Expression nodes - error in pattern context
        .binop_plus, .binop_minus, .binop_star, .binop_slash => {
            try self.pushDiagnostic(allocator, .expr_in_pattern_context, node_region);
            // Mutate to malformed pattern tag
            self.mutateNodeTag(node_idx, .patt_malformed);
            // Return with mutability encoded (malformed patterns can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
        },

        else => {
            // Unsupported pattern type
            try self.pushDiagnostic(allocator, .unsupported_node, node_region);
            // Mutate to malformed pattern tag
            self.mutateNodeTag(node_idx, .patt_malformed);
            // Return with mutability encoded (malformed patterns can't be mutable)
            return Patt.Idx.withMutability(node_idx, false);
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

    /// Push a new scope onto the stack
    pub fn pushScope(self: *ScopeState, allocator: Allocator, is_function_boundary: bool) !void {
        try self.scopes.append(allocator, Scope.init(is_function_boundary));
    }

    /// Pop the current scope
    pub fn popScope(self: *ScopeState, allocator: Allocator) void {
        if (self.scopes.items.len > 0) {
            var scope = self.scopes.pop();
            scope.deinit(allocator);
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

test "CIR2 canonicalize immutable assignment" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser2 = parse.Parser2;

    // Parse a simple assignment
    const source = "x = 42";

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

    // Parse the statement
    const node_idx = try parser.parseExpr(); // Parser currently returns expressions

    // Verify we got a binop_equals node
    const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(node.tag == .binop_equals);

    // Now canonicalize to CIR2
    var cir = try CIR.initCapacity(allocator, 100, &byte_slices);
    defer cir.deinit(allocator);

    var scope_state = ScopeState{};
    defer scope_state.deinit(allocator);
    try scope_state.pushScope(allocator, false); // Push initial scope

    const stmt_idx = try cir.canonicalizeStmt(allocator, node_idx, &scope_state);

    // Verify the CIR2 statement
    const stmt = cir.stmts.get(@enumFromInt(@intFromEnum(stmt_idx)));
    try testing.expect(stmt.tag == .assign); // Immutable assignment

    // Verify we created one statement, one pattern, and one expression
    try testing.expectEqual(@as(usize, 1), cir.stmts.len());
    try testing.expectEqual(@as(usize, 1), cir.patts.len());
    try testing.expectEqual(@as(usize, 1), cir.exprs.len());

    // Verify the pattern is an identifier
    const patt = cir.patts.get(@enumFromInt(0));
    try testing.expect(patt.tag == .ident);

    // Verify the expression is a number literal
    const expr = cir.exprs.get(@enumFromInt(0));
    try testing.expect(expr.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 42), expr.payload.num_literal_i32);
}

test "CIR2 canonicalize mutable variable declaration" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const Parser2 = parse.Parser2;

    // Parse a mutable variable declaration
    const source = "var x = 10";

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

    // Parse the statement
    const node_idx = try parser.parseExpr();

    // Verify we got a binop_equals node
    const node = ast.nodes.get(@enumFromInt(@intFromEnum(node_idx)));
    try testing.expect(node.tag == .binop_equals);

    // Verify the LHS is a var_lc node
    const ast_binop = ast.node_slices.binOp(node.payload.binop);
    const lhs_node = ast.nodes.get(@enumFromInt(@intFromEnum(ast_binop.lhs)));
    try testing.expect(lhs_node.tag == .var_lc);

    // Now canonicalize to CIR2
    var cir = try CIR.initCapacity(allocator, 100, &byte_slices);
    defer cir.deinit(allocator);

    var scope_state = ScopeState{};
    defer scope_state.deinit(allocator);
    try scope_state.pushScope(allocator, false); // Push initial scope

    const stmt_idx = try cir.canonicalizeStmt(allocator, node_idx, &scope_state);

    // Verify the CIR2 statement
    const stmt = cir.stmts.get(@enumFromInt(@intFromEnum(stmt_idx)));
    try testing.expect(stmt.tag == .init_var); // Mutable variable initialization

    // Verify we created one statement, one pattern, and one expression
    try testing.expectEqual(@as(usize, 1), cir.stmts.len());
    try testing.expectEqual(@as(usize, 1), cir.patts.len());
    try testing.expectEqual(@as(usize, 1), cir.exprs.len());

    // Verify the pattern is a mutable identifier
    const patt = cir.patts.get(@enumFromInt(0));
    try testing.expect(patt.tag == .var_ident);

    // Verify the expression is a number literal
    const expr = cir.exprs.get(@enumFromInt(0));
    try testing.expect(expr.tag == .num_literal_i32);
    try testing.expectEqual(@as(i32, 10), expr.payload.num_literal_i32);
}

test "CIR2 error: expression in statement context" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a standalone expression node in AST
    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create a number literal node (expression, not statement)
    const node_idx = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 0 },
        .payload = .{ .num_literal_i32 = 42 },
    });

    // Initialize CIR
    var cir = try CIR.initCapacity(allocator, 10, &byte_slices);
    defer cir.deinit(allocator);

    var scope_state = ScopeState{};
    defer scope_state.deinit(allocator);
    try scope_state.pushScope(allocator, false); // Push initial scope

    // Try to canonicalize expression as statement - should produce error
    const stmt_idx = try cir.canonicalizeStmt(allocator, @enumFromInt(@intFromEnum(node_idx)), &scope_state);

    // Verify we got a statement (even though it's an error case)
    const stmt = cir.stmts.get(@enumFromInt(@intFromEnum(stmt_idx)));
    try testing.expect(stmt.tag == .malformed); // Error case uses malformed tag

    // Verify we got a diagnostic error
    try testing.expectEqual(@as(usize, 1), cir.diagnostics.items.len);
    const diagnostic = cir.diagnostics.items[0];
    try testing.expect(diagnostic.tag == .expr_in_stmt_context);
}

test "CIR2 demonstrates in-place tag mutation" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create an AST node
    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create an identifier node - this will be transformed based on context
    const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };

    const node_idx = try ast.nodes.append(allocator, .{
        .tag = .lc, // Lowercase identifier in AST
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = ident_idx },
    });

    // Initialize CIR
    var cir = try CIR.initCapacity(allocator, 10, &byte_slices);
    defer cir.deinit(allocator);

    // IMPORTANT: This demonstrates the key concept of tag mutation during canonicalization.
    // The same node structure can have different tags based on its context:

    // When canonicalized as an expression, .lc becomes .lookup
    const expr_idx = try cir.canonicalizeExpr(allocator, &ast, @enumFromInt(@intFromEnum(node_idx)));
    const expr = cir.exprs.get(@enumFromInt(@intFromEnum(expr_idx)));
    try testing.expect(expr.tag == .lookup); // Tag transformed from .lc to .lookup

    // When canonicalized as a pattern, .lc becomes .ident
    const patt_idx = try cir.canonicalizePatt(allocator, @enumFromInt(@intFromEnum(node_idx)));
    const patt = cir.patts.get(@enumFromInt(@intFromEnum(patt_idx)));
    try testing.expect(patt.tag == .ident); // Tag transformed from .lc to .ident

    // This shows how the same AST node (.lc) gets categorized differently:
    // - In expression context: becomes Expr with tag .lookup
    // - In pattern context: becomes Patt with tag .ident
    //
    // The key insight is that we're not modifying the original AST,
    // but creating new categorized nodes with appropriate tags.
}

test "CIR2 demonstrates reassignment tracking" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // This test demonstrates proper variable mutability tracking

    var ast = try AST2.initCapacity(allocator, 20);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create nodes for "var x = 10" (mutable declaration)
    const var_ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };

    const var_node = try ast.nodes.append(allocator, .{
        .tag = .var_lc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = var_ident },
    });

    const num_node1 = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 6 },
        .payload = .{ .num_literal_i32 = 10 },
    });

    const binop_idx1 = try ast.node_slices.appendBinOp(allocator, @enumFromInt(@intFromEnum(var_node)), @enumFromInt(@intFromEnum(num_node1)));

    const assign_node1 = try ast.nodes.append(allocator, .{
        .tag = .binop_equals,
        .start = Position{ .offset = 4 },
        .payload = .{ .binop = binop_idx1 },
    });

    // Create nodes for "x = 20" (reassignment)
    const lc_node = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 10 },
        .payload = .{ .ident = var_ident }, // Same identifier
    });

    const num_node2 = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 14 },
        .payload = .{ .num_literal_i32 = 20 },
    });

    const binop_idx2 = try ast.node_slices.appendBinOp(allocator, @enumFromInt(@intFromEnum(lc_node)), @enumFromInt(@intFromEnum(num_node2)));

    const assign_node2 = try ast.nodes.append(allocator, .{
        .tag = .binop_equals,
        .start = Position{ .offset = 12 },
        .payload = .{ .binop = binop_idx2 },
    });

    // Initialize CIR
    var cir = try CIR.initCapacity(allocator, 20, &byte_slices);
    defer cir.deinit(allocator);

    var scope_state = ScopeState{};
    defer scope_state.deinit(allocator);
    try scope_state.pushScope(allocator, false); // Push initial scope

    // First assignment: var x = 10 (creates init_var statement)
    const stmt1_idx = try cir.canonicalizeStmt(allocator, @enumFromInt(@intFromEnum(assign_node1)), &scope_state);
    const stmt1 = cir.stmts.get(@enumFromInt(@intFromEnum(stmt1_idx)));
    try testing.expect(stmt1.tag == .init_var); // Mutable variable initialization

    // Verify the pattern was registered
    const patt1_idx = @as(Patt.Idx, @enumFromInt(0));
    try testing.expect(scope_state.lookupIdent(var_ident) != null);
    try testing.expect(ScopeState.isVarPattern(patt1_idx));

    // Second assignment: x = 20 (should create reassign statement)
    const stmt2_idx = try cir.canonicalizeStmt(allocator, @enumFromInt(@intFromEnum(assign_node2)), &scope_state);
    const stmt2 = cir.stmts.get(@enumFromInt(@intFromEnum(stmt2_idx)));
    try testing.expect(stmt2.tag == .reassign); // Reassignment to mutable variable

    // Verify we have 2 statements, 1 pattern (reused), and 2 expressions (10, 20)
    try testing.expectEqual(@as(usize, 2), cir.stmts.len());
    try testing.expectEqual(@as(usize, 1), cir.patts.len()); // Only one pattern for 'x'
    try testing.expectEqual(@as(usize, 2), cir.exprs.len()); // 10 and 20 (no lookup needed for reassignment)

    // Verify the reassignment references the same pattern
    const reassign_payload = stmt2.payload.assignment;
    try testing.expect(reassign_payload.pattern_idx == patt1_idx); // Same pattern being reassigned
}

test "CIR2 error: reassign immutable variable" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var ast = try AST2.initCapacity(allocator, 20);
    defer ast.deinit(allocator);

    var byte_slices = ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Create nodes for "x = 10" (immutable declaration)
    const ident = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };

    const lc_node1 = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = ident },
    });

    const num_node1 = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 4 },
        .payload = .{ .num_literal_i32 = 10 },
    });

    const binop_idx1 = try ast.node_slices.appendBinOp(allocator, @enumFromInt(@intFromEnum(lc_node1)), @enumFromInt(@intFromEnum(num_node1)));

    const assign_node1 = try ast.nodes.append(allocator, .{
        .tag = .binop_equals,
        .start = Position{ .offset = 2 },
        .payload = .{ .binop = binop_idx1 },
    });

    // Create nodes for "x = 20" (attempted reassignment)
    const lc_node2 = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 10 },
        .payload = .{ .ident = ident }, // Same identifier
    });

    const num_node2 = try ast.nodes.append(allocator, .{
        .tag = .num_literal_i32,
        .start = Position{ .offset = 14 },
        .payload = .{ .num_literal_i32 = 20 },
    });

    const binop_idx2 = try ast.node_slices.appendBinOp(allocator, @enumFromInt(@intFromEnum(lc_node2)), @enumFromInt(@intFromEnum(num_node2)));

    const assign_node2 = try ast.nodes.append(allocator, .{
        .tag = .binop_equals,
        .start = Position{ .offset = 12 },
        .payload = .{ .binop = binop_idx2 },
    });

    // Initialize CIR
    var cir = try CIR.initCapacity(allocator, 20, &byte_slices);
    defer cir.deinit(allocator);

    var scope_state = ScopeState{};
    defer scope_state.deinit(allocator);
    try scope_state.pushScope(allocator, false); // Push initial scope

    // First assignment: x = 10 (creates assign statement - immutable)
    const stmt1_idx = try cir.canonicalizeStmt(allocator, @enumFromInt(@intFromEnum(assign_node1)), &scope_state);
    const stmt1 = cir.stmts.get(@enumFromInt(@intFromEnum(stmt1_idx)));
    try testing.expect(stmt1.tag == .assign); // Immutable assignment

    // Verify the pattern was registered as immutable
    const patt1_idx = @as(Patt.Idx, @enumFromInt(0));
    try testing.expect(scope_state.lookupIdent(ident) != null);
    try testing.expect(!ScopeState.isVarPattern(patt1_idx)); // NOT a var pattern

    // Second assignment: x = 20 (should produce error)
    const stmt2_idx = try cir.canonicalizeStmt(allocator, @enumFromInt(@intFromEnum(assign_node2)), &scope_state);
    const stmt2 = cir.stmts.get(@enumFromInt(@intFromEnum(stmt2_idx)));
    try testing.expect(stmt2.tag == .malformed); // Error case creates malformed statement

    // Verify we got a diagnostic error
    try testing.expectEqual(@as(usize, 1), cir.diagnostics.items.len);
    const diagnostic = cir.diagnostics.items[0];
    try testing.expect(diagnostic.tag == .ident_already_defined);
}
// Add these tests to the end of CIR2.zig

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

    var ast = try AST2.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    // Create CIR with shared AST storage
    var cir = CIR.init(&ast);
    defer cir.deinit(allocator);

    // Create two identifier nodes
    const ident_idx = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };

    const node1_idx = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 0 },
        .payload = .{ .ident = ident_idx },
    });

    const node2_idx = try ast.nodes.append(allocator, .{
        .tag = .lc,
        .start = Position{ .offset = 10 },
        .payload = .{ .ident = ident_idx },
    });

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
