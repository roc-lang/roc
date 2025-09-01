//! This file implements the Intermediate Representation (IR) for Roc's parser.
//!
//! The IR provides a structured, tree-based representation of Roc source code after parsing.
//!
//! The design uses an arena-based memory allocation strategy with a "multi-list" approach where nodes
//! are stored in a flat list but cross-referenced via indices rather than pointers. This improves
//! memory locality and efficiency.
//!
//! This AST uses a unified approach where types, patterns, and expressions are
//! all represented as expressions with operators. Programs consist of statements,
//! and statements contain expressions.
//!
//! Key concepts:
//! - `Foo : Bar` and `Foo = Bar` and `Foo := Bar` are all binary operations (like `+` and `-` and `*` etc.)
//! - Statement/expression boundaries occur at whitespace not followed by operators/delimiters
//!   - e.g. `foo = bar baz = blah` is 2 statements: `foo = bar` and `baz = blah`
//! - A module is a header (parsed separately from statements and expressions) followed by 0+ statements and/or expressions
//!   - Canonicalization will give errors for expressions at the top level of a module; only statements are valid there!
//!   - Also, only some statements are valid; canonicalization will also give errors for invalid statements.
//! - Operator precedence determines expression/statement structure
//! - In many cases, we just store the start region and don't bother storing the end region, because we can derive it
//!   quickly enough, and storing only the start region saves 4B per node (which we use to store a payload instead).
//!   - e.g. if it's a record or block literal, we can find the next token after the end of the last
//!   node inside it, and that will be the closing delimiter
//!   - Start region is not so quick to re-derive, because accounting for line comments when scanning forwards is trivial,
//!     but super expensive when scanning backwards - so we always store start regions (except in binops, which just use lhs).
//!   - Also, if no nodes stored start regions, then tons of re-scanning would end up needing to happen in practice.
//! - We have an AstData array of u32 unions which is a side table that nodes use to store extra data.
//!   - A very common pattern is for a node to store an AstData.Idx in its u32 payload (instead of end region),
//!     and then the first AstData entry is a 32-bit length of how many AstData entries follow it - so, an inline slice.
//!   - Another common pattern is to store the length in bytes and then have multiple AstData entries after the length
//!     be a flat u8 slice (e.g. for string literals).
//!     - This can waste up to 3 bytes at the end (because each slot is 4 bytes, but some strings may not have lengths
//!       that are multiples of 4).
//!     - However, trying to avoid that potential waste would end up introducing alignment padding overhead, which
//!       would likely be overall more wasteful in practice - in addition to being more complex and error-prone.

const std = @import("std");
const base = @import("base");
const reporting = @import("reporting");
const error_reporting = @import("error_reporting.zig");
const CommonEnv = base.CommonEnv;
const collections = @import("collections");
const tokenize = @import("tokenize_iter.zig");

const Region = base.Region;
const Position = Region.Position;
const Ident = base.Ident;
const ByteSlices = collections.ByteSlices;
const Allocator = std.mem.Allocator;
const Ast = @This();

nodes: collections.SafeMultiList(Node),
node_slices: collections.NodeSlices(Node.Idx), // Slices of node indices for things like list literals, used during other compilation stages
byte_slices: ByteSlices, // Slices of backing bytes for things like string literals and number literals, used at runtime
header: ?Header, // Optional module header (app, package, platform, etc.) - not used if we're parsing a standalone expression

/// Initialize a new AST with pre-allocated capacity
pub fn initCapacity(allocator: Allocator, estimated_node_count: usize) Allocator.Error!Ast {
    var nodes = collections.SafeMultiList(Node){};
    try nodes.ensureTotalCapacity(allocator, estimated_node_count);

    // Reserve index 0 as a sentinel/NIL value
    // Append a dummy node that should never be accessed
    _ = try nodes.append(allocator, .{
        .tag = .malformed,
        .region = Region{ .start = Position{ .offset = 0 }, .end = Position{ .offset = 0 } },
        .payload = .{ .malformed = .internal_parser_error },
    });

    return .{
        .nodes = nodes,
        .node_slices = .{ .entries = .{} },
        .byte_slices = .{ .entries = .{} },
        .header = null,
    };
}

/// Deinitialize the AST and free all memory
pub fn deinit(self: *Ast, allocator: Allocator) void {
    self.nodes.deinit(allocator);
    self.node_slices.entries.deinit(allocator);
    self.byte_slices.entries.deinit(allocator);
}

/// Append a new node to the AST
pub fn appendNode(self: *Ast, allocator: Allocator, node_region: Region, node_tag: Node.Tag, node_payload: Node.Payload) Allocator.Error!Node.Idx {
    const idx = @intFromEnum(try self.nodes.append(allocator, .{
        .tag = node_tag,
        .region = node_region,
        .payload = node_payload,
    }));
    return @as(Node.Idx, @enumFromInt(idx));
}

/// Append a slice of nodes and return an index to access them later
pub fn appendNodeSlice(self: *Ast, allocator: Allocator, nodes: []const Node.Idx) Allocator.Error!collections.NodeSlices(Node.Idx).Idx {
    return try self.node_slices.append(allocator, nodes);
}

/// Append binop operands and return an index to access them later
pub fn appendBinOp(self: *Ast, allocator: Allocator, lhs: Node.Idx, rhs: Node.Idx) Allocator.Error!collections.NodeSlices(Node.Idx).Idx {
    return try self.node_slices.appendBinOp(allocator, lhs, rhs);
}

/// Append bytes for literals and return an index to access them later
pub fn appendByteSlice(self: *Ast, allocator: Allocator, bytes: []const u8) Allocator.Error!ByteSlices.Idx {
    return try self.byte_slices.append(allocator, bytes);
}

// NodeSlices moved to collections/NodeSlices.zig as a generic type

/// Returns an iterator over all the nodes in a block.
/// Panics in debug builds if the given Node.Idx does not refer to a .block node.
fn nodesInBlock(self: *const Ast, idx: Node.Idx) collections.NodeSlices(Node.Idx).Iterator {
    std.debug.assert(self.tag(idx) == .block);

    return self.node_slices.nodes(&self.payloadPtr(idx).nodes);
}

/// Get a pointer to the payload (for the optimization where we pass pointers to nodes())
pub fn payloadPtr(self: *const Ast, idx: Node.Idx) *const Node.Payload {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return &self.nodes.fieldItem(.payload, multi_list_idx);
}

/// Returns an iterator over all the nodes in a string interpolation.
/// Panics in debug builds if the given Node.Idx does not refer to a .str_interpolation node.
fn nodesInInterpolation(self: *const Ast, idx: Node.Idx) collections.NodeSlices(Node.Idx).Iterator {
    std.debug.assert(self.tag(idx) == .str_interpolation);

    return self.node_slices.nodes(&self.payloadPtr(idx).str_interpolated_nodes);
}

/// A lambda expression, e.g. `|a, b| c`
pub const Lambda = struct {
    body: Node.Idx,
    args_idx: collections.NodeSlices(Node.Idx).Idx, // Index to access args via iterator
};

/// A while loop, e.g. `while condition body`
pub const WhileLoop = struct {
    condition: Node.Idx,
    body: Node.Idx,
};

/// A for loop, e.g. `for pattern in expr body`
pub const ForLoop = struct {
    pattern: Node.Idx,
    iterable: Node.Idx,
    body: Node.Idx,
};

/// Iterator for lambda args
pub const LambdaArgsIterator = struct {
    iter: collections.NodeSlices(Node.Idx).Iterator,
    skipped_body: bool,

    pub fn next(self: *LambdaArgsIterator) ?Node.Idx {
        if (!self.skipped_body) {
            _ = self.iter.next(); // Skip the body
            self.skipped_body = true;
        }
        return self.iter.next();
    }
};

/// Panics in debug builds if the given Node.Idx does not refer to a .lambda node.
pub fn lambda(self: *const Ast, idx: Node.Idx) Lambda {
    std.debug.assert(self.tag(idx) == .lambda);

    const body_then_args_idx = self.payload(idx).body_then_args;
    var iter = self.node_slices.nodes(&body_then_args_idx);

    // First node is the body
    // Safe: Parser always ensures lambda nodes have at least a body
    // The parser constructs lambda nodes with body_then_args containing body first
    const body = iter.next() orelse unreachable; // Lambda must have at least a body

    return Lambda{
        .body = body,
        .args_idx = body_then_args_idx,
    };
}

/// Panics in debug builds if the given Node.Idx does not refer to a .while_loop node.
pub fn whileLoop(self: *const Ast, idx: Node.Idx) WhileLoop {
    std.debug.assert(self.tag(idx) == .while_loop);
    const nodes_idx = self.payload(idx).nodes;
    var iter = self.node_slices.nodes(&nodes_idx);

    // Safe: Parser always ensures while_loop nodes have exactly 2 children (condition, body)
    // The parser constructs while_loop nodes with these two nodes in parseWhile()
    const condition = iter.next() orelse unreachable;
    const body = iter.next() orelse unreachable;

    return WhileLoop{
        .condition = condition,
        .body = body,
    };
}

/// Panics in debug builds if the given Node.Idx does not refer to a .for_loop node.
pub fn forLoop(self: *const Ast, idx: Node.Idx) ForLoop {
    std.debug.assert(self.tag(idx) == .for_loop);
    const nodes_idx = self.payload(idx).nodes;
    var iter = self.node_slices.nodes(&nodes_idx);

    // Safe: Parser always ensures for_loop nodes have exactly 3 children (pattern, iterable, body)
    // The parser constructs for_loop nodes with these three nodes in parseFor()
    const pattern = iter.next() orelse unreachable;
    const iterable = iter.next() orelse unreachable;
    const body = iter.next() orelse unreachable;

    return ForLoop{
        .pattern = pattern,
        .iterable = iterable,
        .body = body,
    };
}

/// Get an iterator for lambda args
pub fn lambdaArgs(self: *const Ast, lambda_val: Lambda) LambdaArgsIterator {
    return LambdaArgsIterator{
        .iter = self.node_slices.nodes(&lambda_val.args_idx),
        .skipped_body = false,
    };
}

/// Panics in debug builds if the given node is not a BinOp.
pub fn binOp(self: *const Ast, idx: Node.Idx) collections.NodeSlices(Node.Idx).BinOp {
    std.debug.assert(self.tag(idx).isBinOp());

    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.node_slices.binOp(self.nodes.fieldItem(.payload, multi_list_idx).binop);
}

/// Special accessor for arrow nodes which may have multiple parameters
/// Returns the first parameter and return type as a BinOp for compatibility
fn arrowBinOp(self: *const Ast, idx: Node.Idx) collections.NodeSlices(Node.Idx).BinOp {
    const node_tag = self.tag(idx);
    std.debug.assert(node_tag == .binop_thin_arrow or node_tag == .binop_thick_arrow);

    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    const node_payload = self.nodes.fieldItem(.payload, multi_list_idx);

    // Try to interpret as a nodes payload (variadic arrow)
    // We can tell by checking if the first entry in the slice is a valid node
    const nodes_idx = node_payload.binop;
    if (nodes_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
        var iter = self.node_slices.nodes(&nodes_idx);
        const maybe_first = iter.next();

        // Check if this looks like a valid nodes slice by seeing if we can get nodes from it
        if (maybe_first) |first_param| {
            // This is a variadic arrow with multiple parameters
            // For compatibility, extract first param as lhs and return type as rhs

            // Skip to the last node (return type)
            var return_type = first_param;
            while (iter.next()) |node| {
                return_type = node;
            }

            return .{
                .lhs = first_param,
                .rhs = return_type,
            };
        }
    }

    // Fall back to regular binop
    return self.node_slices.binOp(node_payload.binop);
}

/// Returns an iterator over all parameters and return type of an arrow node
/// The last node in the iteration is the return type, all others are parameters
fn arrowNodes(self: *const Ast, idx: Node.Idx) collections.NodeSlices(Node.Idx).Iterator {
    const node_tag = self.tag(idx);
    std.debug.assert(node_tag == .binop_thin_arrow or node_tag == .binop_thick_arrow);

    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    const node_payload = self.nodes.fieldItem(.payload, multi_list_idx);

    // For arrow nodes with multiple parameters, we store them using .nodes
    // For single parameter arrows, we use .binop
    // We can reinterpret the payload to check which one it is
    const nodes_idx = node_payload.binop;

    return self.node_slices.nodes(&nodes_idx);
}

/// Given the idx to a lambda, return the region of just its args (the `| ... |` including the pipes)
pub const Node = struct {
    tag: Node.Tag, // u8 discriminant
    region: Region, // Region spans from start of first token to end of last token in this AST node
    payload: Node.Payload, // u32 union of extra information that varies based on tag

    /// Index to a node in the AST.
    /// Although this is an i32, node indices are never negative in practice.
    /// We use i32 instead of u32 so we can use the sign bit as a sentinel
    /// terminator when storing node indices in collections like NodeSlices.
    pub const Idx = enum(i32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    // BinOp moved to NodeSlices in collections

    pub const Tag = enum(u8) {
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
        binop_as, //               as
        binop_exposing, //         exposing (for imports)
        binop_where, //            where (for type constraints)
        binop_platform, //         platform (for app headers)
        binop_pipe, //             | for pattern alternatives (maybe we should replace this with `or`, not sure)

        // Identifiers, possibly with modifiers
        uc, // uppercase identifier (e.g. `Foo`) - could be a tag or a type
        lc, // lowercase identifier (e.g. `foo`) - could be a lookup, pattern ident, type variable, or record field name
        var_lc, // `var` followed by lowercase identifier (e.g. `var foo`)
        neg_lc, // negated identifier (e.g. `-foo`)
        not_lc, // bang followed by lowercase identifier (e.g. `!foo`)
        dot_lc, // dot followed by lowercase identifier (e.g. `.foo`)
        double_dot_lc, // two dots followed by lowercase identifier (e.g. `..others`)
        uc_dot_ucs, // 2+ uppercase idents separated by dots (e.g. `Module.Type` or `Module.NominalType.TagName`)
        lc_dot_ucs, // like .uc_dot_ucs except first one is lc (e.g. `pkg.Module` or `pkg.Module.Nominal.Tag`)
        dot_num, // dot followed by number (e.g. `.0`) - this is a tuple accessor
        underscore, // underscore pattern (e.g. `_`) - no payload needed
        ellipsis, // triple dot (e.g. `...`) - represents "not yet implemented"
        import, // import statement (e.g. `import foo`) - payload stores imported nodes
        expect, // expect statement (e.g. `expect foo == bar`) - payload stores the condition expression

        // Literals that are small enough to be stored right here in .payload's u32 - by far the most common case for numbers
        num_literal_i32, // e.g. `42`
        int_literal_i32, // e.g. `0x42`
        frac_literal_small, // e.g. `0.2` - fits in a 32-bit SmallDec
        str_literal_small, // Null-terminated ASCII with escapes resolved (if it contains '\0', must use .str_literal_big)

        // Literals that don't fit in u32, and must be instead stored in AstData.
        num_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bignums
        int_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bigints
        frac_literal_big, // Like a bigint literal but stores 2 lengths first, for digits before/after decimal point
        str_literal_big, // Byte length followed by UTF-8 bytes (across multiple AstData entries) with all escapes resolved.
        str_interpolation, // e.g. `"abc${def}ghi${jkl}mno"` - payload is list of nodes that will all get appended togther
        list_literal, // e.g. `[1, 2, 3]` - note that this is nonempty; .empty_list has its own variant
        tuple_literal, // e.g. `(foo, bar)` - we know it's a tuple literal because of the commas
        record_literal, // e.g. `{ foo, bar }` or `{ foo, }` - only records have commas; `{ foo }` is a block

        // Other
        apply_lc, // e.g. `foo(bar, baz)`
        apply_uc, // e.g. `Foo(bar, baz)` or `(foo(bar, baz))(blah, etc)`
        apply_anon, // e.g. `(foo(bar, baz))(blah, etc)`
        apply_module, // e.g. `module(a)` in where clauses
        block, // Block with curly braces, e.g. `{ expr1, expr2, ... }` - could end up being a record (expr or destructure)
        lambda, // e.g. `|x, y| x + y` - payload stores a slice of body_then_args
        lambda_no_args, // e.g. `|| x + y` - payload stores only the body node
        match, // e.g. `match cond { Ok(a) => a Err(b) => b }` - needs to store cond as well as branches
        if_else, // e.g. `if cond then_branch else_branch` - needs to store cond as well as branches. if-exprs must have else.
        if_without_else, // e.g. `if cond do_something!()` - stores exactly 1 cond node followed by exactly 1 body node
        unary_not, // e.g. `!(foo())` - note that `!foo` is special-cased to .not_lc instead
        unary_neg, // e.g. `-(foo())` - note that `-foo` is special-cased to .neg_lc instead
        unary_double_dot, // e.g. `..(foo())` - note that `..foo` is special-cased to .double_dot_lc instead)
        ret, // e.g. `return blah` - stores the expr to return in Node.Payload
        for_loop, // e.g. `for x in y { ... }` - payload stores 3+ nodes: `for node1 in node2 { node3+... }`
        while_loop, // e.g. `while x { ... }` - payload stores 2+ nodes: `while node1 { node2+... }`
        crash, // e.g. `crash "blah"` - stores the expr in Node.Payload
        malformed, // e.g. tokenization or parsing failed (stores a Diagnostic.Tag)

        fn isBinOp(self: Tag) bool {
            // This is intentionally exhasustive so we don't forget to handle new variants that get added later.
            return switch (self) {
                .binop_equals,
                .binop_double_equals,
                .binop_not_equals,
                .binop_colon,
                .binop_colon_equals,
                .binop_dot,
                .binop_plus,
                .binop_minus,
                .binop_star,
                .binop_slash,
                .binop_double_slash,
                .binop_double_question,
                .binop_gt,
                .binop_gte,
                .binop_lt,
                .binop_lte,
                .binop_thick_arrow,
                .binop_thin_arrow,
                .binop_and,
                .binop_or,
                .binop_as,
                .binop_exposing,
                .binop_where,
                .binop_platform,
                .binop_pipe,
                => true,

                .uc,
                .lc,
                .lc_dot_ucs,
                .uc_dot_ucs,
                .var_lc,
                .apply_lc,
                .apply_uc,
                .apply_anon,
                .apply_module,
                .neg_lc,
                .not_lc,
                .dot_lc,
                .double_dot_lc,
                .dot_num,
                .underscore,
                .ellipsis,
                .import,
                .expect,
                .num_literal_i32,
                .int_literal_i32,
                .frac_literal_small,
                .str_literal_small,
                .num_literal_big,
                .int_literal_big,
                .frac_literal_big,
                .str_literal_big,
                .str_interpolation,
                .list_literal,
                .tuple_literal,
                .record_literal,
                .block,
                .lambda,
                .lambda_no_args,
                .match,
                .if_else,
                .if_without_else,
                .unary_not,
                .unary_neg,
                .unary_double_dot,
                .ret,
                .for_loop,
                .while_loop,
                .crash,
                .malformed,
                => false,
            };
        }
    };

    /// Represents a small decimal as numerator / (10^power)
    pub const SmallDec = struct {
        numerator: i16,
        denominator_power_of_ten: u8,
    };

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.

        list_elems: u32, // Number of elements in the list literal
        nodes: collections.NodeSlices(Node.Idx).Idx, // Nested nodes inside this one (e.g. statements in a block)
        body_then_args: collections.NodeSlices(Node.Idx).Idx, // For lambdas, the Node.Idx of the body followed by 0+ Node.Idx entries for args.
        if_branches: u32, // Branches before the `else` - each branch begins with a conditional node
        binop: collections.NodeSlices(Node.Idx).Idx, // Pass this to NodeSlices.binOp() to get lhs and rhs
        ident: Ident.Idx, // For both .uc and .lc tags

        // Number literals that are small enough to be stored inline right here - by far the most common case
        num_literal_i32: i32, // e.g. `42`
        int_literal_i32: i32, // e.g. `0x42`
        frac_literal_small: SmallDec, // e.g. `0.2` - fits in a 32-bit SmallDec

        // Number literals that don't fit inline, and must be instead stored in ByteSlices.
        num_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bignums
        int_literal_big: ByteSlices.Idx, // Stores length followed by 1-byte digits, for userspace bigints
        frac_literal_big: ByteSlices.Idx, // Like a bigint literal but stores 2 lengths first, for digits before and after decimal

        // String literals
        str_literal_small: [4]u8, // Null-terminated ASCII bytes (if there's a '\0' in it, then it must be .str_literal_big
        str_literal_big: ByteSlices.Idx, // Stores length followed by UTF-8 bytes (which can include \0 bytes).
        str_interpolated_nodes: collections.NodeSlices(Node.Idx).Idx, // Stores length followed by node indices (some will be string literal nodes)

        import_nodes: collections.NodeSlices(Node.Idx).Idx, // Stores imported module nodes for import statements

        malformed: Diagnostic.Tag, // Malformed nodes store the diagnostic tag
    };
};

// Diagnostic system for error reporting
pub const Diagnostic = struct {
    tag: Tag,
    region: Region,

    pub const Tag = enum {
        // Header errors
        multiple_platforms,
        no_platform,
        missing_header,
        missing_arrow,
        expected_exposes,
        expected_exposes_close_square,
        expected_exposes_open_square,
        expected_imports,
        expected_package_or_platform_name,
        expected_package_or_platform_colon,
        expected_package_or_platform_string,
        expected_package_platform_close_curly,
        expected_package_platform_open_curly,
        expected_packages,
        expected_packages_close_curly,
        expected_packages_open_curly,
        expected_platform_name_end,
        expected_platform_name_start,
        expected_platform_name_string,
        expected_platform_string,
        expected_provides,
        expected_provides_close_square,
        expected_provides_open_square,
        expected_requires,
        expected_requires_rigids_close_curly,
        expected_requires_rigids_open_curly,
        expected_requires_signatures_close_curly,
        expected_requires_signatures_open_curly,
        header_expected_open_square,
        header_expected_close_square,

        // Pattern errors
        pattern_unexpected_token,
        pattern_list_rest_old_syntax,
        pattern_unexpected_eof,
        bad_as_pattern_name,
        expected_arrow_after_pattern,
        expected_identifier_after_as,
        expected_lower_ident_pat_field_name,
        expected_colon_after_pat_field_name,

        // Type annotation errors
        ty_anno_unexpected_token,
        expected_type_field_name,
        expected_colon_after_type_field_name,
        expected_arrow,
        multi_arrow_needs_parens,
        expected_ty_close_curly_or_comma,
        expected_ty_close_square_or_comma,
        expected_ty_apply_close_round,
        expected_ty_anno_close_round,
        expected_ty_anno_close_round_or_comma,
        invalid_type_arg,

        // Statement errors
        statement_unexpected_token,
        expected_colon_after_type_annotation,
        import_must_be_top_level,

        // String errors
        string_unexpected_token,
        string_expected_close_interpolation,
        string_unclosed,

        // Expression errors
        expr_unexpected_token,
        expr_no_space_dot_int,
        state_not_implemented,
        no_else,
        expected_expr_bar,
        backslash_not_valid_lambda_syntax,
        obsolete_interface_keyword,
        expected_expr_close_curly_or_comma,
        expected_expr_close_round_or_comma,
        expected_expr_close_square_or_comma,
        expected_close_curly_at_end_of_match,
        expected_close_round,
        expected_open_curly_after_match,
        expected_expr_record_field_name,
        expected_expr_apply_close_round,
        expr_arrow_expects_ident,
        expected_expr_comma,
        expected_expr_close_curly,
        expr_dot_suffix_not_allowed,

        // Import/Export errors
        import_exposing_no_open,
        import_exposing_no_close,
        expected_lower_name_after_exposed_item_as,
        expected_upper_name_after_exposed_item_as,
        exposed_item_unexpected_token,
        expected_upper_name_after_import_as,

        // Where clause errors
        where_expected_mod_open,
        where_expected_var,
        where_expected_mod_close,
        where_expected_arg_open,
        where_expected_arg_close,
        where_expected_method_arrow,
        where_expected_method_or_alias_name,
        where_expected_module,
        where_expected_colon,
        where_expected_constraints,

        // Var errors
        var_only_allowed_in_a_body,
        var_must_have_ident,
        var_expected_equals,

        // For loop errors
        for_expected_in,

        // Match errors
        match_branch_wrong_arrow,
        match_branch_missing_arrow,

        // Internal parser errors (should not happen in correct parser)
        internal_parser_error,
    };
};

// Module header structures
pub const Header = union(enum) {
    app: struct {
        exposes: collections.NodeSlices(Node.Idx).Idx, // List of exposed items (exports)
        packages: collections.NodeSlices(Node.Idx).Idx, // List of package nodes including platform
        region: Position, // Start position
    },
    module: struct {
        exposes: collections.NodeSlices(Node.Idx).Idx, // List of exposed items (as nodes)
        region: Position,
    },
    package: struct {
        exposes: collections.NodeSlices(Node.Idx).Idx, // List of exposed items (as nodes)
        packages: collections.NodeSlices(Node.Idx).Idx, // List of package nodes
        region: Position,
    },
    platform: struct {
        name: Node.Idx, // Platform name identifier
        requires_rigids: collections.NodeSlices(Node.Idx).Idx, // List of type variables
        requires_signatures: Node.Idx, // Type annotation node
        exposes: collections.NodeSlices(Node.Idx).Idx, // List of exposed items
        packages: collections.NodeSlices(Node.Idx).Idx, // List of packages
        provides: collections.NodeSlices(Node.Idx).Idx, // List of provided items
        region: Position,
    },
    hosted: struct {
        exposes: collections.NodeSlices(Node.Idx).Idx, // List of exposed items
        region: Position,
    },
    interface: struct {
        exposes: collections.NodeSlices(Node.Idx).Idx, // List of exposed items
        imports: collections.NodeSlices(Node.Idx).Idx, // List of import nodes
        region: Position,
    },
    malformed: struct {
        diagnostic_tag: u32, // Error tag (we'll add proper diagnostics later)
        region: Position,
    },
};

pub fn tag(self: *const Ast, idx: Node.Idx) Node.Tag {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.tag, multi_list_idx);
}

pub fn start(self: *const Ast, idx: Node.Idx) Position {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.region, multi_list_idx).start;
}

pub fn getRegion(self: *const Ast, idx: Node.Idx) Region {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.region, multi_list_idx);
}

pub fn payload(self: *const Ast, idx: Node.Idx) Node.Payload {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.payload, multi_list_idx);
}

test "collections.NodeSlices with negative sentinel" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var node_slices = collections.NodeSlices(Node.Idx){ .entries = collections.SafeList(Node.Idx){} };
    defer node_slices.entries.items.deinit(allocator);

    // Test single node
    const single = [_]Node.Idx{@as(Node.Idx, @enumFromInt(42))};
    const idx1 = try node_slices.append(allocator, &single);

    var iter1 = node_slices.nodes(&idx1);
    try testing.expectEqual(@as(Node.Idx, @enumFromInt(42)), iter1.next().?);
    try testing.expectEqual(@as(?Node.Idx, null), iter1.next());

    // Test multiple nodes
    const multiple = [_]Node.Idx{
        @as(Node.Idx, @enumFromInt(10)),
        @as(Node.Idx, @enumFromInt(20)),
        @as(Node.Idx, @enumFromInt(30)),
    };
    const idx2 = try node_slices.append(allocator, &multiple);

    // idx2 should be 0 since single element was stored directly, not in entries
    try testing.expectEqual(@as(i32, 0), @intFromEnum(idx2));

    // Get entries to verify storage
    const entries = node_slices.entries.items.items;

    // First verify entries are correct before iterating
    try testing.expectEqual(@as(usize, 3), entries.len);
    try testing.expectEqual(@as(i32, 10), @intFromEnum(entries[0]));
    try testing.expectEqual(@as(i32, 20), @intFromEnum(entries[1]));

    var iter2 = node_slices.nodes(&idx2);
    const first = iter2.next().?;
    try testing.expectEqual(@as(Node.Idx, @enumFromInt(10)), first);
    const second = iter2.next().?;
    try testing.expectEqual(@as(Node.Idx, @enumFromInt(20)), second);
    const third = iter2.next().?;
    try testing.expectEqual(@as(Node.Idx, @enumFromInt(30)), third);
    try testing.expectEqual(@as(?Node.Idx, null), iter2.next());

    // Verify the actual storage format
    const sign_bit = std.math.minInt(i32);

    // Single element is NOT stored in entries (optimization)
    // It's encoded directly in idx1 with sign bit set
    const idx1_val = @intFromEnum(idx1);
    try testing.expect(idx1_val < 0); // Sign bit should be set
    // We add 1 during encoding, so we expect 43 after clearing sign bit
    try testing.expectEqual(@as(i32, 43), idx1_val & ~@as(i32, sign_bit)); // Shifted value should be 43

    // Multiple nodes ARE stored in entries (last one with sign bit set)
    try testing.expectEqual(@as(usize, 3), entries.len);
    try testing.expectEqual(@as(i32, 10), @intFromEnum(entries[0]));
    try testing.expectEqual(@as(i32, 20), @intFromEnum(entries[1]));
    const last_val = @intFromEnum(entries[2]);
    try testing.expect(last_val < 0); // Sign bit should be set
    try testing.expectEqual(@as(i32, 30), last_val & ~@as(i32, sign_bit)); // Original value should be 30
}

/// Convert a parse diagnostic to a reporting.Report for error display
fn parseDiagnosticToReport(self: *const @This(), env: *const CommonEnv, diagnostic: Diagnostic, allocator: std.mem.Allocator, filename: []const u8) !reporting.Report {
    _ = self; // AST is not needed for diagnostic to report conversion

    const title = switch (diagnostic.tag) {
        .missing_header => "MISSING HEADER",
        .multiple_platforms => "MULTIPLE PLATFORMS",
        .no_platform => "NO PLATFORM",
        .missing_arrow => "MISSING ARROW",
        .expected_exposes => "EXPECTED EXPOSES",
        .pattern_unexpected_token => "UNEXPECTED TOKEN IN PATTERN",
        .expected_arrow_after_pattern => "EXPECTED ARROW AFTER PATTERN",
        .expected_identifier_after_as => "EXPECTED IDENTIFIER AFTER 'AS'",
        .expr_unexpected_token => "UNEXPECTED TOKEN IN EXPRESSION",
        .state_not_implemented => "PARSER STATE NOT IMPLEMENTED",
        .string_unexpected_token => "UNEXPECTED TOKEN IN STRING",
        .ty_anno_unexpected_token => "UNEXPECTED TOKEN IN TYPE ANNOTATION",
        .statement_unexpected_token => "UNEXPECTED TOKEN",
        .import_must_be_top_level => "IMPORT MUST BE TOP LEVEL",
        .expected_expr_close_square_or_comma => "LIST NOT CLOSED",
        .no_else => "IF WITHOUT ELSE",
        else => "PARSE ERROR",
    };

    var report = reporting.Report.init(allocator, title, .runtime_error);

    // Add detailed error message based on the diagnostic type
    switch (diagnostic.tag) {
        .missing_header => {
            try report.document.addReflowingText("Roc files must start with a module header.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addText("For example:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addCodeBlock("module [main]");
            try report.document.addLineBreak();
            try report.document.addText("or for an app:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addCodeBlock("app [main!] { pf: platform \"../basic-cli/platform.roc\" }");
        },
        .multiple_platforms => {
            try report.document.addReflowingText("Only one platform declaration is allowed per file.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Remove the duplicate platform declaration.");
        },
        .no_platform => {
            try report.document.addReflowingText("App files must specify a platform.");
            try report.document.addLineBreak();
            try report.document.addText("Add a platform specification like:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addCodeBlock("{ pf: platform \"../basic-cli/platform.roc\" }");
        },
        .missing_arrow => {
            try report.document.addText("Expected an arrow ");
            try report.document.addAnnotated("->", .emphasized);
            try report.document.addText(" here.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Function type annotations require arrows between parameter and return types.");
        },
        .expected_exposes => {
            try report.document.addReflowingText("Module headers must have an ");
            try report.document.addKeyword("exposing");
            try report.document.addReflowingText(" section that lists what the module exposes.");
            try report.document.addLineBreak();
            try report.document.addText("For example: ");
            try report.document.addCodeBlock("module [main, add, subtract]");
        },
        .no_else => {
            try report.document.addText("This ");
            try report.document.addKeyword("if");
            try report.document.addText(" is being used as an expression, but it doesn't have an ");
            try report.document.addKeyword("else");
            try report.document.addText(".");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addText("When ");
            try report.document.addKeyword("if");
            try report.document.addText(" is used as an expression (to evaluate to a value), it must have an ");
            try report.document.addKeyword("else");
            try report.document.addText(" branch to specify what value to use when the condition is ");
            try report.document.addAnnotated("False", .emphasized);
            try report.document.addText(".");
        },
        .expr_unexpected_token => {
            try report.document.addText("I found an unexpected token while parsing an expression.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("I was expecting a valid expression, but I found something that doesn't belong here.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("This could be a missing operator, incorrect syntax, or a token in the wrong place.");
        },
        .state_not_implemented => {
            try report.document.addText("Parser encountered an unimplemented state.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("This is a limitation in the current parser implementation. The feature you're trying to use may not be fully implemented yet.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Please report this issue with your code example to help improve the parser.");
        },
        else => {
            // Generic parse error message
            try report.document.addText("A parsing error occurred: ");
            try report.document.addAnnotated(@tagName(diagnostic.tag), .emphasized);
            try report.document.addLineBreak();
            try report.document.addReflowingText("This is an unexpected parsing error. Please check your syntax.");
        },
    }

    // Add source location
    try report.document.addLineBreak();
    try report.document.addLineBreak();

    // Convert region to RegionInfo for reporting
    const region_info: base.RegionInfo = base.RegionInfo.position(env.source, env.line_starts.items.items, diagnostic.region.start.offset, diagnostic.region.end.offset) catch base.RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
    };

    try report.document.addSourceRegion(
        region_info,
        .error_highlight,
        filename,
        env.source,
        env.line_starts.items.items,
    );

    return report;
}
