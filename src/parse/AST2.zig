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
const collections = @import("collections");
const tokenize = @import("tokenize.zig");

const Region = base.Region;
const Position = Region.Position;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;
const Ast = @This();

nodes: collections.SafeMultiList(Node),
node_slices: NodeSlices, // Slices of node indices for things like list literals, used during other compilation stages
byte_slices: ByteSlices, // Slices of backing bytes for things like string literals and number literals, used at runtime
header: ?Header, // Optional module header (app, package, platform, etc.) - not used if we're parsing a standalone expression

/// Initialize a new AST with pre-allocated capacity
pub fn initCapacity(allocator: Allocator, estimated_node_count: usize) Allocator.Error!Ast {
    var nodes = collections.SafeMultiList(Node){};
    try nodes.ensureTotalCapacity(allocator, estimated_node_count);

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

pub const NodeSlices = struct {
    entries: collections.SafeList(NodeSlices.Entry),

    pub const Idx = enum(u32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const Entry = union {
        node_len: u32, // The number of Node.Idx values immediately following this. They will all be .node_idx entries.
        node_idx: Node.Idx, // An individual Node.Idx in a slice. (The slice will begin with a .node_len entry.)
        binop_lhs: Node.Idx, // This is a BinOp's lhs node, and its rhs will be stored immediately after this entry.
        binop_rhs: Node.Idx, // This is a BinOp's rhs node, and its lhs will be stored immediately before this entry.
    };

    pub fn slice(self: *const NodeSlices, idx: NodeSlices.Idx) []Node.Idx {
        const slice_len = @as(usize, @intCast(self.entries.items.items[idx.asUsize()].node_len));
        const slice_start = idx.asUsize() + 1;

        return self.nodes()[slice_start .. slice_start + slice_len];
    }

    pub fn binOp(self: *const NodeSlices, idx: NodeSlices.Idx) Node.BinOp {
        const lhs_idx = idx.asUsize();
        return .{
            .lhs = self.nodes()[lhs_idx],
            .rhs = self.nodes()[lhs_idx + 1],
        };
    }

    fn nodes(self: *const NodeSlices) []Node.Idx {
        // Cast the entries to Node.Idx - this should only be used internally when returning slices of
        // entries that are known to be Node.Idx, because not all nodes in this list are Node.Idx!
        return @as([*]Node.Idx, @ptrCast(self.entries.items.items.ptr))[0..self.entries.items.items.len];
    }
};

pub const ByteSlices = struct {
    entries: collections.SafeList(u8),

    pub const Idx = enum(u32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub fn slice(self: *const ByteSlices, idx: ByteSlices.Idx) []u8 {
        const ptr = self.entries.items.items.ptr + idx;
        const slice_len = *@as(*const u32, @ptrCast(ptr));
        const slice_start = @as(usize, @intCast(ptr + @sizeOf(u32)));

        return self.entries.items.items[slice_start .. slice_start + @as(usize, @intCast(slice_len))];
    }

    /// Appends the given slice inline to the bytes, with the u32 length written first
    /// (after up to three zeros for alignment padding as necessary), then returns
    /// the index of the length.
    pub fn append(self: *ByteSlices, allocator: Allocator, bytes: []u8) Allocator.Error!ByteSlices.Idx {
        // We may need some alignment padding bytes to store a u32 in our bytes array.
        const current_len = self.entries.items.len;
        const len_type = u32;
        const len_size = @sizeOf(len_type);
        const len_alignment = @alignOf(len_type);
        const padding = (len_alignment - (current_len % len_alignment)) % len_alignment;

        // Store the length right after the alignment padding.
        const len_idx = current_len + padding;

        // Reserve enough space for alignment padding, u32 length, and the actual bytes.
        try self.entries.ensureUnusedCapacity(allocator, padding + len_size + bytes.len);

        // Branchlessly zero out the padding by appending three zeros.
        // There will definitely be enough space, because we just reserved
        // space for at *least* the 4-byte length, and if it turned out
        // we didn't need any padding, the length will override these anyway.
        // This approach guarantees we don't pay for a branch misprediction.
        inline for (0..len_size - 1) |_| {
            self.entries.appendAssumeCapacity(0);
        }

        // Now that we've padded our way to the correct alignment, write the length.
        std.debug.assert(@intFromPtr(self.entries.items.ptr + len_idx) % len_alignment == 0);
        const len_ptr = @as(*len_type, @ptrCast(@alignCast(self.entries.items.ptr + len_idx)));
        len_ptr.* = @as(len_type, @intCast(bytes.len));
        self.entries.items.len = len_idx + len_size;

        // Append the bytes after the length.
        self.entries.appendSliceAssumeCapacity(bytes);

        // Return the index where the length was written
        return @as(ByteSlices.Idx, @enumFromInt(@as(len_size, @intCast(len_idx))));
    }
};

/// Returns a slice of all the nodes in a block.
/// Panics in debug builds if the given Node.Idx does not refer to a .block node.
pub fn nodesInBlock(self: *const Ast, idx: Node.Idx) []Node.Idx {
    std.debug.assert(self.tag(idx) == .block);

    return self.node_slices.slice(self.payload(idx).block_nodes);
}

/// Returns a slice of all the nodes in a string interpolation.
/// Panics in debug builds if the given Node.Idx does not refer to a .str_interpolation node.
pub fn nodesInInterpolation(self: *const Ast, idx: Node.Idx) []Node.Idx {
    std.debug.assert(self.tag(idx) == .str_interpolation);

    return self.node_slices.slice(self.payload(idx).str_interpolated_nodes);
}

/// A lambda expression, e.g. `|a, b| c`
pub const Lambda = struct {
    args: []Node.Idx,
    body: Node.Idx,
};

/// Panics in debug builds if the given Node.Idx does not refer to a .lambda node.
pub fn lambda(self: *const Ast, idx: Node.Idx) Lambda {
    std.debug.assert(self.tag(idx) == .lambda);

    const body_then_args = self.node_slices.slice(self.payload(idx).body_then_args);

    return .{
        .body = body_then_args[0],
        .args = body_then_args[1..],
    };
}

/// Panics in debug builds if the given node is not a BinOp.
pub fn binOp(self: *const Ast, idx: Node.Idx) Node.BinOp {
    std.debug.assert(self.tag(idx).isBinOp());

    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.node_slices.binOp(self.nodes.fieldItem(.payload, multi_list_idx).binop);
}

/// Given the idx to a lambda, return the region of just its args (the `| ... |` including the pipes)
pub fn lambdaArgsRegion(self: *const Ast, idx: Node.Idx, raw_src: []u8, ident_store: *const Ident.Store) Region {
    // Opening `|` args delimiter
    const region_start = self.start(idx);

    // The closing `|` delimiter is the next token after the end of the last arg node.
    const args = self.lambda(idx).args;
    const last_arg_end =
        if (args.len > 0)
            self.region(args[args.len - 1], raw_src, ident_store).end.offset
        else
            region_start.offset; // If it had no args, e.g. `||`, start right after the opening `|`
    const after_last_arg = last_arg_end + 1;
    const region_end = after_last_arg + nextTokenIndex(raw_src[after_last_arg..]);

    return .{
        .start = region_start,
        .end = Position{ .offset = @as(u32, @intCast(region_end)) },
    };
}

/// Given the idx to a BinOp, return the region of just its symbol (e.g. "*" or "==") etc.
pub fn binOpSymbolRegion(self: *const Ast, idx: Node.Idx, raw_src: []u8, ident_store: *const Ident.Store) Region {
    const binop = self.binOp(idx);

    // To find the binop symbol itself, we scan from lhs_end to either rhs_start or first whitespace.
    const lhs_end: usize = self.region(binop.lhs, raw_src, ident_store).end.offset;
    const rhs_start: usize = self.region(binop.rhs, raw_src, ident_store).start.offset;

    // These relationships should always be true. If not, there is a bug in some earlier step!
    std.debug.assert(lhs_end < rhs_start);
    std.debug.assert(rhs_start < raw_src.len);

    // Find the next token (non-whitespace, skipping comments) starting from the byte after lhs_end.
    // That must be where the binop symbol starts.
    const binop_start = nextTokenIndex(raw_src[lhs_end + 1 ..]);

    // Binop ends when we hit whitespace or rhs_start (e.g. `x+y` would end when we hit rhs_start - so, `y`)
    var binop_end = binop_start + 1;
    var src_byte = raw_src[binop_end];

    while (!tokenize.Token.isWhitespace(src_byte) and (binop_end + 1) < rhs_start) {
        binop_end += 1;
        std.debug.assert(binop_end < raw_src.len); // We should never run off the end.
        src_byte = raw_src[binop_end];
    }

    return .{
        .start = Position{ .offset = @as(u32, @intCast(binop_start)) },
        .end = Position{ .offset = @as(u32, @intCast(binop_end)) },
    };
}

/// Returns the region spanning the entire node and everything inside it.
///
/// The raw source bytes are necesary because (to save memory) some node types don't
/// store their exact region, and need to scan to find the delimiter location (or similar).
/// The ident store is necessary to get the lengths of any (string-interned) ident nodes.
pub fn region(
    self: *const Ast,
    idx: Node.Idx,
    raw_src: []u8,
    ident_store: *const Ident.Store,
) base.Region {
    switch (self.tag(idx)) {
        .binop_equals,
        .binop_double_equals,
        .binop_not_equals,
        .binop_colon,
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
        .binop_pipe,
        => {
            const binop = self.binOp(idx);

            // The binop expression's region spans from the start of the lhs to the end of the rhs
            return .{
                .start = self.region(binop.lhs, raw_src, ident_store).start,
                .end = self.region(binop.rhs, raw_src, ident_store).end,
            };
        },
        .uc, .lc => {
            return self.identRegion(idx, ident_store);
        },
        .dot_lc, .not_lc, .neg_lc => {
            var ident_region = self.identRegion(idx, ident_store);
            ident_region.start.offset -= 1; // Account for the dot/bang/minus
            return ident_region;
        },
        .double_dot_lc => {
            var ident_region = self.identRegion(idx, ident_store);
            ident_region.start.offset -= 2; // Account for the ".."
            return ident_region;
        },
        .var_lc => {
            const ident_idx = self.payload(idx).ident;
            const ident_len = ident_store.getText(ident_idx).len;
            const region_start_pos = self.start(idx);

            // region_start begins at the `var`; skip over "var " and any whitespace/comments after it.
            var region_start_offset = region_start_pos.offset;
            region_start_offset += tokenize.Token.KW_VAR.len + 2; // +1 for the mandatory whitespace after `var`, +1 for ident start
            region_start_offset += @as(u32, @intCast(nextTokenIndex(raw_src[region_start_offset..])));

            return .{
                .start = Position{ .offset = region_start_offset },
                .end = Position{ .offset = region_start_offset + @as(u32, @intCast(ident_len)) },
            };
        },
        .dot_num => {
            @panic("TODO");
        },

        // // Literals that are small enough to be stored right here in .payload's u32 - by far the most common case for numbers
        // num_literal_i32, // e.g. `42`
        // int_literal_i32, // e.g. `0x42`
        // frac_literal_small, // e.g. `0.2` - fits in a 32-bit SmallDec
        // str_literal_small, // Null-terminated ASCII with escapes resolved (if it contains '\0', must use .str_literal_big)

        // // Literals that don't fit in u32, and must be instead stored in AstData.
        // num_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bignums
        // int_literal_big, // Digit length followed by 1-byte digits (across multiple AstData entries), for userspace bigints
        // frac_literal_big, // Like a bigint literal but stores 2 lengths first, for digits before/after decimal point
        // str_literal_big, // Byte length followed by UTF-8 bytes (across multiple AstData entries) with all escapes resolved.
        .str_interpolation => {
            // Opening quote
            const region_start = self.start(idx);

            // If the last node is a string literal, then it ends in a quote and we can just
            // use its ending region.
            // However, if it's *not* a string literal - e.g. the interpolation is `"abc${def}"` - then
            // we need to add to get the close quote that must be right after the closing `}` delimiter.
            const nodes = self.nodesInInterpolation(idx);
            const last_node_idx = nodes[nodes.len - 1];
            const last_node_tag = self.tag(last_node_idx);
            var region_end = self.region(last_node_idx, raw_src, ident_store).end;

            // Note: It's possible that we end in a string interpolation, but in that case we wouldn't actually
            // end in a quotation mark, because that would look like `"foo${"nested${blah}"}"` - this would be
            // a silly thing to write, but it's technically legal, and we still need to find the closing delimiter.
            if (last_node_tag != .str_literal_small and last_node_tag != .str_literal_big) {
                // Find the closing `}` after the interpolation's end
                const interpolation_end = nextTokenIndex(raw_src[@as(usize, @intCast(region_end.offset))..]);

                // +1 so we end on that close quote rather than on the closing `}` that ends the interpolation
                region_end.offset += @as(u32, @intCast(interpolation_end + 1));

                // Verify that there was in fact a close quote immediately after the `}`
                // (because otherwise our last node should have been a string literal).
                std.debug.assert(raw_src[@as(usize, @intCast(region_end.offset))] == '"');
            }

            return .{
                .start = region_start,
                .end = region_end,
            };
        },
        // list_literal, // e.g. `[1, 2, 3]` - note that this is nonempty; .empty_list has its own variant
        // tuple_literal, // e.g. `(foo, bar)` - we know it's a tuple literal because of the commas
        // record_literal, // e.g. `{ foo, bar }` or `{ foo, }` - only records have commas; `{ foo }` is a block

        .apply => {
            // e.g. `foo(bar, baz)` or `Foo(bar, baz)` or `(foo(bar, baz))(blah, etc)`
            @panic("TODO");
        },
        .block => {
            // Opening curly brace
            const region_start = self.start(idx);

            // The closing curly brace is the next token after the end of the last node in the block
            const nodes = self.nodesInBlock(idx);
            const last_node_region = self.region(nodes[nodes.len - 1], raw_src, ident_store);
            const after_last_node = @as(usize, @intCast(last_node_region.end.offset)) + 1;
            const region_end = after_last_node + nextTokenIndex(raw_src[after_last_node..]);

            return .{
                .start = region_start,
                .end = Position{ .offset = @as(u32, @intCast(region_end)) },
            };
        },
        .empty_record, .empty_list => {
            return .{
                .start = self.start(idx),
                .end = self.payload(idx).src_bytes_end,
            };
        },
        .lambda => {
            // Opening `|`
            const region_start = self.start(idx);

            // The closing `|` is the next token after the end of the last arg node.
            // (We provide the region of the `| ... |` rather than the entire lambda expression,
            // because that's trivial: span from the lambda's start region to end of its body region.)
            const args = self.lambda(idx).args;
            const last_arg_end =
                if (args.len > 0)
                    self.region(args[args.len - 1], raw_src, ident_store).end.offset
                else
                    region_start.offset; // If it had no args, e.g. `||`, start right after the opening `|`
            const after_last_arg = @as(usize, @intCast(last_arg_end)) + 1;
            const region_end = after_last_arg + nextTokenIndex(raw_src[after_last_arg..]);

            return .{
                .start = region_start,
                .end = Position{ .offset = @as(u32, @intCast(region_end)) },
            };
        },
        .num_literal_i32, .int_literal_i32, .frac_literal_small, .str_literal_small, .num_literal_big, .int_literal_big, .frac_literal_big, .str_literal_big, .list_literal, .tuple_literal, .record_literal, .match, .if_else, .if_without_else, .unary_not, .unary_neg, .unary_double_dot, .ret, .for_loop, .while_loop, .crash, .malformed => {
            @panic("TODO");
        },
    }
}

/// Given a Node.Idx that refers to an ident, return the region of the ident itself in the source bytes.
fn identRegion(self: *const Ast, idx: Node.Idx, ident_store: *const Ident.Store) Region {
    const region_start = self.start(idx);
    const ident_idx = self.payload(idx).ident;
    const ident_len = ident_store.getText(ident_idx).len;

    return .{
        .start = region_start,
        .end = Position{ .offset = region_start.offset + @as(u32, @intCast(ident_len)) },
    };
}

/// Returns the index of the next token (a non-whitespace byte, skipping over comments)
/// in the given slice. Panics in debug builds if we don't find a token; only call this
/// when you know there is a token that will be found! (This is for regenerating regions;
/// we should know there's a token before needing to use this.)
fn nextTokenIndex(bytes: []u8) usize {
    std.debug.assert(bytes.len > 0);

    var index: usize = 0;
    var byte = bytes[0];
    var is_in_comment = false;

    // Skip past whitespace and_or comments
    while (tokenize.Token.isWhitespace(byte) or is_in_comment) {
        // Branchlessly deal with entering and exiting line comments
        is_in_comment = byte == '#' or (is_in_comment and byte != '\n');
        index += 1;

        std.debug.assert(index < bytes.len); // We should never run off the end.
        byte = bytes[index];
    }

    return index;
}

pub const Node = struct {
    tag: Node.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
    payload: Node.Payload, // u32 union of extra information that varies based on tag

    pub const Idx = enum(u32) {
        _,

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }
    };

    pub const BinOp = struct {
        lhs: Node.Idx,
        rhs: Node.Idx,
    };

    pub const Tag = enum {
        // Binops
        binop_equals, //           =
        binop_double_equals, //    ==
        binop_not_equals, //       !=
        binop_colon, //            :
        binop_plus, //             +
        binop_minus, //            -
        binop_star, //             *
        binop_slash, //            /
        binop_double_slash, //     //
        binop_double_question, //  ??
        binop_gt, //               >
        binop_gte, //              >=
        binop_lt, //               <-
        binop_lte, //              <=
        binop_thick_arrow, //      =>
        binop_thin_arrow, //       ->
        binop_and, //              and
        binop_or, //               or
        binop_pipe, //             | for pattern alternatives (maybe we should replace this with `or`, not sure)

        // Identifiers, possibly with modifiers
        uc, // uppercase identifier (e.g. `Foo`) - could be a tag or a type
        lc, // lowercase identifier (e.g. `foo`) - could be a lookup, pattern ident, type variable, or record field name
        var_lc, // `var` followed by lowercase identifier (e.g. `var foo`)
        neg_lc, // negated identifier (e.g. `-foo`)
        not_lc, // bang followed by lowercase identifier (e.g. `!foo`)
        dot_lc, // dot followed by lowercase identifier (e.g. `.foo`)
        double_dot_lc, // two dots followed by lowercase identifier (e.g. `..others`)
        dot_num, // dot followed by number (e.g. `.0`) - this is a tuple accessor

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
        apply, // e.g. `foo(bar, baz)` or `Foo(bar, baz)` or `(foo(bar, baz))(blah, etc)`
        block, // Block with curly braces, e.g. `{ expr1, expr2, ... }` - could end up being a record (expr or destructure)
        empty_record, // e.g. `{}` - no data inside; we just store region and that's it.
        empty_list, // e.g. `[]` - no data inside; we just store region and that's it.
        lambda, // e.g. `|x, y| x + y` - payload stores a slice of body_then_args
        match, // e.g. `match cond { Ok(a) => a Err(b) => b }` - needs to store cond as well as branches
        if_else, // e.g. `if cond then_branch else_branch` - needs to store cond as well as branches. if-exprs must have else.
        if_without_else, // e.g. `if cond do_something!()` - stores exactly 1 cond node followed by exactly 1 body node
        unary_not, // e.g. `!(foo())` - note that `!foo` is special-cased to .not_lc instead
        unary_neg, // e.g. `-(foo())` - note that `-foo` is special-cased to .neg_lc instead
        unary_double_dot, // e.g. `..others`
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
                .binop_pipe,
                => true,

                .uc,
                .lc,
                .var_lc,
                .neg_lc,
                .not_lc,
                .dot_lc,
                .double_dot_lc,
                .dot_num,
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
                .apply,
                .block,
                .empty_record,
                .empty_list,
                .lambda,
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

    pub const Payload = union {
        src_bytes_end: Position, // The last byte where this node appeared in the source code. Used in error reporting.

        list_elems: u32, // Number of elements in the list literal
        block_nodes: NodeSlices.Idx, // Number of nodes in a block (or fields in a record, if it turns out to be a record)
        body_then_args: NodeSlices.Idx, // For lambdas, the Node.Idx of the body followed by 0+ Node.Idx entries for args.
        if_branches: u32, // Branches before the `else` - each branch begins with a conditional node
        match_branches: u32, // Total number of branches - each branch begins with an `if` (if there's a guard) or list (if multiple alternatives) or expr (normal pattern)
        binop: NodeSlices.Idx, // Pass this to NodeSlices.binOp() to get lhs and rhs
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
        str_interpolated_nodes: NodeSlices.Idx, // Stores length followed by node indices (some will be string literal nodes)
    };
};

// Diagnostic system for error reporting
pub const Diagnostic = struct {
    tag: Tag,
    region: Region,

    pub const Tag = enum {
        // Header errors
        missing_header,
        invalid_header,

        // Expression errors
        expr_unexpected_token,
        expr_incomplete,

        // Pattern errors
        pattern_unexpected_token,
        pattern_incomplete,

        // Type annotation errors
        type_unexpected_token,
        type_incomplete,

        // Add more as needed
    };
};

// Module header structures
pub const Header = union(enum) {
    app: struct {
        provides: NodeSlices.Idx, // List of exposed items (as nodes)
        platform_idx: Node.Idx, // Platform specification node
        packages: NodeSlices.Idx, // List of package nodes
        region: Position, // Start position
    },
    module: struct {
        exposes: NodeSlices.Idx, // List of exposed items (as nodes)
        region: Position,
    },
    package: struct {
        exposes: NodeSlices.Idx, // List of exposed items (as nodes)
        packages: NodeSlices.Idx, // List of package nodes
        region: Position,
    },
    platform: struct {
        name: Node.Idx, // Platform name identifier
        requires_rigids: NodeSlices.Idx, // List of type variables
        requires_signatures: Node.Idx, // Type annotation node
        exposes: NodeSlices.Idx, // List of exposed items
        packages: NodeSlices.Idx, // List of packages
        provides: NodeSlices.Idx, // List of provided items
        region: Position,
    },
    hosted: struct {
        exposes: NodeSlices.Idx, // List of exposed items
        region: Position,
    },
    interface: struct {
        exposes: NodeSlices.Idx, // List of exposed items
        imports: NodeSlices.Idx, // List of import nodes
        region: Position,
    },
    malformed: struct {
        diagnostic_tag: u32, // Error tag (we'll add proper diagnostics later)
        region: Position,
    },
};

fn tag(self: *const Ast, idx: Node.Idx) Node.Tag {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.tag, multi_list_idx);
}

fn start(self: *const Ast, idx: Node.Idx) Position {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.start, multi_list_idx);
}

fn payload(self: *const Ast, idx: Node.Idx) Node.Payload {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.payload, multi_list_idx);
}
