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

/// Append a new node to the AST
pub fn appendNode(self: *Ast, allocator: Allocator, start_pos: Position, node_tag: Node.Tag, node_payload: Node.Payload) Allocator.Error!Node.Idx {
    const idx = @intFromEnum(try self.nodes.append(allocator, .{
        .tag = node_tag,
        .start = start_pos,
        .payload = node_payload,
    }));
    return @as(Node.Idx, @enumFromInt(idx));
}

/// Append a slice of nodes and return an index to access them later
pub fn appendNodeSlice(self: *Ast, allocator: Allocator, nodes: []const Node.Idx) Allocator.Error!NodeSlices.Idx {
    return try self.node_slices.append(allocator, nodes);
}

/// Append binop operands and return an index to access them later
pub fn appendBinOp(self: *Ast, allocator: Allocator, lhs: Node.Idx, rhs: Node.Idx) Allocator.Error!NodeSlices.Idx {
    return try self.node_slices.appendBinOp(allocator, lhs, rhs);
}

/// Append bytes for literals and return an index to access them later
pub fn appendByteSlice(self: *Ast, allocator: Allocator, bytes: []const u8) Allocator.Error!ByteSlices.Idx {
    return try self.byte_slices.append(allocator, bytes);
}

pub const NodeSlices = struct {
    entries: collections.SafeList(NodeSlices.Entry),

    pub const Idx = enum(i32) {
        _,

        // NIL sentinel for empty slices - sign bit set (i32 min value)
        pub const NIL: Idx = @enumFromInt(std.math.minInt(i32));

        fn asUsize(self: Idx) usize {
            return @intCast(@intFromEnum(self));
        }

        pub fn isNil(self: Idx) bool {
            return self == NIL;
        }
    };

    pub const Entry = union {
        node_idx: Node.Idx, // An individual Node.Idx in a slice. The last one will be negative to mark the end.
        binop_lhs: Node.Idx, // This is a BinOp's lhs node, and its rhs will be stored immediately after this entry.
        binop_rhs: Node.Idx, // This is a BinOp's rhs node, and its lhs will be stored immediately before this entry.
    };

    pub fn append(self: *NodeSlices, allocator: Allocator, node_slice: []const Node.Idx) Allocator.Error!NodeSlices.Idx {
        // Handle empty slices by returning NIL
        if (node_slice.len == 0) {
            return Idx.NIL;
        }

        // OPTIMIZATION: For single elements, encode the Node.Idx directly with sign bit set
        // This saves 4 bytes by avoiding an indirection through NodeSlices
        if (node_slice.len == 1) {
            const node_val = @intFromEnum(node_slice[0]);
            // To avoid conflict with NIL (which is minInt(i32)), we need to ensure
            // we never produce that exact value. We'll add 1 before encoding.
            // This means valid single elements will be in range [minInt(i32)+1, -1]
            const shifted_val = node_val + 1;
            // Set the sign bit to indicate this is a direct Node.Idx, not a NodeSlices.Idx
            const encoded_val = shifted_val | std.math.minInt(i32);
            return @as(NodeSlices.Idx, @enumFromInt(encoded_val));
        }

        const idx = @as(NodeSlices.Idx, @enumFromInt(self.entries.items.items.len));

        // Reserve capacity for all nodes (no length stored anymore)
        try self.entries.items.ensureUnusedCapacity(allocator, node_slice.len);

        // Append all nodes except the last one
        for (node_slice[0 .. node_slice.len - 1]) |node| {
            self.entries.items.appendAssumeCapacity(.{ .node_idx = node });
        }

        // Append the last node with sign bit set to mark the end
        const last_node = node_slice[node_slice.len - 1];
        const last_val = @intFromEnum(last_node);
        // Set the sign bit to mark this as the last element
        const sign_bit = std.math.minInt(i32);
        const marked_val = last_val | sign_bit;
        const marked_last = @as(Node.Idx, @enumFromInt(marked_val));
        self.entries.items.appendAssumeCapacity(.{ .node_idx = marked_last });

        return idx;
    }

    pub fn appendBinOp(self: *NodeSlices, allocator: Allocator, lhs: Node.Idx, rhs: Node.Idx) Allocator.Error!NodeSlices.Idx {
        const idx = @as(NodeSlices.Idx, @enumFromInt(self.entries.items.items.len));

        // Reserve capacity for both nodes
        try self.entries.items.ensureUnusedCapacity(allocator, 2);

        self.entries.items.appendAssumeCapacity(.{ .binop_lhs = lhs });
        self.entries.items.appendAssumeCapacity(.{ .binop_rhs = rhs });

        return idx;
    }

    pub const Iterator = struct {
        entries: []const Entry,
        index: usize,
        done: bool,
        single_element: ?Node.Idx, // For single-element optimization

        pub fn next(self: *Iterator) ?Node.Idx {
            if (self.done) return null;

            // Handle single-element case
            if (self.single_element) |node| {
                self.done = true;
                // Clear the sign bit and subtract 1 to get the original node
                const node_val = @intFromEnum(node);
                const sign_bit = std.math.minInt(i32);
                const cleared_val = node_val & ~@as(i32, sign_bit);
                const original_val = cleared_val - 1; // Undo the shift we applied during encoding
                return @as(Node.Idx, @enumFromInt(original_val));
            }

            // Bounds check
            if (self.index >= self.entries.len) {
                self.done = true;
                return null;
            }

            const node_idx = self.entries[self.index].node_idx;
            const node_val = @intFromEnum(node_idx);

            self.index += 1;

            // Check if sign bit is set (marking the end)
            if (node_val < 0) {
                self.done = true;
                // Clear the sign bit to get the original value
                const sign_bit = std.math.minInt(i32);
                const original_val = node_val & ~@as(i32, sign_bit);
                return @as(Node.Idx, @enumFromInt(original_val));
            }

            return node_idx;
        }
    };

    pub fn nodes(self: *const NodeSlices, idx_ptr: *const NodeSlices.Idx) Iterator {
        const idx = idx_ptr.*;

        // Check for NIL sentinel (empty slice)
        if (idx == NodeSlices.Idx.NIL) {
            return .{
                .entries = self.entries.items.items,
                .index = 0,
                .done = true, // Mark as done immediately for empty list
                .single_element = null,
            };
        }

        // Check if this is a single element encoded directly (sign bit set)
        const idx_val = @intFromEnum(idx);
        if (idx_val < 0) {
            // This is a single Node.Idx encoded directly (negative value due to sign bit)
            // Store it for the iterator to return
            return .{
                .entries = self.entries.items.items,
                .index = 0,
                .done = false,
                .single_element = @as(Node.Idx, @enumFromInt(idx_val)), // Store with sign bit still set
            };
        }

        // Multiple elements - start reading from entries
        const slice_start = @as(usize, @intCast(idx_val));

        return .{
            .entries = self.entries.items.items,
            .index = slice_start,
            .done = false,
            .single_element = null,
        };
    }

    pub fn binOp(self: *const NodeSlices, idx: NodeSlices.Idx) Node.BinOp {
        // BinOps should never be single elements or NIL
        std.debug.assert(@intFromEnum(idx) >= 0); // Not encoded directly

        const entry_idx = @as(usize, @intCast(@intFromEnum(idx)));
        // The binop entries are stored as .binop_lhs and .binop_rhs
        // We need to extract the actual node indices from these entries
        const lhs = self.entries.items.items[entry_idx].binop_lhs;
        const rhs = self.entries.items.items[entry_idx + 1].binop_rhs;
        return .{
            .lhs = lhs,
            .rhs = rhs,
        };
    }
};

/// Returns an iterator over all the nodes in a block.
/// Panics in debug builds if the given Node.Idx does not refer to a .block node.
pub fn nodesInBlock(self: *const Ast, idx: Node.Idx) NodeSlices.Iterator {
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
pub fn nodesInInterpolation(self: *const Ast, idx: Node.Idx) NodeSlices.Iterator {
    std.debug.assert(self.tag(idx) == .str_interpolation);

    return self.node_slices.nodes(&self.payloadPtr(idx).str_interpolated_nodes);
}

/// A lambda expression, e.g. `|a, b| c`
pub const Lambda = struct {
    body: Node.Idx,
    args_idx: NodeSlices.Idx, // Index to access args via iterator
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
    iter: NodeSlices.Iterator,
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
pub fn binOp(self: *const Ast, idx: Node.Idx) Node.BinOp {
    std.debug.assert(self.tag(idx).isBinOp());

    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.node_slices.binOp(self.nodes.fieldItem(.payload, multi_list_idx).binop);
}

/// Special accessor for arrow nodes which may have multiple parameters
/// Returns the first parameter and return type as a BinOp for compatibility
pub fn arrowBinOp(self: *const Ast, idx: Node.Idx) Node.BinOp {
    const node_tag = self.tag(idx);
    std.debug.assert(node_tag == .binop_thin_arrow or node_tag == .binop_thick_arrow);

    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    const node_payload = self.nodes.fieldItem(.payload, multi_list_idx);

    // Try to interpret as a nodes payload (variadic arrow)
    // We can tell by checking if the first entry in the slice is a valid node
    const nodes_idx = node_payload.binop;
    if (nodes_idx != NodeSlices.Idx.NIL) {
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
pub fn arrowNodes(self: *const Ast, idx: Node.Idx) NodeSlices.Iterator {
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
pub fn lambdaArgsRegion(self: *const Ast, idx: Node.Idx, raw_src: []u8, ident_store: *const Ident.Store) Region {
    // Opening `|` args delimiter
    const region_start = self.start(idx);

    // The closing `|` delimiter is the next token after the end of the last arg node.
    const lambda_val = self.lambda(idx);
    var args_iter = self.lambdaArgs(lambda_val);

    // Find the last arg by iterating through all args
    var last_arg: ?Node.Idx = null;
    while (args_iter.next()) |arg| {
        last_arg = arg;
    }

    const last_arg_end =
        if (last_arg) |arg|
            self.region(arg, raw_src, ident_store).end.offset
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
        .binop_colon_equals,
        .binop_dot,
        .binop_as,
        .binop_where,
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
        .uc, .lc, .lc_dot_ucs, .uc_dot_ucs => {
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
            // .dot_num is for tuple accessors like .0, .1, etc.
            // The payload likely contains the number
            const region_start = self.start(idx);
            // Estimate: dot + 1-2 digits typically
            // Without knowing the exact number, assume 2 chars (.0 to .9 are most common)
            return .{
                .start = region_start,
                .end = Position{ .offset = region_start.offset + 2 },
            };
        },
        .underscore => {
            // Underscore is just a single character
            const region_start = self.start(idx);
            return .{
                .start = region_start,
                .end = Position{ .offset = region_start.offset + 1 },
            };
        },
        .ellipsis => {
            // Ellipsis is three characters
            const region_start = self.start(idx);
            return .{
                .start = region_start,
                .end = Position{ .offset = region_start.offset + 3 },
            };
        },
        .apply_lc, .apply_uc, .apply_anon, .apply_module => {
            // Function application: func(args...)
            // The payload should contain nodes for func and args
            const nodes_idx = self.payload(idx).nodes;

            // Check if this is an empty apply (no arguments)
            if (nodes_idx.isNil()) {
                // For empty apply, we need to determine the function name from the tag
                // and calculate the region based on that
                const region_start = self.start(idx);
                // This is tricky without the function node. We might need to keep some info.
                // For now, estimate based on typical function call pattern
                return .{
                    .start = region_start,
                    .end = Position{ .offset = region_start.offset + 2 }, // Rough estimate for "()"
                };
            }

            var iter = self.node_slices.nodes(&nodes_idx);

            // Get the first node (the function)
            // Safe: Parser ensures apply nodes always have at least the function node
            // The parseApply() function always includes the function as the first node
            const first_node = iter.next() orelse unreachable; // Should have at least the function

            // Find the last node (last argument or the function if no args)
            var last_node = first_node;
            while (iter.next()) |node| {
                last_node = node;
            }

            // The region spans from the function to the closing paren after the last arg
            const last_region = self.region(last_node, raw_src, ident_store);
            // Add 1 for the closing paren
            return .{
                .start = self.region(first_node, raw_src, ident_store).start,
                .end = Position{ .offset = last_region.end.offset + 1 },
            };
        },
        .import => {
            // Import statement spans from the 'import' keyword to the end of the imported items
            const region_start = self.start(idx);
            var iter = self.node_slices.nodes(&self.payloadPtr(idx).import_nodes);
            var last_node: ?Node.Idx = null;
            while (iter.next()) |node| {
                last_node = node;
            }

            if (last_node) |last_node_idx| {
                const last_node_region = self.region(last_node_idx, raw_src, ident_store);
                return .{
                    .start = region_start,
                    .end = last_node_region.end,
                };
            } else {
                // Empty import? Just use the keyword length
                return .{
                    .start = region_start,
                    .end = Position{ .offset = region_start.offset + 6 }, // "import" is 6 chars
                };
            }
        },
        .expect => {
            // Expect statement spans from the 'expect' keyword to the end of the condition expression
            const region_start = self.start(idx);
            var iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);

            if (iter.next()) |condition_node| {
                const condition_region = self.region(condition_node, raw_src, ident_store);
                return .{
                    .start = region_start,
                    .end = condition_region.end,
                };
            } else {
                // Empty expect? Just use the keyword length
                return .{
                    .start = region_start,
                    .end = Position{ .offset = region_start.offset + 6 }, // "expect" is 6 chars
                };
            }
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
            var nodes_iter = self.nodesInInterpolation(idx);
            var last_node_idx: Node.Idx = undefined;
            var has_nodes = false;
            while (nodes_iter.next()) |node| {
                last_node_idx = node;
                has_nodes = true;
            }
            // String interpolations should always have at least one node
            std.debug.assert(has_nodes);
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
        .block => {
            const nodes_iter = self.nodesInBlock(idx);
            return self.containerRegion(idx, nodes_iter, tokenize.Token.DELIM_CLOSE_CURLY, raw_src, ident_store);
        },
        .list_literal => {
            const nodes_iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);
            return self.containerRegion(idx, nodes_iter, tokenize.Token.DELIM_CLOSE_SQUARE, raw_src, ident_store);
        },
        .tuple_literal => {
            const nodes_iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);
            return self.containerRegion(idx, nodes_iter, tokenize.Token.DELIM_CLOSE_ROUND, raw_src, ident_store);
        },
        .record_literal => {
            const nodes_iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);
            return self.containerRegion(idx, nodes_iter, tokenize.Token.DELIM_CLOSE_CURLY, raw_src, ident_store);
        },
        .lambda => {
            // Opening `|`
            const region_start = self.start(idx);

            // The closing `|` is the next token after the end of the last arg node.
            // (We provide the region of the `| ... |` rather than the entire lambda expression,
            // because that's trivial: span from the lambda's start region to end of its body region.)
            const lambda_val = self.lambda(idx);
            var args_iter = self.lambdaArgs(lambda_val);
            var last_arg: ?Node.Idx = null;
            while (args_iter.next()) |arg| {
                last_arg = arg;
            }
            const last_arg_end =
                if (last_arg) |arg|
                    self.region(arg, raw_src, ident_store).end.offset
                else
                    region_start.offset; // If it had no args, e.g. `||`, start right after the opening `|`
            const after_last_arg = @as(usize, @intCast(last_arg_end)) + 1;
            const region_end = after_last_arg + nextTokenIndex(raw_src[after_last_arg..]);

            return .{
                .start = region_start,
                .end = Position{ .offset = @as(u32, @intCast(region_end)) },
            };
        },
        .lambda_no_args => {
            // Just `||` - no args
            const region_start = self.start(idx);
            const region_end = region_start.offset + 2; // Two pipes
            return .{
                .start = region_start,
                .end = Position{ .offset = region_end },
            };
        },
        .unary_not => {
            // `!` followed by expression - the payload should contain the operand node
            // For now, assume the operand is stored in block_nodes as a single-element slice
            const region_start = self.start(idx);
            const nodes_iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);
            var iter = nodes_iter;
            // Safe: Parser ensures unary_not nodes always have exactly one operand
            // The parser creates these with a single operand
            const operand = iter.next() orelse unreachable; // Should have the operand
            const operand_region = self.region(operand, raw_src, ident_store);

            return .{
                .start = region_start, // Start includes the `!`
                .end = operand_region.end,
            };
        },
        .unary_neg => {
            // `-` followed by expression - the payload should contain the operand node
            const region_start = self.start(idx);
            const nodes_iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);
            var iter = nodes_iter;
            // Safe: Parser ensures unary_neg nodes always have exactly one operand
            // The parser creates these with a single operand
            const operand = iter.next() orelse unreachable; // Should have the operand
            const operand_region = self.region(operand, raw_src, ident_store);

            return .{
                .start = region_start, // Start includes the `-`
                .end = operand_region.end,
            };
        },
        .unary_double_dot => {
            // `..` followed by expression - the payload should contain the operand node
            const region_start = self.start(idx);
            const nodes_iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);
            var iter = nodes_iter;
            // Safe: Parser ensures unary_double_dot nodes always have exactly one operand
            // The parser creates these with a single operand
            const operand = iter.next() orelse unreachable; // Should have the operand
            const operand_region = self.region(operand, raw_src, ident_store);

            return .{
                .start = region_start, // Start includes the `..`
                .end = operand_region.end,
            };
        },
        .ret => {
            // `return` followed by expression
            const region_start = self.start(idx);
            const nodes_iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);
            var iter = nodes_iter;
            // Safe: Parser ensures ret nodes always have exactly one expression
            // The parseReturn() function always creates ret nodes with an expression
            const expr = iter.next() orelse unreachable; // Should have the expression to return
            const expr_region = self.region(expr, raw_src, ident_store);

            return .{
                .start = region_start, // Start at the 'return' keyword
                .end = expr_region.end, // End at the end of the expression
            };
        },
        .crash => {
            // `crash` followed by expression
            const region_start = self.start(idx);
            const nodes_iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);
            var iter = nodes_iter;
            // Safe: Parser ensures crash nodes always have exactly one expression
            // The parseCrash() function always creates crash nodes with an expression
            const expr = iter.next() orelse unreachable; // Should have the expression to crash with
            const expr_region = self.region(expr, raw_src, ident_store);

            return .{
                .start = region_start, // Start at the 'crash' keyword
                .end = expr_region.end, // End at the end of the expression
            };
        },
        .while_loop => {
            // while condition body
            const while_val = self.whileLoop(idx);
            const region_start = self.start(idx);
            const body_region = self.region(while_val.body, raw_src, ident_store);
            return .{
                .start = region_start,
                .end = body_region.end,
            };
        },
        .for_loop => {
            // for pattern in expr body
            const for_val = self.forLoop(idx);
            const region_start = self.start(idx);
            const body_region = self.region(for_val.body, raw_src, ident_store);
            return .{
                .start = region_start,
                .end = body_region.end,
            };
        },
        .num_literal_i32, .int_literal_i32 => {
            // For inline number literals, scan from the start position to find the end
            const start_pos = self.start(idx);
            const start_offset = @as(usize, @intCast(start_pos.offset));

            // Scan for the end of the number literal
            var end_offset = start_offset;

            // Check for negative sign
            if (end_offset < raw_src.len and raw_src[end_offset] == '-') {
                end_offset += 1;
            }

            // Check for hex/octal/binary prefix
            if (self.tag(idx) == .int_literal_i32 and end_offset + 1 < raw_src.len and raw_src[end_offset] == '0') {
                const next_char = raw_src[end_offset + 1];
                if (next_char == 'x' or next_char == 'X' or next_char == 'o' or next_char == 'b') {
                    end_offset += 2; // Skip "0x", "0o", or "0b"

                    // Scan hex/octal/binary digits
                    while (end_offset < raw_src.len) : (end_offset += 1) {
                        const c = raw_src[end_offset];
                        if (next_char == 'x' or next_char == 'X') {
                            // Hex digits
                            if (!std.ascii.isHex(c) and c != '_') break;
                        } else if (next_char == 'o') {
                            // Octal digits
                            if ((c < '0' or c > '7') and c != '_') break;
                        } else if (next_char == 'b') {
                            // Binary digits
                            if (c != '0' and c != '1' and c != '_') break;
                        }
                    }
                } else {
                    // Regular decimal number
                    while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                        end_offset += 1;
                    }
                }
            } else {
                // Regular decimal number
                while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                    end_offset += 1;
                }
            }

            return base.Region{ .start = start_pos, .end = Position{ .offset = @as(u32, @intCast(end_offset)) } };
        },
        .frac_literal_small => {
            // For inline fraction literals, scan from the start position to find the end
            const start_pos = self.start(idx);
            const start_offset = @as(usize, @intCast(start_pos.offset));

            var end_offset = start_offset;

            // Check for negative sign
            if (end_offset < raw_src.len and raw_src[end_offset] == '-') {
                end_offset += 1;
            }

            // Scan integer part
            while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                end_offset += 1;
            }

            // Scan decimal point and fractional part
            if (end_offset < raw_src.len and raw_src[end_offset] == '.') {
                end_offset += 1;
                while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                    end_offset += 1;
                }
            }

            // Check for exponent
            if (end_offset < raw_src.len and (raw_src[end_offset] == 'e' or raw_src[end_offset] == 'E')) {
                end_offset += 1;
                if (end_offset < raw_src.len and (raw_src[end_offset] == '+' or raw_src[end_offset] == '-')) {
                    end_offset += 1;
                }
                while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                    end_offset += 1;
                }
            }

            return base.Region{ .start = start_pos, .end = Position{ .offset = @as(u32, @intCast(end_offset)) } };
        },
        .str_literal_small => {
            // For small string literals, scan from the start to find the closing quote
            const start_pos = self.start(idx);
            const start_offset = @as(usize, @intCast(start_pos.offset));

            var end_offset = start_offset;

            // Skip opening quote
            if (end_offset < raw_src.len and raw_src[end_offset] == '"') {
                end_offset += 1;

                // Scan until closing quote (handling escapes)
                while (end_offset < raw_src.len) {
                    if (raw_src[end_offset] == '\\' and end_offset + 1 < raw_src.len) {
                        end_offset += 2; // Skip escape sequence
                    } else if (raw_src[end_offset] == '"') {
                        end_offset += 1; // Include closing quote
                        break;
                    } else {
                        end_offset += 1;
                    }
                }
            }

            return base.Region{ .start = start_pos, .end = Position{ .offset = @as(u32, @intCast(end_offset)) } };
        },
        .num_literal_big, .int_literal_big, .frac_literal_big => {
            // For big number literals stored in ByteSlices, we need to scan the source
            const start_pos = self.start(idx);
            const start_offset = @as(usize, @intCast(start_pos.offset));

            var end_offset = start_offset;

            // Check for negative sign
            if (end_offset < raw_src.len and raw_src[end_offset] == '-') {
                end_offset += 1;
            }

            // Check for hex/octal/binary prefix for int_literal_big
            if (self.tag(idx) == .int_literal_big and end_offset + 1 < raw_src.len and raw_src[end_offset] == '0') {
                const next_char = raw_src[end_offset + 1];
                if (next_char == 'x' or next_char == 'X' or next_char == 'o' or next_char == 'b') {
                    end_offset += 2;

                    // Scan appropriate digits
                    while (end_offset < raw_src.len) : (end_offset += 1) {
                        const c = raw_src[end_offset];
                        if (next_char == 'x' or next_char == 'X') {
                            if (!std.ascii.isHex(c) and c != '_') break;
                        } else if (next_char == 'o') {
                            if ((c < '0' or c > '7') and c != '_') break;
                        } else if (next_char == 'b') {
                            if (c != '0' and c != '1' and c != '_') break;
                        }
                    }
                } else {
                    // Regular decimal
                    while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                        end_offset += 1;
                    }
                }
            } else {
                // Regular decimal or fraction
                while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                    end_offset += 1;
                }

                // For fractions, check for decimal point
                if (self.tag(idx) == .frac_literal_big and end_offset < raw_src.len and raw_src[end_offset] == '.') {
                    end_offset += 1;
                    while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                        end_offset += 1;
                    }

                    // Check for exponent
                    if (end_offset < raw_src.len and (raw_src[end_offset] == 'e' or raw_src[end_offset] == 'E')) {
                        end_offset += 1;
                        if (end_offset < raw_src.len and (raw_src[end_offset] == '+' or raw_src[end_offset] == '-')) {
                            end_offset += 1;
                        }
                        while (end_offset < raw_src.len and (std.ascii.isDigit(raw_src[end_offset]) or raw_src[end_offset] == '_')) {
                            end_offset += 1;
                        }
                    }
                }
            }

            return base.Region{ .start = start_pos, .end = Position{ .offset = @as(u32, @intCast(end_offset)) } };
        },
        .str_literal_big => {
            // For big string literals, scan from the start to find the closing quote or end of multiline string
            const start_pos = self.start(idx);
            const start_offset = @as(usize, @intCast(start_pos.offset));

            var end_offset = start_offset;

            // Check if it's a multiline string (starts with """)
            if (end_offset + 2 < raw_src.len and
                raw_src[end_offset] == '"' and
                raw_src[end_offset + 1] == '"' and
                raw_src[end_offset + 2] == '"')
            {
                // Multiline string
                end_offset += 3; // Skip opening """

                // Scan until closing """
                while (end_offset + 2 < raw_src.len) {
                    if (raw_src[end_offset] == '"' and
                        raw_src[end_offset + 1] == '"' and
                        raw_src[end_offset + 2] == '"')
                    {
                        end_offset += 3; // Include closing """
                        break;
                    }
                    end_offset += 1;
                }
            } else if (end_offset < raw_src.len and raw_src[end_offset] == '"') {
                // Regular string
                end_offset += 1; // Skip opening quote

                // Scan until closing quote (handling escapes)
                while (end_offset < raw_src.len) {
                    if (raw_src[end_offset] == '\\' and end_offset + 1 < raw_src.len) {
                        end_offset += 2; // Skip escape sequence
                    } else if (raw_src[end_offset] == '"') {
                        end_offset += 1; // Include closing quote
                        break;
                    } else {
                        end_offset += 1;
                    }
                }
            }

            return base.Region{ .start = start_pos, .end = Position{ .offset = @as(u32, @intCast(end_offset)) } };
        },
        .match => {
            // Match expressions contain: scrutinee, then branch patterns and bodies
            // The region spans from the 'match' keyword to the end of the last branch
            const region_start = self.start(idx);
            var iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);

            var last_node: ?Node.Idx = null;
            while (iter.next()) |node| {
                last_node = node;
            }

            if (last_node) |last_node_idx| {
                const last_node_region = self.region(last_node_idx, raw_src, ident_store);
                return .{
                    .start = region_start,
                    .end = last_node_region.end,
                };
            } else {
                // Empty match? Just use the keyword length
                return .{
                    .start = region_start,
                    .end = Position{ .offset = region_start.offset + 5 }, // "match" is 5 chars
                };
            }
        },
        .if_else => {
            // If-else contains: condition, then branch, else branch
            const region_start = self.start(idx);
            var iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);

            var last_node: ?Node.Idx = null;
            while (iter.next()) |node| {
                last_node = node;
            }

            if (last_node) |last_node_idx| {
                const last_node_region = self.region(last_node_idx, raw_src, ident_store);
                return .{
                    .start = region_start,
                    .end = last_node_region.end,
                };
            } else {
                // Empty if? Just use the keyword length
                return .{
                    .start = region_start,
                    .end = Position{ .offset = region_start.offset + 2 }, // "if" is 2 chars
                };
            }
        },
        .if_without_else => {
            // If without else contains: condition, then branch
            const region_start = self.start(idx);
            var iter = self.node_slices.nodes(&self.payloadPtr(idx).nodes);

            var last_node: ?Node.Idx = null;
            while (iter.next()) |node| {
                last_node = node;
            }

            if (last_node) |last_node_idx| {
                const last_node_region = self.region(last_node_idx, raw_src, ident_store);
                return .{
                    .start = region_start,
                    .end = last_node_region.end,
                };
            } else {
                // Empty if? Just use the keyword length
                return .{
                    .start = region_start,
                    .end = Position{ .offset = region_start.offset + 2 }, // "if" is 2 chars
                };
            }
        },
        .malformed => {
            // Malformed nodes only have a start position
            // We return a zero-width region at that position
            const region_start = self.start(idx);
            return .{
                .start = region_start,
                .end = region_start,
            };
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

/// Helper function to calculate region for containers with delimiters (blocks, lists, records, tuples)
/// Takes the nodes iterator and expected closing delimiter character
fn containerRegion(
    self: *const Ast,
    idx: Node.Idx,
    nodes_iter: NodeSlices.Iterator,
    closing_delimiter: u8,
    raw_src: []u8,
    ident_store: *const Ident.Store,
) Region {
    const region_start = self.start(idx);

    // Find the last node in the container
    var iter = nodes_iter;
    var last_node_idx: ?Node.Idx = null;
    while (iter.next()) |node| {
        last_node_idx = node;
    }

    const closing_delim_offset = if (last_node_idx) |last_node| blk: {
        // Has nodes - find closing delimiter after last node
        const last_node_region = self.region(last_node, raw_src, ident_store);
        const after_last_node = @as(usize, @intCast(last_node_region.end.offset)) + 1;
        const next_token_offset = nextTokenIndex(raw_src[after_last_node..]);
        const delim_offset = after_last_node + next_token_offset;

        // Verify we found the expected delimiter
        std.debug.assert(raw_src[delim_offset] == closing_delimiter);
        break :blk delim_offset;
    } else blk: {
        // Empty container - find closing delimiter immediately after opening delimiter
        const after_open = @as(usize, @intCast(region_start.offset)) + 1;
        const next_token_offset = nextTokenIndex(raw_src[after_open..]);
        const delim_offset = after_open + next_token_offset;

        // Verify we found the expected delimiter
        std.debug.assert(raw_src[delim_offset] == closing_delimiter);
        break :blk delim_offset;
    };

    return .{
        .start = region_start,
        .end = Position{ .offset = @as(u32, @intCast(closing_delim_offset)) },
    };
}

pub const Node = struct {
    tag: Node.Tag, // u8 discriminant
    start: Position, // u32 UTF-8 bytes from start of source bytes where this begins
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
        binop_where, //            where (for type constraints)
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
                .binop_where,
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
        nodes: NodeSlices.Idx, // Nested nodes inside this one (e.g. statements in a block)
        body_then_args: NodeSlices.Idx, // For lambdas, the Node.Idx of the body followed by 0+ Node.Idx entries for args.
        if_branches: u32, // Branches before the `else` - each branch begins with a conditional node
        match_branches: u32, // Total number of branches - each branch begins with an `if` (if there's a guard) or list (if multiple alternatives) or expr (normal pattern)
        binop: NodeSlices.Idx, // Pass this to NodeSlices.binOp() to get lhs and rhs
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
        str_interpolated_nodes: NodeSlices.Idx, // Stores length followed by node indices (some will be string literal nodes)

        import_nodes: NodeSlices.Idx, // Stores imported module nodes for import statements

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
        no_else,
        expected_expr_bar,
        expected_expr_close_curly_or_comma,
        expected_expr_close_round_or_comma,
        expected_expr_close_square_or_comma,
        expected_close_curly_at_end_of_match,
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

pub fn tag(self: *const Ast, idx: Node.Idx) Node.Tag {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.tag, multi_list_idx);
}

pub fn start(self: *const Ast, idx: Node.Idx) Position {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.start, multi_list_idx);
}

pub fn payload(self: *const Ast, idx: Node.Idx) Node.Payload {
    const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(@intFromEnum(idx)));
    return self.nodes.fieldItem(.payload, multi_list_idx);
}

test "NodeSlices with negative sentinel" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var node_slices = NodeSlices{ .entries = collections.SafeList(NodeSlices.Entry){} };
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
    try testing.expectEqual(@as(i32, 10), @intFromEnum(entries[0].node_idx));
    try testing.expectEqual(@as(i32, 20), @intFromEnum(entries[1].node_idx));

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
    try testing.expectEqual(@as(i32, 10), @intFromEnum(entries[0].node_idx));
    try testing.expectEqual(@as(i32, 20), @intFromEnum(entries[1].node_idx));
    const last_val = @intFromEnum(entries[2].node_idx);
    try testing.expect(last_val < 0); // Sign bit should be set
    try testing.expectEqual(@as(i32, 30), last_val & ~@as(i32, sign_bit)); // Original value should be 30
}

/// Convert a parse diagnostic to a reporting.Report for error display
pub fn parseDiagnosticToReport(self: *const @This(), env: *const CommonEnv, diagnostic: Diagnostic, allocator: std.mem.Allocator, filename: []const u8) !reporting.Report {
    _ = self; // unused for now

    const title = switch (diagnostic.tag) {
        .missing_header => "MISSING HEADER",
        .multiple_platforms => "MULTIPLE PLATFORMS",
        .no_platform => "NO PLATFORM",
        .missing_arrow => "MISSING ARROW",
        .expected_exposes => "EXPECTED EXPOSES",
        .pattern_unexpected_token => "UNEXPECTED TOKEN IN PATTERN",
        .expr_unexpected_token => "UNEXPECTED TOKEN IN EXPRESSION",
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
