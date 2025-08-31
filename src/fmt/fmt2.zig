//! Production-ready formatter for AST2
//! Matches the behavior of the original fmt.zig but works with AST2/Parser2

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const collections = @import("collections");
const can = @import("can");

const AST2 = parse.AST2;
const Node = AST2.Node;
const Parser2 = parse.Parser2;
const tokenize_iter = parse.tokenize_iter;
const Token = tokenize_iter.Token;
const CommonEnv = base.CommonEnv;
const ByteSlices = collections.ByteSlices;
const Position = base.Region.Position;
const Ident = base.Ident;

/// Main formatter struct for AST2
const Formatter = struct {
    allocator: std.mem.Allocator,
    ast: *const AST2,
    source: []const u8,
    ident_store: *const Ident.Store,
    tokens: []const Token, // Now includes comment tokens!
    output: std.ArrayList(u8),

    // Formatting state
    curr_indent_level: u32 = 0,
    has_newline: bool = true, // Starts true since beginning of file is considered a newline
    token_cursor: usize = 0, // Current position in tokens array for efficient traversal

    fn init(allocator: std.mem.Allocator, ast: *const AST2, source: []const u8, ident_store: *const Ident.Store, tokens: []const Token) !Formatter {
        return Formatter{
            .allocator = allocator,
            .ast = ast,
            .source = source,
            .ident_store = ident_store,
            .tokens = tokens, // Use pre-collected tokens including comments
            .output = std.ArrayList(u8).init(allocator),
            .token_cursor = 0,
        };
    }

    fn deinit(self: *Formatter) void {
        // tokens are now owned externally, don't deinit them
        self.output.deinit();
    }

    fn format(self: *Formatter, root_node: ?Node.Idx) !void {

        // Format the header if present
        if (self.ast.header) |header| {
            try self.formatHeader(header);
            try self.ensureBlankLine();
        }

        // Format the root node (which is usually a block containing all top-level statements)
        if (root_node) |root| {
            const node = self.getNode(root);
            if (node.tag == .block) {
                // Format the block contents without the braces
                try self.formatBlockContents(root);
            } else {
                // Single expression or statement
                try self.formatNode(root);
            }
        }

        // Flush any trailing comments using token-based approach
        if (self.tokens.len > 0) {
            _ = try self.flushTrailingComments();
        }
    }

    /// Helper to get a node from an index - eliminates massive boilerplate
    inline fn getNode(self: *const Formatter, idx: Node.Idx) Node {
        return self.ast.nodes.get(nodeIdxToMultiListIdx(idx));
    }

    /// Convert Node.Idx (i32) to SafeMultiList.Idx (u32 enum)
    inline fn nodeIdxToMultiListIdx(idx: Node.Idx) collections.SafeMultiList(Node).Idx {
        const idx_val = @intFromEnum(idx);
        const u32_idx = @as(u32, @intCast(idx_val));
        return @as(collections.SafeMultiList(Node).Idx, @enumFromInt(u32_idx));
    }

    /// Convert u32 payload to NodeSlices.Idx
    inline fn payloadToNodeSlicesIdx(payload_u32: u32) collections.NodeSlices(Node.Idx).Idx {
        // The payload is already the index value (possibly bit-cast in parser)
        return @as(collections.NodeSlices(Node.Idx).Idx, @enumFromInt(payload_u32));
    }

    /// Flush any comment and blank line tokens before the given position
    fn flushCommentsBeforePosition(self: *Formatter, pos: Position) !bool {
        var found_any = false;

        while (self.token_cursor < self.tokens.len) {
            const token = self.tokens[self.token_cursor];

            // Stop if we've reached or passed the position
            if (token.region.start.offset >= pos.offset) {
                break;
            }

            switch (token.tag) {
                .LineComment, .DocComment => {
                    // Ensure we're on a new line before outputting the comment
                    if (!self.has_newline) {
                        try self.pushAll(" ");
                    } else {
                        try self.pushIndent();
                    }

                    // Output the comment text (doesn't include newline)
                    try self.pushLiteralFromRegion(token.region.start.offset, token.region.end.offset);

                    // Add newline after comment
                    try self.newline();

                    found_any = true;
                    self.token_cursor += 1;
                },
                .BlankLine => {
                    // When we encounter a BlankLine while flushing comments, we need to handle it
                    // A blank line after a comment should be output as just a newline
                    // (the previous comment already added one newline, so we just need one more)

                    // Debug: Make sure we're not copying the source content with spaces
                    if (std.debug.runtime_safety) {
                        const region = self.source[token.region.start.offset..token.region.end.offset];
                        var space_count: usize = 0;
                        for (region) |c| {
                            if (c == ' ') space_count += 1;
                        }
                        if (space_count >= 4) {
                            std.debug.print("WARNING: BlankLine token contains {} spaces, but we're NOT copying them\n", .{space_count});
                        }
                    }

                    // Output a clean newline for the blank line (no spaces!)
                    try self.newline();
                    self.token_cursor += 1;
                    found_any = true;
                },
                else => break, // Stop at non-comment/blank tokens
            }
        }

        return found_any;
    }

    fn formatHeader(self: *Formatter, header: AST2.Header) !void {
        switch (header) {
            .module => |m| {
                _ = try self.flushCommentsBeforePosition(m.region);
                try self.pushAll("module");

                // Format the exposed list
                try self.formatExposedList(m.exposes);
            },
            .app => |a| {
                _ = try self.flushCommentsBeforePosition(a.region);
                try self.pushAll("app");

                // Format exports list BEFORE packages
                if (a.exposes != collections.NodeSlices(Node.Idx).Idx.NIL) {
                    try self.pushAll(" ");
                    try self.formatExposedList(a.exposes);
                }

                // Check if we need multiline formatting
                const packages_idx = a.packages;
                var needs_multiline = false;

                if (packages_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
                    // Use multiline only if we have multiple packages
                    // Single package with platform clause should stay on one line
                    var package_count: usize = 0;

                    var check_it = self.ast.node_slices.nodes(&packages_idx);
                    while (check_it.next()) |_| {
                        package_count += 1;
                    }

                    // Check if there are comments between packages or a trailing comma
                    var pkg_iter = self.ast.node_slices.nodes(&packages_idx);
                    var prev_end: usize = 0;
                    while (pkg_iter.next()) |pkg_idx| {
                        const pkg_node = self.getNode(pkg_idx);
                        if (prev_end > 0 and self.hasCommentBetween(prev_end, pkg_node.region.start.offset)) {
                            needs_multiline = true;
                            break;
                        }
                        prev_end = pkg_node.region.end.offset;
                    }

                    // Also check for trailing comma after last package using tokens
                    if (!needs_multiline and prev_end > 0) {
                        // Binary search to find tokens after the last package
                        var cursor: usize = 0;
                        var search_end = self.tokens.len;
                        while (cursor < search_end) {
                            const mid = cursor + (search_end - cursor) / 2;
                            if (self.tokens[mid].region.start.offset < prev_end) {
                                cursor = mid + 1;
                            } else {
                                search_end = mid;
                            }
                        }

                        // Look for a comma token after the last package
                        while (cursor < self.tokens.len) : (cursor += 1) {
                            const token = self.tokens[cursor];
                            switch (token.tag) {
                                .Comma => {
                                    needs_multiline = true;
                                    break;
                                },
                                .CloseCurly => break, // Found the closing brace
                                .LineComment, .DocComment => continue, // Skip comments
                                else => break, // Any other token means no trailing comma
                            }
                        }
                    }
                }

                if (needs_multiline) {
                    // Multiline format with proper comment preservation

                    // Comments after "app" keyword will be handled when we flush before the next token

                    try self.ensureNewline();
                    try self.pushIndent();
                    try self.push('{');
                    try self.ensureNewline();
                    self.curr_indent_level += 1;

                    if (packages_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
                        var it = self.ast.node_slices.nodes(&packages_idx);
                        while (it.next()) |pkg| {
                            const pkg_node = self.getNode(pkg);

                            // Flush any comments before this package
                            _ = try self.flushCommentsBeforePosition(pkg_node.region.start);

                            try self.pushIndent();

                            if (pkg_node.tag == .binop_colon) {
                                const binop = self.ast.node_slices.binOp(pkg_node.payload.binop);
                                try self.formatNode(binop.lhs);
                                try self.push(':');
                                try self.ensureSpace();

                                const value_node = self.getNode(binop.rhs);
                                if (value_node.tag == .binop_platform) {
                                    const platform_binop = self.ast.node_slices.binOp(value_node.payload.binop);
                                    // Format: "path" platform [exports]
                                    try self.formatNode(platform_binop.lhs); // The path string
                                    try self.pushAll(" platform ");

                                    // Platform exports should always format as list
                                    try self.formatPlatformExports(platform_binop.rhs);
                                } else {
                                    try self.formatNode(binop.rhs);
                                }
                            } else {
                                try self.formatNode(pkg);
                            }

                            try self.push(',');
                            try self.ensureNewline();
                        }
                    }

                    self.curr_indent_level -= 1;
                    try self.pushIndent();
                    try self.push('}');
                } else {
                    // Single line format (simpler case)
                    try self.ensureSpace();
                    try self.pushAll("{ ");
                    if (packages_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
                        var it = self.ast.node_slices.nodes(&packages_idx);
                        var first = true;
                        while (it.next()) |pkg| {
                            if (!first) try self.pushAll(", ");
                            first = false;
                            const pkg_node = self.getNode(pkg);
                            if (pkg_node.tag == .binop_colon) {
                                const binop = self.ast.node_slices.binOp(pkg_node.payload.binop);

                                try self.formatNode(binop.lhs);
                                try self.push(':');
                                try self.ensureSpace();
                                const value_node = self.getNode(binop.rhs);
                                if (value_node.tag == .binop_platform) {
                                    const platform_binop = self.ast.node_slices.binOp(value_node.payload.binop);
                                    // Format: "path" platform [exports]
                                    try self.formatNode(platform_binop.lhs); // The path string
                                    try self.pushAll(" platform ");

                                    // Platform exports should always format as list
                                    try self.formatPlatformExports(platform_binop.rhs);
                                } else {
                                    try self.formatNode(binop.rhs);
                                }
                            } else {
                                try self.formatNode(pkg);
                            }
                        }
                    }
                    try self.ensureSpace();
                    try self.push('}');
                }
            },
            .package => |p| {
                _ = try self.flushCommentsBeforePosition(p.region);
                try self.pushAll("package");

                // Comments after package keyword will be handled when we flush before the next token

                // Format exposes
                try self.formatExposedList(p.exposes);

                // Format packages
                const packages_idx = p.packages;
                if (packages_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
                    try self.pushAll(" packages ");
                    try self.push('{');
                    var it = self.ast.node_slices.nodes(&packages_idx);
                    var first = true;
                    while (it.next()) |pkg| {
                        if (!first) try self.pushAll(", ");
                        first = false;
                        try self.formatNode(pkg);
                    }
                    try self.push('}');
                }
            },
            .platform => |p| {
                _ = try self.flushCommentsBeforePosition(p.region);
                try self.pushAll("platform");

                // Comments after platform keyword will be handled when we flush before the next token

                // Format name
                try self.pushAll(" ");
                try self.formatNode(p.name);

                // Format requires if present
                if (p.requires_rigids != collections.NodeSlices(Node.Idx).Idx.NIL or @intFromEnum(p.requires_signatures) != std.math.minInt(i32)) {
                    try self.pushAll(" requires ");

                    // Format rigids
                    const rigids_idx = p.requires_rigids;
                    if (rigids_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
                        try self.push('{');
                        var it = self.ast.node_slices.nodes(&rigids_idx);
                        var first = true;
                        while (it.next()) |rigid| {
                            if (!first) try self.pushAll(", ");
                            first = false;
                            try self.formatNode(rigid);
                        }
                        try self.pushAll("} ");
                    }

                    // Format signatures
                    try self.formatNode(p.requires_signatures);
                }

                // Format exposes
                try self.pushAll(" exposes");
                try self.formatExposedList(p.exposes);

                // Format packages
                const packages_idx = p.packages;
                if (packages_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
                    try self.pushAll(" packages ");
                    try self.push('{');
                    var it = self.ast.node_slices.nodes(&packages_idx);
                    var first = true;
                    while (it.next()) |pkg| {
                        if (!first) try self.pushAll(", ");
                        first = false;
                        try self.formatNode(pkg);
                    }
                    try self.push('}');
                }
            },
            .hosted => |h| {
                _ = try self.flushCommentsBeforePosition(h.region);
                try self.pushAll("hosted");

                // Format exposes
                try self.formatExposedList(h.exposes);
            },
            .interface => |i| {
                _ = try self.flushCommentsBeforePosition(i.region);
                try self.pushAll("interface");

                // Format exposes
                try self.formatExposedList(i.exposes);
            },
            .malformed => |m| {
                _ = try self.flushCommentsBeforePosition(m.region);
                // Malformed headers just get flushed as-is
            },
        }
    }

    fn formatExposedList(self: *Formatter, exposes_idx: collections.NodeSlices(Node.Idx).Idx) !void {
        try self.ensureSpace();
        try self.pushAll("[");

        // Check if we have exposed items
        // NodeSlices uses NIL (minInt(i32)) for empty, all other values are valid indices
        if (exposes_idx == collections.NodeSlices(Node.Idx).Idx.NIL) {
            // Truly empty list
            try self.push(']');
            return;
        }

        // Count items and check for comments to decide on formatting
        var item_count: usize = 0;
        var has_comments = false;
        var iter = self.ast.node_slices.nodes(&exposes_idx);

        // Get the first node to check for comments before it
        var first_node_idx: ?Node.Idx = null;

        // First pass: count and check for complexity
        while (iter.next()) |node_idx| {
            item_count += 1;
            if (first_node_idx == null) {
                first_node_idx = node_idx;
            }
            const node = self.getNode(node_idx);

            // Check if there are comments before this node
            // Save current cursor position and check ahead
            const saved_cursor = self.token_cursor;
            var check_cursor = self.token_cursor;
            while (check_cursor < self.tokens.len) : (check_cursor += 1) {
                const token = self.tokens[check_cursor];
                if (token.region.start.offset >= node.region.start.offset) break;
                switch (token.tag) {
                    .LineComment, .DocComment => {
                        has_comments = true;
                        break;
                    },
                    else => {},
                }
            }
            self.token_cursor = saved_cursor;
        }

        // Use multiline if there are comments or trailing comma
        // Check for trailing comma using tokens
        var has_trailing_comma = false;
        if (item_count > 0) {
            // Reset iterator to find last item
            iter = self.ast.node_slices.nodes(&exposes_idx);
            var last_node_idx: Node.Idx = undefined;
            while (iter.next()) |n| {
                last_node_idx = n;
            }
            const last_node = self.getNode(last_node_idx);

            // Use binary search to find tokens after the last node
            var cursor: usize = 0;
            var search_end = self.tokens.len;
            while (cursor < search_end) {
                const mid = cursor + (search_end - cursor) / 2;
                if (self.tokens[mid].region.start.offset < last_node.region.end.offset) {
                    cursor = mid + 1;
                } else {
                    search_end = mid;
                }
            }

            // Look for a comma token immediately after the last node
            while (cursor < self.tokens.len) : (cursor += 1) {
                const token = self.tokens[cursor];
                switch (token.tag) {
                    .Comma => {
                        has_trailing_comma = true;
                        break;
                    },
                    .CloseSquare => break, // Found the closing bracket, no trailing comma
                    .LineComment, .DocComment => continue, // Skip comments
                    else => break, // Any other token means no trailing comma
                }
            }
        }

        const multiline = has_comments or has_trailing_comma;

        if (multiline) {
            self.curr_indent_level += 1;
            // Check for comments after the opening bracket before newline
            if (first_node_idx) |first_idx| {
                const first_node = self.getNode(first_idx);
                _ = try self.flushCommentsBeforePosition(first_node.region.start);
            }
            try self.ensureNewline();
        }

        // Reset iterator for second pass
        iter = self.ast.node_slices.nodes(&exposes_idx);
        var first = true;

        while (iter.next()) |node_idx| {
            if (multiline) {
                // For items after the first, check for comments
                if (!first) {
                    const node = self.getNode(node_idx);
                    _ = try self.flushCommentsBeforePosition(node.region.start);
                }
                try self.pushIndent();
            } else {
                if (!first) try self.pushAll(", ");
            }

            try self.formatNode(node_idx);

            if (multiline) {
                try self.push(',');
                try self.ensureNewline();
            }

            first = false;
        }

        if (multiline) {
            self.curr_indent_level -= 1;
            try self.pushIndent();
        }

        try self.push(']');
    }

    fn formatNode(self: *Formatter, node_idx: Node.Idx) anyerror!void {
        const node = self.getNode(node_idx);

        // Flush any comments before this node
        _ = try self.flushCommentsBeforePosition(node.region.start);

        try self.formatNodeWithoutCommentFlush(node_idx);

        // Check for blank lines after this node (for separating statement groups)
        try self.flushBlankLinesAfterPosition(node.region.end);
    }

    fn formatNodeInBlock(self: *Formatter, node_idx: Node.Idx) anyerror!void {
        const node = self.getNode(node_idx);

        // Flush any comments before this node
        _ = try self.flushCommentsBeforePosition(node.region.start);

        // After flushing comments (which end with a newline) or if no comments,
        // we're at the start of a line and just need indentation
        try self.pushIndent();

        try self.formatNodeWithoutCommentFlush(node_idx);

        // Check for blank lines after this node (for separating statement groups)
        try self.flushBlankLinesAfterPosition(node.region.end);
    }

    fn flushBlankLinesAfterPosition(self: *Formatter, pos: Position) !void {
        // Look ahead for blank line tokens immediately after this position
        if (self.token_cursor < self.tokens.len) {
            const token = self.tokens[self.token_cursor];
            if (token.tag == .BlankLine and token.region.start.offset >= pos.offset) {
                // Found a blank line token after this statement
                // Add an extra newline to create the blank line

                // Debug: Make sure we just add a clean newline, nothing else
                if (std.debug.runtime_safety) {
                    const output_before = self.output.items.len;
                    try self.newline();
                    const output_after = self.output.items.len;

                    // Check what was added
                    if (output_after > output_before) {
                        const added = self.output.items[output_before..output_after];
                        if (added.len != 1 or added[0] != '\n') {
                            std.debug.panic("ERROR: flushBlankLinesAfterPosition added more than just newline: {s}\n", .{added});
                        }
                    }
                } else {
                    try self.newline();
                }
                self.token_cursor += 1;
            }
        }
    }

    fn formatNodeWithoutCommentFlush(self: *Formatter, node_idx: Node.Idx) anyerror!void {
        const node = self.getNode(node_idx);

        switch (node.tag) {
            // Binary operations
            .binop_equals => try self.formatBinOp(node_idx, "="),
            .binop_double_equals => try self.formatBinOp(node_idx, "=="),
            .binop_not_equals => try self.formatBinOp(node_idx, "!="),
            .binop_colon => try self.formatTypeAnnotation(node_idx),
            .binop_colon_equals => try self.formatNominalTypeDefinition(node_idx),
            .binop_dot => try self.formatDotAccess(node_idx),
            .binop_plus => try self.formatBinOp(node_idx, "+"),
            .binop_minus => try self.formatBinOp(node_idx, "-"),
            .binop_star => try self.formatBinOp(node_idx, "*"),
            .binop_slash => try self.formatBinOp(node_idx, "/"),
            .binop_double_slash => try self.formatBinOp(node_idx, "//"),
            .binop_double_question => try self.formatBinOp(node_idx, "??"),
            .binop_gt => try self.formatBinOp(node_idx, ">"),
            .binop_gte => try self.formatBinOp(node_idx, ">="),
            .binop_lt => try self.formatBinOp(node_idx, "<"),
            .binop_lte => try self.formatBinOp(node_idx, "<="),
            .binop_thick_arrow => try self.formatBinOp(node_idx, "=>"),
            .binop_thin_arrow => try self.formatBinOp(node_idx, "->"),
            .binop_and => try self.formatBinOp(node_idx, "&&"),
            .binop_or => try self.formatBinOp(node_idx, "||"),
            .binop_as => try self.formatBinOp(node_idx, "as"),
            .binop_exposing => try self.formatBinOp(node_idx, "exposing"),
            .binop_where => try self.formatWhereClause(node_idx),
            .binop_platform => try self.formatBinOp(node_idx, "platform"),
            .binop_pipe => {
                // Check if this is a module field access pattern
                const pipe_node = self.getNode(node_idx);
                const binop = self.ast.node_slices.binOp(pipe_node.payload.binop);
                const lhs_node = self.getNode(binop.lhs);
                const rhs_node = self.getNode(binop.rhs);

                // If LHS is apply_module and RHS is dot_lc, format as module field access
                if (lhs_node.tag == .apply_module and rhs_node.tag == .dot_lc) {
                    try self.formatNode(binop.lhs);
                    try self.formatNode(binop.rhs);
                } else {
                    // Regular pipe for pattern alternatives
                    try self.formatBinOp(node_idx, "|");
                }
            },

            // Identifiers
            .uc => try self.formatIdent(node_idx),
            .lc => try self.formatIdent(node_idx),
            .dot_lc => try self.formatDotIdent(node_idx),
            .uc_dot_ucs => try self.formatQualifiedIdent(node_idx),
            .lc_dot_ucs => try self.formatQualifiedIdent(node_idx),
            .dot_num => try self.formatDotNum(node_idx),
            .underscore => try self.pushAll("_"),
            .var_lc => {
                try self.pushAll("var ");
                try self.formatIdent(node_idx);
            },
            .neg_lc => {
                try self.pushAll("-");
                try self.formatIdent(node_idx);
            },
            .not_lc => {
                try self.formatIdent(node_idx);
                try self.pushAll("!");
            },
            .double_dot_lc => {
                try self.pushAll("..");
                try self.formatIdent(node_idx);
            },
            .ellipsis => try self.pushAll("..."),
            .import => try self.formatImport(node_idx),
            .expect => try self.formatExpectStatement(node_idx),

            // Literals - format from source to preserve underscores
            .num_literal_i32, .num_literal_big => {
                try self.formatNumberFromSource(node_idx);
            },
            .int_literal_i32, .int_literal_big => {
                try self.formatNumberFromSource(node_idx);
            },
            .frac_literal_small, .frac_literal_big => {
                try self.formatNumberFromSource(node_idx);
            },
            .str_literal_small, .str_literal_big => {
                try self.formatStringFromSource(node_idx);
            },
            .str_interpolation => try self.formatStringInterpolation(node_idx),

            // Blocks and collections
            .block => try self.formatBlock(node_idx),
            .list_literal => try self.formatList(node_idx),
            .record_literal => try self.formatRecord(node_idx),
            .tuple_literal => try self.formatTuple(node_idx),

            // Control flow
            .if_else => try self.formatIfElse(node_idx),
            .if_without_else => try self.formatIfWithoutElse(node_idx),
            .match => try self.formatMatch(node_idx),
            .lambda => try self.formatLambda(node_idx),
            .lambda_no_args => {
                // Lambda with no args should just be a lambda with empty args
                // Format as ||
                try self.pushAll("||");
                // The body is in the nodes payload
                var iter = self.ast.node_slices.nodes(&node.payload.nodes);
                if (iter.next()) |body_idx| {
                    try self.pushAll(" ");
                    try self.formatNode(body_idx);
                }
            },

            // Application
            .apply_lc, .apply_uc, .apply_anon => try self.formatApply(node_idx),
            .apply_module => {
                // Module application in where clauses
                try self.pushAll("module(");
                var iter = self.ast.node_slices.nodes(&node.payload.nodes);
                var first = true;
                while (iter.next()) |arg_idx| {
                    if (!first) {
                        try self.pushAll(", ");
                    }
                    try self.formatNode(arg_idx);
                    first = false;
                }
                try self.push(')');
            },

            // Unary operators
            .unary_not => try self.formatUnaryNot(node_idx),
            .unary_neg => try self.formatUnaryNeg(node_idx),

            // Import/Export

            // Loops
            .for_loop => try self.formatForLoop(node_idx),
            .while_loop => try self.formatWhileLoop(node_idx),

            // Statements
            .crash => try self.formatCrash(node_idx),

            // Unary double dot operator
            .unary_double_dot => {
                try self.pushAll("..");
                // The expression follows in the nodes payload
                var iter = self.ast.node_slices.nodes(&node.payload.nodes);
                if (iter.next()) |expr_idx| {
                    try self.formatNode(expr_idx);
                }
            },

            // Return statements
            .ret => {
                try self.pushAll("return");
                // Return has nodes payload with the expression
                var iter = self.ast.node_slices.nodes(&node.payload.nodes);
                if (iter.next()) |expr_idx| {
                    // Check if expression needs space before it
                    const expr_node = self.getNode(expr_idx);
                    const needs_space = switch (expr_node.tag) {
                        // No space for parenthesized expressions or certain literals
                        .tuple_literal => false,
                        .list_literal => false,
                        .record_literal => false,
                        .block => false,
                        else => true,
                    };

                    if (needs_space) {
                        try self.ensureSpace();
                    }

                    try self.formatNode(expr_idx);
                }
            },

            // Malformed nodes - preserve source bytes but skip pure whitespace
            .malformed => |_| {
                // For malformed nodes, we need to be careful not to output raw whitespace
                // from the source, as that violates our formatting rules
                const start = node.region.start.offset;
                const end = node.region.end.offset;

                if (end <= self.source.len) {
                    const source_text = self.source[start..end];

                    // Check if this is just whitespace - if so, skip it
                    var is_all_whitespace = true;
                    for (source_text) |c| {
                        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') {
                            is_all_whitespace = false;
                            break;
                        }
                    }

                    // Only output non-whitespace malformed content
                    if (!is_all_whitespace) {
                        // For malformed content, we need to replace source indentation with tabs
                        // Split the content by lines and re-indent with tabs

                        var line_start = start;
                        var i = start;
                        while (i < end) : (i += 1) {
                            if (self.source[i] == '\n') {
                                // Output this line with tab indentation
                                const line_end = i;

                                // Count leading spaces
                                var space_count: usize = 0;
                                var content_start = line_start;
                                while (content_start < line_end and self.source[content_start] == ' ') {
                                    content_start += 1;
                                    space_count += 1;
                                }

                                // If the line had indentation, add a single tab
                                if (space_count > 0 and content_start < line_end) {
                                    try self.push('\t');
                                }

                                // Output the content without leading spaces
                                if (content_start < line_end) {
                                    try self.pushLiteralFromRegion(content_start, line_end);
                                }

                                // Add the newline
                                try self.newline();

                                // Move to next line
                                line_start = i + 1;
                            }
                        }

                        // Handle last line if it doesn't end with newline
                        if (line_start < end) {
                            // Count leading spaces
                            var space_count: usize = 0;
                            var content_start = line_start;
                            while (content_start < end and self.source[content_start] == ' ') {
                                content_start += 1;
                                space_count += 1;
                            }

                            // If the line had indentation, add a single tab
                            if (space_count > 0 and content_start < end) {
                                try self.push('\t');
                            }

                            // Output the content without leading spaces
                            if (content_start < end) {
                                try self.pushLiteralFromRegion(content_start, end);
                            }
                        }
                    }

                    // Update token cursor to skip past this malformed region
                    while (self.token_cursor < self.tokens.len and
                        self.tokens[self.token_cursor].region.start.offset < end)
                    {
                        self.token_cursor += 1;
                    }
                }
            },
        }

        // Comments after nodes will be handled when we flush before the next token
    }

    fn formatDotAccess(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const binop_idx = node.payload.binop;
        const binop = self.ast.node_slices.binOp(binop_idx);

        // Format dot access without spaces
        try self.formatNode(binop.lhs);
        try self.push('.');
        try self.formatNode(binop.rhs);
    }

    fn formatBinOp(self: *Formatter, node_idx: Node.Idx, op_str: []const u8) !void {
        // Debug check: operators should never have leading or trailing whitespace
        if (std.debug.runtime_safety) {
            if (op_str.len > 0) {
                if (op_str[0] == ' ' or op_str[0] == '\t') {
                    std.debug.panic("ERROR: Operator has leading whitespace: '{s}'\n", .{op_str});
                }
                if (op_str[op_str.len - 1] == ' ' or op_str[op_str.len - 1] == '\t') {
                    std.debug.panic("ERROR: Operator has trailing whitespace: '{s}'\n", .{op_str});
                }
            }
        }

        const node = self.getNode(node_idx);
        const binop_idx = node.payload.binop;
        const binop = self.ast.node_slices.binOp(binop_idx);

        const lhs = binop.lhs;
        const rhs = binop.rhs;

        // Special case for binop_platform: format as "string" platform [provides]
        if (node.tag == .binop_platform) {
            try self.formatNode(lhs); // The platform string
            try self.ensureSpace();
            try self.pushAll(op_str); // "platform"
            try self.ensureSpace();

            // The RHS is a block containing the provides list - format it as a list
            const rhs_node = self.getNode(rhs);
            if (rhs_node.tag == .block) {
                // Format as a list with square brackets
                const multiline = self.collectionIsMultiline(rhs);

                try self.pushAll("[");

                if (multiline) {
                    self.curr_indent_level += 1;
                }

                var it = self.ast.node_slices.nodes(&rhs_node.payload.nodes);
                var first_elem = true;
                while (it.next()) |elem| {
                    if (multiline) {
                        if (!first_elem) {
                            try self.push(',');
                        }
                        try self.ensureNewline();
                        try self.pushIndent();
                    } else {
                        if (!first_elem) {
                            try self.pushAll(", ");
                        }
                    }
                    first_elem = false;
                    try self.formatNode(elem);
                }

                if (multiline) {
                    // Always add trailing comma in multiline mode
                    try self.push(',');
                    self.curr_indent_level -= 1;
                    try self.ensureNewline();
                    try self.pushIndent();
                }

                try self.pushAll("]");
            } else {
                // Shouldn't happen but fall back to regular formatting
                try self.formatNode(rhs);
            }
            return;
        }

        // Special case: binop_pipe with identifiers is actually field access or qualified name
        // like Bool.True being parsed as (binop_pipe (uc "Bool") (uc "True"))
        // or Stdout.line! being parsed as (binop_pipe (uc "Stdout") (not_lc "line"))
        if (node.tag == .binop_pipe) {
            const lhs_node = self.getNode(lhs);
            const rhs_node = self.getNode(rhs);
            // Check if this looks like field access or qualified name
            if ((lhs_node.tag == .uc or lhs_node.tag == .lc) and
                (rhs_node.tag == .uc or rhs_node.tag == .lc or rhs_node.tag == .dot_lc or rhs_node.tag == .not_lc))
            {
                // This is field access or a qualified name (Roc doesn't have a pipe operator)
                try self.formatNode(lhs);
                try self.push('.');

                // For dot_lc, we already have the dot, so just format the identifier part
                // For not_lc, format it as identifier followed by !
                if (rhs_node.tag == .dot_lc) {
                    // Skip the dot since we already added it
                    try self.formatIdent(rhs);
                } else if (rhs_node.tag == .not_lc) {
                    // Format as identifier!
                    try self.formatIdent(rhs);
                    try self.push('!');
                } else {
                    // Regular identifier
                    try self.formatNode(rhs);
                }
                return;
            }
        }

        // Check if we need parentheses for operands based on precedence
        const needs_lhs_parens = self.needsParens(lhs, node.tag, true);
        const needs_rhs_parens = self.needsParens(rhs, node.tag, false);

        var multiline = self.nodeWillBeMultiline(node_idx);

        // Special handling for dot-access with multiline
        if (node.tag == .binop_dot and multiline) {
            // Format as multiline dot-access
            if (needs_lhs_parens) try self.push('(');
            try self.formatNode(lhs);
            if (needs_lhs_parens) try self.push(')');

            try self.ensureNewline();
            self.curr_indent_level += 1;
            try self.pushIndent();
            try self.push('.');

            if (needs_rhs_parens) try self.push('(');
            try self.formatNode(rhs);
            if (needs_rhs_parens) try self.push(')');

            self.curr_indent_level -= 1;
            return;
        }

        // Special handling for equals in statements
        if (node.tag == .binop_equals) {
            // Format pattern
            if (needs_lhs_parens) try self.push('(');
            try self.formatNode(lhs);
            if (needs_lhs_parens) try self.push(')');

            try self.ensureSpace();
            try self.pushAll("=");

            // Remember where we are before formatting RHS to detect if it's multiline
            const output_len_before_rhs = self.output.items.len;

            // Check for comments before RHS
            const rhs_node = self.getNode(rhs);
            const has_comments = try self.flushCommentsBeforePosition(rhs_node.region.start);

            if (has_comments) {
                // Force multiline if we found any comments
                multiline = true;
            }

            if (multiline) {
                self.curr_indent_level += 1;
                if (!has_comments) {
                    // Only add blank line if there wasn't a comment (which already added newline)
                    try self.ensureBlankLine();
                    try self.pushIndent();
                } else {
                    // Comment already added newline and indentation
                    // Don't add duplicate indentation
                }
                if (needs_rhs_parens) try self.push('(');
                try self.formatNode(rhs);
                if (needs_rhs_parens) try self.push(')');
                self.curr_indent_level -= 1;

                // Add a blank line after multiline assignment expressions
                try self.ensureBlankLine();
            } else {
                try self.ensureSpace();
                if (needs_rhs_parens) try self.push('(');
                try self.formatNode(rhs);
                if (needs_rhs_parens) try self.push(')');

                // Check if the RHS actually formatted as multiline
                // by checking if any newlines were added
                var formatted_multiline = false;
                for (self.output.items[output_len_before_rhs..]) |c| {
                    if (c == '\n') {
                        formatted_multiline = true;
                        break;
                    }
                }

                // If the RHS formatted as multiline, add a blank line after it
                if (formatted_multiline) {
                    try self.ensureBlankLine();
                }
            }
        } else {
            // Normal binary operator
            if (needs_lhs_parens) try self.push('(');
            try self.formatNode(lhs);
            if (needs_lhs_parens) try self.push(')');

            // Comments around operators will be handled when we flush before the RHS

            // Add space before operator if needed
            // Debug check first
            if (std.debug.runtime_safety) {
                // Check that operator doesn't start or end with spaces
                if (op_str.len > 0) {
                    if (op_str[0] == ' ' or op_str[op_str.len - 1] == ' ') {
                        std.debug.panic("ERROR: Operator '{s}' starts or ends with whitespace!\n", .{op_str});
                    }
                }
            }

            try self.ensureSpace();
            try self.pushAll(op_str);
            try self.ensureSpace();

            // Check for comments after operator using token-based approach
            const rhs_node = self.getNode(rhs);
            _ = try self.flushCommentsBeforePosition(rhs_node.region.start);

            if (needs_rhs_parens) try self.push('(');
            try self.formatNode(rhs);
            if (needs_rhs_parens) try self.push(')');
        }
    }

    fn formatTypeAnnotation(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const binop_idx = node.payload.binop;
        const binop = self.ast.node_slices.binOp(binop_idx);

        const lhs = binop.lhs;
        const rhs = binop.rhs;

        // Format the left side (identifier or pattern)
        try self.formatNode(lhs);

        // Check if this is a complex type that should be multiline
        const rhs_node = self.getNode(rhs);
        const is_complex_type = switch (rhs_node.tag) {
            // Only make function types multiline if they have nested arrows (curried functions)
            .binop_thin_arrow => blk: {
                const arrow_binop = self.ast.node_slices.binOp(rhs_node.payload.binop);
                const arrow_lhs_node = self.getNode(arrow_binop.lhs);
                // Only multiline for nested arrows, not simple tuples
                break :blk arrow_lhs_node.tag == .binop_thin_arrow;
            },
            // Record types with many fields
            .record_literal => self.collectionIsMultiline(rhs),
            // Type applications - check if they have multiline markers
            .apply_uc => self.collectionIsMultiline(rhs),
            else => false,
        };

        if (is_complex_type) {
            try self.ensureSpace();
            try self.push(':');
            try self.ensureNewline();
            self.curr_indent_level += 1;
            try self.pushIndent();
            try self.formatTypeExpression(rhs);
            self.curr_indent_level -= 1;
        } else {
            try self.ensureSpace();
            try self.push(':');
            try self.ensureSpace();
            try self.formatTypeExpression(rhs);
        }
    }

    fn formatNominalTypeDefinition(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const binop_idx = node.payload.binop;
        const binop = self.ast.node_slices.binOp(binop_idx);

        const lhs = binop.lhs;
        const rhs = binop.rhs;

        // Format the left side (type name with parameters)
        try self.formatNode(lhs);

        try self.ensureSpace();
        try self.pushAll(":=");
        try self.ensureSpace();

        // Format the implementation type
        try self.formatTypeExpression(rhs);
    }

    fn formatTypeExpression(self: *Formatter, node_idx: Node.Idx) anyerror!void {
        const node = self.getNode(node_idx);

        switch (node.tag) {
            .binop_thin_arrow => {
                // Function type: format with proper precedence and spacing
                const binop = self.ast.node_slices.binOp(node.payload.binop);

                // Check if LHS needs parentheses (another function type)
                const lhs_node = self.getNode(binop.lhs);
                const needs_parens = lhs_node.tag == .binop_thin_arrow;

                if (needs_parens) try self.push('(');
                try self.formatTypeExpression(binop.lhs);
                if (needs_parens) try self.push(')');

                try self.pushAll(" -> ");
                try self.formatTypeExpression(binop.rhs);
            },
            .record_literal => {
                // Record type: { field1 : Type1, field2 : Type2 }
                try self.formatRecordType(node_idx);
            },
            .apply_uc => {
                // Type application: List a, Dict k v
                try self.formatTypeApplication(node_idx);
            },
            else => {
                // Default: format as normal node
                try self.formatNode(node_idx);
            },
        }
    }

    fn formatRecordType(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const multiline = self.collectionIsMultiline(node_idx);

        try self.push('{');

        if (multiline) {
            self.curr_indent_level += 1;
        }

        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        var first_field = true;

        while (it.next()) |field| {
            if (multiline) {
                if (!first_field) {
                    try self.push(',');
                }
                try self.ensureNewline();
                try self.pushIndent();
            } else {
                if (!first_field) {
                    try self.pushAll(", ");
                }
            }
            first_field = false;

            // Fields are binop_colon nodes
            const field_node = self.getNode(field);
            if (field_node.tag == .binop_colon) {
                const field_binop = self.ast.node_slices.binOp(field_node.payload.binop);
                try self.formatNode(field_binop.lhs);
                try self.ensureSpace();
                try self.push(':');
                try self.ensureSpace();
                try self.formatTypeExpression(field_binop.rhs);
            } else {
                try self.formatNode(field);
            }
        }

        if (multiline) {
            // Check for trailing comma
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
            }
            self.curr_indent_level -= 1;
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.push('}');
    }

    fn formatTypeApplication(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Type applications store the constructor and arguments
        var it = self.ast.node_slices.nodes(&node.payload.nodes);

        // First is the type constructor
        if (it.next()) |constructor| {
            try self.formatNode(constructor);
        }

        // Check if there's exactly one argument and it's a tuple_literal
        // If so, format the tuple's contents directly with parens
        var args_slice = self.ast.node_slices.nodes(&node.payload.nodes);
        _ = args_slice.next(); // Skip constructor
        const first_arg_opt = args_slice.next();
        const has_more = args_slice.next() != null;

        if (first_arg_opt) |first_arg| {
            if (!has_more) {
                // Only one argument - check if it's a tuple
                const arg_node = self.getNode(first_arg);
                if (arg_node.tag == .tuple_literal) {
                    // Format tuple contents directly with single parens
                    try self.push('(');
                    var tuple_it = self.ast.node_slices.nodes(&arg_node.payload.nodes);
                    var first_tuple_elem = true;
                    while (tuple_it.next()) |elem| {
                        if (!first_tuple_elem) {
                            try self.pushAll(", ");
                        }
                        first_tuple_elem = false;
                        try self.formatTypeExpression(elem);
                    }
                    try self.push(')');
                    return;
                }
            }
        }

        // Normal case: space-separated type arguments
        it = self.ast.node_slices.nodes(&node.payload.nodes);
        _ = it.next(); // Skip constructor again

        while (it.next()) |elem| {
            try self.ensureSpace();

            // Check if argument needs parentheses (e.g., function types as arguments)
            const elem_node = self.getNode(elem);
            const needs_parens = elem_node.tag == .binop_thin_arrow;

            if (needs_parens) try self.push('(');
            try self.formatTypeExpression(elem);
            if (needs_parens) try self.push(')');
        }
    }

    fn formatWhereClause(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const binop_idx = node.payload.binop;
        const binop = self.ast.node_slices.binOp(binop_idx);

        const lhs = binop.lhs;
        const rhs = binop.rhs;

        // Format the main type expression
        try self.formatTypeExpression(lhs);

        // Check if constraints should be multiline
        const rhs_node = self.getNode(rhs);
        const multiline = switch (rhs_node.tag) {
            // Multiple constraints (comma-separated)
            .record_literal, .block => self.collectionIsMultiline(rhs),
            // Chained where clauses
            .binop_where => true,
            else => false,
        };

        if (multiline) {
            try self.pushAll(" where");
            try self.ensureNewline();
            self.curr_indent_level += 1;
            try self.pushIndent();
            try self.formatWhereConstraints(rhs);
            self.curr_indent_level -= 1;
        } else {
            try self.pushAll(" where ");
            try self.formatWhereConstraints(rhs);
        }
    }

    fn formatWhereConstraints(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        switch (node.tag) {
            .record_literal, .block => {
                // Multiple constraints
                var it = self.ast.node_slices.nodes(&node.payload.nodes);
                var first = true;

                while (it.next()) |constraint| {
                    if (!first) {
                        try self.push(',');
                        try self.ensureNewline();
                        try self.pushIndent();
                    }
                    first = false;
                    try self.formatNode(constraint);
                }
            },
            .binop_where => {
                // Nested where clause - format recursively
                const binop = self.ast.node_slices.binOp(node.payload.binop);
                try self.formatWhereConstraints(binop.lhs);
                try self.push(',');
                try self.ensureNewline();
                try self.pushIndent();
                try self.formatWhereConstraints(binop.rhs);
            },
            else => {
                // Single constraint
                try self.formatNode(node_idx);
            },
        }
    }

    fn formatQualifiedIdent(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Qualified identifiers store their segments in nodes
        var it = self.ast.node_slices.nodes(&node.payload.nodes);

        var first = true;
        while (it.next()) |segment| {
            if (!first) {
                try self.pushAll(".");
            }
            first = false;
            try self.formatNode(segment);
        }
    }

    fn formatDotNum(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // .dot_num is for tuple accessors like .0, .1, etc.
        // Extract from source
        try self.push('.');

        const start = node.region.start.offset + 1; // Skip the dot
        var end = start;

        // Find the number after the dot
        while (end < self.source.len and std.ascii.isDigit(self.source[end])) {
            end += 1;
        }

        if (end > start) {
            try self.pushLiteralFromRegion(start, end);
        }
    }

    fn formatDotIdent(self: *Formatter, node_idx: Node.Idx) !void {
        // For .foo identifiers, just format as .foo
        const node = self.getNode(node_idx);

        // The node region includes the dot and the identifier
        try self.pushLiteralFromRegion(node.region.start.offset, node.region.end.offset);
    }

    fn formatUnaryNot(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // For unary not, format as !operand (not operator)
        try self.push('!');
        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        if (it.next()) |operand| {
            try self.formatNode(operand);
        }
    }

    fn formatUnaryNeg(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("-");
        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        if (it.next()) |operand| {
            try self.formatNode(operand);
        }
    }

    fn formatImport(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // New import structure: the import node contains a single child
        // which is either:
        // - A simple path node (e.g., pf.Task)
        // - A binop_as node (e.g., pf.Task as Task)
        // - A binop_exposing node (e.g., pf.Task exposing [Task, await])

        try self.pushAll("import ");

        // Get the single child node - imports use import_nodes field
        var it = self.ast.node_slices.nodes(&node.payload.import_nodes);
        if (it.next()) |child_idx| {
            const child = self.getNode(child_idx);

            switch (child.tag) {
                .binop_as => try self.formatImportWithAs(child_idx),
                .binop_exposing => try self.formatImportWithExposing(child_idx),
                else => {
                    // Simple path import
                    try self.formatNode(child_idx);
                },
            }
        }
    }

    fn formatImportWithAs(self: *Formatter, node_idx: Node.Idx) !void {
        // binop_as node: lhs is the path (or binop_exposing), rhs is the alias
        const binop = self.ast.binOp(node_idx);

        // Check if lhs is binop_exposing (import path exposing [...] as alias)
        const lhs_node = self.getNode(binop.lhs);
        if (lhs_node.tag == .binop_exposing) {
            // Format: path exposing [...] as alias
            try self.formatImportWithExposing(binop.lhs);
        } else {
            // Format: path as alias
            try self.formatNode(binop.lhs);
        }

        try self.pushAll(" as ");
        try self.formatNode(binop.rhs);
    }

    fn formatImportWithExposing(self: *Formatter, node_idx: Node.Idx) !void {
        // binop_exposing node: lhs is the path, rhs is the exposing list
        const binop = self.ast.binOp(node_idx);

        // Format the module path
        try self.formatNode(binop.lhs);

        try self.pushAll(" exposing ");

        // The rhs should be a list of exposed items
        const rhs_node = self.getNode(binop.rhs);
        if (rhs_node.tag == .list_literal) {
            // Format as list
            try self.formatNode(binop.rhs);
        } else {
            // Single exposed item or other structure
            try self.pushAll("[");
            try self.formatNode(binop.rhs);
            try self.pushAll("]");
        }
    }

    fn formatExpectStatement(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("expect ");

        // Expect statements store the condition expression in nodes
        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        if (it.next()) |condition| {
            try self.formatNode(condition);
        }
    }

    fn formatCrash(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("crash ");

        // Crash statements store the expression to crash with
        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        if (it.next()) |expr| {
            try self.formatNode(expr);
        }
    }

    fn formatPlatformExports(self: *Formatter, node_idx: Node.Idx) !void {
        // Platform exports are parsed as record_literal but should format as list
        const node = self.getNode(node_idx);

        // Check if this list should be multiline
        const multiline = self.collectionIsMultiline(node_idx);

        try self.pushAll("[");

        if (multiline) {
            self.curr_indent_level += 1;
        }

        // Get elements from record node
        var it = self.ast.node_slices.nodes(&node.payload.nodes);

        var first_elem = true;
        while (it.next()) |elem| {
            const elem_node = self.getNode(elem);

            if (multiline) {
                if (!first_elem) {
                    try self.push(',');
                    // Comments after comma will be handled when we flush before the next element
                }
                _ = try self.flushCommentsBeforePosition(elem_node.region.start);
                try self.ensureNewline();
                try self.pushIndent();
            } else {
                if (!first_elem) {
                    try self.pushAll(", ");
                }
            }
            first_elem = false;
            try self.formatNode(elem);

            if (multiline) {
                // Comments will be handled when we flush before the next token
            }
        }

        if (multiline) {
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
                // Comments will be handled when we flush before the next token
            }
            self.curr_indent_level -= 1;
            _ = try self.flushCommentsBeforePosition(Position{ .offset = @intCast(self.getNode(node_idx).region.end.offset) });
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.pushAll("]");
    }

    fn formatList(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Check if this list should be multiline
        const multiline = self.collectionIsMultiline(node_idx);

        try self.pushAll("[");

        if (multiline) {
            self.curr_indent_level += 1;
        }

        // List literals store elements in nodes
        var it = self.ast.node_slices.nodes(&node.payload.nodes);

        var first_elem = true;
        while (it.next()) |elem| {
            const elem_node = self.getNode(elem);

            if (multiline) {
                if (!first_elem) {
                    try self.push(',');
                    // Check for inline comments after the comma
                    // Comments after comma will be handled when we flush before the next element
                }
                // Flush any comments before this element
                _ = try self.flushCommentsBeforePosition(elem_node.region.start);
                try self.ensureNewline();
                try self.pushIndent();
            } else {
                if (!first_elem) {
                    try self.pushAll(", ");
                }
            }
            first_elem = false;
            try self.formatNode(elem);

            // After formatting the element, check for inline comments
            if (multiline) {
                // Comments will be handled when we flush before the next token
            }
        }

        if (multiline) {
            // Always add trailing comma in multiline mode
            try self.push(',');
            // Check for comment after trailing comma
            // Comments will be handled when we flush before the closing bracket
            self.curr_indent_level -= 1;
            // Flush any remaining comments before the closing bracket
            _ = try self.flushCommentsBeforePosition(Position{ .offset = @intCast(self.getNode(node_idx).region.end.offset) });
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.pushAll("]");
    }

    fn formatRecord(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Count fields and check for special cases
        var field_count: usize = 0;
        var is_single_ident_with_comma = false;
        var field_iter = self.ast.node_slices.nodes(&node.payload.nodes);
        while (field_iter.next()) |field| {
            field_count += 1;
            if (field_count == 1) {
                const field_node = self.getNode(field);
                // Check if it's just an identifier (not field:value)
                is_single_ident_with_comma = (field_node.tag == .lc or field_node.tag == .var_lc) and self.hasTrailingComma(node_idx);
            }
        }

        // Special case: empty record - format as {} with no space
        if (field_count == 0) {
            // Check if there are any comments inside the empty record
            const has_internal_comments = blk: {
                var scan = self.token_cursor;
                while (scan < self.tokens.len) {
                    const token = self.tokens[scan];
                    if (token.region.start.offset >= node.region.end.offset) break;
                    if (token.region.start.offset > node.region.start.offset and
                        (token.tag == .LineComment or token.tag == .DocComment))
                    {
                        break :blk true;
                    }
                    scan += 1;
                }
                break :blk false;
            };

            if (has_internal_comments) {
                // Format with comments preserved
                try self.push('{');
                self.curr_indent_level += 1;
                _ = try self.flushCommentsBeforePosition(Position{ .offset = @intCast(node.region.end.offset) });
                self.curr_indent_level -= 1;
                try self.ensureNewline();
                try self.pushIndent();
                try self.push('}');
            } else {
                // Simple empty record - no space
                try self.pushAll("{}");
            }
            return;
        }

        // Check if this record should be multiline
        const multiline = if (is_single_ident_with_comma and field_count == 1) false else self.collectionIsMultiline(node_idx);

        if (multiline) {
            try self.push('{');
            self.curr_indent_level += 1;
        } else {
            try self.pushAll("{ ");
        }

        // Record literals store fields in nodes
        var it = self.ast.node_slices.nodes(&node.payload.nodes);

        var first_field = true;
        while (it.next()) |field| {
            const field_node = self.getNode(field);

            // Skip malformed nodes
            if (field_node.tag == .malformed) {
                continue;
            }

            if (multiline) {
                if (!first_field) {
                    try self.push(',');
                    // Check for inline comments after the comma
                    // Comments after comma will be handled when we flush before the next element
                }
                // Flush any comments before this field
                _ = try self.flushCommentsBeforePosition(field_node.region.start);
                try self.ensureNewline();
                try self.pushIndent();
            } else {
                if (!first_field) {
                    try self.pushAll(", ");
                }
            }
            first_field = false;
            try self.formatNode(field);

            // After formatting the field, check for inline comments
            if (multiline) {
                // Comments will be handled when we flush before the next token
            }
        }

        if (multiline) {
            // Always add trailing comma in multiline mode
            try self.push(',');
            // Check for comment after trailing comma
            // Comments will be handled when we flush before the closing bracket
            self.curr_indent_level -= 1;
            // Flush any remaining comments before the closing brace
            _ = try self.flushCommentsBeforePosition(Position{ .offset = @intCast(self.getNode(node_idx).region.end.offset) });
            try self.ensureNewline();
            try self.pushIndent();
            try self.pushAll("}");
        } else {
            try self.ensureSpace();
            try self.push('}');
        }
    }

    fn formatTuple(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Check if this tuple should be multiline
        const multiline = self.collectionIsMultiline(node_idx);

        // Check if this tuple contains a where clause type - if so, don't add parens
        const nodes_idx = node.payload.nodes;
        var has_where_type = false;
        if (!nodes_idx.isNil()) {
            var check_it = self.ast.node_slices.nodes(&nodes_idx);
            while (check_it.next()) |child| {
                const child_node = self.getNode(child);
                // Check for type with where clause
                if (child_node.tag == .binop_thin_arrow) {
                    const arrow_binop = self.ast.node_slices.binOp(child_node.payload.binop);
                    const lhs_node = self.getNode(arrow_binop.lhs);
                    if (lhs_node.tag == .binop_where) {
                        has_where_type = true;
                        break;
                    }
                }
                // Also check for binop_pipe which indicates module constraints
                if (child_node.tag == .binop_pipe) {
                    const pipe_binop = self.ast.node_slices.binOp(child_node.payload.binop);
                    const pipe_lhs = self.getNode(pipe_binop.lhs);
                    if (pipe_lhs.tag == .apply_module) {
                        has_where_type = true;
                        break;
                    }
                }
            }
        }

        if (!has_where_type) {
            try self.push('(');
        }

        if (multiline) {
            self.curr_indent_level += 1;
        }

        var iter = self.ast.node_slices.nodes(&node.payload.nodes);
        var first = true;
        while (iter.next()) |child_idx| {
            const child_node = self.getNode(child_idx);

            // Skip malformed nodes
            if (child_node.tag == .malformed) {
                continue;
            }

            // Skip malformed nodes - they shouldn't be part of the formatted output
            // The parser adds malformed nodes when it encounters syntax errors
            if (child_node.tag == .malformed) {
                continue;
            }

            if (multiline) {
                if (!first) {
                    try self.push(',');
                    // Check for inline comments after the comma
                    // Comments after comma will be handled when we flush before the next element
                }
                // Flush any comments before this element
                _ = try self.flushCommentsBeforePosition(child_node.region.start);
                try self.ensureNewline();
                try self.pushIndent();
            } else {
                if (!first) {
                    try self.pushAll(", ");
                }
            }
            try self.formatNode(child_idx);

            // After formatting the element, check for inline comments
            if (multiline) {
                // Comments will be handled when we flush before the next token
            }
            first = false;
        }

        if (multiline) {
            // Always add trailing comma in multiline mode
            try self.push(',');
            self.curr_indent_level -= 1;
            try self.ensureNewline();
            try self.pushIndent();
        }

        if (!has_where_type) {
            try self.push(')');
        }
    }

    fn formatBlockContents(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const nodes_idx = node.payload.nodes;
        var iter = self.ast.node_slices.nodes(&nodes_idx);

        var first = true;
        while (iter.next()) |stmt_idx| {
            if (!first) {
                // Always add a newline between statements
                try self.ensureNewline();
            }

            try self.formatNode(stmt_idx);
            first = false;
        }
    }

    fn formatBlock(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const nodes_idx = node.payload.nodes;

        // Count statements
        var stmt_count: usize = 0;
        var count_iter = self.ast.node_slices.nodes(&nodes_idx);
        while (count_iter.next()) |_| {
            stmt_count += 1;
        }

        // Special case: empty block - format as {} with no space
        if (stmt_count == 0) {
            // Check if there are any comments inside the empty block
            const has_internal_comments = blk: {
                var scan = self.token_cursor;
                while (scan < self.tokens.len) {
                    const token = self.tokens[scan];
                    if (token.region.start.offset >= node.region.end.offset) break;
                    if (token.region.start.offset > node.region.start.offset and
                        (token.tag == .LineComment or token.tag == .DocComment))
                    {
                        break :blk true;
                    }
                    scan += 1;
                }
                break :blk false;
            };

            if (has_internal_comments) {
                // Format with comments preserved
                try self.push('{');
                self.curr_indent_level += 1;
                _ = try self.flushCommentsBeforePosition(Position{ .offset = @intCast(node.region.end.offset) });
                self.curr_indent_level -= 1;
                try self.ensureNewline();
                try self.pushIndent();
                try self.push('}');
            } else {
                // Simple empty block - no space
                try self.pushAll("{}");
            }
            return;
        }

        try self.push('{');
        self.curr_indent_level += 1;

        var iter = self.ast.node_slices.nodes(&nodes_idx);
        var first = true;
        while (iter.next()) |stmt_idx| {
            const stmt_node = self.getNode(stmt_idx);

            // Add newline before each statement
            if (!first) {
                try self.ensureNewline();
            } else {
                // For the first statement, we need a newline after the opening brace
                try self.newline();
            }

            // Flush any comments before this statement (they handle their own indentation and newlines)
            _ = try self.flushCommentsBeforePosition(stmt_node.region.start);

            // Add indentation for the statement
            // Debug: Check what we're about to indent
            if (std.debug.runtime_safety) {
                if (self.output.items.len > 0) {
                    // Count how many chars back to the last newline
                    var chars_since_newline: usize = 0;
                    var i = self.output.items.len;
                    while (i > 0) {
                        i -= 1;
                        if (self.output.items[i] == '\n') {
                            break;
                        }
                        chars_since_newline += 1;
                    }

                    // If we have exactly 4 spaces after the last newline, that's our bug!
                    if (chars_since_newline == 4) {
                        // Check if they're all spaces
                        var all_spaces = true;
                        for (self.output.items[self.output.items.len - 4 ..]) |c| {
                            if (c != ' ') {
                                all_spaces = false;
                                break;
                            }
                        }
                        if (all_spaces) {
                            std.debug.panic("ERROR: About to add indentation when we already have 4 spaces on the line!\nOutput: {s}\n", .{self.output.items});
                        }
                    }
                }
            }
            try self.pushIndent();

            // Format the statement itself
            try self.formatNodeWithoutCommentFlush(stmt_idx);

            // Check for blank lines after this node
            try self.flushBlankLinesAfterPosition(stmt_node.region.end);

            first = false;
        }

        self.curr_indent_level -= 1;
        try self.ensureNewline();
        try self.pushIndent();
        try self.push('}');
    }

    fn formatIfWithoutElse(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("if ");

        // if_without_else uses if_branches payload
        const nodes_idx = payloadToNodeSlicesIdx(node.payload.if_branches);
        var it = self.ast.node_slices.nodes(&nodes_idx);

        // Condition
        if (it.next()) |condition| {
            try self.formatNode(condition);
        }

        // Space between condition and body
        try self.ensureSpace();

        // Body (single statement/expression)
        if (it.next()) |body| {
            try self.formatNode(body);
        }
    }

    fn formatIfElse(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // The if_branches field contains the node slice index
        const nodes_idx = payloadToNodeSlicesIdx(node.payload.if_branches);

        // Get the nodes for this if-else
        var it = self.ast.node_slices.nodes(&nodes_idx);

        // Collect all nodes for analysis
        var condition: ?Node.Idx = null;
        var then_branch: ?Node.Idx = null;
        var else_branch: ?Node.Idx = null;

        if (it.next()) |c| condition = c;
        if (it.next()) |t| then_branch = t;
        if (it.next()) |e| else_branch = e;

        if (condition == null or then_branch == null) return; // Malformed

        // Check if any part is a block (blocks force multiline)
        var has_block = false;
        if (then_branch) |tb| {
            const tb_node = self.getNode(tb);
            if (tb_node.tag == .block) has_block = true;
        }
        if (else_branch) |eb| {
            const eb_node = self.getNode(eb);
            if (eb_node.tag == .block) has_block = true;
        }

        const multiline = has_block;

        try self.pushAll("if ");
        try self.formatNode(condition.?);

        if (multiline) {
            // Roc doesn't use 'then', just space before the block
            try self.ensureNewline();
            self.curr_indent_level += 1;
            try self.pushIndent();
            try self.formatNode(then_branch.?);
            self.curr_indent_level -= 1;

            if (else_branch) |eb| {
                try self.ensureNewline();
                try self.pushIndent();
                try self.pushAll("else");

                // Check for else-if chain
                const eb_node = self.getNode(eb);
                if (eb_node.tag == .if_else) {
                    try self.pushAll(" ");
                    // Format the if-else directly inline (it will handle its own formatting)
                    try self.formatIfElse(eb);
                } else if (eb_node.tag == .block) {
                    // Blocks handle their own formatting
                    try self.pushAll(" ");
                    try self.formatNode(eb);
                } else {
                    // Non-block else branch stays on same line as "else"
                    try self.pushAll(" ");
                    try self.formatNode(eb);
                }
            }
        } else {
            // Single-line formatting
            try self.pushAll(" ");
            try self.formatNode(then_branch.?);

            if (else_branch) |eb| {
                try self.pushAll(" else ");
                try self.formatNode(eb);
            }
        }
    }

    // Removed shouldIfElseBeMultiline - if-else is always multiline now

    fn formatMatch(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("match ");

        // Match nodes store scrutinee followed by pattern-body pairs in the nodes field
        const nodes_idx = node.payload.nodes;
        var it = self.ast.node_slices.nodes(&nodes_idx);

        // First node is the scrutinee
        if (it.next()) |scrutinee| {
            try self.formatNode(scrutinee);
        }

        // Match expressions in Roc use indentation, not curly braces
        // Format is:
        // match expr
        //     pattern1 => body1
        //     pattern2 => body2

        try self.ensureNewline();
        self.curr_indent_level += 1;

        // Format branches - each branch is stored as a binop_thick_arrow node
        // where the lhs is the pattern and rhs is the body
        while (it.next()) |branch_idx| {
            const branch_node = self.getNode(branch_idx);

            // Flush any comments before this branch
            _ = try self.flushCommentsBeforePosition(branch_node.region.start);

            try self.pushIndent();

            // Each branch should be a thick arrow (=>) binop
            if (branch_node.tag == .binop_thick_arrow) {
                const binop = self.ast.binOp(branch_idx);

                // Format pattern - handle pattern alternatives (|) specially
                const pattern_node = self.getNode(binop.lhs);
                if (pattern_node.tag == .binop_pipe) {
                    // This is a pattern alternative like Ok(x) | Err(x)
                    try self.formatPatternWithAlternatives(binop.lhs);
                } else {
                    try self.formatPattern(binop.lhs);
                }

                try self.ensureSpace();
                try self.pushAll("=>");
                try self.ensureSpace();

                // Format body
                const body_node = self.getNode(binop.rhs);

                // Complex bodies might need their own line and extra indentation
                const needs_block_indent = switch (body_node.tag) {
                    .block, .if_else, .match, .lambda => true,
                    else => false,
                };

                if (needs_block_indent) {
                    try self.ensureNewline();
                    self.curr_indent_level += 1;

                    // For blocks in match branches, format contents without braces
                    if (body_node.tag == .block) {
                        // Format block contents with proper indentation
                        const block_node = body_node;
                        const block_nodes_idx = block_node.payload.nodes;
                        var block_it = self.ast.node_slices.nodes(&block_nodes_idx);

                        var first_stmt = true;
                        while (block_it.next()) |stmt_idx| {
                            if (!first_stmt) {
                                try self.ensureNewline();
                            }
                            try self.pushIndent();
                            try self.formatNode(stmt_idx);
                            first_stmt = false;
                        }
                    } else {
                        try self.pushIndent();
                        try self.formatNode(binop.rhs);
                    }
                    self.curr_indent_level -= 1;
                } else {
                    try self.formatNode(binop.rhs);
                }

                // Check for inline comments after the body
                // Comments will be handled when we flush before the next token
            } else {
                // Shouldn't happen - branches should always be thick arrows
                // But handle gracefully by formatting the node as-is
                try self.formatNode(branch_idx);
            }

            // Add newline after each branch (except potentially the last)
            try self.ensureNewline();
        }

        self.curr_indent_level -= 1;
    }

    fn formatPatternWithAlternatives(self: *Formatter, node_idx: Node.Idx) !void {
        // Format pattern alternatives like Ok(x) | Err(x)
        const node = self.getNode(node_idx);

        if (node.tag != .binop_pipe) {
            // Not a pattern alternative, format as pattern
            try self.formatPattern(node_idx);
            return;
        }

        // Recursively format pattern alternatives
        const binop = self.ast.node_slices.binOp(node.payload.binop);

        // Check if left side is also a pipe (chained alternatives)
        const lhs_node = self.getNode(binop.lhs);
        if (lhs_node.tag == .binop_pipe) {
            try self.formatPatternWithAlternatives(binop.lhs);
        } else {
            try self.formatPattern(binop.lhs);
        }

        try self.pushAll(" | ");
        try self.formatPattern(binop.rhs);
    }

    fn formatPattern(self: *Formatter, node_idx: Node.Idx) anyerror!void {
        const node = self.getNode(node_idx);

        switch (node.tag) {
            // Literal patterns
            .num_literal_i32, .num_literal_big, .int_literal_i32, .int_literal_big, .frac_literal_small, .frac_literal_big, .str_literal_small, .str_literal_big => {
                try self.formatNode(node_idx);
            },

            // Identifier patterns
            .lc => {
                try self.formatIdent(node_idx);
            },
            .underscore => {
                try self.pushAll("_");
            },

            // Constructor patterns
            .apply_uc => {
                try self.formatConstructorPattern(node_idx);
            },
            .uc => {
                // Bare constructor
                try self.formatIdent(node_idx);
            },

            // Collection patterns
            .list_literal => {
                try self.formatListPattern(node_idx);
            },
            .record_literal => {
                try self.formatRecordPattern(node_idx);
            },
            .tuple_literal => {
                try self.formatTuplePattern(node_idx);
            },

            // As-patterns (e.g., pattern as name)
            .binop_as => {
                const binop = self.ast.node_slices.binOp(node.payload.binop);
                try self.formatPattern(binop.lhs);
                try self.pushAll(" as ");
                try self.formatIdent(binop.rhs);
            },

            // Guard patterns (pattern if condition)
            .if_without_else => {
                const nodes_idx_val = @as(u32, @bitCast(node.payload.if_branches));
                const nodes_idx = @as(collections.NodeSlices(Node.Idx).Idx, @enumFromInt(nodes_idx_val));
                var it = self.ast.node_slices.nodes(&nodes_idx);

                // First node is the pattern
                if (it.next()) |pattern| {
                    try self.formatPattern(pattern);
                }

                try self.pushAll(" if ");

                // Second node is the guard condition
                if (it.next()) |guard| {
                    try self.formatNode(guard);
                }
            },

            else => {
                // Default: format as normal node
                try self.formatNode(node_idx);
            },
        }
    }

    fn formatConstructorPattern(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        var it = self.ast.node_slices.nodes(&node.payload.nodes);

        // First node is the constructor
        if (it.next()) |constructor| {
            try self.formatNode(constructor);
        }

        // Check if there's exactly one argument and it's a tuple_literal
        // If so, format the tuple's contents directly without double parens
        var args_slice = self.ast.node_slices.nodes(&node.payload.nodes);
        _ = args_slice.next(); // Skip constructor
        const first_arg_opt = args_slice.next();
        const has_more = args_slice.next() != null;

        if (first_arg_opt) |first_arg| {
            if (!has_more) {
                // Only one argument - check if it's a tuple
                const arg_node = self.getNode(first_arg);
                if (arg_node.tag == .tuple_literal) {
                    // Format tuple contents directly with single parens
                    try self.push('(');
                    var tuple_it = self.ast.node_slices.nodes(&arg_node.payload.nodes);
                    var first_tuple_elem = true;
                    while (tuple_it.next()) |elem| {
                        if (!first_tuple_elem) {
                            try self.pushAll(", ");
                        }
                        first_tuple_elem = false;
                        try self.formatPattern(elem);
                    }
                    try self.push(')');
                    return;
                }
            }
        }

        // Normal case: format arguments with parentheses
        it = self.ast.node_slices.nodes(&node.payload.nodes);
        _ = it.next(); // Skip constructor again

        var has_args = false;
        var first_arg = true;
        while (it.next()) |arg| {
            if (first_arg) {
                try self.push('(');
                has_args = true;
            } else {
                try self.pushAll(", ");
            }
            first_arg = false;
            try self.formatPattern(arg);
        }

        if (has_args) {
            try self.push(')');
        }
    }

    fn formatListPattern(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.push('[');

        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        var first = true;
        var has_rest = false;

        while (it.next()) |elem| {
            if (!first) {
                try self.pushAll(", ");
            }
            first = false;

            const elem_node = self.getNode(elem);
            if (elem_node.tag == .double_dot_lc or elem_node.tag == .unary_double_dot) {
                // Rest pattern: ..rest or ..
                has_rest = true;
                if (elem_node.tag == .double_dot_lc) {
                    try self.pushAll("..");
                    try self.formatIdent(elem);
                } else {
                    try self.pushAll("..");
                }
            } else {
                try self.formatPattern(elem);
            }
        }

        try self.push(']');
    }

    fn formatRecordPattern(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const multiline = self.collectionIsMultiline(node_idx);

        try self.push('{');

        if (multiline) {
            self.curr_indent_level += 1;
        }

        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        var first = true;

        while (it.next()) |field| {
            if (multiline) {
                if (!first) {
                    try self.push(',');
                }
                try self.ensureNewline();
                try self.pushIndent();
            } else {
                if (!first) {
                    try self.pushAll(", ");
                }
            }
            first = false;

            const field_node = self.getNode(field);
            if (field_node.tag == .binop_colon) {
                // Field with explicit pattern: field: pattern
                const binop = self.ast.node_slices.binOp(field_node.payload.binop);
                try self.formatNode(binop.lhs);
                try self.pushAll(": ");
                try self.formatPattern(binop.rhs);
            } else if (field_node.tag == .double_dot_lc) {
                // Rest fields: ..rest
                try self.pushAll("..");
                try self.formatIdent(field);
            } else {
                // Punned field: just the field name
                try self.formatNode(field);
            }
        }

        if (multiline) {
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
            }
            self.curr_indent_level -= 1;
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.push('}');
    }

    fn formatTuplePattern(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.push('(');

        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        var first = true;

        while (it.next()) |elem| {
            if (!first) {
                try self.pushAll(", ");
            }
            first = false;
            try self.formatPattern(elem);
        }

        try self.push(')');
    }

    fn formatLambdaNoArgs(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("|| ");

        // Lambda with no args stores only the body node
        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        if (it.next()) |body| {
            try self.formatNode(body);
        }
    }

    fn formatLambda(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Check if lambda args should be multiline based on trailing comma
        const multiline = self.hasTrailingComma(node_idx);

        try self.pushAll("|");

        // Lambda nodes use body_then_args which stores body first, then args
        var it = self.ast.node_slices.nodes(&node.payload.body_then_args);

        // First node is the body - save it for later
        const body = it.next();

        if (multiline) {
            self.curr_indent_level += 1;
        }

        // Format arguments
        var first_arg = true;
        while (it.next()) |arg| {
            const arg_node = self.getNode(arg);

            // Special case: if the argument is a tuple_literal in a lambda,
            // format its elements as individual parameters without parentheses
            if (arg_node.tag == .tuple_literal and first_arg) {
                // This is the first (and likely only) argument and it's a tuple
                // Format its elements as individual lambda parameters
                var tuple_it = self.ast.node_slices.nodes(&arg_node.payload.nodes);
                var first_param = true;
                while (tuple_it.next()) |param| {
                    if (multiline) {
                        if (!first_param) {
                            try self.push(',');
                        }
                        try self.ensureNewline();
                        try self.pushIndent();
                    } else {
                        if (!first_param) {
                            try self.pushAll(", ");
                        }
                    }
                    first_param = false;
                    try self.formatNode(param);
                }
                first_arg = false;
                continue;
            }

            // Normal case: single parameter
            if (multiline) {
                if (!first_arg) {
                    try self.push(',');
                }
                try self.ensureNewline();
                try self.pushIndent();
            } else {
                if (!first_arg) {
                    try self.pushAll(", ");
                }
            }
            first_arg = false;
            try self.formatNode(arg);
        }

        if (multiline) {
            // Always add trailing comma in multiline mode
            try self.push(',');
            self.curr_indent_level -= 1;
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.pushAll("| ");

        // Format body
        if (body) |body_node| {
            try self.formatNode(body_node);
        }
    }

    fn formatApply(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Check if function args should be multiline based on trailing comma
        const multiline = self.hasTrailingComma(node_idx);

        // Format arguments
        const args_idx = node.payload.nodes;
        var iter = self.ast.node_slices.nodes(&args_idx);

        // First node is the function
        if (iter.next()) |func| {
            const func_node = self.getNode(func);
            // Lambdas need parentheses when applied
            const needs_parens = func_node.tag == .lambda;
            if (needs_parens) {
                try self.push('(');
            }
            try self.formatNode(func);
            if (needs_parens) {
                try self.push(')');
            }
        }

        // Format arguments in parentheses
        try self.push('(');

        if (multiline) {
            self.curr_indent_level += 1;
        }

        var first_arg = true;
        while (iter.next()) |arg| {
            if (multiline) {
                if (!first_arg) {
                    try self.push(',');
                }
                try self.ensureNewline();
                try self.pushIndent();
            } else {
                if (!first_arg) {
                    try self.pushAll(", ");
                }
            }
            first_arg = false;
            try self.formatNode(arg);
        }

        if (multiline) {
            // Preserve trailing comma
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
            }
            self.curr_indent_level -= 1;
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.push(')');
    }

    fn formatForLoop(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("for ");

        // For loops store: pattern, iterator, body nodes
        var it = self.ast.node_slices.nodes(&node.payload.nodes);

        // Pattern
        if (it.next()) |pattern| {
            try self.formatNode(pattern);
        }

        try self.pushAll(" in ");

        // Iterator
        if (it.next()) |iterator| {
            try self.formatNode(iterator);
        }

        try self.ensureSpace();
        try self.push('{');

        // Format the loop body
        try self.formatLoopBody(&it);
    }

    fn formatWhileLoop(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("while ");

        // While loops store: condition, body nodes
        var it = self.ast.node_slices.nodes(&node.payload.nodes);

        // Condition
        if (it.next()) |condition| {
            try self.formatNode(condition);
        }

        try self.ensureSpace();
        try self.push('{');

        // Format the loop body
        try self.formatLoopBody(&it);
    }

    fn formatLoopBody(self: *Formatter, it: *collections.NodeSlices(Node.Idx).Iterator) !void {
        // Collect body nodes to check if single-line is appropriate
        var body_nodes = std.ArrayList(Node.Idx).init(self.allocator);
        defer body_nodes.deinit();

        while (it.next()) |stmt| {
            try body_nodes.append(stmt);
        }

        // Check if body should be single-line
        const single_line = body_nodes.items.len == 1 and
            !self.nodeWillBeMultiline(body_nodes.items[0]);

        if (single_line) {
            // Single line: { expr }
            try self.ensureSpace();
            try self.formatNode(body_nodes.items[0]);
            try self.ensureSpace();
            try self.push('}');
        } else {
            // Multi-line body
            self.curr_indent_level += 1;
            for (body_nodes.items, 0..) |stmt, idx| {
                try self.ensureNewline();
                try self.pushIndent();
                try self.formatNode(stmt);

                // Add blank line between complex statements
                if (idx < body_nodes.items.len - 1) {
                    const stmt_node = self.getNode(stmt);
                    const is_complex = switch (stmt_node.tag) {
                        .binop_equals, .if_else, .match, .for_loop, .while_loop => true,
                        else => false,
                    };
                    if (is_complex) {
                        try self.ensureNewline();
                    }
                }
            }
            self.curr_indent_level -= 1;

            try self.ensureNewline();
            try self.pushIndent();
            try self.push('}');
        }
    }

    fn formatIdent(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Get the identifier text from the ident store
        const ident_idx = node.payload.ident;
        const ident_text = self.ident_store.getText(ident_idx);
        try self.pushAll(ident_text);
    }

    fn findMaxChildEnd(self: *const Formatter, node_idx: Node.Idx) usize {
        const node = self.getNode(node_idx);
        var max_end: usize = node.region.start.offset;

        // Get appropriate iterator based on node type
        switch (node.tag) {
            .import => {
                var it = self.ast.node_slices.nodes(&node.payload.import_nodes);
                while (it.next()) |child| {
                    const child_node = self.getNode(child);
                    const child_end = child_node.region.end.offset;
                    if (child_end > max_end) {
                        max_end = child_end;
                    }
                }
            },
            .match => {
                // Match nodes have complete regions now
                return node.region.end.offset;
            },
            .if_else, .if_without_else => {
                // If expressions have complete regions now
                return node.region.end.offset;
            },
            .lambda => {
                // Lambda uses body_then_args
                var it = self.ast.node_slices.nodes(&node.payload.body_then_args);
                while (it.next()) |child| {
                    const child_node = self.getNode(child);
                    const child_end = child_node.region.end.offset;
                    if (child_end > max_end) {
                        max_end = child_end;
                    }
                }
            },
            .binop_colon, .binop_equals, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_double_question, .binop_lt, .binop_gt, .binop_lte, .binop_gte, .binop_double_equals, .binop_not_equals, .binop_and, .binop_or, .binop_pipe, .binop_dot, .binop_colon_equals, .binop_as, .binop_where, .binop_thick_arrow, .binop_thin_arrow, .binop_platform => {
                // Binary operators use binop payload
                const binop = self.ast.node_slices.binOp(node.payload.binop);
                const lhs_node = self.getNode(binop.lhs);
                const rhs_node = self.getNode(binop.rhs);
                const lhs_end = lhs_node.region.end.offset;
                const rhs_end = rhs_node.region.end.offset;
                return @max(lhs_end, rhs_end);
            },
            else => {
                var it = self.ast.node_slices.nodes(&node.payload.nodes);
                while (it.next()) |child| {
                    const child_node = self.getNode(child);
                    const child_end = child_node.region.end.offset;
                    if (child_end > max_end) {
                        max_end = child_end;
                    }
                }
            },
        }

        // Check for closing paren if this is an application
        switch (node.tag) {
            .apply_lc, .apply_uc, .apply_anon, .apply_module => {
                if (max_end < self.source.len and self.source[max_end] == ')') {
                    return max_end + 1;
                }
            },
            else => {},
        }

        return max_end;
    }

    fn findIdentifierEnd(self: *const Formatter, start: usize) usize {
        var end = start;

        // Scan forward to find the end of the identifier
        while (end < self.source.len) {
            const c = self.source[end];
            // Identifiers can contain letters, numbers, and underscores
            if (std.ascii.isAlphanumeric(c) or c == '_') {
                end += 1;
            } else {
                break;
            }
        }

        return end;
    }

    fn formatNumberFromSource(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Find the number text from the source
        const start = node.region.start.offset;
        var end = start;

        // Scan forward to find the end of the number
        // Numbers can contain digits, underscores, dots (for fractions), hex/binary/octal letters
        while (end < self.source.len) {
            const c = self.source[end];
            // Include hex digits, binary/octal/hex prefixes, underscores, decimal points, and scientific notation
            if (std.ascii.isHex(c) or c == '_' or c == '.' or
                c == 'x' or c == 'X' or // Hex prefix
                c == 'b' or c == 'B' or // Binary prefix
                c == 'o' or c == 'O' or // Octal prefix
                c == 'e' or c == 'E' or // Scientific notation
                c == '+' or c == '-')
            { // Exponent sign
                end += 1;
            } else {
                break;
            }
        }

        // Now format it, collapsing multiple underscores and fixing uppercase base prefixes
        var i = start;
        var last_was_underscore = false;
        var is_at_base_prefix = false;

        // Check if this is a number with a base prefix (0x, 0X, 0b, 0B, 0o, 0O)
        if (end - start > 2 and self.source[start] == '0') {
            const second_char = self.source[start + 1];
            is_at_base_prefix = (second_char == 'X' or second_char == 'B' or second_char == 'O' or
                second_char == 'x' or second_char == 'b' or second_char == 'o');
        }

        while (i < end) : (i += 1) {
            const c = self.source[i];
            if (c == '_') {
                if (!last_was_underscore) {
                    try self.push('_');
                    last_was_underscore = true;
                }
            } else {
                // Convert uppercase base prefixes to lowercase
                if (is_at_base_prefix and i == start + 1) {
                    // This is the base prefix character
                    if (c == 'X') {
                        try self.push('x');
                    } else if (c == 'B') {
                        try self.push('b');
                    } else if (c == 'O') {
                        try self.push('o');
                    } else {
                        try self.push(c);
                    }
                    is_at_base_prefix = false; // Only convert the prefix, not hex digits
                } else {
                    try self.push(c);
                }
                last_was_underscore = false;
            }
        }
    }

    fn formatStringInterpolation(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // String interpolation is handled by preserving source with expression formatting
        // For now, extract from source and format embedded expressions
        const start = node.region.start.offset;
        const end = node.region.end.offset;

        if (start >= end or end > self.source.len) {
            try self.pushAll("\"\"");
            return;
        }

        // Start the interpolated string
        try self.push('"');

        // Process the interpolation nodes
        var iter = self.ast.node_slices.nodes(&node.payload.nodes);
        var is_string_part = true;

        while (iter.next()) |part_idx| {
            if (is_string_part) {
                // String parts - extract from source
                const part = self.getNode(part_idx);
                const part_start = part.region.start.offset;
                const part_node = self.getNode(part_idx);
                const part_end = part_node.region.end.offset;

                // Copy the string content (without quotes)
                if (part_start < part_end and part_end <= self.source.len) {
                    var content = self.source[part_start..part_end];

                    // Skip quotes if present
                    if (content.len > 0 and content[0] == '"') {
                        content = content[1..];
                    }
                    if (content.len > 0 and content[content.len - 1] == '"') {
                        content = content[0 .. content.len - 1];
                    }

                    try self.pushAll(content);
                }
            } else {
                // Expression part
                try self.pushAll("${");
                try self.formatNode(part_idx);
                try self.push('}');
            }

            is_string_part = !is_string_part;
        }

        try self.push('"');
    }

    fn formatStringFromSource(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Find the string in the source - it starts with a quote
        const start = node.region.start.offset;

        // Handle single-quoted strings (single characters)
        if (start < self.source.len and self.source[start] == '\'') {
            // Single-quoted string - output until closing quote
            var end = start + 1;
            while (end < self.source.len and self.source[end] != '\'') {
                end += 1;
            }
            if (end < self.source.len) {
                end += 1; // Include closing quote
            }
            const string_text = self.source[start..end];
            try self.pushAll(string_text);
            return;
        }

        var end = start + 1; // Skip opening quote

        // Find the closing quote, handling escapes
        while (end < self.source.len) {
            const c = self.source[end];
            if (c == '\\' and end + 1 < self.source.len) {
                end += 2; // Skip escape sequence
            } else if (c == '"') {
                end += 1; // Include closing quote
                break;
            } else {
                end += 1;
            }
        }

        // Output the string as-is from source
        try self.pushLiteralFromRegion(start, end);
    }

    fn nodeRegion(self: *const Formatter, node_idx: Node.Idx) base.Region {
        const node = self.getNode(node_idx);
        const end_offset = node.region.end.offset;

        // Calculate the end position based on the offset
        // We need to count lines and columns from the start
        var line = node.region.start.line;
        var column = node.region.start.column;
        var pos = node.region.start.offset;

        while (pos < end_offset and pos < self.source.len) {
            if (self.source[pos] == '\n') {
                line += 1;
                column = 0;
            } else {
                column += 1;
            }
            pos += 1;
        }

        return .{
            .start = node.region.start,
            .end = .{
                .offset = end_offset,
                .line = line,
                .column = column,
            },
        };
    }

    fn nodeWillBeMultiline(self: *const Formatter, node_idx: Node.Idx) bool {
        const node = self.getNode(node_idx);

        return switch (node.tag) {
            // Block expressions are always multiline
            .block => true,
            // Collections use the collectionIsMultiline check (trailing comma or too long)
            .list_literal, .record_literal, .tuple_literal => self.collectionIsMultiline(node_idx),
            // Everything else is single-line
            else => false,
        };
    }

    fn collectionIsMultiline(self: *const Formatter, node_idx: Node.Idx) bool {
        // Collections are multiline if:
        // 1. They have a trailing comma, OR
        // 2. They contain inline comments
        // We DO NOT enforce line lengths in this formatter by design

        // First check for trailing comma
        if (self.hasTrailingComma(node_idx)) {
            return true;
        }

        // Check if the collection contains any comments
        const node = self.getNode(node_idx);
        if (node.region.start.offset < node.region.end.offset and
            node.region.end.offset <= self.source.len)
        {
            const collection_text = self.source[node.region.start.offset..node.region.end.offset];
            // If there's a comment inside the collection, use multiline formatting
            if (std.mem.indexOf(u8, collection_text, "#") != null) {
                return true;
            }
        }

        return false;
    }

    fn hasTrailingComma(self: *const Formatter, node_idx: Node.Idx) bool {
        const node = self.getNode(node_idx);
        const start = node.region.start.offset;
        const end = node.region.end.offset;

        if (end <= start or end > self.source.len) {
            return false;
        }

        // Use binary search to find tokens within the node's region
        var left: usize = 0;
        var right: usize = self.tokens.len;

        // Binary search for first token with offset >= start
        while (left < right) {
            const mid = left + (right - left) / 2;
            if (self.tokens[mid].region.start.offset < start) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }

        // Find the last token before the closing delimiter
        var last_content_token: ?Token = null;
        var found_closing_delimiter = false;

        // Scan tokens within the node's region
        var cursor = left;
        while (cursor < self.tokens.len) : (cursor += 1) {
            const token = self.tokens[cursor];

            // Stop if we're past the node's region
            if (token.region.start.offset >= end) break;

            switch (token.tag) {
                // Skip comments and whitespace
                .LineComment, .DocComment => continue,

                // Check for closing delimiters
                .CloseSquare, .CloseCurly, .CloseRound => {
                    // If this delimiter is within our node's region, it's our closing delimiter
                    if (token.region.start.offset < end) {
                        found_closing_delimiter = true;
                        // Check if the previous non-comment token was a comma
                        if (last_content_token) |last| {
                            return last.tag == .Comma;
                        }
                        return false;
                    }
                },

                // Track the last non-comment, non-delimiter token
                .Comma, .LowerIdent, .UpperIdent, .String, .Int, .Float, .IntBase, .SingleQuote, .MultilineString, .OpenCurly, .OpenSquare, .OpenRound, .Dot, .OpUnaryMinus, .OpBinaryMinus, .OpBang => {
                    last_content_token = token;
                },

                else => {
                    // Track any other token as potential content
                    last_content_token = token;
                },
            }
        }

        // If we found a closing delimiter and the last token was a comma, we have a trailing comma
        if (found_closing_delimiter and last_content_token != null) {
            return last_content_token.?.tag == .Comma;
        }

        return false;
    }

    fn needsParens(self: *const Formatter, child_idx: Node.Idx, parent_tag: Node.Tag, is_left: bool) bool {
        const child = self.getNode(child_idx);

        // Only binary operators need parentheses based on precedence
        if (!std.mem.startsWith(u8, @tagName(child.tag), "binop_")) {
            return false;
        }

        // Special case: binop_pipe that's actually module/field access never needs parens
        if (child.tag == .binop_pipe) {
            const binop = self.ast.node_slices.binOp(child.payload.binop);
            const lhs_node = self.getNode(binop.lhs);
            const rhs_node = self.getNode(binop.rhs);

            // Check if this is module/field access (Bool.True, Stdout.line, etc.)
            if ((lhs_node.tag == .uc or lhs_node.tag == .lc) and
                (rhs_node.tag == .uc or rhs_node.tag == .lc or rhs_node.tag == .dot_lc or rhs_node.tag == .not_lc))
            {
                return false; // Module/field access never needs parentheses
            }
        }

        const parent_prec = self.getOperatorPrecedence(parent_tag);
        const child_prec = self.getOperatorPrecedence(child.tag);

        // Need parens if child has lower precedence than parent
        // Or if same precedence and associativity matters
        if (child_prec.left < parent_prec.left) {
            return true;
        }

        // For same precedence, check associativity
        if (child_prec.left == parent_prec.left) {
            // Right associative operators need parens on the left
            // Left associative operators need parens on the right
            if (is_left and parent_prec.right > parent_prec.left) {
                return true; // Right associative, left operand needs parens
            }
            if (!is_left and parent_prec.right == parent_prec.left + 1) {
                return true; // Left associative, right operand needs parens if same precedence
            }
        }

        return false;
    }

    fn getOperatorPrecedence(_: *const Formatter, tag: Node.Tag) Parser2.BindingPower {
        // Convert Node.Tag to Token.Tag, then use parser's getBindingPower
        const token_tag = nodeTagToTokenTag(tag);
        return Parser2.getBindingPower(token_tag);
    }

    fn nodeTagToTokenTag(tag: Node.Tag) Token.Tag {
        // Map AST node tags to their corresponding token tags
        return switch (tag) {
            .binop_pipe => .OpBar,
            .binop_or => .OpOr,
            .binop_and => .OpAnd,
            .binop_double_equals => .OpEquals,
            .binop_not_equals => .OpNotEquals,
            .binop_lt => .OpLessThan,
            .binop_lte => .OpLessThanOrEq,
            .binop_gt => .OpGreaterThan,
            .binop_gte => .OpGreaterThanOrEq,
            .binop_plus => .OpPlus,
            .binop_minus => .OpBinaryMinus,
            .binop_star => .OpStar,
            .binop_slash => .OpSlash,
            .binop_double_slash => .OpDoubleSlash,
            .binop_double_question => .OpDoubleQuestion,
            .binop_equals => .OpAssign,
            .binop_colon => .OpColon,
            .binop_thin_arrow => .OpArrow,
            .binop_thick_arrow => .OpFatArrow,
            .binop_as => .KwAs,
            .binop_where => .KwWhere,
            .binop_exposing => .KwExposing,
            .binop_platform => .KwPlatform,
            else => .EndOfFile, // Fallback for non-operator nodes
        };
    }

    fn hasBlankLine(_: *const Formatter, text: []const u8) bool {
        var newline_count: u32 = 0;
        for (text) |c| {
            if (c == '\n') {
                newline_count += 1;
                if (newline_count >= 2) return true;
            } else if (c != ' ' and c != '\t' and c != '\r') {
                newline_count = 0;
            }
        }
        return false;
    }

    fn push(self: *Formatter, c: u8) !void {
        // Debug check for consecutive spaces
        if (std.debug.runtime_safety) {
            if (c == ' ' and self.output.items.len > 0) {
                const prev = self.output.items[self.output.items.len - 1];
                if (prev == '\n') {
                    // Track consecutive spaces after newline
                    var consecutive_spaces: usize = 1;

                    // Look ahead in the current output to see if we're building up spaces
                    if (self.output.items.len >= 2) {
                        var i = self.output.items.len - 2;
                        while (i > 0) : (i -= 1) {
                            if (self.output.items[i] == ' ') {
                                consecutive_spaces += 1;
                            } else if (self.output.items[i] == '\n') {
                                // Found the newline, stop counting
                                break;
                            } else {
                                // Hit non-space, non-newline
                                break;
                            }
                        }
                    }

                    if (consecutive_spaces >= 4) {
                        std.debug.panic("ERROR: push() is adding space #{} after a newline! Output so far:\n{s}\n", .{ consecutive_spaces, self.output.items });
                    }
                } else if (prev == ' ') {
                    // Check if we're adding consecutive spaces
                    var space_count: usize = 1;
                    var i = self.output.items.len - 1;
                    while (i > 0 and self.output.items[i] == ' ') : (i -= 1) {
                        space_count += 1;
                    }
                    if (space_count >= 2) {
                        std.debug.panic("ERROR: push() is adding consecutive space #{} at output position {}! Preceding context: {s}\n", .{ space_count + 1, self.output.items.len, self.output.items[std.math.sub(usize, self.output.items.len, 50) catch 0 ..] });
                    }
                }
            }
        }

        self.has_newline = c == '\n';
        try self.output.append(c);
    }

    fn pushAll(self: *Formatter, str: []const u8) !void {
        if (str.len == 0) return;

        // Debug checks in debug builds only
        if (std.debug.runtime_safety) {
            // Check for consecutive spaces
            var prev_was_space = false;
            for (str, 0..) |c, i| {
                if (c == ' ') {
                    if (prev_was_space) {
                        std.debug.panic("ERROR: Multiple consecutive spaces in pushAll at position {}: {s}\n", .{ i, str });
                    }
                    prev_was_space = true;
                } else {
                    prev_was_space = false;
                }

                // Check tab placement - tabs should only come after newline or another tab
                if (c == '\t') {
                    if (i > 0) {
                        const prev = str[i - 1];
                        if (prev != '\n' and prev != '\t') {
                            std.debug.panic("ERROR: Tab not after newline or tab at position {} in: {s}\n", .{ i, str });
                        }
                    } else if (self.output.items.len > 0) {
                        // Check what's at the end of output
                        const last = self.output.items[self.output.items.len - 1];
                        if (last != '\n' and last != '\t') {
                            std.debug.panic("ERROR: Tab not after newline or tab (from output) in: {s}\n", .{str});
                        }
                    }
                }
            }

            // Check if output ends in space and str starts with space
            if (self.output.items.len > 0 and str.len > 0) {
                if (self.output.items[self.output.items.len - 1] == ' ' and str[0] == ' ') {
                    std.debug.panic("ERROR: Output ends in space and pushAll starts with space: {s}\n", .{str});
                }
            }
        }
        self.has_newline = str[str.len - 1] == '\n';
        try self.output.appendSlice(str);
    }

    /// Push literal content from source (e.g., string literals, number literals, malformed content)
    /// This doesn't check for formatting violations since we're preserving source content
    fn pushLiteralFromRegion(self: *Formatter, start: usize, end: usize) !void {
        if (std.debug.runtime_safety) {
            std.debug.assert(end > start); // Should not be empty
            std.debug.assert(end <= self.source.len); // Should be within bounds
        }

        const str = self.source[start..end];

        self.has_newline = str[str.len - 1] == '\n';
        try self.output.appendSlice(str);
    }

    fn ensureNewline(self: *Formatter) !void {
        if (!self.has_newline) {
            try self.newline();
        }
    }

    fn ensureSpace(self: *Formatter) !void {
        // Don't add a space if we're at the beginning of a line (after newline)
        if (self.output.items.len > 0 and self.output.items[self.output.items.len - 1] == '\n') {
            // We're at the start of a line, don't add a space
            return;
        }

        // Only add a space if the output doesn't already end with one
        if (self.output.items.len == 0 or self.output.items[self.output.items.len - 1] != ' ') {
            try self.push(' ');
        }
    }

    fn ensureBlankLine(self: *Formatter) !void {
        // Ensure we have at least one newline
        try self.ensureNewline();
        // Then add another one for the blank line
        try self.newline();
    }

    fn newline(self: *Formatter) !void {
        try self.push('\n');
    }

    fn pushIndent(self: *Formatter) !void {
        if (self.curr_indent_level == 0) {
            return;
        }

        // Use TABS for indentation, not spaces!
        // OPTIMIZATION: Pre-allocated string of 32 tabs for common cases.
        // This avoids dynamic allocation for indentation levels 1-32 (covers 99.9% of real code).
        // For levels 1-32: Single pushAll() with a slice = one memcpy operation
        // For levels >32: Still handled correctly with the fallback loop below
        // This is NOT a limitation - just an optimization for the common case!
        const tabs = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t";
        const indent_level = @min(self.curr_indent_level, tabs.len);
        try self.pushAll(tabs[0..indent_level]);

        // Handle very deep indentation (unlikely but possible)
        // This ensures there's no maximum indentation limit
        if (self.curr_indent_level > tabs.len) {
            for (tabs.len..self.curr_indent_level) |_| {
                try self.push('\t');
            }
        }
    }

    /// Check if there are comment tokens between two positions
    fn hasCommentBetween(self: *const Formatter, start: usize, end: usize) bool {
        // Use binary search to find the first token that could be in range
        var left: usize = 0;
        var right: usize = self.tokens.len;

        // Binary search for first token with offset >= start
        while (left < right) {
            const mid = left + (right - left) / 2;
            if (self.tokens[mid].region.start.offset < start) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }

        // Now scan from this position until we reach end
        var cursor = left;
        while (cursor < self.tokens.len) : (cursor += 1) {
            const token = self.tokens[cursor];
            if (token.region.start.offset >= end) break;
            switch (token.tag) {
                .LineComment, .DocComment => return true,
                else => {},
            }
        }
        return false;
    }

    fn flushTrailingComments(self: *Formatter) !bool {
        // Flush all remaining comment tokens
        var found_any = false;
        while (self.token_cursor < self.tokens.len) {
            const token = self.tokens[self.token_cursor];
            switch (token.tag) {
                .LineComment, .DocComment => {
                    try self.pushIndent();
                    try self.pushLiteralFromRegion(token.region.start.offset, token.region.end.offset);
                    try self.newline();
                    found_any = true;
                },
                else => {},
            }
            self.token_cursor += 1;
        }
        return found_any;
    }
};

/// Formats AST2 with source code
pub fn formatAst(
    allocator: std.mem.Allocator,
    ast: *const AST2,
    source: []const u8,
    ident_store: *const Ident.Store,
    root_node: ?Node.Idx,
) ![]u8 {
    // Just tokenize - we don't need to parse since we already have an AST
    var env = try CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var messages: [256]tokenize_iter.Diagnostic = undefined;

    // Tokenize without parsing
    var tokens = std.ArrayList(tokenize_iter.Token).init(allocator);
    defer tokens.deinit();

    var token_iter = try tokenize_iter.TokenIterator.init(&env, allocator, source, &messages, &byte_slices);
    defer token_iter.deinit(allocator);

    // Collect all tokens
    while (try token_iter.next(allocator)) |token| {
        try tokens.append(token);
        if (token.tag == .EndOfFile) break;
    }

    var formatter = try Formatter.init(allocator, ast, source, ident_store, tokens.items);
    defer formatter.deinit();

    try formatter.format(root_node);

    // Ensure the output ends with exactly one newline
    // Remove any trailing blank lines, then ensure we have exactly one newline
    while (formatter.output.items.len > 1 and
        formatter.output.items[formatter.output.items.len - 1] == '\n' and
        formatter.output.items[formatter.output.items.len - 2] == '\n')
    {
        _ = formatter.output.pop();
    }

    // Ensure we end with a newline if we don't already
    if (formatter.output.items.len == 0 or formatter.output.items[formatter.output.items.len - 1] != '\n') {
        try formatter.output.append('\n');
    }

    // Transfer ownership to caller by moving the items
    const result = try formatter.output.toOwnedSlice();

    // Debug: Check for lines with exactly 4 spaces
    if (std.debug.runtime_safety) {
        var i: usize = 0;
        var line_start: usize = 0;
        var line_num: usize = 1;

        while (i < result.len) : (i += 1) {
            if (result[i] == '\n') {
                const line = result[line_start..i];
                if (line.len == 4) {
                    var all_spaces = true;
                    for (line) |c| {
                        if (c != ' ') {
                            all_spaces = false;
                            break;
                        }
                    }
                    if (all_spaces) {
                        std.debug.panic("ERROR: Formatter is returning line {} with exactly 4 spaces!\n", .{line_num});
                    }
                }
                line_num += 1;
                line_start = i + 1;
            }
        }
    }

    // Debug assertion: Ensure file ends with exactly one newline
    if (std.debug.runtime_safety) {
        if (result.len == 0) {
            std.debug.panic("ERROR: Formatter returned empty output!\n", .{});
        }

        // Check that file ends with exactly one newline
        if (result[result.len - 1] != '\n') {
            std.debug.panic("ERROR: Formatted file does not end with a newline! Last char: '{c}'\n", .{result[result.len - 1]});
        }

        // Check that there's no blank line at the end (no double newline)
        if (result.len > 1 and result[result.len - 2] == '\n') {
            std.debug.panic("ERROR: Formatted file ends with a blank line (double newline)!\n", .{});
        }
    }

    return result;
}
