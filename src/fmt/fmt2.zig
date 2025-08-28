//! Production-ready formatter for AST2
//! Matches the behavior of the original fmt.zig but works with AST2/Parser2

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const collections = @import("collections");
const can = @import("can");

const AST2 = parse.AST2;
const Node = AST2.Node;
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
    tokens: std.ArrayList(Token),
    token_regions: std.ArrayList(base.Region), // Region for each token
    output: std.ArrayList(u8),

    // Formatting state - matches original formatter
    curr_indent_level: u32 = 0,
    has_newline: bool = true, // Starts true since beginning of file is considered a newline
    last_formatted_pos: usize = 0, // Current position in source being formatted
    last_comment_end: usize = 0, // Track last comment position to avoid duplicates

    fn init(allocator: std.mem.Allocator, ast: *const AST2, source: []const u8, ident_store: *const Ident.Store) !Formatter {
        var formatter = Formatter{
            .allocator = allocator,
            .ast = ast,
            .source = source,
            .ident_store = ident_store,
            .tokens = std.ArrayList(Token).init(allocator),
            .token_regions = std.ArrayList(base.Region).init(allocator),
            .output = std.ArrayList(u8).init(allocator),
            .last_formatted_pos = 0,
            .last_comment_end = 0,
        };

        // Tokenize once at the beginning
        try formatter.tokenizeSource();

        return formatter;
    }

    fn deinit(self: *Formatter) void {
        self.tokens.deinit();
        self.token_regions.deinit();
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
        if (self.token_regions.items.len > 0) {
            _ = try self.flushTrailingComments();
        }
    }

    /// Helper to get a node from an index - eliminates massive boilerplate
    inline fn getNode(self: *const Formatter, idx: Node.Idx) Node {
        // Node.Idx is i32, SafeMultiList.Idx expects u32
        const idx_val = @intFromEnum(idx);
        const u32_idx = @as(u32, @intCast(idx_val));
        const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(u32_idx));
        return self.ast.nodes.get(multi_list_idx);
    }

    fn tokenizeSource(self: *Formatter) !void {
        var env = try CommonEnv.init(self.allocator, self.source);
        defer env.deinit(self.allocator);

        var byte_slices = ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(self.allocator);

        var messages: [128]tokenize_iter.Diagnostic = undefined;
        var iter = try tokenize_iter.TokenIterator.init(&env, self.allocator, self.source, &messages, &byte_slices);
        while (try iter.next(self.allocator)) |token| {
            try self.tokens.append(token);

            // Store the region for this token
            try self.token_regions.append(token.region);
        }
    }

    /// Find the token index that contains or is after the given position
    fn findTokenAtPosition(self: *const Formatter, pos: Position) ?usize {
        for (self.token_regions.items, 0..) |region, i| {
            if (region.start.offset >= pos.offset) {
                return i;
            }
        }
        return null;
    }

    /// Find the token index that is before the given position
    fn findTokenBeforePosition(self: *const Formatter, pos: Position) ?usize {
        var result: ?usize = null;
        for (self.token_regions.items, 0..) |region, i| {
            if (region.end.offset <= pos.offset) {
                result = i;
            } else {
                break;
            }
        }
        return result;
    }

    fn formatHeader(self: *Formatter, header: AST2.Header) !void {
        switch (header) {
            .module => |m| {
                _ = try self.flushCommentsBeforeToken(m.region);
                try self.pushAll("module");

                // Check for inline comment after module keyword
                _ = try self.flushCommentsAfterToken(m.region);

                // Format the exposed list
                try self.formatExposedList(m.exposes);
            },
            .app => |a| {
                _ = try self.flushCommentsBeforeToken(a.region);
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

                    needs_multiline = package_count > 1;
                }

                if (needs_multiline) {
                    // Multiline format with proper comment preservation

                    // Check for comments after "app" keyword
                    // The comment would be between the "app" token and the opening brace token
                    // Since we don't have the exact position of the brace, check after current position
                    const app_end_pos = Position{ .offset = @intCast(a.region.offset + 3) }; // "app" is 3 chars
                    const comment_after_app = try self.flushCommentsAfterToken(app_end_pos);

                    if (comment_after_app) {
                        // If there was a comment, it's already on its own line
                    }

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
                            _ = try self.flushCommentsBeforeToken(pkg_node.region.start);

                            try self.pushIndent();

                            if (pkg_node.tag == .binop_colon) {
                                const binop = self.ast.node_slices.binOp(pkg_node.payload.binop);
                                try self.formatNode(binop.lhs);
                                try self.pushAll(": ");

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
                    try self.pushAll(" { ");
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
                                try self.pushAll(": ");
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
                    try self.pushAll(" }");
                }
            },
            .package => |p| {
                _ = try self.flushCommentsBeforeToken(p.region);
                try self.pushAll("package");

                // Check for comment after package keyword
                _ = try self.flushCommentsAfterToken(p.region);

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
                _ = try self.flushCommentsBeforeToken(p.region);
                try self.pushAll("platform");

                // Check for comment after platform keyword
                _ = try self.flushCommentsAfterToken(p.region);

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
                try self.pushAll(" exposes ");
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
                _ = try self.flushCommentsBeforeToken(h.region);
                try self.pushAll("hosted");

                // Format exposes
                try self.formatExposedList(h.exposes);
            },
            .interface => |i| {
                _ = try self.flushCommentsBeforeToken(i.region);
                try self.pushAll("interface");

                // Format exposes
                try self.formatExposedList(i.exposes);
            },
            .malformed => |m| {
                _ = try self.flushCommentsBeforeToken(m.region);
                // Malformed headers just get flushed as-is
            },
        }
    }

    fn formatExposedList(self: *Formatter, exposes_idx: collections.NodeSlices(Node.Idx).Idx) !void {
        try self.pushAll(" [");

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

        // First pass: count and check for complexity
        while (iter.next()) |node_idx| {
            item_count += 1;
            const node = self.getNode(node_idx);

            // Check if there are comments before this node
            if (self.hasCommentBetween(self.last_formatted_pos, node.region.start.offset)) {
                has_comments = true;
            }
        }

        // Use multiline if: more than 3 items, or has comments
        const multiline = item_count > 3 or has_comments;

        if (multiline) {
            self.curr_indent_level += 1;
            try self.ensureNewline();
        }

        // Reset iterator for second pass
        iter = self.ast.node_slices.nodes(&exposes_idx);
        var first = true;

        while (iter.next()) |node_idx| {
            if (multiline) {
                // Check for comments before this item
                const node = self.getNode(node_idx);
                _ = try self.flushCommentsBeforeToken(node.region.start);
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
        _ = try self.flushCommentsBeforeToken(node.region.start);

        switch (node.tag) {
            // Binary operations
            .binop_equals => try self.formatBinOp(node_idx, " = "),
            .binop_double_equals => try self.formatBinOp(node_idx, " == "),
            .binop_not_equals => try self.formatBinOp(node_idx, " != "),
            .binop_colon => try self.formatTypeAnnotation(node_idx),
            .binop_colon_equals => try self.formatNominalTypeDefinition(node_idx),
            .binop_dot => try self.formatBinOp(node_idx, "."),
            .binop_plus => try self.formatBinOp(node_idx, " + "),
            .binop_minus => try self.formatBinOp(node_idx, " - "),
            .binop_star => try self.formatBinOp(node_idx, " * "),
            .binop_slash => try self.formatBinOp(node_idx, " / "),
            .binop_double_slash => try self.formatBinOp(node_idx, " // "),
            .binop_double_question => try self.formatBinOp(node_idx, " ?? "),
            .binop_gt => try self.formatBinOp(node_idx, " > "),
            .binop_gte => try self.formatBinOp(node_idx, " >= "),
            .binop_lt => try self.formatBinOp(node_idx, " < "),
            .binop_lte => try self.formatBinOp(node_idx, " <= "),
            .binop_thick_arrow => try self.formatBinOp(node_idx, " => "),
            .binop_thin_arrow => try self.formatBinOp(node_idx, " -> "),
            .binop_and => try self.formatBinOp(node_idx, " && "),
            .binop_or => try self.formatBinOp(node_idx, " || "),
            .binop_as => try self.formatBinOp(node_idx, " as "),
            .binop_exposing => try self.formatBinOp(node_idx, " exposing "),
            .binop_where => try self.formatWhereClause(node_idx),
            .binop_platform => try self.formatBinOp(node_idx, " platform "),
            .binop_pipe => {
                // Check if this is a module field access pattern
                const node = self.getNode(node_idx);
                const binop = self.ast.node_slices.binOp(node.payload.binop);
                const lhs_node = self.getNode(binop.lhs);
                const rhs_node = self.getNode(binop.rhs);

                // If LHS is apply_module and RHS is dot_lc, format as module field access
                if (lhs_node.tag == .apply_module and rhs_node.tag == .dot_lc) {
                    try self.formatNode(binop.lhs);
                    try self.formatNode(binop.rhs);
                } else {
                    // Regular pipe for pattern alternatives
                    try self.formatBinOp(node_idx, " | ");
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
                        try self.push(' ');
                    }

                    try self.formatNode(expr_idx);
                }
            },

            // Malformed nodes - preserve source bytes with diagnostic-aware recovery
            .malformed => |_| {
                // For malformed nodes, we preserve the original source text
                const start = node.region.start.offset;
                const end = node.region.end.offset;

                if (start < end and end <= self.source.len) {
                    const source_text = self.source[start..end];
                    if (source_text.len > 0) {
                        try self.pushAll(source_text);
                    }
                    self.last_formatted_pos = end;
                }
            },
        }

        // Update position to past this node
        const node_end = node.region.end.offset;
        if (node_end > self.last_formatted_pos) {
            // Check for inline comments after this node
            _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(node.region.end.offset) });

            // If no inline comment was found, update position
            if (self.last_formatted_pos < node_end) {
                self.last_formatted_pos = node_end;
            }
        }
    }

    fn formatBinOp(self: *Formatter, node_idx: Node.Idx, op_str: []const u8) !void {
        const node = self.getNode(node_idx);
        const binop_idx = node.payload.binop;
        const binop = self.ast.node_slices.binOp(binop_idx);

        const lhs = binop.lhs;
        const rhs = binop.rhs;

        // Special case for binop_platform: format as "string" platform [provides]
        if (node.tag == .binop_platform) {
            try self.formatNode(lhs); // The platform string
            try self.pushAll(" platform ");

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
                // This is actually field access or a qualified name, not a pipe operator
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

            // Check for comments between LHS and equals using token-based approach
            const lhs_node = self.getNode(lhs);
            const has_comment_after_lhs = try self.flushCommentsAfterToken(lhs_node.region.end);

            try self.pushAll(" = ");

            // Check for comments after equals before RHS using token-based approach
            const rhs_node = self.getNode(rhs);
            const comment_after_equals = try self.flushCommentsBeforeToken(rhs_node.region.start);

            if (has_comment_after_lhs or comment_after_equals) {
                // Force multiline if we found any comments
                multiline = true;
            }

            if (multiline) {
                self.curr_indent_level += 1;
                if (!comment_after_equals) {
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
            } else {
                if (needs_rhs_parens) try self.push('(');
                try self.formatNode(rhs);
                if (needs_rhs_parens) try self.push(')');
            }
        } else {
            // Normal binary operator
            if (needs_lhs_parens) try self.push('(');
            try self.formatNode(lhs);
            if (needs_lhs_parens) try self.push(')');

            // Check for comments before operator using token-based approach
            const lhs_node = self.getNode(lhs);
            _ = try self.flushCommentsAfterToken(lhs_node.region.end);

            try self.pushAll(op_str);

            // Check for comments after operator using token-based approach
            const rhs_node = self.getNode(rhs);
            _ = try self.flushCommentsBeforeToken(rhs_node.region.start);

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
            // Function types with multiple arrows
            .binop_thin_arrow => blk: {
                const arrow_binop = self.ast.node_slices.binOp(rhs_node.payload.binop);
                const arrow_lhs_node = self.getNode(arrow_binop.lhs);
                // Check if the function has multiple parameters (tuple) or nested arrows
                break :blk arrow_lhs_node.tag == .tuple_literal or arrow_lhs_node.tag == .binop_thin_arrow;
            },
            // Record types with many fields
            .record_literal => self.collectionIsMultiline(rhs),
            // Type applications with multiple parameters
            .apply_uc => blk: {
                var count: usize = 0;
                var it = self.ast.node_slices.nodes(&rhs_node.payload.nodes);
                while (it.next()) |_| count += 1;
                break :blk count > 2;
            },
            else => false,
        };

        if (is_complex_type) {
            try self.pushAll(" :");
            try self.ensureNewline();
            self.curr_indent_level += 1;
            try self.pushIndent();
            try self.formatTypeExpression(rhs);
            self.curr_indent_level -= 1;
        } else {
            try self.pushAll(" : ");
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

        try self.pushAll(" := ");

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
                try self.pushAll(" : ");
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
            try self.push(' ');

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
            try self.pushAll(self.source[start..end]);
        }
    }

    fn formatDotIdent(self: *Formatter, node_idx: Node.Idx) !void {
        // For .foo identifiers, just format as .foo
        const node = self.getNode(node_idx);

        // The node region includes the dot and the identifier
        const text = self.source[node.region.start.offset..node.region.end.offset];
        try self.pushAll(text);
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
                    _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
                }
                _ = try self.flushCommentsBeforeToken(elem_node.region.start);
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
                _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
            }
        }

        if (multiline) {
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
                _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
            }
            self.curr_indent_level -= 1;
            _ = try self.flushCommentsBeforeToken(Position{ .offset = @intCast(self.getNode(node_idx).region.end.offset) });
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
                    _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
                }
                // Flush any comments before this element
                _ = try self.flushCommentsBeforeToken(elem_node.region.start);
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
                _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
            }
        }

        if (multiline) {
            // Always add trailing comma in multiline mode
            try self.push(',');
            // Check for comment after trailing comma
            _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
            self.curr_indent_level -= 1;
            // Flush any remaining comments before the closing bracket
            _ = try self.flushCommentsBeforeToken(Position{ .offset = @intCast(self.getNode(node_idx).region.end.offset) });
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.pushAll("]");
    }

    fn formatRecord(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Special case: single-field record with just identifier and comma e.g. { foo, }
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
                    _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
                }
                // Flush any comments before this field
                _ = try self.flushCommentsBeforeToken(field_node.region.start);
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
                _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
            }
        }

        if (multiline) {
            // Always add trailing comma in multiline mode
            try self.push(',');
            // Check for comment after trailing comma
            _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
            self.curr_indent_level -= 1;
            // Flush any remaining comments before the closing brace
            _ = try self.flushCommentsBeforeToken(Position{ .offset = @intCast(self.getNode(node_idx).region.end.offset) });
            try self.ensureNewline();
            try self.pushIndent();
            try self.pushAll("}");
        } else {
            try self.pushAll(" }");
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
                    _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
                }
                // Flush any comments before this element
                _ = try self.flushCommentsBeforeToken(child_node.region.start);
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
                _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
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

        var prev_stmt_end: usize = self.last_formatted_pos;
        var first = true;

        while (iter.next()) |stmt_idx| {
            const stmt_node = self.getNode(stmt_idx);
            const stmt_start = stmt_node.region.start.offset;

            if (!first) {
                // Check source between statements for blank lines
                if (prev_stmt_end < stmt_start and stmt_start < self.source.len) {
                    const between = self.source[prev_stmt_end..stmt_start];

                    // Count newlines to determine if there was a blank line
                    var newline_count: u32 = 0;
                    for (between) |c| {
                        if (c == '\n') {
                            newline_count += 1;
                        }
                    }

                    // If there were 2+ newlines (blank line), preserve one blank line
                    // Otherwise just one newline
                    try self.ensureNewline();
                    if (newline_count >= 2) {
                        try self.ensureNewline(); // Add blank line
                    }
                }
            }

            try self.formatNode(stmt_idx);

            // Check for trailing comment on the same line as this statement
            _ = try self.flushCommentsAfterToken(stmt_node.region.end);

            prev_stmt_end = stmt_node.region.end.offset;
            first = false;
        }
    }

    fn formatBlock(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const nodes_idx = node.payload.nodes;

        try self.push('{');
        self.curr_indent_level += 1;

        var iter = self.ast.node_slices.nodes(&nodes_idx);
        var first = true;
        while (iter.next()) |stmt_idx| {
            try self.ensureNewline();
            try self.pushIndent();
            try self.formatNode(stmt_idx);
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
        const if_branches_u32 = node.payload.if_branches;
        const nodes_idx = @as(collections.NodeSlices(Node.Idx).Idx, @enumFromInt(if_branches_u32));
        var it = self.ast.node_slices.nodes(&nodes_idx);

        // Condition
        if (it.next()) |condition| {
            try self.formatNode(condition);
        }

        try self.pushAll(" ");

        // Body (single statement/expression)
        if (it.next()) |body| {
            try self.formatNode(body);
        }
    }

    fn formatIfElse(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // The if_branches field contains the node slice index encoded as a u32
        const if_branches_u32 = node.payload.if_branches;
        const nodes_idx = @as(collections.NodeSlices(Node.Idx).Idx, @enumFromInt(if_branches_u32));

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
            _ = try self.flushCommentsBeforeToken(branch_node.region.start);

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

                try self.pushAll(" => ");

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
                    try self.pushIndent();
                    try self.formatNode(binop.rhs);
                    self.curr_indent_level -= 1;
                } else {
                    try self.formatNode(binop.rhs);
                }

                // Check for inline comments after the body
                _ = try self.flushCommentsAfterToken(Position{ .offset = @intCast(self.last_formatted_pos) });
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
                const nodes_idx = node.payload.nodes;
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

        try self.pushAll(" {");

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

        try self.pushAll(" {");

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
            try self.push(' ');
            try self.formatNode(body_nodes.items[0]);
            try self.pushAll(" }");
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

    fn findNumberEnd(self: *const Formatter, start: usize) usize {
        var end = start;
        while (end < self.source.len) {
            const c = self.source[end];
            if (std.ascii.isHex(c) or c == '_' or c == '.' or c == 'x' or c == 'e' or c == 'E' or c == '+' or c == '-') {
                end += 1;
            } else {
                break;
            }
        }
        return end;
    }

    fn findIfEnd(self: *const Formatter, node_idx: Node.Idx) usize {
        const node = self.getNode(node_idx);
        var pos = node.region.start.offset;

        // Scan for the end of the if expression
        // Look for the last relevant token after "else"
        var found_else = false;
        var depth: usize = 0;
        var in_string = false;
        var escape_next = false;
        var last_relevant_pos = pos;

        while (pos < self.source.len) {
            const ch = self.source[pos];

            if (in_string) {
                if (escape_next) {
                    escape_next = false;
                } else if (ch == '\\') {
                    escape_next = true;
                } else if (ch == '"') {
                    in_string = false;
                    last_relevant_pos = pos + 1;
                }
            } else {
                switch (ch) {
                    '"' => {
                        in_string = true;
                    },
                    '{' => {
                        depth += 1;
                        last_relevant_pos = pos + 1;
                    },
                    '}' => {
                        if (depth > 0) {
                            depth -= 1;
                            last_relevant_pos = pos + 1;
                        } else if (found_else) {
                            // End of if expression
                            return pos + 1;
                        }
                    },
                    '(' => {
                        depth += 1;
                    },
                    ')' => {
                        if (depth > 0) {
                            depth -= 1;
                            last_relevant_pos = pos + 1;
                        } else if (found_else) {
                            // End of if expression
                            return pos + 1;
                        }
                    },
                    '\n', ' ', '\t', '\r' => {},
                    else => {
                        // Check for "else" keyword
                        if (!found_else and pos + 4 <= self.source.len) {
                            if (std.mem.eql(u8, self.source[pos .. pos + 4], "else")) {
                                found_else = true;
                                pos += 3; // Will be incremented again at loop end
                            }
                        }
                        if (depth == 0 and found_else) {
                            last_relevant_pos = pos + 1;
                        }
                    },
                }
            }
            pos += 1;

            // If we've found else and we're at depth 0, check for end of expression
            if (found_else and depth == 0 and pos < self.source.len) {
                const next_ch = self.source[pos];
                if (next_ch == '\n' or next_ch == ',' or next_ch == '}' or next_ch == ')' or next_ch == ']') {
                    return last_relevant_pos;
                }
            }
        }

        return last_relevant_pos;
    }

    fn findMatchEnd(self: *const Formatter, node_idx: Node.Idx) usize {
        const node = self.getNode(node_idx);
        var pos = node.region.start.offset;

        // Scan for the closing brace of the match expression
        var brace_depth: usize = 0;
        var in_string = false;
        var escape_next = false;

        while (pos < self.source.len) {
            const ch = self.source[pos];

            if (in_string) {
                if (escape_next) {
                    escape_next = false;
                } else if (ch == '\\') {
                    escape_next = true;
                } else if (ch == '"') {
                    in_string = false;
                }
            } else {
                switch (ch) {
                    '"' => in_string = true,
                    '{' => brace_depth += 1,
                    '}' => {
                        if (brace_depth == 0) {
                            // Found the closing brace
                            return pos + 1;
                        }
                        brace_depth -= 1;
                    },
                    else => {},
                }
            }
            pos += 1;
        }

        return pos;
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
        try self.pushAll(self.source[start..end]);
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

        // Look for a comma before the closing delimiter
        const start = node.region.start.offset;
        const end_estimate = node.region.end.offset;

        if (end_estimate <= start or end_estimate > self.source.len) {
            // For tuples, if the region is incomplete, try to find the closing paren
            if (node.tag == .tuple_literal and start < self.source.len) {
                // Scan forward from the end of the known region to find the closing paren
                var scan_pos = if (end_estimate <= self.source.len) end_estimate else start;
                var depth: i32 = 0;

                while (scan_pos < self.source.len) {
                    const c = self.source[scan_pos];
                    if (c == '(') {
                        depth += 1;
                    } else if (c == ')') {
                        if (depth == 0) {
                            // Found the closing paren
                            // Now check backward for a comma
                            var i = scan_pos;
                            while (i > start) {
                                i -= 1;
                                const prev_c = self.source[i];
                                if (prev_c == ',') {
                                    return true;
                                } else if (prev_c != ' ' and prev_c != '\t' and prev_c != '\n' and prev_c != '\r') {
                                    // Found non-whitespace that's not a comma
                                    return false;
                                }
                            }
                            return false;
                        }
                        depth -= 1;
                    }
                    scan_pos += 1;
                }
            }
            return false;
        }

        // Single backward scan to find both delimiter and comma
        var i = end_estimate;
        var found_delimiter = false;

        // For tuples, if we haven't found a closing paren at the end, look forward a bit
        if (node.tag == .tuple_literal and i < self.source.len and self.source[i - 1] != ')') {
            // Scan forward to find the real end
            while (i < self.source.len and i < end_estimate + 10) {
                if (self.source[i] == ')') {
                    i += 1; // Include the paren
                    break;
                }
                i += 1;
            }
        }

        while (i > start) {
            i -= 1;
            const c = self.source[i];

            if (!found_delimiter) {
                if (c == ']' or c == '}' or c == ')') {
                    found_delimiter = true;
                }
            } else {
                // We've found the delimiter, now looking for comma
                if (c == ',') {
                    return true;
                } else if (c != ' ' and c != '\t' and c != '\n' and c != '\r') {
                    // Found non-whitespace that's not a comma
                    return false;
                }
            }
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

    fn getOperatorPrecedence(_: *const Formatter, tag: Node.Tag) struct { left: u8, right: u8 } {
        // Based on getBindingPower from Parser2.zig
        return switch (tag) {
            .binop_pipe => .{ .left = 10, .right = 11 },
            .binop_or => .{ .left = 20, .right = 21 },
            .binop_and => .{ .left = 30, .right = 31 },
            .binop_double_equals, .binop_not_equals => .{ .left = 40, .right = 41 },
            .binop_lt, .binop_lte, .binop_gt, .binop_gte => .{ .left = 50, .right = 51 },
            .binop_plus, .binop_minus => .{ .left = 60, .right = 61 },
            .binop_star, .binop_slash, .binop_double_slash => .{ .left = 70, .right = 71 },
            .binop_double_question => .{ .left = 80, .right = 81 },
            .binop_equals => .{ .left = 5, .right = 6 },
            .binop_colon => .{ .left = 3, .right = 4 },
            .binop_thin_arrow => .{ .left = 2, .right = 3 },
            .binop_thick_arrow => .{ .left = 1, .right = 2 },
            .binop_as => .{ .left = 3, .right = 4 },
            .binop_exposing => .{ .left = 3, .right = 4 },
            .binop_where => .{ .left = 1, .right = 2 },
            .binop_platform => .{ .left = 3, .right = 4 },
            else => .{ .left = 0, .right = 0 },
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

    /// Flush comments and blank lines before a position
    /// Returns true if there was at least one newline
    fn findLineEnd(self: *const Formatter, start: usize) usize {
        var i = start;
        while (i < self.source.len and self.source[i] != '\n') {
            i += 1;
        }
        return i;
    }

    fn findMalformedEndSmart(self: *const Formatter, node_idx: Node.Idx, diagnostic: anytype) usize {
        // The best way to find where a malformed node ends is to look at the next node in the AST
        const node = self.getNode(node_idx);
        const start = node.region.start.offset;

        // Try to find the next sibling node in the AST by checking all nodes
        // and finding the one with the smallest start position that's after this node
        var next_node_start: ?usize = null;

        // Iterate through all nodes to find the next one after this malformed node
        // This is more reliable than string matching
        var i: u32 = 0;
        const node_count = self.ast.nodes.len();
        while (i < node_count) : (i += 1) {
            const idx = @as(Node.Idx, @enumFromInt(i));
            if (idx == node_idx) continue; // Skip self

            const other_node = self.getNode(idx);
            const other_start = other_node.region.start.offset;

            // Is this node after ours and before our current best candidate?
            if (other_start > start) {
                if (next_node_start == null or other_start < next_node_start.?) {
                    next_node_start = other_start;
                }
            }
        }

        // If we found a next node, use its start as our end
        if (next_node_start) |next_start| {
            // Back up to remove any whitespace between nodes
            var end = next_start;
            while (end > start and (self.source[end - 1] == ' ' or
                self.source[end - 1] == '\t' or
                self.source[end - 1] == '\n' or
                self.source[end - 1] == '\r'))
            {
                end -= 1;
            }
            return end;
        }

        // No next node found, try using diagnostic information
        const has_missing_close = @hasField(@TypeOf(diagnostic), "missing_close");
        const has_unexpected_token = @hasField(@TypeOf(diagnostic), "unexpected_token");

        if (has_missing_close) {
            // Look for the next matching closing delimiter
            var depth: i32 = 1;
            var pos = start + 1;
            while (pos < self.source.len) {
                switch (self.source[pos]) {
                    '{', '[', '(' => depth += 1,
                    '}', ']', ')' => {
                        depth -= 1;
                        if (depth == 0) return pos + 1;
                    },
                    else => {},
                }
                pos += 1;
            }
        }

        if (has_unexpected_token) {
            // Stop at the unexpected token
            return self.findNextTokenBoundary(start);
        }

        // Fall back to finding the end based on heuristics
        return self.findMalformedEndHeuristic(node_idx, diagnostic);
    }

    fn trimMalformedTextSmart(self: *const Formatter, text: []const u8, diagnostic: anytype) []const u8 {
        // Apply smarter trimming based on diagnostic
        const has_incomplete = @hasField(@TypeOf(diagnostic), "incomplete");
        const has_trailing_comma = @hasField(@TypeOf(diagnostic), "trailing_comma");

        var result = text;

        // Always trim trailing whitespace
        while (result.len > 0) {
            const last = result[result.len - 1];
            if (last == ' ' or last == '\t' or last == '\n' or last == '\r') {
                result = result[0 .. result.len - 1];
            } else {
                break;
            }
        }

        if (has_incomplete or has_trailing_comma) {
            // Remove trailing comma
            if (result.len > 0 and result[result.len - 1] == ',') {
                result = result[0 .. result.len - 1];
            }
        }

        // Fall back to original trimming
        return self.trimMalformedText(result, diagnostic);
    }

    fn findNextTokenBoundary(self: *const Formatter, start: usize) usize {
        var pos = start;
        var in_identifier = false;

        while (pos < self.source.len) {
            const c = self.source[pos];

            const is_id_char = (c >= 'a' and c <= 'z') or
                (c >= 'A' and c <= 'Z') or
                (c >= '0' and c <= '9') or
                c == '_';

            if (in_identifier) {
                if (!is_id_char) return pos;
            } else {
                if (is_id_char) {
                    in_identifier = true;
                } else if (c != ' ' and c != '\t') {
                    // Found a non-whitespace, non-identifier character
                    return pos;
                }
            }
            pos += 1;
        }

        return self.source.len;
    }

    fn findMalformedEndHeuristic(self: *const Formatter, node_idx: Node.Idx, diagnostic: anytype) usize {
        const node = self.getNode(node_idx);
        const start = node.region.start.offset;
        var pos = start;
        var depth: i32 = 0;
        var in_string = false;
        var escape_next = false;

        // Since diagnostic is a generic tag, we'll use a simple heuristic
        // Look for natural boundaries like unmatched delimiters or new statements
        _ = diagnostic; // Unused for now, but could be used for smarter recovery

        while (pos < self.source.len) {
            const c = self.source[pos];

            // Handle string literals to avoid false positives
            if (escape_next) {
                escape_next = false;
                pos += 1;
                continue;
            }

            if (c == '\\' and in_string) {
                escape_next = true;
                pos += 1;
                continue;
            }

            if (c == '"') {
                in_string = !in_string;
                pos += 1;
                continue;
            }

            if (!in_string) {
                switch (c) {
                    '{', '[', '(' => depth += 1,
                    '}', ']', ')' => {
                        // If depth goes negative, we found an unmatched closer
                        depth -= 1;
                        if (depth < 0) {
                            // Unmatched closer - stop here
                            return pos;
                        }
                    },
                    '\n' => {
                        if (depth == 0) {
                            // Check if next line starts a new statement
                            var next_pos = pos + 1;
                            while (next_pos < self.source.len and
                                (self.source[next_pos] == ' ' or self.source[next_pos] == '\t'))
                            {
                                next_pos += 1;
                            }

                            if (next_pos < self.source.len) {
                                const next_char = self.source[next_pos];
                                // Check for statement keywords or identifiers at column 0-4 indentation
                                const indent = next_pos - (pos + 1);
                                if (indent <= 4 and
                                    ((next_char >= 'a' and next_char <= 'z') or
                                        (next_char >= 'A' and next_char <= 'Z') or
                                        next_char == '_'))
                                {
                                    // At low indentation with an identifier - likely a new statement
                                    // Stop here rather than continuing into the next statement
                                    return pos;
                                }
                            }
                        }
                    },
                    else => {},
                }
            }

            pos += 1;
        }

        return self.source.len;
    }

    fn trimMalformedText(_: *const Formatter, text: []const u8, diagnostic: anytype) []const u8 {
        var result = text;

        // Trim trailing whitespace
        while (result.len > 0) {
            const last = result[result.len - 1];
            if (last == ' ' or last == '\t' or last == '\n' or last == '\r') {
                result = result[0 .. result.len - 1];
            } else {
                break;
            }
        }

        // Also trim trailing commas if present (often incomplete)
        if (result.len > 0 and result[result.len - 1] == ',') {
            result = result[0 .. result.len - 1];
        }

        _ = diagnostic; // Not used currently but could enable smarter trimming

        return result;
    }

    /// Flush comments and blank lines, preserving formatting
    /// Returns true if there was at least one newline
    fn flushComments(self: *Formatter, between_text: []const u8) !bool {
        var found_comment = false;
        var newline_count: usize = 0;
        var i: usize = 0;

        while (i < between_text.len) {
            if (between_text[i] == '#') {
                // Found a comment - extract it
                const comment_start = i + 1; // Skip the #
                var comment_end = comment_start;
                while (comment_end < between_text.len and between_text[comment_end] != '\n' and between_text[comment_end] != '\r') {
                    comment_end += 1;
                }

                // Output the comment with proper indentation
                try self.pushIndent();
                try self.push('#');
                const comment_text = between_text[comment_start..comment_end];
                if (comment_text.len > 0 and comment_text[0] != ' ') {
                    try self.push(' ');
                }
                try self.pushAll(comment_text);
                found_comment = true;

                try self.ensureNewline();
                newline_count = 1; // Reset to avoid excessive newlines
                i = comment_end;
                if (i < between_text.len and (between_text[i] == '\n' or between_text[i] == '\r')) {
                    i += 1; // Skip any additional newlines
                }
            } else if (between_text[i] == '\n') {
                // Count newlines but don't output them immediately
                newline_count += 1;
                i += 1;
            } else if (between_text[i] == '\r') {
                // Handle carriage returns
                i += 1;
                if (i < between_text.len and between_text[i] == '\n') {
                    i += 1;
                }
                newline_count += 1;
            } else if (between_text[i] == ' ' or between_text[i] == '\t') {
                // Skip whitespace
                i += 1;
            } else {
                // Unexpected character between tokens
                i += 1;
            }
        }

        // Return true if there was at least one newline
        return newline_count > 0;
    }

    fn push(self: *Formatter, c: u8) !void {
        self.has_newline = c == '\n';
        try self.output.append(c);
    }

    fn pushAll(self: *Formatter, str: []const u8) !void {
        if (str.len == 0) return;
        self.has_newline = str[str.len - 1] == '\n';
        try self.output.appendSlice(str);
    }

    fn ensureNewline(self: *Formatter) !void {
        if (!self.has_newline) {
            try self.newline();
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
        // Batch the indentation to avoid multiple allocations
        const tabs = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t";
        const indent_level = @min(self.curr_indent_level, tabs.len);
        try self.pushAll(tabs[0..indent_level]);

        // Handle very deep indentation (unlikely but possible)
        if (self.curr_indent_level > tabs.len) {
            for (tabs.len..self.curr_indent_level) |_| {
                try self.push('\t');
            }
        }
    }

    /// Flush comments before a given position using token positions
    fn flushCommentsBeforeToken(self: *Formatter, pos: Position) !bool {
        // Find the token at or after this position
        const token_idx = self.findTokenAtPosition(pos) orelse return false;
        if (token_idx == 0) return false;

        // Get the region between previous token and this position
        const prev_token_end = self.token_regions.items[token_idx - 1].end.offset;
        const end_offset = pos.offset;

        if (end_offset <= prev_token_end) return false;

        return self.flushCommentsInRange(self.source[prev_token_end..end_offset]);
    }

    /// Flush comments after a given position using token positions
    fn flushCommentsAfterToken(self: *Formatter, pos: Position) !bool {
        // Find the token before this position
        const token_idx = self.findTokenBeforePosition(pos) orelse return false;
        if (token_idx >= self.token_regions.items.len - 1) return false;

        // Get the region between this position and next token
        const start_offset = pos.offset;
        const next_token_start = self.token_regions.items[token_idx + 1].start.offset;

        if (next_token_start <= start_offset) return false;

        return self.flushCommentsInRange(self.source[start_offset..next_token_start]);
    }

    /// Core comment flushing logic for a range of text
    fn flushCommentsInRange(self: *Formatter, text: []const u8) !bool {
        // Calculate absolute offset of this text in source
        const text_start = @intFromPtr(text.ptr) - @intFromPtr(self.source.ptr);

        // Skip if we've already processed this region
        if (text_start < self.last_comment_end) {
            // Find where we need to start from
            const skip_bytes = self.last_comment_end - text_start;
            if (skip_bytes >= text.len) return false;
            return self.flushCommentsInRangeInternal(text[skip_bytes..]);
        }

        return self.flushCommentsInRangeInternal(text);
    }

    fn flushCommentsInRangeInternal(self: *Formatter, text: []const u8) !bool {
        var found_comment = false;
        var newline_count: usize = 0;
        var i: usize = 0;

        while (i < text.len) {
            if (text[i] == '#') {
                // If we've seen 2+ newlines (blank line) before this comment,
                // it's a standalone comment for the next statement - don't attach it
                if (newline_count >= 2 and !found_comment) {
                    // This is a standalone comment, leave it for the next statement
                    break;
                }

                // Found a comment to attach
                const comment_start = i + 1; // Skip the #
                var comment_end = comment_start;
                while (comment_end < text.len and text[comment_end] != '\n' and text[comment_end] != '\r') {
                    comment_end += 1;
                }

                // Output appropriate spacing before comment
                if (found_comment or newline_count > 0) {
                    try self.pushIndent();
                } else if (!self.has_newline) {
                    // Check if we already have a trailing space
                    const output_len = self.output.items.len;
                    const has_trailing_space = output_len > 0 and self.output.items[output_len - 1] == ' ';
                    if (!has_trailing_space) {
                        try self.pushAll(" ");
                    }
                }

                // Output the comment
                try self.push('#');
                if (comment_end > comment_start) {
                    try self.output.appendSlice(text[comment_start..comment_end]);
                }

                // Move to after the comment
                i = comment_end;

                // Handle newline after comment
                if (i < text.len and (text[i] == '\n' or text[i] == '\r')) {
                    try self.ensureNewline();
                    if (text[i] == '\r') i += 1;
                    if (i < text.len and text[i] == '\n') i += 1;
                    newline_count = 1;
                } else {
                    i += 1;
                }

                // Update position to track we've processed this comment
                const text_start = @intFromPtr(text.ptr) - @intFromPtr(self.source.ptr);
                self.last_comment_end = text_start + i;

                found_comment = true;
            } else if (text[i] == '\n') {
                newline_count += 1;
                // Preserve blank lines
                if (newline_count > 1 and found_comment) {
                    try self.ensureBlankLine();
                }
                i += 1;
            } else if (text[i] == '\r') {
                i += 1;
            } else if (text[i] == ' ' or text[i] == '\t') {
                i += 1;
            } else {
                // Non-whitespace, non-comment - stop scanning
                break;
            }
        }

        return found_comment or newline_count > 0;
    }

    /// Flush trailing comments at end of file using token positions
    fn hasCommentBetween(self: *Formatter, start: usize, end: usize) bool {
        if (start >= end or end > self.source.len) return false;
        const text = self.source[start..end];
        return std.mem.indexOf(u8, text, "#") != null;
    }

    fn flushTrailingComments(self: *Formatter) !bool {
        if (self.token_regions.items.len == 0) return false;

        const last_token_end = self.token_regions.items[self.token_regions.items.len - 1].end.offset;
        if (last_token_end >= self.source.len) return false;

        const remaining = self.source[last_token_end..];
        return self.flushCommentsInRange(remaining);
    }
};

/// Formats AST2 with source code
pub fn formatAst(allocator: std.mem.Allocator, ast: *const AST2, source: []const u8, ident_store: *const Ident.Store, root_node: ?Node.Idx) ![]u8 {
    var formatter = try Formatter.init(allocator, ast, source, ident_store);
    defer formatter.deinit();

    try formatter.format(root_node);

    // Transfer ownership to caller
    const output = try allocator.dupe(u8, formatter.output.items);

    return output;
}

// Keep the old interface for compatibility
