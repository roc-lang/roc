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

/// Format options for customizing formatter behavior
pub const FormatOptions = struct {
    /// Whether to preserve trailing commas
    preserve_trailing_commas: bool = true,
};

/// Main formatter struct for AST2
const Formatter = struct {
    allocator: std.mem.Allocator,
    ast: *const AST2,
    source: []const u8,
    ident_store: *const Ident.Store,
    tokens: std.ArrayList(Token),
    output: std.ArrayList(u8),
    options: FormatOptions,

    // Formatting state - matches original formatter
    curr_indent: u32 = 0,
    has_newline: bool = true, // Starts true since beginning of file is considered a newline
    last_formatted_pos: usize = 0, // Track position in source for comment preservation

    fn init(allocator: std.mem.Allocator, ast: *const AST2, source: []const u8, ident_store: *const Ident.Store, options: FormatOptions) !Formatter {
        return Formatter{
            .allocator = allocator,
            .ast = ast,
            .source = source,
            .ident_store = ident_store,
            .tokens = std.ArrayList(Token).init(allocator),
            .output = std.ArrayList(u8).init(allocator),
            .options = options,
        };
    }

    fn deinit(self: *Formatter) void {
        self.tokens.deinit();
        self.output.deinit();
    }

    fn format(self: *Formatter, root_node: ?Node.Idx) !void {
        // First, collect all tokens for comment preservation
        try self.collectTokens();

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

        // Flush any trailing comments
        if (self.tokens.items.len > 0) {
            _ = try self.flushCommentsToEnd();
        }
    }

    /// Helper to get a node from an index - eliminates massive boilerplate
    fn getNode(self: *const Formatter, idx: Node.Idx) Node {
        // Node.Idx is i32, SafeMultiList.Idx expects u32
        const idx_val = @intFromEnum(idx);
        const u32_idx = @as(u32, @intCast(idx_val));
        const multi_list_idx = @as(collections.SafeMultiList(Node).Idx, @enumFromInt(u32_idx));
        return self.ast.nodes.get(multi_list_idx);
    }

    fn collectTokens(self: *Formatter) !void {
        // Only collect tokens if there are comments in the source
        // This optimization avoids tokenizing the entire file when not needed
        const has_comments = std.mem.indexOf(u8, self.source, "#") != null;
        if (!has_comments) {
            // No comments, no need to tokenize
            return;
        }

        var env = try CommonEnv.init(self.allocator, self.source);
        defer env.deinit(self.allocator);

        var byte_slices = ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(self.allocator);

        var messages: [128]tokenize_iter.Diagnostic = undefined;
        var iter = try tokenize_iter.TokenIterator.init(&env, self.allocator, self.source, &messages, &byte_slices);
        while (try iter.next(self.allocator)) |token| {
            try self.tokens.append(token);
        }
    }

    fn formatHeader(self: *Formatter, header: AST2.Header) !void {
        switch (header) {
            .module => |m| {
                _ = try self.flushCommentsBefore(m.region);
                try self.pushAll("module");

                // Update position to after "module" keyword
                // Find where we are in the source
                if (std.mem.indexOf(u8, self.source[self.last_formatted_pos..], "module")) |module_offset| {
                    self.last_formatted_pos += module_offset + 6; // "module" is 6 chars
                }

                // Check for inline comment after module keyword
                try self.flushInlineComment(self.last_formatted_pos);

                // Format the exposed list
                try self.formatExposedList(m.exposes);
            },
            .app => |a| {
                _ = try self.flushCommentsBefore(a.region);
                try self.pushAll("app");

                // Check if we need multiline formatting
                const packages_idx = a.packages;
                var needs_multiline = false;

                if (packages_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
                    // Use multiline if:
                    // 1. We have multiple packages
                    // 2. Any package has a platform clause (complex structure)
                    // 3. There are comments in the header region

                    var package_count: usize = 0;
                    var has_platform_clause = false;

                    var check_it = self.ast.node_slices.nodes(&packages_idx);
                    while (check_it.next()) |pkg| {
                        package_count += 1;
                        const pkg_node = self.getNode(pkg);
                        if (pkg_node.tag == .binop_colon) {
                            const binop = self.ast.node_slices.binOp(pkg_node.payload.binop);
                            const value_node = self.getNode(binop.rhs);
                            if (value_node.tag == .binop_platform) {
                                has_platform_clause = true;
                            }
                        }
                    }

                    // Check for comments by examining if tokens were collected
                    // (tokens are only collected when comments are present)
                    const has_comments = self.tokens.items.len > 0;

                    needs_multiline = package_count > 1 or has_platform_clause or has_comments;
                }

                if (needs_multiline) {
                    // Multiline format with proper comment preservation
                    try self.ensureNewline();
                    try self.pushIndent();
                    try self.push('{');
                    try self.ensureNewline();
                    self.curr_indent += 1;

                    if (packages_idx != collections.NodeSlices(Node.Idx).Idx.NIL) {
                        var it = self.ast.node_slices.nodes(&packages_idx);
                        while (it.next()) |pkg| {
                            const pkg_node = self.getNode(pkg);

                            // Flush any comments before this package
                            _ = try self.flushCommentsBefore(pkg_node.start);

                            try self.pushIndent();

                            if (pkg_node.tag == .binop_colon) {
                                const binop = self.ast.node_slices.binOp(pkg_node.payload.binop);
                                try self.formatNode(binop.lhs);
                                try self.pushAll(": ");

                                const value_node = self.getNode(binop.rhs);
                                if (value_node.tag == .binop_platform) {
                                    const platform_binop = self.ast.node_slices.binOp(value_node.payload.binop);
                                    try self.formatNode(platform_binop.lhs);
                                    try self.pushAll(" platform ");

                                    const rhs_node = self.getNode(platform_binop.rhs);
                                    if (rhs_node.tag == .block) {
                                        try self.push('[');
                                        try self.ensureNewline();
                                        self.curr_indent += 1;

                                        const nodes_idx = rhs_node.payload.nodes;
                                        var provides_it = self.ast.node_slices.nodes(&nodes_idx);
                                        while (provides_it.next()) |item| {
                                            const item_node = self.getNode(item);
                                            _ = try self.flushCommentsBefore(item_node.start);
                                            try self.pushIndent();
                                            try self.formatNode(item);
                                            try self.push(',');
                                            try self.ensureNewline();
                                        }

                                        self.curr_indent -= 1;
                                        try self.pushIndent();
                                        try self.push(']');
                                    } else {
                                        try self.formatNode(platform_binop.rhs);
                                    }
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

                    self.curr_indent -= 1;
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
                                    try self.formatNode(platform_binop.lhs);
                                    try self.pushAll(" platform ");
                                    const rhs_node = self.getNode(platform_binop.rhs);
                                    if (rhs_node.tag == .block) {
                                        try self.push('[');
                                        const nodes_idx = rhs_node.payload.nodes;
                                        var provides_it = self.ast.node_slices.nodes(&nodes_idx);
                                        var provides_first = true;
                                        while (provides_it.next()) |item| {
                                            if (!provides_first) try self.pushAll(", ");
                                            provides_first = false;
                                            try self.formatNode(item);
                                        }
                                        try self.push(']');
                                    } else {
                                        try self.formatNode(platform_binop.rhs);
                                    }
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
                _ = try self.flushCommentsBefore(p.region);
                try self.pushAll("package");

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
                _ = try self.flushCommentsBefore(p.region);
                try self.pushAll("platform");

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
                _ = try self.flushCommentsBefore(h.region);
                try self.pushAll("hosted");

                // Format exposes
                try self.formatExposedList(h.exposes);
            },
            .interface => |i| {
                _ = try self.flushCommentsBefore(i.region);
                try self.pushAll("interface");

                // Format exposes
                try self.formatExposedList(i.exposes);
            },
            .malformed => |m| {
                _ = try self.flushCommentsBefore(m.region);
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

        // For now, always format exposed lists as multiline
        // This matches the original formatter behavior
        self.curr_indent += 1;
        try self.ensureNewline();

        var iter = self.ast.node_slices.nodes(&exposes_idx);

        while (iter.next()) |node_idx| {
            // Check for comments before this item
            const node = self.getNode(node_idx);
            _ = try self.flushCommentsBefore(node.start);

            try self.pushIndent();
            try self.formatNode(node_idx);
            try self.push(',');
            try self.ensureNewline();
        }

        self.curr_indent -= 1;
        try self.push(']');
    }

    fn flushInlineComment(self: *Formatter, start_pos: usize) !void {
        // Check if there's an inline comment on the same line after this position
        if (start_pos >= self.source.len) {
            return;
        }

        // Scan forward from this position, looking for a comment on the same line
        var pos = start_pos;
        var found_newline = false;

        while (pos < self.source.len) {
            const c = self.source[pos];

            if (c == '\n') {
                // Reached end of line without finding a comment
                found_newline = true;
                break;
            } else if (c == '#') {
                // Found an inline comment!
                try self.pushAll(" ");
                try self.push('#');
                pos += 1;

                // Output the rest of the comment
                while (pos < self.source.len and self.source[pos] != '\n') {
                    try self.push(self.source[pos]);
                    pos += 1;
                }

                // Update our position to after the comment
                self.last_formatted_pos = pos;
                break;
            } else if (c == ' ' or c == '\t') {
                // Skip whitespace
                pos += 1;
            } else {
                // Hit non-whitespace, non-comment - stop looking
                break;
            }
        }
    }

    fn formatNode(self: *Formatter, node_idx: Node.Idx) anyerror!void {
        const node = self.getNode(node_idx);

        // Flush any comments before this node
        try self.flushCommentsBeforeNode(node);

        switch (node.tag) {
            // Binary operations
            .binop_equals => try self.formatBinOp(node_idx, " = "),
            .binop_double_equals => try self.formatBinOp(node_idx, " == "),
            .binop_not_equals => try self.formatBinOp(node_idx, " != "),
            .binop_colon => try self.formatBinOp(node_idx, ": "),
            .binop_colon_equals => try self.formatBinOp(node_idx, " := "),
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
            .binop_where => try self.formatBinOp(node_idx, " where "),
            .binop_platform => try self.formatBinOp(node_idx, " platform "),
            .binop_pipe => try self.formatBinOp(node_idx, " | "),

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
            .import => try self.formatImportStatement(node_idx),
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
            .malformed => |diagnostic| {
                // For malformed nodes, we preserve the original source text
                // to avoid losing what the user wrote
                const start = node.start.offset;

                // Find the end of this malformed node using diagnostic information
                // The diagnostic gives us hints about what went wrong
                const end = self.findMalformedEndSmart(node_idx, diagnostic);

                if (start < end and end <= self.source.len) {
                    // Get the source text for this malformed node
                    var source_text = self.source[start..end];

                    // Apply diagnostic-aware trimming
                    source_text = self.trimMalformedTextSmart(source_text, diagnostic);

                    // Output the preserved source
                    if (source_text.len > 0) {
                        try self.pushAll(source_text);
                    }

                    // Update our position
                    if (end > self.last_formatted_pos) {
                        self.last_formatted_pos = end;
                    }
                }
            },
        }

        // Update position to past this node
        const node_end = self.findNodeEnd(node_idx);
        if (node_end > self.last_formatted_pos) {
            // Check for inline comments after this node
            try self.flushInlineComment(node_end);

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
                    self.curr_indent += 1;
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
                    // Preserve trailing comma if it exists
                    if (self.hasTrailingComma(rhs)) {
                        try self.push(',');
                    }
                    self.curr_indent -= 1;
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
            self.curr_indent += 1;
            try self.pushIndent();
            try self.push('.');

            if (needs_rhs_parens) try self.push('(');
            try self.formatNode(rhs);
            if (needs_rhs_parens) try self.push(')');

            self.curr_indent -= 1;
            return;
        }

        // Special handling for equals in statements
        if (node.tag == .binop_equals) {
            // Format pattern
            if (needs_lhs_parens) try self.push('(');
            try self.formatNode(lhs);
            if (needs_lhs_parens) try self.push(')');

            // Check for comments between LHS and equals
            const lhs_end = self.findNodeEnd(lhs);
            try self.flushInlineComment(lhs_end);

            try self.pushAll(" = ");

            // Check for comments after equals before RHS
            const rhs_node = self.getNode(rhs);
            var comment_after_equals = false;

            // Find the equals sign in the source
            var equals_pos = lhs_end;
            while (equals_pos < self.source.len and self.source[equals_pos] != '=') {
                equals_pos += 1;
            }
            if (equals_pos < self.source.len) {
                equals_pos += 1; // Move past the =

                // Now check for comments between = and RHS
                const rhs_start = rhs_node.start.offset;
                // ALWAYS check for comments, even if positions seem wrong
                if (equals_pos < rhs_start) {
                    const between = self.source[equals_pos..rhs_start];
                    var i: usize = 0;

                    // Skip initial whitespace
                    while (i < between.len and (between[i] == ' ' or between[i] == '\t')) {
                        i += 1;
                    }

                    if (i < between.len and between[i] == '#') {
                        // Found inline comment after equals
                        try self.pushAll(" #");
                        i += 1;

                        // Output the rest of the comment
                        while (i < between.len and between[i] != '\n') {
                            try self.push(between[i]);
                            i += 1;
                        }

                        // Always move to new line after inline comment
                        try self.ensureNewline();

                        // IMMEDIATELY output indentation for the RHS
                        // Since we have a comment, RHS should be indented
                        try self.push('\t');

                        // Update position to after the comment and any following whitespace
                        // This prevents the comment from being processed again
                        var pos = equals_pos + i;
                        if (pos < self.source.len and self.source[pos] == '\n') {
                            pos += 1;
                            // Skip any leading whitespace on the next line
                            while (pos < self.source.len and (self.source[pos] == ' ' or self.source[pos] == '\t')) {
                                pos += 1;
                            }
                        }
                        self.last_formatted_pos = pos;

                        // Force multiline since we have a comment
                        multiline = true;
                        comment_after_equals = true;
                    }
                }
            }

            if (multiline) {
                self.curr_indent += 1;
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
                self.curr_indent -= 1;
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

            // Check for comments before operator
            const lhs_end = self.findNodeEnd(lhs);
            try self.flushInlineComment(lhs_end);

            try self.pushAll(op_str);

            // Check for comments after operator
            const rhs_node = self.getNode(rhs);
            const between_start = self.last_formatted_pos;
            const rhs_start = rhs_node.start.offset;
            if (rhs_start > between_start) {
                // Flush any comments between operator and RHS
                try self.flushCommentsBeforeNode(rhs_node);
            }

            if (needs_rhs_parens) try self.push('(');
            try self.formatNode(rhs);
            if (needs_rhs_parens) try self.push(')');
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

        const start = node.start.offset + 1; // Skip the dot
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
        try self.pushAll(".");
        const node = self.getNode(node_idx);

        // Find the identifier text from the source (skip the dot)
        const start = node.start.offset + 1; // Skip the dot
        const end = self.findIdentifierEnd(start);

        const ident = self.source[start..end];
        try self.pushAll(ident);
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

    fn formatImportStatement(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("import ");

        // The parser creates import nodes in this exact order:
        // 1. Module path components (series of lc/uc nodes, potentially with dots)
        // 2. Optional alias (single uc/lc node after encountering "as" keyword)
        // 3. Optional exposed items (nodes after encountering "exposing" keyword)

        // The parser tracks the position where it sees "as" and "exposing" keywords,
        // so we can detect the structure by looking at the source between nodes

        var it = self.ast.node_slices.nodes(&node.payload.import_nodes);
        var nodes = std.ArrayList(Node.Idx).init(self.allocator);
        defer nodes.deinit();

        while (it.next()) |item| {
            try nodes.append(item);
        }

        if (nodes.items.len == 0) return;

        // To determine the structure properly, we need to check the source text
        // between nodes for the "as" and "exposing" keywords
        var path_components = std.ArrayList(Node.Idx).init(self.allocator);
        defer path_components.deinit();
        var alias_node: ?Node.Idx = null;
        var exposing_items = std.ArrayList(Node.Idx).init(self.allocator);
        defer exposing_items.deinit();

        // State machine for parsing import structure
        const State = enum { path, after_as, after_exposing };
        var state: State = .path;

        for (nodes.items, 0..) |node_idx_item, idx| {
            const curr_node = self.getNode(node_idx_item);

            // Check source between previous node and current node for keywords
            if (idx > 0) {
                const prev_node_idx = nodes.items[idx - 1];
                const prev_end = self.findNodeEnd(prev_node_idx);
                const curr_start = curr_node.start.offset;

                if (prev_end < curr_start) {
                    const between = self.source[prev_end..curr_start];

                    // Look for "as" keyword
                    if (std.mem.indexOf(u8, between, " as ") != null) {
                        state = .after_as;
                    }
                    // Look for "exposing" keyword
                    else if (std.mem.indexOf(u8, between, " exposing") != null) {
                        state = .after_exposing;
                    }
                }
            }

            // Place node in appropriate category based on state
            switch (state) {
                .path => try path_components.append(node_idx_item),
                .after_as => {
                    if (alias_node == null) {
                        alias_node = node_idx_item;
                        // After alias, check if we have exposing
                        state = .path; // Reset to look for exposing
                    } else {
                        // This shouldn't happen but handle it
                        try exposing_items.append(node_idx_item);
                    }
                },
                .after_exposing => try exposing_items.append(node_idx_item),
            }
        }

        // Format the module path
        for (path_components.items, 0..) |path_node, idx| {
            if (idx > 0) {
                const curr = self.getNode(path_node);
                // Only add dot if not a string literal (they include their own dot)
                if (curr.tag != .str_literal_small and curr.tag != .str_literal_big) {
                    try self.push('.');
                }
            }
            try self.formatNode(path_node);
        }

        // Format alias if present
        if (alias_node) |alias| {
            try self.pushAll(" as ");
            try self.formatNode(alias);
        }

        // Format exposing clause if present
        if (exposing_items.items.len > 0) {
            try self.pushAll(" exposing [");

            for (exposing_items.items, 0..) |exposed, idx| {
                if (idx > 0) {
                    try self.pushAll(", ");
                }
                try self.formatNode(exposed);
            }

            try self.push(']');
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

    fn formatList(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Check if this list should be multiline
        const multiline = self.collectionIsMultiline(node_idx);

        try self.pushAll("[");

        if (multiline) {
            self.curr_indent += 1;
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
                    _ = try self.flushInlineComment(self.last_formatted_pos);
                }
                // Flush any comments before this element
                _ = try self.flushCommentsBefore(elem_node.start);
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
                _ = try self.flushInlineComment(self.last_formatted_pos);
            }
        }

        if (multiline) {
            // Preserve trailing comma if it exists
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
                // Check for comment after trailing comma
                _ = try self.flushInlineComment(self.last_formatted_pos);
            }
            self.curr_indent -= 1;
            // Flush any remaining comments before the closing bracket
            _ = try self.flushCommentsBeforePosition(self.findClosingBracket(node_idx));
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.pushAll("]");
    }

    fn formatRecord(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Check if this record should be multiline
        const multiline = self.collectionIsMultiline(node_idx);

        if (multiline) {
            try self.push('{');
            self.curr_indent += 1;
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
                    _ = try self.flushInlineComment(self.last_formatted_pos);
                }
                // Flush any comments before this field
                _ = try self.flushCommentsBefore(field_node.start);
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
                _ = try self.flushInlineComment(self.last_formatted_pos);
            }
        }

        if (multiline) {
            // Preserve trailing comma if it exists
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
                // Check for comment after trailing comma
                _ = try self.flushInlineComment(self.last_formatted_pos);
            }
            self.curr_indent -= 1;
            // Flush any remaining comments before the closing brace
            _ = try self.flushCommentsBeforePosition(self.findClosingBrace(node_idx));
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

        try self.push('(');

        if (multiline) {
            self.curr_indent += 1;
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
                    _ = try self.flushInlineComment(self.last_formatted_pos);
                }
                // Flush any comments before this element
                _ = try self.flushCommentsBefore(child_node.start);
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
                _ = try self.flushInlineComment(self.last_formatted_pos);
            }
            first = false;
        }

        if (multiline) {
            // Preserve trailing comma if it exists
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
            }
            self.curr_indent -= 1;
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.push(')');
    }

    fn formatBlockContents(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const nodes_idx = node.payload.nodes;
        var iter = self.ast.node_slices.nodes(&nodes_idx);

        var prev_stmt_end: usize = self.last_formatted_pos;
        var first = true;

        while (iter.next()) |stmt_idx| {
            const stmt_node = self.getNode(stmt_idx);
            const stmt_start = stmt_node.start.offset;

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
            prev_stmt_end = self.findNodeEnd(stmt_idx);
            first = false;
        }
    }

    fn formatBlock(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);
        const nodes_idx = node.payload.nodes;

        try self.push('{');
        self.curr_indent += 1;

        var iter = self.ast.node_slices.nodes(&nodes_idx);
        var first = true;
        while (iter.next()) |stmt_idx| {
            try self.ensureNewline();
            try self.pushIndent();
            try self.formatNode(stmt_idx);
            first = false;
        }

        self.curr_indent -= 1;
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

        // Check if multiline formatting is needed
        const multiline = self.shouldIfElseBeMultiline(node_idx, condition.?, then_branch.?, else_branch);

        try self.pushAll("if ");
        try self.formatNode(condition.?);

        if (multiline) {
            // Roc doesn't use 'then', just space before the block
            try self.ensureNewline();
            self.curr_indent += 1;
            try self.pushIndent();
            try self.formatNode(then_branch.?);
            self.curr_indent -= 1;

            if (else_branch) |eb| {
                try self.ensureNewline();
                try self.pushIndent();

                // Check for else-if chain
                const eb_node = self.getNode(eb);
                if (eb_node.tag == .if_else) {
                    try self.pushAll("else ");
                    try self.formatIfElse(eb);
                } else {
                    try self.pushAll("else");
                    try self.ensureNewline();
                    self.curr_indent += 1;
                    try self.pushIndent();
                    try self.formatNode(eb);
                    self.curr_indent -= 1;
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

    fn shouldIfElseBeMultiline(self: *Formatter, _: Node.Idx, condition: Node.Idx, then_branch: Node.Idx, else_branch: ?Node.Idx) bool {
        // Check if condition is complex
        if (self.nodeWillBeMultiline(condition)) return true;

        // Check if then branch is complex
        if (self.nodeWillBeMultiline(then_branch)) return true;

        // Check if else branch exists and is complex
        if (else_branch) |eb| {
            if (self.nodeWillBeMultiline(eb)) return true;
        }

        // Check for comments between parts
        const cond_end = self.findNodeEnd(condition);
        const then_node = self.getNode(then_branch);
        const then_start = then_node.start.offset;
        if (cond_end < then_start) {
            const between = self.source[cond_end..then_start];
            for (between) |c| {
                if (c == '\n' or c == '#') return true;
            }
        }

        // Check estimated total length
        const cond_node = self.getNode(condition);
        const estimated_len = 20 + (cond_end - cond_node.start.offset) +
            (self.findNodeEnd(then_branch) - then_start);
        if (else_branch) |eb| {
            const eb_node = self.getNode(eb);
            const estimated_else = 6 + (self.findNodeEnd(eb) - eb_node.start.offset);
            if (estimated_len + estimated_else > 80) return true;
        }

        return estimated_len > 80;
    }

    fn formatMatch(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        try self.pushAll("when ");

        // Match nodes use match_branches to count branches
        const branch_count = node.payload.match_branches;
        const nodes_idx = @as(collections.NodeSlices(Node.Idx).Idx, @enumFromInt(branch_count));
        var it = self.ast.node_slices.nodes(&nodes_idx);

        // First node is the scrutinee
        if (it.next()) |scrutinee| {
            try self.formatNode(scrutinee);
        }

        try self.pushAll(" is");

        // Check if multiline
        const multiline = self.nodeWillBeMultiline(node_idx);
        if (multiline) {
            try self.ensureNewline();
            self.curr_indent += 1;
        } else {
            try self.pushAll(" ");
        }

        // Format branches (pattern -> expr pairs)
        var first_branch = true;
        while (it.next()) |pattern_idx| {
            if (!first_branch) {
                if (multiline) {
                    try self.ensureNewline();
                } else {
                    try self.pushAll(", ");
                }
            }
            first_branch = false;

            if (multiline) {
                const pattern_node = self.getNode(pattern_idx);
                // Flush any comments before this pattern
                _ = try self.flushCommentsBefore(pattern_node.start);
                try self.pushIndent();
            }

            // Format pattern - handle pattern alternatives (|) specially
            const pattern_node = self.getNode(pattern_idx);
            if (pattern_node.tag == .binop_pipe) {
                // This is a pattern alternative like Ok(x) | Err(x)
                try self.formatPatternWithAlternatives(pattern_idx);
            } else {
                try self.formatNode(pattern_idx);
            }

            try self.pushAll(" -> ");

            // Format body
            if (it.next()) |body_idx| {
                const body_node = self.getNode(body_idx);

                // Complex bodies get their own line
                const needs_indent = switch (body_node.tag) {
                    .block, .if_else, .match, .lambda => true,
                    else => false,
                };

                if (needs_indent and multiline) {
                    try self.ensureNewline();
                    self.curr_indent += 1;
                    try self.pushIndent();
                    try self.formatNode(body_idx);
                    self.curr_indent -= 1;
                } else {
                    try self.formatNode(body_idx);
                }

                // Check for inline comments
                if (multiline) {
                    _ = try self.flushInlineComment(self.last_formatted_pos);
                }
            }
        }

        if (multiline) {
            self.curr_indent -= 1;
        }
    }

    fn formatPatternWithAlternatives(self: *Formatter, node_idx: Node.Idx) !void {
        // Format pattern alternatives like Ok(x) | Err(x)
        const node = self.getNode(node_idx);

        if (node.tag != .binop_pipe) {
            // Not a pattern alternative, format normally
            try self.formatNode(node_idx);
            return;
        }

        // Recursively format pattern alternatives
        const binop = self.ast.node_slices.binOp(node.payload.binop);

        // Check if left side is also a pipe (chained alternatives)
        const lhs_node = self.getNode(binop.lhs);
        if (lhs_node.tag == .binop_pipe) {
            try self.formatPatternWithAlternatives(binop.lhs);
        } else {
            try self.formatNode(binop.lhs);
        }

        try self.pushAll(" | ");
        try self.formatNode(binop.rhs);
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

        try self.pushAll("\\");

        // Lambda nodes use body_then_args which stores body first, then args
        var it = self.ast.node_slices.nodes(&node.payload.body_then_args);

        // First node is the body - save it for later
        const body = it.next();

        if (multiline) {
            self.curr_indent += 1;
        }

        // Format arguments
        var first_arg = true;
        while (it.next()) |arg| {
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
            // Preserve trailing comma if it exists
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
            }
            self.curr_indent -= 1;
            try self.ensureNewline();
            try self.pushIndent();
        }

        try self.pushAll(" -> ");

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
            try self.formatNode(func);
        }

        // Format arguments in parentheses
        try self.push('(');

        if (multiline) {
            self.curr_indent += 1;
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
            self.curr_indent -= 1;
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
            self.curr_indent += 1;
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
            self.curr_indent -= 1;

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

    fn findQualifiedIdentifierEnd(self: *const Formatter, start: usize) usize {
        var end = start;
        while (end < self.source.len) {
            const c = self.source[end];
            if (std.ascii.isAlphanumeric(c) or c == '_' or c == '!') {
                end += 1;
            } else if (c == '.' and end + 1 < self.source.len and
                (std.ascii.isAlphabetic(self.source[end + 1]) or self.source[end + 1] == '_'))
            {
                end += 1; // Include the dot
            } else {
                break;
            }
        }
        return end;
    }

    fn findStringEnd(self: *const Formatter, start: usize) usize {
        var end = start;
        if (end < self.source.len and self.source[end] == '"') {
            end += 1; // Skip opening quote
            while (end < self.source.len) {
                if (self.source[end] == '"' and (end == 0 or self.source[end - 1] != '\\')) {
                    return end + 1;
                }
                end += 1;
            }
        }
        return end;
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
        var pos = node.start.offset;

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
        var pos = node.start.offset;

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
        var max_end: usize = node.start.offset;

        // Get appropriate iterator based on node type
        switch (node.tag) {
            .import => {
                var it = self.ast.node_slices.nodes(&node.payload.import_nodes);
                while (it.next()) |child| {
                    const child_end = self.findNodeEnd(child);
                    if (child_end > max_end) {
                        max_end = child_end;
                    }
                }
            },
            .match => {
                // Match nodes store branch count, not nodes
                // We need to scan the source to find the end
                return self.findMatchEnd(node_idx);
            },
            .if_else, .if_without_else => {
                // If expressions store branch count in if_branches
                // Scan for the end of the if expression
                return self.findIfEnd(node_idx);
            },
            .lambda => {
                // Lambda uses body_then_args
                var it = self.ast.node_slices.nodes(&node.payload.body_then_args);
                while (it.next()) |child| {
                    const child_end = self.findNodeEnd(child);
                    if (child_end > max_end) {
                        max_end = child_end;
                    }
                }
            },
            .binop_colon, .binop_equals, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_double_question, .binop_lt, .binop_gt, .binop_lte, .binop_gte, .binop_double_equals, .binop_not_equals, .binop_and, .binop_or, .binop_pipe, .binop_dot, .binop_colon_equals, .binop_as, .binop_where, .binop_thick_arrow, .binop_thin_arrow, .binop_platform => {
                // Binary operators use binop payload
                const binop = self.ast.node_slices.binOp(node.payload.binop);
                const lhs_end = self.findNodeEnd(binop.lhs);
                const rhs_end = self.findNodeEnd(binop.rhs);
                return @max(lhs_end, rhs_end);
            },
            else => {
                var it = self.ast.node_slices.nodes(&node.payload.nodes);
                while (it.next()) |child| {
                    const child_end = self.findNodeEnd(child);
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
        const start = node.start.offset;
        var end = start;

        // Scan forward to find the end of the number
        // Numbers can contain digits, underscores, dots (for fractions), and hex letters
        while (end < self.source.len) {
            const c = self.source[end];
            if (std.ascii.isHex(c) or c == '_' or c == '.' or c == 'x' or c == 'e' or c == 'E' or c == '+' or c == '-') {
                end += 1;
            } else {
                break;
            }
        }

        // Now format it, collapsing multiple underscores
        var i = start;
        var last_was_underscore = false;
        while (i < end) : (i += 1) {
            const c = self.source[i];
            if (c == '_') {
                if (!last_was_underscore) {
                    try self.push('_');
                    last_was_underscore = true;
                }
            } else {
                try self.push(c);
                last_was_underscore = false;
            }
        }
    }

    fn formatStringInterpolation(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // String interpolation is handled by preserving source with expression formatting
        // For now, extract from source and format embedded expressions
        const start = node.start.offset;
        const end = self.findNodeEnd(node_idx);

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
                const part_start = part.start.offset;
                const part_end = self.findNodeEnd(part_idx);

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
        const start = node.start.offset;
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
        const end_offset = self.findNodeEnd(node_idx);

        // Calculate the end position based on the offset
        // We need to count lines and columns from the start
        var line = node.start.line;
        var column = node.start.column;
        var pos = node.start.offset;

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
            .start = node.start,
            .end = .{
                .offset = end_offset,
                .line = line,
                .column = column,
            },
        };
    }

    fn nodeWillBeMultiline(self: *const Formatter, node_idx: Node.Idx) bool {
        const node = self.getNode(node_idx);

        // Only dot-access with whitespace before the dot should be multiline
        if (node.tag == .binop_dot) {
            const binop = self.ast.node_slices.binOp(node.payload.binop);
            const lhs_end = self.findNodeEnd(binop.lhs);
            const rhs_node = self.getNode(binop.rhs);
            const rhs_start = rhs_node.start.offset;

            // Check if there's whitespace before the dot
            if (lhs_end < rhs_start and rhs_start < self.source.len) {
                const between = self.source[lhs_end..rhs_start];
                for (between) |c| {
                    if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                        return true; // Found whitespace before dot
                    } else if (c == '.') {
                        break; // Reached the dot without whitespace
                    }
                }
            }
        }

        // All other operators are single-line
        return false;
    }

    fn collectionIsMultiline(self: *const Formatter, node_idx: Node.Idx) bool {
        // Collections are multiline if they:
        // 1. Have a trailing comma
        // 2. Contain comments
        // 3. Have newlines between elements
        // 4. Are too long to fit on one line (>80 chars)
        // 5. Contain malformed nodes

        // Check for trailing comma first (cheapest check)
        if (self.hasTrailingComma(node_idx)) {
            return true;
        }

        const node = self.getNode(node_idx);

        // Check if any child is malformed - those should be multiline to be clearer
        var iter = self.ast.node_slices.nodes(&node.payload.nodes);
        var element_count: usize = 0;
        while (iter.next()) |child_idx| {
            element_count += 1;
            const child = self.getNode(child_idx);
            if (child.tag == .malformed) {
                // Skip malformed nodes in formatting
                continue;
            }
        }

        // If we have many elements, format as multiline
        if (element_count > 4) {
            return true;
        }

        // Check for comments or newlines in the collection
        const start = node.start.offset;
        const end = self.findNodeEnd(node_idx);

        if (start >= end or end > self.source.len) {
            return false;
        }

        // Estimate the line length if formatted on one line
        const estimated_length = end - start;
        if (estimated_length > 60) { // Conservative estimate for line length
            return true;
        }

        // Scan the source for comments or newlines
        var i = start;
        while (i < end) {
            const c = self.source[i];
            if (c == '#') {
                // Found a comment - definitely multiline
                return true;
            }
            if (c == '\n') {
                // Found a newline - check if it's between elements (not just after opening bracket)
                // Look back to see if we've seen any content
                var j = i;
                while (j > start) {
                    j -= 1;
                    const prev = self.source[j];
                    if (prev != ' ' and prev != '\t' and prev != '\n' and prev != '[' and prev != '{' and prev != '(') {
                        // Found content before the newline - this is multiline
                        return true;
                    }
                    if (prev == '[' or prev == '{' or prev == '(') {
                        // Newline right after opening - not necessarily multiline
                        break;
                    }
                }
            }
            i += 1;
        }

        return false;
    }

    fn hasTrailingComma(self: *const Formatter, node_idx: Node.Idx) bool {
        const node = self.getNode(node_idx);

        // Look for a comma before the closing delimiter
        const start = node.start.offset;
        const end_estimate = self.findNodeEnd(node_idx);

        if (end_estimate <= start or end_estimate > self.source.len) {
            return false;
        }

        // Single backward scan to find both delimiter and comma
        var i = end_estimate;
        var found_delimiter = false;

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

    fn findNodeEnd(self: *const Formatter, node_idx: Node.Idx) usize {
        const node = self.getNode(node_idx);

        switch (node.tag) {
            // Simple identifiers - scan for end of identifier
            .lc, .uc, .dot_lc, .var_lc, .neg_lc, .not_lc, .double_dot_lc => {
                return self.findIdentifierEnd(node.start.offset);
            },

            // Qualified identifiers - scan including dots
            .uc_dot_ucs, .lc_dot_ucs => {
                return self.findQualifiedIdentifierEnd(node.start.offset);
            },

            // String literals - scan for closing quote
            .str_literal_small => {
                return self.findStringEnd(node.start.offset);
            },

            // Number literals - scan for end of number
            .num_literal_i32, .num_literal_big, .int_literal_i32, .int_literal_big, .frac_literal_small, .frac_literal_big => {
                return self.findNumberEnd(node.start.offset);
            },

            // Delimited collections
            .list_literal => return self.scanForClosingDelimiter(node.start.offset, '[', ']'),
            .record_literal, .block => return self.scanForClosingDelimiter(node.start.offset, '{', '}'),
            .tuple_literal => return self.scanForClosingDelimiter(node.start.offset, '(', ')'),

            // All binary operators - return end of RHS
            .binop_equals, .binop_double_equals, .binop_not_equals, .binop_colon, .binop_colon_equals, .binop_dot, .binop_plus, .binop_minus, .binop_star, .binop_slash, .binop_double_slash, .binop_double_question, .binop_gt, .binop_gte, .binop_lt, .binop_lte, .binop_thick_arrow, .binop_thin_arrow, .binop_and, .binop_or, .binop_as, .binop_where, .binop_platform, .binop_pipe => {
                const binop = self.ast.node_slices.binOp(node.payload.binop);
                return self.findNodeEnd(binop.rhs);
            },

            // Nodes with children - find max end of all children
            .apply_lc, .apply_uc, .apply_anon, .apply_module, .import, .expect, .crash, .for_loop, .while_loop, .match, .if_else, .if_without_else => {
                return self.findMaxChildEnd(node_idx);
            },

            // Lambda - find end of body
            .lambda => {
                const lambda = self.ast.lambda(node_idx);
                return self.findNodeEnd(lambda.body);
            },
            .lambda_no_args => {
                var it = self.ast.node_slices.nodes(&node.payload.nodes);
                if (it.next()) |body| {
                    return self.findNodeEnd(body);
                }
                return node.start.offset;
            },

            else => {
                // For unknown node types, scan forward to find a reasonable end
                // Look for common delimiters or whitespace that would end most constructs
                var pos = node.start.offset;
                while (pos < self.source.len) {
                    const c = self.source[pos];
                    // Stop at delimiters, operators, or newlines that likely end this node
                    if (c == ',' or c == ';' or c == ')' or c == ']' or c == '}' or
                        c == '\n' or c == '=' or c == '>' or c == '<' or c == '|' or
                        c == '&' or c == '+' or c == '-' or c == '*' or c == '/')
                    {
                        break;
                    }
                    pos += 1;
                }
                return pos;
            },
        }
    }

    fn scanForClosingDelimiter(self: *const Formatter, start: usize, open_delim: u8, close_delim: u8) usize {
        if (start >= self.source.len) {
            return start;
        }

        // Check if we're starting at the opening delimiter
        var pos = start;
        var depth: i32 = 0;

        // If we start at the opening delimiter, count it
        if (pos < self.source.len and self.source[pos] == open_delim) {
            depth = 1;
            pos += 1;
        } else {
            // We're starting inside, so we need to find the closing delimiter
            // that matches our implicit opening
            depth = 1;
        }

        var in_string = false;
        var escape_next = false;

        while (pos < self.source.len) {
            const c = self.source[pos];

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
                if (c == open_delim) {
                    depth += 1;
                } else if (c == close_delim) {
                    depth -= 1;
                    if (depth == 0) {
                        return pos + 1; // Include the closing delimiter
                    }
                }
            }

            pos += 1;
        }

        // If we couldn't find the closing delimiter, return end of source
        return self.source.len;
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
        const start = node.start.offset;

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
            const other_start = other_node.start.offset;

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
        const start = node.start.offset;
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

    fn findClosingBracket(self: *const Formatter, node_idx: Node.Idx) usize {
        return self.scanForClosingDelimiter(self.getNode(node_idx).start.offset, '[', ']');
    }

    fn findClosingBrace(self: *const Formatter, node_idx: Node.Idx) usize {
        return self.scanForClosingDelimiter(self.getNode(node_idx).start.offset, '{', '}');
    }

    fn findClosingParen(self: *const Formatter, node_idx: Node.Idx) usize {
        return self.scanForClosingDelimiter(self.getNode(node_idx).start.offset, '(', ')');
    }

    fn flushCommentsBeforePosition(self: *Formatter, position: usize) !bool {
        if (position <= self.last_formatted_pos or position > self.source.len) {
            return false;
        }

        const between = self.source[self.last_formatted_pos..position];
        return self.flushComments(between);
    }

    fn flushCommentsBefore(self: *Formatter, pos: Position) !bool {
        // If we haven't collected tokens, can't flush comments
        if (self.tokens.items.len == 0) {
            return false;
        }

        // Find the range of source text to check for comments
        const start_offset = self.last_formatted_pos;
        const end_offset = pos.offset;
        if (end_offset <= start_offset) return false;

        const between_text = self.source[start_offset..end_offset];
        return self.flushComments(between_text);
    }

    fn flushCommentsBeforeNode(self: *Formatter, node: Node) !void {
        const node_start = node.start.offset;

        // Don't flush if we're already at or past this position
        if (self.last_formatted_pos >= node_start) {
            return;
        }

        // Get the source between last position and this node
        const between = self.source[self.last_formatted_pos..node_start];

        // Scan for comments and preserve them
        var i: usize = 0;
        while (i < between.len) {
            const c = between[i];

            if (c == '#') {
                // Found a comment - check if it's a doc comment (##)
                const is_doc_comment = (i + 1 < between.len and between[i + 1] == '#');

                // Output the comment marker(s)
                try self.push('#');
                i += 1;

                if (is_doc_comment) {
                    try self.push('#');
                    i += 1;
                }

                // Output the rest of the line
                while (i < between.len and between[i] != '\n') {
                    try self.push(between[i]);
                    i += 1;
                }

                // Doc comments should be preserved with proper spacing
                if (i < between.len and between[i] == '\n') {
                    try self.ensureNewline();
                    i += 1;

                    // For doc comments, preserve blank line after if present
                    if (is_doc_comment and i < between.len and between[i] == '\n') {
                        try self.ensureBlankLine();
                    }
                }
            } else if (c == '\n') {
                // Preserve blank lines
                if (i + 1 < between.len and between[i + 1] == '\n') {
                    try self.ensureBlankLine();
                    // Skip all consecutive newlines
                    while (i < between.len and between[i] == '\n') {
                        i += 1;
                    }
                } else {
                    i += 1;
                }
            } else if (c == ' ' or c == '\t' or c == '\r') {
                // Skip whitespace
                i += 1;
            } else {
                // Non-whitespace, non-comment - stop
                break;
            }
        }

        // Update position to the start of the node
        self.last_formatted_pos = node_start;
    }

    fn flushCommentsToEnd(self: *Formatter) !bool {
        // Flush any remaining comments at the end of the file
        if (self.last_formatted_pos < self.source.len) {
            const remaining = self.source[self.last_formatted_pos..];

            // Scan for comments, preserving blank lines
            var i: usize = 0;
            var found_comment = false;
            var newline_count: u32 = 0;

            while (i < remaining.len) {
                const c = remaining[i];

                if (c == '#') {
                    // Output appropriate newlines before the comment
                    if (newline_count == 0) {
                        // No newlines before comment - it's on the same line as previous content
                        // This shouldn't happen for trailing comments
                    } else if (newline_count == 1) {
                        // One newline - comment is on the next line
                        try self.ensureNewline();
                    } else {
                        // Two or more newlines - preserve blank line
                        try self.ensureNewline();
                        try self.newline(); // Force a second newline for the blank line
                    }

                    found_comment = true;
                    // Output the comment
                    try self.push('#');
                    i += 1;

                    while (i < remaining.len and remaining[i] != '\n') {
                        try self.push(remaining[i]);
                        i += 1;
                    }

                    if (i < remaining.len and remaining[i] == '\n') {
                        try self.ensureNewline();
                        i += 1;
                    }

                    // Reset newline count after comment
                    newline_count = 0;
                } else if (c == '\n') {
                    newline_count += 1;
                    i += 1;
                } else if (c == ' ' or c == '\t' or c == '\r') {
                    i += 1;
                } else {
                    // Non-whitespace, non-comment - shouldn't happen at end
                    break;
                }
            }

            return found_comment;
        }
        return false;
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
        if (self.curr_indent == 0) {
            return;
        }
        // Use TABS for indentation, not spaces!
        // Batch the indentation to avoid multiple allocations
        const tabs = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t";
        const indent_level = @min(self.curr_indent, tabs.len);
        try self.pushAll(tabs[0..indent_level]);

        // Handle very deep indentation (unlikely but possible)
        if (self.curr_indent > tabs.len) {
            for (tabs.len..self.curr_indent) |_| {
                try self.push('\t');
            }
        }
    }
};

/// Formats AST2 with source code using the default options
pub fn formatAst(allocator: std.mem.Allocator, ast: *const AST2, source: []const u8, ident_store: *const Ident.Store, root_node: ?Node.Idx) ![]u8 {
    return formatAstWithOptions(allocator, ast, source, ident_store, root_node, .{});
}

/// Formats AST2 with custom options
pub fn formatAstWithOptions(allocator: std.mem.Allocator, ast: *const AST2, source: []const u8, ident_store: *const Ident.Store, root_node: ?Node.Idx, options: FormatOptions) ![]u8 {
    var formatter = try Formatter.init(allocator, ast, source, ident_store, options);
    defer formatter.deinit();

    try formatter.format(root_node);

    // Transfer ownership to caller
    const output = try allocator.dupe(u8, formatter.output.items);

    return output;
}

// Keep the old interface for compatibility
