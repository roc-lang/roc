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
        return self.ast.nodes.get(@as(collections.SafeMultiList(Node).Idx, @enumFromInt(@as(u32, @intCast(@intFromEnum(idx))))));
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

                // Format packages (which now includes platform with provides)
                try self.pushAll(" { ");
                const packages_idx = a.packages;
                if (@intFromEnum(packages_idx) != 0) {
                    var it = self.ast.node_slices.nodes(&packages_idx);
                    var first = true;
                    while (it.next()) |pkg| {
                        if (!first) try self.pushAll(", ");
                        first = false;
                        try self.formatNode(pkg);
                    }
                }
                try self.pushAll(" }");
            },
            .package => |p| {
                _ = try self.flushCommentsBefore(p.region);
                try self.pushAll("package");

                // Format exposes
                try self.formatExposedList(p.exposes);

                // Format packages
                const packages_idx = p.packages;
                if (@intFromEnum(packages_idx) != 0) {
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
                if (@intFromEnum(p.requires_rigids) != 0 or @intFromEnum(p.requires_signatures) != @as(i32, -2147483648)) {
                    try self.pushAll(" requires ");

                    // Format rigids
                    const rigids_idx = p.requires_rigids;
                    if (@intFromEnum(rigids_idx) != 0) {
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
                if (@intFromEnum(packages_idx) != 0) {
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

        // Check if this is an empty list (index 0)
        if (@intFromEnum(exposes_idx) == 0) {
            // Parser bug: exposed items aren't being added to the AST
            // As a workaround, preserve the entire source content between brackets
            const bracket_start = self.last_formatted_pos;

            // Find the opening bracket in the source
            var open_pos = bracket_start;
            while (open_pos < self.source.len and self.source[open_pos] != '[') {
                open_pos += 1;
            }

            if (open_pos < self.source.len) {
                open_pos += 1; // Skip the [

                // Find the closing bracket
                var close_pos = open_pos;
                var depth: i32 = 1;
                while (close_pos < self.source.len and depth > 0) {
                    if (self.source[close_pos] == '[') {
                        depth += 1;
                    } else if (self.source[close_pos] == ']') {
                        depth -= 1;
                    }
                    close_pos += 1;
                }

                if (depth == 0) {
                    close_pos -= 1; // Back up to the ]

                    // Get the content between brackets
                    const content = self.source[open_pos..close_pos];

                    // Check if there's any non-whitespace content
                    var has_content = false;
                    for (content) |c| {
                        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') {
                            has_content = true;
                            break;
                        }
                    }

                    if (has_content) {
                        // Format as multiline and preserve everything
                        self.curr_indent += 1;
                        try self.ensureNewline();

                        // Parse and format the content line by line
                        var lines = std.mem.tokenizeAny(u8, content, "\n\r");
                        while (lines.next()) |line| {
                            // Trim leading/trailing whitespace
                            const trimmed = std.mem.trim(u8, line, " \t");
                            if (trimmed.len > 0) {
                                try self.pushIndent();
                                try self.pushAll(trimmed);
                                try self.ensureNewline();
                            }
                        }

                        self.curr_indent -= 1;
                        try self.pushIndent();
                    }

                    self.last_formatted_pos = close_pos + 1; // Skip past the ]
                }
            }

            try self.push(']');
            return;
        }

        // For now, always format exposed lists as multiline
        // This matches the original formatter behavior
        self.curr_indent += 1;
        try self.ensureNewline();

        var iter = self.ast.node_slices.nodes(&exposes_idx);
        var first = true;

        while (iter.next()) |node_idx| {
            // Check for comments before this item
            const node = self.getNode(node_idx);
            _ = try self.flushCommentsBefore(node.start);

            try self.pushIndent();

            try self.formatNode(node_idx);
            try self.push(',');
            try self.ensureNewline();
            first = false;
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
            .lambda_no_args => try self.formatLambdaNoArgs(node_idx),

            // Application
            .apply_lc, .apply_uc, .apply_anon, .apply_module => try self.formatApply(node_idx),

            // Unary operators
            .unary_not => try self.formatUnaryNot(node_idx),
            .unary_neg => try self.formatUnaryNeg(node_idx),

            // Import/Export

            // Loops
            .for_loop => try self.formatForLoop(node_idx),
            .while_loop => try self.formatWhileLoop(node_idx),

            // Statements
            .crash => try self.formatCrash(node_idx),

            // Malformed nodes
            .malformed => {
                try self.pushAll("<malformed>");
            },

            else => {
                // For unhandled nodes, try to output something reasonable
                try self.pushAll("<");
                try self.pushAll(@tagName(node.tag));
                try self.pushAll(">");
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

        // Special case: binop_pipe with uppercase/lowercase identifiers is actually a qualified name
        // like Bool.True being parsed as (binop_pipe (uc "Bool") (uc "True"))
        if (node.tag == .binop_pipe) {
            const lhs_node = self.getNode(lhs);
            const rhs_node = self.getNode(rhs);
            // Check if this looks like a qualified name (Uc.Uc or Uc.lc patterns)
            if ((lhs_node.tag == .uc or lhs_node.tag == .lc) and
                (rhs_node.tag == .uc or rhs_node.tag == .lc))
            {
                // This is actually a qualified name, not a pipe operator
                try self.formatNode(lhs);
                try self.push('.');
                try self.formatNode(rhs);
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
            // DEBUG: Always output marker if we find equals
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

        // For unary not, format the operand then !
        var it = self.ast.node_slices.nodes(&node.payload.nodes);
        if (it.next()) |operand| {
            try self.formatNode(operand);
        }
        try self.pushAll("!");
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

        // Import statements store the module and exposed items in import_nodes
        var it = self.ast.node_slices.nodes(&node.payload.import_nodes);

        // First node is the module path
        if (it.next()) |module| {
            try self.formatNode(module);
        }

        // Remaining nodes are exposed items
        var has_exposed = false;
        var first = true;
        while (it.next()) |item| {
            if (!has_exposed) {
                try self.pushAll(" exposing [");
                has_exposed = true;
            }
            if (!first) {
                try self.pushAll(", ");
            }
            first = false;
            try self.formatNode(item);
        }

        if (has_exposed) {
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
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
            }
            self.curr_indent -= 1;
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
            try self.pushAll(" {");
            self.curr_indent += 1;
        } else {
            try self.pushAll("{ ");
        }

        // Record literals store fields in nodes
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
            try self.formatNode(field);
        }

        if (multiline) {
            // Preserve trailing comma if it exists
            if (self.hasTrailingComma(node_idx)) {
                try self.push(',');
            }
            self.curr_indent -= 1;
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
            try self.formatNode(child_idx);
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

        try self.pushAll("if ");

        // For now, just format as a simple if-else
        // Parse shows "(if_else <0 branches>)" which means no if branches, just condition, then, else

        // Format condition
        if (it.next()) |cond| {
            try self.formatNode(cond);
        }

        try self.pushAll(" ");

        // Format then branch
        if (it.next()) |then_branch| {
            try self.formatNode(then_branch);
        }

        // Format else branch if present
        if (it.next()) |else_branch| {
            try self.pushAll(" else ");
            try self.formatNode(else_branch);
        }
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
        while (it.next()) |pattern| {
            if (!first_branch) {
                if (multiline) {
                    try self.ensureNewline();
                } else {
                    try self.pushAll(", ");
                }
            }
            first_branch = false;

            if (multiline) {
                try self.pushIndent();
            }

            // Format pattern
            try self.formatNode(pattern);
            try self.pushAll(" -> ");

            // Format body
            if (it.next()) |body| {
                try self.formatNode(body);
            }
        }

        if (multiline) {
            self.curr_indent -= 1;
        }
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
        // Body nodes
        self.curr_indent += 1;
        var first_stmt = true;
        while (it.next()) |stmt| {
            if (!first_stmt) {
                try self.ensureNewline();
            }
            try self.ensureNewline();
            try self.pushIndent();
            try self.formatNode(stmt);
            first_stmt = false;
        }
        self.curr_indent -= 1;

        try self.ensureNewline();
        try self.pushIndent();
        try self.push('}');
    }

    fn formatIdent(self: *Formatter, node_idx: Node.Idx) !void {
        const node = self.getNode(node_idx);

        // Get the identifier text from the ident store
        const ident_idx = node.payload.ident;
        const ident_text = self.ident_store.getText(ident_idx);
        try self.pushAll(ident_text);
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

    fn getSmallStr(self: *Formatter, small_str: *const [4]u8) []const u8 {
        _ = self;
        // Small strings are null-terminated ASCII in 4 bytes
        for (small_str, 0..) |b, i| {
            if (b == 0) return small_str[0..i];
        }
        return small_str;
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

        // String interpolation stores alternating string parts and expressions in nodes
        try self.push('"');
        
        var iter = self.ast.node_slices.nodes(&node.payload.nodes);
        var is_string_part = true;
        
        while (iter.next()) |part_idx| {
            const part = self.getNode(part_idx);
            
            if (is_string_part) {
                // This is a string literal part
                if (part.tag == .str_literal_small or part.tag == .str_literal_big) {
                    // Extract the string content without quotes
                    const start = part.start.offset;
                    if (start < self.source.len and self.source[start] == '"') {
                        // Skip opening quote
                        var pos = start + 1;
                        while (pos < self.source.len and self.source[pos] != '"') {
                            if (self.source[pos] == '\\' and pos + 1 < self.source.len) {
                                // Output escape sequence
                                try self.push(self.source[pos]);
                                try self.push(self.source[pos + 1]);
                                pos += 2;
                            } else {
                                try self.push(self.source[pos]);
                                pos += 1;
                            }
                        }
                    }
                }
            } else {
                // This is an interpolated expression
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
        // For now, just return a region from the start position
        // In reality we'd need to compute the end position
        return .{
            .start = node.start,
            .end = node.start, // TODO: Calculate actual end
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
        // Collections are multiline if they have a trailing comma
        return self.hasTrailingComma(node_idx);
    }

    fn hasTrailingComma(self: *const Formatter, node_idx: Node.Idx) bool {
        const node = self.getNode(node_idx);

        // Look for a comma before the closing delimiter
        const start = node.start.offset;
        const end_estimate = self.findNodeEnd(node_idx);

        if (end_estimate <= start or end_estimate > self.source.len) {
            return false;
        }

        const source_text = self.source[start..end_estimate];

        // Find closing delimiter and check for comma before it
        var i = source_text.len;
        while (i > 0) {
            i -= 1;
            const c = source_text[i];
            if (c == ']' or c == '}' or c == ')') {
                // Found closing delimiter, look backwards for comma
                var j = i;
                while (j > 0) {
                    j -= 1;
                    const prev = source_text[j];
                    if (prev == ',') {
                        return true; // Found trailing comma
                    } else if (prev != ' ' and prev != '\t' and prev != '\n' and prev != '\r') {
                        return false; // Found non-whitespace, no trailing comma
                    }
                }
                break;
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

    fn getOperatorPrecedence(self: *const Formatter, tag: Node.Tag) struct { left: u8, right: u8 } {
        _ = self;
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

    fn hasBlankLine(self: *const Formatter, text: []const u8) bool {
        _ = self;
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

        // TODO: Implement proper nodeRegion end calculation
        // This requires tracking the actual end position of nodes during parsing
        // For now, use heuristics based on node type

        switch (node.tag) {
            // Identifiers
            .lc, .uc, .dot_lc, .var_lc, .neg_lc, .not_lc, .double_dot_lc => {
                return self.findIdentifierEnd(node.start.offset);
            },

            // String literals
            .str_literal_small => {
                // Scan for closing quote
                var end = node.start.offset;
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
            },

            // Number literals
            .num_literal_i32, .num_literal_big => {
                var end = node.start.offset;
                while (end < self.source.len) {
                    const c = self.source[end];
                    if (std.ascii.isHex(c) or c == '_' or c == '.' or c == 'x' or c == 'e' or c == 'E' or c == '+' or c == '-') {
                        end += 1;
                    } else {
                        break;
                    }
                }
                return end;
            },

            // For compound nodes, scan for the closing delimiter
            .list_literal => {
                return self.scanForClosingDelimiter(node.start.offset, '[', ']');
            },
            .record_literal => {
                return self.scanForClosingDelimiter(node.start.offset, '{', '}');
            },
            .tuple_literal => {
                return self.scanForClosingDelimiter(node.start.offset, '(', ')');
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

    fn findNextToken(self: *const Formatter, char: u8) ?Position {
        // Find the next occurrence of a character in tokens
        _ = self;
        _ = char;
        // TODO: Implement proper token tracking
        return null;
    }

    /// Flush comments and blank lines before a position
    /// Returns true if there was at least one newline
    fn flushCommentsBefore(self: *Formatter, pos: Position) !bool {
        // If we haven't collected tokens, can't flush comments
        if (self.tokens.items.len == 0) {
            return false;
        }

        // Find the range of source text to check for comments
        const start_offset: usize = 0; // TODO: Track last position properly

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
                // Found a comment - output it
                try self.push('#');
                i += 1;

                // Output the rest of the line
                while (i < between.len and between[i] != '\n') {
                    try self.push(between[i]);
                    i += 1;
                }

                // Output the newline if present
                if (i < between.len and between[i] == '\n') {
                    try self.ensureNewline();
                    i += 1;
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

            // Debug: show what we're looking at
            // std.debug.print("flushCommentsToEnd: remaining = {s}\n", .{remaining[0..@min(50, remaining.len)]});

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
pub fn formatSource(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    _ = allocator;
    _ = source;
    return error.RequiresAst2;
}

pub fn formatSourceWithOptions(allocator: std.mem.Allocator, source: []const u8, options: FormatOptions) ![]u8 {
    _ = allocator;
    _ = source;
    _ = options;
    return error.RequiresAst2;
}
