//!
//! This file implements the Intermediate Representation (IR) for Roc's parser.
//!
//! The IR provides a structured, tree-based representation of Roc source code after parsing
//!
//! The design uses an arena-based memory allocation strategy with a "multi-list" approach where nodes
//! are stored in a flat list but cross-referenced via indices rather than pointers. This improves
//! memory locality and efficiency.
//!
//! The implementation includes comprehensive facilities for building, manipulating, and traversing
//! the IR, as well as converting it to S-expressions for debugging and visualization.

const std = @import("std");
const testing = std.testing;
const base = @import("../../base.zig");
const tokenize = @import("tokenize.zig");
const collections = @import("../../collections.zig");
const reporting = @import("../../reporting.zig");

const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const Token = tokenize.Token;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const exitOnOom = collections.utils.exitOnOom;

const SExpr = base.SExpr;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

const AST = @This();

source: []const u8,
tokens: TokenizedBuffer,
store: NodeStore,
root_node_idx: u32 = 0,
tokenize_diagnostics: std.ArrayListUnmanaged(tokenize.Diagnostic),
parse_diagnostics: std.ArrayListUnmanaged(AST.Diagnostic),

/// Calculate whether this region is - or will be - multiline
pub fn regionIsMultiline(self: *AST, region: TokenizedRegion) bool {
    var i = region.start;
    const tags = self.tokens.tokens.items(.tag);
    while (i <= region.end) {
        if (tags[i] == .Newline) {
            return true;
        }
        if (tags[i] == .Comma and (tags[i + 1] == .CloseSquare or
            tags[i + 1] == .CloseRound or
            tags[i + 1] == .CloseCurly))
        {
            return true;
        }
        i += 1;
    }
    return false;
}

/// Returns whether this AST has any diagnostic errors.
pub fn hasErrors(self: *AST) bool {
    return self.tokenize_diagnostics.items.len > 0 or self.parse_diagnostics.items.len > 0;
}

/// Returns diagnostic position information for the given region.
pub fn calcRegionInfo(self: *AST, region: TokenizedRegion, line_starts: []const u32) base.RegionInfo {
    const start = self.tokens.resolve(region.start);
    const end = self.tokens.resolve(region.end);
    const info = base.RegionInfo.position(self.source, line_starts, start.start.offset, end.end.offset) catch {
        // std.debug.panic("failed to calculate position info for region {?}, start: {}, end: {}", .{ region, start, end });
        return .{
            .start_line_idx = 0,
            .start_col_idx = 0,
            .end_line_idx = 0,
            .end_col_idx = 0,
            .line_text = "",
        };
    };

    return info;
}

pub fn deinit(self: *AST, gpa: std.mem.Allocator) void {
    defer self.tokens.deinit();
    defer self.store.deinit();
    defer self.tokenize_diagnostics.deinit(gpa);
    defer self.parse_diagnostics.deinit(gpa);
}

/// Convert a tokenize diagnostic to a Report for rendering
pub fn tokenizeDiagnosticToReport(self: *AST, diagnostic: tokenize.Diagnostic, allocator: std.mem.Allocator) !reporting.Report {
    _ = self; // TODO: Use self for source information
    const title = switch (diagnostic.tag) {
        .MisplacedCarriageReturn => "MISPLACED CARRIAGE RETURN",
        .AsciiControl => "ASCII CONTROL CHARACTER",
        .LeadingZero => "LEADING ZERO",
        .UppercaseBase => "UPPERCASE BASE",
        .InvalidUnicodeEscapeSequence => "INVALID UNICODE ESCAPE SEQUENCE",
        .InvalidEscapeSequence => "INVALID ESCAPE SEQUENCE",
        .UnclosedString => "UNCLOSED STRING",
        .UnclosedSingleQuote => "UNCLOSED SINGLE QUOTE",
        .OverClosedBrace => "OVER CLOSED BRACE",
        .MismatchedBrace => "MISMATCHED BRACE",
        .NonPrintableUnicodeInStrLiteral => "NON-PRINTABLE UNICODE IN STRING LITERAL",
    };

    const body = switch (diagnostic.tag) {
        .MisplacedCarriageReturn => "Carriage return characters (\\r) are not allowed in Roc source code.",
        .AsciiControl => "ASCII control characters are not allowed in Roc source code.",
        .LeadingZero => "Numbers cannot have leading zeros.",
        .UppercaseBase => "Number base prefixes must be lowercase (0x, 0o, 0b).",
        .InvalidUnicodeEscapeSequence => "This Unicode escape sequence is not valid.",
        .InvalidEscapeSequence => "This escape sequence is not recognized.",
        .UnclosedString => "This string is missing a closing quote.",
        .UnclosedSingleQuote => "This character literal is missing a closing single quote.",
        .OverClosedBrace => "There are too many closing braces here.",
        .MismatchedBrace => "This brace does not match the corresponding opening brace.",
        .NonPrintableUnicodeInStrLiteral => "Non-printable Unicode characters are not allowed in string literals.",
    };

    var report = reporting.Report.init(allocator, title, .runtime_error);
    try report.document.addText(body);
    return report;
}

/// Convert TokenizedRegion to base.Region for error reporting
pub fn tokenizedRegionToRegion(self: *AST, tokenized_region: TokenizedRegion) base.Region {
    const token_count: u32 = @intCast(self.tokens.tokens.len);

    // Ensure both start and end are within bounds
    const safe_start_idx = if (tokenized_region.start >= token_count)
        token_count - 1
    else
        tokenized_region.start;

    const safe_end_idx = if (tokenized_region.end >= token_count)
        token_count - 1
    else
        tokenized_region.end;

    // Ensure end is at least start to prevent invalid regions
    const final_end_idx = if (safe_end_idx < safe_start_idx)
        safe_start_idx
    else
        safe_end_idx;

    const start_region = self.tokens.resolve(safe_start_idx);
    const end_region = self.tokens.resolve(final_end_idx);
    return .{
        .start = start_region.start,
        .end = end_region.end,
    };
}

/// Get the text content of a token for error reporting
fn getTokenText(self: *AST, token_idx: Token.Idx) []const u8 {
    const token_region = self.tokens.resolve(@intCast(token_idx));
    return self.source[token_region.start.offset..token_region.end.offset];
}

/// Convert a parse diagnostic to a Report for rendering
pub fn parseDiagnosticToReport(self: *AST, diagnostic: Diagnostic, allocator: std.mem.Allocator, filename: []const u8) !reporting.Report {
    const raw_region = self.tokenizedRegionToRegion(diagnostic.region);

    // Ensure region bounds are valid for source slicing
    const region = base.Region{
        .start = .{ .offset = @min(raw_region.start.offset, self.source.len) },
        .end = .{ .offset = @min(@max(raw_region.end.offset, raw_region.start.offset), self.source.len) },
    };

    const title = switch (diagnostic.tag) {
        .bad_indent => "BAD INDENTATION",
        .multiple_platforms => "MULTIPLE PLATFORMS",
        .no_platform => "NO PLATFORM",
        .missing_header => "MISSING HEADER",
        .list_not_closed => "LIST NOT CLOSED",
        .missing_arrow => "MISSING ARROW",
        .expected_exposes => "EXPECTED EXPOSES",
        .expected_exposes_close_square => "EXPECTED CLOSING BRACKET",
        .expected_exposes_open_square => "EXPECTED OPENING BRACKET",
        .expected_imports => "EXPECTED IMPORTS",
        .expected_imports_close_curly => "EXPECTED CLOSING BRACE",
        .expected_imports_open_curly => "EXPECTED OPENING BRACE",
        .header_unexpected_token => "UNEXPECTED TOKEN IN HEADER",
        .pattern_unexpected_token => "UNEXPECTED TOKEN IN PATTERN",
        .pattern_unexpected_eof => "UNEXPECTED END OF FILE IN PATTERN",
        .ty_anno_unexpected_token => "UNEXPECTED TOKEN IN TYPE ANNOTATION",
        .statement_unexpected_eof => "UNEXPECTED END OF FILE",
        .statement_unexpected_token => "UNEXPECTED TOKEN",
        .string_unexpected_token => "UNEXPECTED TOKEN IN STRING",
        .expr_unexpected_token => "UNEXPECTED TOKEN IN EXPRESSION",
        .import_must_be_top_level => "IMPORT MUST BE TOP LEVEL",
        .expected_expr_close_square_or_comma => "LIST NOT CLOSED",
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
        .bad_indent => {
            try report.document.addReflowingText("The indentation here is inconsistent with the surrounding code.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Make sure to use consistent spacing for indentation.");
        },
        .list_not_closed => {
            try report.document.addReflowingText("This list is missing a closing bracket.");
            try report.document.addLineBreak();
            try report.document.addText("Add a ");
            try report.document.addAnnotated("]", .emphasized);
            try report.document.addText(" to close the list.");
        },
        .missing_arrow => {
            try report.document.addText("Expected an arrow ");
            try report.document.addAnnotated("->", .emphasized);
            try report.document.addText(" here.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Function type annotations require arrows between parameter and return types.");
        },
        .expected_exposes, .expected_exposes_close_square, .expected_exposes_open_square => {
            try report.document.addReflowingText("Module headers must have an ");
            try report.document.addKeyword("exposing");
            try report.document.addReflowingText(" section that lists what the module exposes.");
            try report.document.addLineBreak();
            try report.document.addText("For example: ");
            try report.document.addCodeBlock("module [main, add, subtract]");
        },
        .expected_imports, .expected_imports_close_curly, .expected_imports_open_curly => {
            try report.document.addReflowingText("Import statements must specify what is being imported.");
            try report.document.addLineBreak();
            try report.document.addText("For example: ");
            try report.document.addCodeBlock("import pf.Stdout exposing [line!]");
        },
        .header_unexpected_token => {
            // Try to get the actual token text
            const token_text = if (diagnostic.region.start != diagnostic.region.end)
                self.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a module header.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Module headers should only contain the module name and exposing list.");
        },
        .pattern_unexpected_token => {
            const token_text = if (diagnostic.region.start != diagnostic.region.end)
                self.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a pattern.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Patterns can contain identifiers, literals, lists, records, or tags.");
        },
        .pattern_unexpected_eof => {
            try report.document.addReflowingText("This pattern is incomplete - the file ended unexpectedly.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Complete the pattern or remove the incomplete pattern.");
        },
        .ty_anno_unexpected_token => {
            const token_text = if (diagnostic.region.start != diagnostic.region.end)
                self.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a type annotation.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Type annotations should contain types like ");
            try report.document.addType("Str");
            try report.document.addText(", ");
            try report.document.addType("Num a");
            try report.document.addText(", or ");
            try report.document.addType("List U64");
            try report.document.addText(".");
        },
        .statement_unexpected_eof => {
            try report.document.addReflowingText("This statement is incomplete - the file ended unexpectedly.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Complete the statement or remove the incomplete statement.");
        },
        .statement_unexpected_token => {
            const token_text = if (diagnostic.region.start != diagnostic.region.end)
                self.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a statement.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Statements can be definitions, assignments, or expressions.");
        },
        .string_unexpected_token => {
            const token_text = if (diagnostic.region.start != diagnostic.region.end)
                self.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a string literal.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("String literals should be enclosed in double quotes.");
        },
        .expr_unexpected_token => {
            const token_text = if (diagnostic.region.start != diagnostic.region.end)
                self.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in an expression.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Expressions can be identifiers, literals, function calls, or operators.");
        },
        .import_must_be_top_level => {
            try report.document.addReflowingText("Import statements must appear at the top level of a module.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Move this import to the top of the file, after the module header but before any definitions.");
        },
        .expected_expr_close_square_or_comma => {
            try report.document.addReflowingText("This list is missing a closing bracket or has a syntax error.");
            try report.document.addLineBreak();
            try report.document.addText("Lists must be closed with ");
            try report.document.addAnnotated("]", .emphasized);
            try report.document.addText(" and list items must be separated by commas.");
            try report.document.addLineBreak();
            try report.document.addText("For example: ");
            try report.document.addCodeBlock("[1, 2, 3]");
        },
        .expected_colon_after_type_annotation => {
            try report.document.addReflowingText("Type applications require parentheses around their type arguments.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addReflowingText("I found a type followed by what looks like a type argument, but they need to be connected with parentheses.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addText("Instead of:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addAnnotated("List U8", .error_highlight);
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addText("Use:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addAnnotated("List(U8)", .emphasized);
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addText("Other valid examples:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addAnnotated("Dict(Str, Num)", .dimmed);
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addAnnotated("Result(a, Str)", .dimmed);
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addAnnotated("Maybe(List(U64))", .dimmed);
        },
        else => {
            const tag_name = @tagName(diagnostic.tag);
            const owned_tag = try report.addOwnedString(tag_name);
            try report.document.addText("A parsing error occurred: ");
            try report.document.addAnnotated(owned_tag, .dimmed);
            try report.document.addLineBreak();
            try report.document.addReflowingText("This is an unexpected parsing error. Please check your syntax.");
        },
    }

    // Add source context if we have a valid region
    if (region.start.offset <= region.end.offset and region.end.offset <= self.source.len) {
        // Compute line_starts from source for proper region info calculation
        var line_starts = std.ArrayList(u32).init(allocator);
        defer line_starts.deinit();

        try line_starts.append(0); // First line starts at 0
        for (self.source, 0..) |char, i| {
            if (char == '\n') {
                try line_starts.append(@intCast(i + 1));
            }
        }

        // Use proper region info calculation with converted region
        const region_info = base.RegionInfo.position(self.source, line_starts.items, region.start.offset, region.end.offset) catch {
            return report; // Return report without source context if region calculation fails
        };

        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("Here is the problematic code:");
        try report.document.addLineBreak();

        // Use the proper addSourceContext method
        try report.addSourceContext(region_info, self.source, filename);
    }

    return report;
}

/// Diagnostics related to parsing
pub const Diagnostic = struct {
    tag: Tag,
    region: TokenizedRegion,

    /// different types of diagnostic errors
    pub const Tag = enum {
        bad_indent,
        multiple_platforms,
        no_platform,
        missing_header,
        list_not_closed,
        missing_arrow,
        expected_exposes,
        expected_exposes_close_square,
        expected_exposes_open_square,
        expected_imports,
        expected_imports_close_curly,
        expected_imports_open_curly,
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
        expect_closing_paren,
        header_expected_open_square,
        header_expected_close_square,
        header_unexpected_token,
        pattern_unexpected_token,
        pattern_unexpected_eof,
        bad_as_pattern_name,
        ty_anno_unexpected_token,
        statement_unexpected_eof,
        statement_unexpected_token,
        string_unexpected_token,
        string_expected_close_interpolation,
        expr_if_missing_else,
        expr_no_space_dot_int,
        import_exposing_no_open,
        import_exposing_no_close,
        no_else,
        expected_type_field_name,
        expected_colon_after_type_field_name,
        expected_arrow,
        expected_ty_close_curly_or_comma,
        expected_ty_close_square_or_comma,
        expected_lower_name_after_exposed_item_as,
        expected_upper_name_after_exposed_item_as,
        exposed_item_unexpected_token,
        expected_upper_name_after_import_as,
        expected_colon_after_type_annotation,
        expected_lower_ident_pat_field_name,
        expected_colon_after_pat_field_name,
        expected_expr_bar,
        expected_expr_close_curly_or_comma,
        expected_expr_close_round_or_comma,
        expected_expr_close_square_or_comma,
        expected_close_curly_at_end_of_match,
        expected_open_curly_after_match,
        expr_unexpected_token,
        expected_expr_record_field_name,
        expected_ty_apply_close_round,
        expected_ty_anno_end_of_function,
        expected_ty_anno_end,
        expected_expr_apply_close_round,
        where_expected_where,
        where_expected_mod_open,
        where_expected_var,
        where_expected_mod_close,
        where_expected_arg_open,
        where_expected_arg_close,
        where_expected_method_arrow,
        where_expected_method_or_alias_name,
        where_expected_var_or_module,
        import_must_be_top_level,
        invalid_type_arg,
        expr_arrow_expects_ident,
        var_only_allowed_in_a_body,
        var_must_have_ident,
        var_expected_equals,
        for_expected_in,
    };
};

/// The first and last token consumed by a Node
pub const TokenizedRegion = struct {
    start: Token.Idx,
    end: Token.Idx,

    pub fn empty() TokenizedRegion {
        return .{ .start = 0, .end = 0 };
    }

    pub fn spanAcross(self: TokenizedRegion, other: TokenizedRegion) TokenizedRegion {
        return .{
            .start = self.start,
            .end = other.end,
        };
    }

    pub fn toBase(self: TokenizedRegion) base.Region {
        return .{
            .start = base.Region.Position{ .offset = self.start },
            .end = base.Region.Position{ .offset = self.end },
        };
    }
};

/// Resolve a token index to a string slice from the source code.
pub fn resolve(self: *AST, token: Token.Idx) []const u8 {
    const range = self.tokens.resolve(token);
    return self.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
}

/// Contains properties of the thing to the right of the `import` keyword.
pub const ImportRhs = packed struct {
    /// e.g. 1 in case we use import `as`: `import Module as Mod`
    aliased: u1,
    /// 1 in case the import is qualified, e.g. `pf` in `import pf.Stdout ...`
    qualified: u1,
    /// The number of things in the exposes list. e.g. 3 in `import SomeModule exposing [a1, a2, a3]`
    num_exposes: u30,
};

// Check that all packed structs are 4 bytes size as they as cast to
// and from a u32
comptime {
    std.debug.assert(@sizeOf(Header.AppHeaderRhs) == 4);
    std.debug.assert(@sizeOf(ImportRhs) == 4);
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}

/// Helper function to convert the AST to a human friendly representation in S-expression format
pub fn toSExprStr(ast: *@This(), env: *base.ModuleEnv, writer: std.io.AnyWriter) !void {
    const file = ast.store.getFile();

    var node = file.toSExpr(env, ast);
    defer node.deinit(env.gpa);

    node.toStringPretty(writer);
}

/// The kind of the type declaration represented, either:
/// 1. An alias of the form `Foo = (Bar, Baz)`
/// 2. A nominal type of the form `Foo := [Bar, Baz]`
pub const TypeDeclKind = enum {
    alias,
    nominal,
};

/// Represents a statement.  Not all statements are valid in all positions.
pub const Statement = union(enum) {
    decl: Decl,
    @"var": struct {
        name: Token.Idx,
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    expr: struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    crash: struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    expect: struct {
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    @"for": struct {
        patt: Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    @"return": struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    import: struct {
        module_name_tok: Token.Idx,
        qualifier_tok: ?Token.Idx,
        alias_tok: ?Token.Idx,
        exposes: ExposedItem.Span,
        region: TokenizedRegion,
    },
    type_decl: struct {
        header: TypeHeader.Idx,
        anno: TypeAnno.Idx,
        where: ?Collection.Idx,
        kind: TypeDeclKind,
        region: TokenizedRegion,
    },
    type_anno: struct {
        name: Token.Idx,
        anno: TypeAnno.Idx,
        where: ?Collection.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub const Decl = struct {
        pattern: Pattern.Idx,
        body: Expr.Idx,
        region: TokenizedRegion,
    };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        switch (self) {
            .decl => |decl| {
                var node = SExpr.init(env.gpa, "s-decl");
                node.appendRegion(env.gpa, ast.calcRegionInfo(decl.region, env.line_starts.items));
                // pattern
                {
                    const pattern = ast.store.getPattern(decl.pattern);
                    var pattern_node = pattern.toSExpr(env, ast);
                    node.appendNode(env.gpa, &pattern_node);
                }
                // body
                {
                    const body = ast.store.getExpr(decl.body);
                    var body_node = body.toSExpr(env, ast);
                    node.appendNode(env.gpa, &body_node);
                }
                return node;
            },
            .@"var" => |v| {
                var node = SExpr.init(env.gpa, "s-var");
                node.appendRegion(env.gpa, ast.calcRegionInfo(v.region, env.line_starts.items));

                const name_str = ast.resolve(v.name);
                node.appendStringAttr(env.gpa, "name", name_str);

                const body = ast.store.getExpr(v.body);
                var body_node = body.toSExpr(env, ast);
                node.appendNode(env.gpa, &body_node);

                return node;
            },
            .expr => |expr| {
                return ast.store.getExpr(expr.expr).toSExpr(env, ast);
            },
            .import => |import| {
                var node = SExpr.init(env.gpa, "s-import");
                node.appendRegion(env.gpa, ast.calcRegionInfo(import.region, env.line_starts.items));

                // name e.g. `Stdout` in `import pf.Stdout`
                node.appendStringAttr(env.gpa, "module", ast.resolve(import.module_name_tok));

                // qualifier e.g. `pf` in `import pf.Stdout`
                if (import.qualifier_tok) |tok| {
                    const qualifier_str = ast.resolve(tok);
                    node.appendStringAttr(env.gpa, "qualifier", qualifier_str);
                }

                // alias e.g. `OUT` in `import pf.Stdout as OUT`
                if (import.alias_tok) |tok| {
                    const qualifier_str = ast.resolve(tok);
                    node.appendStringAttr(env.gpa, "alias", qualifier_str);
                }

                // exposed identifiers e.g. [foo, bar] in `import pf.Stdout exposing [foo, bar]`
                const exposed_slice = ast.store.exposedItemSlice(import.exposes);
                if (exposed_slice.len > 0) {
                    var exposed = SExpr.init(env.gpa, "exposing");
                    for (ast.store.exposedItemSlice(import.exposes)) |e| {
                        var exposed_item = ast.store.getExposedItem(e).toSExpr(env, ast);
                        exposed.appendNode(env.gpa, &exposed_item);
                    }
                    node.appendNode(env.gpa, &exposed);
                }
                return node;
            },
            // (type_decl (header <name> [<args>]) <annotation>)
            .type_decl => |a| {
                var node = SExpr.init(env.gpa, "s-type-decl");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                var header = SExpr.init(env.gpa, "header");

                // pattern
                {
                    const ty_header = ast.store.getTypeHeader(a.header);
                    header.appendRegion(env.gpa, ast.calcRegionInfo(ty_header.region, env.line_starts.items));
                    header.appendStringAttr(env.gpa, "name", ast.resolve(ty_header.name));

                    var args_node = SExpr.init(env.gpa, "args");

                    for (ast.store.typeAnnoSlice(ty_header.args)) |b| {
                        const anno = ast.store.getTypeAnno(b);
                        var anno_sexpr = anno.toSExpr(env, ast);
                        args_node.appendNode(env.gpa, &anno_sexpr);
                    }
                    header.appendNode(env.gpa, &args_node);

                    node.appendNode(env.gpa, &header);
                }

                // annotation
                {
                    var annotation = ast.store.getTypeAnno(a.anno).toSExpr(env, ast);
                    node.appendNode(env.gpa, &annotation);
                }
                return node;
            },
            // (crash <expr>)
            .crash => |a| {
                var node = SExpr.init(env.gpa, "s-crash");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = ast.store.getExpr(a.expr).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            // (expect <body>)
            .expect => |a| {
                var node = SExpr.init(env.gpa, "s-expect");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = ast.store.getExpr(a.body).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            .@"for" => |a| {
                var node = SExpr.init(env.gpa, "s-for");

                // patt
                {
                    var child = ast.store.getPattern(a.patt).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }
                // expr
                {
                    var child = ast.store.getExpr(a.expr).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }
                // body
                {
                    var child = ast.store.getExpr(a.body).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }

                return node;
            },
            // (return <expr>)
            .@"return" => |a| {
                var node = SExpr.init(env.gpa, "s-return");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = ast.store.getExpr(a.expr).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            // (type_anno <annotation>)
            .type_anno => |a| {
                var node = SExpr.init(env.gpa, "s-type-anno");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                node.appendStringAttr(env.gpa, "name", ast.resolve(a.name));

                var child = ast.store.getTypeAnno(a.anno).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);

                return node;
            },
            .malformed => |a| {
                var node = SExpr.init(env.gpa, "s-malformed");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "tag", @tagName(a.reason));
                return node;
            },
        }
    }
};

/// Represents a Body, or a block of statements.
pub const Body = struct {
    /// The statements that constitute the block
    statements: Statement.Span,
    region: TokenizedRegion,

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        var block_node = SExpr.init(env.gpa, "e-block");
        block_node.appendRegion(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        var statements_node = SExpr.init(env.gpa, "statements");

        for (ast.store.statementSlice(self.statements)) |stmt_idx| {
            const stmt = ast.store.getStatement(stmt_idx);

            var stmt_node = stmt.toSExpr(env, ast);

            statements_node.appendNode(env.gpa, &stmt_node);
        }

        block_node.appendNode(env.gpa, &statements_node);

        return block_node;
    }
};

/// Represents a Pattern used in pattern matching.
pub const Pattern = union(enum) {
    ident: struct {
        ident_tok: Token.Idx,
        region: TokenizedRegion,
    },
    tag: struct {
        tag_tok: Token.Idx,
        args: Pattern.Span,
        region: TokenizedRegion,
    },
    int: struct {
        number_tok: Token.Idx,
        region: TokenizedRegion,
    },
    frac: struct {
        number_tok: Token.Idx,
        region: TokenizedRegion,
    },
    string: struct {
        string_tok: Token.Idx,
        region: TokenizedRegion,
        expr: Expr.Idx,
    },
    record: struct {
        fields: PatternRecordField.Span,
        region: TokenizedRegion,
    },
    list: struct {
        patterns: Pattern.Span,
        region: TokenizedRegion,
    },
    list_rest: struct {
        name: ?Token.Idx,
        region: TokenizedRegion,
    },
    tuple: struct {
        patterns: Pattern.Span,
        region: TokenizedRegion,
    },
    underscore: struct {
        region: TokenizedRegion,
    },
    alternatives: struct {
        patterns: Pattern.Span,
        region: TokenizedRegion,
    },
    as: struct {
        pattern: Pattern.Idx,
        name: Token.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .ident => |p| p.region,
            .tag => |p| p.region,
            .int => |p| p.region,
            .frac => |p| p.region,
            .string => |p| p.region,
            .record => |p| p.region,
            .list => |p| p.region,
            .list_rest => |p| p.region,
            .tuple => |p| p.region,
            .underscore => |p| p.region,
            .alternatives => |p| p.region,
            .as => |p| p.region,
            .malformed => |p| p.region,
        };
    }

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        switch (self) {
            .ident => |ident| {
                var node = SExpr.init(env.gpa, "p-ident");

                node.appendRegion(env.gpa, ast.calcRegionInfo(ident.region, env.line_starts.items));

                node.appendStringAttr(env.gpa, "raw", ast.resolve(ident.ident_tok));

                return node;
            },
            .tag => |tag| {
                var node = SExpr.init(env.gpa, "p-tag");

                node.appendRegion(env.gpa, ast.calcRegionInfo(tag.region, env.line_starts.items));

                node.appendStringAttr(env.gpa, "raw", ast.resolve(tag.tag_tok));

                // Add arguments if there are any
                for (ast.store.patternSlice(tag.args)) |arg| {
                    var arg_node = ast.store.getPattern(arg).toSExpr(env, ast);
                    node.appendNode(env.gpa, &arg_node);
                }

                return node;
            },
            .int => |num| {
                var node = SExpr.init(env.gpa, "p-int");
                node.appendRegion(env.gpa, ast.calcRegionInfo(num.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "raw", ast.resolve(num.number_tok));
                return node;
            },
            .frac => |num| {
                var node = SExpr.init(env.gpa, "p-frac");
                node.appendRegion(env.gpa, ast.calcRegionInfo(num.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "raw", ast.resolve(num.number_tok));
                return node;
            },
            .string => |str| {
                var node = SExpr.init(env.gpa, "p-string");
                node.appendRegion(env.gpa, ast.calcRegionInfo(str.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "raw", ast.resolve(str.string_tok));
                return node;
            },
            .record => |rec| {
                var node = SExpr.init(env.gpa, "p-record");
                node.appendRegion(env.gpa, ast.calcRegionInfo(rec.region, env.line_starts.items));

                for (ast.store.patternRecordFieldSlice(rec.fields)) |field_idx| {
                    const field = ast.store.getPatternRecordField(field_idx);
                    var field_node = SExpr.init(env.gpa, "field");
                    field_node.appendRegion(env.gpa, ast.calcRegionInfo(field.region, env.line_starts.items));
                    field_node.appendStringAttr(env.gpa, "name", ast.resolve(field.name));

                    if (field.value) |value| {
                        var value_node = ast.store.getPattern(value).toSExpr(env, ast);
                        field_node.appendNode(env.gpa, &value_node);
                    }

                    field_node.appendBoolAttr(env.gpa, "rest", field.rest);

                    node.appendNode(env.gpa, &field_node);
                }

                return node;
            },
            .list => |list| {
                var node = SExpr.init(env.gpa, "p-list");
                node.appendRegion(env.gpa, ast.calcRegionInfo(list.region, env.line_starts.items));

                for (ast.store.patternSlice(list.patterns)) |pat| {
                    var pattern_node = ast.store.getPattern(pat).toSExpr(env, ast);
                    node.appendNode(env.gpa, &pattern_node);
                }

                return node;
            },
            .list_rest => |rest| {
                var node = SExpr.init(env.gpa, "p-list-rest");
                node.appendRegion(env.gpa, ast.calcRegionInfo(rest.region, env.line_starts.items));

                if (rest.name) |name_tok| {
                    node.appendStringAttr(env.gpa, "name", ast.resolve(name_tok));
                }

                return node;
            },
            .tuple => |tuple| {
                var node = SExpr.init(env.gpa, "p-tuple");
                node.appendRegion(env.gpa, ast.calcRegionInfo(tuple.region, env.line_starts.items));

                for (ast.store.patternSlice(tuple.patterns)) |pat| {
                    var pattern_node = ast.store.getPattern(pat).toSExpr(env, ast);
                    node.appendNode(env.gpa, &pattern_node);
                }

                return node;
            },
            .underscore => {
                return SExpr.init(env.gpa, "p-underscore");
            },
            .alternatives => |a| {
                // '|' separated list of patterns
                var node = SExpr.init(env.gpa, "p-alternatives");
                for (ast.store.patternSlice(a.patterns)) |pat| {
                    var patNode = ast.store.getPattern(pat).toSExpr(env, ast);
                    node.appendNode(env.gpa, &patNode);
                }
                return node;
            },
            .as => |a| {
                var node = SExpr.init(env.gpa, "p-as");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var pattern_node = ast.store.getPattern(a.pattern).toSExpr(env, ast);
                node.appendStringAttr(env.gpa, "name", ast.resolve(a.name));
                node.appendNode(env.gpa, &pattern_node);
                return node;
            },
            .malformed => |a| {
                var node = SExpr.init(env.gpa, "p-malformed");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "tag", @tagName(a.reason));
                return node;
            },
        }
    }
};

/// TODO
pub const BinOp = struct {
    left: Expr.Idx,
    right: Expr.Idx,
    operator: Token.Idx,
    region: TokenizedRegion,

    /// (binop <op> <left> <right>) e.g. (binop '+' 1 2)
    pub fn toSExpr(self: *const @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        var node = SExpr.init(env.gpa, "e-binop");
        node.appendRegion(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        node.appendStringAttr(env.gpa, "op", ast.resolve(self.operator));

        var left = ast.store.getExpr(self.left).toSExpr(env, ast);
        node.appendNode(env.gpa, &left);

        var right = ast.store.getExpr(self.right).toSExpr(env, ast);
        node.appendNode(env.gpa, &right);
        return node;
    }
};

/// TODO
pub const Unary = struct {
    operator: Token.Idx,
    expr: Expr.Idx,
    region: TokenizedRegion,

    pub fn toSExpr(self: *const @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        var node = SExpr.init(env.gpa, "unary");
        node.appendRegion(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        node.appendStringAttr(env.gpa, "op", ast.resolve(self.operator));

        var expr = ast.store.getExpr(self.expr).toSExpr(env, ast);
        node.appendNode(env.gpa, &expr);

        return node;
    }
};

/// Represents a delimited collection of other nodes
pub const Collection = struct {
    span: base.DataSpan,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
};

/// Represents a Roc file.
pub const File = struct {
    header: Header.Idx,
    statements: Statement.Span,
    region: TokenizedRegion,

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        var file_node = SExpr.init(env.gpa, "file");

        file_node.appendRegion(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));

        const header = ast.store.getHeader(self.header);
        var header_node = header.toSExpr(env, ast);

        file_node.appendNode(env.gpa, &header_node);

        var statements_node = SExpr.init(env.gpa, "statements");

        for (ast.store.statementSlice(self.statements)) |stmt_id| {
            const stmt = ast.store.getStatement(stmt_id);
            var stmt_node = stmt.toSExpr(env, ast);
            statements_node.appendNode(env.gpa, &stmt_node);
        }

        file_node.appendNode(env.gpa, &statements_node);

        return file_node;
    }
};

/// Represents a module header.
pub const Header = union(enum) {
    app: struct {
        provides: Collection.Idx,
        platform_idx: RecordField.Idx,
        packages: Collection.Idx,
        region: TokenizedRegion,
    },
    module: struct {
        exposes: Collection.Idx,
        region: TokenizedRegion,
    },
    package: struct {
        exposes: Collection.Idx,
        packages: Collection.Idx,
        region: TokenizedRegion,
    },
    platform: struct {
        // TODO: complete this
        name: Token.Idx,
        requires_rigids: Collection.Idx,
        requires_signatures: TypeAnno.Idx,
        exposes: Collection.Idx,
        packages: Collection.Idx,
        provides: Collection.Idx,
        region: TokenizedRegion,
    },
    hosted: struct {
        exposes: Collection.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };

    pub const AppHeaderRhs = packed struct { num_packages: u10, num_provides: u22 };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        switch (self) {
            .app => |a| {
                var node = SExpr.init(env.gpa, "app");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                // Provides
                const provides_coll = ast.store.getCollection(a.provides);
                const provides_items = ast.store.exposedItemSlice(.{ .span = provides_coll.span });
                var provides_node = SExpr.init(env.gpa, "provides");
                provides_node.appendRegion(env.gpa, ast.calcRegionInfo(provides_coll.region, env.line_starts.items));
                for (provides_items) |item_idx| {
                    const item = ast.store.getExposedItem(item_idx);
                    var item_node = item.toSExpr(env, ast);
                    provides_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &provides_node);
                // Platform
                const platform = ast.store.getRecordField(a.platform_idx);
                var platform_node = platform.toSExpr(env, ast);
                node.appendNode(env.gpa, &platform_node);
                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = SExpr.init(env.gpa, "packages");
                packages_node.appendRegion(env.gpa, ast.calcRegionInfo(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ast);
                    packages_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &packages_node);
                return node;
            },
            .module => |module| {
                var node = SExpr.init(env.gpa, "module");
                node.appendRegion(env.gpa, ast.calcRegionInfo(module.region, env.line_starts.items));
                const exposes = ast.store.getCollection(module.exposes);
                var exposes_node = SExpr.init(env.gpa, "exposes");
                exposes_node.appendRegion(env.gpa, ast.calcRegionInfo(exposes.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    exposes_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &exposes_node);
                return node;
            },
            .package => |a| {
                var node = SExpr.init(env.gpa, "package");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                // Exposes
                const exposes = ast.store.getCollection(a.exposes);
                var exposes_node = SExpr.init(env.gpa, "exposes");
                exposes_node.appendRegion(env.gpa, ast.calcRegionInfo(exposes.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    exposes_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &exposes_node);
                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = SExpr.init(env.gpa, "packages");
                packages_node.appendRegion(env.gpa, ast.calcRegionInfo(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ast);
                    packages_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &packages_node);
                return node;
            },
            .platform => |a| {
                var node = SExpr.init(env.gpa, "platform");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                // Name
                node.appendStringAttr(env.gpa, "name", ast.resolve(a.name));
                // Requires Rigids
                const rigids = ast.store.getCollection(a.requires_rigids);
                var rigids_node = SExpr.init(env.gpa, "rigids");
                rigids_node.appendRegion(env.gpa, ast.calcRegionInfo(rigids.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = rigids.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    rigids_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &rigids_node);
                // Requires Signatures
                const signatures = ast.store.getTypeAnno(a.requires_signatures);
                var signatures_node = signatures.toSExpr(env, ast);
                node.appendNode(env.gpa, &signatures_node);
                // Exposes
                const exposes = ast.store.getCollection(a.exposes);
                var exposes_node = SExpr.init(env.gpa, "exposes");
                exposes_node.appendRegion(env.gpa, ast.calcRegionInfo(exposes.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    exposes_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &exposes_node);
                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                var packages_node = SExpr.init(env.gpa, "packages");
                packages_node.appendRegion(env.gpa, ast.calcRegionInfo(packages_coll.region, env.line_starts.items));
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    var item_node = item.toSExpr(env, ast);
                    packages_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &packages_node);
                // Provides
                const provides = ast.store.getCollection(a.provides);
                var provides_node = SExpr.init(env.gpa, "provides");
                provides_node.appendRegion(env.gpa, ast.calcRegionInfo(provides.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = provides.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    provides_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &provides_node);
                return node;
            },
            .hosted => |a| {
                var node = SExpr.init(env.gpa, "hosted");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                const exposes = ast.store.getCollection(a.exposes);
                var exposes_node = SExpr.init(env.gpa, "exposes");
                exposes_node.appendRegion(env.gpa, ast.calcRegionInfo(exposes.region, env.line_starts.items));
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    var item_node = item.toSExpr(env, ast);
                    exposes_node.appendNode(env.gpa, &item_node);
                }
                node.appendNode(env.gpa, &exposes_node);
                return node;
            },
            .malformed => |a| {
                var node = SExpr.init(env.gpa, "malformed-header");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "tag", @tagName(a.reason));
                return node;
            },
        }
    }
};

/// TODO
pub const ExposedItem = union(enum) {
    lower_ident: struct {
        as: ?Token.Idx,
        ident: Token.Idx,
        region: TokenizedRegion,
    },
    upper_ident: struct {
        as: ?Token.Idx,
        ident: Token.Idx,
        region: TokenizedRegion,
    },
    upper_ident_star: struct {
        ident: Token.Idx,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        _ = env.line_starts.items;
        switch (self) {
            .lower_ident => |i| {
                var node = SExpr.init(env.gpa, "exposed-lower-ident");
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                node.appendStringAttr(env.gpa, "text", text);
                if (i.as) |a| {
                    const as_tok = ast.tokens.tokens.get(a);
                    const as_text = env.idents.getText(as_tok.extra.interned);
                    node.appendStringAttr(env.gpa, "as", as_text);
                }
                return node;
            },
            .upper_ident => |i| {
                var node = SExpr.init(env.gpa, "exposed-upper-ident");
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                node.appendStringAttr(env.gpa, "text", text);
                if (i.as) |a| {
                    const as_tok = ast.tokens.tokens.get(a);
                    const as_text = env.idents.getText(as_tok.extra.interned);
                    node.appendStringAttr(env.gpa, "as", as_text);
                }
                return node;
            },
            .upper_ident_star => |i| {
                var node = SExpr.init(env.gpa, "exposed-upper-ident-star");
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                node.appendStringAttr(env.gpa, "text", text);
                return node;
            },
        }
    }
};

/// TODO
pub const TypeHeader = struct {
    name: Token.Idx,
    args: TypeAnno.Span,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
};

/// TODO
pub const TypeAnno = union(enum) {
    apply: struct {
        args: TypeAnno.Span,
        region: TokenizedRegion,
    },
    ty_var: struct {
        tok: Token.Idx,
        region: TokenizedRegion,
    },
    underscore: struct {
        region: TokenizedRegion,
    },
    ty: struct {
        ident: base.Ident.Idx,
        // Region starts with the type token.
        region: TokenizedRegion,
    },
    mod_ty: struct {
        mod_ident: base.Ident.Idx,
        ty_ident: base.Ident.Idx,
        // Region starts with the mod token and ends with the type token.
        region: TokenizedRegion,
    },
    tag_union: struct {
        tags: TypeAnno.Span,
        open_anno: ?TypeAnno.Idx,
        region: TokenizedRegion,
    },
    tuple: struct {
        annos: TypeAnno.Span,
        region: TokenizedRegion,
    },
    record: struct {
        fields: AnnoRecordField.Span,
        region: TokenizedRegion,
    },
    @"fn": struct {
        args: TypeAnno.Span,
        ret: TypeAnno.Idx,
        effectful: bool,
        region: TokenizedRegion,
    },
    parens: struct {
        anno: TypeAnno.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub const TagUnionRhs = packed struct { open: u1, tags_len: u31 };
    pub const TypeAnnoFnRhs = packed struct { effectful: u1, args_len: u31 };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        switch (self) {
            // (apply <ty> [<args>])
            .apply => |a| {
                var node = SExpr.init(env.gpa, "ty-apply");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.typeAnnoSlice(a.args)) |b| {
                    var child = ast.store.getTypeAnno(b).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }

                return node;
            },
            // (ty_var <var>)
            .ty_var => |a| {
                var node = SExpr.init(env.gpa, "ty-var");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                node.appendStringAttr(env.gpa, "raw", ast.resolve(a.tok));
                return node;
            },
            // (_)
            .underscore => {
                return SExpr.init(env.gpa, "_");
            },
            .ty => |a| {
                var node = SExpr.init(env.gpa, "ty");
                node.appendStringAttr(env.gpa, "name", ast.resolve(a.region.start));
                return node;
            },
            .mod_ty => |a| {
                var node = SExpr.init(env.gpa, "ty-mod");

                const mod_text = env.idents.getText(a.mod_ident);
                node.appendStringAttr(env.gpa, "module", mod_text);

                const type_text = env.idents.getText(a.ty_ident);
                node.appendStringAttr(env.gpa, "name", type_text);

                return node;
            },
            .tag_union => |a| {
                var node = SExpr.init(env.gpa, "ty-tag-union");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                const tags = ast.store.typeAnnoSlice(a.tags);
                var tags_node = SExpr.init(env.gpa, "tags");
                for (tags) |tag_idx| {
                    const tag = ast.store.getTypeAnno(tag_idx);
                    var tag_node = tag.toSExpr(env, ast);
                    tags_node.appendNode(env.gpa, &tag_node);
                }
                node.appendNode(env.gpa, &tags_node);
                if (a.open_anno) |anno_idx| {
                    const anno = ast.store.getTypeAnno(anno_idx);
                    var anno_node = anno.toSExpr(env, ast);
                    node.appendNode(env.gpa, &anno_node);
                }
                return node;
            },
            // (tuple [<elems>])
            .tuple => |a| {
                var node = SExpr.init(env.gpa, "ty-tuple");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.typeAnnoSlice(a.annos)) |b| {
                    var child = ast.store.getTypeAnno(b).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }
                return node;
            },
            // (record [<fields>])
            .record => |a| {
                var node = SExpr.init(env.gpa, "ty-record");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.annoRecordFieldSlice(a.fields)) |f_idx| {
                    const field = ast.store.getAnnoRecordField(f_idx);
                    var field_node = field.toSExpr(env, ast);
                    node.appendNode(env.gpa, &field_node);
                }
                return node;
            },
            // (fn <ret> [<args>])
            .@"fn" => |a| {
                var node = SExpr.init(env.gpa, "ty-fn");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                // arguments
                for (ast.store.typeAnnoSlice(a.args)) |b| {
                    var child = ast.store.getTypeAnno(b).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }

                // return value
                var ret = ast.store.getTypeAnno(a.ret).toSExpr(env, ast);
                node.appendNode(env.gpa, &ret);

                return node;
            },
            // ignore parens... use inner
            .parens => |a| {
                return ast.store.getTypeAnno(a.anno).toSExpr(env, ast);
            },
            .malformed => |a| {
                var node = SExpr.init(env.gpa, "ty-malformed");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "tag", @tagName(a.reason));
                return node;
            },
        }
    }
};

/// TODO
pub const AnnoRecordField = struct {
    name: Token.Idx,
    ty: TypeAnno.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        var node = SExpr.init(env.gpa, "anno-record-field");
        node.appendRegion(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        node.appendStringAttr(env.gpa, "name", ast.resolve(self.name));
        const anno = ast.store.getTypeAnno(self.ty);
        var ty_node = anno.toSExpr(env, ast);
        node.appendNode(env.gpa, &ty_node);
        return node;
    }
};

/// The clause of a `where` constraint
///
/// e.g. `a.hash(hasher) -> hasher`
/// or   `a.Hash`
pub const WhereClause = union(enum) {
    alias: struct {
        var_tok: Token.Idx,
        alias_tok: Token.Idx,
        region: TokenizedRegion,
    },
    method: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        args: Collection.Idx,
        ret_anno: TypeAnno.Idx,
        region: TokenizedRegion,
    },
    mod_method: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        args: Collection.Idx,
        ret_anno: TypeAnno.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// Represents an expression.
pub const Expr = union(enum) {
    int: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },
    frac: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },

    string_part: struct { // TODO: this should be more properly represented in its own union enum
        token: Token.Idx,
        region: TokenizedRegion,
    },
    string: struct {
        token: Token.Idx,
        region: TokenizedRegion,
        parts: Expr.Span,
    },
    list: struct {
        items: Expr.Span,
        region: TokenizedRegion,
    },
    tuple: struct {
        items: Expr.Span,
        region: TokenizedRegion,
    },
    record: struct {
        fields: RecordField.Span,
        region: TokenizedRegion,
    },
    tag: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },
    lambda: struct {
        args: Pattern.Span,
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    apply: struct {
        args: Expr.Span,
        @"fn": Expr.Idx,
        region: TokenizedRegion,
    },
    record_updater: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },
    field_access: BinOp,
    local_dispatch: BinOp,
    bin_op: BinOp,
    suffix_single_question: Unary,
    unary_op: Unary,
    if_then_else: struct {
        condition: Expr.Idx,
        then: Expr.Idx,
        @"else": Expr.Idx,
        region: TokenizedRegion,
    },
    match: struct {
        expr: Expr.Idx,
        branches: WhenBranch.Span,
        region: TokenizedRegion,
    },
    ident: struct {
        token: Token.Idx,
        qualifier: ?Token.Idx,
        region: TokenizedRegion,
    },
    dbg: struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    record_builder: struct {
        mapper: Expr.Idx,
        fields: RecordField.Idx,
        region: TokenizedRegion,
    },
    ellipsis: struct {
        region: TokenizedRegion,
    },
    block: Body,
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn as_string_part_region(self: @This()) !TokenizedRegion {
        switch (self) {
            .string_part => |part| return part.region,
            else => return error.ExpectedStringPartRegion,
        }
    }

    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .ident => |e| e.region,
            .int => |e| e.region,
            .frac => |e| e.region,
            .string => |e| e.region,
            .tag => |e| e.region,
            .list => |e| e.region,
            .record => |e| e.region,
            .tuple => |e| e.region,
            .field_access => |e| e.region,
            .local_dispatch => |e| e.region,
            .lambda => |e| e.region,
            .record_updater => |e| e.region,
            .bin_op => |e| e.region,
            .unary_op => |e| e.region,
            .suffix_single_question => |e| e.region,
            .apply => |e| e.region,
            .if_then_else => |e| e.region,
            .match => |e| e.region,
            .dbg => |e| e.region,
            .block => |e| e.region,
            .record_builder => |e| e.region,
            .ellipsis => |e| e.region,
            .malformed => |e| e.region,
            .string_part => |e| e.region,
        };
    }

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        switch (self) {
            .int => |int| {
                var node = SExpr.init(env.gpa, "e-int");
                node.appendRegion(env.gpa, ast.calcRegionInfo(int.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "raw", ast.resolve(int.token));
                return node;
            },
            .string => |str| {
                var node = SExpr.init(env.gpa, "e-string");
                node.appendRegion(env.gpa, ast.calcRegionInfo(str.region, env.line_starts.items));
                for (ast.store.exprSlice(str.parts)) |part_id| {
                    const part_expr = ast.store.getExpr(part_id);
                    var part_sexpr = part_expr.toSExpr(env, ast);
                    node.appendNode(env.gpa, &part_sexpr);
                }
                return node;
            },
            .string_part => |sp| {
                var node = SExpr.init(env.gpa, "e-string-part");
                node.appendRegion(env.gpa, ast.calcRegionInfo(sp.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "raw", ast.resolve(sp.token));
                return node;
            },
            // (tag <tag>)
            .tag => |tag| {
                var node = SExpr.init(env.gpa, "e-tag");

                node.appendRegion(env.gpa, ast.calcRegionInfo(tag.region, env.line_starts.items));

                node.appendStringAttr(env.gpa, "raw", ast.resolve(tag.token));
                return node;
            },
            .block => |block| {
                return block.toSExpr(env, ast);
            },
            .if_then_else => |stmt| {
                var node = SExpr.init(env.gpa, "e-if-then-else");

                node.appendRegion(env.gpa, ast.calcRegionInfo(stmt.region, env.line_starts.items));

                var condition = ast.store.getExpr(stmt.condition).toSExpr(env, ast);
                node.appendNode(env.gpa, &condition);

                var then = ast.store.getExpr(stmt.then).toSExpr(env, ast);
                node.appendNode(env.gpa, &then);

                var else_ = ast.store.getExpr(stmt.@"else").toSExpr(env, ast);

                node.appendNode(env.gpa, &else_);

                return node;
            },
            .ident => |ident| {
                var node = SExpr.init(env.gpa, "e-ident");

                node.appendRegion(env.gpa, ast.calcRegionInfo(ident.region, env.line_starts.items));

                node.appendStringAttr(env.gpa, "qaul", if (ident.qualifier != null) ast.resolve(ident.qualifier.?) else "");

                node.appendStringAttr(env.gpa, "raw", ast.resolve(ident.token));
                return node;
            },
            .list => |a| {
                var node = SExpr.init(env.gpa, "e-list");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.exprSlice(a.items)) |b| {
                    var child = ast.store.getExpr(b).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }
                return node;
            },
            .malformed => |a| {
                var node = SExpr.init(env.gpa, "e-malformed");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));
                node.appendStringAttr(env.gpa, "reason", @tagName(a.reason));
                return node;
            },
            .frac => |a| {
                var node = SExpr.init(env.gpa, "e-frac");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                node.appendStringAttr(env.gpa, "raw", ast.resolve(a.token));
                return node;
            },
            .tuple => |a| {
                var node = SExpr.init(env.gpa, "e-tuple");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.exprSlice(a.items)) |item| {
                    var child = ast.store.getExpr(item).toSExpr(env, ast);
                    node.appendNode(env.gpa, &child);
                }

                return node;
            },
            .record => |a| {
                var node = SExpr.init(env.gpa, "e-record");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                for (ast.store.recordFieldSlice(a.fields)) |field_idx| {
                    const record_field = ast.store.getRecordField(field_idx);
                    var record_field_node = SExpr.init(env.gpa, "field");
                    record_field_node.appendStringAttr(env.gpa, "field", ast.resolve(record_field.name));
                    if (record_field.value != null) {
                        var value_node = ast.store.getExpr(record_field.value.?).toSExpr(env, ast);
                        record_field_node.appendNode(env.gpa, &value_node);
                    }
                    record_field_node.appendBoolAttr(env.gpa, "optional", record_field.optional);
                    node.appendNode(env.gpa, &record_field_node);
                }

                return node;
            },
            .apply => |a| {
                var node = SExpr.init(env.gpa, "e-apply");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var apply_fn = ast.store.getExpr(a.@"fn").toSExpr(env, ast);
                node.appendNode(env.gpa, &apply_fn);

                for (ast.store.exprSlice(a.args)) |arg| {
                    var arg_node = ast.store.getExpr(arg).toSExpr(env, ast);
                    node.appendNode(env.gpa, &arg_node);
                }

                return node;
            },
            .field_access => |a| {
                var node = SExpr.init(env.gpa, "e-field-access");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = a.toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            .local_dispatch => |a| {
                var node = SExpr.init(env.gpa, "e-local-dispatch");
                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var left = ast.store.getExpr(a.left).toSExpr(env, ast);
                var right = ast.store.getExpr(a.right).toSExpr(env, ast);
                node.appendNode(env.gpa, &left);
                node.appendNode(env.gpa, &right);
                return node;
            },
            .bin_op => |a| {
                return a.toSExpr(env, ast);
            },
            .lambda => |a| {
                var node = SExpr.init(env.gpa, "e-lambda");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                // arguments
                var args = SExpr.init(env.gpa, "args");
                for (ast.store.patternSlice(a.args)) |arg| {
                    var arg_node = ast.store.getPattern(arg).toSExpr(env, ast);
                    args.appendNode(env.gpa, &arg_node);
                }
                node.appendNode(env.gpa, &args);

                // body
                var body = ast.store.getExpr(a.body).toSExpr(env, ast);
                node.appendNode(env.gpa, &body);

                return node;
            },
            .dbg => |a| {
                var node = SExpr.init(env.gpa, "e-dbg");

                var arg = ast.store.getExpr(a.expr).toSExpr(env, ast);
                node.appendNode(env.gpa, &arg);

                return node;
            },
            .match => |a| {
                var node = SExpr.init(env.gpa, "e-match");

                var expr = ast.store.getExpr(a.expr).toSExpr(env, ast);

                // handle branches
                var branches = SExpr.init(env.gpa, "branches");
                for (ast.store.whenBranchSlice(a.branches)) |branch| {
                    var branch_node = ast.store.getBranch(branch).toSExpr(env, ast);
                    branches.appendNode(env.gpa, &branch_node);
                }

                node.appendNode(env.gpa, &expr);

                node.appendNode(env.gpa, &branches);

                return node;
            },
            .ellipsis => {
                return SExpr.init(env.gpa, "e-ellipsis");
            },
            .suffix_single_question => |a| {
                var node = SExpr.init(env.gpa, "e-question-suffix");

                node.appendRegion(env.gpa, ast.calcRegionInfo(a.region, env.line_starts.items));

                var child = ast.store.getExpr(a.expr).toSExpr(env, ast);
                node.appendNode(env.gpa, &child);
                return node;
            },
            else => {
                std.debug.print("\n\n toSExpr not implement for Expr {}\n\n", .{self});
                @panic("not implemented yet");
            },
        }
    }
};

/// TODO
pub const PatternRecordField = struct {
    name: Token.Idx,
    value: ?Pattern.Idx,
    rest: bool,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// TODO
pub const RecordField = struct {
    name: Token.Idx,
    value: ?Expr.Idx,
    optional: bool,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        var node = SExpr.init(env.gpa, "record-field");
        node.appendRegion(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        node.appendStringAttr(env.gpa, "name", ast.resolve(self.name));
        if (self.value) |idx| {
            const value = ast.store.getExpr(idx);
            var value_node = value.toSExpr(env, ast);
            node.appendNode(env.gpa, &value_node);
        }
        return node;
    }
};

/// TODO
pub const IfElse = struct {
    condition: Expr.Idx,
    body: Expr.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// TODO
pub const WhenBranch = struct {
    pattern: Pattern.Idx,
    body: Expr.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ast: *AST) SExpr {
        var node = SExpr.init(env.gpa, "branch");
        node.appendRegion(env.gpa, ast.calcRegionInfo(self.region, env.line_starts.items));
        var pattern = ast.store.getPattern(self.pattern).toSExpr(env, ast);
        node.appendNode(env.gpa, &pattern);
        var body = ast.store.getExpr(self.body).toSExpr(env, ast);
        node.appendNode(env.gpa, &body);
        return node;
    }
};
