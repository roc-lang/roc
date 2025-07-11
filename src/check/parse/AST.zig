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
const SExprTree = base.SExprTree;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

const AST = @This();

env: *base.ModuleEnv,
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
    const info = base.RegionInfo.position(self.env.source, line_starts, start.start.offset, end.end.offset) catch {
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

/// Append region information to an S-expression node for diagnostics
pub fn appendRegionInfoToSexprTree(self: *AST, env: *base.ModuleEnv, tree: *SExprTree, region: TokenizedRegion) void {
    const start = self.tokens.resolve(region.start);
    const end = self.tokens.resolve(region.end);
    const info: base.RegionInfo = base.RegionInfo.position(self.env.source, env.line_starts.items.items, start.start.offset, end.end.offset) catch .{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
        .line_text = "",
    };
    tree.pushBytesRange(start.start.offset, end.end.offset, info);
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
        .InvalidUtf8InSource => "INVALID UTF-8",
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
        .InvalidUtf8InSource => "Invalid UTF-8 encoding found in source code. Roc source files must be valid UTF-8.",
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
    return self.env.source[token_region.start.offset..token_region.end.offset];
}

/// Convert a parse diagnostic to a Report for rendering
pub fn parseDiagnosticToReport(self: *AST, env: *base.ModuleEnv, diagnostic: Diagnostic, allocator: std.mem.Allocator, filename: []const u8) !reporting.Report {
    const raw_region = self.tokenizedRegionToRegion(diagnostic.region);

    // Ensure region bounds are valid for source slicing
    const region = base.Region{
        .start = .{ .offset = @min(raw_region.start.offset, self.env.source.len) },
        .end = .{ .offset = @min(@max(raw_region.end.offset, raw_region.start.offset), self.env.source.len) },
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
        .pattern_list_rest_old_syntax => "BAD LIST REST PATTERN SYNTAX",
        .pattern_unexpected_eof => "UNEXPECTED END OF FILE IN PATTERN",
        .ty_anno_unexpected_token => "UNEXPECTED TOKEN IN TYPE ANNOTATION",
        .statement_unexpected_eof => "UNEXPECTED END OF FILE",
        .statement_unexpected_token => "UNEXPECTED TOKEN",
        .string_unexpected_token => "UNEXPECTED TOKEN IN STRING",
        .expr_unexpected_token => "UNEXPECTED TOKEN IN EXPRESSION",
        .import_must_be_top_level => "IMPORT MUST BE TOP LEVEL",
        .expected_expr_close_square_or_comma => "LIST NOT CLOSED",
        .where_expected_where => "WHERE CLAUSE ERROR",
        .where_expected_mod_open => "WHERE CLAUSE ERROR",
        .where_expected_var => "WHERE CLAUSE ERROR",
        .where_expected_mod_close => "WHERE CLAUSE ERROR",
        .where_expected_arg_open => "WHERE CLAUSE ERROR",
        .where_expected_arg_close => "WHERE CLAUSE ERROR",
        .where_expected_method_arrow => "WHERE CLAUSE ERROR",
        .where_expected_method_or_alias_name => "WHERE CLAUSE ERROR",
        .where_expected_module => "WHERE CLAUSE ERROR",
        .where_expected_colon => "WHERE CLAUSE ERROR",
        .where_expected_constraints => "WHERE CLAUSE ERROR",
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
                self.env.source[region.start.offset..region.end.offset]
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
                self.env.source[region.start.offset..region.end.offset]
            else
                "<unknown>";
            const owned_token = try report.addOwnedString(token_text);
            try report.document.addText("The token ");
            try report.document.addAnnotated(owned_token, .error_highlight);
            try report.document.addText(" is not expected in a pattern.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Patterns can contain identifiers, literals, lists, records, or tags.");
        },
        .pattern_list_rest_old_syntax => {
            try report.document.addReflowingText("List rest patterns should use the `.. as name` syntax, not `..name`.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("For example, use `[first, .. as rest]` instead of `[first, ..rest]`.");
        },
        .pattern_unexpected_eof => {
            try report.document.addReflowingText("This pattern is incomplete - the file ended unexpectedly.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Complete the pattern or remove the incomplete pattern.");
        },
        .ty_anno_unexpected_token => {
            const token_text = if (diagnostic.region.start != diagnostic.region.end)
                self.env.source[region.start.offset..region.end.offset]
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
                self.env.source[region.start.offset..region.end.offset]
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
                self.env.source[region.start.offset..region.end.offset]
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
                self.env.source[region.start.offset..region.end.offset]
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
        .where_expected_where => {
            try report.document.addReflowingText("Expected a ");
            try report.document.addKeyword("where");
            try report.document.addText(" clause here.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Where clauses define constraints on type variables.");
        },
        .where_expected_mod_open => {
            try report.document.addReflowingText("Expected an opening parenthesis after ");
            try report.document.addKeyword("module");
            try report.document.addText(" in this where clause.");
            try report.document.addLineBreak();
            try report.document.addText("Module constraints should look like: ");
            try report.document.addCodeBlock("module(a).method : Type");
        },
        .where_expected_var => {
            try report.document.addReflowingText("Expected a type variable name here.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Type variables are lowercase identifiers that represent types.");
        },
        .where_expected_mod_close => {
            try report.document.addReflowingText("Expected a closing parenthesis after the type variable in this module constraint.");
            try report.document.addLineBreak();
            try report.document.addText("Module constraints should look like: ");
            try report.document.addCodeBlock("module(a).method : Type");
        },
        .where_expected_arg_open => {
            try report.document.addReflowingText("Expected an opening parenthesis for the method arguments.");
            try report.document.addLineBreak();
            try report.document.addText("Method constraints should look like: ");
            try report.document.addCodeBlock("module(a).method : args -> ret");
        },
        .where_expected_arg_close => {
            try report.document.addReflowingText("Expected a closing parenthesis after the method arguments.");
            try report.document.addLineBreak();
            try report.document.addText("Method constraints should look like: ");
            try report.document.addCodeBlock("module(a).method : args -> ret");
        },
        .where_expected_method_arrow => {
            try report.document.addReflowingText("Expected an arrow ");
            try report.document.addAnnotated("->", .emphasized);
            try report.document.addText(" after the method arguments.");
            try report.document.addLineBreak();
            try report.document.addText("Method constraints should look like: ");
            try report.document.addCodeBlock("module(a).method : args -> ret");
        },
        .where_expected_method_or_alias_name => {
            try report.document.addReflowingText("Expected a method name or type alias after the dot.");
            try report.document.addLineBreak();
            try report.document.addText("Where clauses can contain:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addText("• Method constraints: ");
            try report.document.addCodeBlock("module(a).method : args -> ret");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addText("• Type aliases: ");
            try report.document.addCodeBlock("module(a).SomeTypeAlias");
        },
        .where_expected_module => {
            try report.document.addReflowingText("Expected ");
            try report.document.addKeyword("module");
            try report.document.addText(" at the start of this where clause constraint.");
            try report.document.addLineBreak();
            try report.document.addText("Where clauses can contain:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addText("• Method constraints: ");
            try report.document.addCodeBlock("module(a).method : Type");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addText("• Type aliases: ");
            try report.document.addCodeBlock("module(a).SomeType");
        },
        .where_expected_colon => {
            try report.document.addReflowingText("Expected a colon ");
            try report.document.addAnnotated(":", .emphasized);
            try report.document.addText(" after the method name in this where clause constraint.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Method constraints require a colon to separate the method name from its type.");
            try report.document.addLineBreak();
            try report.document.addText("For example: ");
            try report.document.addCodeBlock("module(a).method : a -> b");
        },
        .where_expected_constraints => {
            try report.document.addReflowingText("A ");
            try report.document.addKeyword("where");
            try report.document.addText(" clause cannot be empty.");
            try report.document.addLineBreak();
            try report.document.addReflowingText("Where clauses must contain at least one constraint.");
            try report.document.addLineBreak();
            try report.document.addText("For example:");
            try report.document.addLineBreak();
            try report.document.addIndent(1);
            try report.document.addCodeBlock("module(a).method : a -> b");
        },
        .match_branch_wrong_arrow => {
            try report.document.addReflowingText("Match branches use `=>` instead of `->`.");
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
    if (region.start.offset <= region.end.offset and region.end.offset <= self.env.source.len) {
        // Use proper region info calculation with converted region
        const region_info = base.RegionInfo.position(self.env.source, env.line_starts.items.items, region.start.offset, region.end.offset) catch {
            return report; // Return report without source context if region calculation fails
        };

        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("Here is the problematic code:");
        try report.document.addLineBreak();

        // Use the proper addSourceContext method with owned filename
        const owned_filename = try report.addOwnedString(filename);
        try report.addSourceContext(region_info, owned_filename);
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
        pattern_list_rest_old_syntax,
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
        where_expected_module,
        where_expected_colon,
        where_expected_constraints,
        import_must_be_top_level,
        invalid_type_arg,
        expr_arrow_expects_ident,
        var_only_allowed_in_a_body,
        var_must_have_ident,
        var_expected_equals,
        for_expected_in,
        match_branch_wrong_arrow,
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
    return self.env.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
}

/// Resolves a fully qualified name from a chain of qualifier tokens and a final token.
/// If there are qualifiers, returns a slice from the first qualifier to the final token.
/// Otherwise, returns the final token text with any leading dot stripped based on the token type.
pub fn resolveQualifiedName(
    self: *AST,
    qualifiers: Token.Span,
    final_token: Token.Idx,
    strip_dot_from_tokens: []const Token.Tag,
) []const u8 {
    const qualifier_tokens = self.store.tokenSlice(qualifiers);

    if (qualifier_tokens.len > 0) {
        // Get the region of the first qualifier token
        const first_qualifier_tok = @as(Token.Idx, @intCast(qualifier_tokens[0]));
        const first_region = self.tokens.resolve(first_qualifier_tok);

        // Get the region of the final token
        const final_region = self.tokens.resolve(final_token);

        // Slice from the start of the first qualifier to the end of the final token
        const start_offset = first_region.start.offset;
        const end_offset = final_region.end.offset;

        return self.env.source[@intCast(start_offset)..@intCast(end_offset)];
    } else {
        // Get the raw token text and strip leading dot if it's one of the specified tokens
        const raw_text = self.resolve(final_token);
        const token_tag = self.tokens.tokens.items(.tag)[@intCast(final_token)];

        for (strip_dot_from_tokens) |dot_token_tag| {
            if (token_tag == dot_token_tag and raw_text.len > 0 and raw_text[0] == '.') {
                return raw_text[1..];
            }
        }

        return raw_text;
    }
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

    var tree = SExprTree.init(env.gpa);
    defer tree.deinit();

    file.pushToSExprTree(env, ast, &tree);

    tree.toStringPretty(writer);
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
    dbg: struct {
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

    /// Push this Statement to the SExprTree stack
    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        switch (self) {
            .decl => |decl| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-decl");
                ast.appendRegionInfoToSexprTree(env, tree, decl.region);
                const attrs = tree.beginNode();

                // pattern
                ast.store.getPattern(decl.pattern).pushToSExprTree(env, ast, tree);

                // body
                ast.store.getExpr(decl.body).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .@"var" => |v| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-var");
                ast.appendRegionInfoToSexprTree(env, tree, v.region);

                const name_str = ast.resolve(v.name);
                tree.pushStringPair("name", name_str);
                const attrs = tree.beginNode();

                ast.store.getExpr(v.body).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .expr => |expr| {
                ast.store.getExpr(expr.expr).pushToSExprTree(env, ast, tree);
            },
            .import => |import| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-import");
                ast.appendRegionInfoToSexprTree(env, tree, import.region);

                // Reconstruct full qualified module name
                const module_name_raw = ast.resolve(import.module_name_tok);
                if (import.qualifier_tok) |tok| {
                    const qualifier_str = ast.resolve(tok);
                    // Strip leading dot from module name if present
                    const module_name_clean = if (module_name_raw.len > 0 and module_name_raw[0] == '.')
                        module_name_raw[1..]
                    else
                        module_name_raw;

                    // Combine qualifier and module name
                    const full_module_name = std.fmt.allocPrint(env.gpa, "{s}.{s}", .{ qualifier_str, module_name_clean }) catch |err| exitOnOom(err);
                    defer env.gpa.free(full_module_name);
                    tree.pushStringPair("raw", full_module_name);
                } else {
                    tree.pushStringPair("raw", module_name_raw);
                }

                // alias e.g. `OUT` in `import pf.Stdout as OUT`
                if (import.alias_tok) |tok| {
                    const alias_str = ast.resolve(tok);
                    tree.pushStringPair("alias", alias_str);
                }

                const attrs = tree.beginNode();

                // exposed identifiers e.g. [foo, bar] in `import pf.Stdout exposing [foo, bar]`
                const exposed_slice = ast.store.exposedItemSlice(import.exposes);
                if (exposed_slice.len > 0) {
                    const exposed = tree.beginNode();
                    tree.pushStaticAtom("exposing");
                    const attrs2 = tree.beginNode();
                    for (ast.store.exposedItemSlice(import.exposes)) |e| {
                        ast.store.getExposedItem(e).pushToSExprTree(env, ast, tree);
                    }
                    tree.endNode(exposed, attrs2);
                }
                tree.endNode(begin, attrs);
            },
            .type_decl => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-type-decl");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // pattern
                {
                    const header = tree.beginNode();
                    tree.pushStaticAtom("header");
                    // Check if the type header node is malformed before calling getTypeHeader
                    const header_node = ast.store.nodes.get(@enumFromInt(@intFromEnum(a.header)));
                    if (header_node.tag == .malformed) {
                        // Handle malformed type header by creating a placeholder
                        ast.appendRegionInfoToSexprTree(env, tree, header_node.region);
                        tree.pushStringPair("name", "<malformed>");
                        const attrs2 = tree.beginNode();
                        const args_begin = tree.beginNode();
                        tree.pushStaticAtom("args");
                        const args_attrs = tree.beginNode();
                        tree.endNode(args_begin, args_attrs);
                        tree.endNode(header, attrs2);
                    } else {
                        const ty_header = ast.store.getTypeHeader(a.header);
                        ast.appendRegionInfoToSexprTree(env, tree, ty_header.region);
                        tree.pushStringPair("name", ast.resolve(ty_header.name));
                        const attrs2 = tree.beginNode();

                        const args_begin = tree.beginNode();
                        tree.pushStaticAtom("args");
                        const args_node = tree.beginNode();

                        for (ast.store.typeAnnoSlice(ty_header.args)) |b| {
                            const anno = ast.store.getTypeAnno(b);
                            anno.pushToSExprTree(env, ast, tree);
                        }
                        tree.endNode(args_begin, args_node);
                        tree.endNode(header, attrs2);
                    }
                }

                ast.store.getTypeAnno(a.anno).pushToSExprTree(env, ast, tree);

                if (a.where) |where_coll| {
                    const where_node = tree.beginNode();
                    tree.pushStaticAtom("where");
                    const attrs2 = tree.beginNode();
                    for (ast.store.whereClauseSlice(.{ .span = ast.store.getCollection(where_coll).span })) |clause_idx| {
                        const clause_child = ast.store.getWhereClause(clause_idx);
                        clause_child.pushToSExprTree(env, ast, tree);
                    }
                    tree.endNode(where_node, attrs2);
                }

                tree.endNode(begin, attrs);
            },
            .crash => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-crash");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                ast.store.getExpr(a.expr).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .dbg => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-dbg");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                ast.store.getExpr(a.expr).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .expect => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-expect");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                ast.store.getExpr(a.body).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .@"for" => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-for");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // pattern
                ast.store.getPattern(a.patt).pushToSExprTree(env, ast, tree);

                // expr
                ast.store.getExpr(a.expr).pushToSExprTree(env, ast, tree);

                // body
                ast.store.getExpr(a.body).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .@"return" => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-return");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                ast.store.getExpr(a.expr).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .type_anno => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-type-anno");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("name", ast.resolve(a.name));
                const attrs = tree.beginNode();

                ast.store.getTypeAnno(a.anno).pushToSExprTree(env, ast, tree);

                if (a.where) |where_coll| {
                    const where_node = tree.beginNode();
                    tree.pushStaticAtom("where");
                    const attrs2 = tree.beginNode();
                    for (ast.store.whereClauseSlice(.{ .span = ast.store.getCollection(where_coll).span })) |clause_idx| {
                        const clause_child = ast.store.getWhereClause(clause_idx);
                        clause_child.pushToSExprTree(env, ast, tree);
                    }
                    tree.endNode(where_node, attrs2);
                }
                tree.endNode(begin, attrs);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("s-malformed");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("tag", @tagName(a.reason));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
        }
    }
};

/// Represents a Body, or a block of statements.
pub const Body = struct {
    /// The statements that constitute the block
    statements: Statement.Span,
    region: TokenizedRegion,

    /// Push this Body to the SExprTree stack
    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("e-block");
        ast.appendRegionInfoToSexprTree(env, tree, self.region);
        const attrs = tree.beginNode();

        const statements = tree.beginNode();
        tree.pushStaticAtom("statements");
        const attrs2 = tree.beginNode();
        // Push all statements
        for (ast.store.statementSlice(self.statements)) |stmt_idx| {
            const stmt = ast.store.getStatement(stmt_idx);
            stmt.pushToSExprTree(env, ast, tree);
        }
        tree.endNode(statements, attrs2);

        tree.endNode(begin, attrs);
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
    single_quote: struct {
        token: Token.Idx,
        region: TokenizedRegion,
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
            .single_quote => |p| p.region,
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

    /// Push this Pattern to the SExprTree stack
    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        switch (self) {
            .ident => |ident| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-ident");
                ast.appendRegionInfoToSexprTree(env, tree, ident.region);

                // Add raw attribute
                const raw_begin = tree.beginNode();
                tree.pushStaticAtom("raw");
                tree.pushString(ast.resolve(ident.ident_tok));
                const attrs2 = tree.beginNode();
                tree.endNode(raw_begin, attrs2);
                const attrs = tree.beginNode();

                tree.endNode(begin, attrs);
            },
            .tag => |tag| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-tag");
                ast.appendRegionInfoToSexprTree(env, tree, tag.region);
                tree.pushStringPair("raw", ast.resolve(tag.tag_tok));
                const attrs = tree.beginNode();

                // Add arguments if there are any
                for (ast.store.patternSlice(tag.args)) |arg| {
                    ast.store.getPattern(arg).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .int => |num| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-int");
                ast.appendRegionInfoToSexprTree(env, tree, num.region);
                tree.pushStringPair("raw", ast.resolve(num.number_tok));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .frac => |num| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-frac");
                ast.appendRegionInfoToSexprTree(env, tree, num.region);
                tree.pushStringPair("raw", ast.resolve(num.number_tok));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .string => |str| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-string");
                ast.appendRegionInfoToSexprTree(env, tree, str.region);
                tree.pushStringPair("raw", ast.resolve(str.string_tok));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .single_quote => |sq| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-single-quote");
                ast.appendRegionInfoToSexprTree(env, tree, sq.region);
                tree.pushStringPair("raw", ast.resolve(sq.token));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .record => |rec| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-record");
                ast.appendRegionInfoToSexprTree(env, tree, rec.region);
                const attrs = tree.beginNode();

                for (ast.store.patternRecordFieldSlice(rec.fields)) |field_idx| {
                    const field = ast.store.getPatternRecordField(field_idx);
                    const field_begin = tree.beginNode();
                    tree.pushStaticAtom("field");
                    ast.appendRegionInfoToSexprTree(env, tree, field.region);
                    tree.pushStringPair("name", ast.resolve(field.name));
                    tree.pushBoolPair("rest", field.rest);
                    const attrs2 = tree.beginNode();

                    if (field.value) |value| {
                        ast.store.getPattern(value).pushToSExprTree(env, ast, tree);
                    }

                    tree.endNode(field_begin, attrs2);
                }

                tree.endNode(begin, attrs);
            },
            .list => |list| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-list");
                ast.appendRegionInfoToSexprTree(env, tree, list.region);
                const attrs = tree.beginNode();

                for (ast.store.patternSlice(list.patterns)) |pat| {
                    ast.store.getPattern(pat).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .list_rest => |rest| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-list-rest");
                ast.appendRegionInfoToSexprTree(env, tree, rest.region);

                if (rest.name) |name_tok| {
                    tree.pushStringPair("name", ast.resolve(name_tok));
                }
                const attrs = tree.beginNode();

                tree.endNode(begin, attrs);
            },
            .tuple => |tuple| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-tuple");
                ast.appendRegionInfoToSexprTree(env, tree, tuple.region);
                const attrs = tree.beginNode();

                for (ast.store.patternSlice(tuple.patterns)) |pat| {
                    ast.store.getPattern(pat).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .underscore => {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-underscore");
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .alternatives => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-alternatives");
                const attrs = tree.beginNode();

                for (ast.store.patternSlice(a.patterns)) |pat| {
                    ast.store.getPattern(pat).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .as => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-as");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("name", ast.resolve(a.name));
                const attrs = tree.beginNode();

                ast.store.getPattern(a.pattern).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("p-malformed");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("tag", @tagName(a.reason));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
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
    pub fn pushToSExprTree(self: *const @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        const begin = tree.beginNode();

        // Push the node name
        tree.pushStaticAtom("e-binop");
        ast.appendRegionInfoToSexprTree(env, tree, self.region);

        // Push the operator as an attribute-style pair
        const op_begin = tree.beginNode();
        tree.pushStaticAtom("op");
        tree.pushString(ast.resolve(self.operator));
        const attrs2 = tree.beginNode();
        tree.endNode(op_begin, attrs2);
        const attrs = tree.beginNode();

        // Push left operand
        ast.store.getExpr(self.left).pushToSExprTree(env, ast, tree);

        // Push right operand
        ast.store.getExpr(self.right).pushToSExprTree(env, ast, tree);

        tree.endNode(begin, attrs);
    }
};

/// TODO
pub const Unary = struct {
    operator: Token.Idx,
    expr: Expr.Idx,
    region: TokenizedRegion,

    /// Push this Unary to the SExprTree stack
    pub fn pushToSExprTree(self: *const @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("unary");
        tree.pushString(ast.resolve(self.operator));
        const attrs = tree.beginNode();

        ast.store.getExpr(self.expr).pushToSExprTree(env, ast, tree);

        tree.endNode(begin, attrs);
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

    /// Push this File to the SExprTree stack
    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("file");
        ast.appendRegionInfoToSexprTree(env, tree, self.region);
        const attrs = tree.beginNode();

        // Push header
        const header = ast.store.getHeader(self.header);
        header.pushToSExprTree(env, ast, tree);

        const begin2 = tree.beginNode();
        tree.pushStaticAtom("statements");
        const attrs2 = tree.beginNode();
        for (ast.store.statementSlice(self.statements)) |stmt_id| {
            const stmt = ast.store.getStatement(stmt_id);
            stmt.pushToSExprTree(env, ast, tree);
        }
        tree.endNode(begin2, attrs2);

        tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        switch (self) {
            .app => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("app");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Provides
                const provides_coll = ast.store.getCollection(a.provides);
                const provides_items = ast.store.exposedItemSlice(.{ .span = provides_coll.span });
                const provides_begin = tree.beginNode();
                tree.pushStaticAtom("provides");
                ast.appendRegionInfoToSexprTree(env, tree, provides_coll.region);
                const attrs2 = tree.beginNode();
                // Could push region info for provides_coll here if desired
                for (provides_items) |item_idx| {
                    const item = ast.store.getExposedItem(item_idx);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(provides_begin, attrs2);

                // Platform
                const platform = ast.store.getRecordField(a.platform_idx);
                platform.pushToSExprTree(env, ast, tree);

                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                const packages_begin = tree.beginNode();
                tree.pushStaticAtom("packages");
                ast.appendRegionInfoToSexprTree(env, tree, packages_coll.region);
                const attrs3 = tree.beginNode();
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(packages_begin, attrs3);

                tree.endNode(begin, attrs);
            },
            .module => |module| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("module");
                ast.appendRegionInfoToSexprTree(env, tree, module.region);
                const attrs = tree.beginNode();

                const exposes = ast.store.getCollection(module.exposes);
                const exposes_begin = tree.beginNode();
                tree.pushStaticAtom("exposes");
                ast.appendRegionInfoToSexprTree(env, tree, exposes.region);
                const attrs2 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(exposes_begin, attrs2);

                tree.endNode(begin, attrs);
            },
            .package => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("package");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Exposes
                const exposes = ast.store.getCollection(a.exposes);
                const exposes_begin = tree.beginNode();
                tree.pushStaticAtom("exposes");
                ast.appendRegionInfoToSexprTree(env, tree, exposes.region);
                const attrs2 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(exposes_begin, attrs2);

                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                const packages_begin = tree.beginNode();
                tree.pushStaticAtom("packages");
                ast.appendRegionInfoToSexprTree(env, tree, packages_coll.region);
                const attrs3 = tree.beginNode();
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(packages_begin, attrs3);

                tree.endNode(begin, attrs);
            },
            .platform => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("platform");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("name", ast.resolve(a.name));
                const attrs = tree.beginNode();

                // Requires Rigids
                const rigids = ast.store.getCollection(a.requires_rigids);
                const rigids_begin = tree.beginNode();
                tree.pushStaticAtom("rigids");
                ast.appendRegionInfoToSexprTree(env, tree, rigids.region);
                const attrs3 = tree.beginNode();
                // Could push region info for rigids here if desired
                for (ast.store.exposedItemSlice(.{ .span = rigids.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(rigids_begin, attrs3);

                // Requires Signatures
                const signatures = ast.store.getTypeAnno(a.requires_signatures);
                signatures.pushToSExprTree(env, ast, tree);

                // Exposes
                const exposes = ast.store.getCollection(a.exposes);
                const exposes_begin = tree.beginNode();
                tree.pushStaticAtom("exposes");
                ast.appendRegionInfoToSexprTree(env, tree, exposes.region);
                const attrs4 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(exposes_begin, attrs4);

                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                const packages_begin = tree.beginNode();
                tree.pushStaticAtom("packages");
                ast.appendRegionInfoToSexprTree(env, tree, packages_coll.region);
                const attrs5 = tree.beginNode();
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(packages_begin, attrs5);

                // Provides
                const provides = ast.store.getCollection(a.provides);
                const provides_begin = tree.beginNode();
                tree.pushStaticAtom("provides");
                ast.appendRegionInfoToSexprTree(env, tree, provides.region);
                const attrs6 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = provides.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(provides_begin, attrs6);

                tree.endNode(begin, attrs);
            },
            .hosted => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("hosted");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                const exposes = ast.store.getCollection(a.exposes);
                const exposes_begin = tree.beginNode();
                tree.pushStaticAtom("exposes");
                ast.appendRegionInfoToSexprTree(env, tree, exposes.region);
                const attrs2 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    item.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(exposes_begin, attrs2);

                tree.endNode(begin, attrs);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("malformed-header");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("tag", @tagName(a.reason));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
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
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        switch (self) {
            .lower_ident => |i| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("exposed-lower-ident");
                // text attribute
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                const text_begin = tree.beginNode();
                tree.pushStaticAtom("text");
                tree.pushString(text);
                const attrs2 = tree.beginNode();
                tree.endNode(text_begin, attrs2);

                // as attribute if present
                if (i.as) |a| {
                    const as_tok = ast.tokens.tokens.get(a);
                    const as_text = env.idents.getText(as_tok.extra.interned);
                    tree.pushStringPair("as", as_text);
                }
                const attrs = tree.beginNode();

                tree.endNode(begin, attrs);
            },
            .upper_ident => |i| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("exposed-upper-ident");

                // text attribute
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                tree.pushStringPair("text", text);

                // as attribute if present
                if (i.as) |a| {
                    const as_tok = ast.tokens.tokens.get(a);
                    const as_text = env.idents.getText(as_tok.extra.interned);
                    tree.pushStringPair("as", as_text);
                }

                const attrs = tree.beginNode();

                tree.endNode(begin, attrs);
            },
            .upper_ident_star => |i| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("exposed-upper-ident-star");

                // text attribute
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.idents.getText(token.extra.interned);
                tree.pushStringPair("text", text);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .malformed => |m| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("exposed-malformed");

                // reason attribute
                const reason_begin = tree.beginNode();
                tree.pushStaticAtom("reason");
                tree.pushString(@tagName(m.reason));
                const attrs2 = tree.beginNode();
                tree.endNode(reason_begin, attrs2);

                // region info
                ast.appendRegionInfoToSexprTree(env, tree, m.region);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
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
        token: Token.Idx,
        qualifiers: Token.Span,
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

    /// Extract the region from any TypeAnno variant
    pub fn toRegion(self: *const @This()) TokenizedRegion {
        switch (self.*) {
            .apply => |a| return a.region,
            .ty_var => |tv| return tv.region,
            .underscore => |u| return u.region,
            .ty => |t| return t.region,
            .mod_ty => |t| return t.region,
            .tag_union => |tu| return tu.region,
            .tuple => |t| return t.region,
            .record => |r| return r.region,
            .@"fn" => |f| return f.region,
            .parens => |p| return p.region,
            .malformed => |m| return m.region,
        }
    }

    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        switch (self) {
            .apply => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-apply");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.typeAnnoSlice(a.args)) |b| {
                    ast.store.getTypeAnno(b).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .ty_var => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-var");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("raw", ast.resolve(a.tok));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .underscore => {
                const begin = tree.beginNode();
                tree.pushStaticAtom("_");
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .ty => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);

                // Resolve the fully qualified name
                const strip_tokens = [_]Token.Tag{.NoSpaceDotUpperIdent};
                const fully_qualified_name = ast.resolveQualifiedName(a.qualifiers, a.token, &strip_tokens);
                tree.pushStringPair("name", fully_qualified_name);
                const attrs = tree.beginNode();

                tree.endNode(begin, attrs);
            },
            .mod_ty => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-mod");
                const attrs = tree.beginNode();

                const mod_text = env.idents.getText(a.mod_ident);
                const type_text = env.idents.getText(a.ty_ident);

                // module attribute
                const module_begin = tree.beginNode();
                tree.pushStaticAtom("module");
                tree.pushString(mod_text);
                const attrs2 = tree.beginNode();
                tree.endNode(module_begin, attrs2);

                // name attribute
                const name_begin = tree.beginNode();
                tree.pushStaticAtom("name");
                tree.pushString(type_text);
                const attrs3 = tree.beginNode();
                tree.endNode(name_begin, attrs3);

                tree.endNode(begin, attrs);
            },
            .tag_union => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-tag-union");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                const tags = ast.store.typeAnnoSlice(a.tags);
                const tags_begin = tree.beginNode();
                tree.pushStaticAtom("tags");
                const attrs2 = tree.beginNode();
                for (tags) |tag_idx| {
                    ast.store.getTypeAnno(tag_idx).pushToSExprTree(env, ast, tree);
                }
                tree.endNode(tags_begin, attrs2);

                if (a.open_anno) |anno_idx| {
                    ast.store.getTypeAnno(anno_idx).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .tuple => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-tuple");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.typeAnnoSlice(a.annos)) |b| {
                    ast.store.getTypeAnno(b).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .record => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-record");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.annoRecordFieldSlice(a.fields)) |f_idx| {
                    const field = ast.store.getAnnoRecordField(f_idx) catch |err| switch (err) {
                        error.MalformedNode => {
                            // Create a malformed-field node for debugging
                            const malformed_begin = tree.beginNode();
                            tree.pushStaticAtom("malformed-field");
                            const attrs2 = tree.beginNode();
                            tree.endNode(malformed_begin, attrs2);
                            continue;
                        },
                    };
                    field.pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .@"fn" => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-fn");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // arguments
                for (ast.store.typeAnnoSlice(a.args)) |b| {
                    ast.store.getTypeAnno(b).pushToSExprTree(env, ast, tree);
                }

                // return value
                ast.store.getTypeAnno(a.ret).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .parens => |a| {
                // Ignore parens, use inner
                ast.store.getTypeAnno(a.anno).pushToSExprTree(env, ast, tree);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("ty-malformed");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("tag", @tagName(a.reason));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("anno-record-field");
        ast.appendRegionInfoToSexprTree(env, tree, self.region);
        tree.pushStringPair("name", ast.resolve(self.name));
        const attrs = tree.beginNode();

        const anno = ast.store.getTypeAnno(self.ty);
        anno.pushToSExprTree(env, ast, tree);

        tree.endNode(begin, attrs);
    }
};

/// The clause of a `where` constraint
///
/// Where clauses specify constraints on type variables that must be satisfied
/// for a function or type to be valid. They enable generic programming with
/// compile-time guarantees about available capabilities.
pub const WhereClause = union(enum) {
    /// Module method constraint specifying a method must exist in the module containing a type.
    ///
    /// This is the most common form of where clause constraint. It specifies that
    /// a type variable must come from a module that provides a specific method.
    ///
    /// Examples:
    /// ```roc
    /// convert : a -> b where module(a).to_b : a -> b
    /// decode : List(U8) -> a where module(a).decode : List(U8) -> a
    /// hash : a -> U64 where module(a).hash : a -> U64
    /// ```
    mod_method: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        args: Collection.Idx,
        ret_anno: TypeAnno.Idx,
        region: TokenizedRegion,
    },

    /// Module type alias constraint.
    ///
    /// Specifies that a type variable must satisfy the constraints for an alias type.
    /// This is useful to avoid writing out the constraints repeatedly which can be cumbersome and error prone
    ///
    /// Example:
    /// ```roc
    /// Sort(a) : a where  module(a).order(elem, elem) -> [LT, EQ, GT]
    ///
    /// sort : List(elem) -> List(elem) where module(elem).Sort
    /// ```
    mod_alias: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        region: TokenizedRegion,
    },

    /// Malformed where clause that failed to parse correctly.
    ///
    /// Contains diagnostic information about what went wrong during parsing.
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },
    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        switch (self) {
            .mod_method => |m| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("method");
                ast.appendRegionInfoToSexprTree(env, tree, m.region);

                tree.pushStringPair("module-of", ast.resolve(m.var_tok));

                // remove preceding dot
                const method_name = ast.resolve(m.name_tok)[1..];
                tree.pushStringPair("name", method_name);
                const attrs = tree.beginNode();

                const args_begin = tree.beginNode();
                tree.pushStaticAtom("args");
                const attrs2 = tree.beginNode();
                const args = ast.store.typeAnnoSlice(.{ .span = ast.store.getCollection(m.args).span });
                for (args) |arg| {
                    ast.store.getTypeAnno(arg).pushToSExprTree(env, ast, tree);
                }
                tree.endNode(args_begin, attrs2);

                ast.store.getTypeAnno(m.ret_anno).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .mod_alias => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("alias");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);

                tree.pushStringPair("module-of", ast.resolve(a.var_tok));

                // remove preceding dot
                const alias_name = ast.resolve(a.name_tok)[1..];
                tree.pushStringPair("name", alias_name);

                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .malformed => |m| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("malformed");
                ast.appendRegionInfoToSexprTree(env, tree, m.region);
                tree.pushStringPair("reason", @tagName(m.reason));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
        }
    }

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
    single_quote: struct {
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
        /// Record extension: { ..person, field: value }
        ext: ?Expr.Idx,
        region: TokenizedRegion,
    },
    tag: TagExpr,
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
        branches: MatchBranch.Span,
        region: TokenizedRegion,
    },
    ident: struct {
        token: Token.Idx,
        qualifiers: Token.Span,
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
            .single_quote => |e| e.region,
        };
    }

    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        switch (self) {
            .int => |int| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-int");
                ast.appendRegionInfoToSexprTree(env, tree, int.region);

                // Add raw attribute
                const raw_begin = tree.beginNode();
                tree.pushStaticAtom("raw");
                tree.pushString(ast.resolve(int.token));
                const attrs2 = tree.beginNode();
                tree.endNode(raw_begin, attrs2);
                const attrs = tree.beginNode();

                tree.endNode(begin, attrs);
            },
            .frac => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-frac");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("raw", ast.resolve(a.token));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .single_quote => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-single-quote");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("raw", ast.resolve(a.token));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .string_part => |sp| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-string-part");
                ast.appendRegionInfoToSexprTree(env, tree, sp.region);
                const raw = tree.beginNode();
                tree.pushStaticAtom("raw");
                tree.pushString(ast.resolve(sp.token));
                const attrs2 = tree.beginNode();
                tree.endNode(raw, attrs2);
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .string => |str| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-string");
                ast.appendRegionInfoToSexprTree(env, tree, str.region);
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(str.parts)) |part_id| {
                    const part_expr = ast.store.getExpr(part_id);
                    part_expr.pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .list => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-list");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(a.items)) |b| {
                    ast.store.getExpr(b).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .tuple => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-tuple");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(a.items)) |b| {
                    ast.store.getExpr(b).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .record => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-record");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Add extension if present
                if (a.ext) |ext_idx| {
                    const ext_wrapper = tree.beginNode();
                    tree.pushStaticAtom("ext");
                    ast.store.getExpr(ext_idx).pushToSExprTree(env, ast, tree);
                    tree.endNode(ext_wrapper, attrs);
                }

                for (ast.store.recordFieldSlice(a.fields)) |field_idx| {
                    const record_field = ast.store.getRecordField(field_idx);
                    const field_node = tree.beginNode();
                    tree.pushStaticAtom("field");
                    tree.pushStringPair("field", ast.resolve(record_field.name));
                    const attrs2 = tree.beginNode();
                    if (record_field.value) |value_id| {
                        ast.store.getExpr(value_id).pushToSExprTree(env, ast, tree);
                    }
                    tree.endNode(field_node, attrs2);
                }

                tree.endNode(begin, attrs);
            },
            .tag => |tag| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-tag");
                ast.appendRegionInfoToSexprTree(env, tree, tag.region);

                // Resolve the fully qualified name
                const strip_tokens = [_]Token.Tag{.NoSpaceDotUpperIdent};
                const fully_qualified_name = ast.resolveQualifiedName(tag.qualifiers, tag.token, &strip_tokens);
                tree.pushStringPair("raw", fully_qualified_name);
                const attrs = tree.beginNode();

                tree.endNode(begin, attrs);
            },
            .lambda => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-lambda");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                const args = tree.beginNode();
                tree.pushStaticAtom("args");
                const attrs2 = tree.beginNode();
                // Push args (patterns)
                for (ast.store.patternSlice(a.args)) |pat| {
                    ast.store.getPattern(pat).pushToSExprTree(env, ast, tree);
                }
                tree.endNode(args, attrs2);

                // Push body
                ast.store.getExpr(a.body).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .apply => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-apply");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push function
                ast.store.getExpr(a.@"fn").pushToSExprTree(env, ast, tree);

                // Push arguments
                for (ast.store.exprSlice(a.args)) |arg_id| {
                    ast.store.getExpr(arg_id).pushToSExprTree(env, ast, tree);
                }

                tree.endNode(begin, attrs);
            },
            .record_updater => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-record-updater");
                tree.pushString(ast.resolve(a.token));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .if_then_else => |stmt| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-if-then-else");
                ast.appendRegionInfoToSexprTree(env, tree, stmt.region);
                const attrs = tree.beginNode();

                ast.store.getExpr(stmt.condition).pushToSExprTree(env, ast, tree);
                ast.store.getExpr(stmt.then).pushToSExprTree(env, ast, tree);
                ast.store.getExpr(stmt.@"else").pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .match => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-match");
                const attrs = tree.beginNode();

                ast.store.getExpr(a.expr).pushToSExprTree(env, ast, tree);

                const branches = tree.beginNode();
                tree.pushStaticAtom("branches");
                const attrs2 = tree.beginNode();

                for (ast.store.matchBranchSlice(a.branches)) |branch_idx| {
                    const branch = ast.store.getBranch(branch_idx);
                    branch.pushToSExprTree(env, ast, tree);
                }
                tree.endNode(branches, attrs2);

                tree.endNode(begin, attrs);
            },
            .ident => |ident| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-ident");
                ast.appendRegionInfoToSexprTree(env, tree, ident.region);

                // Add raw attribute
                const raw_begin = tree.beginNode();
                tree.pushStaticAtom("raw");
                // Resolve the fully qualified name
                const strip_tokens = [_]Token.Tag{ .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent };
                const fully_qualified_name = ast.resolveQualifiedName(ident.qualifiers, ident.token, &strip_tokens);
                tree.pushString(fully_qualified_name);
                const attrs2 = tree.beginNode();
                tree.endNode(raw_begin, attrs2);

                const attrs = tree.beginNode();

                tree.endNode(begin, attrs);
            },
            .dbg => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-dbg");
                const attrs = tree.beginNode();

                ast.store.getExpr(a.expr).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .record_builder => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-record-builder");
                const attrs = tree.beginNode();

                // Push mapper
                ast.store.getExpr(a.mapper).pushToSExprTree(env, ast, tree);

                // Push single field (not a collection)
                const field = ast.store.getRecordField(a.fields);
                field.pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .ellipsis => {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-ellipsis");
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .block => |block| {
                // Delegate to Body.pushToSExprTree
                block.pushToSExprTree(env, ast, tree);
            },
            .bin_op => |a| {
                a.pushToSExprTree(env, ast, tree);
            },
            .field_access => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-field-access");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push left expression
                ast.store.getExpr(a.left).pushToSExprTree(env, ast, tree);

                // Push right expression
                ast.store.getExpr(a.right).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .local_dispatch => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-local-dispatch");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push left expression
                ast.store.getExpr(a.left).pushToSExprTree(env, ast, tree);

                // Push right expression
                ast.store.getExpr(a.right).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
            },
            .unary_op => |a| {
                a.pushToSExprTree(env, ast, tree);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-malformed");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                tree.pushStringPair("reason", @tagName(a.reason));
                const attrs = tree.beginNode();
                tree.endNode(begin, attrs);
            },
            .suffix_single_question => |a| {
                const begin = tree.beginNode();
                tree.pushStaticAtom("e-question-suffix");
                ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push child expression
                ast.store.getExpr(a.expr).pushToSExprTree(env, ast, tree);

                tree.endNode(begin, attrs);
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
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("record-field");
        ast.appendRegionInfoToSexprTree(env, tree, self.region);
        const name = tree.beginNode();
        tree.pushStaticAtom("name");
        tree.pushString(ast.resolve(self.name));
        const attrs2 = tree.beginNode();
        tree.endNode(name, attrs2);
        const attrs = tree.beginNode();

        if (self.value) |idx| {
            const value = ast.store.getExpr(idx);
            value.pushToSExprTree(env, ast, tree);
        }

        tree.endNode(begin, attrs);
    }
};

/// A tag expr
pub const TagExpr = struct {
    token: Token.Idx,
    qualifiers: Token.Span,
    region: TokenizedRegion,
};

/// An if-else expr
pub const IfElse = struct {
    condition: Expr.Idx,
    body: Expr.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// A match branch
pub const MatchBranch = struct {
    pattern: Pattern.Idx,
    body: Expr.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn pushToSExprTree(self: @This(), env: *base.ModuleEnv, ast: *AST, tree: *SExprTree) void {
        const begin = tree.beginNode();
        tree.pushStaticAtom("branch");
        ast.appendRegionInfoToSexprTree(env, tree, self.region);
        const attrs = tree.beginNode();

        ast.store.getPattern(self.pattern).pushToSExprTree(env, ast, tree);
        ast.store.getExpr(self.body).pushToSExprTree(env, ast, tree);

        tree.endNode(begin, attrs);
    }
};
