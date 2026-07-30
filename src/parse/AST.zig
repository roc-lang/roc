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
const base = @import("base");
const reporting = @import("reporting");

const NodeStore = @import("NodeStore.zig");
pub const DeclIndex = @import("DeclIndex.zig");
const NumericLiteral = @import("NumericLiteral.zig");
pub const Token = tokenize.Token;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const SExprTree = base.SExprTree;
const Allocator = std.mem.Allocator;
const CommonEnv = base.CommonEnv;
const tokenize = @import("tokenize.zig");

pub const tokensToHtml = @import("HTML.zig").tokensToHtml;

const AST = @This();

/// The allocator used for internal allocations (tokens, nodes, diagnostics).
/// Also used to free the AST struct itself in deinit().
gpa: Allocator,

env: *CommonEnv,
tokens: TokenizedBuffer,
store: NodeStore,
decl_index: DeclIndex,
root_node_idx: u32 = 0,
tokenize_diagnostics: std.ArrayList(tokenize.Diagnostic),
parse_diagnostics: std.ArrayList(AST.Diagnostic),

/// Calculate whether this region is - or will be - multiline
pub fn regionIsMultiline(self: *AST, region: TokenizedRegion) bool {
    if (region.start >= region.end) return false;

    // Check if there's a newline in the source text between start and end tokens
    const start_region = self.tokens.resolve(region.start);
    const end_region = self.tokens.resolve(region.end - 1);

    const source_start = start_region.start.offset;
    const source_end = end_region.end.offset;

    // Look for line breaks in the source text. A carriage return is invalid
    // Roc source, but formatting a comment terminated by one emits a newline,
    // so it must make the same multiline layout decision on the first pass.
    for (self.env.source[source_start..source_end]) |c| {
        if (c == '\n' or c == '\r') {
            return true;
        }
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
        };
    };

    return info;
}

/// Append region information to an S-expression node for diagnostics
pub fn appendRegionInfoToSexprTree(self: *const AST, env: *const CommonEnv, tree: *SExprTree, region: TokenizedRegion) std.mem.Allocator.Error!void {
    const start = self.tokens.resolve(region.start);
    const region_end_idx = if (region.end > 0) region.end - 1 else region.end;
    const end = self.tokens.resolve(region_end_idx);
    const info: base.RegionInfo = base.RegionInfo.position(self.env.source, env.line_starts.items.items, start.start.offset, end.end.offset) catch .{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
    };
    try tree.pushBytesRange(start.start.offset, end.end.offset, info);
}

/// Frees all internal allocations AND the AST struct itself.
/// This follows the Zig std pattern (see std/Build/Watch.zig) where
/// deinit() includes gpa.destroy(self) for always-heap-allocated types.
pub fn deinit(self: *AST) void {
    const gpa = self.gpa;

    self.tokens.deinit(gpa);
    self.store.deinit();
    self.decl_index.deinit();
    self.tokenize_diagnostics.deinit(gpa);
    self.parse_diagnostics.deinit(gpa);

    gpa.destroy(self);
}

/// Convert a tokenize diagnostic to a Report for rendering
pub fn tokenizeDiagnosticToReport(self: *AST, diagnostic: tokenize.Diagnostic, allocator: std.mem.Allocator, filename: ?[]const u8) Allocator.Error!reporting.Report {
    const title = switch (diagnostic.tag) {
        .MisplacedCarriageReturn => "Misplaced Carriage Return",
        .AsciiControl => "ASCII Control Character",
        .LeadingZero => "Leading Zero",
        .UppercaseBase => "Uppercase Base",
        .InvalidUnicodeEscapeSequence => "Invalid Unicode Escape Sequence",
        .InvalidEscapeSequence => "Invalid Escape Sequence",
        .UnclosedString => "Unclosed String",
        .NonPrintableUnicodeInStrLiteral => "Nonprintable Unicode in String Literal",
        .InvalidUtf8InSource => "Invalid UTF-8",
        .DollarInMiddleOfIdentifier => "Stray Dollar Sign",
        .SingleQuoteTooLong => "Single Quote Too Long",
        .SingleQuoteEmpty => "Single Quote Empty",
        .SingleQuoteUnclosed => "Unclosed Single Quote",
    };

    const body = switch (diagnostic.tag) {
        .MisplacedCarriageReturn => "Carriage return characters (\\r) are not allowed in Roc source code.",
        .AsciiControl => "ASCII control characters are not allowed in Roc source code.",
        .LeadingZero => "Numbers cannot have leading zeros.",
        .UppercaseBase => "Number base prefixes must be lowercase (0x, 0o, 0b).",
        .InvalidUnicodeEscapeSequence => "This Unicode escape sequence is not valid.",
        .InvalidEscapeSequence => "This escape sequence is not recognized.",
        .UnclosedString => "This string is missing a closing quote.",
        .NonPrintableUnicodeInStrLiteral => "Non-printable Unicode characters are not allowed in string-like literals.",
        .InvalidUtf8InSource => "Invalid UTF-8 encoding found in source code. Roc source files must be valid UTF-8.",
        .DollarInMiddleOfIdentifier => "Dollar sign ($) is only allowed at the very beginning of a name, not in the middle or at the end.",
        .SingleQuoteTooLong, .SingleQuoteEmpty => "Single-quoted literals must contain exactly one valid UTF-8 codepoint.",
        .SingleQuoteUnclosed => "This single-quoted literal is missing a closing quote.",
    };

    var report = try reporting.Report.init(allocator, title, body, .runtime_error);

    // Add the region information from the diagnostic if valid
    if (diagnostic.region.start.offset < diagnostic.region.end.offset and
        diagnostic.region.end.offset <= self.env.source.len)
    {
        var env = self.env.*;
        if (env.line_starts.items.items.len == 0) {
            try env.calcLineStarts(allocator);
        }

        // Convert region to RegionInfo
        const region_info = base.RegionInfo.position(
            self.env.source,
            env.line_starts.items.items,
            diagnostic.region.start.offset,
            diagnostic.region.end.offset,
        ) catch {
            // If we can't calculate region info, just return the report without source context
            return report;
        };

        // Add source region to the report
        try report.document.addSourceRegion(
            region_info,
            .error_highlight,
            filename,
            self.env.source,
            env.line_starts.items.items,
        );
    }

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

    const safe_end_idx = if (tokenized_region.end > token_count)
        token_count
    else
        tokenized_region.end;

    // Ensure end is at least start to prevent invalid regions
    const final_end_idx = if (safe_end_idx < safe_start_idx)
        safe_start_idx + 1
    else
        safe_end_idx;

    const start_region = self.tokens.resolve(safe_start_idx);
    // Protect against underflow when final_end_idx is 0
    const end_idx = if (final_end_idx > 0) final_end_idx - 1 else 0;
    const end_region = self.tokens.resolve(end_idx);
    return .{
        .start = start_region.start,
        .end = end_region.end,
    };
}

const ParseReportContext = struct {
    ast: *AST,
    env: *const CommonEnv,
    diagnostic: Diagnostic,
    allocator: Allocator,
    filename: []const u8,
    region: base.Region,

    fn init(ast: *AST, env: *const CommonEnv, diagnostic: Diagnostic, allocator: Allocator, filename: []const u8) ParseReportContext {
        const raw_region = ast.tokenizedRegionToRegion(diagnostic.region);
        return .{
            .ast = ast,
            .env = env,
            .diagnostic = diagnostic,
            .allocator = allocator,
            .filename = filename,
            .region = .{
                .start = .{ .offset = @min(raw_region.start.offset, ast.env.source.len) },
                .end = .{ .offset = @min(@max(raw_region.end.offset, raw_region.start.offset), ast.env.source.len) },
            },
        };
    }

    fn tokenText(self: ParseReportContext) []const u8 {
        if (self.region.start.offset < self.region.end.offset and self.region.end.offset <= self.ast.env.source.len) {
            return self.ast.env.source[self.region.start.offset..self.region.end.offset];
        }
        return "";
    }

    fn tokenTag(self: ParseReportContext) Token.Tag {
        if (self.diagnostic.region.start < self.ast.tokens.tokens.len) {
            const tags = self.ast.tokens.tokens.items(.tag);
            return tags[@intCast(self.diagnostic.region.start)];
        }
        return .EndOfFile;
    }
};

const ParseReportOptions = struct {
    example: ?[]const u8 = null,
    show_found: bool = true,
};

fn finishParseReport(ctx: ParseReportContext, report: *reporting.Report) Allocator.Error!reporting.Report {
    if (ctx.region.start.offset <= ctx.region.end.offset and ctx.region.end.offset <= ctx.ast.env.source.len) {
        const region_info = base.RegionInfo.position(ctx.ast.env.source, ctx.env.line_starts.items.items, ctx.region.start.offset, ctx.region.end.offset) catch {
            return report.*;
        };

        try report.document.addLineBreak();
        try report.document.addLineBreak();

        const owned_filename = try report.addOwnedString(ctx.filename);
        try report.addSourceContext(region_info, owned_filename, ctx.ast.env.source, ctx.env.line_starts.items.items);
    }

    return report.*;
}

fn addFoundSyntaxNote(ctx: ParseReportContext, report: *reporting.Report) Allocator.Error!void {
    const token_text = ctx.tokenText();

    try report.document.addLineBreak();
    try report.document.addLineBreak();

    if (token_text.len == 0 or ctx.tokenTag() == .EndOfFile) {
        try report.document.addReflowingText("I reached the end of the file before this construct was complete.");
        return;
    }

    const owned_token = try report.addOwnedString(token_text);
    try report.document.addText("I found ");
    try report.document.addInlineCode(owned_token);
    try report.document.addText(" here.");

    if (token_text[0] == '$') {
        try report.document.addLineBreak();
        try report.document.addReflowingText("Dollar-prefixed names are mutable variables in Roc. Record fields are labels, so they cannot start with `$`.");
        return;
    }

    switch (ctx.tokenTag()) {
        .UpperIdent,
        .DotUpperIdent,
        .NoSpaceDotUpperIdent,
        => {
            try report.document.addLineBreak();
            try report.document.addReflowingText("Names that start with uppercase letters are used for tags, type names, and module names in Roc.");
        },
        .LowerIdent,
        .DotLowerIdent,
        .NoSpaceDotLowerIdent,
        => {
            try report.document.addLineBreak();
            try report.document.addReflowingText("Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.");
        },
        .KwApp,
        .KwAs,
        .KwCrash,
        .KwDbg,
        .KwElse,
        .KwExpect,
        .KwExposes,
        .KwExposing,
        .KwFor,
        .KwGenerates,
        .KwHas,
        .KwHosted,
        .KwIf,
        .KwImplements,
        .KwImport,
        .KwImports,
        .KwIn,
        .KwInterface,
        .KwMatch,
        .KwModule,
        .KwPackage,
        .KwPackages,
        .KwPlatform,
        .KwProvides,
        .KwRequires,
        .KwReturn,
        .KwTargets,
        .KwVar,
        .KwWhere,
        .KwWhile,
        .KwWith,
        .KwBreak,
        => {
            try report.document.addLineBreak();
            try report.document.addReflowingText("That word is reserved by Roc, so it cannot be used as a name in this position.");
        },
        .Comma => {
            try report.document.addLineBreak();
            try report.document.addReflowingText("A comma separates items, but there must be a valid item on both sides of it.");
        },
        .CloseCurly,
        .CloseRound,
        .CloseSquare,
        => {
            try report.document.addLineBreak();
            try report.document.addReflowingText("This closes the current construct, so the parser was looking for the missing item before it.");
        },
        .MalformedUnicodeIdent,
        .MalformedDotUnicodeIdent,
        .MalformedNoSpaceDotUnicodeIdent,
        .MalformedNamedUnderscoreUnicode,
        .MalformedOpaqueNameUnicode,
        .MalformedOpaqueNameWithoutName,
        .MalformedUnknownToken,
        => {
            try report.document.addLineBreak();
            try report.document.addReflowingText("This token is malformed, so it cannot be used as ordinary Roc syntax.");
        },
        else => {},
    }

    if (reporting.CommonMisspellings.getTokenTip(token_text)) |tip| {
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("Tip: ");
        try report.document.addReflowingTextWithBackticks(tip);
    }
}

fn reportParseProblem(
    ctx: ParseReportContext,
    title: []const u8,
    headline: []const u8,
    body: []const u8,
    options: ParseReportOptions,
) Allocator.Error!reporting.Report {
    var report = try reporting.Report.init(ctx.allocator, title, headline, .runtime_error);
    try report.document.addReflowingTextWithBackticks(body);

    if (options.example) |example| {
        try report.document.addLineBreak();
        try report.document.addLineBreak();
        try report.document.addText("For example:");
        try report.document.addLineBreak();
        try report.document.addCodeBlock(example);
    }

    if (options.show_found) {
        try addFoundSyntaxNote(ctx, &report);
    }

    return try finishParseReport(ctx, &report);
}

fn reportDeprecatedNumberSuffix(ctx: ParseReportContext) Allocator.Error!reporting.Report {
    const token_text = ctx.tokenText();
    const split = NumericLiteral.deprecatedSuffixFromSource(token_text);
    const type_name = split.deprecated_suffix.newTypeName() orelse "";
    const suggested = try std.fmt.allocPrint(ctx.allocator, "{s}.{s}", .{ split.number_text, type_name });
    defer ctx.allocator.free(suggested);

    var report = try reporting.Report.init(
        ctx.allocator,
        "Deprecated Number Suffix",
        "This number literal uses an old suffix syntax.",
        .runtime_error,
    );

    const owned_suffix = try report.addOwnedString(split.deprecated_suffix_text);
    const owned_suggested = try report.addOwnedString(suggested);
    try report.document.addText("The suffix ");
    try report.document.addInlineCode(owned_suffix);
    try report.document.addText(" is deprecated. Write ");
    try report.document.addInlineCode(owned_suggested);
    try report.document.addText(" instead.");

    return try finishParseReport(ctx, &report);
}

/// Convert a parse diagnostic to a Report for rendering.
pub fn parseDiagnosticToReport(self: *AST, env: *const CommonEnv, diagnostic: Diagnostic, allocator: std.mem.Allocator, filename: []const u8) Allocator.Error!reporting.Report {
    const ctx = ParseReportContext.init(self, env, diagnostic, allocator, filename);

    return switch (diagnostic.tag) {
        .multiple_platforms => reportParseProblem(ctx, "Multiple Platforms", "I was parsing an app header, and it names more than one platform.", "An app can use exactly one `platform` entry. Keep the platform entry you want to run with, and make every other dependency a normal package string.", .{ .example = "app [main] { pf: platform \"../platform/main.roc\", json: \"../json/main.roc\" }" }),
        .no_platform => reportParseProblem(ctx, "Missing Platform", "I was parsing an app header, and I could not find a platform entry.", "App headers must include one field whose value starts with `platform`. That platform tells Roc how to run the app.", .{ .example = "app [main] { pf: platform \"../basic-cli/platform.roc\" }" }),
        .missing_arrow => reportParseProblem(ctx, "Missing Arrow", "I was parsing a function type, and I expected an arrow here.", "Function types use `->` between arguments and return values. Add the missing arrow or wrap the surrounding type in parentheses if a different grouping was intended.", .{ .example = "Str -> U64" }),
        .expected_exposes => reportParseProblem(ctx, "Expected Exposes", "I was parsing a platform header, and I expected the `exposes` section.", "A platform header must list the values it exposes before the package and provides sections.", .{ .example = "exposes [main]" }),
        .expected_exposes_close_square => reportParseProblem(ctx, "Expected Closing Bracket", "I was parsing an `exposes` list, and I expected a closing `]`.", "Every exposes list starts with `[` and ends with `]`. Add the closing bracket after the last exposed name.", .{ .example = "exposes [main, helper]" }),
        .expected_exposes_open_square => reportParseProblem(ctx, "Expected Opening Bracket", "I was parsing an `exposes` section, and I expected an opening `[`.", "The exposed names must be written inside square brackets.", .{ .example = "exposes [main]" }),
        .expected_package_or_platform_name => reportParseProblem(ctx, "Expected Dependency Name", "I was parsing an app dependency record, and I expected a lowercase field name.", "Each package or platform entry starts with a lowercase field name, followed by `:` and a string path or `platform` path.", .{ .example = "pf: platform \"../platform/main.roc\"" }),
        .expected_package_or_platform_colon => reportParseProblem(ctx, "Expected Dependency Colon", "I was parsing an app dependency entry, and I expected `:` after the field name.", "A dependency entry uses a colon between the local package name and the package path.", .{ .example = "json: \"../json/main.roc\"" }),
        .expected_package_or_platform_string => reportParseProblem(ctx, "Expected Package Path", "I was parsing an app dependency entry, and I expected a string path.", "Normal package dependencies use a string path after the colon. Use `platform \"...\"` only for the single platform entry.", .{ .example = "json: \"../json/main.roc\"" }),
        .expected_package_platform_close_curly => reportParseProblem(ctx, "Expected Closing Brace", "I was parsing an app or package dependency record, and I expected a closing `}`.", "Dependency records must be closed with `}` after the final entry.", .{ .example = "{ pf: platform \"../platform/main.roc\" }" }),
        .expected_package_platform_open_curly => reportParseProblem(ctx, "Expected Opening Brace", "I was parsing an app or package header, and I expected an opening `{` for dependencies.", "App and package headers write dependencies in a record after the exposes list.", .{ .example = "app [main] { pf: platform \"../platform/main.roc\" }" }),
        .expected_packages => reportParseProblem(ctx, "Expected Packages", "I was parsing a platform header, and I expected the `packages` section.", "A platform header must include a packages record that names package dependencies.", .{ .example = "packages { base: \"../base/main.roc\" }" }),
        .expected_packages_close_curly => reportParseProblem(ctx, "Expected Closing Brace", "I was parsing a `packages` record, and I expected a closing `}`.", "Close the packages record after the last package entry.", .{ .example = "packages { base: \"../base/main.roc\" }" }),
        .expected_packages_open_curly => reportParseProblem(ctx, "Expected Opening Brace", "I was parsing a `packages` section, and I expected an opening `{`.", "Package dependencies are written as record fields inside braces.", .{ .example = "packages { base: \"../base/main.roc\" }" }),
        .expected_platform_name_end => reportParseProblem(ctx, "Expected Closing Quote", "I was parsing a platform name, and I expected the closing quote.", "Platform headers start with a quoted platform name. Finish the string before writing the `requires` section.", .{ .example = "platform \"basic-cli\"" }),
        .expected_platform_name_start => reportParseProblem(ctx, "Expected Platform Name", "I was parsing a platform header, and I expected a quoted platform name.", "Put the platform name in double quotes immediately after the `platform` keyword.", .{ .example = "platform \"basic-cli\"" }),
        .expected_platform_name_string => reportParseProblem(ctx, "Expected Platform Name Text", "I was parsing a platform name, and I expected text inside the quotes.", "A platform name cannot be empty. Put the platform name between the opening and closing quotes.", .{ .example = "platform \"basic-cli\"" }),
        .expected_platform_string => reportParseProblem(ctx, "Expected Platform Reference", "I was parsing a platform dependency, and I expected a string path or compiler-owned platform after `platform`.", "A platform entry uses the `platform` keyword followed by a string path to the platform file, or by the compiler-owned `glue` platform.", .{ .example = "pf: platform \"../platform/main.roc\"" }),
        .expected_provides => reportParseProblem(ctx, "Expected Provides", "I was parsing a platform header, and I expected the `provides` section.", "A platform header must map host symbols to Roc functions in a `provides` record.", .{ .example = "provides { \"roc_main\": main }" }),
        .expected_provides_open_square => reportParseProblem(ctx, "Expected Provides List", "I was parsing a package or app header, and I expected an opening `[` for the provided names.", "The names provided by this module go in square brackets after the header keyword.", .{ .example = "package [Parser, parse]" }),
        .expected_provides_close_curly => reportParseProblem(ctx, "Expected Closing Brace", "I was parsing a `provides` symbol map, and I expected a closing `}`.", "Close the provides record after the final host-symbol mapping.", .{ .example = "provides { \"roc_main\": main }" }),
        .expected_provides_open_curly => reportParseProblem(ctx, "Expected Opening Brace", "I was parsing a `provides` section, and I expected an opening `{`.", "Host symbol mappings are written as record-like entries inside braces.", .{ .example = "provides { \"roc_main\": main }" }),
        .expected_symbol_string => reportParseProblem(ctx, "Expected Host Symbol", "I was parsing a host symbol map, and I expected a string symbol name.", "Each host symbol entry starts with the exported symbol name in quotes.", .{ .example = "\"roc_main\": main" }),
        .expected_symbol_map_colon => reportParseProblem(ctx, "Expected Symbol Colon", "I was parsing a host symbol map entry, and I expected `:` after the symbol string.", "Use a colon between the host symbol string and the Roc function that implements it.", .{ .example = "\"roc_main\": main" }),
        .expected_symbol_map_function => reportParseProblem(ctx, "Expected Roc Function", "I was parsing a host symbol map entry, and I expected a Roc function name.", "The right side of a host symbol entry must be a lowercase function name, optionally qualified by a module path.", .{ .example = "\"roc_main\": Platform.main!" }),
        .expected_hosted_open_curly => reportParseProblem(ctx, "Expected Hosted Map", "I was parsing a `hosted` section, and I expected an opening `{`.", "Hosted symbol mappings are written inside braces.", .{ .example = "hosted { \"roc_alloc\": alloc }" }),
        .expected_hosted_close_curly => reportParseProblem(ctx, "Expected Closing Brace", "I was parsing a `hosted` section, and I expected a closing `}`.", "Close the hosted symbol map after the final host function mapping.", .{ .example = "hosted { \"roc_alloc\": alloc }" }),
        .expected_requires => reportParseProblem(ctx, "Expected Requires", "I was parsing a platform header, and I expected the `requires` section.", "A platform header must state which entrypoints the app must provide.", .{ .example = "requires { main : {} => I32 }" }),
        .expected_requires_rigids_open_curly => reportParseProblem(ctx, "Expected Requires Entries", "I was parsing a `requires` section, and I expected an opening `{`.", "Required entrypoints are written inside braces after `requires`.", .{ .example = "requires { main : {} => I32 }" }),
        .expected_requires_signatures_close_curly => reportParseProblem(ctx, "Expected Closing Brace", "I was parsing a `requires` section, and I expected a closing `}`.", "Close the requires record after the final entrypoint signature.", .{ .example = "requires { main : {} => I32 }" }),
        .expected_for_clause_close_square => reportParseProblem(ctx, "Expected Closing Bracket", "I was parsing a `for` clause in `requires`, and I expected a closing `]`.", "Type aliases in a `for` clause are written inside square brackets before `for`.", .{ .example = "[Arg : a] for main : a -> I32" }),
        .expected_for_clause_alias_name => reportParseProblem(ctx, "Expected Alias Name", "I was parsing a `for` clause in `requires`, and I expected an uppercase alias name.", "Alias names in a `for` clause start with uppercase letters.", .{ .example = "[Arg : a] for main : a -> I32" }),
        .expected_for_clause_colon => reportParseProblem(ctx, "Expected Alias Colon", "I was parsing a `for` clause alias, and I expected `:` after the alias name.", "Use a colon between the uppercase alias name and the lowercase type variable it stands for.", .{ .example = "[Arg : a] for main : a -> I32" }),
        .expected_for_clause_rigid_name => reportParseProblem(ctx, "Expected Type Variable", "I was parsing a `for` clause alias, and I expected a lowercase type variable.", "The right side of a `for` alias must be a lowercase type variable.", .{ .example = "[Arg : a] for main : a -> I32" }),
        .expected_for_keyword => reportParseProblem(ctx, "Expected For", "I was parsing type aliases in a `requires` entry, and I expected the `for` keyword.", "After the alias list, write `for` before the required entrypoint name.", .{ .example = "[Arg : a] for main : a -> I32" }),
        .expected_for_clause_entrypoint_name => reportParseProblem(ctx, "Expected Entrypoint Name", "I was parsing a `requires` entry, and I expected a lowercase entrypoint name.", "Required entrypoint names are lowercase value names.", .{ .example = "main : {} => I32" }),
        .expected_for_clause_type_colon => reportParseProblem(ctx, "Expected Entrypoint Type", "I was parsing a `requires` entry, and I expected `:` before the type.", "Use a colon between the required entrypoint name and its type annotation.", .{ .example = "main : {} => I32" }),
        .header_expected_open_square => reportParseProblem(ctx, "Expected Exposing List", "I was parsing a package, app, platform, or hosted header, and I expected an opening `[`.", "The names exposed by this header are written in square brackets after the header keyword.", .{ .example = "package [Parser, parse]" }),
        .header_expected_close_square => reportParseProblem(ctx, "Expected Closing Bracket", "I was parsing a header exposing list, and I expected a closing `]`.", "Close the list after the final exposed name.", .{ .example = "package [Parser, parse]" }),
        .pattern_unexpected_token => reportParseProblem(ctx, "Unexpected Pattern Syntax", "I was parsing a pattern, and this token cannot start a pattern here.", "Patterns can be lowercase names, tags, literals, lists, records, tuples, underscores, or nested patterns.", .{ .example = "{ name, age }" }),
        .pattern_list_rest_old_syntax => reportParseProblem(ctx, "Old List Rest Pattern", "I was parsing a list pattern, and this uses the old rest syntax.", "List rest patterns now use `.. as name`. The name is optional, but if it is present it must come after `as`.", .{ .example = "[first, .. as rest]", .show_found = false }),
        .pattern_unexpected_eof => reportParseProblem(ctx, "Unfinished Pattern", "I was parsing a pattern, and the file ended before it was complete.", "Complete the pattern or remove the incomplete syntax.", .{ .example = "[first, second]" }),
        .bad_as_pattern_name => reportParseProblem(ctx, "Expected Pattern Name", "I was parsing an `as` pattern, and I expected a lowercase name after `as`.", "The name after `as` binds the whole matched value, so it must be a lowercase value name.", .{ .example = "Some(value) as whole" }),
        .ty_anno_unexpected_token => reportParseProblem(ctx, "Unexpected Type Syntax", "I was parsing a type annotation, and this token cannot start a type here.", "Types can be type variables, uppercase type names, function types, tuples, records, or tag unions.", .{ .example = "List(U64)" }),
        .statement_unexpected_token => reportParseProblem(ctx, "Unexpected Statement", "I was parsing a statement, and this token cannot start a statement here.", "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.", .{ .example = "answer = 42" }),
        .string_unexpected_token => reportParseProblem(ctx, "Unexpected String Syntax", "I was parsing a string literal, and this token is not valid inside the string.", "Strings contain text, escapes, and interpolations. Close the string or fix the interpolation before continuing.", .{ .example = "\"Hello, ${name}!\"" }),
        .string_expected_close_interpolation => reportParseProblem(ctx, "Expected Interpolation End", "I was parsing a string interpolation, and I expected `}` before returning to the string.", "String interpolations start with `${` and must close with `}` after the embedded expression.", .{ .example = "\"Hello, ${name}!\"" }),
        .string_unclosed => reportParseProblem(ctx, "Unclosed String", "I was parsing a string literal, and the file ended before the closing quote.", "Add the closing quote, or use a multiline string if the text should span multiple lines.", .{ .example = "\"hello\"" }),
        .expr_no_space_dot_int => reportParseProblem(ctx, "Invalid Tuple Access", "I was parsing tuple access, and I expected a tuple field number after `.`.", "Tuple access must use a dot immediately followed by a number, such as `.0` or `.1`.", .{ .example = "pair.0" }),
        .import_exposing_no_open => reportParseProblem(ctx, "Expected Exposing List", "I was parsing an import exposing clause, and I expected `[` after `exposing`.", "The imported names go in square brackets after `exposing`.", .{ .example = "import Json exposing [decode, encode]" }),
        .import_exposing_no_close => reportParseProblem(ctx, "Expected Closing Bracket", "I was parsing an import exposing clause, and I expected a closing `]`.", "Close the exposing list after the final imported name.", .{ .example = "import Json exposing [decode, encode]" }),
        .expected_type_field_name => reportParseProblem(ctx, "Expected Type Field", "I was parsing a record type, and I expected a field name.", "Record type fields start with lowercase names, `_`, or named underscores, followed by `:` and the field type.", .{ .example = "{ name : Str, age : U64 }" }),
        .expected_colon_after_type_field_name => reportParseProblem(ctx, "Expected Field Type", "I was parsing a record type field, and I expected `:` after the field name.", "Record type fields use a colon between the field name and its type.", .{ .example = "{ name : Str }" }),
        .expected_arrow => reportParseProblem(ctx, "Expected Function Arrow", "I was parsing a function type, and I expected `->` or `=>` before the return type.", "Function types list argument types first, then an arrow, then the return type.", .{ .example = "Str, U64 -> Bool" }),
        .multi_arrow_needs_parens => reportParseProblem(ctx, "Ambiguous Function Type", "I was parsing a function type, and multiple arrows need parentheses.", "Use parentheses to say whether the function returns another function or takes a function as an argument.", .{ .example = "a -> (b -> c)\n(a -> b) -> c", .show_found = false }),
        .expected_ty_close_curly_or_comma => reportParseProblem(ctx, "Expected Record Type Separator", "I was parsing a record type, and I expected `,` or `}`.", "Separate record type fields with commas and close the record type with `}`.", .{ .example = "{ name : Str, age : U64 }" }),
        .expected_ty_close_square_or_comma => reportParseProblem(ctx, "Expected Tag Union Separator", "I was parsing a tag union type, and I expected `,` or `]`.", "Separate tag union alternatives with commas and close the tag union with `]`.", .{ .example = "[Ok(a), Err(Str)]" }),
        .expected_lower_name_after_exposed_item_as => reportParseProblem(ctx, "Expected Lowercase Alias", "I was parsing an exposed value alias, and I expected a lowercase name after `as`.", "Aliases for exposed lowercase values must also be lowercase value names.", .{ .example = "package [oldName as newName]" }),
        .expected_upper_name_after_exposed_item_as => reportParseProblem(ctx, "Expected Uppercase Alias", "I was parsing an exposed type or tag alias, and I expected an uppercase name after `as`.", "Aliases for exposed uppercase names must also start with an uppercase letter.", .{ .example = "package [Result as Outcome]" }),
        .exposed_item_unexpected_token => reportParseProblem(ctx, "Expected Exposed Name", "I was parsing an exposing list, and I expected an exposed name.", "Exposing lists contain lowercase values, uppercase types or tags, and `Type.*` entries.", .{ .example = "package [main, Result, Result.*]" }),
        .expected_upper_name_after_import_as => reportParseProblem(ctx, "Expected Import Alias", "I was parsing an import alias, and I expected an uppercase module name after `as`.", "Import aliases rename modules, so they must start with an uppercase letter.", .{ .example = "import Json.Decode as Decode" }),
        .expected_colon_after_type_annotation => reportParseProblem(ctx, "Type Application Needs Parentheses", "I was parsing a type annotation, and I found a type argument without parentheses.", "Roc type applications use parentheses around their arguments. Write `List(U8)`, not `List U8`.", .{ .example = "List(U8)" }),
        .expected_lower_ident_pat_field_name => reportParseProblem(ctx, "Expected Pattern Field", "I was parsing a record pattern, and I expected a lowercase field name.", "Record pattern fields start with lowercase names. You can bind the field directly or write `name: pattern`.", .{ .example = "{ name, age: years }" }),
        .expected_colon_after_pat_field_name => reportParseProblem(ctx, "Expected Pattern Field Colon", "I was parsing a record pattern field, and I expected `:` after the field name.", "Use a colon when a record pattern field has a nested pattern instead of just punning the field name.", .{ .example = "{ point: { x, y } }" }),
        .expected_expr_bar => reportParseProblem(ctx, "Expected Lambda Bar", "I was parsing a lambda expression, and I expected the closing `|` after its arguments.", "Lambda arguments go between two `|` characters before the body expression.", .{ .example = "|x, y| x + y" }),
        .expected_expr_close_curly_or_comma => reportParseProblem(ctx, "Expected Record Separator", "I was parsing a record expression, and I expected `,` or `}`.", "Separate record fields with commas and close the record with `}`.", .{ .example = "{ name: \"Ada\", age: 36 }" }),
        .expected_expr_close_round_or_comma => reportParseProblem(ctx, "Expected Tuple Separator", "I was parsing a parenthesized expression or tuple, and I expected `,` or `)`.", "Separate tuple elements with commas and close the tuple or parenthesized expression with `)`.", .{ .example = "(x, y)" }),
        .expected_expr_close_square_or_comma => reportParseProblem(ctx, "Expected List Separator", "I was parsing a list expression, and I expected `,` or `]`.", "Separate list elements with commas and close the list with `]`.", .{ .example = "[1, 2, 3]" }),
        .expected_close_curly_at_end_of_match => reportParseProblem(ctx, "Unclosed Match", "I was parsing a match expression, and the file ended before the closing `}`.", "Add a closing brace after the final match branch.", .{ .example = "match value {\n    Ok(x) => x\n}" }),
        .expected_open_curly_after_match => reportParseProblem(ctx, "Expected Match Body", "I was parsing a match expression, and I expected `{` after the matched value.", "Match branches are written inside braces after the expression being matched.", .{ .example = "match result {\n    Ok(x) => x\n    Err(_) => 0\n}" }),
        .expr_unexpected_token => reportParseProblem(ctx, "Unexpected Expression Syntax", "I was parsing an expression, and this token cannot start an expression here.", "Expressions can be names, literals, tags, records, lists, tuples, lambdas, blocks, conditionals, matches, or function calls.", .{ .example = "add(1, 2)" }),
        .crash_statement_in_expr_position => reportParseProblem(ctx, "Crash Statement In Expression", "I was parsing an expression, but `crash` starts a statement.", "If you need to crash in expression position, wrap the crash statement in a block expression.", .{ .example = "{\n    crash \"unreachable\"\n}" }),
        .return_outside_function => reportParseProblem(ctx, "Return Outside Function", "I was parsing a statement, and `return` appeared outside a function body.", "`return` exits from the current function. Move it inside a function body, or remove it if this code is already the final expression.", .{ .example = "foo = |x| {\n    if x < 0 { return Err(Negative) }\n    Ok(x)\n}" }),
        .expected_expr_record_field_name => reportParseProblem(ctx, "Expected Record Field", "I was parsing a record expression, and I expected a lowercase field name.", "Record fields start with lowercase names. After the name, either write `: value` or omit the value to use field punning.", .{ .example = "{ name: \"Ada\", age }" }),
        .record_field_name_cannot_be_var => reportParseProblem(ctx, "Invalid Record Field Name", "Record field names cannot start with a dollar sign.", "Names that start with `$` are reassignable variables declared with the `var` keyword, so they cannot be used as record field names.", .{ .show_found = false }),
        .expected_ty_apply_close_round => reportParseProblem(ctx, "Expected Type Argument End", "I was parsing type arguments, and I expected `)`.", "Type applications put their arguments inside parentheses.", .{ .example = "Dict(Str, U64)" }),
        .expected_expr_apply_close_round => reportParseProblem(ctx, "Expected Call Argument End", "I was parsing function or method call arguments, and I expected `)`.", "Function call arguments go inside parentheses and are separated with commas.", .{ .example = "add(1, 2)" }),
        .where_expected_open_bracket => reportParseProblem(ctx, "Expected Where Clause List", "I was parsing a `where` clause, and I expected `[`.", "Where constraints are written in a square-bracketed list after `where`.", .{ .example = "where [a.hash : a -> U64]" }),
        .where_expected_close_bracket => reportParseProblem(ctx, "Expected Where Clause End", "I was parsing a `where` clause, and I expected `]`.", "Close the where constraint list after the final constraint.", .{ .example = "where [a.hash : a -> U64]" }),
        .where_expected_var => reportParseProblem(ctx, "Expected Type Variable", "I was parsing a `where` constraint, and I expected a lowercase type variable.", "A where constraint starts with the type variable being constrained.", .{ .example = "where [a.hash : a -> U64]" }),
        .where_expected_method_or_alias_name => reportParseProblem(ctx, "Expected Method Or Alias", "I was parsing a `where` constraint, and I expected a method or alias name after `.`.", "Use a lowercase method name for method constraints, or an uppercase alias name for alias constraints.", .{ .example = "where [a.hash : a -> U64]" }),
        .where_expected_colon => reportParseProblem(ctx, "Expected Constraint Type", "I was parsing a `where` method constraint, and I expected `:` before the method type.", "Method constraints use a colon between the method name and its type.", .{ .example = "where [a.hash : a -> U64]" }),
        .where_expected_constraints => reportParseProblem(ctx, "Expected Where Constraint", "I was parsing a `where` clause, and I expected at least one constraint.", "Remove the empty `where` clause or add a constraint inside the brackets.", .{ .example = "where [a.hash : a -> U64]" }),
        .import_must_be_top_level => reportParseProblem(ctx, "Import Must Be Top Level", "I was parsing an import, but imports are only allowed at the top level.", "Move this import after the module header and before declarations or executable statements.", .{ .example = "import Json\n\nmain = 1" }),
        .invalid_type_arg => reportParseProblem(ctx, "Expected Type Argument", "I was parsing type parameters, and I expected a lowercase type variable or `_`.", "Type declaration parameters are lowercase names, named underscores, or `_`.", .{ .example = "Result(ok, err)" }),
        .expr_arrow_expects_ident => reportParseProblem(ctx, "Expected Arrow Target", "I was parsing an arrow expression, and I expected a name or parenthesized expression after the arrow.", "The right side of this arrow form must start with a value name, tag name, or parenthesized expression.", .{ .example = "value -> next" }),
        .expr_double_dot_is_not_range => reportParseProblem(ctx, "Not A Range Operator", "I was parsing an expression, and `..` is not a range operator.", "Use `..<` for an exclusive range or `..=` for an inclusive range.", .{ .example = "1..<10\n1..=10", .show_found = false }),
        .var_only_allowed_in_a_body => reportParseProblem(ctx, "Var Outside Body", "I was parsing a statement, and `var` appeared outside a function or block body.", "Mutable variables are local body statements. Move this `var` into a body, or use an ordinary top-level declaration.", .{ .example = "main = {\n    var count = 0\n    count\n}" }),
        .var_must_have_ident => reportParseProblem(ctx, "Expected Var Name", "I was parsing a `var` statement, and I expected a lowercase name.", "A mutable variable declaration starts with `var`, followed by the variable name.", .{ .example = "var count = 0" }),
        .var_expected_equals => reportParseProblem(ctx, "Expected Var Initializer", "I was parsing a `var` statement, and I expected `=` before the initial value.", "Mutable variables must be initialized when they are declared.", .{ .example = "var count = 0" }),
        .var_type_anno_needs_var_keyword => reportParseProblem(ctx, "Var Annotation Needs Keyword", "I was parsing a type annotation for a mutable variable, and I expected the `var` keyword.", "Dollar-prefixed mutable names must be introduced with `var` when they have a type annotation.", .{ .example = "var $count : U64" }),
        .for_expected_in => reportParseProblem(ctx, "Expected In", "I was parsing a `for` expression or statement, and I expected `in` after the pattern.", "A `for` loop writes the pattern first, then `in`, then the collection being iterated.", .{ .example = "for item in items {\n    item\n}" }),
        .match_branch_wrong_arrow => reportParseProblem(ctx, "Wrong Match Arrow", "I was parsing a match branch, and I found `->` where Roc uses `=>`.", "Match branches use a fat arrow between the pattern and the branch body.", .{ .example = "Ok(value) => value", .show_found = false }),
        .match_branch_missing_arrow => reportParseProblem(ctx, "Missing Match Arrow", "I was parsing a match branch, and I expected `=>` before the branch body.", "Add `=>` after the pattern or guard.", .{ .example = "Err(msg) => crash msg" }),
        .match_has_no_branches => reportParseProblem(ctx, "Empty Match", "I was parsing a match expression, but it has no branches.", "A match expression needs at least one branch inside the braces.", .{ .example = "match result {\n    Ok(value) => value\n}" }),
        .expected_ty_anno_close_round => reportParseProblem(ctx, "Expected Closing Parenthesis", "I was parsing a parenthesized type, and I expected `)`.", "Close the parenthesized type after the final type expression.", .{ .example = "(Str -> U64)" }),
        .expected_ty_anno_close_round_or_comma => reportParseProblem(ctx, "Expected Type Separator", "I was parsing type parameters, and I expected `,` or `)`.", "Separate type parameters with commas and close the parameter list with `)`.", .{ .example = "Result(ok, err)" }),
        .expected_expr_comma => reportParseProblem(ctx, "Expected Comma", "I was parsing a record update, and I expected `,` before the fields.", "A record update writes the base record after `..`, then a comma, then the updated fields.", .{ .example = "{ ..person, name: \"Ada\" }" }),
        .expected_expr_close_curly => reportParseProblem(ctx, "Expected Closing Brace", "I was parsing a block expression, and I expected `}` before the file ended.", "Close the block after its final statement or expression.", .{ .example = "{\n    answer = 42\n    answer\n}" }),
        .expr_dot_suffix_not_allowed => reportParseProblem(ctx, "Expected Record Accessor", "I was parsing access after `.`, and I expected a field name or tuple index.", "Record access uses a lowercase field name like `.name`. Tuple access uses a number like `.0`. Uppercase names, malformed names, and a bare `.` are not valid accessors.", .{ .example = "person.name\npair.0" }),
        .incomplete_import => reportParseProblem(ctx, "Incomplete Import", "I was parsing an import, and the module path is incomplete.", "Imports must name a module, optionally with a qualifier and exposing list.", .{ .example = "import Json.Decode exposing [decode]" }),
        .file_import_expected_as => reportParseProblem(ctx, "Expected File Import Name", "I was parsing a file import, and I expected `as` after the path.", "File imports give the file contents a local name using `as`.", .{ .example = "import \"data.txt\" as data : Str" }),
        .file_import_expected_name => reportParseProblem(ctx, "Expected File Import Binding", "I was parsing a file import, and I expected a lowercase binding name.", "The name after `as` is the local value that will contain the imported file contents.", .{ .example = "import \"data.txt\" as data : Str" }),
        .file_import_expected_type => reportParseProblem(ctx, "Expected File Import Type", "I was parsing a file import, and I expected a type annotation.", "File imports must say whether the imported contents are `Str` or `List(U8)`.", .{ .example = "import \"data.bin\" as bytes : List(U8)" }),
        .file_import_invalid_type => reportParseProblem(ctx, "Invalid File Import Type", "I was parsing a file import type, and only `Str` or `List(U8)` is allowed.", "Use `Str` for text files and `List(U8)` for raw bytes.", .{ .example = "import \"data.txt\" as data : Str" }),
        .nominal_associated_cannot_have_final_expression => reportParseProblem(ctx, "Unexpected Associated Expression", "I was parsing associated items for a nominal type, and I found a plain final expression.", "Associated item blocks can contain associated types and values. Remove the trailing expression or turn it into a named associated value.", .{ .example = "Id := U64 implements [\n    zero = @Id 0\n]" }),
        .type_alias_cannot_have_associated => reportParseProblem(ctx, "Type Alias With Associated Items", "I was parsing a type alias, but only nominal types can have associated items.", "Use `:=` to define a nominal type with associated items, or remove the associated item block from this alias.", .{ .example = "Id := U64 implements [\n    zero = @Id 0\n]" }),
        .deprecated_number_suffix => reportDeprecatedNumberSuffix(ctx),
        .expected_targets_colon => reportParseProblem(ctx, "Expected Targets Colon", "I was parsing a `targets` section, and I expected `:` after `targets`.", "The targets section starts with `targets:` followed by a configuration record.", .{ .example = "targets: { linux: { inputs: [app] } }" }),
        .expected_targets_open_curly => reportParseProblem(ctx, "Expected Targets Record", "I was parsing a `targets` section, and I expected `{`.", "Targets are configured with fields inside a record.", .{ .example = "targets: { linux: { inputs: [app] } }" }),
        .expected_targets_close_curly => reportParseProblem(ctx, "Expected Targets End", "I was parsing a `targets` section, and I expected `}`.", "Close the targets record after the final target entry.", .{ .example = "targets: { linux: { inputs: [app] } }" }),
        .expected_targets_field_name => reportParseProblem(ctx, "Expected Targets Field", "I was parsing a target configuration, and I expected a lowercase field name.", "Target entries and target options start with lowercase field names.", .{ .example = "linux: { inputs: [app] }" }),
        .expected_targets_field_colon => reportParseProblem(ctx, "Expected Targets Field Value", "I was parsing a target field, and I expected `:` before the value.", "Use a colon between a target field name and its value.", .{ .example = "inputs: [app]" }),
        .expected_target_link_open_curly => reportParseProblem(ctx, "Expected Link Config", "I was parsing a target link configuration, and I expected `{`.", "Link configuration values are written as records.", .{ .example = "link: { kind: static }" }),
        .expected_target_link_close_curly => reportParseProblem(ctx, "Expected Link Config End", "I was parsing a target link configuration, and I expected `}`.", "Close the link configuration record after the final option.", .{ .example = "link: { kind: static }" }),
        .expected_target_name => reportParseProblem(ctx, "Expected Target Name", "I was parsing a target entry, and I expected a lowercase target name.", "Target names are lowercase field names inside the targets record.", .{ .example = "linux: { inputs: [app] }" }),
        .expected_target_colon => reportParseProblem(ctx, "Expected Target Config", "I was parsing a target entry, and I expected `:` before the target configuration.", "Use a colon between the target name and its configuration record.", .{ .example = "linux: { inputs: [app] }" }),
        .expected_target_files_open_square => reportParseProblem(ctx, "Expected Input List", "I was parsing target inputs, and I expected `[`.", "The `inputs` field must contain a list of input files or special input names.", .{ .example = "inputs: [app, \"src/main.c\"]" }),
        .expected_target_files_close_square => reportParseProblem(ctx, "Expected Input List End", "I was parsing target inputs, and I expected `]`.", "Close the input list after the final file or input name.", .{ .example = "inputs: [app, \"src/main.c\"]" }),
        .expected_target_file => reportParseProblem(ctx, "Expected Target File", "I was parsing a target input list, and I expected a file string or special input name.", "Target inputs can be string file paths, lowercase special names, or `app`.", .{ .example = "inputs: [app, \"src/main.c\"]" }),
        .expected_target_file_string_end => reportParseProblem(ctx, "Unclosed Target File", "I was parsing a target file string, and I expected the closing quote.", "Finish the file path string before continuing the target configuration.", .{ .example = "\"src/main.c\"" }),
    };
}

/// Diagnostics related to parsing
pub const Diagnostic = struct {
    tag: Tag,
    region: TokenizedRegion,

    /// different types of diagnostic errors
    pub const Tag = enum {
        multiple_platforms,
        no_platform,
        missing_arrow,
        expected_exposes,
        expected_exposes_close_square,
        expected_exposes_open_square,
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
        expected_provides_open_square,
        expected_provides_close_curly,
        expected_provides_open_curly,
        expected_symbol_string,
        expected_symbol_map_colon,
        expected_symbol_map_function,
        expected_hosted_open_curly,
        expected_hosted_close_curly,
        expected_requires,
        expected_requires_rigids_open_curly,
        expected_requires_signatures_close_curly,
        expected_for_clause_close_square,
        expected_for_clause_alias_name,
        expected_for_clause_colon,
        expected_for_clause_rigid_name,
        expected_for_keyword,
        expected_for_clause_entrypoint_name,
        expected_for_clause_type_colon,
        header_expected_open_square,
        header_expected_close_square,
        pattern_unexpected_token,
        pattern_list_rest_old_syntax,
        pattern_unexpected_eof,
        bad_as_pattern_name,
        ty_anno_unexpected_token,
        statement_unexpected_token,
        string_unexpected_token,
        string_expected_close_interpolation,
        string_unclosed,
        expr_no_space_dot_int,
        import_exposing_no_open,
        import_exposing_no_close,
        expected_type_field_name,
        expected_colon_after_type_field_name,
        expected_arrow,
        multi_arrow_needs_parens,
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
        crash_statement_in_expr_position,
        return_outside_function,
        expected_expr_record_field_name,
        /// `$name` idents are reassignable variables and cannot name record fields
        record_field_name_cannot_be_var,
        expected_ty_apply_close_round,
        expected_expr_apply_close_round,
        where_expected_open_bracket,
        where_expected_close_bracket,
        where_expected_var,
        where_expected_method_or_alias_name,
        where_expected_colon,
        where_expected_constraints,
        import_must_be_top_level,
        invalid_type_arg,
        expr_arrow_expects_ident,
        /// `a..b` is not range syntax — ranges are `a..<b` (exclusive) or `a..=b` (inclusive)
        expr_double_dot_is_not_range,
        var_only_allowed_in_a_body,
        var_must_have_ident,
        var_expected_equals,
        var_type_anno_needs_var_keyword,
        for_expected_in,
        match_branch_wrong_arrow,
        match_branch_missing_arrow,
        match_has_no_branches,
        expected_ty_anno_close_round,
        expected_ty_anno_close_round_or_comma,
        expected_expr_comma,
        expected_expr_close_curly,
        expr_dot_suffix_not_allowed,
        incomplete_import,
        file_import_expected_as,
        file_import_expected_name,
        file_import_expected_type,
        file_import_invalid_type,
        nominal_associated_cannot_have_final_expression,
        type_alias_cannot_have_associated,
        deprecated_number_suffix,

        // Targets section parse errors
        expected_targets_colon,
        expected_targets_open_curly,
        expected_targets_close_curly,
        expected_targets_field_name,
        expected_targets_field_colon,

        // Target entry parse errors
        expected_target_link_open_curly,
        expected_target_link_close_curly,
        expected_target_name,
        expected_target_colon,
        expected_target_files_open_square,
        expected_target_files_close_square,
        expected_target_file,
        expected_target_file_string_end,
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

/// Check whether the parsed file has a top-level `main!` declaration.
/// Used to distinguish default_app modules (headerless files that provide
/// a main! entry point) from plain type modules.
pub fn hasMainBangDecl(self: *const AST) bool {
    const file = self.store.getFile();
    for (self.store.statementSlice(file.statements)) |stmt_id| {
        const stmt = self.store.getStatement(stmt_id);
        if (stmt == .decl) {
            const pattern = self.store.getPattern(stmt.decl.pattern);
            if (pattern == .ident) {
                const ident_text = self.resolve(pattern.ident.ident_tok);
                if (std.mem.eql(u8, ident_text, "main!")) return true;
            }
        }
    }
    return false;
}

/// Resolve a token index to a string slice from the source code.
pub fn resolve(self: *const AST, token: Token.Idx) []const u8 {
    const range = self.tokens.resolve(token);
    return self.env.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
}

/// Resolves a fully qualified name from a chain of qualifier tokens and a final token.
/// If there are qualifiers, returns a slice from the first qualifier to the final token.
/// Otherwise, returns the final token text with any leading dot stripped based on the token type.
pub fn resolveQualifiedName(
    self: *const AST,
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

/// Resolves the full module path for an import statement.
/// For auto-expose imports, module_name_tok points to the second-to-last token.
/// For explicit clause imports, module_name_tok points to the first token and
/// we iterate through consecutive uppercase tokens.
pub fn resolveImportModulePath(self: *const AST, module_name_tok: Token.Idx, qualifier_tok: ?Token.Idx, exposes: ExposedItem.Span) []const u8 {
    const tags = self.tokens.tokens.items(.tag);

    // Check if this is auto-expose by seeing if the first exposed item's token
    // immediately follows module_name_tok
    var is_auto_expose = false;
    if (exposes.span.len > 0) {
        const exposed_slice = self.store.exposedItemSlice(exposes);
        if (exposed_slice.len > 0) {
            const first_exposed = self.store.getExposedItem(exposed_slice[0]);
            const first_exposed_tok: ?Token.Idx = switch (first_exposed) {
                .lower_ident => |i| i.ident,
                .upper_ident => |i| i.ident,
                .upper_ident_star => |i| i.ident,
                .malformed => null,
            };
            if (first_exposed_tok) |tok| {
                if (tok == module_name_tok + 1) {
                    is_auto_expose = true;
                }
            }
        }
    }

    // Get start position (qualifier or first module segment)
    const start_offset: usize = if (qualifier_tok) |q|
        self.tokens.resolve(q).start.offset
    else
        self.tokens.resolve(module_name_tok).start.offset;

    // Find the end token
    var end_tok = module_name_tok;
    if (!is_auto_expose) {
        // For explicit clauses, iterate through consecutive uppercase tokens
        var tok = module_name_tok + 1;
        while (tok < tags.len) {
            const tag = tags[tok];
            if (tag == .NoSpaceDotUpperIdent or tag == .DotUpperIdent) {
                end_tok = tok;
                tok += 1;
            } else {
                break;
            }
        }
    }

    // Get end position
    const end_offset = self.tokens.resolve(end_tok).end.offset;

    return self.env.source[start_offset..end_offset];
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
    _ = std.testing.refAllDecls(@This());
}

/// Helper function to convert the AST to a human friendly representation in S-expression format
pub fn toSExprStr(ast: *@This(), gpa: std.mem.Allocator, env: *const CommonEnv, writer: anytype) (Allocator.Error || error{WriteFailed})!void {
    const file = ast.store.getFile();

    var tree = SExprTree.init(gpa);
    defer tree.deinit();

    try file.pushToSExprTree(gpa, env, ast, &tree);

    try tree.toStringPretty(writer, .include_linecol);
}

/// The kind of the type declaration represented:
/// 1. An alias of the form `Foo = (Bar, Baz)`
/// 2. A nominal type of the form `Foo := [Bar, Baz]`
/// 3. An opaque type of the form `Foo :: [Bar, Baz]`
pub const TypeDeclKind = enum {
    alias,
    nominal,
    @"opaque",
};

/// Represents a statement.  Not all statements are valid in all positions.
pub const Statement = union(enum) {
    decl: Decl,
    @"var": struct {
        name: Token.Idx,
        body: ?Expr.Idx,
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
    @"while": struct {
        cond: Expr.Idx,
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    @"return": struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    @"break": struct {
        region: TokenizedRegion,
    },
    import: struct {
        module_name_tok: Token.Idx,
        qualifier_tok: ?Token.Idx,
        alias_tok: ?Token.Idx,
        exposes: ExposedItem.Span,
        /// True when importing like `import json.Parser.Config` where Config is auto-exposed
        /// but Parser should not become an alias (unlike `import json.Parser exposing [Config]`)
        nested_import: bool,
        region: TokenizedRegion,
    },
    /// File import: `import "path" as name : Type`
    /// Embeds file contents as Str or List(U8).
    file_import: struct {
        path_tok: Token.Idx,
        name_tok: Token.Idx,
        type_tok: Token.Idx,
        is_bytes: bool,
        region: TokenizedRegion,
    },
    type_decl: struct {
        header: TypeHeader.Idx,
        anno: TypeAnno.Idx,
        kind: TypeDeclKind,
        /// Where clause (invalid in type declarations, but preserved for error recovery/formatting)
        where: ?Collection.Idx,
        /// Associated items block for .nominal types
        /// (e.g. the curly braces in `Foo := [A, B].{ x = 5 }`)
        associated: ?Associated,
        region: TokenizedRegion,
    },
    type_anno: struct {
        name: Token.Idx,
        anno: TypeAnno.Idx,
        where: ?Collection.Idx,
        is_var: bool,
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
    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        switch (self) {
            .decl => |decl| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-decl");
                try ast.appendRegionInfoToSexprTree(env, tree, decl.region);
                const attrs = tree.beginNode();

                // pattern
                try ast.store.getPattern(decl.pattern).pushToSExprTree(gpa, env, ast, tree);

                // body
                try ast.store.getExpr(decl.body).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .@"var" => |v| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-var");
                try ast.appendRegionInfoToSexprTree(env, tree, v.region);

                const name_str = ast.resolve(v.name);
                try tree.pushStringPair("name", name_str);
                const attrs = tree.beginNode();

                if (v.body) |body| {
                    try ast.store.getExpr(body).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .expr => |expr| {
                try ast.store.getExpr(expr.expr).pushToSExprTree(gpa, env, ast, tree);
            },
            .import => |import| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-import");
                try ast.appendRegionInfoToSexprTree(env, tree, import.region);

                // Reconstruct full qualified module name using the new helper
                const full_module_name = ast.resolveImportModulePath(import.module_name_tok, import.qualifier_tok, import.exposes);
                try tree.pushStringPair("raw", full_module_name);

                // alias e.g. `OUT` in `import pf.Stdout as OUT`
                if (import.alias_tok) |tok| {
                    const alias_str = ast.resolve(tok);
                    try tree.pushStringPair("alias", alias_str);
                }

                const attrs = tree.beginNode();

                // exposed identifiers e.g. [foo, bar] in `import pf.Stdout exposing [foo, bar]`
                const exposed_slice = ast.store.exposedItemSlice(import.exposes);
                if (exposed_slice.len > 0) {
                    const exposed = tree.beginNode();
                    try tree.pushStaticAtom("exposing");
                    const attrs2 = tree.beginNode();
                    for (ast.store.exposedItemSlice(import.exposes)) |e| {
                        try ast.store.getExposedItem(e).pushToSExprTree(env, ast, tree);
                    }
                    try tree.endNode(exposed, attrs2);
                }
                try tree.endNode(begin, attrs);
            },
            .file_import => |fi| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-file-import");
                try ast.appendRegionInfoToSexprTree(env, tree, fi.region);
                const attrs = tree.beginNode();

                try tree.pushStringPair("path", ast.resolve(fi.path_tok));
                try tree.pushStringPair("name", ast.resolve(fi.name_tok));
                try tree.pushStringPair("type", if (fi.is_bytes) "List(U8)" else "Str");

                try tree.endNode(begin, attrs);
            },
            .type_decl => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-type-decl");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // pattern
                {
                    const header = tree.beginNode();
                    try tree.pushStaticAtom("header");
                    // Check if the type header node is malformed before calling getTypeHeader
                    const header_node = ast.store.nodes.get(@enumFromInt(@intFromEnum(a.header)));
                    if (header_node.tag == .malformed) {
                        // Handle malformed type header by creating a placeholder
                        try ast.appendRegionInfoToSexprTree(env, tree, header_node.region);
                        try tree.pushStringPair("name", "<malformed>");
                        const attrs2 = tree.beginNode();
                        const args_begin = tree.beginNode();
                        try tree.pushStaticAtom("args");
                        const args_attrs = tree.beginNode();
                        try tree.endNode(args_begin, args_attrs);
                        try tree.endNode(header, attrs2);
                    } else {
                        const ty_header = ast.store.getTypeHeader(a.header) catch unreachable; // Malformed handled above
                        try ast.appendRegionInfoToSexprTree(env, tree, ty_header.region);
                        try tree.pushStringPair("name", ast.resolve(ty_header.name));
                        const attrs2 = tree.beginNode();

                        const args_begin = tree.beginNode();
                        try tree.pushStaticAtom("args");
                        const args_node = tree.beginNode();

                        for (ast.store.typeAnnoSlice(ty_header.args)) |b| {
                            const anno = ast.store.getTypeAnno(b);
                            try anno.pushToSExprTree(gpa, env, ast, tree);
                        }
                        try tree.endNode(args_begin, args_node);
                        try tree.endNode(header, attrs2);
                    }
                }

                try ast.store.getTypeAnno(a.anno).pushToSExprTree(gpa, env, ast, tree);

                // Add associated block if present
                if (a.associated) |assoc| {
                    const assoc_begin = tree.beginNode();
                    try tree.pushStaticAtom("associated");
                    try ast.appendRegionInfoToSexprTree(env, tree, assoc.region);
                    const assoc_attrs = tree.beginNode();

                    for (ast.store.statementSlice(assoc.statements)) |stmt_idx| {
                        const stmt = ast.store.getStatement(stmt_idx);
                        try stmt.pushToSExprTree(gpa, env, ast, tree);
                    }

                    try tree.endNode(assoc_begin, assoc_attrs);
                }

                try tree.endNode(begin, attrs);
            },
            .crash => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-crash");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                try ast.store.getExpr(a.expr).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .dbg => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-dbg");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                try ast.store.getExpr(a.expr).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .expect => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-expect");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                try ast.store.getExpr(a.body).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .@"for" => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-for");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // pattern
                try ast.store.getPattern(a.patt).pushToSExprTree(gpa, env, ast, tree);

                // expr
                try ast.store.getExpr(a.expr).pushToSExprTree(gpa, env, ast, tree);

                // body
                try ast.store.getExpr(a.body).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .@"while" => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-while");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // condition
                try ast.store.getExpr(a.cond).pushToSExprTree(gpa, env, ast, tree);

                // body
                try ast.store.getExpr(a.body).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .@"break" => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-break");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .@"return" => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-return");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                try ast.store.getExpr(a.expr).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .type_anno => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-type-anno");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("name", ast.resolve(a.name));
                const attrs = tree.beginNode();

                try ast.store.getTypeAnno(a.anno).pushToSExprTree(gpa, env, ast, tree);

                if (a.where) |where_coll| {
                    const where_node = tree.beginNode();
                    try tree.pushStaticAtom("where");
                    const attrs2 = tree.beginNode();
                    for (ast.store.whereClauseSlice(.{ .span = ast.store.getCollection(where_coll).span })) |clause_idx| {
                        const clause_child = ast.store.getWhereClause(clause_idx);
                        try clause_child.pushToSExprTree(gpa, env, ast, tree);
                    }
                    try tree.endNode(where_node, attrs2);
                }
                try tree.endNode(begin, attrs);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("s-malformed");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("tag", @tagName(a.reason));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }

    /// Extract the region from any Statement variant
    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .decl => |s| s.region,
            .@"var" => |s| s.region,
            .expr => |s| s.region,
            .import => |s| s.region,
            .type_decl => |s| s.region,
            .crash => |s| s.region,
            .dbg => |s| s.region,
            .expect => |s| s.region,
            .@"for" => |s| s.region,
            .@"while" => |s| s.region,
            .@"return" => |s| s.region,
            .@"break" => |s| s.region,
            .type_anno => |s| s.region,
            .malformed => |m| m.region,
            .file_import => |fi| fi.region,
        };
    }
};

/// Represents a block of statements.
pub const Block = struct {
    /// The statements that constitute the block
    statements: Statement.Span,
    scope: DeclIndex.ScopeIdx,
    region: TokenizedRegion,

    /// Push this Block to the SExprTree stack
    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("e-block");
        try ast.appendRegionInfoToSexprTree(env, tree, self.region);
        const attrs = tree.beginNode();

        const statements = tree.beginNode();
        try tree.pushStaticAtom("statements");
        const attrs2 = tree.beginNode();
        // Push all statements
        for (ast.store.statementSlice(self.statements)) |stmt_idx| {
            const stmt = ast.store.getStatement(stmt_idx);
            try stmt.pushToSExprTree(gpa, env, ast, tree);
        }
        try tree.endNode(statements, attrs2);

        try tree.endNode(begin, attrs);
    }
};

/// Represents associated items for nominal type declarations.
/// Associated items are the statements in the `.{ }` block after a nominal type,
/// e.g., `Foo := [A, B].{ x = 5 }`
pub const Associated = struct {
    /// The statements in the associated items block
    statements: Statement.Span,
    scope: DeclIndex.ScopeIdx,
    region: TokenizedRegion,
};

/// Represents a Pattern used in pattern matching.
pub const Pattern = union(enum) {
    ident: struct {
        ident_tok: Token.Idx,
        region: TokenizedRegion,
    },
    /// A mutable variable binding in a pattern, e.g., `var $x` in `|var $x, y|`
    var_ident: struct {
        ident_tok: Token.Idx,
        region: TokenizedRegion,
    },
    tag: struct {
        tag_tok: Token.Idx,
        args: Pattern.Span,
        qualifiers: Token.Span,
        /// True when the tag was written with an argument list, including an
        /// empty argument list such as `Tag()`.
        has_args: bool = false,
        /// True when written as `Type.(pattern)` — a nominal-value destructure
        /// (the inverse of `Type.(value)` construction), where `tag_tok` is the
        /// nominal type and `args` is the backing pattern. False for ordinary
        /// tag patterns like `Tag(args)` / `Module.Tag`.
        backing_value: bool = false,
        /// True when a nominal record destructure uses the record shorthand
        /// `Type.{ fields }` instead of the general `Type.({ fields })` form.
        /// This implies `backing_value` and exactly one record pattern in `args`.
        record_shorthand: bool = false,
        region: TokenizedRegion,
    },
    int: struct {
        number_tok: Token.Idx,
        literal: NumericLiteral.Idx,
        region: TokenizedRegion,
    },
    frac: struct {
        number_tok: Token.Idx,
        literal: NumericLiteral.Idx,
        region: TokenizedRegion,
    },
    typed_int: struct {
        number_tok: Token.Idx,
        type_ident: base.Ident.Idx,
        literal: NumericLiteral.Idx,
        region: TokenizedRegion,
    },
    typed_frac: struct {
        number_tok: Token.Idx,
        type_ident: base.Ident.Idx,
        literal: NumericLiteral.Idx,
        region: TokenizedRegion,
    },
    string: struct {
        string_tok: Token.Idx,
        region: TokenizedRegion,
        parts: PatternStringPart.Span,
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

    /// Extract the region from any Pattern variant
    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .ident => |p| p.region,
            .var_ident => |p| p.region,
            .tag => |p| p.region,
            .int => |p| p.region,
            .frac => |p| p.region,
            .typed_int => |p| p.region,
            .typed_frac => |p| p.region,
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
    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        switch (self) {
            .ident => |ident| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-ident");
                try ast.appendRegionInfoToSexprTree(env, tree, ident.region);

                // Add raw attribute
                const raw_begin = tree.beginNode();
                try tree.pushStaticAtom("raw");
                try tree.pushString(ast.resolve(ident.ident_tok));
                const attrs2 = tree.beginNode();
                try tree.endNode(raw_begin, attrs2);
                const attrs = tree.beginNode();

                try tree.endNode(begin, attrs);
            },
            .var_ident => |ident| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-var-ident");
                try ast.appendRegionInfoToSexprTree(env, tree, ident.region);

                // Add raw attribute
                const raw_begin = tree.beginNode();
                try tree.pushStaticAtom("raw");
                try tree.pushString(ast.resolve(ident.ident_tok));
                const attrs2 = tree.beginNode();
                try tree.endNode(raw_begin, attrs2);
                const attrs = tree.beginNode();

                try tree.endNode(begin, attrs);
            },
            .tag => |tag| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-tag");
                try ast.appendRegionInfoToSexprTree(env, tree, tag.region);
                try tree.pushStringPair("raw", ast.resolve(tag.tag_tok));
                const attrs = tree.beginNode();

                // Add arguments if there are any
                for (ast.store.patternSlice(tag.args)) |arg| {
                    try ast.store.getPattern(arg).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .int => |num| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-int");
                try ast.appendRegionInfoToSexprTree(env, tree, num.region);
                try tree.pushStringPair("raw", ast.resolve(num.number_tok));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .frac => |num| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-frac");
                try ast.appendRegionInfoToSexprTree(env, tree, num.region);
                try tree.pushStringPair("raw", ast.resolve(num.number_tok));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .typed_int => |num| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-typed-int");
                try ast.appendRegionInfoToSexprTree(env, tree, num.region);
                try tree.pushStringPair("raw", ast.resolve(num.number_tok));
                try tree.pushStringPair("type", env.getIdent(num.type_ident));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .typed_frac => |num| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-typed-frac");
                try ast.appendRegionInfoToSexprTree(env, tree, num.region);
                try tree.pushStringPair("raw", ast.resolve(num.number_tok));
                try tree.pushStringPair("type", env.getIdent(num.type_ident));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .string => |str| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-string");
                try ast.appendRegionInfoToSexprTree(env, tree, str.region);
                try tree.pushStringPair("raw", ast.resolve(str.string_tok));
                const attrs = tree.beginNode();
                for (ast.store.patternStringPartSlice(str.parts)) |part_idx| {
                    try ast.store.getPatternStringPart(part_idx).pushToSExprTree(env, ast, tree);
                }
                try tree.endNode(begin, attrs);
            },
            .single_quote => |sq| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-single-quote");
                try ast.appendRegionInfoToSexprTree(env, tree, sq.region);
                try tree.pushStringPair("raw", ast.resolve(sq.token));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .record => |rec| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-record");
                try ast.appendRegionInfoToSexprTree(env, tree, rec.region);
                const attrs = tree.beginNode();

                for (ast.store.patternRecordFieldSlice(rec.fields)) |field_idx| {
                    const field = ast.store.getPatternRecordField(field_idx);
                    const field_begin = tree.beginNode();
                    try tree.pushStaticAtom("field");
                    try ast.appendRegionInfoToSexprTree(env, tree, field.region);
                    if (field.name) |name_tok| {
                        try tree.pushStringPair("name", ast.resolve(name_tok));
                    }
                    try tree.pushBoolPair("rest", field.rest);
                    const attrs2 = tree.beginNode();

                    if (field.value) |value| {
                        try ast.store.getPattern(value).pushToSExprTree(gpa, env, ast, tree);
                    }

                    try tree.endNode(field_begin, attrs2);
                }

                try tree.endNode(begin, attrs);
            },
            .list => |list| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-list");
                try ast.appendRegionInfoToSexprTree(env, tree, list.region);
                const attrs = tree.beginNode();

                for (ast.store.patternSlice(list.patterns)) |pat| {
                    try ast.store.getPattern(pat).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .list_rest => |rest| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-list-rest");
                try ast.appendRegionInfoToSexprTree(env, tree, rest.region);

                if (rest.name) |name_tok| {
                    try tree.pushStringPair("name", ast.resolve(name_tok));
                }
                const attrs = tree.beginNode();

                try tree.endNode(begin, attrs);
            },
            .tuple => |tuple| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-tuple");
                try ast.appendRegionInfoToSexprTree(env, tree, tuple.region);
                const attrs = tree.beginNode();

                for (ast.store.patternSlice(tuple.patterns)) |pat| {
                    try ast.store.getPattern(pat).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .underscore => {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-underscore");
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .alternatives => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-alternatives");
                const attrs = tree.beginNode();

                for (ast.store.patternSlice(a.patterns)) |pat| {
                    try ast.store.getPattern(pat).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .as => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-as");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("name", ast.resolve(a.name));
                const attrs = tree.beginNode();

                try ast.store.getPattern(a.pattern).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-malformed");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("tag", @tagName(a.reason));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }
};

/// A part of a string pattern. Unlike expression strings, interpolation holes
/// in patterns are pattern binders or discards, never expressions.
pub const PatternStringPart = union(enum) {
    text: struct {
        token: Token.Idx,
        region: TokenizedRegion,
    },
    capture: struct {
        name: ?Token.Idx,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn pushToSExprTree(self: @This(), env: *const CommonEnv, ast: *const AST, tree: *SExprTree) Allocator.Error!void {
        switch (self) {
            .text => |text| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-string-text");
                try ast.appendRegionInfoToSexprTree(env, tree, text.region);
                try tree.pushStringPair("raw", ast.resolve(text.token));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .capture => |capture| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("p-string-capture");
                try ast.appendRegionInfoToSexprTree(env, tree, capture.region);
                if (capture.name) |name| {
                    try tree.pushStringPair("name", ast.resolve(name));
                } else {
                    try tree.pushBoolPair("discard", true);
                }
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
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
    pub fn pushToSExprTree(self: *const @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();

        // Push the node name
        try tree.pushStaticAtom("e-binop");
        try ast.appendRegionInfoToSexprTree(env, tree, self.region);

        // Push the operator as an attribute-style pair
        const op_begin = tree.beginNode();
        try tree.pushStaticAtom("op");
        try tree.pushString(ast.resolve(self.operator));
        const attrs2 = tree.beginNode();
        try tree.endNode(op_begin, attrs2);
        const attrs = tree.beginNode();

        // Push left operand
        try ast.store.getExpr(self.left).pushToSExprTree(gpa, env, ast, tree);

        // Push right operand
        try ast.store.getExpr(self.right).pushToSExprTree(gpa, env, ast, tree);

        try tree.endNode(begin, attrs);
    }
};

/// TODO
pub const Unary = struct {
    operator: Token.Idx,
    expr: Expr.Idx,
    region: TokenizedRegion,

    /// Push this Unary to the SExprTree stack
    pub fn pushToSExprTree(self: *const @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("unary");
        try tree.pushString(ast.resolve(self.operator));
        const attrs = tree.beginNode();

        try ast.store.getExpr(self.expr).pushToSExprTree(gpa, env, ast, tree);

        try tree.endNode(begin, attrs);
    }
};

/// Represents a delimited collection of other nodes
/// Records the source-requested layout of a comma-separated construct.
pub const CollectionLayout = enum(u8) {
    compact,
    expanded,
};

/// A delimited span of AST nodes and its explicitly requested layout.
pub const Collection = struct {
    span: base.DataSpan,
    region: TokenizedRegion,
    layout: CollectionLayout = .compact,

    pub const Idx = enum(u32) { _ };
};

/// Represents a Roc file.
pub const File = struct {
    header: Header.Idx,
    statements: Statement.Span,
    scope: DeclIndex.ScopeIdx,
    region: TokenizedRegion,

    /// Push this File to the SExprTree stack
    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("file");
        try ast.appendRegionInfoToSexprTree(env, tree, self.region);
        const attrs = tree.beginNode();

        // Push header
        const header = ast.store.getHeader(self.header);
        try header.pushToSExprTree(gpa, env, ast, tree);

        const begin2 = tree.beginNode();
        try tree.pushStaticAtom("statements");
        const attrs2 = tree.beginNode();
        for (ast.store.statementSlice(self.statements)) |stmt_id| {
            const stmt = ast.store.getStatement(stmt_id);
            try stmt.pushToSExprTree(gpa, env, ast, tree);
        }
        try tree.endNode(begin2, attrs2);

        try tree.endNode(begin, attrs);
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
        name: Token.Idx,
        requires_entries: RequiresEntry.Span, // [Model : model] for main : () -> { ... }
        exposes: Collection.Idx,
        packages: Collection.Idx,
        provides: SymbolMapEntry.Span, // provides { "roc_main": main_for_host! }
        hosted: SymbolMapEntry.Span, // hosted { "roc_stdout_line": Stdout.line! }
        targets: ?TargetsSection.Idx, // Required for new platforms, optional during migration
        region: TokenizedRegion,
    },
    hosted: struct {
        exposes: Collection.Idx,
        region: TokenizedRegion,
    },
    type_module: struct {
        region: TokenizedRegion,
    },
    default_app: struct {
        // Stores reference to the main! function
        // This will be filled in during canonicalization when main! is found
        main_fn_idx: u32, // Will store CIR Def.Idx
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };

    pub const AppHeaderRhs = packed struct { num_packages: u10, num_provides: u22 };

    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        switch (self) {
            .app => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("app");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Provides
                const provides_coll = ast.store.getCollection(a.provides);
                const provides_items = ast.store.exposedItemSlice(.{ .span = provides_coll.span });
                const provides_begin = tree.beginNode();
                try tree.pushStaticAtom("provides");
                try ast.appendRegionInfoToSexprTree(env, tree, provides_coll.region);
                const attrs2 = tree.beginNode();
                // Could push region info for provides_coll here if desired
                for (provides_items) |item_idx| {
                    const item = ast.store.getExposedItem(item_idx);
                    try item.pushToSExprTree(env, ast, tree);
                }
                try tree.endNode(provides_begin, attrs2);

                // Platform
                const platform = ast.store.getRecordField(a.platform_idx);
                try platform.pushToSExprTree(gpa, env, ast, tree);

                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                const packages_begin = tree.beginNode();
                try tree.pushStaticAtom("packages");
                try ast.appendRegionInfoToSexprTree(env, tree, packages_coll.region);
                const attrs3 = tree.beginNode();
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    try item.pushToSExprTree(gpa, env, ast, tree);
                }
                try tree.endNode(packages_begin, attrs3);

                try tree.endNode(begin, attrs);
            },
            .module => |module| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("module");
                try ast.appendRegionInfoToSexprTree(env, tree, module.region);
                const attrs = tree.beginNode();

                const exposes = ast.store.getCollection(module.exposes);
                const exposes_begin = tree.beginNode();
                try tree.pushStaticAtom("exposes");
                try ast.appendRegionInfoToSexprTree(env, tree, exposes.region);
                const attrs2 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    try item.pushToSExprTree(env, ast, tree);
                }
                try tree.endNode(exposes_begin, attrs2);

                try tree.endNode(begin, attrs);
            },
            .package => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("package");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Exposes
                const exposes = ast.store.getCollection(a.exposes);
                const exposes_begin = tree.beginNode();
                try tree.pushStaticAtom("exposes");
                try ast.appendRegionInfoToSexprTree(env, tree, exposes.region);
                const attrs2 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    try item.pushToSExprTree(env, ast, tree);
                }
                try tree.endNode(exposes_begin, attrs2);

                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                const packages_begin = tree.beginNode();
                try tree.pushStaticAtom("packages");
                try ast.appendRegionInfoToSexprTree(env, tree, packages_coll.region);
                const attrs3 = tree.beginNode();
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    try item.pushToSExprTree(gpa, env, ast, tree);
                }
                try tree.endNode(packages_begin, attrs3);

                try tree.endNode(begin, attrs);
            },
            .platform => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("platform");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("name", ast.resolve(a.name));
                const attrs = tree.beginNode();

                // Requires Entries (for-clause syntax)
                const requires_begin = tree.beginNode();
                try tree.pushStaticAtom("requires");
                const attrs3 = tree.beginNode();
                for (ast.store.requiresEntrySlice(a.requires_entries)) |entry_idx| {
                    const entry = ast.store.getRequiresEntry(entry_idx);
                    const entry_begin = tree.beginNode();
                    try tree.pushStaticAtom("requires-entry");
                    try ast.appendRegionInfoToSexprTree(env, tree, entry.region);
                    const entry_attrs = tree.beginNode();

                    // Type aliases
                    const aliases_begin = tree.beginNode();
                    try tree.pushStaticAtom("type-aliases");
                    const aliases_attrs = tree.beginNode();
                    for (ast.store.forClauseTypeAliasSlice(entry.type_aliases)) |alias_idx| {
                        const alias = ast.store.getForClauseTypeAlias(alias_idx);
                        const alias_begin = tree.beginNode();
                        try tree.pushStaticAtom("alias");
                        try tree.pushStringPair("name", ast.resolve(alias.alias_name));
                        try tree.pushStringPair("rigid", ast.resolve(alias.rigid_name));
                        const alias_attrs = tree.beginNode();
                        try tree.endNode(alias_begin, alias_attrs);
                    }
                    try tree.endNode(aliases_begin, aliases_attrs);

                    // Entrypoint name
                    try tree.pushStringPair("entrypoint", ast.resolve(entry.entrypoint_name));

                    // Type annotation
                    const type_anno = ast.store.getTypeAnno(entry.type_anno);
                    try type_anno.pushToSExprTree(gpa, env, ast, tree);

                    try tree.endNode(entry_begin, entry_attrs);
                }
                try tree.endNode(requires_begin, attrs3);

                // Exposes
                const exposes = ast.store.getCollection(a.exposes);
                const exposes_begin = tree.beginNode();
                try tree.pushStaticAtom("exposes");
                try ast.appendRegionInfoToSexprTree(env, tree, exposes.region);
                const attrs4 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    try item.pushToSExprTree(env, ast, tree);
                }
                try tree.endNode(exposes_begin, attrs4);

                // Packages
                const packages_coll = ast.store.getCollection(a.packages);
                const packages_items = ast.store.recordFieldSlice(.{ .span = packages_coll.span });
                const packages_begin = tree.beginNode();
                try tree.pushStaticAtom("packages");
                try ast.appendRegionInfoToSexprTree(env, tree, packages_coll.region);
                const attrs5 = tree.beginNode();
                for (packages_items) |item_idx| {
                    const item = ast.store.getRecordField(item_idx);
                    try item.pushToSExprTree(gpa, env, ast, tree);
                }
                try tree.endNode(packages_begin, attrs5);

                // Provides
                const provides_begin = tree.beginNode();
                try tree.pushStaticAtom("provides");
                const attrs6 = tree.beginNode();
                for (ast.store.symbolMapEntrySlice(a.provides)) |entry_idx| {
                    const entry = ast.store.getSymbolMapEntry(entry_idx);
                    const entry_begin = tree.beginNode();
                    try tree.pushStaticAtom("symbol-map-entry");
                    try tree.pushStringPair("symbol", ast.resolve(entry.symbol));
                    if (entry.module) |module_tok| {
                        try tree.pushStringPair("module", ast.resolve(module_tok));
                    }
                    try tree.pushStringPair("func", ast.resolve(entry.func));
                    const entry_attrs = tree.beginNode();
                    try tree.endNode(entry_begin, entry_attrs);
                }
                try tree.endNode(provides_begin, attrs6);

                // Hosted
                if (a.hosted.span.len > 0) {
                    const hosted_begin = tree.beginNode();
                    try tree.pushStaticAtom("hosted");
                    const attrs7 = tree.beginNode();
                    for (ast.store.symbolMapEntrySlice(a.hosted)) |entry_idx| {
                        const entry = ast.store.getSymbolMapEntry(entry_idx);
                        const entry_begin = tree.beginNode();
                        try tree.pushStaticAtom("symbol-map-entry");
                        try tree.pushStringPair("symbol", ast.resolve(entry.symbol));
                        if (entry.module) |module_tok| {
                            try tree.pushStringPair("module", ast.resolve(module_tok));
                        }
                        // Functions on nested type modules span several
                        // tokens (Foo.Idx.get!); cover everything after the
                        // module, stripping the leading dot.
                        const func_text = blk: {
                            const module_tok = entry.module orelse break :blk ast.resolve(entry.func);
                            if (entry.func == module_tok + 1) break :blk ast.resolve(entry.func);
                            const first = ast.tokens.resolve(module_tok + 1);
                            const last = ast.tokens.resolve(entry.func);
                            break :blk ast.env.source[first.start.offset + 1 .. last.end.offset];
                        };
                        try tree.pushStringPair("func", func_text);
                        const entry_attrs = tree.beginNode();
                        try tree.endNode(entry_begin, entry_attrs);
                    }
                    try tree.endNode(hosted_begin, attrs7);
                }

                try tree.endNode(begin, attrs);
            },
            .hosted => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("hosted");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                const exposes = ast.store.getCollection(a.exposes);
                const exposes_begin = tree.beginNode();
                try tree.pushStaticAtom("exposes");
                try ast.appendRegionInfoToSexprTree(env, tree, exposes.region);
                const attrs2 = tree.beginNode();
                for (ast.store.exposedItemSlice(.{ .span = exposes.span })) |exposed| {
                    const item = ast.store.getExposedItem(exposed);
                    try item.pushToSExprTree(env, ast, tree);
                }
                try tree.endNode(exposes_begin, attrs2);

                try tree.endNode(begin, attrs);
            },
            .type_module => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("type-mod");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .default_app => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("default-app");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();
                try tree.pushStringPairFmt("main-fn-idx", "{d}", .{a.main_fn_idx});
                try tree.endNode(begin, attrs);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("malformed-header");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("tag", @tagName(a.reason));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }

    /// Extract the region from any Header variant
    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .app => |a| a.region,
            .module => |m| m.region,
            .package => |p| p.region,
            .platform => |p| p.region,
            .hosted => |h| h.region,
            .type_module => |t| t.region,
            .default_app => |d| d.region,
            .malformed => |m| m.region,
        };
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
        qualifiers: Token.Span,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn pushToSExprTree(self: @This(), env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        switch (self) {
            .lower_ident => |i| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("exposed-lower-ident");
                try ast.appendRegionInfoToSexprTree(env, tree, i.region);

                const attrs = tree.beginNode();

                const text_begin = tree.beginNode();
                try tree.pushStaticAtom("text");
                const ident_idx = ast.tokens.resolveIdentifier(i.ident) orelse {
                    // Fallback for malformed tokens
                    try tree.pushString("MALFORMED");
                    const attrs2 = tree.beginNode();
                    try tree.endNode(text_begin, attrs2);
                    try tree.endNode(begin, attrs);
                    return;
                };
                const text = env.getIdent(ident_idx);
                try tree.pushString(text);
                const attrs2 = tree.beginNode();
                try tree.endNode(text_begin, attrs2);

                // as attribute if present
                if (i.as) |a| {
                    const as_ident = ast.tokens.resolveIdentifier(a) orelse {
                        try tree.pushStringPair("as", "MALFORMED");
                        try tree.endNode(begin, attrs);
                        return;
                    };
                    const as_text = env.getIdent(as_ident);
                    try tree.pushStringPair("as", as_text);
                }

                try tree.endNode(begin, attrs);
            },
            .upper_ident => |i| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("exposed-upper-ident");
                try ast.appendRegionInfoToSexprTree(env, tree, i.region);

                // text attribute
                const token = ast.tokens.tokens.get(i.ident);
                const text = env.getIdent(token.extra.interned);
                try tree.pushStringPair("text", text);

                // as attribute if present
                if (i.as) |a| {
                    const as_tok = ast.tokens.tokens.get(a);
                    const as_text = env.getIdent(as_tok.extra.interned);
                    try tree.pushStringPair("as", as_text);
                }

                const attrs = tree.beginNode();

                try tree.endNode(begin, attrs);
            },
            .upper_ident_star => |i| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("exposed-upper-ident-star");
                try ast.appendRegionInfoToSexprTree(env, tree, i.region);

                // text attribute
                const strip_tokens = [_]Token.Tag{ .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent };
                const text = ast.resolveQualifiedName(i.qualifiers, i.ident, &strip_tokens);
                try tree.pushStringPair("text", text);
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .malformed => |m| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("exposed-malformed");
                try ast.appendRegionInfoToSexprTree(env, tree, m.region);

                // reason attribute
                const reason_begin = tree.beginNode();
                try tree.pushStaticAtom("reason");
                try tree.pushString(@tagName(m.reason));
                const attrs2 = tree.beginNode();
                try tree.endNode(reason_begin, attrs2);

                // region info
                try ast.appendRegionInfoToSexprTree(env, tree, m.region);

                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }

    /// Extract the region from any ExposedItem variant
    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .lower_ident => |i| i.region,
            .upper_ident => |i| i.region,
            .upper_ident_star => |i| i.region,
            .malformed => |m| m.region,
        };
    }
};

/// A targets section in a platform header
pub const TargetsSection = struct {
    inputs_dir: ?Token.Idx, // "inputs_dir:" directive string literal
    entries: TargetEntry.Span, // per-target entries
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
};

/// An entry mapping a linker symbol string to a platform function:
/// `"roc_main": main_for_host!` (provides) or `"roc_stdout_line": Stdout.line!` (hosted)
pub const SymbolMapEntry = struct {
    symbol: Token.Idx, // StringPart token holding the linker symbol text
    module: ?Token.Idx, // UpperIdent for qualified functions (e.g. Stdout); null for bare ones
    func: Token.Idx, // LowerIdent (or NoSpaceDotLowerIdent when qualified) naming the function
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct {
        span: base.DataSpan,
        region: TokenizedRegion = TokenizedRegion.empty(),
        layout: CollectionLayout = .compact,
    };
};

/// Single target entry: x64musl: { inputs: ["crt1.o", "host.o", app], output: Exe }
pub const TargetEntry = struct {
    target: Token.Idx, // LowerIdent token (e.g., x64musl, arm64mac)
    config: TargetConfig.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// File item in target list
pub const TargetFile = union(enum) {
    string_literal: ?Token.Idx, // Content token for "crt1.o"; null for ""
    special_ident: Token.Idx, // app, win_gui
    malformed: struct { reason: Diagnostic.Tag, region: TokenizedRegion },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// Per-target configuration inside a target entry record.
pub const TargetConfig = struct {
    entries: TargetConfigEntry.Span,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
};

/// A single field in a target configuration record.
pub const TargetConfigEntry = struct {
    name: Token.Idx,
    value: TargetConfigValue.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// Literal or top-level identifier syntax accepted in target configuration.
pub const TargetConfigValue = union(enum) {
    int_literal: Token.Idx,
    string_literal: ?Token.Idx,
    tag_literal: Token.Idx,
    ident: Token.Idx,
    list: TargetConfigValue.Span,
    files: TargetFile.Span,
    malformed: struct { reason: Diagnostic.Tag, region: TokenizedRegion },

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// A type alias mapping in a for-clause: Model : model
/// Maps an uppercase alias (Model) to a lowercase rigid variable (model)
pub const ForClauseTypeAlias = struct {
    /// The alias name token (e.g., "Model") - UpperIdent
    alias_name: Token.Idx,
    /// The rigid variable name token (e.g., "model") - LowerIdent
    rigid_name: Token.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// A requires entry with for-clause: [Model : model] for main : () -> { ... }
pub const RequiresEntry = struct {
    /// Type aliases: [Model : model, Foo : foo]
    type_aliases: ForClauseTypeAlias.Span,
    /// The entrypoint name token (e.g., "main") - LowerIdent
    entrypoint_name: Token.Idx,
    /// The type annotation for this entrypoint
    type_anno: TypeAnno.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
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
    underscore_type_var: struct {
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
    tag_union: struct {
        tags: TypeAnno.Span,
        /// Extension for open tag unions
        ext: TagUnionExt,
        region: TokenizedRegion,
    },
    tuple: struct {
        annos: TypeAnno.Span,
        region: TokenizedRegion,
    },
    record: struct {
        fields: AnnoRecordField.Span,
        ext: RecordExt,
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

    /// Extension type for open tag unions
    pub const TagUnionExt = union(enum) {
        /// Closed tag union: `[A, B, C]`
        closed,
        /// Anonymous open tag union: `[A, B, ..]` - stores the DoubleDot token index
        open: Token.Idx,
        /// Named open tag union: `[A, B, ..ext]`
        named: struct { anno: TypeAnno.Idx, region: TokenizedRegion },
    };

    pub const TagUnionRhs = packed struct {
        /// 0 = closed, 1 = anonymous open, 2 = named open
        ext_kind: u2,
        _padding: u30 = 0,
    };

    /// Extension type for open records
    pub const RecordExt = union(enum) {
        /// Closed record: `{ name: Str }`
        closed,
        /// Anonymous open record: `{ name: Str, .. }` - stores the DoubleDot token index
        open: Token.Idx,
        /// Named open record: `{ name: Str, ..ext }`
        named: struct { anno: TypeAnno.Idx, region: TokenizedRegion },
    };

    pub const RecordRhs = packed struct {
        /// 0 = closed, 1 = anonymous open, 2 = named open
        ext_kind: u2,
        _padding: u30 = 0,
    };

    pub const TypeAnnoFnRhs = packed struct { effectful: u1, args_len: u31 };

    /// Extract the region from any TypeAnno variant
    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        switch (self) {
            .apply => |a| return a.region,
            .ty_var => |tv| return tv.region,
            .underscore_type_var => |utv| return utv.region,
            .underscore => |u| return u.region,
            .ty => |t| return t.region,
            .tag_union => |tu| return tu.region,
            .tuple => |t| return t.region,
            .record => |r| return r.region,
            .@"fn" => |f| return f.region,
            .parens => |p| return p.region,
            .malformed => |m| return m.region,
        }
    }

    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        switch (self) {
            .apply => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-apply");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.typeAnnoSlice(a.args)) |b| {
                    try ast.store.getTypeAnno(b).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .ty_var => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-var");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("raw", ast.resolve(a.tok));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .underscore_type_var => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("underscore-ty-var");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("raw", ast.resolve(a.tok));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .underscore => {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("_");
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .ty => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);

                // Resolve the fully qualified name
                const strip_tokens = [_]Token.Tag{.NoSpaceDotUpperIdent};
                const fully_qualified_name = ast.resolveQualifiedName(a.qualifiers, a.token, &strip_tokens);
                try tree.pushStringPair("name", fully_qualified_name);
                const attrs = tree.beginNode();

                try tree.endNode(begin, attrs);
            },
            .tag_union => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-tag-union");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                const tags = ast.store.typeAnnoSlice(a.tags);
                const tags_begin = tree.beginNode();
                try tree.pushStaticAtom("tags");
                const attrs2 = tree.beginNode();
                for (tags) |tag_idx| {
                    try ast.store.getTypeAnno(tag_idx).pushToSExprTree(gpa, env, ast, tree);
                }
                try tree.endNode(tags_begin, attrs2);

                switch (a.ext) {
                    .named => |named| {
                        try ast.store.getTypeAnno(named.anno).pushToSExprTree(gpa, env, ast, tree);
                    },
                    .open => {
                        try tree.pushStaticAtom("..");
                    },
                    .closed => {},
                }

                try tree.endNode(begin, attrs);
            },
            .tuple => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-tuple");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.typeAnnoSlice(a.annos)) |b| {
                    try ast.store.getTypeAnno(b).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .record => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-record");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.annoRecordFieldSlice(a.fields)) |f_idx| {
                    const field = ast.store.getAnnoRecordField(f_idx) catch |err| switch (err) {
                        error.MalformedNode => {
                            // Create a malformed-field node for debugging
                            const malformed_begin = tree.beginNode();
                            try tree.pushStaticAtom("malformed-field");
                            const attrs2 = tree.beginNode();
                            try tree.endNode(malformed_begin, attrs2);
                            continue;
                        },
                    };
                    try field.pushToSExprTree(gpa, env, ast, tree);
                }

                // Output extension if present
                switch (a.ext) {
                    .named => |named| {
                        const ext_begin = tree.beginNode();
                        try tree.pushStaticAtom("ty-record-ext");
                        const ext_attrs = tree.beginNode();
                        try ast.store.getTypeAnno(named.anno).pushToSExprTree(gpa, env, ast, tree);
                        try tree.endNode(ext_begin, ext_attrs);
                    },
                    .open => {
                        const ext_begin = tree.beginNode();
                        try tree.pushStaticAtom("ty-record-ext");
                        const ext_attrs = tree.beginNode();
                        try tree.pushStaticAtom("..");
                        try tree.endNode(ext_begin, ext_attrs);
                    },
                    .closed => {},
                }

                try tree.endNode(begin, attrs);
            },
            .@"fn" => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-fn");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // arguments
                for (ast.store.typeAnnoSlice(a.args)) |b| {
                    try ast.store.getTypeAnno(b).pushToSExprTree(gpa, env, ast, tree);
                }

                // return value
                try ast.store.getTypeAnno(a.ret).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .parens => |a| {
                // Ignore parens, use inner
                try ast.store.getTypeAnno(a.anno).pushToSExprTree(gpa, env, ast, tree);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("ty-malformed");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("tag", @tagName(a.reason));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
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

    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("anno-record-field");
        try ast.appendRegionInfoToSexprTree(env, tree, self.region);
        try tree.pushStringPair("name", ast.resolve(self.name));
        const attrs = tree.beginNode();

        const anno = ast.store.getTypeAnno(self.ty);
        try anno.pushToSExprTree(gpa, env, ast, tree);

        try tree.endNode(begin, attrs);
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
    /// convert : a -> b where [a.to_b : a -> b]
    /// decode : List(U8) -> a where [a.decode : List(U8) -> a]
    /// hash : a -> U64 where [a.hash : a -> U64]
    /// ```
    mod_method: struct {
        var_tok: Token.Idx,
        name_tok: Token.Idx,
        args: Collection.Idx,
        ret_anno: TypeAnno.Idx,
        effectful: bool,
        region: TokenizedRegion,
    },

    /// Module type alias constraint.
    ///
    /// Specifies that a type variable must satisfy the constraints for an alias type.
    /// This is useful to avoid writing out the constraints repeatedly which can be cumbersome and error prone
    ///
    /// Example:
    /// ```roc
    /// Sort(a) : a where [a.order : elem, elem -> [LT, EQ, GT]]
    ///
    /// sort : List(elem) -> List(elem) where [elem.Sort]
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
    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        switch (self) {
            .mod_method => |m| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("method");
                try ast.appendRegionInfoToSexprTree(env, tree, m.region);

                try tree.pushStringPair("module-of", ast.resolve(m.var_tok));

                // remove preceding dot
                const method_name = ast.resolve(m.name_tok)[1..];
                try tree.pushStringPair("name", method_name);
                if (m.effectful) try tree.pushBoolPair("effectful", true);
                const attrs = tree.beginNode();

                const args_begin = tree.beginNode();
                try tree.pushStaticAtom("args");
                const attrs2 = tree.beginNode();
                const args = ast.store.typeAnnoSlice(.{ .span = ast.store.getCollection(m.args).span });
                for (args) |arg| {
                    try ast.store.getTypeAnno(arg).pushToSExprTree(gpa, env, ast, tree);
                }
                try tree.endNode(args_begin, attrs2);

                try ast.store.getTypeAnno(m.ret_anno).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .mod_alias => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("alias");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);

                try tree.pushStringPair("module-of", ast.resolve(a.var_tok));

                // remove preceding dot
                const alias_name = ast.resolve(a.name_tok)[1..];
                try tree.pushStringPair("name", alias_name);

                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .malformed => |m| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("malformed");
                try ast.appendRegionInfoToSexprTree(env, tree, m.region);
                try tree.pushStringPair("reason", @tagName(m.reason));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }

    /// Extract the region from any WhereClause variant
    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        switch (self) {
            .mod_method => |m| return m.region,
            .mod_alias => |a| return a.region,
            .malformed => |m| return m.region,
        }
    }

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };
};

/// Represents an expression.
pub const Expr = union(enum) {
    int: struct {
        token: Token.Idx,
        literal: NumericLiteral.Idx,
        region: TokenizedRegion,
    },
    frac: struct {
        token: Token.Idx,
        literal: NumericLiteral.Idx,
        region: TokenizedRegion,
    },
    /// An integer with an explicit type annotation: `123.U64`
    /// Deprecated suffix syntax such as `123u64` is desugared to this form during parsing.
    typed_int: struct {
        token: Token.Idx,
        type_ident: base.Ident.Idx,
        literal: NumericLiteral.Idx,
        region: TokenizedRegion,
    },
    /// A fractional number with an explicit type annotation: `3.14.Dec`
    /// Deprecated suffix syntax such as `3.14dec` is desugared to this form during parsing.
    typed_frac: struct {
        token: Token.Idx,
        type_ident: base.Ident.Idx,
        literal: NumericLiteral.Idx,
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
    string: StringLike,
    multiline_string: StringLike,
    typed_string: TypedStringLike,
    typed_multiline_string: TypedStringLike,
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
    /// Record field access, written `receiver.field` with no argument list.
    /// Calling a function stored in the field is a separate ordinary apply,
    /// written `(receiver.field)(args)`.
    field_access: BinOp,
    /// Attached method call, written `receiver.method(args)`.
    method_call: struct {
        receiver: Expr.Idx,
        method_token: Token.Idx,
        args: Expr.Span,
        region: TokenizedRegion,
    },
    /// Tuple element access: `tuple.0`, `tuple.1`, etc.
    tuple_access: struct {
        /// The tuple expression being accessed
        expr: Expr.Idx,
        /// The token containing the element index (NoSpaceDotInt or DotInt)
        elem_token: Token.Idx,
        region: TokenizedRegion,
    },
    arrow_call: BinOp,
    bin_op: BinOp,
    suffix_single_question: Unary,
    unary_op: Unary,
    if_then_else: struct {
        condition: Expr.Idx,
        then: Expr.Idx,
        @"else": Expr.Idx,
        region: TokenizedRegion,
    },
    if_without_else: struct {
        condition: Expr.Idx,
        then: Expr.Idx,
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
        fields: RecordField.Span,
        region: TokenizedRegion,
    },
    nominal_record: struct {
        mapper: Expr.Idx,
        backing: Expr.Idx,
        region: TokenizedRegion,
    },
    nominal_apply: struct {
        mapper: Expr.Idx,
        args: Expr.Span,
        region: TokenizedRegion,
    },
    ellipsis: struct {
        region: TokenizedRegion,
    },
    @"break": struct {
        region: TokenizedRegion,
    },
    @"return": struct {
        expr: Expr.Idx,
        region: TokenizedRegion,
    },
    block: Block,
    for_expr: struct {
        patt: Pattern.Idx,
        expr: Expr.Idx,
        body: Expr.Idx,
        region: TokenizedRegion,
    },
    malformed: struct {
        reason: Diagnostic.Tag,
        region: TokenizedRegion,
    },

    pub const StringLike = struct {
        token: Token.Idx,
        region: TokenizedRegion,
        parts: Expr.Span,
    };

    /// A string literal with an explicit type suffix, e.g. `"foo".MyType`.
    pub const TypedStringLike = struct {
        token: Token.Idx,
        type_ident: base.Ident.Idx,
        region: TokenizedRegion,
        parts: Expr.Span,
    };

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn as_string_part_region(self: @This()) Allocator.Error!TokenizedRegion {
        switch (self) {
            .string_part => |part| return part.region,
            else => return error.ExpectedStringPartRegion,
        }
    }

    /// Extract the region from any Expr variant
    pub fn to_tokenized_region(self: @This()) TokenizedRegion {
        return switch (self) {
            .ident => |e| e.region,
            .int => |e| e.region,
            .frac => |e| e.region,
            .typed_int => |e| e.region,
            .typed_frac => |e| e.region,
            .string => |e| e.region,
            .multiline_string => |e| e.region,
            .typed_string => |e| e.region,
            .typed_multiline_string => |e| e.region,
            .tag => |e| e.region,
            .list => |e| e.region,
            .record => |e| e.region,
            .tuple => |e| e.region,
            .field_access => |e| e.region,
            .method_call => |e| e.region,
            .tuple_access => |e| e.region,
            .arrow_call => |e| e.region,
            .lambda => |e| e.region,
            .record_updater => |e| e.region,
            .bin_op => |e| e.region,
            .unary_op => |e| e.region,
            .suffix_single_question => |e| e.region,
            .apply => |e| e.region,
            .if_then_else => |e| e.region,
            .if_without_else => |e| e.region,
            .match => |e| e.region,
            .dbg => |e| e.region,
            .block => |e| e.region,
            .record_builder => |e| e.region,
            .nominal_record => |e| e.region,
            .nominal_apply => |e| e.region,
            .ellipsis => |e| e.region,
            .@"break" => |e| e.region,
            .@"return" => |e| e.region,
            .for_expr => |e| e.region,
            .malformed => |e| e.region,
            .string_part => |e| e.region,
            .single_quote => |e| e.region,
        };
    }

    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        switch (self) {
            .int => |int| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-int");
                try ast.appendRegionInfoToSexprTree(env, tree, int.region);

                // Add raw attribute
                const raw_begin = tree.beginNode();
                try tree.pushStaticAtom("raw");
                try tree.pushString(ast.resolve(int.token));
                const attrs2 = tree.beginNode();
                try tree.endNode(raw_begin, attrs2);
                const attrs = tree.beginNode();

                try tree.endNode(begin, attrs);
            },
            .frac => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-frac");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("raw", ast.resolve(a.token));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .typed_int => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-typed-int");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("raw", ast.resolve(a.token));
                try tree.pushStringPair("type", env.getIdent(a.type_ident));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .typed_frac => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-typed-frac");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("raw", ast.resolve(a.token));
                try tree.pushStringPair("type", env.getIdent(a.type_ident));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .single_quote => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-single-quote");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("raw", ast.resolve(a.token));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .string_part => |sp| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-string-part");
                try ast.appendRegionInfoToSexprTree(env, tree, sp.region);
                const raw = tree.beginNode();
                try tree.pushStaticAtom("raw");
                try tree.pushString(ast.resolve(sp.token));
                const attrs2 = tree.beginNode();
                try tree.endNode(raw, attrs2);
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .string => |str| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-string");
                try ast.appendRegionInfoToSexprTree(env, tree, str.region);
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(str.parts)) |part_id| {
                    const part_expr = ast.store.getExpr(part_id);
                    try part_expr.pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .multiline_string => |str| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-multiline-string");
                try ast.appendRegionInfoToSexprTree(env, tree, str.region);
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(str.parts)) |part_id| {
                    const part_expr = ast.store.getExpr(part_id);
                    try part_expr.pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .typed_string => |str| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-typed-string");
                try ast.appendRegionInfoToSexprTree(env, tree, str.region);
                try tree.pushStringPair("type", env.getIdent(str.type_ident));
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(str.parts)) |part_id| {
                    const part_expr = ast.store.getExpr(part_id);
                    try part_expr.pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .typed_multiline_string => |str| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-typed-multiline-string");
                try ast.appendRegionInfoToSexprTree(env, tree, str.region);
                try tree.pushStringPair("type", env.getIdent(str.type_ident));
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(str.parts)) |part_id| {
                    const part_expr = ast.store.getExpr(part_id);
                    try part_expr.pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .list => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-list");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(a.items)) |b| {
                    try ast.store.getExpr(b).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .tuple => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-tuple");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                for (ast.store.exprSlice(a.items)) |b| {
                    try ast.store.getExpr(b).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .record => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-record");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Add extension if present
                if (a.ext) |ext_idx| {
                    const ext_wrapper = tree.beginNode();
                    try tree.pushStaticAtom("ext");
                    try ast.store.getExpr(ext_idx).pushToSExprTree(gpa, env, ast, tree);
                    try tree.endNode(ext_wrapper, attrs);
                }

                for (ast.store.recordFieldSlice(a.fields)) |field_idx| {
                    const record_field = ast.store.getRecordField(field_idx);
                    const field_node = tree.beginNode();
                    try tree.pushStaticAtom("field");
                    try tree.pushStringPair("field", ast.resolve(record_field.name));
                    const attrs2 = tree.beginNode();
                    if (record_field.value) |value_id| {
                        try ast.store.getExpr(value_id).pushToSExprTree(gpa, env, ast, tree);
                    }
                    try tree.endNode(field_node, attrs2);
                }

                try tree.endNode(begin, attrs);
            },
            .tag => |tag| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-tag");
                try ast.appendRegionInfoToSexprTree(env, tree, tag.region);

                // Resolve the fully qualified name
                const strip_tokens = [_]Token.Tag{.NoSpaceDotUpperIdent};
                const fully_qualified_name = ast.resolveQualifiedName(tag.qualifiers, tag.token, &strip_tokens);
                try tree.pushStringPair("raw", fully_qualified_name);
                const attrs = tree.beginNode();

                try tree.endNode(begin, attrs);
            },
            .lambda => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-lambda");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                const args = tree.beginNode();
                try tree.pushStaticAtom("args");
                const attrs2 = tree.beginNode();
                // Push args (patterns)
                for (ast.store.patternSlice(a.args)) |pat| {
                    try ast.store.getPattern(pat).pushToSExprTree(gpa, env, ast, tree);
                }
                try tree.endNode(args, attrs2);

                // Push body
                try ast.store.getExpr(a.body).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .apply => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-apply");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push function
                try ast.store.getExpr(a.@"fn").pushToSExprTree(gpa, env, ast, tree);

                // Push arguments
                for (ast.store.exprSlice(a.args)) |arg_id| {
                    try ast.store.getExpr(arg_id).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .record_updater => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-record-updater");
                try tree.pushString(ast.resolve(a.token));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .if_then_else => |stmt| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-if-then-else");
                try ast.appendRegionInfoToSexprTree(env, tree, stmt.region);
                const attrs = tree.beginNode();

                try ast.store.getExpr(stmt.condition).pushToSExprTree(gpa, env, ast, tree);
                try ast.store.getExpr(stmt.then).pushToSExprTree(gpa, env, ast, tree);
                try ast.store.getExpr(stmt.@"else").pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .if_without_else => |stmt| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-if-without-else");
                try ast.appendRegionInfoToSexprTree(env, tree, stmt.region);
                const attrs = tree.beginNode();

                try ast.store.getExpr(stmt.condition).pushToSExprTree(gpa, env, ast, tree);
                try ast.store.getExpr(stmt.then).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .match => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-match");
                const attrs = tree.beginNode();

                try ast.store.getExpr(a.expr).pushToSExprTree(gpa, env, ast, tree);

                const branches = tree.beginNode();
                try tree.pushStaticAtom("branches");
                const attrs2 = tree.beginNode();

                for (ast.store.matchBranchSlice(a.branches)) |branch_idx| {
                    const branch = ast.store.getBranch(branch_idx);
                    try branch.pushToSExprTree(gpa, env, ast, tree);
                }
                try tree.endNode(branches, attrs2);

                try tree.endNode(begin, attrs);
            },
            .ident => |ident| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-ident");
                try ast.appendRegionInfoToSexprTree(env, tree, ident.region);

                // Add raw attribute
                const raw_begin = tree.beginNode();
                try tree.pushStaticAtom("raw");
                // Resolve the fully qualified name
                const strip_tokens = [_]Token.Tag{ .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent };
                const fully_qualified_name = ast.resolveQualifiedName(ident.qualifiers, ident.token, &strip_tokens);
                try tree.pushString(fully_qualified_name);
                const attrs2 = tree.beginNode();
                try tree.endNode(raw_begin, attrs2);

                const attrs = tree.beginNode();

                try tree.endNode(begin, attrs);
            },
            .dbg => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-dbg");
                const attrs = tree.beginNode();

                try ast.store.getExpr(a.expr).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .record_builder => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-record-builder");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push mapper (the type suffix)
                const mapper_wrapper = tree.beginNode();
                try tree.pushStaticAtom("mapper");
                try ast.store.getExpr(a.mapper).pushToSExprTree(gpa, env, ast, tree);
                const mapper_attrs = tree.beginNode();
                try tree.endNode(mapper_wrapper, mapper_attrs);

                // Push fields
                for (ast.store.recordFieldSlice(a.fields)) |field_idx| {
                    const record_field = ast.store.getRecordField(field_idx);
                    const field_node = tree.beginNode();
                    try tree.pushStaticAtom("field");
                    try tree.pushStringPair("field", ast.resolve(record_field.name));
                    const attrs2 = tree.beginNode();
                    if (record_field.value) |value_id| {
                        try ast.store.getExpr(value_id).pushToSExprTree(gpa, env, ast, tree);
                    }
                    try tree.endNode(field_node, attrs2);
                }

                try tree.endNode(begin, attrs);
            },
            .nominal_record => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-nominal-record");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                const mapper_wrapper = tree.beginNode();
                try tree.pushStaticAtom("mapper");
                try ast.store.getExpr(a.mapper).pushToSExprTree(gpa, env, ast, tree);
                const mapper_attrs = tree.beginNode();
                try tree.endNode(mapper_wrapper, mapper_attrs);

                const backing_wrapper = tree.beginNode();
                try tree.pushStaticAtom("backing");
                try ast.store.getExpr(a.backing).pushToSExprTree(gpa, env, ast, tree);
                const backing_attrs = tree.beginNode();
                try tree.endNode(backing_wrapper, backing_attrs);

                try tree.endNode(begin, attrs);
            },
            .nominal_apply => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-nominal-apply");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                const mapper_wrapper = tree.beginNode();
                try tree.pushStaticAtom("mapper");
                try ast.store.getExpr(a.mapper).pushToSExprTree(gpa, env, ast, tree);
                const mapper_attrs = tree.beginNode();
                try tree.endNode(mapper_wrapper, mapper_attrs);

                for (ast.store.exprSlice(a.args)) |arg_idx| {
                    try ast.store.getExpr(arg_idx).pushToSExprTree(gpa, env, ast, tree);
                }

                try tree.endNode(begin, attrs);
            },
            .ellipsis => {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-ellipsis");
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .@"break" => |b| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-break");
                try ast.appendRegionInfoToSexprTree(env, tree, b.region);
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .@"return" => |ret| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-return");
                try ast.appendRegionInfoToSexprTree(env, tree, ret.region);
                const attrs = tree.beginNode();
                try ast.store.getExpr(ret.expr).pushToSExprTree(gpa, env, ast, tree);
                try tree.endNode(begin, attrs);
            },
            .block => |block| {
                // Delegate to Block.pushToSExprTree
                try block.pushToSExprTree(gpa, env, ast, tree);
            },
            .bin_op => |a| {
                try a.pushToSExprTree(gpa, env, ast, tree);
            },
            .field_access => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-field-access");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push left expression
                try ast.store.getExpr(a.left).pushToSExprTree(gpa, env, ast, tree);

                // Push right expression
                try ast.store.getExpr(a.right).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .method_call => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-method-call");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("method", ast.resolve(a.method_token));
                const attrs = tree.beginNode();

                const receiver = tree.beginNode();
                try tree.pushStaticAtom("receiver");
                const receiver_attrs = tree.beginNode();
                try ast.store.getExpr(a.receiver).pushToSExprTree(gpa, env, ast, tree);
                try tree.endNode(receiver, receiver_attrs);

                const args = tree.beginNode();
                try tree.pushStaticAtom("args");
                const args_attrs = tree.beginNode();
                for (ast.store.exprSlice(a.args)) |arg_id| {
                    try ast.store.getExpr(arg_id).pushToSExprTree(gpa, env, ast, tree);
                }
                try tree.endNode(args, args_attrs);

                try tree.endNode(begin, attrs);
            },
            .tuple_access => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-tuple-access");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push the tuple expression
                try ast.store.getExpr(a.expr).pushToSExprTree(gpa, env, ast, tree);

                // Push the element index
                try tree.pushString(ast.resolve(a.elem_token));

                try tree.endNode(begin, attrs);
            },
            .arrow_call => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-arrow-call");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push left expression
                try ast.store.getExpr(a.left).pushToSExprTree(gpa, env, ast, tree);

                // Push right expression
                try ast.store.getExpr(a.right).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .unary_op => |a| {
                try a.pushToSExprTree(gpa, env, ast, tree);
            },
            .malformed => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-malformed");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                try tree.pushStringPair("reason", @tagName(a.reason));
                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .suffix_single_question => |a| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-question-suffix");
                try ast.appendRegionInfoToSexprTree(env, tree, a.region);
                const attrs = tree.beginNode();

                // Push child expression
                try ast.store.getExpr(a.expr).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
            .for_expr => |f| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("e-for");
                try ast.appendRegionInfoToSexprTree(env, tree, f.region);
                const attrs = tree.beginNode();

                // Push pattern
                try ast.store.getPattern(f.patt).pushToSExprTree(gpa, env, ast, tree);

                // Push list expression
                try ast.store.getExpr(f.expr).pushToSExprTree(gpa, env, ast, tree);

                // Push body expression
                try ast.store.getExpr(f.body).pushToSExprTree(gpa, env, ast, tree);

                try tree.endNode(begin, attrs);
            },
        }
    }
};

/// TODO
pub const PatternRecordField = struct {
    /// The field name, or `null` for a bare rest pattern (`..`), which has no name.
    name: ?Token.Idx,
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

    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("record-field");
        try ast.appendRegionInfoToSexprTree(env, tree, self.region);
        const name = tree.beginNode();
        try tree.pushStaticAtom("name");
        try tree.pushString(ast.resolve(self.name));
        const attrs2 = tree.beginNode();
        try tree.endNode(name, attrs2);
        const attrs = tree.beginNode();

        if (self.value) |idx| {
            const value = ast.store.getExpr(idx);
            try value.pushToSExprTree(gpa, env, ast, tree);
        }

        try tree.endNode(begin, attrs);
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
    guard: ?Expr.Idx,
    region: TokenizedRegion,

    pub const Idx = enum(u32) { _ };
    pub const Span = struct { span: base.DataSpan };

    pub fn pushToSExprTree(self: @This(), gpa: std.mem.Allocator, env: *const CommonEnv, ast: *const AST, tree: *SExprTree) std.mem.Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("branch");
        try ast.appendRegionInfoToSexprTree(env, tree, self.region);
        const attrs = tree.beginNode();

        try ast.store.getPattern(self.pattern).pushToSExprTree(gpa, env, ast, tree);
        if (self.guard) |guard| {
            const guard_begin = tree.beginNode();
            try tree.pushStaticAtom("guard");
            const guard_attrs = tree.beginNode();
            try ast.store.getExpr(guard).pushToSExprTree(gpa, env, ast, tree);
            try tree.endNode(guard_begin, guard_attrs);
        }
        try ast.store.getExpr(self.body).pushToSExprTree(gpa, env, ast, tree);

        try tree.endNode(begin, attrs);
    }
};
