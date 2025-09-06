const std = @import("std");
const base = @import("base");
const reporting = @import("reporting");

/// Shared error reporting logic between old and new parsers
/// This centralizes diagnostic formatting to ensure consistency
pub const CommonEnv = struct {
    source: []const u8,
    line_starts: []const u32,
};

/// Diagnostic tags shared between parsers
pub const DiagnosticTag = enum {
    // Platform/Header errors
    multiple_platforms,
    no_platform,
    missing_header,
    missing_arrow,
    expected_exposes,
    expected_exposes_close_square,
    expected_exposes_open_square,
    expected_imports,
    header_expected_open_square,
    header_expected_close_square,

    // Pattern errors
    pattern_unexpected_token,
    pattern_list_rest_old_syntax,
    pattern_unexpected_eof,

    // Type annotation errors
    ty_anno_unexpected_token,

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
    expr_dot_suffix_not_allowed,

    // Statement errors
    statement_unexpected_token,
    import_must_be_top_level,

    // Match errors
    match_branch_missing_arrow,
    expected_arrow,

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
};

/// Get the title for a diagnostic
pub fn getDiagnosticTitle(tag: DiagnosticTag) []const u8 {
    return switch (tag) {
        .multiple_platforms => "MULTIPLE PLATFORMS",
        .no_platform => "NO PLATFORM",
        .missing_header => "MISSING HEADER",
        .missing_arrow => "MISSING ARROW",
        .expected_exposes => "EXPECTED EXPOSES",
        .expected_exposes_close_square => "EXPECTED CLOSING BRACKET",
        .expected_exposes_open_square => "EXPECTED OPENING BRACKET",
        .expected_imports => "EXPECTED IMPORTS",
        .pattern_unexpected_token => "UNEXPECTED TOKEN IN PATTERN",
        .pattern_list_rest_old_syntax => "BAD LIST REST PATTERN SYNTAX",
        .pattern_unexpected_eof => "UNEXPECTED END OF FILE IN PATTERN",
        .ty_anno_unexpected_token => "UNEXPECTED TOKEN IN TYPE ANNOTATION",
        .string_unexpected_token => "UNEXPECTED TOKEN IN STRING",
        .expr_unexpected_token => "UNEXPECTED TOKEN IN EXPRESSION",
        .import_must_be_top_level => "IMPORT MUST BE TOP LEVEL",
        .expected_expr_close_square_or_comma => "LIST NOT CLOSED",
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
        .no_else => "IF WITHOUT ELSE",
        .header_expected_open_square => "EXPECTED OPENING BRACKET",
        .header_expected_close_square => "EXPECTED CLOSING BRACKET",
        else => "PARSE ERROR",
    };
}

/// Generate a report for a diagnostic
pub fn diagnosticToReport(env: *const CommonEnv, tag: DiagnosticTag, region: base.Region, allocator: std.mem.Allocator, filename: []const u8) !reporting.Report {
    const title = getDiagnosticTitle(tag);
    var report = reporting.Report.init(allocator, title, .runtime_error);

    // Add detailed error message based on the diagnostic type
    switch (tag) {
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
        .expected_exposes, .expected_exposes_close_square, .expected_exposes_open_square => {
            try report.document.addReflowingText("Module headers must have an ");
            try report.document.addKeyword("exposing");
            try report.document.addReflowingText(" section that lists what the module exposes.");
            try report.document.addLineBreak();
            try report.document.addText("For example: ");
            try report.document.addCodeBlock("module [main, add, subtract]");
        },
        .header_expected_open_square => {
            try report.document.addText("Expected an opening bracket ");
            try report.document.addAnnotated("[", .emphasized);
            try report.document.addText(" after ");
            try report.document.addKeyword("module");
            try report.document.addText(".");
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
            try report.document.addAnnotated(@tagName(tag), .emphasized);
            try report.document.addLineBreak();
            try report.document.addReflowingText("This is an unexpected parsing error. Please check your syntax.");
        },
    }

    // Add source location
    try report.document.addLineBreak();
    try report.document.addLineBreak();

    // Convert region to RegionInfo for reporting
    const region_info: base.RegionInfo = base.RegionInfo.position(env.source, env.line_starts, region.start.offset, region.end.offset) catch base.RegionInfo{
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
        env.line_starts,
    );

    return report;
}
