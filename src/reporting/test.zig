//! Main test entry point for all reporting-related
//! functionality including rendering, document generation, styling, and reports.

const std = @import("std");
const testing = std.testing;

const Allocator = std.mem.Allocator;
const Document = @import("document.zig").Document;
const Report = @import("report.zig").Report;
const ColorPalette = @import("style.zig").ColorPalette;
const ReportingConfig = @import("config.zig").ReportingConfig;

const reporting = @import("mod.zig");

// Test cases for canonicalize error reports

test "SYNTAX_PROBLEM report along with all four render types" {
    const gpa = testing.allocator;
    var writer = std.Io.Writer.Allocating.init(gpa);
    defer writer.deinit();

    // Create a Report
    var r = try Report.init(gpa, "Syntax Problem", "", .runtime_error);
    defer r.deinit();

    // Add the document which describes the problem
    r.document = try buildSyntaxProblemReport(gpa);
    try testing.expect(r.document.elementCount() > 0);
    try testing.expect(!r.document.isEmpty());

    // Markdown
    try reporting.renderReportToMarkdown(&r, &writer.writer, @import("config.zig").ReportingConfig.initMarkdown());
    const expected =
        \\**Syntax Problem**
        \\Using more than one `+` like this requires parentheses, to clarify how things should be grouped.
        \\```roc
        \\example.roc
        \\```
        \\         ^^^^^^^^^^
        \\
        \\
        \\
    ;
    try expectMultilineEqual(expected, writer.written());

    // HTML
    writer.clearRetainingCapacity();
    try reporting.renderReportToHtml(&r, &writer.writer, @import("config.zig").ReportingConfig.initHtml());
    const expected_html =
        \\<div class="report error">
        \\<h1 class="report-title">syntax problem</h1>
        \\<div class="report-content">
        \\Using more than one <span class="operator">+</span> like this requires parentheses, to clarify how things should be grouped.<br>
        \\<div class="source-region"><pre class="error">example.roc</pre></div></div>
        \\</div>
        \\
    ;
    try expectMultilineEqual(expected_html, writer.written());

    // Language Server Protocol
    writer.clearRetainingCapacity();
    try reporting.renderReportToLsp(&r, &writer.writer, @import("config.zig").ReportingConfig.initLsp());
    const expected_lsp =
        \\syntax problem
        \\
        \\Using more than one + like this requires parentheses, to clarify how things should be grouped.
        \\example.roc
        \\
    ;
    try expectMultilineEqual(expected_lsp, writer.written());

    // Plain-text header format
    writer.clearRetainingCapacity();
    try reporting.renderReportToPlain(&r, &writer.writer, @import("config.zig").ReportingConfig.initMarkdown());
    const plain_out = writer.written();
    try testing.expect(std.mem.find(u8, plain_out, "── ✗ syntax problem ") != null);
    try testing.expect(std.mem.find(u8, plain_out, "example.roc:1:10") != null);
    try testing.expect(std.mem.find(u8, plain_out, "^^") != null);
}

fn buildSyntaxProblemReport(allocator: Allocator) Allocator.Error!Document {
    var doc = Document.init(allocator);
    try doc.addText("Using more than one ");
    try doc.addBinaryOperator("+");
    try doc.addReflowingText(" like this requires parentheses, to clarify how things should be grouped.");
    try doc.addLineBreak();
    try doc.addSourceRegion(.{
        .start_line_idx = 0,
        .start_col_idx = 9,
        .end_line_idx = 0,
        .end_col_idx = 19,
    }, .error_highlight, "example.roc", "example.roc", &[_]u32{0});
    return doc;
}

fn buildDuplicateDefinitionReport(allocator: Allocator) Allocator.Error!Report {
    const source =
        "c = if args.len() < 20 { \"a\" } else { 23 }\n" ++
        "c = 5";
    const second_line: u32 = @intCast((std.mem.findScalar(u8, source, '\n') orelse unreachable) + 1);
    const line_starts = [_]u32{ 0, second_line };

    var report = try Report.init(allocator, "Duplicate Definition", "", .warning);
    errdefer report.deinit();

    try report.headline.addReflowingText("The name ");
    try report.headline.addUnqualifiedSymbol("c");
    try report.headline.addReflowingText(" is being redeclared here:");

    try report.document.addSourceRegion(
        .{ .start_line_idx = 1, .start_col_idx = 0, .end_line_idx = 1, .end_col_idx = 1 },
        .warning_highlight,
        "example.roc",
        source,
        &line_starts,
    );
    try report.document.addLineBreak();
    try report.document.addReflowingText("In this scope, ");
    try report.document.addUnqualifiedSymbol("c");
    try report.document.addReflowingText(" was already defined in ");
    try report.document.addSourceLocation(
        .{ .start_line_idx = 0, .start_col_idx = 0, .end_line_idx = 0, .end_col_idx = 1 },
        "example.roc",
    );
    try report.document.addReflowingText(":");
    try report.document.addLineBreak();
    try report.document.addSourceRegion(
        .{ .start_line_idx = 0, .start_col_idx = 0, .end_line_idx = 0, .end_col_idx = 1 },
        .warning_highlight,
        "example.roc",
        source,
        &line_starts,
    );

    return report;
}

test "terminal diagnostic layout has exact plain and ANSI output" {
    var report = try buildDuplicateDefinitionReport(testing.allocator);
    defer report.deinit();

    var config = ReportingConfig.initColorTerminal();
    config.max_line_width = 80;

    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    try reporting.renderReportToPlain(&report, &writer.writer, config);
    try testing.expectEqualStrings(
        "── ● duplicate definition " ++ ("─" ** 38) ++ " example.roc:2:1\n" ++
            "\n" ++
            "The name c is being redeclared here:\n" ++
            "\n" ++
            "c = 5\n" ++
            "^\n" ++
            "\n" ++
            "In this scope, c was already defined in example.roc:1:1:\n" ++
            "\n" ++
            "c = if args.len() < 20 { \"a\" } else { 23 }\n" ++
            "^\n" ++
            "\n",
        writer.written(),
    );

    writer.clearRetainingCapacity();
    try reporting.renderReportToTerminal(&report, &writer.writer, ColorPalette.ANSI, config);
    const gray = "\x1b[90m";
    const yellow = "\x1b[33m";
    const cyan = "\x1b[36m";
    const reset = "\x1b[0m";
    try testing.expectEqualStrings(
        gray ++ "── " ++ yellow ++ "● duplicate definition " ++ gray ++ ("─" ** 38) ++ " " ++ cyan ++ "example.roc" ++ gray ++ ":2:1" ++ reset ++ "\n" ++
            "\n" ++
            "The name " ++ cyan ++ "c" ++ reset ++ " is being redeclared here:\n" ++
            "\n" ++
            "c = 5\n" ++
            yellow ++ "^" ++ reset ++ "\n" ++
            "\n" ++
            "In this scope, " ++ cyan ++ "c" ++ reset ++ " was already defined in " ++ cyan ++ "example.roc" ++ gray ++ ":1:1" ++ reset ++ ":\n" ++
            "\n" ++
            "c = if args.len() < 20 { \"a\" } else { 23 }\n" ++
            yellow ++ "^" ++ reset ++ "\n" ++
            "\n",
        writer.written(),
    );
}

test "terminal diagnostic headers follow the configured width up to 120 columns" {
    var report = try buildDuplicateDefinitionReport(testing.allocator);
    defer report.deinit();

    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    for ([_]struct { configured: u32, expected: usize }{
        .{ .configured = 55, .expected = 55 },
        .{ .configured = 200, .expected = 120 },
    }) |case| {
        var config = ReportingConfig.initColorTerminal();
        config.max_line_width = case.configured;
        try reporting.renderReportToPlain(&report, &writer.writer, config);
        const first_newline = std.mem.findScalar(u8, writer.written(), '\n') orelse unreachable;
        try testing.expectEqual(case.expected, reporting.source_region.displayWidth(writer.written()[0..first_newline]));
        writer.clearRetainingCapacity();
    }
}
// Test Helpers

/// Should only print out the debug copy-paste ready string if the string comparison fails.
fn expectMultilineEqual(expected: []const u8, actual: []const u8) error{TestExpectedEqual}!void {
    if (!std.mem.eql(u8, expected, actual)) {
        std.debug.print("\n--- DEBUG EXPECTED vs ACTUAL ---\nEXPECTED:\n{s}\nACTUAL:\n{s}\n", .{ expected, actual });
    }
    try testing.expectEqualStrings(expected, actual);
}
