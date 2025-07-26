//! Main test entry point for all reporting-related
//! functionality including rendering, document generation, styling, and reports.

const std = @import("std");
const testing = std.testing;
const reporting = @import("reporting");

const Allocator = std.mem.Allocator;
const Document = reporting.Document;
const DocumentBuilder = reporting.DocumentBuilder;
const Annotation = reporting.Annotation;
const DocumentElement = reporting.DocumentElement;
const SourceRegion = reporting.SourceRegion;
const Report = reporting.Report;
const Severity = reporting.Severity;
const ReportingConfig = reporting.ReportingConfig;
const ColorPalette = reporting.ColorPalette;

// Test cases for canonicalize error reports

test "SYNTAX_PROBLEM report along with all four render types" {
    const gpa = testing.allocator;
    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    // Create a Report
    var r = Report.init(gpa, "SYNTAX PROBLEM", .runtime_error);
    defer r.deinit();

    // Add the document which describes the problem
    r.document = try buildSyntaxProblemReport(gpa);
    try testing.expect(r.document.elementCount() > 0);
    try testing.expect(!r.document.isEmpty());

    // Markdown
    try reporting.renderReportToMarkdown(&r, buffer.writer(), reporting.ReportingConfig.initMarkdown());

    const expected =
        \\**SYNTAX PROBLEM**
        \\Using more than one `+` like this requires parentheses, to clarify how things should be grouped.
        \\**example.roc:1:10:1:20:**
        \\```roc
        \\example.roc
        \\```
        \\         ^^^^^^^^^^
        \\
        \\
        \\
    ;

    try expectMultilineEqual(expected, buffer.items);

    // HTML
    buffer.clearRetainingCapacity();

    try reporting.renderReportToHtml(&r, buffer.writer(), reporting.ReportingConfig.initHtml());

    const expected_html =
        \\<div class="report error">
        \\<h1 class="report-title">SYNTAX PROBLEM</h1>
        \\<div class="report-content">
        \\Using more than one <span class="operator">+</span> like this requires parentheses, to clarify how things should be grouped.<br>
        \\<div class="source-region"><span class="filename">example.roc:1:10:1:20:</span> <pre class="error">example.roc</pre></div></div>
        \\</div>
        \\
    ;

    try expectMultilineEqual(expected_html, buffer.items);

    // Language Server Protocol
    buffer.clearRetainingCapacity();

    try reporting.renderReportToLsp(&r, buffer.writer(), reporting.ReportingConfig.initLsp());

    const expected_lsp =
        \\SYNTAX PROBLEM
        \\
        \\Using more than one + like this requires parentheses, to clarify how things should be grouped.
        \\example.roc:1:10:1:20: example.roc
        \\
    ;

    try expectMultilineEqual(expected_lsp, buffer.items);

    // Terminal (TTY)
    buffer.clearRetainingCapacity();

    try reporting.renderReportToTerminal(&r, buffer.writer(), ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal());

    // let's forget about comparing with ansi escape codes present... doesn't seem worth the effort.
    // we'll have to QA the old fashioned way.
}

fn buildSyntaxProblemReport(allocator: Allocator) !Document {
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
// Test Helpers

/// Should only print out the debug copy-paste ready string if the string comparison fails.
fn expectMultilineEqual(expected: []const u8, actual: []const u8) !void {
    testing.expectEqualStrings(expected, actual) catch {
        std.debug.print("\n--- DEBUG STRING COMPARISON (copy-paste ready) ---\n", .{});
        std.debug.print("const expected = \n", .{});
        printAsMultilineString(actual);
        std.debug.print(";\n", .{});
    };
}

fn printAsMultilineString(s: []const u8) void {
    if (s.len == 0) {
        std.debug.print("        \\\\\n", .{});
        return;
    }

    var lines = std.mem.splitScalar(u8, s, '\n');
    var first = true;
    while (lines.next()) |line| {
        if (first) {
            first = false;
            std.debug.print("        \\\\", .{});
        } else {
            std.debug.print("        \\\\", .{});
        }

        // Print each character with proper escaping
        for (line) |c| {
            switch (c) {
                '\\' => std.debug.print("\\\\", .{}),
                '"' => std.debug.print("\\\"", .{}),
                '\'' => std.debug.print("\\'", .{}),
                '\t' => std.debug.print("\\t", .{}),
                '\r' => std.debug.print("\\r", .{}),
                else => std.debug.print("{c}", .{c}),
            }
        }
        std.debug.print("\n", .{});
    }
}
