//! Main test entry point for all reporting-related
//! functionality including rendering, document generation, styling, and reports.

const std = @import("std");
const testing = std.testing;

const Allocator = std.mem.Allocator;
const Document = @import("document.zig").Document;
const Report = @import("report.zig").Report;

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
        \\Using more than one `+` like this requires parentheses, to clarify how things should be grouped. (example.roc:1:10):
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
        \\<h1 class="report-title">SYNTAX PROBLEM</h1>
        \\<div class="report-content">
        \\Using more than one <span class="operator">+</span> like this requires parentheses, to clarify how things should be grouped. (example.roc:1:10):<br>
        \\<div class="source-region"><pre class="error">example.roc</pre></div></div>
        \\</div>
        \\
    ;
    try expectMultilineEqual(expected_html, writer.written());

    // Language Server Protocol
    writer.clearRetainingCapacity();
    try reporting.renderReportToLsp(&r, &writer.writer, @import("config.zig").ReportingConfig.initLsp());
    const expected_lsp =
        \\SYNTAX PROBLEM
        \\
        \\Using more than one + like this requires parentheses, to clarify how things should be grouped. (example.roc:1:10):
        \\example.roc
        \\
    ;
    try expectMultilineEqual(expected_lsp, writer.written());

    // Plain-text header format
    writer.clearRetainingCapacity();
    try reporting.renderReportToBoxPlain(&r, &writer.writer, @import("config.zig").ReportingConfig.initMarkdown());
    const plain_out = writer.written();
    try testing.expect(std.mem.find(u8, plain_out, "── ✗ syntax problem ") != null);
    try testing.expect(std.mem.find(u8, plain_out, " (example.roc:1:10):") != null);
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
// Test Helpers

/// Should only print out the debug copy-paste ready string if the string comparison fails.
fn expectMultilineEqual(expected: []const u8, actual: []const u8) error{TestExpectedEqual}!void {
    if (!std.mem.eql(u8, expected, actual)) {
        std.debug.print("\n--- DEBUG EXPECTED vs ACTUAL ---\nEXPECTED:\n{s}\nACTUAL:\n{s}\n", .{ expected, actual });
    }
    try testing.expectEqualStrings(expected, actual);
}
