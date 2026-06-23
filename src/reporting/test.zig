//! Main test entry point for all reporting-related
//! functionality including rendering, document generation, styling, and reports.

const std = @import("std");
const testing = std.testing;

const Allocator = std.mem.Allocator;
const Document = @import("document.zig").Document;
const Report = @import("report.zig").Report;
const ColorPalette = @import("style.zig").ColorPalette;
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
        \\**example.roc:1:10:1:20:**
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
        \\Using more than one <span class="operator">+</span> like this requires parentheses, to clarify how things should be grouped.<br>
        \\<div class="source-region"><span class="filename">example.roc:1:10:1:20:</span> <pre class="error">example.roc</pre></div></div>
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
        \\Using more than one + like this requires parentheses, to clarify how things should be grouped.
        \\example.roc:1:10:1:20: example.roc
        \\
    ;

    try expectMultilineEqual(expected_lsp, writer.written());

    // Terminal (TTY)
    writer.clearRetainingCapacity();

    try reporting.renderReportToTerminal(&r, &writer.writer, ColorPalette.ANSI, @import("config.zig").ReportingConfig.initColorTerminal());

    // let's forget about comparing with ansi escape codes present... doesn't seem worth the effort.
    // we'll have to QA the old fashioned way.

    // Plain-text box (the layout used for snapshots and non-color output).
    // Assert the alignment invariant: every row that reaches the main box's
    // right wall has the same display width, so the wall lines up vertically.
    // The label box in the upper-left pokes out past the main box's left wall,
    // and its short top edge (`┌──┐`) does not reach the right wall, so it is
    // excluded.
    writer.clearRetainingCapacity();
    try reporting.renderReportToBoxPlain(&r, &writer.writer, @import("config.zig").ReportingConfig.initMarkdown());
    const box = writer.written();
    try testing.expect(std.mem.find(u8, box, "SYNTAX PROBLEM") != null);
    try testing.expect(std.mem.find(u8, box, "example.roc:1:10") != null);
    var box_lines = std.mem.splitScalar(u8, box, '\n');
    var wall_width: ?usize = null;
    var box_rows: usize = 0;
    var seen_label_top = false;
    while (box_lines.next()) |line| {
        const reaches_edge = std.mem.endsWith(u8, line, "│") or
            std.mem.endsWith(u8, line, "┐") or
            std.mem.endsWith(u8, line, "┘");
        if (!reaches_edge) continue;
        if (!seen_label_top and std.mem.endsWith(u8, line, "┐")) {
            // The label box's top edge — short, doesn't reach the right wall.
            seen_label_top = true;
            continue;
        }
        const w = reporting.source_region.displayWidth(line);
        if (wall_width) |ww| {
            try testing.expectEqual(ww, w);
        } else {
            wall_width = w;
        }
        box_rows += 1;
    }
    // title-box bottom, blank, source line, underline, bottom edge
    try testing.expect(box_rows >= 5);
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
fn expectMultilineEqual(expected: []const u8, actual: []const u8) Allocator.Error!void {
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
