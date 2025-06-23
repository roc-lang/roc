//! Provides functions to render Reports and Documents to various
//! output formats without the complexity of vtables or interfaces.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Report = @import("report.zig").Report;
const Document = @import("document.zig").Document;
const DocumentElement = @import("document.zig").DocumentElement;
const Annotation = @import("document.zig").Annotation;
const ColorPalette = @import("style.zig").ColorPalette;
const ColorUtils = @import("style.zig").ColorUtils;
const ReportingConfig = @import("config.zig").ReportingConfig;
const collections = @import("../collections.zig");
const exitOnOom = collections.utils.exitOnOom;
const source_region = @import("source_region.zig");

/// TODO find a better solution this is temporary to make CI happy
///
/// Makes a file path relative for error reporting.
/// For snapshot files, returns just the filename.
/// For other files, returns the original path.
fn sanitisePathForSnapshots(path: []const u8) []const u8 {

    // Check if this is a snapshot file (contains /snapshots/ or \snapshots\)
    if (std.mem.indexOf(u8, path, "/snapshots/") != null or
        std.mem.indexOf(u8, path, "\\snapshots\\") != null)
    {
        // For snapshot files, just return the basename
        return std.fs.path.basename(path);
    }

    // For non-snapshot files, return the original path for now
    return path;
}

/// Supported rendering targets.
pub const RenderTarget = enum {
    color_terminal,
    markdown,
    html,
    language_server,
};

/// Render a report to the specified target format.
pub fn renderReport(report: *const Report, writer: anytype, target: RenderTarget) !void {
    const palette = ColorUtils.getPaletteForConfig(report.config);
    switch (target) {
        .color_terminal => try renderReportToTerminal(report, writer, palette),
        .markdown => try renderReportToMarkdown(report, writer),
        .html => try renderReportToHtml(report, writer),
        .language_server => try renderReportToLsp(report, writer),
    }
}

/// Render a report to terminal with color support.
pub fn renderReportToTerminal(report: *const Report, writer: anytype, palette: ColorPalette) !void {
    // Render title with appropriate severity styling
    const title_color = switch (report.severity) {
        .fatal => palette.error_color,
        .runtime_error => palette.error_color,
        .warning => palette.warning,
    };

    try writer.writeAll("-- ");
    try writer.writeAll(palette.bold);
    try writer.writeAll(title_color);
    try writer.writeAll(report.title);
    try writer.writeAll(palette.reset);
    try writer.writeAll(" ");

    const PADDING_WIDTH = 50 - 4; // 4 is the width of the 3 char before and 1 space after the title
    const title_length = report.title.len;
    const padding = if (title_length < PADDING_WIDTH) PADDING_WIDTH - title_length else 0;

    // stack allocate a buffer for our variable padding width
    var buf: [PADDING_WIDTH]u8 = undefined;
    for (buf[0..padding]) |*b| {
        b.* = '-';
    }
    try writer.writeAll(buf[0..padding]);

    try writer.writeAll("\n\n");

    // Render document content
    try renderDocumentToTerminal(&report.document, writer, palette);

    try writer.writeAll("\n");
}

/// Render a report to plain text.
pub fn renderReportToMarkdown(report: *const Report, writer: anytype) !void {
    try writer.writeAll("**");
    try writer.writeAll(report.title);
    try writer.writeAll("**\n");
    try renderDocumentToMarkdown(&report.document, writer);
    try writer.writeAll("\n");
}

/// Render a report to HTML.
pub fn renderReportToHtml(report: *const Report, writer: anytype) !void {
    const title_class = switch (report.severity) {
        .fatal => "error",
        .runtime_error => "error",
        .warning => "warning",
    };

    try writer.print("<div class=\"report {s}\">\n", .{title_class});
    try writer.writeAll("<h1 class=\"report-title\">");
    try writeEscapedHtml(writer, report.title);
    try writer.writeAll("</h1>\n");
    try writer.writeAll("<div class=\"report-content\">\n");
    try renderDocumentToHtml(&report.document, writer);
    try writer.writeAll("</div>\n</div>\n");
}

/// Render a report for language server protocol.
pub fn renderReportToLsp(report: *const Report, writer: anytype) !void {
    // LSP typically wants plain text without formatting
    try writer.writeAll(report.title);
    try writer.writeAll("\n\n");
    try renderDocumentToLsp(&report.document, writer);
}

/// Render a document to the specified target format.
pub fn renderDocument(document: *const Document, writer: anytype, target: RenderTarget) !void {
    switch (target) {
        .color_terminal => {
            const palette = ColorUtils.getPaletteForConfig(ReportingConfig.init());
            try renderDocumentToTerminal(document, writer, palette);
        },
        .markdown => try renderDocumentToMarkdown(document, writer),
        .html => try renderDocumentToHtml(document, writer),
        .language_server => try renderDocumentToLsp(document, writer),
    }
}

/// Render a document to terminal with color support.
pub fn renderDocumentToTerminal(document: *const Document, writer: anytype, palette: ColorPalette) !void {
    var annotation_stack = std.ArrayList(Annotation).init(document.allocator);
    defer annotation_stack.deinit();

    for (document.elements.items) |element| {
        try renderElementToTerminal(element, writer, palette, &annotation_stack);
    }
}

/// Render a document to plain text.
pub fn renderDocumentToMarkdown(document: *const Document, writer: anytype) !void {
    for (document.elements.items) |element| {
        try renderElementToMarkdown(element, writer);
    }
}

/// Render a document to HTML.
pub fn renderDocumentToHtml(document: *const Document, writer: anytype) !void {
    var annotation_stack = std.ArrayList(Annotation).init(document.allocator);
    defer annotation_stack.deinit();

    for (document.elements.items) |element| {
        try renderElementToHtml(element, writer, &annotation_stack);
    }
}

/// Render a document for language server protocol.
pub fn renderDocumentToLsp(document: *const Document, writer: anytype) !void {
    for (document.elements.items) |element| {
        try renderElementToLsp(element, writer);
    }
}

// Terminal rendering functions

fn renderElementToTerminal(element: DocumentElement, writer: anytype, palette: ColorPalette, annotation_stack: *std.ArrayList(Annotation)) !void {
    switch (element) {
        .text => |text| try writer.writeAll(text),
        .annotated => |annotated| {
            const color = getAnnotationColor(annotated.annotation, palette);
            try writer.writeAll(color);
            try writer.writeAll(annotated.content);
            try writer.writeAll(palette.reset);
        },
        .line_break => try writer.writeAll("\n"),
        .indent => |levels| {
            var i: u32 = 0;
            while (i < levels) : (i += 1) {
                try writer.writeAll("    ");
            }
        },
        .space => |count| {
            var i: u32 = 0;
            while (i < count) : (i += 1) {
                try writer.writeAll(" ");
            }
        },
        .horizontal_rule => |width| {
            const rule_width = width orelse 80;
            var i: u32 = 0;
            while (i < rule_width) : (i += 1) {
                try writer.writeAll("─");
            }
        },
        .annotation_start => |annotation| {
            annotation_stack.append(annotation) catch |err| exitOnOom(err);
            const color = getAnnotationColor(annotation, palette);
            try writer.writeAll(color);
        },
        .annotation_end => {
            if (annotation_stack.items.len > 0) {
                _ = annotation_stack.pop();
                try writer.writeAll(palette.reset);
                // Re-apply previous annotation if any
                if (annotation_stack.items.len > 0) {
                    const prev_annotation = annotation_stack.items[annotation_stack.items.len - 1];
                    const color = getAnnotationColor(prev_annotation, palette);
                    try writer.writeAll(color);
                }
            }
        },
        .raw => |content| try writer.writeAll(content),
        .reflowing_text => |text| try writer.writeAll(text),
        .vertical_stack => |elements| {
            for (elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll("\n");
                try renderElementToTerminal(elem, writer, palette, annotation_stack);
            }
        },
        .horizontal_concat => |elements| {
            for (elements) |elem| {
                try renderElementToTerminal(elem, writer, palette, annotation_stack);
            }
        },
        .source_code_region => |region| {
            // Calculate the width needed for line numbers
            const line_num_width = source_region.calculateLineNumberWidth(region.end_line);

            // Print location header with proper alignment
            try source_region.printSpaces(writer, line_num_width);
            try writer.writeAll(palette.secondary);
            if (region.filename) |filename| {
                try writer.print(" ┌─ {s}:{}:{}\n", .{ sanitisePathForSnapshots(filename), region.start_line, region.start_column });
            } else {
                try writer.print(" ┌─ <source>:{}:{}\n", .{ region.start_line, region.start_column });
            }
            try writer.writeAll(palette.reset);

            // Print separator line
            try source_region.printSpaces(writer, line_num_width);
            try writer.writeAll(palette.secondary);
            try writer.writeAll(" │\n");
            try writer.writeAll(palette.reset);

            // Extract and print the source lines with line numbers
            const lines = source_region.extractLines(region.source, region.start_line, region.end_line);
            var line_num = region.start_line;
            var iter = std.mem.tokenizeScalar(u8, lines, '\n');
            while (iter.next()) |line| {
                // Print line number
                try writer.writeAll(palette.secondary);
                try source_region.formatLineNumber(writer, line_num, line_num_width);
                try writer.writeAll(" │ ");
                try writer.writeAll(palette.reset);

                // Print the line content with highlighting
                const color = getAnnotationColor(region.region_annotation, palette);
                try writer.writeAll(color);
                try writer.writeAll(line);
                try writer.writeAll(palette.reset);
                try writer.writeAll("\n");

                // Print column indicator if this is a single-line region
                if (region.start_line == region.end_line and line_num == region.start_line) {
                    try writer.writeAll(palette.secondary);
                    // Print spaces for line number width
                    try source_region.printSpaces(writer, line_num_width);
                    try writer.writeAll(" │ ");
                    try writer.writeAll(palette.reset);

                    // Print spaces up to the start column
                    try source_region.printSpaces(writer, region.start_column - 1);

                    // Print the underline
                    try writer.writeAll(color);
                    const underline_len = source_region.calculateUnderlineLength(region.start_column, region.end_column);
                    var i: u32 = 0;
                    while (i < underline_len) : (i += 1) {
                        try writer.writeAll("^");
                    }
                    try writer.writeAll(palette.reset);
                    try writer.writeAll("\n");
                }

                line_num += 1;
            }
        },
        .source_code_multi_region => |multi| {
            if (multi.filename) |filename| {
                try writer.print("{s}: ", .{sanitisePathForSnapshots(filename)});
            }
            try writer.writeAll(multi.source);
            try writer.writeAll("\n");
            for (multi.regions) |region| {
                const color = getAnnotationColor(region.annotation, palette);
                try writer.writeAll(color);
                try writer.print("  {}:{}-{}:{}\n", .{ region.start_line, region.start_column, region.end_line, region.end_column });
                try writer.writeAll(palette.reset);
            }
        },
    }
}

fn getAnnotationColor(annotation: Annotation, palette: ColorPalette) []const u8 {
    return switch (annotation) {
        .emphasized => palette.bold,
        .keyword => palette.keyword,
        .type_variable => palette.type_variable,
        .error_highlight => palette.error_color,
        .warning_highlight => palette.warning,
        .suggestion => palette.success,
        .code_block, .inline_code => palette.primary,
        .symbol => palette.symbol,
        .path => palette.path,
        .literal => palette.literal,
        .comment => palette.comment,
        .underline => palette.underline,
        .dimmed => palette.dim,
        .symbol_qualified => palette.symbol,
        .symbol_unqualified => palette.symbol,
        .module_name => palette.primary,
        .record_field => palette.type_variable,
        .tag_name => palette.type_variable,
        .binary_operator => palette.keyword,
        .source_region => palette.primary,
        .reflowing_text => palette.reset,
    };
}

// Plain text rendering functions

fn renderElementToMarkdown(element: DocumentElement, writer: anytype) !void {
    switch (element) {
        .text => |text| try writer.writeAll(text),
        .annotated => |annotated| {
            switch (annotated.annotation) {
                .emphasized => {
                    try writer.writeAll("**");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("**");
                },
                .keyword => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .type_variable => {
                    try writer.writeAll("_");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("_");
                },
                .error_highlight => {
                    try writer.writeAll("**");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("**");
                },
                .warning_highlight => {
                    try writer.writeAll("_");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("_");
                },
                .suggestion => {
                    try writer.writeAll("**");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("**");
                },
                .code_block => {
                    try writer.writeAll("```\n");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("\n```");
                },
                .inline_code => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .symbol => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .path => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .literal => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .comment => {
                    try writer.writeAll("_");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("_");
                },
                .underline => {
                    try writer.writeAll("__");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("__");
                },
                .dimmed => {
                    try writer.writeAll("~~");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("~~");
                },
                .symbol_qualified, .symbol_unqualified => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .module_name => {
                    try writer.writeAll("**");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("**");
                },
                .record_field => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .tag_name => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .binary_operator => {
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .source_region => try writer.writeAll(annotated.content),
                .reflowing_text => try writer.writeAll(annotated.content),
            }
        },
        .line_break => try writer.writeAll("\n"),
        .indent => |levels| {
            var i: u32 = 0;
            while (i < levels) : (i += 1) {
                try writer.writeAll("    ");
            }
        },
        .space => |count| {
            var i: u32 = 0;
            while (i < count) : (i += 1) {
                try writer.writeAll(" ");
            }
        },
        .horizontal_rule => |width| {
            const rule_width = width orelse 80;
            try writer.writeAll("\n---\n");
            _ = rule_width; // Markdown uses standard horizontal rule
        },
        .annotation_start, .annotation_end => {}, // Handled in annotated case
        .raw => |content| try writer.writeAll(content),
        .reflowing_text => |text| try writer.writeAll(text),
        .vertical_stack => |elements| {
            for (elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll("\n");
                try renderElementToMarkdown(elem, writer);
            }
        },
        .horizontal_concat => |elements| {
            for (elements) |elem| {
                try renderElementToMarkdown(elem, writer);
            }
        },
        .source_code_region => |region| {
            if (region.filename) |filename| {
                try writer.print("**{s}:{d}-{d}:{d}:**\n", .{ sanitisePathForSnapshots(filename), region.start_line, region.start_column, region.end_line });
            }
            try writer.writeAll("```roc\n");
            const lines = source_region.extractLines(region.source, region.start_line, region.end_line);
            try writer.writeAll(lines);
            try writer.writeAll("\n```\n");
        },
        .source_code_multi_region => |multi| {
            if (multi.filename) |filename| {
                try writer.print("**{s}:**\n", .{filename});
            }
            try writer.writeAll("```roc\n");
            try writer.writeAll(multi.source);
            try writer.writeAll("\n```\n");
            for (multi.regions) |region| {
                try writer.print("- Line {d}:{d}-{d}:{d}\n", .{ region.start_line, region.start_column, region.end_line, region.end_column });
            }
        },
    }
}

// HTML rendering functions

fn renderElementToHtml(element: DocumentElement, writer: anytype, annotation_stack: *std.ArrayList(Annotation)) !void {
    switch (element) {
        .text => |text| try writeEscapedHtml(writer, text),
        .annotated => |annotated| {
            const tag = getAnnotationHtmlTag(annotated.annotation);
            const class = getAnnotationHtmlClass(annotated.annotation);
            try writer.print("<{s} class=\"{s}\">", .{ tag, class });
            try writeEscapedHtml(writer, annotated.content);
            try writer.print("</{s}>", .{tag});
        },
        .line_break => try writer.writeAll("<br>\n"),
        .indent => |levels| {
            var i: u32 = 0;
            while (i < levels) : (i += 1) {
                try writer.writeAll("&nbsp;&nbsp;&nbsp;&nbsp;");
            }
        },
        .space => |count| {
            var i: u32 = 0;
            while (i < count) : (i += 1) {
                try writer.writeAll("&nbsp;");
            }
        },
        .horizontal_rule => |width| {
            const rule_width = width orelse 80;
            try writer.print("<hr style=\"width: {d}ch;\">\n", .{rule_width});
        },
        .annotation_start => |annotation| {
            annotation_stack.append(annotation) catch |err| exitOnOom(err);
            const tag = getAnnotationHtmlTag(annotation);
            const class = getAnnotationHtmlClass(annotation);
            try writer.print("<{s} class=\"{s}\">", .{ tag, class });
        },
        .annotation_end => {
            if (annotation_stack.items.len > 0) {
                const annotation = annotation_stack.items[annotation_stack.items.len - 1];
                const tag = getAnnotationHtmlTag(annotation);
                _ = annotation_stack.pop();
                try writer.print("</{s}>", .{tag});
            }
        },
        .raw => |content| try writer.writeAll(content),
        .reflowing_text => |text| try writeEscapedHtml(writer, text),
        .vertical_stack => |elements| {
            try writer.writeAll("<div class=\"vertical-stack\">\n");
            for (elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll("\n");
                try renderElementToHtml(elem, writer, annotation_stack);
            }
            try writer.writeAll("</div>\n");
        },
        .horizontal_concat => |elements| {
            try writer.writeAll("<span class=\"horizontal-concat\">");
            for (elements) |elem| {
                try renderElementToHtml(elem, writer, annotation_stack);
            }
            try writer.writeAll("</span>");
        },
        .source_code_region => |region| {
            try writer.writeAll("<div class=\"source-region\">");
            if (region.filename) |filename| {
                try writer.print("<span class=\"filename\">{s}:{}-{}:{}:</span> ", .{ sanitisePathForSnapshots(filename), region.start_line, region.start_column, region.end_line });
            }
            const class = getAnnotationHtmlClass(region.region_annotation);
            try writer.print("<pre class=\"{s}\">", .{class});
            const lines = source_region.extractLines(region.source, region.start_line, region.end_line);
            try writeEscapedHtml(writer, lines);
            try writer.writeAll("</pre></div>");
        },
        .source_code_multi_region => |multi| {
            try writer.writeAll("<div class=\"source-multi-region\">");
            if (multi.filename) |filename| {
                try writer.print("<span class=\"filename\">{s}:</span> ", .{sanitisePathForSnapshots(filename)});
            }
            try writer.writeAll("<pre>");
            try writeEscapedHtml(writer, multi.source);
            try writer.writeAll("</pre>\n<ul class=\"regions\">");
            for (multi.regions) |region| {
                const class = getAnnotationHtmlClass(region.annotation);
                try writer.print("<li class=\"{s}\">{d}:{d}-{d}:{d}</li>", .{ class, region.start_line, region.start_column, region.end_line, region.end_column });
            }
            try writer.writeAll("</ul></div>");
        },
    }
}

fn getAnnotationHtmlTag(annotation: Annotation) []const u8 {
    return switch (annotation) {
        .emphasized => "strong",
        .code_block => "pre",
        .inline_code => "code",
        else => "span",
    };
}

fn getAnnotationHtmlClass(annotation: Annotation) []const u8 {
    return annotation.semanticName();
}

fn writeEscapedHtml(writer: anytype, text: []const u8) !void {
    for (text) |char| {
        switch (char) {
            '<' => try writer.writeAll("&lt;"),
            '>' => try writer.writeAll("&gt;"),
            '&' => try writer.writeAll("&amp;"),
            '"' => try writer.writeAll("&quot;"),
            '\'' => try writer.writeAll("&#39;"),
            else => try writer.writeByte(char),
        }
    }
}

// LSP rendering functions

fn renderElementToLsp(element: DocumentElement, writer: anytype) !void {
    switch (element) {
        .text => |text| try writer.writeAll(text),
        .annotated => |annotated| try writer.writeAll(annotated.content),
        .line_break => try writer.writeAll("\n"),
        .indent => |levels| {
            var i: u32 = 0;
            while (i < levels) : (i += 1) {
                try writer.writeAll("  "); // Use 2 spaces for LSP
            }
        },
        .space => |count| {
            var i: u32 = 0;
            while (i < count) : (i += 1) {
                try writer.writeAll(" ");
            }
        },
        .horizontal_rule => |width| {
            const rule_width = width orelse 40; // Shorter for LSP
            var i: u32 = 0;
            while (i < rule_width) : (i += 1) {
                try writer.writeAll("-");
            }
        },
        .annotation_start, .annotation_end => {}, // Ignore annotations for LSP
        .raw => |content| try writer.writeAll(content),
        .reflowing_text => |text| try writer.writeAll(text),
        .vertical_stack => |elements| {
            for (elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll("\n");
                try renderElementToLsp(elem, writer);
            }
        },
        .horizontal_concat => |elements| {
            for (elements) |elem| {
                try renderElementToLsp(elem, writer);
            }
        },
        .source_code_region => |region| {
            if (region.filename) |filename| {
                try writer.print("{s}:{}-{}:{}: ", .{ sanitisePathForSnapshots(filename), region.start_line, region.start_column, region.end_line });
            }
            const lines = source_region.extractLines(region.source, region.start_line, region.end_line);
            try writer.writeAll(lines);
            try writer.writeAll("\n");
        },
        .source_code_multi_region => |multi| {
            if (multi.filename) |filename| {
                try writer.print("{s}: ", .{filename});
            }
            try writer.writeAll(multi.source);
            try writer.writeAll("\n");
            for (multi.regions) |region| {
                try writer.print("  {}:{}-{}:{}\n", .{ region.start_line, region.start_column, region.end_line, region.end_column });
            }
        },
    }
}

// Tests
const testing = std.testing;

test "render report to markdown" {
    const config = ReportingConfig.initForTesting();
    var report = Report.init(testing.allocator, "TEST ERROR", .runtime_error, config);
    defer report.deinit();

    try report.document.addText("This is a test error message.");

    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try renderReportToMarkdown(&report, buffer.writer());

    try testing.expect(std.mem.indexOf(u8, buffer.items, "**TEST ERROR**") != null);
    try testing.expect(std.mem.indexOf(u8, buffer.items, "This is a test error message.") != null);
}

test "render document with annotations to markdown" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    try doc.addText("Hello ");
    try doc.addAnnotated("world", .emphasized);
    try doc.addText("!");

    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try renderDocumentToMarkdown(&doc, buffer.writer());

    try testing.expectEqualStrings("Hello **world**!", buffer.items);
}

test "render HTML escaping" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    try doc.addText("<script>alert('test')</script>");

    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try renderDocumentToHtml(&doc, buffer.writer());

    try testing.expect(std.mem.indexOf(u8, buffer.items, "&lt;script&gt;") != null);
    try testing.expect(std.mem.indexOf(u8, buffer.items, "<script>") == null);
}

test "render indentation and spacing" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    try doc.addIndent(2);
    try doc.addText("indented");
    try doc.addSpace(3);
    try doc.addText("spaced");

    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try renderDocumentToMarkdown(&doc, buffer.writer());

    try testing.expectEqualStrings("        indented   spaced", buffer.items);
}

test "render horizontal rule" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    try doc.addHorizontalRule(5);

    var buffer = std.ArrayList(u8).init(testing.allocator);
    defer buffer.deinit();

    try renderDocumentToMarkdown(&doc, buffer.writer());

    try testing.expectEqualStrings("\n---\n", buffer.items);
}
