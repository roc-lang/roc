//! Provides functions to render Reports and Documents to various
//! output formats without the complexity of vtables or interfaces.

const std = @import("std");

const source_region = @import("source_region.zig");
const Allocator = std.mem.Allocator;
const Report = @import("report.zig").Report;
const Document = @import("document.zig").Document;
const DocumentElement = @import("document.zig").DocumentElement;
const Annotation = @import("document.zig").Annotation;
const ColorPalette = @import("style.zig").ColorPalette;
const ColorUtils = @import("style.zig").ColorUtils;
pub const ReportingConfig = @import("config.zig").ReportingConfig;

/// TODO find a better solution this is temporary to make CI happy
///
/// Makes a file path relative for error reporting.
/// For snapshot files, returns just the filename.
/// For other files, returns the original path.
fn sanitisePathForSnapshots(path: []const u8) []const u8 {

    // Check if this is a snapshot file (contains /snapshots/ or \snapshots\)
    if (std.mem.find(u8, path, "/snapshots/") != null or
        std.mem.find(u8, path, "\\snapshots\\") != null)
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
pub fn renderReport(report: *const Report, writer: *std.Io.Writer, target: RenderTarget) (Allocator.Error || error{WriteFailed})!void {
    // Create appropriate config based on render target
    const config = switch (target) {
        .color_terminal => ReportingConfig.initColorTerminal(),
        .markdown => ReportingConfig.initMarkdown(),
        .html => ReportingConfig.initHtml(),
        .language_server => ReportingConfig.initLsp(),
    };

    const palette = ColorUtils.getPaletteForConfig(config);
    switch (target) {
        .color_terminal => try renderReportToTerminal(report, writer, palette, config),
        .markdown => try renderReportToMarkdown(report, writer, config),
        .html => try renderReportToHtml(report, writer, config),
        .language_server => try renderReportToLsp(report, writer, config),
    }
}

/// Render a report with an explicit reporting configuration.
pub fn renderReportWithConfig(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    const palette = ColorUtils.getPaletteForConfig(config);
    switch (config.getRenderTarget()) {
        .color_terminal => try renderReportToTerminal(report, writer, palette, config),
        .markdown => try renderReportToMarkdown(report, writer, config),
        .html => try renderReportToHtml(report, writer, config),
        .language_server => try renderReportToLsp(report, writer, config),
    }
}

/// Render a report to terminal with color support.
pub fn renderReportToTerminal(report: *const Report, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try renderReportBoxed(report, writer, palette, config);
}

/// Render a report to plain markdown. This is the stable, machine-friendly
/// format used by internal tests and EXPECTED-section tooling.
pub fn renderReportToMarkdown(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try writer.writeAll("**");
    try writer.writeAll(report.title);
    try writer.writeAll("**\n");
    if (report.headline.len > 0) {
        try writer.writeAll(report.headline);
        try writer.writeByte('\n');
    }
    try renderDocumentToMarkdown(&report.document, writer, config);
    try writer.writeAll("\n\n");
}

/// Render a report as a plain-text box (the terminal box layout without ANSI
/// color). Used for snapshot PROBLEMS sections and non-TTY user output.
pub fn renderReportToBoxPlain(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try renderReportBoxed(report, writer, ColorPalette.NO_COLOR, config);
}

// Boxed report rendering.
//
// Lays a report out as a box drawn around the offending source snippet:
//
//     ┌─────────────┐
//     │ TYPE MISMATCH ├── <one-line summary> ──────────── ... ──────┐
//     └┬──────────────┘                                             │
//      │                                                            │
//      │  <source code>                                             │
//      │         ‾‾‾‾‾                                              │
//      └─────────────────────────────── path/to/file.roc:6:8 ──────┘
//
//         <detailed explanation, indented under the box>
//
// The summary rides the top edge (wrapping under its own start when long, and
// growing the box for 3+ lines). The same layout is used for the colored
// terminal output and the plain markdown/snapshot output — the only difference
// is the palette (ANSI vs NO_COLOR).

/// The thin red rule under the offending span. U+203E sits at the top of its
/// cell so it visually underlines the source line above it.
const box_underline = "‾";

/// A source region pulled out of a document, normalized for box rendering.
const BoxedRegion = struct {
    index: usize,
    filename: ?[]const u8,
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    line_text: []const u8,
};

fn findBoxedRegion(elements: []const DocumentElement) ?BoxedRegion {
    for (elements, 0..) |el, i| {
        switch (el) {
            .source_code_region => |r| return .{
                .index = i,
                .filename = r.filename,
                .start_line = r.start_line,
                .start_column = r.start_column,
                .end_line = r.end_line,
                .end_column = r.end_column,
                .line_text = r.line_text,
            },
            .source_code_with_underlines => |d| {
                const dr = d.display_region;
                var sc = dr.start_column;
                var ec = dr.end_column;
                if (d.underline_regions.len > 0) {
                    sc = d.underline_regions[0].start_column;
                    ec = d.underline_regions[0].end_column;
                }
                return .{
                    .index = i,
                    .filename = dr.filename,
                    .start_line = dr.start_line,
                    .start_column = sc,
                    .end_line = dr.end_line,
                    .end_column = ec,
                    .line_text = dr.line_text,
                };
            },
            else => {},
        }
    }
    return null;
}

/// Append the plain text content of a run of elements (line breaks become spaces).
fn collectPlainText(elements: []const DocumentElement, buf: *std.array_list.Managed(u8)) Allocator.Error!void {
    for (elements) |el| {
        switch (el) {
            .text => |t| try buf.appendSlice(t),
            .reflowing_text => |t| try buf.appendSlice(t),
            .raw => |t| try buf.appendSlice(t),
            .annotated => |a| try buf.appendSlice(a.content),
            .line_break => try buf.append(' '),
            .space => |n| {
                var i: u32 = 0;
                while (i < n) : (i += 1) try buf.append(' ');
            },
            else => {},
        }
    }
}

/// Greedy word-wrap `text` into lines no wider than `width` columns.
fn wrapSummary(text: []const u8, width: usize, out: *std.array_list.Managed([]const u8)) Allocator.Error!void {
    if (text.len == 0) return;
    var start: usize = 0;
    while (start < text.len) {
        while (start < text.len and text[start] == ' ') start += 1;
        if (start >= text.len) break;
        var end = start;
        var last_break: ?usize = null;
        var i = start;
        var disp: usize = 0; // display columns accumulated on this line
        while (i < text.len) {
            const seq_len = std.unicode.utf8ByteSequenceLength(text[i]) catch 1;
            const next = @min(i + seq_len, text.len);
            const cw = source_region.displayWidth(text[i..next]);
            if (disp + cw > width and end > start) break;
            if (text[i] == ' ') last_break = i;
            disp += cw;
            i = next;
            end = i;
        }
        if (i < text.len) {
            if (last_break) |lb| {
                if (lb > start) end = lb;
            }
        }
        try out.append(text[start..end]);
        start = end;
    }
}

fn padTo(writer: *std.Io.Writer, from_col: usize, to_col: usize) error{WriteFailed}!void {
    if (to_col > from_col) try writer.splatByteAll(' ', to_col - from_col);
}

/// Pad with spaces and write the right wall `│` at column `rw`.
fn closeRow(writer: *std.Io.Writer, palette: ColorPalette, col: usize, rw: usize) error{WriteFailed}!void {
    try writer.splatByteAll(' ', (rw -| 1) -| col);
    try writer.writeAll(palette.secondary);
    try writer.writeAll("│");
    try writer.writeAll(palette.reset);
    try writer.writeByte('\n');
}

/// A blank interior row: `│ ... │`.
fn sepRow(writer: *std.Io.Writer, palette: ColorPalette, rw: usize) error{WriteFailed}!void {
    try writer.writeAll(palette.secondary);
    try writer.writeAll("│");
    try writer.writeAll(palette.reset);
    try closeRow(writer, palette, 1, rw);
}

/// Render the report as a box around its source snippet.
pub fn renderReportBoxed(report: *const Report, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    const gpa = report.document.allocator;
    const elements = report.document.elements.items;

    const region = findBoxedRegion(elements) orelse {
        try renderReportPlainFallback(report, writer, palette, config);
        return;
    };

    // The headline rides the box's top edge. When the report supplies one
    // explicitly, the whole document goes below the box. Otherwise (reports
    // not yet migrated to a required headline) fall back to deriving it from
    // the lead text up to the first line break before the region.
    var summary_buf = std.array_list.Managed(u8).init(gpa);
    defer summary_buf.deinit();
    var below_start: usize = 0;
    var summary: []const u8 = report.headline;
    if (report.headline.len == 0) {
        var summary_end = region.index;
        var i: usize = 0;
        while (i < region.index) : (i += 1) {
            if (elements[i] == .line_break) {
                summary_end = i;
                break;
            }
        }
        try collectPlainText(elements[0..summary_end], &summary_buf);
        summary = std.mem.trim(u8, summary_buf.items, " ");
        below_start = if (summary_end < region.index) summary_end + 1 else region.index;
    }

    const title = report.title;
    const total: usize = config.getMaxLineWidth();

    // The label box is right-aligned: its right edge is the last column, and it
    // pokes one column past the main box's right wall.
    const inner_len = title.len + 2; // " TITLE "
    const tbw = inner_len + 2; // label box width
    const tb_left = @max((total + 1) -| tbw, 2); // 1-based column of the label box's left edge
    const rw = total -| 1; // main box right wall column
    // Summary starts at column 4 (after "┌─ ") and wraps before the label box.
    const avail: usize = @max(tb_left -| 5, 8);

    var lines = std.array_list.Managed([]const u8).init(gpa);
    defer lines.deinit();
    try wrapSummary(summary, avail, &lines);

    const sec = palette.secondary;
    const rst = palette.reset;

    // Row 1: label box top (right-aligned).
    try writer.splatByteAll(' ', tb_left -| 1);
    try writer.writeAll(sec);
    try writer.writeAll("┌");
    try writer.splatBytesAll("─", inner_len);
    try writer.writeAll("┐");
    try writer.writeAll(rst);
    try writer.writeByte('\n');

    // Row 2: top edge — "┌─ <summary[0]> ─…─┤ TITLE │".
    {
        var col: usize = 0;
        try writer.writeAll(sec);
        try writer.writeAll("┌─ ");
        col += 3;
        const line0: []const u8 = if (lines.items.len > 0) lines.items[0] else "";
        try writer.writeAll(line0);
        col += source_region.displayWidth(line0);
        try writer.writeAll(" ");
        col += 1;
        try writer.splatBytesAll("─", (tb_left -| 1) -| col);
        try writer.writeAll("┤ ");
        try writer.writeAll(palette.bold);
        try writer.writeAll(palette.primary);
        try writer.writeAll(title);
        try writer.writeAll(rst);
        try writer.writeAll(sec);
        try writer.writeAll(" │");
        try writer.writeAll(rst);
        try writer.writeByte('\n');
    }

    // Row 3: main left wall, summary line 1 (if present), then the label box
    // bottom (└─…─┬┘) whose ┬ drops the main box's right wall.
    {
        var col: usize = 0;
        try writer.writeAll(sec);
        try writer.writeAll("│");
        col += 1;
        if (lines.items.len > 1) {
            try writer.writeAll("  ");
            col += 2;
            try writer.writeAll(lines.items[1]);
            col += source_region.displayWidth(lines.items[1]);
        }
        try padTo(writer, col, tb_left -| 1);
        try writer.writeAll("└");
        try writer.splatBytesAll("─", tbw -| 3);
        try writer.writeAll("┬┘");
        try writer.writeAll(rst);
        try writer.writeByte('\n');
    }

    // Summary lines 2+ each get their own row (growing the box).
    if (lines.items.len > 2) {
        for (lines.items[2..]) |ln| {
            try writer.writeAll(sec);
            try writer.writeAll("│");
            try writer.writeAll(rst);
            try writer.writeAll("  ");
            try writer.writeAll(sec);
            try writer.writeAll(ln);
            try closeRow(writer, palette, 3 + source_region.displayWidth(ln), rw);
        }
    }

    // Blank separator row, then the source line(s) and underline.
    try sepRow(writer, palette, rw);
    {
        var line_no = region.start_line;
        var it = std.mem.splitScalar(u8, region.line_text, '\n');
        while (it.next()) |code_line| {
            var col: usize = 0;
            try writer.writeAll(sec);
            try writer.writeAll("│");
            try writer.writeAll(rst);
            col += 1;
            try writer.writeAll("  ");
            col += 2;
            // Render tabs as single spaces: a literal tab would otherwise throw
            // off both the right wall and the underline below it.
            for (code_line) |ch| try writer.writeByte(if (ch == '\t') ' ' else ch);
            col += source_region.displayWidth(code_line);
            try closeRow(writer, palette, col, rw);

            if (region.start_line == region.end_line and line_no == region.start_line) {
                var ucol: usize = 0;
                try writer.writeAll(sec);
                try writer.writeAll("│");
                try writer.writeAll(rst);
                ucol += 1;
                try writer.writeAll("  ");
                ucol += 2;
                // Roc reports columns as byte offsets; map them to display
                // columns so the underline lines up under wide/multi-byte chars.
                const start_byte = @min(@as(usize, region.start_column -| 1), code_line.len);
                const end_byte = @max(@min(@as(usize, region.end_column -| 1), code_line.len), start_byte);
                const lead = source_region.displayWidth(code_line[0..start_byte]);
                try writer.splatByteAll(' ', lead);
                ucol += lead;
                const span_width = source_region.displayWidth(code_line[start_byte..end_byte]);
                const ulen = if (span_width > 0) span_width else 1;
                try writer.writeAll(palette.error_color);
                try writer.splatBytesAll(box_underline, ulen);
                try writer.writeAll(rst);
                ucol += ulen;
                try closeRow(writer, palette, ucol, rw);
            }
            line_no += 1;
        }
    }

    // Bottom edge with the filename tucked into the bottom-right corner.
    // Bottom edge is a plain rule; the location goes on its own line beneath,
    // where a long path can't overflow the box.
    try writer.writeAll(sec);
    try writer.writeAll("└");
    try writer.splatBytesAll("─", rw -| 2);
    try writer.writeAll("┘");
    try writer.writeAll(rst);
    try writer.writeByte('\n');

    {
        const fname = if (region.filename) |f| sanitisePathForSnapshots(f) else "<source>";
        const loc = try std.fmt.allocPrint(gpa, "{s}:{}:{}", .{ fname, region.start_line, region.start_column });
        defer gpa.free(loc);
        try writer.writeAll("    ");
        try writer.writeAll(sec);
        try writer.writeAll(loc);
        try writer.writeAll(rst);
        try writer.writeByte('\n');
    }

    // Detailed explanation below the box, indented 4 spaces.
    try renderBelowContent(writer, palette, config, elements, below_start, region.index, gpa);
}

/// Render the elements after the summary's first line (excluding the region),
/// indented by 4 spaces beneath the box.
fn renderBelowContent(
    writer: *std.Io.Writer,
    palette: ColorPalette,
    config: ReportingConfig,
    elements: []const DocumentElement,
    below_start: usize,
    region_idx: usize,
    gpa: Allocator,
) (Allocator.Error || error{WriteFailed})!void {
    var buf = std.Io.Writer.Allocating.init(gpa);
    defer buf.deinit();
    var ann = std.array_list.Managed(Annotation).init(gpa);
    defer ann.deinit();

    const mid_start = @min(below_start, region_idx);
    if (mid_start < region_idx) {
        for (elements[mid_start..region_idx]) |el| try renderElementToTerminal(el, &buf.writer, palette, &ann, config);
    }
    if (region_idx + 1 < elements.len) {
        for (elements[region_idx + 1 ..]) |el| try renderElementToTerminal(el, &buf.writer, palette, &ann, config);
    }

    const trimmed = std.mem.trim(u8, buf.written(), "\n");
    if (trimmed.len == 0) {
        try writer.writeByte('\n');
        return;
    }

    try writer.writeByte('\n');
    const width: usize = config.getMaxLineWidth();
    var it = std.mem.splitScalar(u8, trimmed, '\n');
    while (it.next()) |ln| {
        try wrapAndEmitBelowLine(writer, ln, 4, width);
    }
}

/// Length of the ANSI escape sequence starting at `bytes[i]`, or 0 if there
/// isn't one there.
fn ansiEscLen(bytes: []const u8, i: usize) usize {
    if (i >= bytes.len or bytes[i] != 0x1b) return 0;
    if (i + 1 >= bytes.len or bytes[i + 1] != '[') return 1;
    var j = i + 2;
    while (j < bytes.len and bytes[j] != 'm') j += 1;
    if (j < bytes.len) j += 1; // include the terminating 'm'
    return j - i;
}

/// Emit a single below-the-box line indented by `base_indent`, word-wrapping it
/// to `width` display columns. Wrapped continuation lines line up under the
/// first line's text (preserving any leading indent the line already had, e.g.
/// for a code block). ANSI escapes pass through and don't count toward width.
fn wrapAndEmitBelowLine(writer: *std.Io.Writer, line: []const u8, base_indent: usize, width: usize) error{WriteFailed}!void {
    if (line.len == 0) {
        try writer.writeByte('\n');
        return;
    }

    // Leading prefix: ANSI escapes and spaces; its space count is the line's
    // own indent (continuation lines reproduce it).
    var prefix_end: usize = 0;
    var lead: usize = 0;
    while (prefix_end < line.len) {
        const esc = ansiEscLen(line, prefix_end);
        if (esc > 0) {
            prefix_end += esc;
        } else if (line[prefix_end] == ' ') {
            lead += 1;
            prefix_end += 1;
        } else break;
    }
    const prefix = line[0..prefix_end];
    const body = line[prefix_end..];
    const avail = @max((width -| base_indent) -| lead, 16);

    var start: usize = 0;
    var first = true;
    while (start < body.len) {
        while (start < body.len and body[start] == ' ') start += 1;
        if (start >= body.len) break;
        var i = start;
        var end = start;
        var w: usize = 0;
        var last_break: ?usize = null;
        while (i < body.len) {
            const esc = ansiEscLen(body, i);
            if (esc > 0) {
                i += esc;
                end = i;
                continue;
            }
            const seq = std.unicode.utf8ByteSequenceLength(body[i]) catch 1;
            const next = @min(i + seq, body.len);
            const cw = source_region.displayWidth(body[i..next]);
            if (w + cw > avail and end > start) break;
            if (body[i] == ' ') last_break = i;
            w += cw;
            i = next;
            end = i;
        }
        if (i < body.len) {
            if (last_break) |lb| {
                if (lb > start) end = lb;
            }
        }
        try writer.splatByteAll(' ', base_indent);
        if (first) {
            try writer.writeAll(prefix);
            first = false;
        } else {
            try writer.splatByteAll(' ', lead);
        }
        try writer.writeAll(body[start..end]);
        try writer.writeByte('\n');
        start = end;
    }
    if (first) {
        // Body was empty (prefix only) — still emit it.
        try writer.splatByteAll(' ', base_indent);
        try writer.writeAll(prefix);
        try writer.writeByte('\n');
    }
}

/// Fallback for reports with no source region: title line then the body.
fn renderReportPlainFallback(report: *const Report, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try writer.writeAll(palette.bold);
    try writer.writeAll(palette.primary);
    try writer.writeAll(report.title);
    try writer.writeAll(palette.reset);
    try writer.writeByte('\n');
    if (report.headline.len > 0) {
        try writer.writeByte('\n');
        const width: usize = config.getMaxLineWidth();
        var it = std.mem.splitScalar(u8, report.headline, '\n');
        while (it.next()) |ln| {
            try wrapAndEmitBelowLine(writer, ln, 0, width);
        }
    }
    try renderDocumentToTerminal(&report.document, writer, palette, config);
    try writer.writeByte('\n');
}

/// Render a report to HTML.
pub fn renderReportToHtml(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    const title_class = switch (report.severity) {
        .info => "info",
        .fatal => "error",
        .runtime_error => "error",
        .warning => "warning",
    };

    try writer.print("<div class=\"report {s}\">\n", .{title_class});
    try writer.writeAll("<h1 class=\"report-title\">");
    try writeEscapedHtml(writer, report.title);
    try writer.writeAll("</h1>\n");
    try writer.writeAll("<div class=\"report-content\">\n");
    if (report.headline.len > 0) {
        try writeEscapedHtml(writer, report.headline);
        try writer.writeAll("<br>\n");
    }
    try renderDocumentToHtml(&report.document, writer, config);
    try writer.writeAll("</div>\n</div>\n");
}

/// Render a report for language server protocol.
pub fn renderReportToLsp(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    // LSP typically wants plain text without formatting
    try writer.writeAll(report.title);
    try writer.writeAll("\n\n");
    if (report.headline.len > 0) {
        try writer.writeAll(report.headline);
        try writer.writeByte('\n');
    }
    try renderDocumentToLsp(&report.document, writer, config);
}

/// Render a document to the specified target format.
pub fn renderDocument(document: *const Document, writer: *std.Io.Writer, target: RenderTarget) (Allocator.Error || error{WriteFailed})!void {
    // Create appropriate config based on render target
    const config = switch (target) {
        .color_terminal => ReportingConfig.initColorTerminal(),
        .markdown => ReportingConfig.initMarkdown(),
        .html => ReportingConfig.initHtml(),
        .language_server => ReportingConfig.initLsp(),
    };

    switch (target) {
        .color_terminal => {
            const palette = ColorUtils.getPaletteForConfig(config);
            try renderDocumentToTerminal(document, writer, palette, config);
        },
        .markdown => try renderDocumentToMarkdown(document, writer, config),
        .html => try renderDocumentToHtml(document, writer, config),
        .language_server => try renderDocumentToLsp(document, writer, config),
    }
}

/// Render a document to terminal with color support.
pub fn renderDocumentToTerminal(document: *const Document, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    var ann_sfa = std.heap.stackFallback(16 * @sizeOf(Annotation), document.allocator);
    var annotation_stack = std.array_list.Managed(Annotation).init(ann_sfa.get());
    defer annotation_stack.deinit();

    for (document.elements.items) |element| {
        try renderElementToTerminal(element, writer, palette, &annotation_stack, config);
    }
}

/// Render a document to plain text.
pub fn renderDocumentToMarkdown(document: *const Document, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    for (document.elements.items) |element| {
        try renderElementToMarkdown(element, writer, config);
    }
}

/// Render a document to HTML.
pub fn renderDocumentToHtml(document: *const Document, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    var ann_sfa = std.heap.stackFallback(16 * @sizeOf(Annotation), document.allocator);
    var annotation_stack = std.array_list.Managed(Annotation).init(ann_sfa.get());
    defer annotation_stack.deinit();

    for (document.elements.items) |element| {
        try renderElementToHtml(element, writer, &annotation_stack, config);
    }
}

/// Render a document for language server protocol.
pub fn renderDocumentToLsp(document: *const Document, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    for (document.elements.items) |element| {
        try renderElementToLsp(element, writer, config);
    }
}

// Terminal rendering functions

fn renderElementToTerminal(element: DocumentElement, writer: *std.Io.Writer, palette: ColorPalette, annotation_stack: *std.array_list.Managed(Annotation), config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    switch (element) {
        .text => |text| try writer.writeAll(text),
        .annotated => |annotated| {
            const color = getAnnotationColor(annotated.annotation, palette);
            try writer.writeAll(color);
            try writer.writeAll(annotated.content);
            try writer.writeAll(palette.reset);
        },
        .line_break => try writer.writeAll("\n"),
        .link => |url| {
            try writer.writeAll("<");
            try writer.writeAll(url);
            try writer.writeAll(">");
        },
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
            const rule_width = width orelse config.getMaxLineWidth();
            var i: u32 = 0;
            while (i < rule_width) : (i += 1) {
                try writer.writeAll("─");
            }
        },
        .annotation_start => |annotation| {
            try annotation_stack.append(annotation);
            const color = getAnnotationColor(annotation, palette);
            try writer.writeAll(color);
        },
        .annotation_end => {
            if (annotation_stack.pop()) |_| {
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
                try renderElementToTerminal(elem, writer, palette, annotation_stack, config);
            }
        },
        .horizontal_concat => |elements| {
            for (elements) |elem| {
                try renderElementToTerminal(elem, writer, palette, annotation_stack, config);
            }
        },
        .source_code_region => |region| {
            // Calculate the width needed for line numbers
            const line_num_width = source_region.calculateLineNumberWidth(region.end_line);

            try renderSourceLocationHeader(writer, palette, config, line_num_width, region.filename, region.start_line, region.start_column);

            // Extract and print the source lines with line numbers
            const lines = region.line_text;
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

                    // Print leading whitespace, preserving tabs from the source line
                    try source_region.printLeadingWhitespace(writer, line, region.start_column);

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
        .source_code_with_underlines => |data| {
            // Calculate the width needed for line numbers
            const line_num_width = source_region.calculateLineNumberWidth(data.display_region.end_line);

            try renderSourceLocationHeader(writer, palette, config, line_num_width, data.display_region.filename, data.display_region.start_line, data.display_region.start_column);

            // Extract and print the source lines with line numbers
            const lines = data.display_region.line_text;
            var line_num = data.display_region.start_line;
            var iter = std.mem.tokenizeScalar(u8, lines, '\n');
            while (iter.next()) |line| {
                // Print line number
                try writer.writeAll(palette.secondary);
                try source_region.formatLineNumber(writer, line_num, line_num_width);
                try writer.writeAll(" │ ");
                try writer.writeAll(palette.reset);

                // Print the line content
                try writer.writeAll(line);
                try writer.writeAll("\n");

                // Check if any underline regions apply to this line
                var has_underlines = false;
                for (data.underline_regions) |underline| {
                    if (underline.start_line == line_num and underline.start_line == underline.end_line) {
                        has_underlines = true;
                        break;
                    }
                }

                if (has_underlines) {
                    // Print the line prefix
                    try writer.writeAll(palette.secondary);
                    try source_region.printSpaces(writer, line_num_width);
                    try writer.writeAll(" │ ");
                    try writer.writeAll(palette.reset);

                    // Print all underlines for this line on the same line
                    var col_position: u32 = 1;
                    for (data.underline_regions) |underline| {
                        if (underline.start_line == line_num and underline.start_line == underline.end_line) {
                            // Print whitespace up to the start column
                            if (underline.start_column > col_position) {
                                if (col_position == 1) {
                                    // First underline: preserve tabs from source
                                    try source_region.printLeadingWhitespace(writer, line, underline.start_column);
                                } else {
                                    // Subsequent underlines: just use spaces
                                    try source_region.printSpaces(writer, underline.start_column - col_position);
                                }
                            }

                            // Print the underline
                            const color = getAnnotationColor(underline.annotation, palette);
                            try writer.writeAll(color);
                            const underline_len = source_region.calculateUnderlineLength(underline.start_column, underline.end_column);
                            var i: u32 = 0;
                            while (i < underline_len) : (i += 1) {
                                try writer.writeAll("^");
                            }
                            try writer.writeAll(palette.reset);

                            // Update column position
                            col_position = underline.end_column;
                        }
                    }
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

/// Render the `┌─` location header line plus the `│` separator beneath it.
///
/// The box-drawing run stretches to the full terminal width so the
/// `filename:line:col` is right-aligned to the terminal edge:
///
///     ┌──────────────────────────── examples/foo.roc:12:34
///     │
fn renderSourceLocationHeader(
    writer: *std.Io.Writer,
    palette: ColorPalette,
    config: ReportingConfig,
    line_num_width: u32,
    filename: ?[]const u8,
    start_line: u32,
    start_column: u32,
) error{WriteFailed}!void {
    const total_width: usize = config.getMaxLineWidth();

    // Display width of the trailing "filename:line:col" (or "<source>:line:col").
    const path = if (filename) |f| sanitisePathForSnapshots(f) else "<source>";
    const loc_len = path.len + 1 + decimalWidth(start_line) + 1 + decimalWidth(start_column);

    // Layout: [gutter spaces] " ┌" [─ fill] " " [location]
    // Everything before the fill occupies `line_num_width + 2` columns, plus a
    // single space before the location, so fill the remainder with ─ to push
    // the location flush against the terminal edge.
    const used = @as(usize, line_num_width) + 2 + 1 + loc_len;
    const fill = if (total_width > used) total_width - used else 1;

    try source_region.printSpaces(writer, line_num_width);
    try writer.writeAll(palette.secondary);
    try writer.writeAll(" ┌");
    try writer.splatBytesAll("─", fill);
    try writer.writeAll(" ");
    try writer.print("{s}:{}:{}\n", .{ path, start_line, start_column });
    try writer.writeAll(palette.reset);

    // Separator line beneath the header, with the `│` under the `┌`.
    try source_region.printSpaces(writer, line_num_width);
    try writer.writeAll(palette.secondary);
    try writer.writeAll(" │\n");
    try writer.writeAll(palette.reset);
}

/// Number of decimal digits needed to print `n` (e.g. 1 for 7, 4 for 1242).
fn decimalWidth(n: u32) usize {
    if (n == 0) return 1;
    return std.math.log10(n) + 1;
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

fn renderElementToMarkdown(element: DocumentElement, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    switch (element) {
        .text => |text| try writer.writeAll(text),
        .annotated => |annotated| {
            switch (annotated.annotation) {
                .emphasized => {
                    try writer.writeAll("**");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("**");
                },
                .keyword, .inline_code, .symbol, .symbol_qualified, .symbol_unqualified, .record_field, .tag_name, .binary_operator => {
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
                    try writer.writeAll("**⚠ ");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("**");
                },
                .suggestion => {
                    try writer.writeAll("**");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("**");
                },
                .code_block => {
                    try writer.writeAll("```roc\n");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("\n```");
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
                    try writer.writeAll("`");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("`");
                },
                .module_name => {
                    try writer.writeAll("**");
                    try writer.writeAll(annotated.content);
                    try writer.writeAll("**");
                },
                .source_region => try writer.writeAll(annotated.content),
                .reflowing_text => try writer.writeAll(annotated.content),
            }
        },
        .line_break => try writer.writeAll("\n"),
        .link => |url| {
            try writer.writeAll("<");
            try writer.writeAll(url);
            try writer.writeAll(">");
        },
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
        .horizontal_rule => {
            try writer.writeAll("\n---\n");
        },
        .annotation_start, .annotation_end => {}, // Handled in annotated case
        .raw => |content| try writer.writeAll(content),
        .reflowing_text => |text| try writer.writeAll(text),
        .vertical_stack => |elements| {
            for (elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll("\n");
                try renderElementToMarkdown(elem, writer, config);
            }
        },
        .horizontal_concat => |elements| {
            for (elements) |elem| {
                try renderElementToMarkdown(elem, writer, config);
            }
        },
        .source_code_region => |region| {
            if (region.filename) |filename| {
                try writer.print("**{s}:{d}:{d}:{d}:{d}:**\n", .{ sanitisePathForSnapshots(filename), region.start_line, region.start_column, region.end_line, region.end_column });
            }
            try writer.writeAll("```roc\n");
            const lines = region.line_text;
            try writer.writeAll(lines);
            try writer.writeAll("\n```\n");

            // Add underline for single-line regions in markdown
            if (region.start_line == region.end_line) {
                // Recreate the exact whitespace from the source line up to the start column.
                // This is critical so that leading tabs end up being in exactly the same place(s).
                const chars_before_target = region.start_column - 1;
                if (chars_before_target > 0 and lines.len >= chars_before_target) {
                    // Extract and print the exact whitespace characters (including tabs) from the source
                    for (lines[0..chars_before_target]) |char| {
                        if (char == '\t') {
                            try writer.writeAll("\t");
                        } else if (char == ' ') {
                            try writer.writeAll(" ");
                        } else {
                            // For non-whitespace characters, use a space to maintain positioning
                            try writer.writeAll(" ");
                        }
                    }
                }

                // Print the underline
                const underline_len = source_region.calculateUnderlineLength(region.start_column, region.end_column);
                var i: u32 = 0;
                while (i < underline_len) : (i += 1) {
                    try writer.writeAll("^");
                }
                try writer.writeAll("\n");
            }
        },
        .source_code_with_underlines => |data| {
            if (data.display_region.filename) |filename| {
                try writer.print("**{s}:{}:{}:**\n", .{ sanitisePathForSnapshots(filename), data.display_region.start_line, data.display_region.start_column });
            }
            try writer.writeAll("```roc\n");
            const lines = data.display_region.line_text;
            try writer.writeAll(lines);
            try writer.writeAll("\n```\n");

            // Show underlines as text
            var line_num = data.display_region.start_line;
            var iter = std.mem.tokenizeScalar(u8, lines, '\n');
            while (iter.next()) |_| {
                var has_underlines = false;
                for (data.underline_regions) |underline| {
                    if (underline.start_line == line_num and underline.start_line == underline.end_line) {
                        has_underlines = true;
                        break;
                    }
                }

                if (has_underlines) {
                    var col_position: u32 = 1;
                    for (data.underline_regions) |underline| {
                        if (underline.start_line == line_num and underline.start_line == underline.end_line) {
                            // Print spaces up to the start column
                            if (underline.start_column > col_position) {
                                var i: u32 = 0;
                                while (i < underline.start_column - col_position) : (i += 1) {
                                    try writer.writeAll(" ");
                                }
                            }

                            // Print the underline
                            const underline_len = source_region.calculateUnderlineLength(underline.start_column, underline.end_column);
                            var i: u32 = 0;
                            while (i < underline_len) : (i += 1) {
                                try writer.writeAll("^");
                            }

                            // Update column position
                            col_position = underline.end_column;
                        }
                    }
                    try writer.writeAll("\n");
                }
                line_num += 1;
            }
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

fn renderElementToHtml(element: DocumentElement, writer: *std.Io.Writer, annotation_stack: *std.array_list.Managed(Annotation), config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
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
        .link => |url| {
            try writer.writeAll("&lt;<a href=\"");
            try writeEscapedHtml(writer, url);
            try writer.writeAll("\">");
            try writeEscapedHtml(writer, url);
            try writer.writeAll("</a>&gt;");
        },
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
            const rule_width = width orelse config.getMaxLineWidth();
            try writer.print("<hr style=\"width: {d}ch;\">\n", .{rule_width});
        },
        .annotation_start => |annotation| {
            try annotation_stack.append(annotation);
            const tag = getAnnotationHtmlTag(annotation);
            const class = getAnnotationHtmlClass(annotation);
            try writer.print("<{s} class=\"{s}\">", .{ tag, class });
        },
        .annotation_end => {
            if (annotation_stack.items.len > 0) {
                const annotation = annotation_stack.pop().?;
                const tag = getAnnotationHtmlTag(annotation);
                try writer.print("</{s}>", .{tag});
            }
        },
        .raw => |content| try writer.writeAll(content),
        .reflowing_text => |text| try writeEscapedHtml(writer, text),
        .vertical_stack => |elements| {
            try writer.writeAll("<div class=\"vertical-stack\">\n");
            for (elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll("\n");
                try renderElementToHtml(elem, writer, annotation_stack, config);
            }
            try writer.writeAll("</div>\n");
        },
        .horizontal_concat => |elements| {
            try writer.writeAll("<span class=\"horizontal-concat\">");
            for (elements) |elem| {
                try renderElementToHtml(elem, writer, annotation_stack, config);
            }
            try writer.writeAll("</span>");
        },
        .source_code_region => |region| {
            try writer.writeAll("<div class=\"source-region\">");
            if (region.filename) |filename| {
                try writer.print("<span class=\"filename\">{s}:{}:{}:{}:{}:</span> ", .{ sanitisePathForSnapshots(filename), region.start_line, region.start_column, region.end_line, region.end_column });
            }
            const class = getAnnotationHtmlClass(region.region_annotation);
            try writer.print("<pre class=\"{s}\">", .{class});
            const lines = region.line_text;
            try writeEscapedHtml(writer, lines);
            try writer.writeAll("</pre></div>");
        },
        .source_code_with_underlines => |data| {
            try writer.writeAll("<div class=\"source-region\">");
            if (data.display_region.filename) |filename| {
                try writer.print("<div class=\"source-location\">{s}:{}:{}</div>", .{ filename, data.display_region.start_line, data.display_region.start_column });
            }
            try writer.writeAll("<pre class=\"source-code\">");
            const lines = data.display_region.line_text;
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

fn writeEscapedHtml(writer: *std.Io.Writer, text: []const u8) error{WriteFailed}!void {
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

fn renderElementToLsp(element: DocumentElement, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    switch (element) {
        .text => |text| try writer.writeAll(text),
        .annotated => |annotated| try writer.writeAll(annotated.content),
        .line_break => try writer.writeAll("\n"),
        .link => |url| {
            try writer.writeAll("<");
            try writer.writeAll(url);
            try writer.writeAll(">");
        },
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
            const rule_width = width orelse config.getMaxLineWidth();
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
                try renderElementToLsp(elem, writer, config);
            }
        },
        .horizontal_concat => |elements| {
            for (elements) |elem| {
                try renderElementToLsp(elem, writer, config);
            }
        },
        .source_code_region => |region| {
            if (region.filename) |filename| {
                try writer.print("{s}:{}:{}:{}:{}: ", .{ sanitisePathForSnapshots(filename), region.start_line, region.start_column, region.end_line, region.end_column });
            }
            const lines = region.line_text;
            try writer.writeAll(lines);
            try writer.writeAll("\n");
        },
        .source_code_with_underlines => |data| {
            if (data.display_region.filename) |filename| {
                try writer.print("{s}:{}:{}: ", .{ filename, data.display_region.start_line, data.display_region.start_column });
            }
            const lines = data.display_region.line_text;
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
    var report = try Report.init(testing.allocator, "TEST ERROR", "Something went wrong.", .runtime_error);
    defer report.deinit();

    try report.document.addText("This is a test error message.");

    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    try renderReportToMarkdown(&report, &writer.writer, ReportingConfig.initMarkdown());

    try testing.expect(std.mem.find(u8, writer.written(), "**TEST ERROR**") != null);
    try testing.expect(std.mem.find(u8, writer.written(), "This is a test error message.") != null);
}

test "render document with annotations to markdown" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    try doc.addText("Hello ");
    try doc.addAnnotated("world", .emphasized);
    try doc.addText("!");

    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    try renderDocumentToMarkdown(&doc, &writer.writer, ReportingConfig.initMarkdown());

    try testing.expectEqualStrings("Hello **world**!", writer.written());
}

test "render HTML escaping" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    try doc.addText("<script>alert('test')</script>");

    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    try renderDocumentToHtml(&doc, &writer.writer, ReportingConfig.initHtml());

    try testing.expect(std.mem.find(u8, writer.written(), "&lt;script&gt;") != null);
    try testing.expect(std.mem.find(u8, writer.written(), "<script>") == null);
}

test "render indentation and spacing" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    try doc.addIndent(2);
    try doc.addText("indented");
    try doc.addSpace(3);
    try doc.addText("spaced");

    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    try renderDocumentToMarkdown(&doc, &writer.writer, ReportingConfig.initMarkdown());

    try testing.expectEqualStrings("        indented   spaced", writer.written());
}

test "render horizontal rule" {
    var doc = Document.init(testing.allocator);
    defer doc.deinit();

    try doc.addHorizontalRule(5);

    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    try renderDocumentToMarkdown(&doc, &writer.writer, ReportingConfig.initMarkdown());

    try testing.expectEqualStrings("\n---\n", writer.written());
}
