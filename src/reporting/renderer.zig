//! Provides functions to render Reports and Documents to various
//! output formats without the complexity of vtables or interfaces.

const std = @import("std");
const builtin = @import("builtin");

const source_region = @import("source_region.zig");
const Allocator = std.mem.Allocator;
const Report = @import("report.zig").Report;
const Document = @import("document.zig").Document;
const DocumentElement = @import("document.zig").DocumentElement;
const Annotation = @import("document.zig").Annotation;
const SourceCodeDisplayRegion = @import("document.zig").SourceCodeDisplayRegion;
const SourceCodeWithUnderlines = @import("document.zig").SourceCodeWithUnderlines;
const SourceCodeMultiRegion = @import("document.zig").SourceCodeMultiRegion;
const SourceLocation = @import("document.zig").SourceLocation;
const UnderlineRegion = @import("document.zig").UnderlineRegion;
const ColorPalette = @import("style.zig").ColorPalette;
const ColorUtils = @import("style.zig").ColorUtils;
pub const ReportingConfig = @import("config.zig").ReportingConfig;

/// TODO find a better solution this is temporary to make CI happy
///
/// Makes a file path relative for error reporting.
/// For snapshot files, returns just the filename.
/// For other files, returns the original path.
pub fn sanitisePathForSnapshots(path: []const u8) []const u8 {

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

/// Byte offset where `path`'s filename starts—just past the last separator,
/// or 0 when there is none. Both separators are handled so the split is the
/// same on every platform.
fn filenameStart(path: []const u8) usize {
    var start: usize = 0;
    for (path, 0..) |c, i| {
        if (c == '/' or c == '\\') start = i + 1;
    }
    return start;
}

/// Supported rendering targets.
pub const RenderTarget = enum {
    color_terminal,
    markdown,
    html,
    language_server,
};

/// In debug builds, enforce that a report's headline reads as a complete
/// sentence (ending in a period) or introduces a code block (ending in a
/// colon). The headline is the one-sentence summary below the diagnostic header
/// (or below the title in markdown/HTML/LSP). Compiled out of
/// release builds. Reports that have not yet been migrated to a headline
/// (none of the elements carry text) are exempt.
fn assertValidHeadline(report: *const Report) void {
    if (builtin.mode != .Debug) return;

    var last: u8 = 0;
    for (report.headline.elements.items) |el| {
        const text = el.getText() orelse continue;
        var i: usize = text.len;
        while (i > 0) : (i -= 1) {
            const c = text[i - 1];
            if (c != ' ' and c != '\t' and c != '\n') {
                last = c;
                break;
            }
        }
    }
    // `last` stays 0 when the headline has no text content at all (legacy
    // reports without a headline)—those are exempt.
    std.debug.assert(last == 0 or last == '.' or last == ':');
}

/// The default reporting configuration for a render target.
fn configFor(target: RenderTarget) ReportingConfig {
    return switch (target) {
        .color_terminal => ReportingConfig.initColorTerminal(),
        .markdown => ReportingConfig.initMarkdown(),
        .html => ReportingConfig.initHtml(),
        .language_server => ReportingConfig.initLsp(),
    };
}

/// Render a report to the specified target format.
pub fn renderReport(report: *const Report, writer: *std.Io.Writer, target: RenderTarget) (Allocator.Error || error{WriteFailed})!void {
    try renderReportWithConfig(report, writer, configFor(target));
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
    try renderReportToTerminalLayout(report, writer, palette, config);
}

/// Render a report to plain markdown. This is the stable, machine-friendly
/// format used by internal tests and EXPECTED-section tooling.
pub fn renderReportToMarkdown(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    assertValidHeadline(report);
    try writer.writeAll("**");
    try writer.writeAll(report.title);
    try writer.writeAll("**\n");
    if (report.headline.elementCount() > 0) {
        try renderDocumentToMarkdown(&report.headline, writer, config);
        try writer.writeByte('\n');
    }
    try renderDocumentToMarkdown(&report.document, writer, config);
    try writer.writeAll("\n\n");
}

/// Render a report as plain text using the terminal layout without ANSI color.
/// Used for snapshot PROBLEMS sections and non-TTY user output.
pub fn renderReportToPlain(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try renderReportToTerminalLayout(report, writer, ColorPalette.NO_COLOR, config);
}

// Header-based report rendering.
//
// Lays a report out as a header line followed by the content, for example:
//
// Header line: ✗ type mismatch, followed by location.
//
// <one-line summary>
//
// <source code>
//  ^^^^^^
//
// <detailed explanation>
//
// The header line starts with a unicode icon representing the severity,
// followed by the title in lowercase, a dash-filled separator, and the location.
// The same layout is used for the colored terminal output and the plain
// markdown/snapshot output—the only difference is the palette (ANSI vs NO_COLOR).

const IconAndColor = struct {
    icon: []const u8,
    color: []const u8,
    width: usize,
};

fn getSeverityIcon(severity: @import("severity.zig").Severity, title: []const u8, palette: ColorPalette) IconAndColor {
    if (std.mem.eql(u8, title, "FAIL")) return .{ .icon = "✗", .color = palette.error_color, .width = 1 };
    return switch (severity) {
        .fatal, .runtime_error => .{ .icon = "✗", .color = palette.error_color, .width = 1 },
        .warning => .{ .icon = "●", .color = palette.warning, .width = 1 },
    };
}

/// A source region pulled out of a document, normalized for rendering.
const PrimaryRegion = struct {
    index: usize,
    filename: ?[]const u8,
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    line_text: []const u8,
};

fn findPrimaryRegion(elements: []const DocumentElement) ?PrimaryRegion {
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
            .text,
            .annotated,
            .line_break,
            .indent,
            .space,
            .horizontal_rule,
            .annotation_start,
            .annotation_end,
            .raw,
            .reflowing_text,
            .link,
            .vertical_stack,
            .horizontal_concat,
            .source_code_multi_region,
            .source_location,
            => {},
        }
    }
    return null;
}

/// Append the plain text of a run of elements (line breaks become spaces), and
/// record the terminal color of each appended byte so the summary text can
/// keep inline code, symbols, etc. colored. `colors` ends up the same length as
/// `plain`. In a no-color palette every color is "" and this is just plain text.
/// Terminal output never adds markup characters. ANSI color distinguishes code
/// spans when enabled; plain output remains literal text suitable for copying.
fn wantsBacktick(_: ColorPalette, _: Annotation) bool {
    return false;
}

fn collectStyledText(
    elements: []const DocumentElement,
    palette: ColorPalette,
    plain: *std.array_list.Managed(u8),
    colors: *std.array_list.Managed([]const u8),
) Allocator.Error!void {
    for (elements) |el| {
        switch (el) {
            .text, .reflowing_text, .raw => |t| for (t) |b| {
                try plain.append(b);
                try colors.append("");
            },
            .annotated => |a| {
                const c = palette.colorForAnnotation(a.annotation);
                const tick = wantsBacktick(palette, a.annotation);
                if (tick) {
                    try plain.append('`');
                    try colors.append(c);
                }
                for (a.content) |b| {
                    try plain.append(b);
                    try colors.append(c);
                }
                if (tick) {
                    try plain.append('`');
                    try colors.append(c);
                }
            },
            .line_break => {
                try plain.append(' ');
                try colors.append("");
            },
            .space => |n| {
                var i: u32 = 0;
                while (i < n) : (i += 1) {
                    try plain.append(' ');
                    try colors.append("");
                }
            },
            .indent,
            .horizontal_rule,
            .annotation_start,
            .annotation_end,
            .link,
            .vertical_stack,
            .horizontal_concat,
            .source_code_region,
            .source_code_multi_region,
            .source_code_with_underlines,
            .source_location,
            => {},
        }
    }
}

/// Emit `line` (a slice of `plain`) with its per-byte colors. Unstyled bytes do
/// not emit an ANSI sequence; moving from a colored span back to plain text
/// emits one reset. ANSI sequences do not count toward display width.
fn writeColoredSummary(
    writer: *std.Io.Writer,
    line: []const u8,
    plain: []const u8,
    colors: []const []const u8,
    palette: ColorPalette,
) error{WriteFailed}!void {
    if (line.len == 0) return;
    const base = @intFromPtr(line.ptr) - @intFromPtr(plain.ptr);
    var current: []const u8 = "";
    for (line, 0..) |b, k| {
        const c = colors[base + k];
        if (!std.mem.eql(u8, c, current)) {
            if (c.len == 0) {
                if (current.len > 0) try writer.writeAll(palette.reset);
            } else {
                try writer.writeAll(c);
            }
            current = c;
        }
        try writer.writeByte(b);
    }
    if (current.len > 0) try writer.writeAll(palette.reset);
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

/// Write `s` with every ASCII letter uppercased. Snapshot EXPECTED sections use
/// uppercase titles even though rendered diagnostic headers use lowercase.
pub fn writeShouted(writer: *std.Io.Writer, s: []const u8) error{WriteFailed}!void {
    for (s) |c| {
        try writer.writeByte(if (c >= 'a' and c <= 'z') c - ('a' - 'A') else c);
    }
}

/// Write `s` to `writer`, converting ASCII characters to lowercase. Used for
/// the header text. Titles are validated as ASCII, so a byte
/// iterator suffices.
pub fn writeLowercased(writer: *std.Io.Writer, s: []const u8) error{WriteFailed}!void {
    for (s) |c| {
        try writer.writeByte(std.ascii.toLower(c));
    }
}

/// Write a `path/to/Scalar.roc:141:1` location, with the filename itself in the
/// cyan so it stands out from the dim directories, `:line:col`, and horizontal
/// rule around it. Leaves the writer in the secondary color.
fn writeLocation(
    writer: *std.Io.Writer,
    palette: ColorPalette,
    path: []const u8,
    line: u32,
    column: u32,
) error{WriteFailed}!void {
    const start = filenameStart(path);
    if (start > 0) {
        try writer.writeAll(palette.secondary);
        try writer.writeAll(path[0..start]);
    }
    try writer.writeAll(palette.primary);
    try writer.writeAll(path[start..]);
    try writer.writeAll(palette.secondary);
    try writer.print(":{d}:{d}", .{ line, column });
}

fn renderHeaderLine(
    writer: *std.Io.Writer,
    palette: ColorPalette,
    config: ReportingConfig,
    icon_info: IconAndColor,
    title: []const u8,
    filename: ?[]const u8,
    start_line: u32,
    start_column: u32,
) error{WriteFailed}!void {
    const total_w = @min(config.getMaxLineWidth(), 120);
    const icon_w: usize = icon_info.width;
    const prefix_w = 3 + icon_w + 1 + source_region.displayWidth(title) + 1;

    var loc_len: usize = 0;
    const fname = if (filename) |f| sanitisePathForSnapshots(f) else null;
    if (fname) |f| {
        loc_len = 1 + source_region.displayWidth(f) + 1 + decimalWidth(start_line) + 1 + decimalWidth(start_column);
    }

    const dashes = @max(total_w -| prefix_w -| loc_len, 1);

    try writer.writeAll(palette.secondary);
    try writer.writeAll("── ");
    try writer.writeAll(icon_info.color);
    try writer.writeAll(icon_info.icon);
    try writer.writeByte(' ');
    try writeLowercased(writer, title);
    try writer.writeByte(' ');
    try writer.writeAll(palette.secondary);
    try writer.splatBytesAll("─", dashes);
    if (fname) |f| {
        try writer.writeByte(' ');
        try writeLocation(writer, palette, f, start_line, start_column);
    }
    try writer.writeAll(palette.reset);
    try writer.writeByte('\n');
}

/// Render the report with a header and its source snippet.
fn renderReportToTerminalLayout(report: *const Report, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    assertValidHeadline(report);
    const gpa = report.document.allocator;
    const elements = report.document.elements.items;

    const region = findPrimaryRegion(elements) orelse {
        try renderReportPlainFallback(report, writer, palette, config);
        return;
    };

    var summary_buf = std.array_list.Managed(u8).init(gpa);
    defer summary_buf.deinit();
    var color_buf = std.array_list.Managed([]const u8).init(gpa);
    defer color_buf.deinit();
    var below_start: usize = 0;
    if (report.headline.elementCount() > 0) {
        try collectStyledText(report.headline.elements.items, palette, &summary_buf, &color_buf);
    } else {
        var summary_end = region.index;
        var i: usize = 0;
        while (i < region.index) : (i += 1) {
            if (elements[i] == .line_break) {
                summary_end = i;
                break;
            }
        }
        try collectStyledText(elements[0..summary_end], palette, &summary_buf, &color_buf);
        below_start = if (summary_end < region.index) summary_end + 1 else region.index;
    }
    const summary = std.mem.trim(u8, summary_buf.items, " ");
    const title = report.title;
    const icon_info = getSeverityIcon(report.severity, title, palette);

    try renderHeaderLine(writer, palette, config, icon_info, title, region.filename, region.start_line, region.start_column);

    if (summary.len > 0) {
        try writer.writeByte('\n');
        var lines = std.array_list.Managed([]const u8).init(gpa);
        defer lines.deinit();
        const avail: usize = config.getMaxLineWidth();
        try wrapSummary(summary, avail, &lines);
        for (lines.items) |ln| {
            try writeColoredSummary(writer, ln, summary_buf.items, color_buf.items, palette);
            try writer.writeByte('\n');
        }
    }

    try writer.writeByte('\n');

    var snippet = std.array_list.Managed([]u8).init(gpa);
    defer {
        for (snippet.items) |l| gpa.free(l);
        snippet.deinit();
    }
    var leads = std.array_list.Managed(usize).init(gpa);
    defer leads.deinit();
    {
        var it = std.mem.splitScalar(u8, region.line_text, '\n');
        while (it.next()) |line| {
            var i: usize = 0;
            var lead: usize = 0;
            while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {
                lead += if (line[i] == '\t') 4 else 1;
            }
            const rest = line[i..];
            const buf = try gpa.alloc(u8, lead + rest.len);
            @memset(buf[0..lead], ' ');
            @memcpy(buf[lead..], rest);
            try snippet.append(buf);
            try leads.append(lead);
        }
    }
    var common: usize = std.math.maxInt(usize);
    for (snippet.items, leads.items) |l, lead| {
        if (lead < l.len) common = @min(common, lead);
    }
    if (common == std.math.maxInt(usize)) common = 0;

    var start_col_adj = region.start_column;
    var end_col_adj = region.end_column;
    {
        var orig_lead: usize = 0;
        while (orig_lead < region.line_text.len and
            (region.line_text[orig_lead] == ' ' or region.line_text[orig_lead] == '\t')) : (orig_lead += 1)
        {}
        const exp0: i64 = if (leads.items.len > 0) @intCast(leads.items[0]) else 0;
        const delta: i64 = exp0 - @as(i64, @intCast(common)) - @as(i64, @intCast(orig_lead));
        start_col_adj = @intCast(@max(@as(i64, @intCast(region.start_column)) + delta, 1));
        end_col_adj = @intCast(@max(@as(i64, @intCast(region.end_column)) + delta, 1));
    }

    {
        var line_no = region.start_line;
        for (snippet.items) |full_line| {
            const code_line = full_line[@min(common, full_line.len)..];
            const is_underline_line = region.start_line == region.end_line and line_no == region.start_line;

            for (code_line) |ch| try writer.writeByte(if (ch == '\t') ' ' else ch);
            try writer.writeByte('\n');

            if (is_underline_line) {
                const start_byte = @min(@as(usize, start_col_adj -| 1), code_line.len);
                const end_byte = @max(@min(@as(usize, end_col_adj -| 1), code_line.len), start_byte);

                const lead = source_region.displayWidth(code_line[0..start_byte]);
                try source_region.printSpaces(writer, @intCast(lead));

                const span_width = if (end_byte > start_byte) source_region.displayWidth(code_line[start_byte..end_byte]) else 0;
                const ulen = if (span_width > 0) span_width else 1;

                try writer.writeAll(icon_info.color);
                try writer.splatBytesAll("^", ulen);
                try writer.writeAll(palette.reset);
                try writer.writeByte('\n');
            }
            line_no += 1;
        }
    }

    try renderBelowContent(writer, palette, config, icon_info.color, elements, below_start, region.index, gpa);
}

/// Render the elements after the summary's first line (excluding the region)
fn renderBelowContent(
    writer: *std.Io.Writer,
    palette: ColorPalette,
    config: ReportingConfig,
    highlight_color: []const u8,
    elements: []const DocumentElement,
    below_start: usize,
    region_idx: usize,
    gpa: Allocator,
) (Allocator.Error || error{WriteFailed})!void {
    const width: usize = config.getMaxLineWidth();
    var buf = std.Io.Writer.Allocating.init(gpa);
    defer buf.deinit();
    var ann = std.array_list.Managed(Annotation).init(gpa);
    defer ann.deinit();
    var ctx = RenderCtx{ .config = config, .palette = palette, .annotation_stack = &ann };

    var started = false;
    var idx = @min(below_start, region_idx);
    while (idx < elements.len) : (idx += 1) {
        if (idx == region_idx) continue;
        switch (elements[idx]) {
            .source_code_region => |r| {
                const text = std.mem.trim(u8, buf.written(), " \n\r\t");
                if (!started) {
                    try writer.writeByte('\n');
                    started = true;
                }
                try renderSecondaryRegion(writer, palette, highlight_color, r.start_line, r.start_column, r.end_line, r.end_column, r.line_text, text, gpa);
                buf.clearRetainingCapacity();
            },
            .source_code_with_underlines => |d| {
                const dr = d.display_region;
                var sc = dr.start_column;
                var ec = dr.end_column;
                if (d.underline_regions.len > 0) {
                    sc = d.underline_regions[0].start_column;
                    ec = d.underline_regions[0].end_column;
                }
                const text = std.mem.trim(u8, buf.written(), " \n\r\t");
                if (!started) {
                    try writer.writeByte('\n');
                    started = true;
                }
                try renderSecondaryRegion(writer, palette, highlight_color, dr.start_line, sc, dr.end_line, ec, dr.line_text, text, gpa);
                buf.clearRetainingCapacity();
            },
            .text,
            .annotated,
            .line_break,
            .indent,
            .space,
            .horizontal_rule,
            .annotation_start,
            .annotation_end,
            .raw,
            .reflowing_text,
            .link,
            .vertical_stack,
            .horizontal_concat,
            .source_code_multi_region,
            .source_location,
            => try renderElementAs(.color_terminal, elements[idx], &buf.writer, &ctx),
        }
    }
    try flushBelowText(writer, &buf, width, &started);
    try writer.writeByte('\n');
}

/// Flush accumulated prose (`buf`) as word-wrapped text.
fn flushBelowText(
    writer: *std.Io.Writer,
    buf: *std.Io.Writer.Allocating,
    width: usize,
    started: *bool,
) error{WriteFailed}!void {
    const trimmed = std.mem.trim(u8, buf.written(), "\n");
    if (trimmed.len == 0) {
        buf.clearRetainingCapacity();
        return;
    }
    if (!started.*) {
        try writer.writeByte('\n');
        started.* = true;
    }
    var it = std.mem.splitScalar(u8, trimmed, '\n');
    while (it.next()) |ln| try wrapAndEmitBelowLine(writer, ln, 0, width);
    buf.clearRetainingCapacity();
}

/// Render a secondary source region after its structured introductory prose.
fn renderSecondaryRegion(
    writer: *std.Io.Writer,
    palette: ColorPalette,
    highlight_color: []const u8,
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    line_text: []const u8,
    preceding_text: []const u8,
    gpa: Allocator,
) error{WriteFailed}!void {
    const body = std.mem.trim(u8, preceding_text, " \n\r\t");

    if (body.len > 0) {
        try writer.writeAll(body);
        try writer.writeByte('\n');
    }

    if (line_text.len > 0) {
        try writer.writeByte('\n');

        var snippet = std.array_list.Managed([]u8).init(gpa);
        defer {
            for (snippet.items) |l| gpa.free(l);
            snippet.deinit();
        }
        var leads = std.array_list.Managed(usize).init(gpa);
        defer leads.deinit();
        {
            var it = std.mem.splitScalar(u8, line_text, '\n');
            while (it.next()) |line| {
                var i: usize = 0;
                var lead: usize = 0;
                while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {
                    lead += if (line[i] == '\t') 4 else 1;
                }
                const rest = line[i..];
                const buf = gpa.alloc(u8, lead + rest.len) catch return error.WriteFailed;
                @memset(buf[0..lead], ' ');
                @memcpy(buf[lead..], rest);
                snippet.append(buf) catch return error.WriteFailed;
                leads.append(lead) catch return error.WriteFailed;
            }
        }
        var common: usize = std.math.maxInt(usize);
        for (snippet.items, leads.items) |l, lead| {
            if (lead < l.len) common = @min(common, lead);
        }
        if (common == std.math.maxInt(usize)) common = 0;

        var start_col_adj = start_column;
        var end_col_adj = end_column;
        {
            var orig_lead: usize = 0;
            while (orig_lead < line_text.len and
                (line_text[orig_lead] == ' ' or line_text[orig_lead] == '\t')) : (orig_lead += 1)
            {}
            const exp0: i64 = if (leads.items.len > 0) @intCast(leads.items[0]) else 0;
            const delta: i64 = exp0 - @as(i64, @intCast(common)) - @as(i64, @intCast(orig_lead));
            start_col_adj = @intCast(@max(@as(i64, @intCast(start_column)) + delta, 1));
            end_col_adj = @intCast(@max(@as(i64, @intCast(end_column)) + delta, 1));
        }

        {
            var line_no = start_line;
            for (snippet.items) |full_line| {
                const code_line = full_line[@min(common, full_line.len)..];
                const is_underline_line = start_line == end_line and line_no == start_line;

                for (code_line) |ch| try writer.writeByte(if (ch == '\t') ' ' else ch);
                try writer.writeByte('\n');

                if (is_underline_line) {
                    const start_byte = @min(@as(usize, start_col_adj -| 1), code_line.len);
                    const end_byte = @max(@min(@as(usize, end_col_adj -| 1), code_line.len), start_byte);

                    const lead = source_region.displayWidth(code_line[0..start_byte]);
                    try source_region.printSpaces(writer, @intCast(lead));

                    const span_width = if (end_byte > start_byte) source_region.displayWidth(code_line[start_byte..end_byte]) else 0;
                    const ulen = if (span_width > 0) span_width else 1;

                    try writer.writeAll(highlight_color);
                    try writer.splatBytesAll("^", ulen);
                    try writer.writeAll(palette.reset);
                    try writer.writeByte('\n');
                }
                line_no += 1;
            }
        }
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

/// Emit a single trailing line indented by `base_indent`, word-wrapping it
/// to `width` display columns. Wrapped continuation lines line up under the
/// first line's text (preserving any leading indent the line already had, e.g.
/// for a code block). ANSI escapes pass through and don't count toward width.
fn wrapAndEmitBelowLine(
    writer: *std.Io.Writer,
    line: []const u8,
    base_indent: usize,
    width: usize,
) error{WriteFailed}!void {
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
        // Body was empty (prefix only)—still emit it.
        try writer.splatByteAll(' ', base_indent);
        try writer.writeAll(prefix);
        try writer.writeByte('\n');
    }
}

/// Fallback for reports with no source region: title line then the body.
fn renderReportPlainFallback(report: *const Report, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    const icon_info = getSeverityIcon(report.severity, report.title, palette);
    try renderHeaderLine(writer, palette, config, icon_info, report.title, null, 0, 0);
    if (report.headline.elementCount() > 0) {
        try writer.writeByte('\n');
        var hbuf = std.Io.Writer.Allocating.init(report.document.allocator);
        defer hbuf.deinit();
        try renderDocumentToTerminal(&report.headline, &hbuf.writer, palette, config);
        const width: usize = config.getMaxLineWidth();
        var it = std.mem.splitScalar(u8, hbuf.written(), '\n');
        while (it.next()) |ln| {
            try wrapAndEmitBelowLine(writer, ln, 0, width);
        }
    }
    if (!report.document.isEmpty()) {
        try writer.writeByte('\n');
        try renderDocumentToTerminal(&report.document, writer, palette, config);
        try writer.writeByte('\n');
    }
    try writer.writeByte('\n');
}

/// Render a report to HTML.
pub fn renderReportToHtml(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    assertValidHeadline(report);
    const title_class = switch (report.severity) {
        .fatal => "error",
        .runtime_error => "error",
        .warning => "warning",
    };

    try writer.print("<div class=\"report {s}\">\n", .{title_class});
    try writer.writeAll("<h1 class=\"report-title\">");
    try writeLowercased(writer, report.title);
    try writer.writeAll("</h1>\n");
    try writer.writeAll("<div class=\"report-content\">\n");
    if (report.headline.elementCount() > 0) {
        try renderDocumentToHtml(&report.headline, writer, config);
        try writer.writeAll("<br>\n");
    }
    try renderDocumentToHtml(&report.document, writer, config);
    try writer.writeAll("</div>\n</div>\n");
}

/// Render a report for language server protocol.
pub fn renderReportToLsp(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    assertValidHeadline(report);
    // LSP typically wants plain text without formatting
    try writeLowercased(writer, report.title);
    try writer.writeAll("\n\n");
    if (report.headline.elementCount() > 0) {
        try renderDocumentToLsp(&report.headline, writer, config);
        try writer.writeByte('\n');
    }
    try renderDocumentToLsp(&report.document, writer, config);
}

/// Render a document to the specified target format.
pub fn renderDocument(document: *const Document, writer: *std.Io.Writer, target: RenderTarget) (Allocator.Error || error{WriteFailed})!void {
    const config = configFor(target);
    switch (target) {
        .color_terminal => try renderDocumentToTerminal(document, writer, ColorUtils.getPaletteForConfig(config), config),
        .markdown => try renderDocumentToMarkdown(document, writer, config),
        .html => try renderDocumentToHtml(document, writer, config),
        .language_server => try renderDocumentToLsp(document, writer, config),
    }
}

/// Render a document to terminal with color support.
pub fn renderDocumentToTerminal(document: *const Document, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try renderDocumentAs(.color_terminal, document, writer, palette, config);
}

/// Render a document to plain markdown.
pub fn renderDocumentToMarkdown(document: *const Document, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try renderDocumentAs(.markdown, document, writer, ColorPalette.NO_COLOR, config);
}

/// Render a document to HTML.
pub fn renderDocumentToHtml(document: *const Document, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try renderDocumentAs(.html, document, writer, ColorPalette.NO_COLOR, config);
}

/// Render a document for language server protocol.
pub fn renderDocumentToLsp(document: *const Document, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try renderDocumentAs(.language_server, document, writer, ColorPalette.NO_COLOR, config);
}

/// Render every element of a document through the single walker.
fn renderDocumentAs(comptime target: RenderTarget, document: *const Document, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    var ann_sfa = std.heap.stackFallback(16 * @sizeOf(Annotation), document.allocator);
    var annotation_stack = std.array_list.Managed(Annotation).init(ann_sfa.get());
    defer annotation_stack.deinit();
    var ctx = RenderCtx{ .config = config, .palette = palette, .annotation_stack = &annotation_stack };

    for (document.elements.items) |element| {
        try renderElementAs(target, element, writer, &ctx);
    }
}

// One walker, four styles.
//
// Documents are rendered by a single traversal of `DocumentElement`
// (`renderElementAs`), generic over the render target. Everything that
// distinguishes one target from another lives in that target's style struct
// (`styleFor`): per-annotation span tables, escaping, structural literals
// (indent/space/line-break units), and hooks for the pieces whose shape is
// genuinely target-specific (source-region framing). The hooks compute their
// caret rows and padding through the shared region math in `source_region.zig`
// and `writeCaretRowForLine`, so layout can never diverge between targets.

/// Rendering state threaded through the walker: the configuration, the color
/// palette (only the terminal target reads it; other targets pass NO_COLOR),
/// and the stack backing `annotation_start`/`annotation_end` regions.
const RenderCtx = struct {
    config: ReportingConfig,
    palette: ColorPalette,
    annotation_stack: *std.array_list.Managed(Annotation),
};

/// Inline markup written immediately before and after a span of annotated
/// content.
const Span = struct {
    open: []const u8 = "",
    close: []const u8 = "",
};

/// Markdown markup for each annotation. Compile-enforced to stay exhaustive:
/// adding an `Annotation` variant fails here until it gets an entry.
const markdown_spans = std.enums.EnumArray(Annotation, Span).init(.{
    .emphasized = .{ .open = "**", .close = "**" },
    .keyword = .{ .open = "`", .close = "`" },
    .type_variable = .{ .open = "_", .close = "_" },
    .error_highlight = .{ .open = "**", .close = "**" },
    .warning_highlight = .{ .open = "**● ", .close = "**" },
    .suggestion = .{ .open = "**", .close = "**" },
    .code_block = .{ .open = "```roc\n", .close = "\n```" },
    .inline_code = .{ .open = "`", .close = "`" },
    .symbol = .{ .open = "`", .close = "`" },
    .path = .{ .open = "`", .close = "`" },
    .literal = .{ .open = "`", .close = "`" },
    .comment = .{ .open = "_", .close = "_" },
    .underline = .{ .open = "__", .close = "__" },
    .dimmed = .{ .open = "`", .close = "`" },
    .symbol_qualified = .{ .open = "`", .close = "`" },
    .symbol_unqualified = .{ .open = "`", .close = "`" },
    .module_name = .{ .open = "**", .close = "**" },
    .record_field = .{ .open = "`", .close = "`" },
    .tag_name = .{ .open = "`", .close = "`" },
    .binary_operator = .{ .open = "`", .close = "`" },
    .source_region = .{},
    .reflowing_text = .{},
});

/// The HTML element each annotation renders as; its class is the annotation's
/// `semanticName`. Compile-enforced to stay exhaustive, like `markdown_spans`.
const html_tags = std.enums.EnumArray(Annotation, []const u8).init(.{
    .emphasized = "strong",
    .keyword = "span",
    .type_variable = "span",
    .error_highlight = "span",
    .warning_highlight = "span",
    .suggestion = "span",
    .code_block = "pre",
    .inline_code = "code",
    .symbol = "span",
    .path = "span",
    .literal = "span",
    .comment = "span",
    .underline = "span",
    .dimmed = "span",
    .symbol_qualified = "span",
    .symbol_unqualified = "span",
    .module_name = "span",
    .record_field = "span",
    .tag_name = "span",
    .binary_operator = "span",
    .source_region = "span",
    .reflowing_text = "span",
});

/// HTML open/close markup per annotation, derived from `html_tags` and
/// `Annotation.semanticName` at compile time.
const html_spans: std.enums.EnumArray(Annotation, Span) = blk: {
    var spans = std.enums.EnumArray(Annotation, Span).initUndefined();
    for (std.enums.values(Annotation)) |annotation| {
        const tag = html_tags.get(annotation);
        spans.set(annotation, .{
            .open = "<" ++ tag ++ " class=\"" ++ annotation.semanticName() ++ "\">",
            .close = "</" ++ tag ++ ">",
        });
    }
    break :blk spans;
};

/// The style struct for a render target.
fn styleFor(comptime target: RenderTarget) type {
    return switch (target) {
        .color_terminal => TerminalStyle,
        .markdown => MarkdownStyle,
        .html => HtmlStyle,
        .language_server => LspStyle,
    };
}

/// Render a single document element for `target`. This is the one traversal
/// of `DocumentElement`: every target renders through this switch, and the
/// target-specific pieces live in the style structs as span tables and hooks,
/// never as another switch over the model.
fn renderElementAs(comptime target: RenderTarget, element: DocumentElement, writer: *std.Io.Writer, ctx: *RenderCtx) (Allocator.Error || error{WriteFailed})!void {
    const S = styleFor(target);
    switch (element) {
        .text => |text| try S.writeText(ctx, writer, text),
        .annotated => |annotated| {
            try S.openInline(ctx, writer, annotated.annotation);
            try S.writeText(ctx, writer, annotated.content);
            try S.closeInline(ctx, writer, annotated.annotation);
        },
        .line_break => try writer.writeAll(S.line_break),
        .link => |url| try S.writeLink(writer, url),
        .indent => |levels| {
            var i: u32 = 0;
            while (i < levels) : (i += 1) {
                try writer.writeAll(S.indent_unit);
            }
        },
        .space => |count| try writer.splatBytesAll(S.space_unit, count),
        .horizontal_rule => |width| try S.writeRule(writer, width, ctx.config),
        .annotation_start => |annotation| {
            try ctx.annotation_stack.append(annotation);
            try S.openRegion(ctx, writer, annotation);
        },
        .annotation_end => {
            if (ctx.annotation_stack.pop()) |popped| {
                try S.closeRegion(ctx, writer, popped);
            }
        },
        .raw => |content| try writer.writeAll(content),
        .reflowing_text => |text| try S.writeText(ctx, writer, text),
        .vertical_stack => |elements| {
            try writer.writeAll(S.stack_open);
            for (elements, 0..) |elem, i| {
                if (i > 0) try writer.writeAll("\n");
                try renderElementAs(target, elem, writer, ctx);
            }
            try writer.writeAll(S.stack_close);
        },
        .horizontal_concat => |elements| {
            try writer.writeAll(S.concat_open);
            for (elements) |elem| {
                try renderElementAs(target, elem, writer, ctx);
            }
            try writer.writeAll(S.concat_close);
        },
        .source_code_region => |region| try S.writeSourceRegion(ctx, writer, region),
        .source_code_with_underlines => |data| try S.writeSourceUnderlines(ctx, writer, data),
        .source_code_multi_region => |multi| try S.writeMultiRegion(ctx, writer, multi),
        .source_location => |location| try S.writeSourceLocation(ctx, writer, location),
    }
}

/// Whether any underline region draws a caret row under line `line_num`.
fn lineHasCaretRow(regions: []const UnderlineRegion, line_num: u32) bool {
    for (regions) |underline| {
        if (source_region.underlineAppliesToLine(underline.start_line, underline.end_line, line_num)) {
            return true;
        }
    }
    return false;
}

/// Write one caret row: the `^^^` line drawn under a source line. The padding
/// before the first span mirrors the source line's own whitespace (preserving
/// tabs so the carets stay aligned), gaps between spans are spaces, and each
/// span becomes a run of carets—colored by its annotation on the terminal
/// target, bare elsewhere. This is the single implementation of underline
/// layout; every target that draws carets draws them through here.
fn writeCaretRowForLine(
    comptime target: RenderTarget,
    ctx: *RenderCtx,
    writer: *std.Io.Writer,
    line: []const u8,
    regions: []const UnderlineRegion,
    line_num: u32,
) error{WriteFailed}!void {
    var col_position: u32 = 1;
    for (regions) |underline| {
        if (!source_region.underlineAppliesToLine(underline.start_line, underline.end_line, line_num)) {
            continue;
        }
        try source_region.printUnderlineGap(writer, line, col_position, underline.start_column);
        if (target == .color_terminal) {
            try writer.writeAll(ctx.palette.colorForAnnotation(underline.annotation));
        }
        try writer.splatBytesAll("^", source_region.calculateUnderlineLength(underline.start_column, underline.end_column));
        if (target == .color_terminal) {
            try writer.writeAll(ctx.palette.reset);
        }
        col_position = underline.end_column;
    }
    try writer.writeByte('\n');
}

/// The one-element underline set equivalent to a plain source region's own
/// span, so single-region and multi-underline displays share the caret-row
/// path.
fn regionUnderline(region: SourceCodeDisplayRegion) [1]UnderlineRegion {
    return .{.{
        .start_line = region.start_line,
        .start_column = region.start_column,
        .end_line = region.end_line,
        .end_column = region.end_column,
        .annotation = region.region_annotation,
    }};
}

/// Write `<url>` for the targets that render links as plain angle-bracketed
/// text (every target but HTML).
fn writePlainLink(writer: *std.Io.Writer, url: []const u8) error{WriteFailed}!void {
    try writer.writeAll("<");
    try writer.writeAll(url);
    try writer.writeAll(">");
}

/// Write a horizontal rule as `glyph` repeated `width` (or the configured
/// maximum line width) times.
fn writeGlyphRule(writer: *std.Io.Writer, glyph: []const u8, width: ?u32, config: ReportingConfig) error{WriteFailed}!void {
    try writer.splatBytesAll(glyph, width orelse config.getMaxLineWidth());
}

/// Write the terminal gutter for a source line: the line number and `│`.
fn writeGutter(writer: *std.Io.Writer, palette: ColorPalette, line_num: u32, line_num_width: u32) error{WriteFailed}!void {
    try writer.writeAll(palette.secondary);
    try source_region.formatLineNumber(writer, line_num, line_num_width);
    try writer.writeAll(" │ ");
    try writer.writeAll(palette.reset);
}

/// Write the terminal gutter for a caret row: blank where the line number
/// would be, then `│`.
fn writeBlankGutter(writer: *std.Io.Writer, palette: ColorPalette, line_num_width: u32) error{WriteFailed}!void {
    try writer.writeAll(palette.secondary);
    try source_region.printSpaces(writer, line_num_width);
    try writer.writeAll(" │ ");
    try writer.writeAll(palette.reset);
}

/// Style description for the color terminal target: ANSI colors from the
/// palette, backticked code spans when the palette has no color, and source
/// regions drawn with line-number gutters.
const TerminalStyle = struct {
    const line_break = "\n";
    const indent_unit = "    ";
    const space_unit = " ";
    const stack_open = "";
    const stack_close = "";
    const concat_open = "";
    const concat_close = "";

    fn writeText(_: *RenderCtx, writer: *std.Io.Writer, text: []const u8) error{WriteFailed}!void {
        try writer.writeAll(text);
    }

    fn openInline(ctx: *RenderCtx, writer: *std.Io.Writer, annotation: Annotation) error{WriteFailed}!void {
        try writer.writeAll(ctx.palette.colorForAnnotation(annotation));
        if (wantsBacktick(ctx.palette, annotation)) try writer.writeByte('`');
    }

    fn closeInline(ctx: *RenderCtx, writer: *std.Io.Writer, annotation: Annotation) error{WriteFailed}!void {
        if (wantsBacktick(ctx.palette, annotation)) try writer.writeByte('`');
        if (ctx.palette.colorForAnnotation(annotation).len > 0) {
            try writer.writeAll(ctx.palette.reset);
            if (ctx.annotation_stack.items.len > 0) {
                const prev = ctx.annotation_stack.items[ctx.annotation_stack.items.len - 1];
                try writer.writeAll(ctx.palette.colorForAnnotation(prev));
            }
        }
    }

    fn openRegion(ctx: *RenderCtx, writer: *std.Io.Writer, annotation: Annotation) error{WriteFailed}!void {
        try writer.writeAll(ctx.palette.colorForAnnotation(annotation));
    }

    fn closeRegion(ctx: *RenderCtx, writer: *std.Io.Writer, popped: Annotation) error{WriteFailed}!void {
        if (ctx.palette.colorForAnnotation(popped).len > 0) {
            try writer.writeAll(ctx.palette.reset);
            // Re-apply the enclosing annotation region's color, if any.
            if (ctx.annotation_stack.items.len > 0) {
                const prev = ctx.annotation_stack.items[ctx.annotation_stack.items.len - 1];
                try writer.writeAll(ctx.palette.colorForAnnotation(prev));
            }
        }
    }

    const writeLink = writePlainLink;

    fn writeRule(writer: *std.Io.Writer, width: ?u32, config: ReportingConfig) error{WriteFailed}!void {
        try writeGlyphRule(writer, "─", width, config);
    }

    fn writeSourceRegion(ctx: *RenderCtx, writer: *std.Io.Writer, region: SourceCodeDisplayRegion) error{WriteFailed}!void {
        const palette = ctx.palette;
        const line_num_width = source_region.calculateLineNumberWidth(region.end_line);

        const underline = regionUnderline(region);
        const color = palette.colorForAnnotation(region.region_annotation);
        var line_num = region.start_line;
        var iter = std.mem.splitScalar(u8, region.line_text, '\n');
        while (iter.next()) |line| {
            try writeGutter(writer, palette, line_num, line_num_width);
            try writer.writeAll(color);
            try writer.writeAll(line);
            try writer.writeAll(palette.reset);
            try writer.writeByte('\n');

            if (lineHasCaretRow(&underline, line_num)) {
                try writeBlankGutter(writer, palette, line_num_width);
                try writeCaretRowForLine(.color_terminal, ctx, writer, line, &underline, line_num);
            }
            line_num += 1;
        }
    }

    fn writeSourceUnderlines(ctx: *RenderCtx, writer: *std.Io.Writer, data: SourceCodeWithUnderlines) error{WriteFailed}!void {
        const palette = ctx.palette;
        const display = data.display_region;
        const line_num_width = source_region.calculateLineNumberWidth(display.end_line);

        var line_num = display.start_line;
        var iter = std.mem.splitScalar(u8, display.line_text, '\n');
        while (iter.next()) |line| {
            try writeGutter(writer, palette, line_num, line_num_width);
            try writer.writeAll(line);
            try writer.writeByte('\n');

            if (lineHasCaretRow(data.underline_regions, line_num)) {
                try writeBlankGutter(writer, palette, line_num_width);
                try writeCaretRowForLine(.color_terminal, ctx, writer, line, data.underline_regions, line_num);
            }
            line_num += 1;
        }
    }

    fn writeMultiRegion(ctx: *RenderCtx, writer: *std.Io.Writer, multi: SourceCodeMultiRegion) error{WriteFailed}!void {
        const palette = ctx.palette;
        try writer.writeAll(multi.source);
        try writer.writeByte('\n');
        for (multi.regions) |region| {
            try writer.writeAll(palette.colorForAnnotation(region.annotation));
            try writer.print("  {}:{}-{}:{}\n", .{ region.start_line, region.start_column, region.end_line, region.end_column });
            try writer.writeAll(palette.reset);
        }
    }

    fn writeSourceLocation(ctx: *RenderCtx, writer: *std.Io.Writer, location: SourceLocation) error{WriteFailed}!void {
        const path = sanitisePathForSnapshots(location.filename orelse "<source>");
        try writeLocation(writer, ctx.palette, path, location.line, location.column);
        try writer.writeAll(ctx.palette.reset);
    }
};

/// Style description for the markdown target: static markers per annotation,
/// no colors, and source regions as fenced code blocks followed by caret rows.
const MarkdownStyle = struct {
    const line_break = "\n";
    const indent_unit = "    ";
    const space_unit = " ";
    const stack_open = "";
    const stack_close = "";
    const concat_open = "";
    const concat_close = "";

    fn writeText(_: *RenderCtx, writer: *std.Io.Writer, text: []const u8) error{WriteFailed}!void {
        try writer.writeAll(text);
    }

    fn openInline(_: *RenderCtx, writer: *std.Io.Writer, annotation: Annotation) error{WriteFailed}!void {
        try writer.writeAll(markdown_spans.get(annotation).open);
    }

    fn closeInline(_: *RenderCtx, writer: *std.Io.Writer, annotation: Annotation) error{WriteFailed}!void {
        try writer.writeAll(markdown_spans.get(annotation).close);
    }

    fn openRegion(_: *RenderCtx, _: *std.Io.Writer, _: Annotation) error{WriteFailed}!void {}

    fn closeRegion(_: *RenderCtx, _: *std.Io.Writer, _: Annotation) error{WriteFailed}!void {}

    const writeLink = writePlainLink;

    fn writeRule(writer: *std.Io.Writer, _: ?u32, _: ReportingConfig) error{WriteFailed}!void {
        try writer.writeAll("\n---\n");
    }

    fn writeSourceRegion(ctx: *RenderCtx, writer: *std.Io.Writer, region: SourceCodeDisplayRegion) error{WriteFailed}!void {
        try writer.writeAll("```roc\n");
        try writer.writeAll(region.line_text);
        try writer.writeAll("\n```\n");

        const underline = regionUnderline(region);
        var line_num = region.start_line;
        var iter = std.mem.splitScalar(u8, region.line_text, '\n');
        while (iter.next()) |line| {
            if (lineHasCaretRow(&underline, line_num)) {
                try writeCaretRowForLine(.markdown, ctx, writer, line, &underline, line_num);
            }
            line_num += 1;
        }
    }

    fn writeSourceUnderlines(ctx: *RenderCtx, writer: *std.Io.Writer, data: SourceCodeWithUnderlines) error{WriteFailed}!void {
        const display = data.display_region;
        try writer.writeAll("```roc\n");
        try writer.writeAll(display.line_text);
        try writer.writeAll("\n```\n");

        var line_num = display.start_line;
        var iter = std.mem.splitScalar(u8, display.line_text, '\n');
        while (iter.next()) |line| {
            if (lineHasCaretRow(data.underline_regions, line_num)) {
                try writeCaretRowForLine(.markdown, ctx, writer, line, data.underline_regions, line_num);
            }
            line_num += 1;
        }
    }

    fn writeMultiRegion(_: *RenderCtx, writer: *std.Io.Writer, multi: SourceCodeMultiRegion) error{WriteFailed}!void {
        try writer.writeAll("```roc\n");
        try writer.writeAll(multi.source);
        try writer.writeAll("\n```\n");
        for (multi.regions) |region| {
            try writer.print("- Line {d}:{d}-{d}:{d}\n", .{ region.start_line, region.start_column, region.end_line, region.end_column });
        }
    }

    fn writeSourceLocation(_: *RenderCtx, writer: *std.Io.Writer, location: SourceLocation) error{WriteFailed}!void {
        const path = sanitisePathForSnapshots(location.filename orelse "<source>");
        try writer.print("{s}:{d}:{d}", .{ path, location.line, location.column });
    }
};

/// Style description for the HTML target: escaped text, a tag-plus-class per
/// annotation (from `html_spans`), and source regions as `<pre>` blocks.
const HtmlStyle = struct {
    const line_break = "<br>\n";
    const indent_unit = "&nbsp;&nbsp;&nbsp;&nbsp;";
    const space_unit = "&nbsp;";
    const stack_open = "<div class=\"vertical-stack\">\n";
    const stack_close = "</div>\n";
    const concat_open = "<span class=\"horizontal-concat\">";
    const concat_close = "</span>";

    fn writeText(_: *RenderCtx, writer: *std.Io.Writer, text: []const u8) error{WriteFailed}!void {
        try writeEscapedHtml(writer, text);
    }

    fn openInline(_: *RenderCtx, writer: *std.Io.Writer, annotation: Annotation) error{WriteFailed}!void {
        try writer.writeAll(html_spans.get(annotation).open);
    }

    fn closeInline(_: *RenderCtx, writer: *std.Io.Writer, annotation: Annotation) error{WriteFailed}!void {
        try writer.writeAll(html_spans.get(annotation).close);
    }

    fn openRegion(_: *RenderCtx, writer: *std.Io.Writer, annotation: Annotation) error{WriteFailed}!void {
        try writer.writeAll(html_spans.get(annotation).open);
    }

    fn closeRegion(_: *RenderCtx, writer: *std.Io.Writer, popped: Annotation) error{WriteFailed}!void {
        try writer.writeAll(html_spans.get(popped).close);
    }

    fn writeLink(writer: *std.Io.Writer, url: []const u8) error{WriteFailed}!void {
        try writer.writeAll("&lt;<a href=\"");
        try writeEscapedHtml(writer, url);
        try writer.writeAll("\">");
        try writeEscapedHtml(writer, url);
        try writer.writeAll("</a>&gt;");
    }

    fn writeRule(writer: *std.Io.Writer, width: ?u32, config: ReportingConfig) error{WriteFailed}!void {
        try writer.print("<hr style=\"width: {d}ch;\">\n", .{width orelse config.getMaxLineWidth()});
    }

    fn writeSourceRegion(_: *RenderCtx, writer: *std.Io.Writer, region: SourceCodeDisplayRegion) error{WriteFailed}!void {
        try writer.writeAll("<div class=\"source-region\">");
        try writer.print("<pre class=\"{s}\">", .{region.region_annotation.semanticName()});
        try writeEscapedHtml(writer, region.line_text);
        try writer.writeAll("</pre></div>");
    }

    fn writeSourceUnderlines(_: *RenderCtx, writer: *std.Io.Writer, data: SourceCodeWithUnderlines) error{WriteFailed}!void {
        const display = data.display_region;
        try writer.writeAll("<div class=\"source-region\">");
        try writer.writeAll("<pre class=\"source-code\">");
        try writeEscapedHtml(writer, display.line_text);
        try writer.writeAll("</pre></div>");
    }

    fn writeMultiRegion(_: *RenderCtx, writer: *std.Io.Writer, multi: SourceCodeMultiRegion) error{WriteFailed}!void {
        try writer.writeAll("<div class=\"source-multi-region\">");
        try writer.writeAll("<pre>");
        try writeEscapedHtml(writer, multi.source);
        try writer.writeAll("</pre>\n<ul class=\"regions\">");
        for (multi.regions) |region| {
            try writer.print("<li class=\"{s}\">{d}:{d}-{d}:{d}</li>", .{ region.annotation.semanticName(), region.start_line, region.start_column, region.end_line, region.end_column });
        }
        try writer.writeAll("</ul></div>");
    }

    fn writeSourceLocation(_: *RenderCtx, writer: *std.Io.Writer, location: SourceLocation) error{WriteFailed}!void {
        const path = sanitisePathForSnapshots(location.filename orelse "<source>");
        try writer.writeAll("<span class=\"source-location\"><span class=\"filename\">");
        try writeEscapedHtml(writer, path);
        try writer.print("</span>:{d}:{d}</span>", .{ location.line, location.column });
    }
};

/// Style description for the language server target: plain text with all
/// markup stripped and locations rendered inline.
const LspStyle = struct {
    const line_break = "\n";
    const indent_unit = "  ";
    const space_unit = " ";
    const stack_open = "";
    const stack_close = "";
    const concat_open = "";
    const concat_close = "";

    fn writeText(_: *RenderCtx, writer: *std.Io.Writer, text: []const u8) error{WriteFailed}!void {
        try writer.writeAll(text);
    }

    fn openInline(_: *RenderCtx, _: *std.Io.Writer, _: Annotation) error{WriteFailed}!void {}

    fn closeInline(_: *RenderCtx, _: *std.Io.Writer, _: Annotation) error{WriteFailed}!void {}

    fn openRegion(_: *RenderCtx, _: *std.Io.Writer, _: Annotation) error{WriteFailed}!void {}

    fn closeRegion(_: *RenderCtx, _: *std.Io.Writer, _: Annotation) error{WriteFailed}!void {}

    const writeLink = writePlainLink;

    fn writeRule(writer: *std.Io.Writer, width: ?u32, config: ReportingConfig) error{WriteFailed}!void {
        try writeGlyphRule(writer, "-", width, config);
    }

    fn writeSourceRegion(_: *RenderCtx, writer: *std.Io.Writer, region: SourceCodeDisplayRegion) error{WriteFailed}!void {
        try writer.writeAll(region.line_text);
        try writer.writeByte('\n');
    }

    fn writeSourceUnderlines(_: *RenderCtx, writer: *std.Io.Writer, data: SourceCodeWithUnderlines) error{WriteFailed}!void {
        const display = data.display_region;
        try writer.writeAll(display.line_text);
        try writer.writeByte('\n');
    }

    fn writeMultiRegion(_: *RenderCtx, writer: *std.Io.Writer, multi: SourceCodeMultiRegion) error{WriteFailed}!void {
        try writer.writeAll(multi.source);
        try writer.writeByte('\n');
        for (multi.regions) |region| {
            try writer.print("  {}:{}-{}:{}\n", .{ region.start_line, region.start_column, region.end_line, region.end_column });
        }
    }

    fn writeSourceLocation(_: *RenderCtx, writer: *std.Io.Writer, location: SourceLocation) error{WriteFailed}!void {
        const path = sanitisePathForSnapshots(location.filename orelse "<source>");
        try writer.print("{s}:{d}:{d}", .{ path, location.line, location.column });
    }
};

/// Number of decimal digits needed to print `n` (e.g. 1 for 7, 4 for 1242).
fn decimalWidth(n: u32) usize {
    if (n == 0) return 1;
    return std.math.log10(n) + 1;
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

// Tests
const testing = std.testing;

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
