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

/// In debug builds, enforce that a report's headline reads as a complete
/// sentence: its last non-whitespace character must be a period. The headline
/// is the one-sentence summary on the box's top edge (or under the title in the
/// markdown/HTML/LSP layouts). Compiled out of release builds. Reports that have
/// not yet been migrated to a headline (none of the elements carry text) are
/// exempt.
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
    // reports without a headline) — those are exempt.
    std.debug.assert(last == 0 or last == '.');
}

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

/// Render a report as a plain-text box (the terminal box layout without ANSI
/// color). Used for snapshot PROBLEMS sections and non-TTY user output.
pub fn renderReportToBoxPlain(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    try renderReportBoxed(report, writer, ColorPalette.NO_COLOR, config);
}

// Boxed report rendering.
//
// Lays a report out as a box drawn around the offending source snippet, with an
// ALL-CAPS label box in the upper-left poking one column out past the main box:
//
//     ┌───────────────┐
//     │ TYPE MISMATCH ├─ <one-line summary> ───────────── ... ─────┐
//     └┬──────────────┘                                            │
//      │                                                           │
//      │  <source code>                                            │
//      │         ‾‾‾‾‾                                              │
//      └──────────────────────────────── path/to/file.roc:6:8 ─────┘
//
//        <detailed explanation, indented under the box>
//
// The summary rides the top edge (wrapping under its own start when long, and
// growing the box for 3+ lines). The location is tucked into the bottom-right
// corner, right-aligned; if it would overrun the wall it drops to its own line
// beneath instead. There is always a blank row above the snippet, and a
// blank-equivalent row above the bottom edge (a single-line region's underline
// counts as blank). The same layout is used for the colored terminal output and
// the plain markdown/snapshot output — the only difference is the palette (ANSI
// vs NO_COLOR).

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

/// Append the plain text of a run of elements (line breaks become spaces), and
/// record the terminal color of each appended byte so the box's top edge can
/// keep inline code, symbols, etc. colored. `colors` ends up the same length as
/// `plain`. In a no-color palette every color is "" and this is just plain text.
/// Whether an annotation marks a code span (identifier, keyword, operator,
/// type, …) — the spans the markdown renderer wraps in backticks. The box
/// renderers wrap these in backticks too, but only when there's no color to
/// distinguish them (see `wantsBacktick`).
fn isCodeAnnotation(a: Annotation) bool {
    return switch (a) {
        .keyword, .inline_code, .symbol, .symbol_qualified, .symbol_unqualified, .record_field, .tag_name, .binary_operator => true,
        else => false,
    };
}

/// In a no-color palette, code spans get backticks (since color can't set them
/// apart); with color, the color does the distinguishing and backticks would be
/// redundant noise. A palette is "no color" when its reset sequence is empty.
fn wantsBacktick(palette: ColorPalette, a: Annotation) bool {
    return palette.reset.len == 0 and isCodeAnnotation(a);
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
                try colors.append(palette.secondary);
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
                try colors.append(palette.secondary);
            },
            .space => |n| {
                var i: u32 = 0;
                while (i < n) : (i += 1) {
                    try plain.append(' ');
                    try colors.append(palette.secondary);
                }
            },
            else => {},
        }
    }
}

/// Emit `line` (a slice of `plain`) with its per-byte `colors`, then reset to
/// `sec`. The ANSI color codes don't count toward display width, so the caller's
/// column accounting (computed from the plain text) stays correct.
fn writeColoredSummary(
    writer: *std.Io.Writer,
    line: []const u8,
    plain: []const u8,
    colors: []const []const u8,
    sec: []const u8,
) error{WriteFailed}!void {
    if (line.len == 0) return;
    const base = @intFromPtr(line.ptr) - @intFromPtr(plain.ptr);
    var cur: usize = 0;
    for (line, 0..) |b, k| {
        const c = colors[base + k];
        const cp = @intFromPtr(c.ptr);
        if (cp != cur) {
            try writer.writeAll(c);
            cur = cp;
        }
        try writer.writeByte(b);
    }
    try writer.writeAll(sec);
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

/// Write `s` with every ASCII letter uppercased. Titles are authored in title
/// case (so the markdown renderer can preserve it), and shouted in ALL CAPS in
/// the box/HTML/LSP/plain renderers. Titles are validated as ASCII, so a byte
/// uppercase is sufficient.
fn writeShouted(writer: *std.Io.Writer, s: []const u8) error{WriteFailed}!void {
    for (s) |c| {
        try writer.writeByte(if (c >= 'a' and c <= 'z') c - ('a' - 'A') else c);
    }
}

/// Pad with spaces and write the right wall `│` at column `rw`.
fn closeRow(writer: *std.Io.Writer, palette: ColorPalette, col: usize, rw: usize) error{WriteFailed}!void {
    try writer.splatByteAll(' ', (rw -| 1) -| col);
    try writer.writeAll(palette.secondary);
    try writer.writeAll("│");
    try writer.writeAll(palette.reset);
    try writer.writeByte('\n');
}

/// The byte offset in `line` at display column `target_col`, clamped to a UTF-8
/// boundary and never overshooting `target_col`. Tabs count as one column.
fn byteAtDisplayCol(line: []const u8, target_col: usize) usize {
    var i: usize = 0;
    var col: usize = 0;
    while (i < line.len and col < target_col) {
        const seq = std.unicode.utf8ByteSequenceLength(line[i]) catch 1;
        const next = @min(i + seq, line.len);
        const w = source_region.displayWidth(line[i..next]);
        if (col + w > target_col) break;
        col += w;
        i = next;
    }
    return i;
}

/// A windowed view of a source line, used when the line is too wide for the box.
const CodeWindow = struct {
    start_byte: usize,
    end_byte: usize,
    left_ellipsis: bool,
    right_ellipsis: bool,
};

/// Choose a slice of `line` to show within `avail` display columns that keeps
/// the byte offset `focus` (the start of the underlined span) visible, with a
/// little left context, eliding the rest with `…`.
fn windowSourceLine(line: []const u8, focus: usize, avail: usize) CodeWindow {
    if (source_region.displayWidth(line) <= avail) {
        return .{ .start_byte = 0, .end_byte = line.len, .left_ellipsis = false, .right_ellipsis = false };
    }
    const budget = avail -| 2; // leave room for an ellipsis on each side
    const focus_col = source_region.displayWidth(line[0..focus]);
    const left_context = @min(@as(usize, 8), budget / 4);
    const start_byte = byteAtDisplayCol(line, focus_col -| left_context);
    const start_col = source_region.displayWidth(line[0..start_byte]);
    const end_byte = byteAtDisplayCol(line, start_col + budget);
    return .{
        .start_byte = start_byte,
        .end_byte = end_byte,
        .left_ellipsis = start_byte > 0,
        .right_ellipsis = end_byte < line.len,
    };
}

/// Render the report as a box around its source snippet.
pub fn renderReportBoxed(report: *const Report, writer: *std.Io.Writer, palette: ColorPalette, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    assertValidHeadline(report);
    // Each report is framed by a blank line above and below, so consecutive
    // reports are separated by two blank lines, and the first/last report keeps
    // exactly one blank line at the top/bottom of the run.
    try writer.writeByte('\n');
    const gpa = report.document.allocator;
    const elements = report.document.elements.items;

    const region = findBoxedRegion(elements) orelse {
        try renderReportPlainFallback(report, writer, palette, config);
        try writer.writeByte('\n');
        return;
    };

    // The headline rides the box's top edge. When the report supplies one
    // explicitly, the whole document goes below the box. Otherwise (reports
    // not yet migrated to a required headline) fall back to deriving it from
    // the lead text up to the first line break before the region.
    var summary_buf = std.array_list.Managed(u8).init(gpa);
    defer summary_buf.deinit();
    var color_buf = std.array_list.Managed([]const u8).init(gpa);
    defer color_buf.deinit();
    var below_start: usize = 0;
    if (report.headline.elementCount() > 0) {
        // The rich headline rides the top edge; keep its inline styling.
        try collectStyledText(report.headline.elements.items, palette, &summary_buf, &color_buf);
    } else {
        // Fallback: derive the summary from the lead text up to the first line
        // break before the region (reports with an empty headline).
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

    // The label box sits flush left and sticks out one column to the left of the
    // indented main box; the title's name rides its middle row, with the summary
    // flowing to the right.
    const inner_len = title.len + 2; // " TITLE "
    const tbw = inner_len + 2; // label box width
    // Grow the box past the configured width if the title is so wide that the
    // label box plus a minimal summary slot wouldn't otherwise fit — so the
    // walls stay aligned on narrow terminals (the whole box just wraps in the
    // terminal) instead of overrunning.
    const total: usize = @max(config.getMaxLineWidth(), tbw + 15);
    const rw = total -| 1; // main box right wall column
    // Every summary line starts at this column (just past "│ TITLE ├─ "), so the
    // headline's left edge is consistent however many lines it wraps to.
    const sum_indent = tbw + 2;
    const avail: usize = @max((rw -| sum_indent) -| 3, 8);

    var lines = std.array_list.Managed([]const u8).init(gpa);
    defer lines.deinit();
    try wrapSummary(summary, avail, &lines);

    const sec = palette.secondary;
    const rst = palette.reset;

    // Row 1: label box top, flush left.
    try writer.writeAll(sec);
    try writer.writeAll("┌");
    try writer.splatBytesAll("─", inner_len);
    try writer.writeAll("┐");
    try writer.writeAll(rst);
    try writer.writeByte('\n');

    // Row 2: "│ TITLE ├─ <summary[0]> ─…─┐".
    {
        var col: usize = 0;
        try writer.writeAll(sec);
        try writer.writeAll("│ ");
        col += 2;
        try writer.writeAll(palette.bold);
        try writer.writeAll(palette.primary);
        try writeShouted(writer, title);
        try writer.writeAll(rst);
        col += title.len;
        try writer.writeAll(sec);
        try writer.writeAll(" ├─ ");
        col += 4;
        const line0: []const u8 = if (lines.items.len > 0) lines.items[0] else "";
        try writeColoredSummary(writer, line0, summary_buf.items, color_buf.items, sec);
        col += source_region.displayWidth(line0);
        try writer.writeAll(" ");
        col += 1;
        try writer.splatBytesAll("─", (rw -| 1) -| col);
        try writer.writeAll("┐");
        try writer.writeAll(rst);
        try writer.writeByte('\n');
    }

    // Row 3: "└┬─…─┘" — the ┬ becomes the indented main left wall — then summary
    // line 1 (if any), aligned under summary line 0.
    {
        var col: usize = 0;
        try writer.writeAll(sec);
        try writer.writeAll("└┬");
        col += 2;
        try writer.splatBytesAll("─", tbw -| 3);
        col += tbw -| 3;
        try writer.writeAll("┘");
        col += 1;
        if (lines.items.len > 1) {
            try padTo(writer, col, sum_indent);
            col = @max(col, sum_indent);
            try writer.writeAll(sec);
            try writeColoredSummary(writer, lines.items[1], summary_buf.items, color_buf.items, sec);
            col += source_region.displayWidth(lines.items[1]);
        }
        try closeRow(writer, palette, col, rw);
    }

    // Summary lines 2+ each get their own row: the indented left wall, then the
    // summary aligned under line 0.
    if (lines.items.len > 2) {
        for (lines.items[2..]) |ln| {
            try writer.writeByte(' ');
            try writer.writeAll(sec);
            try writer.writeAll("│");
            try writer.writeAll(rst);
            try padTo(writer, 2, sum_indent);
            try writer.writeAll(sec);
            try writeColoredSummary(writer, ln, summary_buf.items, color_buf.items, sec);
            try closeRow(writer, palette, sum_indent + source_region.displayWidth(ln), rw);
        }
    }

    // Blank separator row, then the source line(s) and underline. Each body row
    // is indented one column (the label box sticks out left of it). Lines that
    // would overrun the right wall are windowed around the underlined span.
    const code_avail = rw -| 5;
    {
        try writer.writeByte(' ');
        try writer.writeAll(sec);
        try writer.writeAll("│");
        try writer.writeAll(rst);
        try closeRow(writer, palette, 2, rw);
    }
    // Normalize snippet indentation: expand leading tabs to 4 spaces, then strip
    // the common leading-space prefix shared by every non-blank line. This keeps
    // relative indentation intact while left-aligning the whole snippet, so the
    // box supplies the only indentation the reader sees.
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
    // The common indent is the smallest expanded leading-space count across the
    // non-blank lines (a line is blank when nothing follows its whitespace).
    var common: usize = std.math.maxInt(usize);
    for (snippet.items, leads.items) |l, lead| {
        if (lead < l.len) common = @min(common, lead);
    }
    if (common == std.math.maxInt(usize)) common = 0;

    // Shift the underline columns (1-based byte offsets into the first line) to
    // track tab expansion and the stripped common indent.
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
            // Underline byte span (meaningful only on the underlined line); also
            // the window's focus so the span stays visible when the line is long.
            const start_byte = @min(@as(usize, start_col_adj -| 1), code_line.len);
            const end_byte = @max(@min(@as(usize, end_col_adj -| 1), code_line.len), start_byte);
            const win = windowSourceLine(code_line, if (is_underline_line) start_byte else 0, code_avail);
            const shown = code_line[win.start_byte..win.end_byte];

            var col: usize = 0;
            try writer.writeByte(' ');
            col += 1;
            try writer.writeAll(sec);
            try writer.writeAll("│");
            try writer.writeAll(rst);
            col += 1;
            try writer.writeAll("  ");
            col += 2;
            if (win.left_ellipsis) {
                try writer.writeAll("…");
                col += 1;
            }
            // Render tabs as single spaces: a literal tab would otherwise throw
            // off both the right wall and the underline below it.
            for (shown) |ch| try writer.writeByte(if (ch == '\t') ' ' else ch);
            col += source_region.displayWidth(shown);
            if (win.right_ellipsis) {
                try writer.writeAll("…");
                col += 1;
            }
            try closeRow(writer, palette, col, rw);

            if (is_underline_line) {
                var ucol: usize = 0;
                try writer.writeByte(' ');
                ucol += 1;
                try writer.writeAll(sec);
                try writer.writeAll("│");
                try writer.writeAll(rst);
                ucol += 1;
                try writer.writeAll("  ");
                ucol += 2;
                // The underline span clipped to the visible window, mapped from
                // byte offsets to display columns so it lines up under
                // wide/multi-byte chars.
                const vs = @max(start_byte, win.start_byte);
                const ve = @min(end_byte, win.end_byte);
                const lead = @as(usize, if (win.left_ellipsis) 1 else 0) +
                    source_region.displayWidth(code_line[win.start_byte..vs]);
                try writer.splatByteAll(' ', lead);
                ucol += lead;
                const span_width = if (ve > vs) source_region.displayWidth(code_line[vs..ve]) else 0;
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

    // Keep a blank-equivalent row directly above the bottom edge. A single-line
    // region's underline already reads as blank, so only multi-line snippets
    // (which have no underline) need an explicit blank row added here.
    if (region.start_line != region.end_line) {
        try writer.writeByte(' ');
        try writer.writeAll(sec);
        try writer.writeAll("│");
        try writer.writeAll(rst);
        try closeRow(writer, palette, 2, rw);
    }

    // Bottom edge with the location tucked into the bottom-right corner
    // (`└─…─ file:line:col ┘`). If the location is too long to fit inside the
    // box without overrunning a wall, fall back to a plain rule with the
    // location on its own line beneath.
    {
        const fname = if (region.filename) |f| sanitisePathForSnapshots(f) else "<source>";
        const loc = try std.fmt.allocPrint(gpa, "{s}:{}:{}", .{ fname, region.start_line, region.start_column });
        defer gpa.free(loc);
        const loc_w = source_region.displayWidth(loc);

        try writer.writeByte(' ');
        try writer.writeAll(sec);
        try writer.writeAll("└");
        if (loc_w + 5 <= rw) {
            // Fits: └ + dashes + " loc " + ┘, with the location right-aligned.
            try writer.splatBytesAll("─", (rw -| 5) -| loc_w);
            try writer.writeAll(" ");
            try writer.writeAll(loc);
            try writer.writeAll(" ┘");
            try writer.writeAll(rst);
            try writer.writeByte('\n');
        } else {
            // Too long: plain rule, location on its own line beneath.
            try writer.splatBytesAll("─", rw -| 3);
            try writer.writeAll("┘");
            try writer.writeAll(rst);
            try writer.writeByte('\n');
            try writer.writeAll("    ");
            try writer.writeAll(sec);
            try writer.writeAll(loc);
            try writer.writeAll(rst);
            try writer.writeByte('\n');
        }
    }

    // Detailed explanation below the box, indented 4 spaces.
    try renderBelowContent(writer, palette, config, elements, below_start, region.index, rw, gpa);
    try writer.writeByte('\n');
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
    rw: usize,
    gpa: Allocator,
) (Allocator.Error || error{WriteFailed})!void {
    const width: usize = config.getMaxLineWidth();
    var buf = std.Io.Writer.Allocating.init(gpa);
    defer buf.deinit();
    var ann = std.array_list.Managed(Annotation).init(gpa);
    defer ann.deinit();

    // Walk the below-the-box elements (everything except the main box's region).
    // Prose accumulates in `buf` and is flushed — word-wrapped and indented —
    // whenever we reach a source region, which renders as its own embedded box.
    // `started` tracks whether the one leading blank line (which separates the
    // below-content from the box) has been emitted yet.
    var started = false;
    var idx = @min(below_start, region_idx);
    while (idx < elements.len) : (idx += 1) {
        if (idx == region_idx) continue;
        switch (elements[idx]) {
            .source_code_region => |r| {
                try flushBelowText(writer, &buf, width, &started);
                if (!started) {
                    try writer.writeByte('\n');
                    started = true;
                }
                try renderEmbeddedBox(writer, palette, rw, r.region_annotation, r.filename, r.start_line, r.start_column, r.end_line, r.end_column, r.line_text, gpa);
            },
            .source_code_with_underlines => |d| {
                try flushBelowText(writer, &buf, width, &started);
                if (!started) {
                    try writer.writeByte('\n');
                    started = true;
                }
                const dr = d.display_region;
                var sc = dr.start_column;
                var ec = dr.end_column;
                if (d.underline_regions.len > 0) {
                    sc = d.underline_regions[0].start_column;
                    ec = d.underline_regions[0].end_column;
                }
                try renderEmbeddedBox(writer, palette, rw, dr.region_annotation, dr.filename, dr.start_line, sc, dr.end_line, ec, dr.line_text, gpa);
            },
            else => try renderElementToTerminal(elements[idx], &buf.writer, palette, &ann, config),
        }
    }
    try flushBelowText(writer, &buf, width, &started);
    if (!started) try writer.writeByte('\n');
}

/// Flush accumulated below-the-box prose (`buf`) as word-wrapped, indented text,
/// emitting the one-time leading blank line first. Clears `buf`.
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
    while (it.next()) |ln| try wrapAndEmitBelowLine(writer, ln, 4, width);
    buf.clearRetainingCapacity();
}

/// Render a secondary source region (e.g. the original definition pointed at by
/// "…was already defined here:") as a box mirroring the main report box — full
/// border with the location tucked into the bottom-right corner — but with the
/// line number rendered in a gutter to the LEFT of the box, outside it.
fn renderEmbeddedBox(
    writer: *std.Io.Writer,
    palette: ColorPalette,
    rw: usize,
    annotation: Annotation,
    filename: ?[]const u8,
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
    line_text: []const u8,
    gpa: Allocator,
) (Allocator.Error || error{WriteFailed})!void {
    const sec = palette.secondary;
    const rst = palette.reset;
    const indent: usize = 4;
    const lnw: usize = source_region.calculateLineNumberWidth(end_line);
    const wall = indent + lnw + 2; // 1-based column of the box's left wall
    const code_avail = rw -| wall -| 3;

    // Top edge (the gutter to its left is blank).
    try writer.splatByteAll(' ', wall -| 1);
    try writer.writeAll(sec);
    try writer.writeAll("┌");
    try writer.splatBytesAll("─", (rw -| wall) -| 1);
    try writer.writeAll("┐");
    try writer.writeAll(rst);
    try writer.writeByte('\n');

    var line_no = start_line;
    var it = std.mem.splitScalar(u8, line_text, '\n');
    while (it.next()) |code_line| {
        const is_underline_line = start_line == end_line and line_no == start_line;
        const start_byte = @min(@as(usize, start_column -| 1), code_line.len);
        const end_byte = @max(@min(@as(usize, end_column -| 1), code_line.len), start_byte);
        const win = windowSourceLine(code_line, if (is_underline_line) start_byte else 0, code_avail);
        const shown = code_line[win.start_byte..win.end_byte];

        // Code row: the line number sits in the gutter, then the boxed code.
        var col: usize = 0;
        try writer.splatByteAll(' ', indent);
        col += indent;
        try writer.writeAll(sec);
        try source_region.formatLineNumber(writer, line_no, @intCast(lnw));
        col += lnw;
        try writer.writeByte(' ');
        col += 1;
        try writer.writeAll("│");
        try writer.writeAll(rst);
        col += 1;
        try writer.writeAll("  ");
        col += 2;
        if (win.left_ellipsis) {
            try writer.writeAll("…");
            col += 1;
        }
        for (shown) |ch| try writer.writeByte(if (ch == '\t') ' ' else ch);
        col += source_region.displayWidth(shown);
        if (win.right_ellipsis) {
            try writer.writeAll("…");
            col += 1;
        }
        try writer.splatByteAll(' ', (rw -| 1) -| col);
        try writer.writeAll(sec);
        try writer.writeAll("│");
        try writer.writeAll(rst);
        try writer.writeByte('\n');

        if (is_underline_line) {
            var ucol: usize = 0;
            try writer.splatByteAll(' ', wall -| 1);
            ucol += wall -| 1;
            try writer.writeAll(sec);
            try writer.writeAll("│");
            try writer.writeAll(rst);
            ucol += 1;
            try writer.writeAll("  ");
            ucol += 2;
            const vs = @max(start_byte, win.start_byte);
            const ve = @min(end_byte, win.end_byte);
            const lead = @as(usize, if (win.left_ellipsis) 1 else 0) +
                source_region.displayWidth(code_line[win.start_byte..vs]);
            try writer.splatByteAll(' ', lead);
            ucol += lead;
            const span_width = if (ve > vs) source_region.displayWidth(code_line[vs..ve]) else 0;
            const ulen = if (span_width > 0) span_width else 1;
            try writer.writeAll(getAnnotationColor(annotation, palette));
            try writer.splatBytesAll(box_underline, ulen);
            try writer.writeAll(rst);
            ucol += ulen;
            try writer.splatByteAll(' ', (rw -| 1) -| ucol);
            try writer.writeAll(sec);
            try writer.writeAll("│");
            try writer.writeAll(rst);
            try writer.writeByte('\n');
        }
        line_no += 1;
    }

    // Bottom edge with the location tucked into the bottom-right corner.
    const fname = if (filename) |f| sanitisePathForSnapshots(f) else "<source>";
    const loc = try std.fmt.allocPrint(gpa, "{s}:{}:{}", .{ fname, start_line, start_column });
    defer gpa.free(loc);
    const loc_w = source_region.displayWidth(loc);
    try writer.splatByteAll(' ', wall -| 1);
    try writer.writeAll(sec);
    try writer.writeAll("└");
    if (loc_w + 4 <= rw -| wall) {
        try writer.splatBytesAll("─", ((rw -| wall) -| loc_w) -| 3);
        try writer.writeByte(' ');
        try writer.writeAll(loc);
        try writer.writeAll(" ┘");
    } else {
        try writer.splatBytesAll("─", (rw -| wall) -| 1);
        try writer.writeAll("┘");
    }
    try writer.writeAll(rst);
    try writer.writeByte('\n');
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
    try writeShouted(writer, report.title);
    try writer.writeAll(palette.reset);
    try writer.writeByte('\n');
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
    try renderDocumentToTerminal(&report.document, writer, palette, config);
    try writer.writeByte('\n');
}

/// Render a report to HTML.
pub fn renderReportToHtml(report: *const Report, writer: *std.Io.Writer, config: ReportingConfig) (Allocator.Error || error{WriteFailed})!void {
    assertValidHeadline(report);
    const title_class = switch (report.severity) {
        .info => "info",
        .fatal => "error",
        .runtime_error => "error",
        .warning => "warning",
    };

    try writer.print("<div class=\"report {s}\">\n", .{title_class});
    try writer.writeAll("<h1 class=\"report-title\">");
    try writeShouted(writer, report.title);
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
    try writeShouted(writer, report.title);
    try writer.writeAll("\n\n");
    if (report.headline.elementCount() > 0) {
        try renderDocumentToLsp(&report.headline, writer, config);
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
            const tick = wantsBacktick(palette, annotated.annotation);
            try writer.writeAll(color);
            if (tick) try writer.writeByte('`');
            try writer.writeAll(annotated.content);
            if (tick) try writer.writeByte('`');
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
    var report = try Report.init(testing.allocator, "Test Error", "Something went wrong.", .runtime_error);
    defer report.deinit();

    try report.document.addText("This is a test error message.");

    var writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer writer.deinit();

    try renderReportToMarkdown(&report, &writer.writer, ReportingConfig.initMarkdown());

    try testing.expect(std.mem.find(u8, writer.written(), "**Test Error**") != null);
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
