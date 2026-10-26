//! Measures how the attached terminal renders sequences whose width Unicode
//! data cannot settle: a ZWJ-connected emoji sequence and a base followed by
//! U+FE0F.
//!
//! The measurement is a cursor-position report. The probe writes `X` and a
//! sample, then asks the terminal where the cursor ended up (`CSI 6n`) and reads
//! the `ESC [ row ; col R` reply. A renderer that collapses the sample into one
//! glyph reports two columns past `X`; one that lays the code points out
//! individually reports more. The sample is then erased.
//!
//! This is the only portable source for that fact, which is why the probe asks
//! the terminal instead of reading its name. `TERM`, `TERM_PROGRAM` and
//! `VTE_VERSION` name a terminfo entry or carry an inherited environment: this
//! very process inherits `VTE_VERSION` while owning no terminal at all. `TERM`
//! also cannot separate GNOME Terminal (which lays `👩‍👩‍👦‍👦` out in eight cells)
//! from WezTerm (which draws it as one glyph), since both report
//! `xterm-256color`; tmux and ssh rewrite it again. Terminfo has no
//! character-width capabilities at all. Selecting a width model from any of
//! those would be a guess about the renderer, and guessing wrong means drawing
//! over text that is already on screen.
//!
//! The probe runs once per REPL session, after the terminal is in raw mode and
//! before the first prompt. Every byte the terminal sends that is not part of a
//! cursor-position report is handed back to the caller so the input parser sees
//! keystrokes the user typed while the probe was running.

const std = @import("std");
const builtin = @import("builtin");
const unicode = @import("unicode.zig");
const Unix = @import("Unix.zig");
const Windows = @import("Windows.zig");

/// How long to wait for the terminal's reply to one cursor-position report
/// before concluding that it does not answer.
pub const report_timeout_ms: u32 = 150;

const Sample = struct {
    bytes: []const u8,
    /// Cell count the sample needs when every code point is laid out on its
    /// own. Guards against a reply that cannot be a real measurement.
    separate_width: usize,
};

const samples = [_]Sample{
    .{ .bytes = "👩‍👩‍👦‍👦", .separate_width = 8 },
    .{ .bytes = "\u{263A}\u{FE0F}", .separate_width = 1 },
};

/// Terminal columns each sample occupies when it is drawn as one glyph.
const merged_glyph_width: usize = 2;

/// Whether stdin is a terminal that can answer a cursor-position report. When
/// it is not, there is nothing to ask and the caller keeps the default model.
pub fn interactive(in: std.Io.File) bool {
    if (comptime builtin.os.tag == .windows) {
        var mode: std.os.windows.CONSOLE_MODE = undefined;
        return std.os.windows.GetConsoleMode(in.handle, &mode) != 0;
    }

    // `TIOCGWINSZ` succeeds only for a terminal; a pipe or a file has no window
    // size to report.
    var winsize: std.posix.winsize = undefined;
    const rc = std.posix.system.ioctl(in.handle, std.posix.T.IOCGWINSZ, @intFromPtr(&winsize));
    return std.posix.errno(rc) == .SUCCESS;
}

/// Queries the terminal for the width model of `samples`. Bytes read from the
/// terminal that are not cursor-position reports are appended to `unexpected`
/// for the caller to feed to its input parser.
pub fn measure(
    std_io: std.Io,
    in: std.Io.File,
    out: *std.Io.Writer,
    unexpected: *std.ArrayList(u8),
    temp: std.mem.Allocator,
) (std.Io.File.ReadStreamingError || std.Io.Writer.Error || std.mem.Allocator.Error)!unicode.WidthModel {
    var model = unicode.WidthModel{};

    for (samples, 0..) |sample, sample_index| {
        const width = try measureSample(std_io, in, out, temp, sample, unexpected) orelse break;
        const merged = width == merged_glyph_width;
        switch (sample_index) {
            0 => model.zwj_sequence_is_one_glyph = merged,
            1 => model.variation_selector_is_wide = merged,
            else => unreachable, // `samples` has two entries
        }
    }

    // Clear whatever the samples left on the line the prompt will occupy.
    try out.writeAll("\r" ++ erase_line);
    try out.flush();

    return model;
}

const erase_line = "\x1b[K";

/// Returns the cell width of `sample` as reported by the terminal, or null when
/// the terminal did not answer.
fn measureSample(
    std_io: std.Io,
    in: std.Io.File,
    out: *std.Io.Writer,
    temp: std.mem.Allocator,
    sample: Sample,
    unexpected: *std.ArrayList(u8),
) (std.Io.File.ReadStreamingError || std.Io.Writer.Error || std.mem.Allocator.Error)!?usize {
    try out.writeAll("\r" ++ erase_line ++ "X");
    try out.writeAll(sample.bytes);
    try out.writeAll("\x1b[6n");
    try out.flush();

    const report = try readCursorReport(std_io, in, temp, unexpected) orelse return null;

    // `X` occupies the first column, so the sample ends one column before the
    // reported cursor position, and the report is one-based.
    if (report.column < 2) return null;
    const width = report.column - 2;
    if (width > sample.separate_width) return null;
    return width;
}

const Report = struct {
    row: u32,
    column: u32,
};

/// Reads terminal output until a cursor-position report arrives, appending
/// everything else to `unexpected`. Returns null when the terminal stays silent.
fn readCursorReport(
    std_io: std.Io,
    in: std.Io.File,
    temp: std.mem.Allocator,
    unexpected: *std.ArrayList(u8),
) (std.Io.File.ReadStreamingError || std.mem.Allocator.Error)!?Report {
    var pending = std.ArrayList(u8).empty;
    defer pending.deinit(temp);

    var chunk: [64]u8 = undefined;
    while (true) {
        switch (scan(pending.items)) {
            .found => |found| {
                try unexpected.appendSlice(temp, pending.items[0..found.start]);
                try unexpected.appendSlice(temp, pending.items[found.end..]);
                return .{ .row = found.row, .column = found.column };
            },
            .keep_tail => |tail_len| {
                const head_len = pending.items.len - tail_len;
                try unexpected.appendSlice(temp, pending.items[0..head_len]);
                std.mem.copyForwards(u8, pending.items[0..tail_len], pending.items[head_len..]);
                pending.shrinkRetainingCapacity(tail_len);
            },
            .nothing => {
                try unexpected.appendSlice(temp, pending.items);
                pending.clearRetainingCapacity();
            },
        }

        // A poll that fails, times out, or sees stdin hang up means the
        // terminal will not answer, which is not an error for a probe.
        const input_ready = switch (comptime builtin.os.tag) {
            .windows => Windows.waitInputReady(report_timeout_ms),
            else => Unix.waitInputReady(report_timeout_ms),
        } catch return null;
        if (!input_ready) return null;

        const bytes_read = try in.readStreaming(std_io, &.{&chunk});
        if (bytes_read == 0) continue;
        try pending.appendSlice(temp, chunk[0..bytes_read]);
    }
}

const Found = struct {
    row: u32,
    column: u32,
    start: usize,
    end: usize,
};

const Scan = union(enum) {
    found: Found,
    /// The bytes after this offset could still grow into a report, so they stay
    /// in the buffer.
    keep_tail: usize,
    nothing,
};

fn scan(bytes: []const u8) Scan {
    var index: usize = 0;
    while (index < bytes.len) : (index += 1) {
        if (bytes[index] != std.ascii.control_code.esc) continue;
        if (parseReport(bytes, index)) |report| {
            return .{ .found = .{ .row = report.row, .column = report.column, .start = index, .end = report.end } };
        }
        if (reportPrefix(bytes[index..]) == bytes.len - index) return .{ .keep_tail = bytes.len - index };
    }
    return .nothing;
}

const Parsed = struct {
    row: u32,
    column: u32,
    end: usize,
};

/// Parses a cursor-position report that starts at `bytes[start]`.
pub fn parseReport(bytes: []const u8, start: usize) ?Parsed {
    if (start + 3 > bytes.len) return null;
    if (bytes[start] != std.ascii.control_code.esc or bytes[start + 1] != '[') return null;

    var index = start + 2;
    const row = parseNumber(bytes, &index) orelse return null;
    if (index >= bytes.len or bytes[index] != ';') return null;
    index += 1;
    const column = parseNumber(bytes, &index) orelse return null;
    if (index >= bytes.len or bytes[index] != 'R') return null;
    return .{ .row = row, .column = column, .end = index + 1 };
}

fn parseNumber(bytes: []const u8, index: *usize) ?u32 {
    const start = index.*;
    while (index.* < bytes.len and std.ascii.isDigit(bytes[index.*])) : (index.* += 1) {}
    if (index.* == start) return null;
    return std.fmt.parseInt(u32, bytes[start..index.*], 10) catch null;
}

/// Returns the length of the longest prefix of `bytes` that is a valid prefix
/// of a cursor-position report. Zero when `bytes` cannot start one.
pub fn reportPrefix(bytes: []const u8) usize {
    if (bytes.len == 0) return 0;
    if (bytes[0] != std.ascii.control_code.esc) return 0;
    if (bytes.len == 1) return 1;
    if (bytes[1] != '[') return 0;

    var index: usize = 2;
    if (!digitsPrefix(bytes, &index)) return 2;
    if (index == bytes.len) return index;
    if (bytes[index] != ';') return 0;
    index += 1;
    if (!digitsPrefix(bytes, &index)) return index;
    if (index == bytes.len) return index;
    if (bytes[index] != 'R') return 0;
    return index + 1;
}

/// Advances `index` past any digits at that position, returning whether there
/// was at least one.
fn digitsPrefix(bytes: []const u8, index: *usize) bool {
    const start = index.*;
    while (index.* < bytes.len and std.ascii.isDigit(bytes[index.*])) : (index.* += 1) {}
    return index.* != start;
}

const testing = std.testing;

test "cursor-position report: parses a complete report" {
    const found = parseReport("\x1b[12;40R", 0).?;
    try testing.expectEqual(@as(u32, 12), found.row);
    try testing.expectEqual(@as(u32, 40), found.column);
    try testing.expectEqual(@as(usize, 8), found.end);
}

test "cursor-position report: finds a report after unrelated output" {
    const bytes = "\x1b[?25labc\x1b[3;7Rrest";
    const found = scan(bytes);
    try testing.expectEqual(@as(usize, 9), found.found.start);
    try testing.expectEqual(@as(usize, 15), found.found.end);
    try testing.expectEqual(@as(u32, 3), found.found.row);
    try testing.expectEqual(@as(u32, 7), found.found.column);
}

test "cursor-position report: rejects malformed reports" {
    try testing.expectEqual(@as(?Parsed, null), parseReport("", 0));
    try testing.expectEqual(@as(?Parsed, null), parseReport("\x1b[12;40", 0));
    try testing.expectEqual(@as(?Parsed, null), parseReport("\x1b[;40R", 0));
    try testing.expectEqual(@as(?Parsed, null), parseReport("\x1b[12;R", 0));
    try testing.expectEqual(@as(?Parsed, null), parseReport("\x1b[12;40X", 0));
}

test "cursor-position report: prefix length tracks a split report" {
    try testing.expectEqual(@as(usize, 0), reportPrefix(""));
    try testing.expectEqual(@as(usize, 1), reportPrefix("\x1b"));
    try testing.expectEqual(@as(usize, 2), reportPrefix("\x1b["));
    try testing.expectEqual(@as(usize, 4), reportPrefix("\x1b[12"));
    try testing.expectEqual(@as(usize, 5), reportPrefix("\x1b[12;"));
    try testing.expectEqual(@as(usize, 7), reportPrefix("\x1b[12;40"));
    try testing.expectEqual(@as(usize, 8), reportPrefix("\x1b[12;40R"));
    try testing.expectEqual(@as(usize, 0), reportPrefix("\x1bX"));
    try testing.expectEqual(@as(usize, 0), reportPrefix("a"));
}

test "cursor-position report: an incomplete tail stays buffered" {
    const scan_result = scan("ab\x1b[4;");
    try testing.expectEqual(@as(usize, 4), scan_result.keep_tail);
}

test "cursor-position report: plain input is not a report" {
    try testing.expectEqual(Scan.nothing, scan("hello"));
}
