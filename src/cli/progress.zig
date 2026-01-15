//! Download progress display for the CLI.
//!
//! This module provides visual feedback during package downloads with:
//! - Animated spinner showing activity
//! - Progress bar showing completion percentage
//! - Human-readable size display (KB/MB)
//!
//! The implementation separates pure rendering logic (testable) from I/O.
//! Supports both TTY (with ANSI codes) and non-TTY (plain text) output.

const std = @import("std");
const builtin = @import("builtin");
const reporting = @import("reporting");

const AnsiCodes = reporting.AnsiCodes;
const ColorPalette = reporting.ColorPalette;

/// Check if stderr is a TTY (supports ANSI escape codes).
/// Returns false on freestanding targets or if stderr is piped/redirected.
pub fn isStderrTty() bool {
    if (comptime builtin.target.os.tag == .freestanding) {
        return false;
    }
    return std.fs.File.stderr().isTty();
}

/// Braille spinner characters for animation.
/// These characters animate smoothly when displayed in sequence.
pub const SPINNER = [_]u21{ '⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏' };

/// Pure data representing download progress state.
/// This struct contains all information needed to render the progress display.
pub const ProgressState = struct {
    package_name: []const u8,
    downloaded: usize,
    total: ?usize,
    spinner_idx: usize,

    pub fn init(package_name: []const u8) ProgressState {
        return .{
            .package_name = package_name,
            .downloaded = 0,
            .total = null,
            .spinner_idx = 0,
        };
    }

    /// Update progress after reading bytes.
    /// Advances the spinner and accumulates downloaded bytes.
    pub fn update(self: *ProgressState, bytes_read: usize) void {
        self.downloaded += bytes_read;
        self.spinner_idx = (self.spinner_idx + 1) % SPINNER.len;
    }

    /// Set the total expected size (from Content-Length header).
    pub fn setTotal(self: *ProgressState, total: usize) void {
        self.total = total;
    }
};

/// Render a progress bar with block characters.
/// `width` is the number of character positions for the bar (excluding brackets).
pub fn renderProgressBar(
    writer: anytype,
    current: usize,
    total: ?usize,
    width: usize,
) !void {
    try writer.writeAll("[");
    if (total) |t| {
        if (t > 0) {
            const filled = @min((current * width) / t, width);
            const empty = width - filled;
            for (0..filled) |_| try writer.writeAll("█");
            for (0..empty) |_| try writer.writeAll("░");
        } else {
            for (0..width) |_| try writer.writeAll("░");
        }
    } else {
        // Unknown total - show empty bar
        for (0..width) |_| try writer.writeAll("░");
    }
    try writer.writeAll("]");
}

/// Format size as human-readable string (KB or MB).
pub fn formatSize(writer: anytype, bytes: usize, total: ?usize) !void {
    const mb: f64 = @as(f64, @floatFromInt(bytes)) / 1_000_000.0;
    if (total) |t| {
        const total_mb: f64 = @as(f64, @floatFromInt(t)) / 1_000_000.0;
        if (total_mb < 1.0) {
            try writer.print("{d:.1} / {d:.1} KB", .{ mb * 1000.0, total_mb * 1000.0 });
        } else {
            try writer.print("{d:.1} / {d:.1} MB", .{ mb, total_mb });
        }
    } else {
        if (mb < 1.0) {
            try writer.print("{d:.1} KB downloaded", .{mb * 1000.0});
        } else {
            try writer.print("{d:.1} MB downloaded", .{mb});
        }
    }
}

/// Render complete progress display to writer.
/// Uses ANSI escape codes for in-place terminal updates when is_tty is true.
/// Falls back to simple text output when is_tty is false.
pub fn renderProgress(writer: anytype, state: *const ProgressState, is_tty: bool) !void {
    if (is_tty) {
        // TTY mode: use ANSI codes for in-place updates
        // Clear line, move to start, print spinner and package name
        try writer.print("{s}{s}", .{ AnsiCodes.CLEAR_LINE, AnsiCodes.CURSOR_TO_START });
        try writer.print("{u} Downloading {s}...\n  ", .{
            SPINNER[state.spinner_idx],
            state.package_name,
        });

        // Progress bar
        try renderProgressBar(writer, state.downloaded, state.total, 30);
        try writer.writeAll(" ");

        // Size info
        try formatSize(writer, state.downloaded, state.total);

        // Move cursor back up for next update
        try writer.writeAll(AnsiCodes.CURSOR_UP_1);
    } else {
        // Non-TTY mode: simple one-line output (only on first call, no updates)
        if (state.spinner_idx == 0 and state.downloaded == 0) {
            try writer.print("Downloading {s}...\n", .{state.package_name});
        }
    }
}

/// Render completion message.
/// Clears both lines that were used by renderProgress before showing the final message.
/// When is_tty is false, outputs a simple completion line.
pub fn renderComplete(writer: anytype, package_name: []const u8, is_tty: bool) !void {
    if (is_tty) {
        const palette = ColorPalette.ANSI;

        // Clear line 1 (where cursor is after renderProgress)
        try writer.print("{s}{s}", .{ AnsiCodes.CLEAR_LINE, AnsiCodes.CURSOR_TO_START });
        // Move down to line 2 and clear it too
        try writer.writeAll("\n");
        try writer.print("{s}{s}", .{ AnsiCodes.CLEAR_LINE, AnsiCodes.CURSOR_TO_START });
        // Move back up to line 1
        try writer.writeAll(AnsiCodes.CURSOR_UP_1);

        // Now write the completion message
        try writer.print("{s}✔{s} Downloaded {s}\n", .{
            palette.success,
            palette.reset,
            package_name,
        });
    } else {
        // Non-TTY mode: simple completion message
        try writer.print("Downloaded {s}\n", .{package_name});
    }
}

// ============================================================================
// Testing Infrastructure
// ============================================================================

/// Minimal virtual terminal for testing ANSI output.
/// Interprets a subset of ANSI escape codes to produce a 2D character buffer,
/// allowing us to test what users would actually see on screen.
/// Uses ArrayList-based rows to properly handle multi-byte UTF-8 characters.
pub const VirtualTerminal = struct {
    width: usize,
    height: usize,
    rows: []std.ArrayList(u8),
    cursor_row: usize,
    cursor_col: usize, // Column in characters, not bytes
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, width: usize, height: usize) !VirtualTerminal {
        const rows = try allocator.alloc(std.ArrayList(u8), height);
        for (rows) |*row| {
            row.* = std.ArrayList(u8).empty;
            // Pre-fill with spaces
            for (0..width) |_| {
                try row.append(allocator, ' ');
            }
        }
        return .{
            .width = width,
            .height = height,
            .rows = rows,
            .cursor_row = 0,
            .cursor_col = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VirtualTerminal) void {
        for (self.rows) |*row| {
            row.deinit(self.allocator);
        }
        self.allocator.free(self.rows);
    }

    /// Process a string containing text and ANSI escape codes.
    pub fn process(self: *VirtualTerminal, input: []const u8) void {
        var i: usize = 0;
        while (i < input.len) {
            if (input[i] == '\x1b' and i + 1 < input.len and input[i + 1] == '[') {
                // Parse escape sequence
                i += 2;
                var code_end = i;
                while (code_end < input.len and !std.ascii.isAlphabetic(input[code_end])) {
                    code_end += 1;
                }
                if (code_end < input.len) {
                    const code = input[i .. code_end + 1];
                    self.handleEscape(code);
                    i = code_end + 1;
                }
            } else if (input[i] == '\n') {
                self.cursor_row += 1;
                self.cursor_col = 0;
                i += 1;
            } else {
                // Regular character - could be multi-byte UTF-8
                const codepoint_len = std.unicode.utf8ByteSequenceLength(input[i]) catch 1;
                if (i + codepoint_len <= input.len) {
                    const char_bytes = input[i .. i + codepoint_len];

                    if (self.cursor_row < self.height and self.cursor_col < self.width) {
                        self.writeCharAtCursor(char_bytes);
                        self.cursor_col += 1;
                    }
                    i += codepoint_len;
                } else {
                    i += 1;
                }
            }
        }
    }

    fn writeCharAtCursor(self: *VirtualTerminal, char_bytes: []const u8) void {
        if (self.cursor_row >= self.height) return;

        const row = &self.rows[self.cursor_row];

        // Find byte position for cursor_col by counting characters
        var byte_pos: usize = 0;
        var char_count: usize = 0;
        while (byte_pos < row.items.len and char_count < self.cursor_col) {
            const len = std.unicode.utf8ByteSequenceLength(row.items[byte_pos]) catch 1;
            byte_pos += len;
            char_count += 1;
        }

        // Calculate how many bytes the current character at this position takes
        var old_char_len: usize = 1;
        if (byte_pos < row.items.len) {
            old_char_len = std.unicode.utf8ByteSequenceLength(row.items[byte_pos]) catch 1;
        }

        // Replace the character
        if (char_bytes.len == old_char_len) {
            // Same size - just overwrite
            for (char_bytes, 0..) |b, j| {
                if (byte_pos + j < row.items.len) {
                    row.items[byte_pos + j] = b;
                }
            }
        } else if (char_bytes.len < old_char_len) {
            // New char is smaller - overwrite and shift left
            for (char_bytes, 0..) |b, j| {
                row.items[byte_pos + j] = b;
            }
            const shift = old_char_len - char_bytes.len;
            const src_start = byte_pos + old_char_len;
            const dest_start = byte_pos + char_bytes.len;
            if (src_start < row.items.len) {
                std.mem.copyForwards(u8, row.items[dest_start..], row.items[src_start..]);
                row.items.len -= shift;
            }
        } else {
            // New char is larger - need to make room
            const extra = char_bytes.len - old_char_len;
            // Grow if needed
            row.ensureTotalCapacity(self.allocator, row.items.len + extra) catch return;
            // Shift right
            const src_start = byte_pos + old_char_len;
            const dest_start = byte_pos + char_bytes.len;
            if (src_start < row.items.len) {
                row.items.len += extra;
                std.mem.copyBackwards(u8, row.items[dest_start..], row.items[src_start .. row.items.len - extra]);
            } else {
                row.items.len += extra;
            }
            // Write new char
            for (char_bytes, 0..) |b, j| {
                row.items[byte_pos + j] = b;
            }
        }
    }

    fn handleEscape(self: *VirtualTerminal, code: []const u8) void {
        if (std.mem.eql(u8, code, "2K")) {
            // Clear entire line - reset to spaces
            if (self.cursor_row < self.height) {
                self.rows[self.cursor_row].clearRetainingCapacity();
                for (0..self.width) |_| {
                    self.rows[self.cursor_row].append(self.allocator, ' ') catch {};
                }
            }
        } else if (std.mem.eql(u8, code, "G")) {
            // Move cursor to column 1
            self.cursor_col = 0;
        } else if (std.mem.eql(u8, code, "1A")) {
            // Move cursor up 1 line
            if (self.cursor_row > 0) self.cursor_row -= 1;
        }
        // Ignore color codes for visual testing
    }

    /// Render buffer as string, trimming trailing spaces per line.
    pub fn toString(self: *const VirtualTerminal, allocator: std.mem.Allocator) ![]u8 {
        var result = std.ArrayList(u8).empty;
        errdefer result.deinit(allocator);

        for (self.rows, 0..) |row, i| {
            // Trim trailing spaces (ASCII only)
            var end = row.items.len;
            while (end > 0 and row.items[end - 1] == ' ') end -= 1;
            try result.appendSlice(allocator, row.items[0..end]);
            if (i < self.rows.len - 1) try result.append(allocator, '\n');
        }

        // Trim trailing empty lines
        while (result.items.len > 0 and result.items[result.items.len - 1] == '\n') {
            _ = result.pop();
        }

        return result.toOwnedSlice(allocator);
    }
};

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "progress at 0%" {
    var term = try VirtualTerminal.init(testing.allocator, 70, 3);
    defer term.deinit();

    var buf: [512]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const state = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 0,
        .total = 5_500_000,
        .spinner_idx = 0,
    };

    try renderProgress(writer, &state, true);
    term.process(stream.getWritten());

    const result = try term.toString(testing.allocator);
    defer testing.allocator.free(result);

    try testing.expectEqualStrings(
        \\⠋ Downloading roc-lang/basic-cli...
        \\  [░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░] 0.0 / 5.5 MB
    , result);
}

test "progress at 50%" {
    var term = try VirtualTerminal.init(testing.allocator, 70, 3);
    defer term.deinit();

    var buf: [512]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const state = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 2_750_000,
        .total = 5_500_000,
        .spinner_idx = 5,
    };

    try renderProgress(writer, &state, true);
    term.process(stream.getWritten());

    const result = try term.toString(testing.allocator);
    defer testing.allocator.free(result);

    try testing.expectEqualStrings(
        \\⠴ Downloading roc-lang/basic-cli...
        \\  [███████████████░░░░░░░░░░░░░░░] 2.8 / 5.5 MB
    , result);
}

test "progress at 100%" {
    var term = try VirtualTerminal.init(testing.allocator, 70, 3);
    defer term.deinit();

    var buf: [512]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const state = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 5_500_000,
        .total = 5_500_000,
        .spinner_idx = 9,
    };

    try renderProgress(writer, &state, true);
    term.process(stream.getWritten());

    const result = try term.toString(testing.allocator);
    defer testing.allocator.free(result);

    try testing.expectEqualStrings(
        \\⠏ Downloading roc-lang/basic-cli...
        \\  [██████████████████████████████] 5.5 / 5.5 MB
    , result);
}

test "progress unknown size" {
    var term = try VirtualTerminal.init(testing.allocator, 70, 3);
    defer term.deinit();

    var buf: [512]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const state = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 1_200_000,
        .total = null,
        .spinner_idx = 3,
    };

    try renderProgress(writer, &state, true);
    term.process(stream.getWritten());

    const result = try term.toString(testing.allocator);
    defer testing.allocator.free(result);

    try testing.expectEqualStrings(
        \\⠸ Downloading roc-lang/basic-cli...
        \\  [░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░] 1.2 MB downloaded
    , result);
}

test "progress small file in KB" {
    var term = try VirtualTerminal.init(testing.allocator, 70, 3);
    defer term.deinit();

    var buf: [512]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const state = ProgressState{
        .package_name = "roc-lang/json",
        .downloaded = 250_000,
        .total = 500_000,
        .spinner_idx = 2,
    };

    try renderProgress(writer, &state, true);
    term.process(stream.getWritten());

    const result = try term.toString(testing.allocator);
    defer testing.allocator.free(result);

    try testing.expectEqualStrings(
        \\⠹ Downloading roc-lang/json...
        \\  [███████████████░░░░░░░░░░░░░░░] 250.0 / 500.0 KB
    , result);
}

test "animation overwrites correctly" {
    var term = try VirtualTerminal.init(testing.allocator, 70, 3);
    defer term.deinit();

    var buf: [512]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    // Frame 1
    const state1 = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 1_000_000,
        .total = 5_000_000,
        .spinner_idx = 0,
    };
    try renderProgress(stream.writer(), &state1, true);
    term.process(stream.getWritten());

    // Frame 2 - should overwrite frame 1
    stream.reset();
    const state2 = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 2_500_000,
        .total = 5_000_000,
        .spinner_idx = 1,
    };
    try renderProgress(stream.writer(), &state2, true);
    term.process(stream.getWritten());

    const result = try term.toString(testing.allocator);
    defer testing.allocator.free(result);

    // Should show frame 2 only
    try testing.expectEqualStrings(
        \\⠙ Downloading roc-lang/basic-cli...
        \\  [███████████████░░░░░░░░░░░░░░░] 2.5 / 5.0 MB
    , result);
}

test "ProgressState update advances spinner" {
    var state = ProgressState.init("test/pkg");
    try testing.expectEqual(@as(usize, 0), state.spinner_idx);
    try testing.expectEqual(@as(usize, 0), state.downloaded);

    state.update(1000);
    try testing.expectEqual(@as(usize, 1), state.spinner_idx);
    try testing.expectEqual(@as(usize, 1000), state.downloaded);

    // Spinner should wrap around
    for (0..9) |_| state.update(100);
    try testing.expectEqual(@as(usize, 0), state.spinner_idx);
}

test "renderComplete clears both lines from progress" {
    // Simulate what happens: renderProgress leaves 2 lines, then renderComplete
    var term = try VirtualTerminal.init(testing.allocator, 70, 4);
    defer term.deinit();

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    // First, render progress (takes 2 lines)
    const state = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 1_000_000,
        .total = 5_000_000,
        .spinner_idx = 0,
    };
    try renderProgress(stream.writer(), &state, true);
    term.process(stream.getWritten());

    // Now render complete - should clear both lines and show single line
    stream.reset();
    try renderComplete(stream.writer(), "roc-lang/basic-cli", true);
    term.process(stream.getWritten());

    const result = try term.toString(testing.allocator);
    defer testing.allocator.free(result);

    // Should show only the completion message, no leftover progress bar
    // Note: The completion message includes color codes which VirtualTerminal ignores
    try testing.expectEqualStrings(
        \\✔ Downloaded roc-lang/basic-cli
    , result);
}

test "non-TTY mode: renderProgress outputs simple message" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    const state = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 0,
        .total = null,
        .spinner_idx = 0,
    };

    // Non-TTY mode should output simple text
    try renderProgress(stream.writer(), &state, false);

    try testing.expectEqualStrings(
        "Downloading roc-lang/basic-cli...\n",
        stream.getWritten(),
    );
}

test "non-TTY mode: renderProgress only outputs once" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    // First call with spinner_idx=0, downloaded=0 should output
    var state = ProgressState{
        .package_name = "roc-lang/basic-cli",
        .downloaded = 0,
        .total = null,
        .spinner_idx = 0,
    };
    try renderProgress(stream.writer(), &state, false);

    // Simulate progress update
    state.update(1000);
    try renderProgress(stream.writer(), &state, false);

    // Should still only have the first message (no spam on non-TTY)
    try testing.expectEqualStrings(
        "Downloading roc-lang/basic-cli...\n",
        stream.getWritten(),
    );
}

test "non-TTY mode: renderComplete outputs simple message" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    try renderComplete(stream.writer(), "roc-lang/basic-cli", false);

    try testing.expectEqualStrings(
        "Downloaded roc-lang/basic-cli\n",
        stream.getWritten(),
    );
}
