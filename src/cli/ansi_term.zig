//! ANSI terminal escape sequence utilities for cursor control and screen manipulation.
const std = @import("std");
const Allocator = std.mem.Allocator;
const control_code = std.ascii.control_code;

const CSI = "\x1B[";

/// ANSI escape sequence to set foreground color to green.
pub const green = "\x1B[32m";
/// ANSI escape sequence to set foreground color to red.
pub const red = "\x1B[31m";
/// ANSI escape sequence to set foreground color to yellow.
pub const yellow = "\x1B[33m";
/// ANSI escape sequence to reset all text attributes.
pub const reset = "\x1B[0m";

/// ANSI escape sequence to enable bracketed paste mode.
/// While enabled, the terminal wraps pasted text with PASTE_START and PASTE_END
/// so applications can distinguish typed input from pasted input.
pub const BRACKETED_PASTE_ENABLE = "\x1B[?2004h";
/// ANSI escape sequence to disable bracketed paste mode.
pub const BRACKETED_PASTE_DISABLE = "\x1B[?2004l";
/// Marker the terminal sends before pasted content when bracketed paste is enabled.
pub const PASTE_START = "\x1B[200~";
/// Marker the terminal sends after pasted content when bracketed paste is enabled.
pub const PASTE_END = "\x1B[201~";

/// ASCII DEL character, used as backspace in terminals.
pub const BACKSPACE: u8 = 127;
/// ANSI escape sequence identifier for the up arrow key.
pub const UP = 0x41;
/// ANSI escape sequence identifier for the down arrow key.
pub const DOWN = 0x42;
/// ANSI escape sequence identifier for the right arrow key.
pub const RIGHT = 0x43;
/// ANSI escape sequence identifier for the left arrow key.
pub const LEFT = 0x44;

/// Converts a character to its Ctrl+key equivalent.
pub inline fn ctrlKey(k: u8) u8 {
    return k & 0x1f;
}

/// Moves the cursor to the specified column on the current line.
pub fn setCursorColumn(out: *std.Io.Writer, column: usize) error{WriteFailed}!void {
    try out.print(CSI ++ "{}G", .{column + 1});
}

/// Moves the cursor to the specified x and y position.
pub fn setCursor(out: *std.Io.Writer, x: usize, y: usize) error{WriteFailed}!void {
    try out.print(CSI ++ "{};{}H", .{ y + 1, x + 1 });
}

/// Erases all characters from the cursor to the end of the line.
pub fn clearFromCursorToLineEnd(out: *std.Io.Writer) error{WriteFailed}!void {
    try out.writeAll(CSI ++ "K");
}

/// Clears the entire terminal screen.
pub fn clearEntireScreen(out: *std.Io.Writer) error{WriteFailed}!void {
    try out.writeAll(CSI ++ "2J");
}

/// Queries the terminal for the current cursor position.
pub fn queryCursorPosition(out: *std.Io.Writer) Allocator.Error!void {
    try out.writeAll(CSI ++ "6n");
}

/// Computes the display width of a UTF-8 string by counting codepoints, skipping
/// ANSI CSI escape sequences (e.g. color codes) since they occupy no columns.
pub fn computeDisplayWidth(prompt: []const u8) (Allocator.Error || error{InvalidUtf8})!usize {
    var utf8 = try std.unicode.Utf8View.init(prompt);
    var it = utf8.iterator();
    var width: usize = 0;
    while (it.nextCodepointSlice()) |slice| {
        if (slice.len == 1 and slice[0] == control_code.esc) {
            // Skip a CSI sequence: ESC '[' params... final-byte (0x40-0x7E).
            if (it.nextCodepointSlice()) |bracket| {
                if (bracket.len == 1 and bracket[0] == '[') {
                    while (it.nextCodepointSlice()) |seq| {
                        if (seq.len == 1 and seq[0] >= 0x40 and seq[0] <= 0x7E) break;
                    }
                    continue;
                }
                // Not a CSI sequence; count the consumed codepoint normally.
                width += 1;
                continue;
            }
            continue;
        }
        width += 1;
    }
    return width;
}

const testing = std.testing;

test "computeDisplayWidth: plain ascii" {
    try testing.expectEqual(@as(usize, 2), try computeDisplayWidth("» "));
}

test "computeDisplayWidth: skips ANSI color codes" {
    // The cyan REPL prompt: ESC[1;36m » ESC[0m space — visible width is 2.
    try testing.expectEqual(@as(usize, 2), try computeDisplayWidth("\x1b[1;36m»\x1b[0m "));
    try testing.expectEqual(@as(usize, 2), try computeDisplayWidth("\x1b[1;36m…\x1b[0m "));
}

test "computeDisplayWidth: lone ESC not followed by bracket counts following codepoint" {
    try testing.expectEqual(@as(usize, 1), try computeDisplayWidth("\x1bx"));
}
