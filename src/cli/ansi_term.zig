//! ANSI terminal escape sequence utilities for cursor control and screen manipulation.
const std = @import("std");

const CSI = "\x1B[";

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
pub fn setCursorColumn(out: *std.Io.Writer, column: usize) !void {
    try out.print(CSI ++ "{}G", .{column + 1});
}

/// Moves the cursor to the specified x and y position.
pub fn setCursor(out: *std.Io.Writer, x: usize, y: usize) !void {
    try out.print(CSI ++ "{};{}H", .{ y + 1, x + 1 });
}

/// Erases all characters from the cursor to the end of the line.
pub fn clearFromCursorToLineEnd(out: *std.Io.Writer) !void {
    try out.writeAll(CSI ++ "K");
}

/// Clears the entire terminal screen.
pub fn clearEntireScreen(out: *std.Io.Writer) !void {
    try out.writeAll(CSI ++ "2J");
}

/// Queries the terminal for the current cursor position.
pub fn queryCursorPosition(out: *std.Io.Writer) !void {
    try out.writeAll(CSI ++ "6n");
}

/// Computes the display width of a UTF-8 string by counting codepoints.
pub fn computeDisplayWidth(prompt: []const u8) !usize {
    var utf8 = try std.unicode.Utf8View.init(prompt);
    var it = utf8.iterator();
    var width: usize = 0;
    while (it.nextCodepointSlice()) |_| {
        width += 1;
    }
    return width;
}
