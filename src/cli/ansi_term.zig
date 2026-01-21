const std = @import("std");

const CSI = "\x1B[";

// ASCII codes
pub const BACKSPACE: u8 = 127;
pub const UP = 0x41;
pub const DOWN = 0x42;
pub const RIGHT = 0x43;
pub const LEFT = 0x44;

pub inline fn ctrlKey(k: u8) u8 {
    return k & 0x1f;
}

pub fn setCursorColumn(out: *std.Io.Writer, column: usize) !void {
    try out.print(CSI ++ "{}G", .{column + 1});
}

pub fn setCursor(out: *std.Io.Writer, x: usize, y: usize) !void {
    try out.print(CSI ++ "{};{}H", .{ y + 1, x + 1 });
}

pub fn clearFromCursorToLineEnd(out: *std.Io.Writer) !void {
    try out.writeAll(CSI ++ "K");
}

pub fn clearEntireScreen(out: *std.Io.Writer) !void {
    try out.writeAll(CSI ++ "2J");
}

pub fn queryCursorPosition(out: *std.Io.Writer) !void {
    try out.writeAll(CSI ++ "6n");
}

pub fn computeDisplayWidth(prompt: []const u8) !usize {
    var utf8 = try std.unicode.Utf8View.init(prompt);
    var it = utf8.iterator();
    var width: usize = 0;
    while (it.nextCodepointSlice()) |codepoint| {
        _ = codepoint;
        width += 1;
    }
    return width;
}
