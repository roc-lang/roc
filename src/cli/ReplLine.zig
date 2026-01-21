const std = @import("std");
const control_code = std.ascii.control_code;
const Allocator = std.mem.Allocator;

const ansi_term = @import("ansi_term.zig");
const Unix = @import("Unix.zig");

const ReplLine = @This();

allocator: Allocator,

pub fn init(allocator: Allocator) ReplLine {
    return ReplLine{
        .allocator = allocator,
    };
}

const CommandError =
    error{ DeleteEmptyLineBuffer, NewLine, ExitRepl } ||
    Allocator.Error ||
    std.fs.File.ReadError ||
    std.Io.Writer.Error;

const CommandFn = *const fn (*LineState) CommandError!void;

const LineState = struct {
    outlive: Allocator,
    temp: Allocator,
    prompt: []const u8,
    prompt_width: usize,
    out: *std.Io.Writer,
    in: std.fs.File,
    col_offset: usize,
    line_buffer: std.ArrayList(u8),
    bytes_read: usize,
    in_buffer: [8]u8,
};

fn printChar(state: *LineState) !void {
    // Insert at col_offset, not just append
    try state.line_buffer.insert(state.temp, state.col_offset, state.in_buffer[0]);
    state.col_offset += 1;

    // Redraw the line after the prompt
    try ansi_term.setCursorColumn(state.out, state.prompt_width);
    try state.out.writeAll(state.line_buffer.items);
    try ansi_term.clearFromCursorToLineEnd(state.out);

    // Move cursor to correct position
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn deleteBefore(state: *LineState) !void {
    if (state.col_offset == 0) return;
    state.col_offset -= 1;
    _ = state.line_buffer.orderedRemove(state.col_offset);
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
    try state.out.writeAll(state.line_buffer.items[state.col_offset..]);
    try state.out.writeByte(' ');
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn doNothing(_: *LineState) !void {}

fn exitRepl(_: *LineState) !void {
    return error.ExitRepl;
}

fn acceptLine(_: *LineState) !void {
    return error.NewLine;
}

fn debugInBuffer(state: *LineState) !void {
    _ = try state.out.print("in_buffer = {any}\n", .{state.in_buffer[0..state.bytes_read]});
}

fn clearScreen(state: *LineState) !void {
    try ansi_term.clearEntireScreen(state.out);
    try ansi_term.setCursor(state.out, 0, 0);

    try state.out.writeAll(state.prompt);
    try state.out.writeAll(state.line_buffer.items);
    try ansi_term.clearFromCursorToLineEnd(state.out);

    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn moveCursorRight(state: *LineState) !void {
    state.col_offset = @min(state.col_offset + 1, state.line_buffer.items.len);
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn moveCursorLeft(state: *LineState) !void {
    state.col_offset -|= 1;
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn findCommandFn(state: *LineState) CommandFn {
    const key = state.in_buffer[0];
    return switch (key) {
        ' '...'~' => printChar,
        ansi_term.BACKSPACE => deleteBefore,
        ansi_term.ctrlKey('D') => exitRepl,
        ansi_term.ctrlKey('L') => clearScreen,
        control_code.lf => acceptLine,
        control_code.esc => {
            if (state.bytes_read >= 3 and state.in_buffer[1] == '[') {
                return switch (state.in_buffer[2]) {
                    ansi_term.LEFT => moveCursorLeft,
                    ansi_term.RIGHT => moveCursorRight,
                    // ansi_term.UP => historyBackward,
                    // ansi_term.DOWN => historyForward,
                    else => debugInBuffer,
                };
            } else {
                state.out.print("got control code\n", .{}) catch unreachable;
                return debugInBuffer;
            }
        },
        else => {
            state.out.print("got byte: {}\n", .{key}) catch unreachable;
            return debugInBuffer;
        },
    };
}

pub fn readLine(self: *ReplLine, outlive: Allocator, prompt: []const u8) ![]u8 {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writerStreaming(&stdout_buffer);

    const stdin_file = std.fs.File.stdin();
    return helper(self, outlive, prompt, &stdout_writer.interface, stdin_file);
}

fn helper(self: *ReplLine, outlive: Allocator, prompt: []const u8, out: *std.Io.Writer, in: std.fs.File) ![]u8 {
    _ = self;
    var arena_allocator = std.heap.ArenaAllocator.init(outlive);
    defer arena_allocator.deinit();
    const temp = arena_allocator.allocator();

    const prompt_width = try ansi_term.computeDisplayWidth(prompt);

    var state = ReplLine.LineState{
        .prompt = prompt,
        .prompt_width = prompt_width,
        .out = out,
        .in = in,
        .col_offset = 0,
        .line_buffer = std.ArrayList(u8).empty,
        .temp = temp,
        .outlive = outlive,
        .bytes_read = undefined,
        .in_buffer = undefined,
    };


    const old = try Unix.init();
    defer old.deinit();

    try out.writeAll(prompt);
    try out.flush();

    while (true) : ({
        try out.flush();
    }) {
        state.bytes_read = try in.read(&state.in_buffer);

        const cmd = ReplLine.findCommandFn(&state);
        cmd(&state) catch |err| {
            switch (err) {
                error.ExitRepl => return try outlive.dupe(u8, "exit"),
                error.NewLine => break,
                else => |readline_error| return readline_error,
            }
        };
    }
    try out.writeAll("\n");
    try out.flush();
    return try outlive.dupe(u8, state.line_buffer.items);
}
