//! REPL line editor with history support and cross-platform terminal handling.
//! Modified from the anyline library: https://codeberg.org/TheShinx317/anyline
const std = @import("std");
const control_code = std.ascii.control_code;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const ansi_term = @import("ansi_term.zig");
const Unix = @import("Unix.zig");
const Windows = @import("Windows.zig");

const SupportedOS = enum { windows, linux, macos };

/// The operating system this build is targeting.
pub const SUPPORTED_OS = switch (builtin.os.tag) {
    .windows => SupportedOS.windows,
    .linux => SupportedOS.linux,
    .macos => SupportedOS.macos,
    else => |tag| @compileError(@tagName(tag) ++ " is not a support OS for ReplLine!"),
};

/// Platform-specific newline sequence.
pub const NEW_LINE = switch (SUPPORTED_OS) {
    .linux, .macos => "\n",
    .windows => "\r\n",
};

// struct to manage REPL history
const History = struct {
    allocator: Allocator,
    entries: std.ArrayList([]const u8),

    pub fn init(allocator: Allocator) History {
        const entries = std.ArrayList([]const u8).empty;
        return History{
            .allocator = allocator,
            .entries = entries,
        };
    }

    pub fn deinit(self: *History) void {
        for (self.entries.items) |line| {
            self.allocator.free(line);
        }
        self.entries.deinit(self.allocator);
    }

    pub fn append(self: *History, input: []const u8) !void {
        const input_copy = try self.allocator.alloc(u8, input.len);
        @memcpy(input_copy, input);
        try self.entries.append(self.allocator, input_copy);
    }
};

// struct to manage REPL line editing
const ReplLine = @This();

allocator: Allocator,
history: History,

pub fn init(allocator: Allocator) ReplLine {
    return ReplLine{ .allocator = allocator, .history = History.init(allocator) };
}

pub fn deinit(self: *ReplLine) void {
    self.history.deinit();
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
    history: *History,
    history_index: ?usize,
};

fn printChar(state: *LineState) !void {
    // Reset history navigation on new input
    state.history_index = null;

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

fn cancelLine(state: *LineState) !void {
    // Clear the buffer and reset cursor position
    state.line_buffer.clearAndFree(state.temp);
    state.col_offset = 0;

    // Move cursor to start of line, print prompt, clear rest of line
    try ansi_term.setCursorColumn(state.out, 0);
    try state.out.writeAll(state.prompt);
    try ansi_term.clearFromCursorToLineEnd(state.out);
    try ansi_term.setCursorColumn(state.out, state.prompt_width);
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

fn historyBackward(state: *LineState) !void {
    const hist_len = state.history.entries.items.len;
    if (hist_len == 0) return;

    if (state.history_index == null) {
        state.history_index = hist_len - 1;
    } else if (state.history_index.? > 0) {
        state.history_index = state.history_index.? - 1;
    }

    const entry = state.history.entries.items[state.history_index.?];
    state.line_buffer.clearAndFree(state.temp);
    try state.line_buffer.appendSlice(state.temp, entry);
    state.col_offset = entry.len;

    try ansi_term.setCursorColumn(state.out, state.prompt_width);
    try state.out.writeAll(state.line_buffer.items);
    try ansi_term.clearFromCursorToLineEnd(state.out); // Clear any ghost text
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn historyForward(state: *LineState) !void {
    const hist_len = state.history.entries.items.len;
    if (hist_len == 0 or state.history_index == null) return;

    if (state.history_index.? < hist_len - 1) {
        state.history_index = state.history_index.? + 1;
        const entry = state.history.entries.items[state.history_index.?];
        state.line_buffer.clearAndFree(state.temp);
        try state.line_buffer.appendSlice(state.temp, entry);
        state.col_offset = entry.len;
    } else {
        // Past the end, clear line
        state.history_index = null;
        state.line_buffer.clearAndFree(state.temp);
        state.col_offset = 0;
    }

    try ansi_term.setCursorColumn(state.out, state.prompt_width);
    try state.out.writeAll(state.line_buffer.items);
    try ansi_term.clearFromCursorToLineEnd(state.out); // Clear any ghost text
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn findCommandFn(state: *LineState) CommandFn {
    const key = state.in_buffer[0];
    return switch (key) {
        ' '...'~' => printChar,
        ansi_term.BACKSPACE => deleteBefore,
        ansi_term.ctrlKey('D') => exitRepl,
        ansi_term.ctrlKey('L') => clearScreen,
        ansi_term.ctrlKey('C') => cancelLine,
        control_code.lf, control_code.cr => acceptLine,
        control_code.esc => {
            if (state.bytes_read >= 3 and state.in_buffer[1] == '[') {
                return switch (state.in_buffer[2]) {
                    ansi_term.LEFT => moveCursorLeft,
                    ansi_term.RIGHT => moveCursorRight,
                    ansi_term.UP => historyBackward,
                    ansi_term.DOWN => historyForward,
                    else => doNothing,
                };
            } else {
                return doNothing;
            }
        },
        else => doNothing,
    };
}

/// All possible errors that can occur during line reading.
pub const ReadLineError =
    error{InvalidUtf8} ||
    Allocator.Error ||
    std.fs.File.ReadError ||
    std.Io.Writer.Error ||
    CommandError ||
    switch (SUPPORTED_OS) {
        .linux, .macos => Unix.Error,
        .windows => Windows.Error,
    };

/// Reads a line of input from stdin with line editing and history support.
/// Falls back to simple line reading when stdin is not a TTY (e.g., piped input).
pub fn readLine(self: *ReplLine, outlive: Allocator, prompt: []const u8) ReadLineError![]u8 {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writerStreaming(&stdout_buffer);

    const stdin_file = std.fs.File.stdin();

    // Use simple line reading for non-TTY input (pipes, redirects, tests)
    if (!stdin_file.isTty()) {
        return readLineSimple(outlive, prompt, &stdout_writer.interface, stdin_file);
    }

    return helper(self, outlive, prompt, &stdout_writer.interface, stdin_file);
}

/// Simple line reading for non-TTY input (no raw mode, no escape sequences).
fn readLineSimple(outlive: Allocator, prompt: []const u8, out: *std.Io.Writer, in: std.fs.File) ReadLineError![]u8 {
    // Print the prompt
    try out.writeAll(prompt);
    try out.flush();

    // Read until newline or EOF
    var line_buffer = std.ArrayList(u8).empty;
    var read_buffer: [1]u8 = undefined;

    while (true) {
        const bytes_read = try in.read(&read_buffer);
        if (bytes_read == 0) {
            // EOF - return "exit" to signal REPL should exit
            line_buffer.deinit(outlive);
            return try outlive.dupe(u8, "exit");
        }

        const char = read_buffer[0];
        if (char == '\n' or char == '\r') {
            break;
        }
        try line_buffer.append(outlive, char);
    }

    try out.writeAll(NEW_LINE);
    try out.flush();

    return try line_buffer.toOwnedSlice(outlive);
}

fn helper(self: *ReplLine, outlive: Allocator, prompt: []const u8, out: *std.Io.Writer, in: std.fs.File) ![]u8 {
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
        .history = &self.history,
        .history_index = null,
    };

    const old = switch (SUPPORTED_OS) {
        .linux, .macos => try Unix.init(),
        .windows => try Windows.init(),
    };
    defer old.deinit();

    if (SUPPORTED_OS == .windows) {
        try ansi_term.setCursorColumn(out, 0);
        try ansi_term.clearFromCursorToLineEnd(out);
    }
    try out.writeAll(prompt);
    try out.flush();

    var read_buf: [256]u8 = undefined;

    while (true) : ({
        try out.flush();
    }) {
        const total = try in.read(&read_buf);
        if (total == 0) continue;

        var done = false;
        var i: usize = 0;
        while (i < total) {
            const key = read_buf[i];

            if (key == control_code.esc and i + 2 < total and read_buf[i + 1] == '[') {
                // Escape sequence: copy 3 bytes into in_buffer for findCommandFn
                state.in_buffer[0] = read_buf[i];
                state.in_buffer[1] = read_buf[i + 1];
                state.in_buffer[2] = read_buf[i + 2];
                state.bytes_read = 3;
                i += 3;
            } else {
                state.in_buffer[0] = key;
                state.bytes_read = 1;
                i += 1;
            }

            const cmd = ReplLine.findCommandFn(&state);
            cmd(&state) catch |err| {
                switch (err) {
                    error.ExitRepl => return try outlive.dupe(u8, "exit"),
                    error.NewLine => {
                        done = true;
                        break;
                    },
                    else => |readline_error| return readline_error,
                }
            };
        }
        if (done) break;
    }
    try out.writeAll(NEW_LINE);
    try out.flush();
    return try outlive.dupe(u8, state.line_buffer.items);
}
