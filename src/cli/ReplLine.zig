//! REPL line editor with history support and cross-platform terminal handling.
//! Modified from the anyline library: https://codeberg.org/TheShinx317/anyline
const std = @import("std");
const control_code = std.ascii.control_code;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const ansi_term = @import("ansi_term.zig");
const Unix = @import("Unix.zig");
const Windows = @import("Windows.zig");
const base = @import("base");

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

/// One unit of meaning produced by `InputParser` from a raw byte stream.
pub const InputEvent = union(enum) {
    /// A single byte to be processed normally (typed character, control key, etc.).
    byte: u8,
    /// A 3-byte CSI sequence (ESC [ X) — typically arrow keys.
    csi3: [3]u8,
    /// A bracketed paste began (consumed an `ESC[200~` marker).
    paste_start,
    /// A byte inside an active bracketed paste. May be any value, including
    /// `\n`, `\r`, or a literal `ESC` that wasn't part of the end marker.
    paste_byte: u8,
    /// A bracketed paste ended (consumed an `ESC[201~` marker).
    paste_end,
};

/// Parses the raw byte stream coming from the terminal. Holds enough state
/// across `feed` calls to reassemble escape sequences split across reads,
/// and tracks whether we are currently inside a bracketed paste.
pub const InputParser = struct {
    /// Scratch space for the tail of the previous chunk when it ended on an
    /// incomplete escape sequence. The longest sequence we track is the
    /// 6-byte `ESC[200~` / `ESC[201~` marker, so 5 trailing bytes suffice.
    carry: [5]u8 = undefined,
    carry_len: usize = 0,
    in_paste: bool = false,

    /// Consume `chunk` (concatenated with any carried tail from the previous
    /// call) and append events for the bytes that fully parsed. Any partial
    /// trailing escape sequence is retained in `self.carry` for the next call.
    pub fn feed(
        self: *InputParser,
        chunk: []const u8,
        events: *std.ArrayList(InputEvent),
        gpa: Allocator,
    ) Allocator.Error!void {
        // Working buffer holds carry + chunk. Its size is bounded by the
        // typical 256-byte read buffer in helper(); for tests we accept
        // arbitrarily large chunks via heap allocation.
        var stack_buf: [288]u8 = undefined;
        const total = self.carry_len + chunk.len;
        var heap_buf: ?[]u8 = null;
        defer if (heap_buf) |h| gpa.free(h);
        const buf: []u8 = if (total <= stack_buf.len)
            stack_buf[0..total]
        else blk: {
            const h = try gpa.alloc(u8, total);
            heap_buf = h;
            break :blk h;
        };
        @memcpy(buf[0..self.carry_len], self.carry[0..self.carry_len]);
        @memcpy(buf[self.carry_len..total], chunk);
        self.carry_len = 0;

        var i: usize = 0;
        while (i < total) {
            if (self.in_paste) {
                if (buf[i] == control_code.esc) {
                    if (total - i < ansi_term.PASTE_END.len) {
                        self.saveCarry(buf[i..total]);
                        return;
                    }
                    if (std.mem.eql(u8, buf[i .. i + ansi_term.PASTE_END.len], ansi_term.PASTE_END)) {
                        try events.append(gpa, .paste_end);
                        self.in_paste = false;
                        i += ansi_term.PASTE_END.len;
                        continue;
                    }
                    // Literal ESC inside the paste content.
                    try events.append(gpa, .{ .paste_byte = buf[i] });
                    i += 1;
                } else {
                    try events.append(gpa, .{ .paste_byte = buf[i] });
                    i += 1;
                }
                continue;
            }

            const key = buf[i];
            if (key == control_code.esc and i + 1 < total and buf[i + 1] == '[') {
                // Default: 3-byte CSI sequence (ESC [ X). The exception is the
                // 6-byte bracketed-paste markers, which are disambiguated from
                // other ESC[2... sequences (e.g. Insert is ESC[2~) by the 4th
                // byte: '0' or '1' means paste marker, anything else is just a
                // 3-byte CSI.
                if (total - i < 3) {
                    self.saveCarry(buf[i..total]);
                    return;
                }
                if (buf[i + 2] == '2') {
                    if (total - i < 4) {
                        self.saveCarry(buf[i..total]);
                        return;
                    }
                    const fourth = buf[i + 3];
                    if (fourth == '0' or fourth == '1') {
                        if (total - i < ansi_term.PASTE_START.len) {
                            self.saveCarry(buf[i..total]);
                            return;
                        }
                        if (std.mem.eql(u8, buf[i .. i + ansi_term.PASTE_START.len], ansi_term.PASTE_START)) {
                            try events.append(gpa, .paste_start);
                            self.in_paste = true;
                            i += ansi_term.PASTE_START.len;
                            continue;
                        }
                        if (std.mem.eql(u8, buf[i .. i + ansi_term.PASTE_END.len], ansi_term.PASTE_END)) {
                            // Stray paste-end outside paste mode; ignore.
                            i += ansi_term.PASTE_END.len;
                            continue;
                        }
                        // Doesn't match a known paste marker; fall through to
                        // the generic 3-byte CSI handling below.
                    }
                }

                try events.append(gpa, .{ .csi3 = .{ buf[i], buf[i + 1], buf[i + 2] } });
                i += 3;
            } else if (key == control_code.esc and i + 1 >= total) {
                // Lone ESC at the end of the buffer — could be the start of a
                // longer sequence whose tail is in the next chunk.
                self.saveCarry(buf[i..total]);
                return;
            } else {
                try events.append(gpa, .{ .byte = key });
                i += 1;
            }
        }
    }

    fn saveCarry(self: *InputParser, tail: []const u8) void {
        std.debug.assert(tail.len <= self.carry.len);
        @memcpy(self.carry[0..tail.len], tail);
        self.carry_len = tail.len;
    }
};

/// Write `buf` to `out`, inserting `indent` spaces after every newline so that
/// each continuation line begins under column `indent`. Original whitespace in
/// `buf` is preserved verbatim, so an indented source line stays indented
/// relative to the prompt-aligned baseline.
///
/// Both `\n` and standalone `\r` trigger indentation; the `\r` of a `\r\n`
/// pair is left unindented to avoid double-padding.
fn writeAlignedToPrompt(out: *std.Io.Writer, buf: []const u8, indent: usize) error{WriteFailed}!void {
    var i: usize = 0;
    while (i < buf.len) : (i += 1) {
        const b = buf[i];
        try out.writeByte(b);
        const is_lf = b == '\n';
        const is_lone_cr = b == '\r' and (i + 1 >= buf.len or buf[i + 1] != '\n');
        if (is_lf or is_lone_cr) {
            try out.splatByteAll(' ', indent);
        }
    }
}

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

    pub fn append(self: *History, input: []const u8) Allocator.Error!void {
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
    std.Io.File.ReadStreamingError ||
    std.Io.Writer.Error;

const CommandFn = *const fn (*LineState) CommandError!void;

const LineState = struct {
    outlive: Allocator,
    temp: Allocator,
    prompt: []const u8,
    prompt_width: usize,
    out: *std.Io.Writer,
    in: std.Io.File,
    col_offset: usize,
    line_buffer: std.ArrayList(u8),
    bytes_read: usize,
    in_buffer: [8]u8,
    history: *History,
    history_index: ?usize,
    /// Set after a Ctrl-C so that a second consecutive Ctrl-C quits. Any other
    /// input event clears it, so the two presses must be back-to-back.
    ctrl_c_armed: bool,
};

fn printChar(state: *LineState) CommandError!void {
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

fn deleteBefore(state: *LineState) CommandError!void {
    if (state.col_offset == 0) return;
    state.col_offset -= 1;
    _ = state.line_buffer.orderedRemove(state.col_offset);
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
    try state.out.writeAll(state.line_buffer.items[state.col_offset..]);
    try state.out.writeByte(' ');
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn doNothing(_: *LineState) Allocator.Error!void {}

fn exitRepl(_: *LineState) CommandError!void {
    return error.ExitRepl;
}

fn acceptLine(_: *LineState) CommandError!void {
    return error.NewLine;
}

fn handleCtrlC(state: *LineState) CommandError!void {
    // Discard whatever was on the current line.
    state.line_buffer.clearAndFree(state.temp);
    state.col_offset = 0;
    state.history_index = null;

    // A second consecutive Ctrl-C (with no other input in between) quits.
    if (state.ctrl_c_armed) return error.ExitRepl;
    state.ctrl_c_armed = true;

    // Move to a fresh line, show the hint, and redraw the prompt.
    try state.out.writeAll(NEW_LINE);
    try state.out.writeAll("Ctrl-C again to quit (or enter :quit, :q, or :exit)");
    try state.out.writeAll(NEW_LINE);
    try state.out.writeAll(state.prompt);
    try ansi_term.setCursorColumn(state.out, state.prompt_width);
}

fn clearScreen(state: *LineState) CommandError!void {
    try ansi_term.clearEntireScreen(state.out);
    try ansi_term.setCursor(state.out, 0, 0);

    try state.out.writeAll(state.prompt);
    try state.out.writeAll(state.line_buffer.items);
    try ansi_term.clearFromCursorToLineEnd(state.out);

    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn moveCursorRight(state: *LineState) CommandError!void {
    state.col_offset = @min(state.col_offset + 1, state.line_buffer.items.len);
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn moveCursorLeft(state: *LineState) CommandError!void {
    state.col_offset -|= 1;
    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
}

fn historyBackward(state: *LineState) CommandError!void {
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

fn historyForward(state: *LineState) CommandError!void {
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
        ansi_term.ctrlKey('C') => handleCtrlC,
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
    std.Io.File.ReadStreamingError ||
    std.Io.Writer.Error ||
    CommandError ||
    switch (SUPPORTED_OS) {
        .linux, .macos => Unix.Error,
        .windows => Windows.Error,
    };

/// Result of reading a line of input: either the line bytes or end-of-input.
pub const ReadLineResult = union(enum) {
    line: []u8,
    eof,
};

/// Reads a line of input from stdin with line editing and history support.
/// Falls back to simple line reading when stdin is not a TTY (e.g., piped input).
pub fn readLine(self: *ReplLine, outlive: Allocator, std_io: std.Io, prompt: []const u8, stdin: std.Io.File) ReadLineError!ReadLineResult {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writerStreaming(std_io, &stdout_buffer);

    // Use simple line reading for non-TTY input (pipes, redirects, tests)
    if (!(stdin.isTty(std_io) catch false)) {
        return readLineSimple(outlive, std_io, prompt, &stdout_writer.interface, stdin);
    }

    return helper(self, outlive, std_io, prompt, &stdout_writer.interface, stdin);
}

/// Simple line reading for non-TTY input (no raw mode, no escape sequences).
fn readLineSimple(outlive: Allocator, std_io: std.Io, prompt: []const u8, out: *std.Io.Writer, in: std.Io.File) ReadLineError!ReadLineResult {
    if (prompt.len > 0) {
        try out.writeAll(prompt);
        try out.flush();
    }

    // Read until newline or EOF
    var line_buffer = std.ArrayList(u8).empty;
    var read_buffer: [1]u8 = undefined;

    while (true) {
        const bytes_read = in.readStreaming(std_io, &.{&read_buffer}) catch |err| switch (err) {
            // std.Io streaming returns error.EndOfStream on EOF rather than returning 0 bytes.
            error.EndOfStream => {
                if (line_buffer.items.len == 0) {
                    line_buffer.deinit(outlive);
                    return .eof;
                }
                return .{ .line = try line_buffer.toOwnedSlice(outlive) };
            },
            else => return err,
        };
        if (bytes_read == 0) {
            // Belt-and-suspenders: treat a zero-byte read as EOF as well.
            if (line_buffer.items.len == 0) {
                line_buffer.deinit(outlive);
                return .eof;
            }
            return .{ .line = try line_buffer.toOwnedSlice(outlive) };
        }

        const char = read_buffer[0];
        if (char == '\n' or char == '\r') {
            break;
        }
        try line_buffer.append(outlive, char);
    }

    if (prompt.len > 0) {
        try out.writeAll(NEW_LINE);
        try out.flush();
    }

    return .{ .line = try line_buffer.toOwnedSlice(outlive) };
}

fn helper(self: *ReplLine, outlive: Allocator, std_io: std.Io, prompt: []const u8, out: *std.Io.Writer, in: std.Io.File) ReadLineError!ReplLine.ReadLineResult {
    var arena_allocator = base.SingleThreadArena.init(outlive);
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
        .ctrl_c_armed = false,
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

    // Enable bracketed paste so we can distinguish a multi-line paste from
    // multiple individually typed Enter presses.
    try out.writeAll(ansi_term.BRACKETED_PASTE_ENABLE);
    defer {
        out.writeAll(ansi_term.BRACKETED_PASTE_DISABLE) catch {};
        out.flush() catch {};
    }

    try out.writeAll(prompt);
    try out.flush();

    var read_buf: [256]u8 = undefined;
    var parser = InputParser{};
    var events = std.ArrayList(InputEvent).empty;
    defer events.deinit(temp);
    var paste_buffer = std.ArrayList(u8).empty;
    defer paste_buffer.deinit(temp);

    while (true) : ({
        try out.flush();
    }) {
        const new_bytes = try in.readStreaming(std_io, &.{&read_buf});
        if (new_bytes == 0) continue;

        events.clearRetainingCapacity();
        try parser.feed(read_buf[0..new_bytes], &events, temp);

        var done = false;
        for (events.items) |event| {
            // The Ctrl-C "press again to quit" arming only survives consecutive
            // Ctrl-C presses; any other input event disarms it.
            const is_ctrl_c = switch (event) {
                .byte => |b| b == ansi_term.ctrlKey('C'),
                else => false,
            };
            if (!is_ctrl_c) state.ctrl_c_armed = false;

            switch (event) {
                .byte => |b| {
                    state.in_buffer[0] = b;
                    state.bytes_read = 1;
                },
                .csi3 => |seq| {
                    state.in_buffer[0] = seq[0];
                    state.in_buffer[1] = seq[1];
                    state.in_buffer[2] = seq[2];
                    state.bytes_read = 3;
                },
                .paste_start => {
                    paste_buffer.clearRetainingCapacity();
                    continue;
                },
                .paste_byte => |b| {
                    try paste_buffer.append(state.temp, b);
                    continue;
                },
                .paste_end => {
                    // Insert pasted content at the current cursor position.
                    try state.line_buffer.insertSlice(state.temp, state.col_offset, paste_buffer.items);
                    state.col_offset += paste_buffer.items.len;
                    state.history_index = null;

                    const has_newline = std.mem.findAny(u8, paste_buffer.items, "\n\r") != null;
                    paste_buffer.clearRetainingCapacity();

                    // Redraw so the user sees the pasted text. For a multi-line
                    // paste the embedded newlines are translated by the terminal
                    // (OPOST/ONLCR) so each pasted line lands on its own row;
                    // indent each continuation line by `prompt_width` so it
                    // aligns under the first character past the prompt.
                    try ansi_term.setCursorColumn(state.out, state.prompt_width);
                    try writeAlignedToPrompt(state.out, state.line_buffer.items, state.prompt_width);
                    try ansi_term.clearFromCursorToLineEnd(state.out);

                    if (has_newline) {
                        // A multi-line paste is treated as a complete
                        // input — submit it as a single REPL entry.
                        done = true;
                        break;
                    }

                    // Single-line paste: position the cursor for further editing.
                    try ansi_term.setCursorColumn(state.out, state.prompt_width + state.col_offset);
                    continue;
                },
            }

            const cmd = ReplLine.findCommandFn(&state);
            cmd(&state) catch |err| {
                switch (err) {
                    error.ExitRepl => return .eof,
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
    return .{ .line = try outlive.dupe(u8, state.line_buffer.items) };
}

const testing = std.testing;

/// Run `parser.feed` for each chunk in `chunks` against a fresh event list and
/// return the accumulated events. Caller owns the returned ArrayList.
fn collectEvents(parser: *InputParser, chunks: []const []const u8) Allocator.Error!std.ArrayList(InputEvent) {
    var events = std.ArrayList(InputEvent).empty;
    errdefer events.deinit(testing.allocator);
    for (chunks) |chunk| {
        try parser.feed(chunk, &events, testing.allocator);
    }
    return events;
}

fn expectEventsEqual(expected: []const InputEvent, actual: []const InputEvent) error{TestExpectedEqual}!void {
    try testing.expectEqual(expected.len, actual.len);
    for (expected, actual) |e, a| {
        try testing.expectEqualDeep(e, a);
    }
}

test "InputParser: plain bytes pass through" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"hello"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .byte = 'h' },
        .{ .byte = 'e' },
        .{ .byte = 'l' },
        .{ .byte = 'l' },
        .{ .byte = 'o' },
    }, events.items);
    try testing.expectEqual(@as(usize, 0), parser.carry_len);
    try testing.expect(!parser.in_paste);
}

test "InputParser: 3-byte CSI arrow key in one chunk" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1b[A"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .csi3 = .{ 0x1b, '[', 'A' } },
    }, events.items);
    try testing.expectEqual(@as(usize, 0), parser.carry_len);
}

test "InputParser: 3-byte CSI split as ESC then [A" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b", "[A" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .csi3 = .{ 0x1b, '[', 'A' } },
    }, events.items);
    try testing.expectEqual(@as(usize, 0), parser.carry_len);
}

test "InputParser: bare ESC followed by non-[ bytes is flushed as bytes" {
    // Once a non-`[` byte appears after ESC, ESC and the trailing bytes are
    // emitted as plain `byte` events (the existing 3-byte CSI path is the
    // only one that consumes ESC specially).
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1bOP"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .byte = 0x1b },
        .{ .byte = 'O' },
        .{ .byte = 'P' },
    }, events.items);
    try testing.expectEqual(@as(usize, 0), parser.carry_len);
}

test "InputParser: full multi-line paste in one chunk" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1b[200~hello\nworld\x1b[201~"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'h' },
        .{ .paste_byte = 'e' },
        .{ .paste_byte = 'l' },
        .{ .paste_byte = 'l' },
        .{ .paste_byte = 'o' },
        .{ .paste_byte = '\n' },
        .{ .paste_byte = 'w' },
        .{ .paste_byte = 'o' },
        .{ .paste_byte = 'r' },
        .{ .paste_byte = 'l' },
        .{ .paste_byte = 'd' },
        .paste_end,
    }, events.items);
    try testing.expect(!parser.in_paste);
    try testing.expectEqual(@as(usize, 0), parser.carry_len);
}

test "InputParser: empty paste" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1b[200~\x1b[201~"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{ .paste_start, .paste_end }, events.items);
    try testing.expect(!parser.in_paste);
}

test "InputParser: two pastes back-to-back in one chunk" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1b[200~a\nb\x1b[201~\x1b[200~c\nd\x1b[201~"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'a' },
        .{ .paste_byte = '\n' },
        .{ .paste_byte = 'b' },
        .paste_end,
        .paste_start,
        .{ .paste_byte = 'c' },
        .{ .paste_byte = '\n' },
        .{ .paste_byte = 'd' },
        .paste_end,
    }, events.items);
    try testing.expect(!parser.in_paste);
}

test "InputParser: paste preserves both \\r and \\n" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1b[200~a\r\nb\x1b[201~"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'a' },
        .{ .paste_byte = '\r' },
        .{ .paste_byte = '\n' },
        .{ .paste_byte = 'b' },
        .paste_end,
    }, events.items);
}

test "InputParser: paste-start split after ESC" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b", "[200~hi\x1b[201~" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'h' },
        .{ .paste_byte = 'i' },
        .paste_end,
    }, events.items);
    try testing.expectEqual(@as(usize, 0), parser.carry_len);
}

test "InputParser: paste-start split after ESC[" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b[", "200~hi\x1b[201~" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'h' },
        .{ .paste_byte = 'i' },
        .paste_end,
    }, events.items);
}

test "InputParser: paste-start split after ESC[2 (4-byte boundary)" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b[2", "00~hi\x1b[201~" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'h' },
        .{ .paste_byte = 'i' },
        .paste_end,
    }, events.items);
}

test "InputParser: paste-start split after ESC[20 (5-byte boundary)" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b[20", "0~hi\x1b[201~" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'h' },
        .{ .paste_byte = 'i' },
        .paste_end,
    }, events.items);
}

test "InputParser: paste-end with trailing ~ in next chunk" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b[200~ab\x1b[201", "~" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'a' },
        .{ .paste_byte = 'b' },
        .paste_end,
    }, events.items);
    try testing.expect(!parser.in_paste);
}

test "InputParser: paste-end split after ESC mid-paste does not leak ESC" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b[200~ab\x1b", "[201~" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'a' },
        .{ .paste_byte = 'b' },
        .paste_end,
    }, events.items);
}

test "InputParser: ESC[2~ (Insert) is not a paste marker" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1b[2~"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .csi3 = .{ 0x1b, '[', '2' } },
        .{ .byte = '~' },
    }, events.items);
    try testing.expect(!parser.in_paste);
}

test "InputParser: ESC[2~ split with ~ in next chunk" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b[2", "~" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .csi3 = .{ 0x1b, '[', '2' } },
        .{ .byte = '~' },
    }, events.items);
    try testing.expect(!parser.in_paste);
}

test "InputParser: malformed paste-start (ESC[200X) falls through" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1b[200X"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .csi3 = .{ 0x1b, '[', '2' } },
        .{ .byte = '0' },
        .{ .byte = '0' },
        .{ .byte = 'X' },
    }, events.items);
    try testing.expect(!parser.in_paste);
}

test "InputParser: stray paste-end outside paste mode is silently consumed" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1b[201~abc"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .byte = 'a' },
        .{ .byte = 'b' },
        .{ .byte = 'c' },
    }, events.items);
}

test "InputParser: ESC bytes inside paste content are preserved" {
    var parser = InputParser{};
    // Paste containing an ANSI color escape: ESC[31m
    var events = try collectEvents(&parser, &.{"\x1b[200~\x1b[31mred\x1b[201~"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 0x1b },
        .{ .paste_byte = '[' },
        .{ .paste_byte = '3' },
        .{ .paste_byte = '1' },
        .{ .paste_byte = 'm' },
        .{ .paste_byte = 'r' },
        .{ .paste_byte = 'e' },
        .{ .paste_byte = 'd' },
        .paste_end,
    }, events.items);
}

test "InputParser: 5-byte carry survives across feeds" {
    // Send the maximum-length partial prefix the parser tracks (5 bytes of
    // a paste-start: ESC [ 2 0 0) and then the final `~` plus content. This
    // exercises the boundary where carry equals the carry-buffer capacity.
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "\x1b[200", "~hi\x1b[201~" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'h' },
        .{ .paste_byte = 'i' },
        .paste_end,
    }, events.items);
}

test "InputParser: byte-by-byte feed of full paste sequence" {
    // Feed every byte of a paste sequence one at a time. Exercises every
    // carry-boundary the parser might encounter, including paste-end.
    const seq = "\x1b[200~ok\x1b[201~";
    var parser = InputParser{};
    var events = std.ArrayList(InputEvent).empty;
    defer events.deinit(testing.allocator);
    for (seq) |b| {
        try parser.feed(&[_]u8{b}, &events, testing.allocator);
    }
    try expectEventsEqual(&.{
        .paste_start,
        .{ .paste_byte = 'o' },
        .{ .paste_byte = 'k' },
        .paste_end,
    }, events.items);
    try testing.expect(!parser.in_paste);
    try testing.expectEqual(@as(usize, 0), parser.carry_len);
}

test "InputParser: bytes around a paste in the same chunk" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"x\x1b[200~hi\x1b[201~y"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .byte = 'x' },
        .paste_start,
        .{ .paste_byte = 'h' },
        .{ .paste_byte = 'i' },
        .paste_end,
        .{ .byte = 'y' },
    }, events.items);
}

fn expectAlignedOutput(input: []const u8, indent: usize, expected: []const u8) (std.mem.Allocator.Error || error{WriteFailed} || error{TestExpectedEqual})!void {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    try writeAlignedToPrompt(&aw.writer, input, indent);
    try testing.expectEqualStrings(expected, aw.writer.buffered());
}

test "writeAlignedToPrompt: no newlines passes bytes through" {
    try expectAlignedOutput("x = 5", 2, "x = 5");
}

test "writeAlignedToPrompt: LF gets indent on the next line" {
    try expectAlignedOutput("z = 5\ny = 6", 2, "z = 5\n  y = 6");
}

test "writeAlignedToPrompt: CRLF only indents once" {
    try expectAlignedOutput("z = 5\r\ny = 6", 2, "z = 5\r\n  y = 6");
}

test "writeAlignedToPrompt: lone CR indents the next line" {
    try expectAlignedOutput("z = 5\ry = 6", 2, "z = 5\r  y = 6");
}

test "writeAlignedToPrompt: original indentation is preserved on top of prompt indent" {
    // First line at column 0 (under prompt baseline), second line indented
    // four spaces relative to baseline must stay four spaces relative to it.
    try expectAlignedOutput("z = 5\n    y = 6", 2, "z = 5\n      y = 6");
}

test "writeAlignedToPrompt: trailing newline still emits indent for empty next line" {
    try expectAlignedOutput("z = 5\n", 2, "z = 5\n  ");
}
