//! REPL line editor with history support and cross-platform terminal handling.
//! Modified from the anyline library: https://codeberg.org/TheShinx317/anyline
const std = @import("std");
const control_code = std.ascii.control_code;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const ansi_term = @import("ansi_term.zig");
const unicode = @import("unicode.zig");
const Unix = @import("Unix.zig");
const Windows = @import("Windows.zig");
const base = @import("base");

const SupportedOS = enum { windows, posix };

/// The operating system this build is targeting.
pub const SUPPORTED_OS = switch (builtin.os.tag) {
    .windows => SupportedOS.windows,
    .linux, .macos, .freebsd, .openbsd, .netbsd, .dragonfly => SupportedOS.posix,
    .freestanding,
    .other,
    .contiki,
    .fuchsia,
    .hermit,
    .managarm,
    .haiku,
    .hurd,
    .illumos,
    .plan9,
    .rtems,
    .serenity,
    .driverkit,
    .ios,
    .maccatalyst,
    .tvos,
    .visionos,
    .watchos,
    .uefi,
    .@"3ds",
    .ps3,
    .ps4,
    .ps5,
    .psp,
    .vita,
    .emscripten,
    .wasi,
    .amdhsa,
    .amdpal,
    .cuda,
    .mesa3d,
    .nvcl,
    .opencl,
    .opengl,
    .vulkan,
    => |tag| @compileError(@tagName(tag) ++ " is not a supported OS for ReplLine!"),
};

/// Platform-specific newline sequence.
pub const NEW_LINE = switch (SUPPORTED_OS) {
    .posix => "\n",
    .windows => "\r\n",
};

/// One unit of meaning produced by `InputParser` from a raw byte stream.
pub const InputEvent = union(enum) {
    /// One ASCII byte to be processed normally (text, control key, etc.).
    byte: u8,
    /// One complete, validated non-ASCII Unicode scalar value.
    codepoint: u21,
    /// A 2-byte ESC sequence (ESC X)—typically Alt-key combinations.
    esc2: [2]u8,
    /// A 3-byte CSI sequence (ESC [ X)—typically arrow keys.
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
    /// incomplete escape sequence or UTF-8 codepoint. The longest sequence we
    /// track is the 6-byte `ESC[200~` / `ESC[201~` marker, so 5 bytes suffice.
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
    ) (Allocator.Error || error{InvalidUtf8})!void {
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
            } else if (key == control_code.esc and i + 1 < total and buf[i + 1] < 0x80 and buf[i + 1] != '[') {
                // 2-byte ESC sequence (ESC X)
                try events.append(gpa, .{ .esc2 = .{ buf[i], buf[i + 1] } });
                i += 2;
            } else if (key == control_code.esc and i + 1 >= total) {
                // Lone ESC at the end of the buffer—could be the start of a
                // longer sequence whose tail is in the next chunk.
                self.saveCarry(buf[i..total]);
                return;
            } else if (key < 0x80) {
                try events.append(gpa, .{ .byte = key });
                i += 1;
            } else {
                const sequence_len = std.unicode.utf8ByteSequenceLength(key) catch return error.InvalidUtf8;
                if (total - i < sequence_len) {
                    self.saveCarry(buf[i..total]);
                    return;
                }
                const sequence = buf[i .. i + sequence_len];
                const codepoint = std.unicode.utf8Decode(sequence) catch return error.InvalidUtf8;
                try events.append(gpa, .{ .codepoint = codepoint });
                i += sequence_len;
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

    pub fn append(self: *History, input: []const u8) (Allocator.Error || error{InvalidUtf8})!void {
        if (!std.unicode.utf8ValidateSlice(input)) return error.InvalidUtf8;

        var it = std.mem.splitScalar(u8, input, '\n');
        while (it.next()) |raw_line| {
            const line = std.mem.trimEnd(u8, raw_line, "\r");
            if (line.len == 0) continue;

            if (self.entries.items.len > 0) {
                const last = self.entries.items[self.entries.items.len - 1];
                if (std.mem.eql(u8, last, line)) continue;
            }

            try self.entries.ensureUnusedCapacity(self.allocator, 1);
            const line_copy = try self.allocator.dupe(u8, line);
            self.entries.appendAssumeCapacity(line_copy);
        }
    }
};

// struct to manage REPL line editing
const ReplLine = @This();

allocator: Allocator,
history: History,
kill_ring: ?[]const u8,
replay_index: ?usize,

pub fn init(allocator: Allocator) ReplLine {
    return ReplLine{
        .allocator = allocator,
        .history = History.init(allocator),
        .kill_ring = null,
        .replay_index = null,
    };
}

pub fn deinit(self: *ReplLine) void {
    self.history.deinit();
    if (self.kill_ring) |k| {
        self.allocator.free(k);
    }
}

/// Add submitted input to this line editor's in-memory history.
pub fn recordHistory(self: *ReplLine, input: []const u8) (Allocator.Error || error{InvalidUtf8})!void {
    try self.history.append(input);
}

const CommandError =
    error{ DeleteEmptyLineBuffer, NewLine, ExitRepl, InvalidUtf8 } ||
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
    /// UTF-8 byte offset of the cursor. This is always a grapheme boundary;
    /// terminal-cell positioning is computed separately from the buffer prefix.
    col_offset: usize,
    line_buffer: std.ArrayList(u8),
    bytes_read: usize,
    in_buffer: [8]u8,
    history: *History,
    history_index: ?usize,
    transient_line: ?[]const u8,
    kill_ring: *?[]const u8,
    replay_index: *?usize,
    /// Set after a Ctrl-C so that a second consecutive Ctrl-C quits. Any other
    /// input event clears it, so the two presses must be back-to-back.
    ctrl_c_armed: bool,
};

fn cursorColumn(state: *const LineState) error{InvalidUtf8}!usize {
    return state.prompt_width + try unicode.displayWidth(state.line_buffer.items[0..state.col_offset], state.prompt_width);
}

fn setEditorCursor(state: *LineState) CommandError!void {
    try ansi_term.setCursorColumn(state.out, try cursorColumn(state));
}

fn redrawLine(state: *LineState) CommandError!void {
    try ansi_term.setCursorColumn(state.out, state.prompt_width);
    try state.out.writeAll(state.line_buffer.items);
    try ansi_term.clearFromCursorToLineEnd(state.out);
    try setEditorCursor(state);
}

fn insertText(state: *LineState, text: []const u8) CommandError!void {
    if (!std.unicode.utf8ValidateSlice(text)) return error.InvalidUtf8;

    // Reset history navigation on new input
    state.history_index = null;
    state.replay_index.* = null;

    // Insert at col_offset, not just append
    const inserted_end = state.col_offset + text.len;
    try state.line_buffer.insertSlice(state.temp, state.col_offset, text);
    state.col_offset = try unicode.graphemeBoundaryAtOrAfter(state.line_buffer.items, inserted_end);

    try redrawLine(state);
}

fn printChar(state: *LineState) CommandError!void {
    try insertText(state, state.in_buffer[0..1]);
}

fn printCodepoint(state: *LineState, codepoint: u21) CommandError!void {
    var encoded: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(codepoint, &encoded) catch unreachable;
    try insertText(state, encoded[0..len]);
}

/// Inserts one complete bracketed paste. Returns whether the paste contains a
/// newline and should therefore be submitted immediately.
fn insertPaste(state: *LineState, pasted: []const u8) CommandError!bool {
    if (!std.unicode.utf8ValidateSlice(pasted)) return error.InvalidUtf8;

    const inserted_end = state.col_offset + pasted.len;
    try state.line_buffer.insertSlice(state.temp, state.col_offset, pasted);
    state.col_offset = try unicode.graphemeBoundaryAtOrAfter(state.line_buffer.items, inserted_end);
    state.history_index = null;
    state.replay_index.* = null;

    const has_newline = std.mem.findAny(u8, pasted, "\n\r") != null;
    if (has_newline) {
        // Embedded newlines are translated by the terminal (OPOST/ONLCR), so
        // indent each continuation line to the prompt.
        try ansi_term.setCursorColumn(state.out, state.prompt_width);
        try writeAlignedToPrompt(state.out, state.line_buffer.items, state.prompt_width);
        try ansi_term.clearFromCursorToLineEnd(state.out);
    } else {
        try redrawLine(state);
    }
    return has_newline;
}

fn deleteBefore(state: *LineState) CommandError!void {
    if (state.col_offset == 0) return;
    state.history_index = null;

    const previous = try unicode.previousGraphemeBoundary(state.line_buffer.items, state.col_offset);
    const removed_len = state.col_offset - previous;
    std.mem.copyForwards(
        u8,
        state.line_buffer.items[previous .. state.line_buffer.items.len - removed_len],
        state.line_buffer.items[state.col_offset..],
    );
    state.line_buffer.shrinkRetainingCapacity(state.line_buffer.items.len - removed_len);
    state.col_offset = try unicode.graphemeBoundaryAtOrAfter(state.line_buffer.items, previous);
    try redrawLine(state);
}

fn doNothing(_: *LineState) Allocator.Error!void {}

fn exitRepl(_: *LineState) CommandError!void {
    return error.ExitRepl;
}

fn acceptLine(state: *LineState) CommandError!void {
    state.replay_index.* = if (state.history_index) |index| index + 1 else null;
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

    try setEditorCursor(state);
}

fn moveCursorRight(state: *LineState) CommandError!void {
    state.col_offset = try unicode.nextGraphemeBoundary(state.line_buffer.items, state.col_offset);
    try setEditorCursor(state);
}

fn moveCursorLeft(state: *LineState) CommandError!void {
    state.col_offset = try unicode.previousGraphemeBoundary(state.line_buffer.items, state.col_offset);
    try setEditorCursor(state);
}

fn moveCursorToStart(state: *LineState) CommandError!void {
    state.col_offset = 0;
    try ansi_term.setCursorColumn(state.out, state.prompt_width);
}

fn moveCursorToEnd(state: *LineState) CommandError!void {
    state.col_offset = state.line_buffer.items.len;
    try setEditorCursor(state);
}

fn replaceKillRing(state: *LineState, text: []const u8) Allocator.Error!void {
    const replacement = try state.outlive.dupe(u8, text);
    if (state.kill_ring.*) |previous| {
        state.outlive.free(previous);
    }
    state.kill_ring.* = replacement;
}

fn killLineToEnd(state: *LineState) CommandError!void {
    const cut_text = state.line_buffer.items[state.col_offset..];
    if (cut_text.len > 0) {
        try replaceKillRing(state, cut_text);
        state.history_index = null;
    }
    state.line_buffer.shrinkAndFree(state.temp, state.col_offset);
    try ansi_term.clearFromCursorToLineEnd(state.out);
}

fn isWordGrapheme(buf: []const u8, start: usize, end: usize) error{InvalidUtf8}!bool {
    const sequence_len = std.unicode.utf8ByteSequenceLength(buf[start]) catch return error.InvalidUtf8;
    if (start + sequence_len > end) return error.InvalidUtf8;
    const codepoint = std.unicode.utf8Decode(buf[start .. start + sequence_len]) catch return error.InvalidUtf8;
    return codepoint >= 0x80 or std.ascii.isAlphanumeric(@intCast(codepoint)) or codepoint == '_';
}

fn findWordStartBackward(buf: []const u8, start: usize) error{InvalidUtf8}!usize {
    var i = start;
    while (i > 0) {
        const previous = try unicode.previousGraphemeBoundary(buf, i);
        if (try isWordGrapheme(buf, previous, i)) break;
        i = previous;
    }
    while (i > 0) {
        const previous = try unicode.previousGraphemeBoundary(buf, i);
        if (!try isWordGrapheme(buf, previous, i)) break;
        i = previous;
    }
    return i;
}

fn deleteToStart(state: *LineState) CommandError!void {
    if (state.col_offset == 0) return;
    state.history_index = null;

    const cut_text = state.line_buffer.items[0..state.col_offset];
    try replaceKillRing(state, cut_text);

    const remaining_len = state.line_buffer.items.len - state.col_offset;
    std.mem.copyForwards(u8, state.line_buffer.items[0..remaining_len], state.line_buffer.items[state.col_offset..]);
    state.line_buffer.shrinkRetainingCapacity(remaining_len);
    state.col_offset = 0;

    try redrawLine(state);
}

fn deleteWordBackward(state: *LineState) CommandError!void {
    if (state.col_offset == 0) return;

    const word_start = try findWordStartBackward(state.line_buffer.items, state.col_offset);
    if (word_start == state.col_offset) return;
    state.history_index = null;

    const cut_text = state.line_buffer.items[word_start..state.col_offset];
    try replaceKillRing(state, cut_text);

    const remaining_len = state.line_buffer.items.len - (state.col_offset - word_start);
    std.mem.copyForwards(
        u8,
        state.line_buffer.items[word_start..remaining_len],
        state.line_buffer.items[state.col_offset..],
    );
    state.line_buffer.shrinkRetainingCapacity(remaining_len);
    state.col_offset = try unicode.graphemeBoundaryAtOrAfter(state.line_buffer.items, word_start);

    try redrawLine(state);
}

fn yank(state: *LineState) CommandError!void {
    const text = state.kill_ring.* orelse return;
    if (text.len == 0) return;
    if (!std.unicode.utf8ValidateSlice(text)) return error.InvalidUtf8;
    state.history_index = null;

    const inserted_end = state.col_offset + text.len;
    try state.line_buffer.insertSlice(state.temp, state.col_offset, text);
    state.col_offset = try unicode.graphemeBoundaryAtOrAfter(state.line_buffer.items, inserted_end);

    try redrawLine(state);
}

fn findWordEndForward(buf: []const u8, start: usize) error{InvalidUtf8}!usize {
    var i = start;
    const len = buf.len;
    while (i < len) {
        const next = try unicode.nextGraphemeBoundary(buf, i);
        if (try isWordGrapheme(buf, i, next)) break;
        i = next;
    }
    while (i < len) {
        const next = try unicode.nextGraphemeBoundary(buf, i);
        if (!try isWordGrapheme(buf, i, next)) break;
        i = next;
    }
    return i;
}

fn moveWordLeft(state: *LineState) CommandError!void {
    state.col_offset = try findWordStartBackward(state.line_buffer.items, state.col_offset);
    try setEditorCursor(state);
}

fn moveWordRight(state: *LineState) CommandError!void {
    state.col_offset = try findWordEndForward(state.line_buffer.items, state.col_offset);
    try setEditorCursor(state);
}

fn killWordForward(state: *LineState) CommandError!void {
    const word_end = try findWordEndForward(state.line_buffer.items, state.col_offset);
    if (word_end == state.col_offset) return;
    state.history_index = null;

    const cut_text = state.line_buffer.items[state.col_offset..word_end];
    try replaceKillRing(state, cut_text);

    const cut_len = word_end - state.col_offset;
    const remaining_len = state.line_buffer.items.len - cut_len;
    std.mem.copyForwards(
        u8,
        state.line_buffer.items[state.col_offset..remaining_len],
        state.line_buffer.items[word_end..],
    );
    state.line_buffer.shrinkRetainingCapacity(remaining_len);
    state.col_offset = try unicode.graphemeBoundaryAtOrAfter(state.line_buffer.items, state.col_offset);

    try redrawLine(state);
}

fn historyBackward(state: *LineState) CommandError!void {
    state.replay_index.* = null;

    const hist_len = state.history.entries.items.len;
    if (hist_len == 0) return;

    if (state.history_index == null) {
        state.transient_line = try state.temp.dupe(u8, state.line_buffer.items);
        state.history_index = hist_len - 1;
    } else if (state.history_index.? > 0) {
        state.history_index = state.history_index.? - 1;
    }

    const entry = state.history.entries.items[state.history_index.?];
    state.line_buffer.clearAndFree(state.temp);
    try state.line_buffer.appendSlice(state.temp, entry);
    state.col_offset = entry.len;

    try redrawLine(state);
}

fn historyForward(state: *LineState) CommandError!void {
    const hist_len = state.history.entries.items.len;
    const replay_index = state.replay_index.*;
    state.replay_index.* = null;
    if (hist_len == 0) return;

    if (state.history_index) |index| {
        if (index < hist_len - 1) {
            const next_index = index + 1;
            state.history_index = next_index;
            const entry = state.history.entries.items[next_index];
            state.line_buffer.clearAndFree(state.temp);
            try state.line_buffer.appendSlice(state.temp, entry);
            state.col_offset = entry.len;
        } else {
            // Past the end, restore transient draft line
            state.history_index = null;
            state.line_buffer.clearAndFree(state.temp);
            if (state.transient_line) |transient| {
                try state.line_buffer.appendSlice(state.temp, transient);
                state.col_offset = transient.len;
            } else {
                state.col_offset = 0;
            }
        }
    } else if (replay_index) |index| {
        if (index >= hist_len) return;

        state.history_index = index;
        const entry = state.history.entries.items[index];
        state.line_buffer.clearAndFree(state.temp);
        try state.line_buffer.appendSlice(state.temp, entry);
        state.col_offset = entry.len;
    } else {
        return;
    }

    try redrawLine(state);
}

fn findCommandFn(state: *LineState) CommandFn {
    const key = state.in_buffer[0];
    if (key >= ' ' and key <= '~') return printChar;
    if (key == ansi_term.BACKSPACE) return deleteBefore;
    if (key == ansi_term.ctrlKey('D')) return exitRepl;
    if (key == ansi_term.ctrlKey('L')) return clearScreen;
    if (key == ansi_term.ctrlKey('C')) return handleCtrlC;
    if (key == ansi_term.ctrlKey('A')) return moveCursorToStart;
    if (key == ansi_term.ctrlKey('E')) return moveCursorToEnd;
    if (key == ansi_term.ctrlKey('K')) return killLineToEnd;
    if (key == ansi_term.ctrlKey('U')) return deleteToStart;
    if (key == ansi_term.ctrlKey('W')) return deleteWordBackward;
    if (key == ansi_term.ctrlKey('Y')) return yank;
    if (key == ansi_term.ctrlKey('B')) return moveCursorLeft;
    if (key == ansi_term.ctrlKey('F')) return moveCursorRight;
    if (key == ansi_term.ctrlKey('H')) return deleteBefore;
    if (key == control_code.lf or key == control_code.cr) return acceptLine;
    if (key != control_code.esc) return doNothing;

    if (state.bytes_read >= 3 and state.in_buffer[1] == '[') {
        const direction = state.in_buffer[2];
        if (direction == ansi_term.LEFT) return moveCursorLeft;
        if (direction == ansi_term.RIGHT) return moveCursorRight;
        if (direction == ansi_term.UP) return historyBackward;
        if (direction == ansi_term.DOWN) return historyForward;
    } else if (state.bytes_read == 2) {
        return switch (state.in_buffer[1]) {
            'b', 'B' => moveWordLeft,
            'f', 'F' => moveWordRight,
            'd', 'D' => killWordForward,
            else => doNothing,
        };
    }
    return doNothing;
}

/// All possible errors that can occur during line reading.
pub const ReadLineError =
    error{InvalidUtf8} ||
    Allocator.Error ||
    std.Io.File.ReadStreamingError ||
    std.Io.Writer.Error ||
    CommandError ||
    switch (SUPPORTED_OS) {
        .posix => Unix.Error,
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
            error.AccessDenied,
            error.Canceled,
            error.ConnectionResetByPeer,
            error.InputOutput,
            error.IsDir,
            error.LockViolation,
            error.NotOpenForReading,
            error.SocketUnconnected,
            error.SystemResources,
            error.Unexpected,
            error.WouldBlock,
            => return err,
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
        .transient_line = null,
        .kill_ring = &self.kill_ring,
        .replay_index = &self.replay_index,
        .ctrl_c_armed = false,
    };

    const old = switch (SUPPORTED_OS) {
        .posix => try Unix.init(),
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
            const is_ctrl_c = std.meta.activeTag(event) == .byte and event.byte == ansi_term.ctrlKey('C');
            if (!is_ctrl_c) state.ctrl_c_armed = false;

            switch (event) {
                .byte => |b| {
                    state.in_buffer[0] = b;
                    state.bytes_read = 1;
                },
                .codepoint => |codepoint| {
                    try printCodepoint(&state, codepoint);
                    continue;
                },
                .esc2 => |seq| {
                    state.in_buffer[0] = seq[0];
                    state.in_buffer[1] = seq[1];
                    state.bytes_read = 2;
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
                    const has_newline = try insertPaste(&state, paste_buffer.items);
                    paste_buffer.clearRetainingCapacity();

                    if (has_newline) {
                        // A multi-line paste is treated as a complete
                        // input—submit it as a single REPL entry.
                        done = true;
                        break;
                    }

                    continue;
                },
            }

            const cmd = ReplLine.findCommandFn(&state);

            if (cmd != historyForward and cmd != historyBackward and cmd != acceptLine) {
                state.replay_index.* = null;
            }

            cmd(&state) catch |err| {
                switch (err) {
                    error.ExitRepl => return .eof,
                    error.NewLine => {
                        done = true;
                        break;
                    },
                    error.AccessDenied,
                    error.Canceled,
                    error.ConnectionResetByPeer,
                    error.DeleteEmptyLineBuffer,
                    error.EndOfStream,
                    error.InputOutput,
                    error.InvalidUtf8,
                    error.IsDir,
                    error.LockViolation,
                    error.NotOpenForReading,
                    error.OutOfMemory,
                    error.SocketUnconnected,
                    error.SystemResources,
                    error.Unexpected,
                    error.WouldBlock,
                    error.WriteFailed,
                    => |readline_error| return readline_error,
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
fn collectEvents(parser: *InputParser, chunks: []const []const u8) (Allocator.Error || error{InvalidUtf8})!std.ArrayList(InputEvent) {
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

test "InputParser: UTF-8 is emitted as complete codepoints across reads" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{ "caf\xc3", "\xa9" });
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .byte = 'c' },
        .{ .byte = 'a' },
        .{ .byte = 'f' },
        .{ .codepoint = 'é' },
    }, events.items);
    try testing.expectEqual(@as(usize, 0), parser.carry_len);
}

test "InputParser: invalid UTF-8 is rejected" {
    var parser = InputParser{};
    var events = std.ArrayList(InputEvent).empty;
    defer events.deinit(testing.allocator);
    try testing.expectError(error.InvalidUtf8, parser.feed("\xc3x", &events, testing.allocator));
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

test "InputParser: bare ESC followed by non-[ bytes is parsed as a 2-byte ESC sequence" {
    // Once a non-`[` byte appears after ESC, it is parsed as a 2-byte ESC sequence.
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1bOP"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .esc2 = .{ 0x1b, 'O' } },
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

test "Keyboard commands: advanced bindings" {
    var line_buffer = std.ArrayList(u8).empty;
    defer line_buffer.deinit(testing.allocator);

    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();

    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |k| testing.allocator.free(k);

    var dummy_replay_index: ?usize = null;
    var state = LineState{
        .outlive = testing.allocator,
        .temp = testing.allocator,
        .prompt = "» ",
        .prompt_width = 2,
        .out = &aw.writer,
        .in = undefined,
        .col_offset = 0,
        .line_buffer = line_buffer,
        .bytes_read = 0,
        .in_buffer = undefined,
        .history = undefined,
        .history_index = null,
        .transient_line = null,
        .kill_ring = &kill_ring,
        .replay_index = &dummy_replay_index,
        .ctrl_c_armed = false,
    };
    defer state.line_buffer.deinit(testing.allocator);

    // Simulate typing "hello world!"
    try state.line_buffer.appendSlice(testing.allocator, "hello world!");
    state.col_offset = 12;

    // Ctrl-A: move cursor to start
    try moveCursorToStart(&state);
    try testing.expectEqual(@as(usize, 0), state.col_offset);

    // Ctrl-E: move cursor to end
    try moveCursorToEnd(&state);
    try testing.expectEqual(@as(usize, 12), state.col_offset);

    // Ctrl-B: move cursor left (backward)
    try moveCursorLeft(&state);
    try testing.expectEqual(@as(usize, 11), state.col_offset);

    // Ctrl-F: move cursor right (forward)
    try moveCursorRight(&state);
    try testing.expectEqual(@as(usize, 12), state.col_offset);

    // Ctrl-H / Backspace: delete character before cursor
    try deleteBefore(&state); // deletes '!'
    try testing.expectEqualStrings("hello world", state.line_buffer.items);
    try testing.expectEqual(@as(usize, 11), state.col_offset);

    // Move cursor back to 5 (after "hello")
    state.col_offset = 5;

    // Ctrl-K: kill line to end (kills " world")
    try killLineToEnd(&state);
    try testing.expectEqualStrings("hello", state.line_buffer.items);
    try testing.expectEqualStrings(" world", kill_ring.?);

    // Ctrl-Y: yank/paste at cursor
    try yank(&state);
    try testing.expectEqualStrings("hello world", state.line_buffer.items);
    try testing.expectEqual(@as(usize, 11), state.col_offset);

    // Ctrl-W: delete word backward (deletes "world")
    try deleteWordBackward(&state);
    try testing.expectEqualStrings("hello ", state.line_buffer.items);
    try testing.expectEqualStrings("world", kill_ring.?);

    // Test that Ctrl-W stops at parentheses and punctuation
    try state.line_buffer.appendSlice(testing.allocator, "foo(bar_baz)");
    state.col_offset = 18; // pointing to end of line: "hello foo(bar_baz)"

    // Ctrl-W: should delete "bar_baz)" (word: "bar_baz", non-word: ")")
    try deleteWordBackward(&state);
    try testing.expectEqualStrings("hello foo(", state.line_buffer.items);
    try testing.expectEqualStrings("bar_baz)", kill_ring.?);

    // Ctrl-W: should delete "foo(" (word: "foo", non-word: "(")
    try deleteWordBackward(&state);
    try testing.expectEqualStrings("hello ", state.line_buffer.items);
    try testing.expectEqualStrings("foo(", kill_ring.?);

    // Ctrl-U: delete to start (deletes "hello ")
    try deleteToStart(&state);
    try testing.expectEqualStrings("", state.line_buffer.items);
    try testing.expectEqualStrings("hello ", kill_ring.?);

    // Reset buffer to "hello foo(bar_baz)" for Alt-B, Alt-F, Alt-D testing
    state.line_buffer.clearRetainingCapacity();
    try state.line_buffer.appendSlice(testing.allocator, "hello foo(bar_baz)");
    state.col_offset = 0;

    // Alt-F (moveWordRight)
    try moveWordRight(&state); // moves past "hello" -> 5 (space before foo)
    try testing.expectEqual(@as(usize, 5), state.col_offset);

    try moveWordRight(&state); // moves past "foo" -> 9 (parenthesis '(')
    try testing.expectEqual(@as(usize, 9), state.col_offset);

    try moveWordRight(&state); // moves past "bar_baz" -> 17 (parenthesis ')')
    try testing.expectEqual(@as(usize, 17), state.col_offset);

    // Alt-B (moveWordLeft)
    try moveWordLeft(&state); // moves to start of "bar_baz" -> 10
    try testing.expectEqual(@as(usize, 10), state.col_offset);

    try moveWordLeft(&state); // moves to start of "foo" -> 6
    try testing.expectEqual(@as(usize, 6), state.col_offset);

    // Alt-D (killWordForward)
    // currently at "hello foo(bar_baz)" with col_offset at 6 (start of "foo")
    try killWordForward(&state); // should delete "foo" (word: "foo", non-word: "(" stops it)
    try testing.expectEqualStrings("hello (bar_baz)", state.line_buffer.items);
    try testing.expectEqualStrings("foo", kill_ring.?);
    try testing.expectEqual(@as(usize, 6), state.col_offset);
}

/// Minimal `LineState` for exercising the keypress path in tests.
fn testLineState(out: *std.Io.Writer, kill_ring: *?[]const u8, replay_index: *?usize) LineState {
    return .{
        .outlive = testing.allocator,
        .temp = testing.allocator,
        .prompt = "» ",
        .prompt_width = 2,
        .out = out,
        .in = undefined,
        .col_offset = 0,
        .line_buffer = std.ArrayList(u8).empty,
        .bytes_read = 0,
        .in_buffer = undefined,
        .history = undefined,
        .history_index = null,
        .transient_line = null,
        .kill_ring = kill_ring,
        .replay_index = replay_index,
        .ctrl_c_armed = false,
    };
}

/// Feeds bytes through the same parser and command dispatch used by `helper`.
fn typeBytes(state: *LineState, bytes: []const u8) CommandError!void {
    var parser = InputParser{};
    var events = std.ArrayList(InputEvent).empty;
    defer events.deinit(testing.allocator);

    for (bytes) |byte| {
        events.clearRetainingCapacity();
        try parser.feed(&.{byte}, &events, testing.allocator);
        for (events.items) |event| switch (event) {
            .byte => |b| {
                state.in_buffer[0] = b;
                state.bytes_read = 1;
                try findCommandFn(state)(state);
            },
            .codepoint => |codepoint| try printCodepoint(state, codepoint),
            .esc2, .csi3, .paste_start, .paste_byte, .paste_end => unreachable,
        };
    }
    std.debug.assert(parser.carry_len == 0);
}

/// Returns the last zero-based absolute cursor column emitted in `out`.
fn lastCursorColumn(out: []const u8) ?usize {
    var result: ?usize = null;
    var index: usize = 0;
    while (std.mem.findPos(u8, out, index, "\x1b[")) |start| {
        const digits_start = start + 2;
        var end = digits_start;
        while (end < out.len and std.ascii.isDigit(out[end])) : (end += 1) {}
        if (end > digits_start and end < out.len and out[end] == 'G') {
            const one_based = std.fmt.parseInt(usize, out[digits_start..end], 10) catch break;
            result = one_based -| 1;
        }
        index = digits_start;
    }
    return result;
}

test "typing a multi-byte UTF-8 character inserts the whole character" {
    // Regression test for https://github.com/roc-lang/roc/issues/10743
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |text| testing.allocator.free(text);
    var replay_index: ?usize = null;
    var state = testLineState(&aw.writer, &kill_ring, &replay_index);
    defer state.line_buffer.deinit(testing.allocator);

    try typeBytes(&state, "café");

    try testing.expectEqualStrings("café", state.line_buffer.items);
    try testing.expectEqual(@as(?usize, 6), lastCursorColumn(aw.writer.buffered()));
}

test "left arrow steps over a whole multi-byte UTF-8 character" {
    // Regression test for https://github.com/roc-lang/roc/issues/10743
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |text| testing.allocator.free(text);
    var replay_index: ?usize = null;
    var state = testLineState(&aw.writer, &kill_ring, &replay_index);
    defer state.line_buffer.deinit(testing.allocator);

    try state.line_buffer.appendSlice(testing.allocator, "aéb");
    try moveCursorToEnd(&state);
    try moveCursorLeft(&state);
    try moveCursorLeft(&state);
    try typeBytes(&state, "X");

    try testing.expectEqualStrings("aXéb", state.line_buffer.items);
    try testing.expectEqual(@as(?usize, 4), lastCursorColumn(aw.writer.buffered()));
}

test "cursor movement and backspace operate on grapheme clusters" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |text| testing.allocator.free(text);
    var replay_index: ?usize = null;
    var state = testLineState(&aw.writer, &kill_ring, &replay_index);
    defer state.line_buffer.deinit(testing.allocator);

    try state.line_buffer.appendSlice(testing.allocator, "ae\u{301}b");
    try moveCursorToEnd(&state);
    try moveCursorLeft(&state);
    try deleteBefore(&state);

    try testing.expectEqualStrings("ab", state.line_buffer.items);
    try testing.expectEqual(@as(?usize, 3), lastCursorColumn(aw.writer.buffered()));
}

test "edits that join adjacent graphemes keep the cursor on a boundary" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |text| testing.allocator.free(text);
    var replay_index: ?usize = null;
    var state = testLineState(&aw.writer, &kill_ring, &replay_index);
    defer state.line_buffer.deinit(testing.allocator);

    try state.line_buffer.appendSlice(testing.allocator, "👩👩");
    state.col_offset = "👩".len;
    try printCodepoint(&state, 0x200d);

    try testing.expectEqualStrings("👩‍👩", state.line_buffer.items);
    try testing.expectEqual(state.line_buffer.items.len, state.col_offset);
    try moveCursorLeft(&state);
    try testing.expectEqual(@as(usize, 0), state.col_offset);
    try moveCursorRight(&state);
    try testing.expectEqual(state.line_buffer.items.len, state.col_offset);
}

test "wide characters and Unicode paste use terminal-cell columns" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |text| testing.allocator.free(text);
    var replay_index: ?usize = null;
    var state = testLineState(&aw.writer, &kill_ring, &replay_index);
    defer state.line_buffer.deinit(testing.allocator);

    try testing.expect(!try insertPaste(&state, "a界é"));
    try testing.expectEqualStrings("a界é", state.line_buffer.items);
    try testing.expectEqual(@as(?usize, 6), lastCursorColumn(aw.writer.buffered()));

    try moveCursorLeft(&state);
    try moveCursorLeft(&state);
    try testing.expectEqual(@as(?usize, 3), lastCursorColumn(aw.writer.buffered()));
}

test "invalid UTF-8 paste is rejected without mutating the line" {
    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();
    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |text| testing.allocator.free(text);
    var replay_index: ?usize = null;
    var state = testLineState(&aw.writer, &kill_ring, &replay_index);
    defer state.line_buffer.deinit(testing.allocator);

    try state.line_buffer.appendSlice(testing.allocator, "ok");
    try moveCursorToEnd(&state);
    try testing.expectError(error.InvalidUtf8, insertPaste(&state, "\xff"));
    try testing.expectEqualStrings("ok", state.line_buffer.items);
    try testing.expectEqual(@as(usize, 2), state.col_offset);
}

test "InputParser: 2-byte ESC sequence" {
    var parser = InputParser{};
    var events = try collectEvents(&parser, &.{"\x1bb\x1bf\x1bd"});
    defer events.deinit(testing.allocator);
    try expectEventsEqual(&.{
        .{ .esc2 = .{ 0x1b, 'b' } },
        .{ .esc2 = .{ 0x1b, 'f' } },
        .{ .esc2 = .{ 0x1b, 'd' } },
    }, events.items);
}

test "History: basic appending and deduplication" {
    var history = History.init(testing.allocator);
    defer history.deinit();

    try history.append("x = 1");
    try history.append("y = 2");
    try history.append("y = 2"); // should be ignored as consecutive duplicate

    try testing.expectEqual(@as(usize, 2), history.entries.items.len);
    try testing.expectEqualStrings("x = 1", history.entries.items[0]);
    try testing.expectEqualStrings("y = 2", history.entries.items[1]);
}

test "History: invalid UTF-8 is rejected" {
    var history = History.init(testing.allocator);
    defer history.deinit();

    try testing.expectError(error.InvalidUtf8, history.append("\xff"));
    try testing.expectEqual(@as(usize, 0), history.entries.items.len);
}

test "History: transient line preservation" {
    var history = History.init(testing.allocator);
    defer history.deinit();

    try history.append("first command");
    try history.append("second command");

    var arena = base.SingleThreadArena.init(testing.allocator);
    defer arena.deinit();
    const temp = arena.allocator();

    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();

    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |k| testing.allocator.free(k);

    var dummy_replay_index: ?usize = null;
    var state = LineState{
        .outlive = testing.allocator,
        .temp = temp,
        .prompt = "> ",
        .prompt_width = 2,
        .out = &aw.writer,
        .in = undefined, // not used in history functions
        .col_offset = 0,
        .line_buffer = std.ArrayList(u8).empty,
        .bytes_read = 0,
        .in_buffer = undefined,
        .history = &history,
        .history_index = null,
        .transient_line = null,
        .kill_ring = &kill_ring,
        .replay_index = &dummy_replay_index,
        .ctrl_c_armed = false,
    };

    // Simulate typing a transient command "third draft"
    try state.line_buffer.appendSlice(temp, "third draft");
    state.col_offset = 11;

    // Navigate back to history (second command)
    try historyBackward(&state);
    try testing.expectEqualStrings("second command", state.line_buffer.items);
    try testing.expectEqualStrings("third draft", state.transient_line.?);
    try testing.expectEqual(@as(?usize, 1), state.history_index);

    // Navigate further back (first command)
    try historyBackward(&state);
    try testing.expectEqualStrings("first command", state.line_buffer.items);
    try testing.expectEqual(@as(?usize, 0), state.history_index);

    // Navigate forward (second command)
    try historyForward(&state);
    try testing.expectEqualStrings("second command", state.line_buffer.items);
    try testing.expectEqual(@as(?usize, 1), state.history_index);

    // Navigate past the end (restores transient command "third draft")
    try historyForward(&state);
    try testing.expectEqualStrings("third draft", state.line_buffer.items);
    try testing.expect(state.history_index == null);
}

test "History: replay index" {
    var history = History.init(testing.allocator);
    defer history.deinit();

    try history.append("cmd 0");
    try history.append("cmd 1");
    try history.append("cmd 2");

    var arena = base.SingleThreadArena.init(testing.allocator);
    defer arena.deinit();
    const temp = arena.allocator();

    var aw: std.Io.Writer.Allocating = .init(testing.allocator);
    defer aw.deinit();

    var kill_ring: ?[]const u8 = null;
    defer if (kill_ring) |k| testing.allocator.free(k);

    var replay_index: ?usize = null;
    var state = LineState{
        .outlive = testing.allocator,
        .temp = temp,
        .prompt = "> ",
        .prompt_width = 2,
        .out = &aw.writer,
        .in = undefined,
        .col_offset = 0,
        .line_buffer = std.ArrayList(u8).empty,
        .bytes_read = 0,
        .in_buffer = undefined,
        .history = &history,
        .history_index = null,
        .transient_line = null,
        .kill_ring = &kill_ring,
        .replay_index = &replay_index,
        .ctrl_c_armed = false,
    };
    defer state.line_buffer.deinit(temp);

    // Navigate backward twice
    try historyBackward(&state);
    try historyBackward(&state);

    try testing.expectEqual(@as(?usize, 1), state.history_index);

    // Accepting a recalled line records the following entry for the next prompt.
    try testing.expectError(error.NewLine, acceptLine(&state));
    try testing.expectEqual(@as(?usize, 2), replay_index);

    // Initialize fresh state for the next prompt
    var state2 = LineState{
        .outlive = testing.allocator,
        .temp = temp,
        .prompt = "> ",
        .prompt_width = 2,
        .out = &aw.writer,
        .in = undefined,
        .col_offset = 0,
        .line_buffer = std.ArrayList(u8).empty,
        .bytes_read = 0,
        .in_buffer = undefined,
        .history = &history,
        .history_index = null,
        .transient_line = null,
        .kill_ring = &kill_ring,
        .replay_index = &replay_index,
        .ctrl_c_armed = false,
    };
    defer state2.line_buffer.deinit(temp);

    // Pressing DOWN on the next blank prompt recalls that following entry.
    try historyForward(&state2);
    try testing.expectEqualStrings("cmd 2", state2.line_buffer.items);
    try testing.expectEqual(@as(?usize, 2), state2.history_index);
    try testing.expect(state2.replay_index.* == null);

    // Editing a recalled entry detaches it from history, so accepting the
    // edited line does not continue replaying the old sequence.
    try deleteBefore(&state2);
    try testing.expect(state2.history_index == null);
    try testing.expectError(error.NewLine, acceptLine(&state2));
    try testing.expect(state2.replay_index.* == null);
}

test "History: multiline command split" {
    var history = History.init(testing.allocator);
    defer history.deinit();

    try history.append("line A\nline B\r\nline C");

    try testing.expectEqual(@as(usize, 3), history.entries.items.len);
    try testing.expectEqualStrings("line A", history.entries.items[0]);
    try testing.expectEqualStrings("line B", history.entries.items[1]);
    try testing.expectEqualStrings("line C", history.entries.items[2]);
}
