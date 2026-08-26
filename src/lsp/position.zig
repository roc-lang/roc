//! Helpers for converting between byte offsets and LSP line/character positions.

const std = @import("std");
const document_symbol_handler = @import("handlers/document_symbol.zig");
const can = @import("can");
const ModuleEnv = can.ModuleEnv;
const Allocator = std.mem.Allocator;

/// Dynamically-allocated table of byte offsets for line starts in a source buffer.
pub const LineOffsets = struct {
    offsets: []u32,
    allocator: Allocator,
    /// The text the offsets describe, needed to count UTF-16 columns.
    source: []const u8,

    pub fn deinit(self: *const LineOffsets) void {
        self.allocator.free(self.offsets);
    }

    /// Which line contains the given byte offset.
    ///
    /// Callers convert many offsets against one table—every occurrence of a
    /// symbol, every foldable region—so this binary searches rather than
    /// scanning from the top of the file for each one.
    pub fn lineAt(self: *const LineOffsets, offset: u32) u32 {
        var low: usize = 0;
        var high: usize = self.offsets.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.offsets[mid] <= offset) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        // `low` is the first line starting after the offset.
        return if (low > 0) @intCast(low - 1) else 0;
    }

    /// The byte offset of a line/character position, or null past the last line.
    pub fn offsetAt(self: *const LineOffsets, line: u32, character: u32) ?u32 {
        if (line >= self.offsets.len) return null;
        return self.offsets[line] + character;
    }
};

/// Build line-start byte offsets for a source buffer.
pub fn buildLineOffsets(allocator: Allocator, source: []const u8) Allocator.Error!LineOffsets {
    // Count newlines first to allocate exactly the right size.
    var count: usize = 1; // line 0 always starts at offset 0
    for (source) |c| {
        if (c == '\n') count += 1;
    }

    const offsets = try allocator.alloc(u32, count);
    errdefer allocator.free(offsets);

    offsets[0] = 0;
    var idx: usize = 1;
    for (source, 0..) |c, i| {
        if (c == '\n') {
            offsets[idx] = @intCast(i + 1);
            idx += 1;
        }
    }

    return .{
        .offsets = offsets,
        .allocator = allocator,
        .source = source,
    };
}

/// Whether the text is plain ASCII, in which case UTF-8 bytes and UTF-16 code
/// units count the same and no conversion is needed.
fn isAscii(text: []const u8) bool {
    for (text) |byte| {
        if (byte >= 0x80) return false;
    }
    return true;
}

/// The UTF-16 column of a byte offset inside one line.
///
/// LSP counts columns in UTF-16 code units. The compiler counts bytes. The two
/// agree until a line contains a character outside ASCII, after which every
/// column to its right differs.
pub fn byteOffsetToUtf16Column(line_text: []const u8, byte_offset: u32) u32 {
    const upto = line_text[0..@min(byte_offset, line_text.len)];
    if (isAscii(upto)) return @intCast(upto.len);

    var units: u32 = 0;
    var index: usize = 0;
    while (index < upto.len) {
        const sequence = utf8SequenceAt(upto, index);
        units += if (sequence.codepoint) |codepoint|
            (if (codepoint <= 0xFFFF) @as(u32, 1) else 2)
        else
            // Not valid UTF-8; count the bytes so the column stays monotonic.
            @as(u32, @intCast(sequence.len));
        index += sequence.len;
    }
    return units;
}

/// One UTF-8 sequence, as far as the bytes allow it to be read.
const Utf8Sequence = struct {
    /// How many bytes to step over to reach the next sequence. Never zero, so
    /// a scan always makes progress.
    len: usize,
    /// What the bytes decode to, or null when they decode to nothing.
    codepoint: ?u21,
};

/// Read the UTF-8 sequence at `index` without trusting the bytes there.
///
/// `std.unicode.Utf8Iterator` may only be pointed at text already known to be
/// valid: it reaches `unreachable` on a byte that starts no sequence, and slices
/// past the end when the last sequence is cut short. Source arrives here
/// unvalidated—the tokenizer reports `InvalidUtf8InSource` as a diagnostic and
/// carries on, so a document being edited holds whatever bytes it holds—which
/// makes both of those ordinary inputs rather than impossible ones.
fn utf8SequenceAt(text: []const u8, index: usize) Utf8Sequence {
    const sequence_len = std.unicode.utf8ByteSequenceLength(text[index]) catch
        return .{ .len = 1, .codepoint = null };
    if (index + sequence_len > text.len) return .{ .len = text.len - index, .codepoint = null };
    const codepoint = std.unicode.utf8Decode(text[index..][0..sequence_len]) catch
        return .{ .len = sequence_len, .codepoint = null };
    return .{ .len = sequence_len, .codepoint = codepoint };
}

/// How a column that does not land on a character boundary is treated.
pub const Landing = enum {
    /// Round to the start of the next character. A query asking about a caret
    /// between surrogates still means the character it sits in.
    nearest,
    /// Reject it. Applying an edit at a position the client miscounted would
    /// corrupt the document, so `didChange` insists on an exact landing.
    exact,
};

/// The byte offset inside one line of a UTF-16 column.
///
/// Returns null when the column runs past the end of the line, or, under
/// `.exact`, when it falls inside a character.
pub fn utf16ColumnToByteOffset(line_text: []const u8, character: usize, landing: Landing) ?usize {
    if (isAscii(line_text)) {
        if (character > line_text.len) return null;
        return character;
    }

    var units: usize = 0;
    var index: usize = 0;
    while (units < character) {
        if (index >= line_text.len) return null;
        const sequence = utf8SequenceAt(line_text, index);
        const cp = sequence.codepoint orelse {
            if (landing == .exact) return null;
            units += sequence.len;
            index += sequence.len;
            continue;
        };
        units += if (cp <= 0xFFFF) @as(usize, 1) else 2;
        index += sequence.len;
    }
    if (landing == .exact and units != character) return null;
    return index;
}

/// A line's text without its terminating EOL sequence.
///
/// LSP columns count within a line's content, which stops before the EOL, so
/// both the `\n` and the `\r` of a `\r\n` pair are dropped.
pub fn trimEol(slice: []const u8) []const u8 {
    var trimmed = slice;
    if (trimmed.len > 0 and trimmed[trimmed.len - 1] == '\n') trimmed = trimmed[0 .. trimmed.len - 1];
    if (trimmed.len > 0 and trimmed[trimmed.len - 1] == '\r') trimmed = trimmed[0 .. trimmed.len - 1];
    return trimmed;
}

/// The text of one line, without its terminating newline.
pub fn lineText(source: []const u8, line_starts: []const u32, line: u32) ?[]const u8 {
    if (line >= line_starts.len) return null;
    const start = line_starts[line];
    if (start > source.len) return null;
    const end = if (line + 1 < line_starts.len) line_starts[line + 1] else @as(u32, @intCast(source.len));
    return trimEol(source[start..@min(end, source.len)]);
}

/// Convert a byte offset into an LSP line/character position using cached line offsets.
pub fn offsetToPosition(offset: u32, line_offsets: *const LineOffsets) document_symbol_handler.Position {
    const line = line_offsets.lineAt(offset);
    const text = lineText(line_offsets.source, line_offsets.offsets, line) orelse return .{
        .line = line,
        .character = 0,
    };
    return .{
        .line = line,
        .character = byteOffsetToUtf16Column(text, offset - line_offsets.offsets[line]),
    };
}

/// Convert an LSP line/character position to a byte offset in the module source.
///
/// `character` is a UTF-16 code unit count, as the protocol specifies and as
/// the server advertises with `positionEncoding`.
pub fn positionToOffset(module_env: *ModuleEnv, line: u32, character: u32) ?u32 {
    const line_starts = module_env.getLineStartsAll();
    if (line >= line_starts.len) return null;

    const text = lineText(module_env.common.source, line_starts, line) orelse return null;
    const column = utf16ColumnToByteOffset(text, character, .nearest) orelse return null;
    return line_starts[line] + @as(u32, @intCast(column));
}
