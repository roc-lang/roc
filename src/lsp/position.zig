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
    var it = std.unicode.Utf8Iterator{ .bytes = upto, .i = 0 };
    while (it.nextCodepointSlice()) |slice| {
        const cp = std.unicode.utf8Decode(slice) catch {
            // Not valid UTF-8; count the bytes so the column stays monotonic.
            units += @intCast(slice.len);
            continue;
        };
        units += if (cp <= 0xFFFF) 1 else 2;
    }
    return units;
}

/// The byte offset inside one line of a UTF-16 column.
///
/// Returns null when the column runs past the end of the line. A column that
/// falls inside a character resolves to the start of the next one, which is
/// what an editor means when it puts the caret between surrogates.
pub fn utf16ColumnToByteOffset(line_text: []const u8, character: u32) ?u32 {
    if (isAscii(line_text)) {
        if (character > line_text.len) return null;
        return character;
    }

    var units: u32 = 0;
    var it = std.unicode.Utf8Iterator{ .bytes = line_text, .i = 0 };
    while (units < character) {
        const slice = it.nextCodepointSlice() orelse return null;
        const cp = std.unicode.utf8Decode(slice) catch {
            units += @intCast(slice.len);
            continue;
        };
        units += if (cp <= 0xFFFF) 1 else 2;
    }
    return @intCast(it.i);
}

/// The text of one line, without its terminating newline.
pub fn lineText(source: []const u8, line_starts: []const u32, line: u32) ?[]const u8 {
    if (line >= line_starts.len) return null;
    const start = line_starts[line];
    if (start > source.len) return null;
    const end = if (line + 1 < line_starts.len) line_starts[line + 1] else @as(u32, @intCast(source.len));
    var slice = source[start..@min(end, source.len)];
    if (slice.len > 0 and slice[slice.len - 1] == '\n') slice = slice[0 .. slice.len - 1];
    if (slice.len > 0 and slice[slice.len - 1] == '\r') slice = slice[0 .. slice.len - 1];
    return slice;
}

/// Convert a byte offset into an LSP line/character position using cached line offsets.
pub fn offsetToPosition(offset: u32, line_offsets: *const LineOffsets) document_symbol_handler.Position {
    var line: u32 = 0;
    for (line_offsets.offsets, 0..) |line_offset, i| {
        if (line_offset > offset) break;
        line = @intCast(i);
    }
    const line_start = line_offsets.offsets[line];
    const text = lineText(line_offsets.source, line_offsets.offsets, line) orelse return .{
        .line = line,
        .character = 0,
    };
    return .{
        .line = line,
        .character = byteOffsetToUtf16Column(text, offset - line_start),
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
    const column = utf16ColumnToByteOffset(text, character) orelse return null;
    return line_starts[line] + column;
}
