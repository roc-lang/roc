//! Borrowed conversion between compiler byte offsets and LSP UTF-16 positions.

const position = @import("position.zig");

pub const Position = struct {
    line: u32,
    character: u32,
};

pub const Range = struct {
    start: Position,
    end: Position,
};

pub const LineIndex = struct {
    source: []const u8,
    line_starts: []const u32,

    pub fn fromLineOffsets(offsets: *const position.LineOffsets) LineIndex {
        return .{ .source = offsets.source, .line_starts = offsets.offsets };
    }

    pub fn utf16ToByte(self: *const LineIndex, line: u32, character: u32, landing: position.Landing) ?u32 {
        if (line >= self.line_starts.len) return null;
        const line_text = position.lineText(self.source, self.line_starts, line) orelse return null;
        const column = position.utf16ColumnToByteOffset(line_text, character, landing, .clamp) orelse return null;
        return self.line_starts[line] + @as(u32, @intCast(column));
    }

    pub fn byteToUtf16(self: *const LineIndex, offset: u32) ?Position {
        if (@as(usize, offset) > self.source.len) return null;
        const line = self.lineForOffset(offset) orelse return null;
        const line_text = position.lineText(self.source, self.line_starts, @intCast(line)) orelse return null;
        const line_start = self.line_starts[line];
        return .{
            .line = @intCast(line),
            .character = position.byteOffsetToUtf16Column(line_text, offset -| line_start),
        };
    }

    pub fn rangeFromBytes(self: *const LineIndex, start: u32, end: u32) ?Range {
        if (start > end) return null;
        return .{
            .start = self.byteToUtf16(start) orelse return null,
            .end = self.byteToUtf16(end) orelse return null,
        };
    }

    /// Return the UTF-16 length of a range contained in one logical line.
    pub fn utf16Length(self: *const LineIndex, start: u32, end: u32) ?u32 {
        if (start > end) return null;
        const start_position = self.byteToUtf16(start) orelse return null;
        const end_position = self.byteToUtf16(end) orelse return null;
        if (start_position.line != end_position.line) return null;
        return end_position.character - start_position.character;
    }

    fn lineForOffset(self: *const LineIndex, offset: u32) ?usize {
        if (self.line_starts.len == 0) return null;

        var low: usize = 0;
        var high: usize = self.line_starts.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.line_starts[mid] <= offset) low = mid + 1 else high = mid;
        }
        return if (low == 0) null else low - 1;
    }
};
