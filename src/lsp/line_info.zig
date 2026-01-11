//! Line information utilities for converting between byte offsets and line/column positions.
//! Used by semantic tokens to encode positions in the LSP delta format.

const std = @import("std");

/// Tracks line start offsets for efficient byte-to-position conversion.
pub const LineInfo = struct {
    /// Byte offsets where each line starts. Index 0 is always 0.
    line_starts: []const u32,
    allocator: std.mem.Allocator,

    /// Position in a document (0-indexed).
    pub const Position = struct {
        line: u32,
        character: u32,
    };

    /// Creates LineInfo by scanning the source for newline characters.
    pub fn init(allocator: std.mem.Allocator, source: []const u8) !LineInfo {
        const line_starts = try computeLineStarts(allocator, source);
        return .{
            .line_starts = line_starts,
            .allocator = allocator,
        };
    }

    /// Releases the line_starts array.
    pub fn deinit(self: *LineInfo) void {
        self.allocator.free(self.line_starts);
        self.* = undefined;
    }

    /// Converts a byte offset to a line/column position.
    /// Returns null if the offset is out of bounds.
    pub fn positionFromOffset(self: *const LineInfo, offset: u32) ?Position {
        if (self.line_starts.len == 0) return null;

        // Binary search to find the line containing this offset
        const line = self.findLine(offset);
        const line_start = self.line_starts[line];

        // Character is the offset from the start of the line
        const character = offset - line_start;

        return .{
            .line = @intCast(line),
            .character = character,
        };
    }

    /// Converts a line/column position back to a byte offset.
    /// Returns null if the position is invalid.
    pub fn offsetFromPosition(self: *const LineInfo, pos: Position) ?u32 {
        if (pos.line >= self.line_starts.len) return null;
        return self.line_starts[pos.line] + pos.character;
    }

    /// Binary search to find which line contains the given offset.
    fn findLine(self: *const LineInfo, offset: u32) usize {
        var low: usize = 0;
        var high: usize = self.line_starts.len;

        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.line_starts[mid] <= offset) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }

        // low is now the first line that starts AFTER offset, so we want low - 1
        return if (low > 0) low - 1 else 0;
    }
};

/// Computes an array of byte offsets where each line starts.
/// Line 0 always starts at offset 0.
pub fn computeLineStarts(allocator: std.mem.Allocator, source: []const u8) ![]u32 {
    // Count newlines first to allocate exact size
    var count: usize = 1; // Line 0 always exists
    for (source) |c| {
        if (c == '\n') count += 1;
    }

    var line_starts = try allocator.alloc(u32, count);
    errdefer allocator.free(line_starts);

    line_starts[0] = 0;
    var line_idx: usize = 1;
    for (source, 0..) |c, i| {
        if (c == '\n') {
            line_starts[line_idx] = @intCast(i + 1);
            line_idx += 1;
        }
    }

    return line_starts;
}
