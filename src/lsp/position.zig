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

    pub fn deinit(self: *const LineOffsets) void {
        self.allocator.free(self.offsets);
    }

    /// Which line contains the given byte offset.
    ///
    /// Callers convert many offsets against one table — every occurrence of a
    /// symbol, every foldable region — so this binary searches rather than
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
    };
}

/// Convert a byte offset into an LSP line/character position using cached line offsets.
pub fn offsetToPosition(offset: u32, line_offsets: *const LineOffsets) document_symbol_handler.Position {
    const line = line_offsets.lineAt(offset);
    return .{
        .line = line,
        .character = offset - line_offsets.offsets[line],
    };
}

/// Convert an LSP line/character position to a byte offset in the module source.
pub fn positionToOffset(module_env: *ModuleEnv, line: u32, character: u32) ?u32 {
    const line_starts = module_env.getLineStartsAll();
    if (line >= line_starts.len) return null;

    const line_start = line_starts[line];
    // For simplicity, treat character as byte offset within line
    // (proper UTF-16 handling would require more work)
    return line_start + character;
}
