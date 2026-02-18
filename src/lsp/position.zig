//! Helpers for converting between byte offsets and LSP line/character positions.

const std = @import("std");
const document_symbol_handler = @import("handlers/document_symbol.zig");
const can = @import("can");
const ModuleEnv = can.ModuleEnv;

/// Fixed-size table of byte offsets for line starts in a source buffer.
pub const LineOffsets = struct {
    offsets: [1024]u32,
    count: usize,
};

/// Build line-start byte offsets for a source buffer.
pub fn buildLineOffsets(source: []const u8) LineOffsets {
    var result = LineOffsets{ .offsets = undefined, .count = 0 };
    result.offsets[0] = 0;
    result.count = 1;

    for (source, 0..) |c, i| {
        if (c == '\n' and result.count < 1024) {
            result.offsets[result.count] = @intCast(i + 1);
            result.count += 1;
        }
    }
    return result;
}

/// Convert a byte offset into an LSP line/character position using cached line offsets.
pub fn offsetToPosition(offset: u32, line_offsets: *const LineOffsets) document_symbol_handler.Position {
    var line: u32 = 0;
    for (0..line_offsets.count) |i| {
        if (line_offsets.offsets[i] > offset) break;
        line = @intCast(i);
    }
    const line_start = line_offsets.offsets[line];
    return .{
        .line = line,
        .character = offset - line_start,
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
