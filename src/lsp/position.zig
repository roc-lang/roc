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
    var line: u32 = 0;
    for (line_offsets.offsets, 0..) |line_offset, i| {
        if (line_offset > offset) break;
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
