//! Handler for LSP `textDocument/selectionRange` requests.
//!
//! Provides semantic selection expansion for the editor.

const std = @import("std");
const protocol = @import("../protocol.zig");
const parse = @import("parse");
const can = @import("can");

/// Handler for `textDocument/selectionRange` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "selectionRange requires params");
                return;
            };

            const obj = switch (params) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "selectionRange params must be an object");
                    return;
                },
            };

            // Extract textDocument.uri
            const text_doc_value = obj.get("textDocument") orelse {
                try self.sendError(id, .invalid_params, "missing textDocument");
                return;
            };
            const text_doc = switch (text_doc_value) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "textDocument must be an object");
                    return;
                },
            };
            const uri_value = text_doc.get("uri") orelse {
                try self.sendError(id, .invalid_params, "missing uri");
                return;
            };
            const uri = switch (uri_value) {
                .string => |s| s,
                else => {
                    try self.sendError(id, .invalid_params, "uri must be a string");
                    return;
                },
            };

            // Extract positions array
            const positions_value = obj.get("positions") orelse {
                try self.sendError(id, .invalid_params, "missing positions");
                return;
            };
            const positions = switch (positions_value) {
                .array => |a| a,
                else => {
                    try self.sendError(id, .invalid_params, "positions must be an array");
                    return;
                },
            };

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else {
                try self.sendResponse(id, &[_]?SelectionRange{});
                return;
            };

            // Process each position
            var results = std.ArrayList(?SelectionRange){};
            defer {
                // Free the linked list nodes
                for (results.items) |maybe_range| {
                    if (maybe_range) |range| {
                        freeSelectionRange(self.allocator, range);
                    }
                }
                results.deinit(self.allocator);
            }

            for (positions.items) |pos_value| {
                const pos_obj = switch (pos_value) {
                    .object => |o| o,
                    else => {
                        try results.append(self.allocator, null);
                        continue;
                    },
                };

                const line: u32 = blk: {
                    const v = pos_obj.get("line") orelse break :blk 0;
                    break :blk switch (v) {
                        .integer => |i| @intCast(i),
                        else => 0,
                    };
                };
                const character: u32 = blk: {
                    const v = pos_obj.get("character") orelse break :blk 0;
                    break :blk switch (v) {
                        .integer => |i| @intCast(i),
                        else => 0,
                    };
                };

                const selection_range = computeSelectionRange(self.allocator, text, line, character) catch null;
                try results.append(self.allocator, selection_range);
            }

            try self.sendResponse(id, results.items);
        }
    };
}

const Position = struct {
    line: u32,
    character: u32,
};

const Range = struct {
    start: Position,
    end: Position,
};

const SelectionRange = struct {
    range: Range,
    parent: ?*const SelectionRange = null,
};

fn freeSelectionRange(allocator: std.mem.Allocator, range: SelectionRange) void {
    var current: ?*const SelectionRange = range.parent;
    while (current) |r| {
        const next = r.parent;
        allocator.destroy(r);
        current = next;
    }
}

/// Compute selection range hierarchy for a position.
fn computeSelectionRange(allocator: std.mem.Allocator, source: []const u8, line: u32, character: u32) !SelectionRange {
    // Build line offset table
    const line_offsets = buildLineOffsets(source);

    // Convert position to offset
    const target_offset = positionToOffset(line, character, &line_offsets) orelse return error.InvalidPosition;

    // Parse to get tokens
    var module_env = can.ModuleEnv.init(allocator, source) catch {
        return error.ParseFailed;
    };
    defer module_env.deinit();

    var ast = parse.parse(&module_env.common, allocator) catch {
        return error.ParseFailed;
    };
    defer ast.deinit(allocator);

    const regions = ast.tokens.tokens.items(.region);

    // Find containing regions, building from smallest to largest
    var containing_regions = std.ArrayList(Range){};
    defer containing_regions.deinit(allocator);

    // First, find the token at the position
    for (regions) |region| {
        const start = region.start.offset;
        const end = region.end.offset;

        if (start <= target_offset and target_offset < end) {
            // This token contains the cursor
            const start_pos = offsetToPosition(start, &line_offsets);
            const end_pos = offsetToPosition(end, &line_offsets);
            try containing_regions.append(allocator, .{
                .start = start_pos,
                .end = end_pos,
            });
            break;
        }
    }

    // Add the whole document as the outermost range
    const doc_end = offsetToPosition(@intCast(source.len), &line_offsets);
    try containing_regions.append(allocator, .{
        .start = .{ .line = 0, .character = 0 },
        .end = doc_end,
    });

    // Build the linked list of selection ranges (innermost to outermost)
    if (containing_regions.items.len == 0) {
        return error.NoRangeFound;
    }

    // Start with the outermost range (no parent)
    var i: usize = containing_regions.items.len;
    var current_parent: ?*const SelectionRange = null;

    while (i > 1) {
        i -= 1;
        const parent_node = try allocator.create(SelectionRange);
        parent_node.* = .{
            .range = containing_regions.items[i],
            .parent = current_parent,
        };
        current_parent = parent_node;
    }

    // Return the innermost range
    return .{
        .range = containing_regions.items[0],
        .parent = current_parent,
    };
}

const LineOffsets = struct {
    offsets: [4096]u32,
    count: usize,
};

fn buildLineOffsets(source: []const u8) LineOffsets {
    var result = LineOffsets{ .offsets = undefined, .count = 0 };
    result.offsets[0] = 0;
    result.count = 1;

    for (source, 0..) |c, i| {
        if (c == '\n' and result.count < 4096) {
            result.offsets[result.count] = @intCast(i + 1);
            result.count += 1;
        }
    }
    return result;
}

fn positionToOffset(line: u32, character: u32, line_offsets: *const LineOffsets) ?u32 {
    if (line >= line_offsets.count) return null;
    return line_offsets.offsets[line] + character;
}

fn offsetToPosition(offset: u32, line_offsets: *const LineOffsets) Position {
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
