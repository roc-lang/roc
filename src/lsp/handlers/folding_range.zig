//! Handler for LSP `textDocument/foldingRange` requests.
//!
//! Provides code folding ranges for the editor.

const std = @import("std");
const protocol = @import("../protocol.zig");
const parse = @import("parse");
const can = @import("can");

const Token = parse.tokenize.Token;

/// Handler for `textDocument/foldingRange` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "foldingRange requires params");
                return;
            };

            const obj = switch (params) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "foldingRange params must be an object");
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

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else {
                try self.sendResponse(id, &[_]FoldingRange{});
                return;
            };

            // Extract folding ranges from the document
            const ranges = extractFoldingRanges(self.allocator, text) catch |err| {
                std.log.err("folding range extraction failed: {s}", .{@errorName(err)});
                try self.sendResponse(id, &[_]FoldingRange{});
                return;
            };
            defer self.allocator.free(ranges);

            try self.sendResponse(id, ranges);
        }
    };
}

const FoldingRange = struct {
    startLine: u32,
    endLine: u32,
    kind: ?[]const u8 = null,
};

/// Extract folding ranges from source code by finding matching brackets.
fn extractFoldingRanges(allocator: std.mem.Allocator, source: []const u8) ![]FoldingRange {
    // Build line offset table
    const line_offsets = buildLineOffsets(source);

    // Track bracket positions for folding
    var ranges = std.ArrayList(FoldingRange){};
    errdefer ranges.deinit(allocator);

    // Stack to track opening bracket positions
    var bracket_stack = std.ArrayList(BracketInfo){};
    defer bracket_stack.deinit(allocator);

    // Parse to get tokens
    var module_env = can.ModuleEnv.init(allocator, source) catch {
        return &[_]FoldingRange{};
    };
    defer module_env.deinit();

    var ast = parse.parse(&module_env.common, allocator) catch {
        return &[_]FoldingRange{};
    };
    defer ast.deinit(allocator);

    const tags = ast.tokens.tokens.items(.tag);
    const regions = ast.tokens.tokens.items(.region);

    for (tags, regions) |tag, region| {
        const offset = region.start.offset;
        const line = offsetToLine(offset, &line_offsets);

        switch (tag) {
            // Opening brackets
            .OpenCurly, .OpenSquare, .OpenRound => {
                try bracket_stack.append(allocator, .{
                    .line = line,
                    .tag = tag,
                });
            },
            // Closing brackets
            .CloseCurly => {
                if (popMatchingBracket(&bracket_stack, .OpenCurly)) |open_info| {
                    if (line > open_info.line) {
                        try ranges.append(allocator, .{
                            .startLine = open_info.line,
                            .endLine = line,
                            .kind = null,
                        });
                    }
                }
            },
            .CloseSquare => {
                if (popMatchingBracket(&bracket_stack, .OpenSquare)) |open_info| {
                    if (line > open_info.line) {
                        try ranges.append(allocator, .{
                            .startLine = open_info.line,
                            .endLine = line,
                            .kind = null,
                        });
                    }
                }
            },
            .CloseRound => {
                if (popMatchingBracket(&bracket_stack, .OpenRound)) |open_info| {
                    if (line > open_info.line) {
                        try ranges.append(allocator, .{
                            .startLine = open_info.line,
                            .endLine = line,
                            .kind = null,
                        });
                    }
                }
            },
            else => {},
        }
    }

    return ranges.toOwnedSlice(allocator);
}

const BracketInfo = struct {
    line: u32,
    tag: Token.Tag,
};

fn popMatchingBracket(stack: *std.ArrayList(BracketInfo), expected_open: Token.Tag) ?BracketInfo {
    // Find and remove the most recent matching open bracket
    var i = stack.items.len;
    while (i > 0) {
        i -= 1;
        if (stack.items[i].tag == expected_open) {
            const info = stack.items[i];
            _ = stack.orderedRemove(i);
            return info;
        }
    }
    return null;
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

fn offsetToLine(offset: u32, line_offsets: *const LineOffsets) u32 {
    var line: u32 = 0;
    for (0..line_offsets.count) |i| {
        if (line_offsets.offsets[i] > offset) break;
        line = @intCast(i);
    }
    return line;
}
