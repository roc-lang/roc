//! Handler for LSP `textDocument/documentHighlight` requests.
//!
//! Highlights all occurrences of a symbol when cursor is on it.
//! Uses CIR for scope-aware highlighting (handles shadowing correctly).
//! Falls back to token-based matching when CIR is unavailable.

const std = @import("std");
const protocol = @import("../protocol.zig");
const parse = @import("parse");
const can = @import("can");
const base = @import("base");

const Allocators = base.Allocators;
const Token = parse.tokenize.Token;

/// Handler for `textDocument/documentHighlight` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "documentHighlight requires params");
                return;
            };

            const obj = switch (params) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "documentHighlight params must be an object");
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

            // Extract position
            const position_value = obj.get("position") orelse {
                try self.sendError(id, .invalid_params, "missing position");
                return;
            };
            const position_obj = switch (position_value) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "position must be an object");
                    return;
                },
            };

            const line: u32 = blk: {
                const v = position_obj.get("line") orelse break :blk 0;
                break :blk switch (v) {
                    .integer => |i| @intCast(i),
                    else => 0,
                };
            };
            const character: u32 = blk: {
                const v = position_obj.get("character") orelse break :blk 0;
                break :blk switch (v) {
                    .integer => |i| @intCast(i),
                    else => 0,
                };
            };

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else {
                try self.sendResponse(id, &[_]DocumentHighlight{});
                return;
            };

            // Try CIR-based highlighting first (scope-aware)
            if (self.syntax_checker.getHighlightsAtPosition(uri, text, line, character) catch null) |result| {
                defer result.deinit(self.allocator);

                // Convert to DocumentHighlight array
                var highlights = std.ArrayList(DocumentHighlight){};
                defer highlights.deinit(self.allocator);

                for (result.regions) |range| {
                    try highlights.append(self.allocator, .{
                        .range = .{
                            .start = .{ .line = range.start_line, .character = range.start_col },
                            .end = .{ .line = range.end_line, .character = range.end_col },
                        },
                        .kind = .text,
                    });
                }

                try self.sendResponse(id, highlights.items);
                return;
            }

            // Fall back to token-based highlighting
            const highlights = findHighlightsByToken(self.allocator, text, line, character) catch |err| {
                std.log.err("document highlight failed: {s}", .{@errorName(err)});
                try self.sendResponse(id, &[_]DocumentHighlight{});
                return;
            };
            defer self.allocator.free(highlights);

            try self.sendResponse(id, highlights);
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

/// LSP DocumentHighlightKind
const HighlightKind = enum(u32) {
    text = 1,
    read = 2,
    write = 3,
};

const DocumentHighlight = struct {
    range: Range,
    kind: ?HighlightKind = null,
};

/// Fallback: find all highlights by matching token text.
/// Used when CIR is not available (e.g., parse errors).
fn findHighlightsByToken(allocator: std.mem.Allocator, source: []const u8, line: u32, character: u32) ![]DocumentHighlight {
    // Build line offset table
    const line_offsets = buildLineOffsets(source);

    // Convert position to offset
    const target_offset = positionToOffset(line, character, &line_offsets) orelse {
        return &[_]DocumentHighlight{};
    };

    // Parse to get tokens
    var module_env = can.ModuleEnv.init(allocator, source) catch {
        return &[_]DocumentHighlight{};
    };
    defer module_env.deinit();

    var allocators: Allocators = undefined;
    allocators.initInPlace(allocator);
    defer allocators.deinit();

    const ast = parse.parse(&allocators, &module_env.common) catch {
        return &[_]DocumentHighlight{};
    };
    defer ast.deinit();

    const tags = ast.tokens.tokens.items(.tag);
    const regions = ast.tokens.tokens.items(.region);

    // Find the token at the cursor position
    var target_text: ?[]const u8 = null;

    for (tags, regions) |tag, region| {
        const start = region.start.offset;
        const end = region.end.offset;

        if (start <= target_offset and target_offset < end) {
            // Only highlight identifiers
            if (isIdentifierTag(tag)) {
                if (start < source.len and end <= source.len) {
                    target_text = source[start..end];
                }
            }
            break;
        }
    }

    // If no identifier found, return empty
    if (target_text == null) {
        return &[_]DocumentHighlight{};
    }

    // Find all occurrences of the same identifier text
    var highlights = std.ArrayList(DocumentHighlight){};
    errdefer highlights.deinit(allocator);

    for (tags, regions) |tag, region| {
        if (!isIdentifierTag(tag)) continue;

        const start = region.start.offset;
        const end = region.end.offset;

        if (start >= source.len or end > source.len) continue;

        const token_text = source[start..end];
        if (std.mem.eql(u8, token_text, target_text.?)) {
            const start_pos = offsetToPosition(start, &line_offsets);
            const end_pos = offsetToPosition(end, &line_offsets);

            try highlights.append(allocator, .{
                .range = .{
                    .start = start_pos,
                    .end = end_pos,
                },
                .kind = .text,
            });
        }
    }

    return highlights.toOwnedSlice(allocator);
}

fn isIdentifierTag(tag: Token.Tag) bool {
    return switch (tag) {
        .LowerIdent, .UpperIdent, .NamedUnderscore => true,
        else => false,
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
