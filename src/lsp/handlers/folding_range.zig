//! Handler for LSP `textDocument/foldingRange` requests.
//!
//! Provides code folding ranges for the editor.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const parse = @import("parse");
const can = @import("can");
const pos = @import("../position.zig");
const Token = parse.tokenize.Token;

/// Handler for `textDocument/foldingRange` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "foldingRange requires params");
                return;
            };

            if (std.meta.activeTag(params) != .object) {
                try self.sendError(id, .invalid_params, "foldingRange params must be an object");
                return;
            }
            const obj = params.object;

            // Extract textDocument.uri
            const text_doc_value = obj.get("textDocument") orelse {
                try self.sendError(id, .invalid_params, "missing textDocument");
                return;
            };
            if (std.meta.activeTag(text_doc_value) != .object) {
                try self.sendError(id, .invalid_params, "textDocument must be an object");
                return;
            }
            const text_doc = text_doc_value.object;
            const uri_value = text_doc.get("uri") orelse {
                try self.sendError(id, .invalid_params, "missing uri");
                return;
            };
            if (std.meta.activeTag(uri_value) != .string) {
                try self.sendError(id, .invalid_params, "uri must be a string");
                return;
            }
            const uri = uri_value.string;

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else {
                try self.sendResponse(id, &[_]FoldingRange{});
                return;
            };

            // Extract folding ranges from the document
            const ranges = try extractFoldingRanges(self.allocator, text);
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
fn extractFoldingRanges(allocator: std.mem.Allocator, source: []const u8) Allocator.Error![]FoldingRange {
    // Build line offset table
    const line_offsets = try pos.buildLineOffsets(allocator, source);
    defer line_offsets.deinit();

    // Track bracket positions for folding
    var ranges: std.ArrayList(FoldingRange) = .empty;
    errdefer ranges.deinit(allocator);

    // Stack to track opening bracket positions
    var bracket_stack: std.ArrayList(BracketInfo) = .empty;
    defer bracket_stack.deinit(allocator);

    // Parse to get tokens
    var module_env = try can.ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    const ast = try parse.file(allocator, &module_env.common);
    defer ast.deinit();

    const tags = ast.tokens.tokens.items(.tag);
    const regions = ast.tokens.tokens.items(.region);

    for (tags, regions) |tag, region| {
        const offset = region.start.offset;
        const line = line_offsets.lineAt(offset);

        // Opening brackets
        if (tag == .OpenCurly or tag == .OpenSquare or tag == .OpenRound) {
            try bracket_stack.append(allocator, .{ .line = line, .tag = tag });
        } else {
            // Closing brackets
            const expected_open: ?Token.Tag = if (tag == .CloseCurly)
                .OpenCurly
            else if (tag == .CloseSquare)
                .OpenSquare
            else if (tag == .CloseRound)
                .OpenRound
            else
                null;
            if (expected_open) |expected| {
                if (popMatchingBracket(&bracket_stack, expected)) |open_info| {
                    if (line > open_info.line) {
                        try ranges.append(allocator, .{ .startLine = open_info.line, .endLine = line, .kind = null });
                    }
                }
            }
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
