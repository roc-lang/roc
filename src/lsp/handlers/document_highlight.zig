//! Handler for LSP `textDocument/documentHighlight` requests.
//!
//! Highlights all occurrences of a symbol when cursor is on it.
//! Uses CIR for scope-aware highlighting (handles shadowing correctly).
//! Falls back to token-based matching when CIR is unavailable.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const position_params = @import("position_params.zig");
const parse = @import("parse");
const can = @import("can");
const Token = parse.tokenize.Token;
const pos = @import("../position.zig");

/// Handler for `textDocument/documentHighlight` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const position = try position_params.parse(self, id, "documentHighlight", maybe_params) orelse return;
            const uri = position.uri;
            const line = position.line;
            const character = position.character;

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else {
                try self.sendResponse(id, &[_]DocumentHighlight{});
                return;
            };

            // Try CIR-based highlighting first (scope-aware)
            const cir_highlights = self.syntax_checker.getHighlightsAtPosition(uri, text, line, character) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.AccessDenied,
                error.AntivirusInterference,
                error.BadPathName,
                error.BuiltinArtifactVersionMismatch,
                error.Canceled,
                error.CorruptArtifact,
                error.CorruptBuiltinArtifact,
                error.CorruptEmbeddedBuiltins,
                error.DeviceBusy,
                error.FileBusy,
                error.FileNotFound,
                error.FileSystem,
                error.FileTooBig,
                error.InputOutput,
                error.IsDir,
                error.NameTooLong,
                error.NetworkNotFound,
                error.NoDevice,
                error.NoSpaceLeft,
                error.NotDir,
                error.OperationUnsupported,
                error.PathAlreadyExists,
                error.PermissionDenied,
                error.PipeBusy,
                error.ProcessFdQuotaExceeded,
                error.StaleEmbeddedBuiltins,
                error.SymLinkLoop,
                error.SystemFdQuotaExceeded,
                error.SystemResources,
                error.Unexpected,
                error.UnrecognizedVolume,
                error.WriteFailed,
                => null,
            };
            if (cir_highlights) |result| {
                defer result.deinit(self.allocator);

                // Convert to DocumentHighlight array
                var highlights: std.ArrayList(DocumentHighlight) = .empty;
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
            const highlights = try findHighlightsByToken(self.allocator, text, line, character);
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
fn findHighlightsByToken(allocator: std.mem.Allocator, source: []const u8, line: u32, character: u32) Allocator.Error![]DocumentHighlight {
    // Build line offset table
    const line_offsets = try pos.buildLineOffsets(allocator, source);
    defer line_offsets.deinit();

    // Convert position to offset
    const target_offset = line_offsets.offsetAt(line, character) orelse {
        return &[_]DocumentHighlight{};
    };

    // Parse to get tokens
    var module_env = try can.ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    const ast = try parse.file(allocator, &module_env.common);
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
    var highlights: std.ArrayList(DocumentHighlight) = .empty;
    errdefer highlights.deinit(allocator);

    for (tags, regions) |tag, region| {
        if (!isIdentifierTag(tag)) continue;

        const start = region.start.offset;
        const end = region.end.offset;

        if (start >= source.len or end > source.len) continue;

        const token_text = source[start..end];
        if (std.mem.eql(u8, token_text, target_text.?)) {
            const start_pos = positionAt(start, &line_offsets);
            const end_pos = positionAt(end, &line_offsets);

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
    return tag == .LowerIdent or tag == .UpperIdent or tag == .NamedUnderscore;
}

/// Convert a byte offset into this handler's position shape.
fn positionAt(offset: u32, line_offsets: *const pos.LineOffsets) Position {
    const converted = pos.offsetToPosition(offset, line_offsets);
    return .{ .line = converted.line, .character = converted.character };
}
