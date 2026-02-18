//! Handler for LSP `textDocument/completion` requests.
//!
//! Provides code completion suggestions including top-level definitions,
//! imported module exports, and local variables in scope.

const std = @import("std");
const protocol = @import("../protocol.zig");

/// CompletionItemKind values as defined by the LSP specification.
pub const CompletionItemKind = enum(u32) {
    text = 1,
    method = 2,
    function = 3,
    constructor = 4,
    field = 5,
    variable = 6,
    class = 7,
    interface = 8,
    module = 9,
    property = 10,
    unit = 11,
    value = 12,
    @"enum" = 13,
    keyword = 14,
    snippet = 15,
    color = 16,
    file = 17,
    reference = 18,
    folder = 19,
    enum_member = 20,
    constant = 21,
    @"struct" = 22,
    event = 23,
    operator = 24,
    type_parameter = 25,
};

/// Handler for `textDocument/completion` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "completion requires params");
                return;
            };

            const obj = switch (params) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "completion params must be an object");
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

            // Extract position (line, character)
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

            const line_value = position_obj.get("line") orelse {
                try self.sendError(id, .invalid_params, "missing line");
                return;
            };
            const line: u32 = switch (line_value) {
                .integer => |i| @intCast(i),
                else => {
                    try self.sendError(id, .invalid_params, "line must be an integer");
                    return;
                },
            };

            const character_value = position_obj.get("character") orelse {
                try self.sendError(id, .invalid_params, "missing character");
                return;
            };
            const character: u32 = switch (character_value) {
                .integer => |i| @intCast(i),
                else => {
                    try self.sendError(id, .invalid_params, "character must be an integer");
                    return;
                },
            };

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else null;

            // Query the syntax checker for completions at this position
            const completion_result = self.syntax_checker.getCompletionsAtPosition(
                uri,
                text,
                line,
                character,
            ) catch |err| {
                std.log.err("completion failed: {s}", .{@errorName(err)});
                // Return empty completion list on error
                const CompletionResponse = struct {
                    isIncomplete: bool = false,
                    items: []const CompletionItem = &.{},
                };
                try self.sendResponse(id, CompletionResponse{});
                return;
            };

            if (completion_result) |result| {
                defer {
                    // Free allocated completion item fields owned by the builder.
                    for (result.items) |item| {
                        self.allocator.free(item.label);
                        if (item.detail) |d| self.allocator.free(d);
                        if (item.documentation) |documentation| self.allocator.free(documentation);
                        if (item.sortText) |sort_text| self.allocator.free(sort_text);
                        if (item.insertText) |insert_text| self.allocator.free(insert_text);
                    }
                    self.allocator.free(result.items);
                }

                // Build the completion response
                const CompletionResponse = struct {
                    isIncomplete: bool,
                    items: []const CompletionItem,
                };

                try self.sendResponse(id, CompletionResponse{
                    .isIncomplete = result.is_incomplete,
                    .items = result.items,
                });
            } else {
                // Return empty completion list
                const CompletionResponse = struct {
                    isIncomplete: bool = false,
                    items: []const CompletionItem = &.{},
                };
                try self.sendResponse(id, CompletionResponse{});
            }
        }
    };
}

/// A single completion item returned to the client.
///
/// Note: optional string fields are owned by the completion builder and must be
/// freed by the caller that consumes the result.
pub const CompletionItem = struct {
    label: []const u8,
    kind: ?u32 = null,
    detail: ?[]const u8 = null,
    documentation: ?[]const u8 = null,
    sortText: ?[]const u8 = null,
    insertText: ?[]const u8 = null,
};

/// Result of a completion query.
pub const CompletionResult = struct {
    items: []const CompletionItem,
    is_incomplete: bool,
};
