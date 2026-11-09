//! Handler for LSP `textDocument/completion` requests.
//!
//! Provides code completion suggestions including top-level definitions,
//! imported module exports, and local variables in scope.

const std = @import("std");
const base = @import("base");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const syntax = @import("../syntax.zig");
const position_params = @import("position_params.zig");

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
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const position = try position_params.parse(self, id, "completion", maybe_params) orelse return;
            const uri = position.uri;
            const line = position.line;
            const character = position.character;

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
                std.log.err("completion failed: {s}", .{try syntax.queryFailureName(err)});
                // Return empty completion list on error
                const CompletionResponse = struct {
                    isIncomplete: bool = false,
                    items: []const CompletionItem = &.{},
                };
                try self.sendResponse(id, CompletionResponse{});
                return;
            };

            if (completion_result) |result| {
                defer result.deinit();

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
/// Its strings are owned by the `CompletionResult` holding it.
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
    /// Owns `items` and every string in them.
    arena: base.SingleThreadArena,

    pub fn deinit(self: CompletionResult) void {
        self.arena.deinit();
    }
};
