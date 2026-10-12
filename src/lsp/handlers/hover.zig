//! Handler for LSP `textDocument/hover` requests.
//!
//! Provides type information when the user hovers over an expression.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const syntax = @import("../syntax.zig");
const position_params = @import("position_params.zig");

/// Handler for `textDocument/hover` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const position = try position_params.parse(self, id, "hover", maybe_params) orelse return;
            const uri = position.uri;
            const line = position.line;
            const character = position.character;

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else null;

            // Query the syntax checker for type information at this position
            const hover_result = self.syntax_checker.getTypeAtPosition(
                uri,
                text,
                line,
                character,
            ) catch |err| {
                std.log.err("hover failed: {s}", .{try syntax.queryFailureName(err)});
                try self.sendNullResponse(id);
                return;
            };

            if (hover_result) |result| {
                defer self.allocator.free(result.type_str);

                // Build the hover response
                const HoverResponse = struct {
                    contents: struct {
                        kind: []const u8,
                        value: []const u8,
                    },
                    range: ?struct {
                        start: struct { line: u32, character: u32 },
                        end: struct { line: u32, character: u32 },
                    },
                };

                const response = HoverResponse{
                    .contents = .{
                        .kind = "markdown",
                        .value = result.type_str,
                    },
                    .range = if (result.range) |r| .{
                        .start = .{ .line = r.start_line, .character = r.start_col },
                        .end = .{ .line = r.end_line, .character = r.end_col },
                    } else null,
                };

                try self.sendResponse(id, response);
            } else {
                try self.sendNullResponse(id);
            }
        }
    };
}
