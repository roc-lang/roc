//! Handler for LSP `textDocument/definition` requests.
//!
//! Provides go-to-definition functionality by finding where a symbol is defined.

const std = @import("std");
const protocol = @import("../protocol.zig");

/// Handler for `textDocument/definition` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "definition requires params");
                return;
            };

            const obj = switch (params) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "definition params must be an object");
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

            // Query the syntax checker for definition location
            const def_result = self.syntax_checker.getDefinitionAtPosition(
                uri,
                text,
                line,
                character,
            ) catch |err| {
                std.log.err("definition failed: {s}", .{@errorName(err)});
                try self.sendNullResponse(id);
                return;
            };

            if (def_result) |result| {
                // Build the Location response
                const LocationResponse = struct {
                    uri: []const u8,
                    range: struct {
                        start: struct { line: u32, character: u32 },
                        end: struct { line: u32, character: u32 },
                    },
                };

                const response = LocationResponse{
                    .uri = result.uri,
                    .range = .{
                        .start = .{ .line = result.range.start_line, .character = result.range.start_col },
                        .end = .{ .line = result.range.end_line, .character = result.range.end_col },
                    },
                };

                try self.sendResponse(id, response);
            } else {
                try self.sendNullResponse(id);
            }
        }
    };
}
