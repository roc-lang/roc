//! Handler for LSP `textDocument/formatting` requests.
//!
//! Formats Roc source code using the built-in formatter.

const std = @import("std");
const protocol = @import("../protocol.zig");
const fmt = @import("fmt");
const parse = @import("parse");
const can = @import("can");

/// Handler for `textDocument/formatting` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "formatting requires params");
                return;
            };

            const obj = switch (params) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "formatting params must be an object");
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
                try self.sendNullResponse(id);
                return;
            };

            // Format the document
            const formatted = formatSource(self.allocator, text) catch |err| {
                std.log.err("formatting failed: {s}", .{@errorName(err)});
                try self.sendNullResponse(id);
                return;
            };
            defer self.allocator.free(formatted);

            // If the formatted text is the same as the original, return empty array
            if (std.mem.eql(u8, text, formatted)) {
                try self.sendResponse(id, &[_]TextEdit{});
                return;
            }

            // Count lines in the original document
            var line_count: u32 = 0;
            var last_line_len: u32 = 0;
            var line_start: usize = 0;
            for (text, 0..) |c, i| {
                if (c == '\n') {
                    line_count += 1;
                    line_start = i + 1;
                }
            }
            last_line_len = @intCast(text.len - line_start);

            // Return a single edit that replaces the entire document
            const edit = TextEdit{
                .range = .{
                    .start = .{ .line = 0, .character = 0 },
                    .end = .{ .line = line_count, .character = last_line_len },
                },
                .newText = formatted,
            };

            try self.sendResponse(id, &[_]TextEdit{edit});
        }
    };
}

const TextEdit = struct {
    range: Range,
    newText: []const u8,
};

const Range = struct {
    start: Position,
    end: Position,
};

const Position = struct {
    line: u32,
    character: u32,
};

/// Format source code and return the formatted result.
fn formatSource(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    // Create ModuleEnv for parsing
    var module_env = try can.ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    // Parse the source
    var ast = try parse.parse(&module_env.common, allocator);
    defer ast.deinit(allocator);

    // Check for parse errors - if there are errors, return the original source
    if (ast.parse_diagnostics.items.len > 0) {
        return error.ParseError;
    }

    // Create an allocating writer for the formatter
    var result: std.Io.Writer.Allocating = .init(allocator);
    defer result.deinit();

    // Format the AST
    try fmt.formatAst(ast, &result.writer);

    return try result.toOwnedSlice();
}
