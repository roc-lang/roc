const std = @import("std");

/// Handler for `textDocument/didOpen` notifications.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, params_value: ?std.json.Value) !void {
            const params = params_value orelse return;
            const obj = switch (params) {
                .object => |o| o,
                else => return,
            };

            const text_doc_value = obj.get("textDocument") orelse return;
            const text_doc = switch (text_doc_value) {
                .object => |o| o,
                else => return,
            };

            const uri_value = text_doc.get("uri") orelse return;
            const uri = switch (uri_value) {
                .string => |s| s,
                else => return,
            };

            const text_value = text_doc.get("text") orelse return;
            const text = switch (text_value) {
                .string => |s| s,
                else => return,
            };

            const version_value = text_doc.get("version") orelse std.json.Value{ .integer = 0 };
            const version: i64 = switch (version_value) {
                .integer => |v| v,
                .float => |f| @intFromFloat(f),
                else => 0,
            };

            self.doc_store.upsert(uri, version, text) catch |err| {
                std.log.err("failed to open {s}: {s}", .{ uri, @errorName(err) });
            };
        }
    };
}
