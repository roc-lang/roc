//! Handler for LSP `textDocument/didOpen` notifications.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Handler for `textDocument/didOpen` notifications.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, params_value: ?std.json.Value) Allocator.Error!void {
            const params = params_value orelse return;
            if (std.meta.activeTag(params) != .object) return;
            const obj = params.object;

            const text_doc_value = obj.get("textDocument") orelse return;
            if (std.meta.activeTag(text_doc_value) != .object) return;
            const text_doc = text_doc_value.object;

            const uri_value = text_doc.get("uri") orelse return;
            if (std.meta.activeTag(uri_value) != .string) return;
            const uri = uri_value.string;

            const text_value = text_doc.get("text") orelse return;
            if (std.meta.activeTag(text_value) != .string) return;
            const text = text_value.string;

            const version_value = text_doc.get("version") orelse std.json.Value{ .integer = 0 };
            const version: i64 = if (std.meta.activeTag(version_value) == .integer)
                version_value.integer
            else if (std.meta.activeTag(version_value) == .float and std.math.isFinite(version_value.float) and version_value.float >= @as(f64, @floatFromInt(std.math.minInt(i64))) and version_value.float < @as(f64, @floatFromInt(std.math.maxInt(i64))))
                @intFromFloat(version_value.float)
            else
                0;

            try self.doc_store.upsert(uri, version, text);

            self.onDocumentChanged(uri);
        }
    };
}
