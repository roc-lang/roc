//! Handler for LSP `textDocument/didClose` notifications.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Handler for `textDocument/didClose` notifications. A closed document's
/// truth is the file on disk again, so the server drops its stored text and
/// what the syntax driver kept for it.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, params_value: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const params = params_value orelse return;
            if (std.meta.activeTag(params) != .object) return;
            const obj = params.object;

            const text_doc_value = obj.get("textDocument") orelse return;
            if (std.meta.activeTag(text_doc_value) != .object) return;
            const text_doc = text_doc_value.object;

            const uri_value = text_doc.get("uri") orelse return;
            if (std.meta.activeTag(uri_value) != .string) return;
            const uri = uri_value.string;

            self.doc_store.remove(uri);
            try self.syntax_checker.forget(uri);
        }
    };
}
