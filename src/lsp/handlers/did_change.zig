const std = @import("std");
const DocumentStore = @import("../document_store.zig").DocumentStore;

/// Handler for `textDocument/didChange` notifications (supports incremental edits).
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

            const version_value = text_doc.get("version") orelse std.json.Value{ .integer = 0 };
            const version: i64 = switch (version_value) {
                .integer => |v| v,
                .float => |f| @intFromFloat(f),
                else => 0,
            };

            const changes_value = obj.get("contentChanges") orelse return;
            const changes = switch (changes_value) {
                .array => |arr| arr,
                else => return,
            };
            if (changes.items.len == 0) return;

            const last_change = changes.items[changes.items.len - 1];
            const change_obj = switch (last_change) {
                .object => |o| o,
                else => return,
            };
            const text_value = change_obj.get("text") orelse return;
            const text = switch (text_value) {
                .string => |s| s,
                else => return,
            };
            if (change_obj.get("range")) |range_value| {
                const range = parseRange(range_value) catch |err| {
                    std.log.err("invalid range for {s}: {s}", .{ uri, @errorName(err) });
                    return;
                };
                self.doc_store.applyRangeReplacement(uri, version, range, text) catch |err| {
                    std.log.err("failed to apply incremental change for {s}: {s}", .{ uri, @errorName(err) });
                };
            } else {
                self.doc_store.upsert(uri, version, text) catch |err| {
                    std.log.err("failed to apply full change for {s}: {s}", .{ uri, @errorName(err) });
                };
            }
        }

        fn parseRange(value: std.json.Value) !DocumentStore.Range {
            const range_obj = switch (value) {
                .object => |o| o,
                else => return error.InvalidRange,
            };
            const start_obj = switch (range_obj.get("start") orelse return error.InvalidRange) {
                .object => |o| o,
                else => return error.InvalidRange,
            };
            const end_obj = switch (range_obj.get("end") orelse return error.InvalidRange) {
                .object => |o| o,
                else => return error.InvalidRange,
            };
            return DocumentStore.Range{
                .start_line = parseIndex(start_obj, "line") catch return error.InvalidRange,
                .start_character = parseIndex(start_obj, "character") catch return error.InvalidRange,
                .end_line = parseIndex(end_obj, "line") catch return error.InvalidRange,
                .end_character = parseIndex(end_obj, "character") catch return error.InvalidRange,
            };
        }

        fn parseIndex(obj: std.json.ObjectMap, field: []const u8) !usize {
            const value = obj.get(field) orelse return error.MissingField;
            return switch (value) {
                .integer => |v| if (v < 0) error.InvalidField else @intCast(v),
                .float => |f| if (f < 0) error.InvalidField else @intFromFloat(f),
                else => return error.InvalidField,
            };
        }
    };
}
