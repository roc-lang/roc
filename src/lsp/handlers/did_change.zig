//! Handler for LSP `textDocument/didChange` notifications.

const std = @import("std");
const Allocator = std.mem.Allocator;
const DocumentStore = @import("../document_store.zig").DocumentStore;

/// Handler for `textDocument/didChange` notifications (supports incremental edits).
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

            const version_value = text_doc.get("version") orelse std.json.Value{ .integer = 0 };
            const version: i64 = if (std.meta.activeTag(version_value) == .integer)
                version_value.integer
            else if (std.meta.activeTag(version_value) == .float and std.math.isFinite(version_value.float) and version_value.float >= @as(f64, @floatFromInt(std.math.minInt(i64))) and version_value.float < @as(f64, @floatFromInt(std.math.maxInt(i64))))
                @intFromFloat(version_value.float)
            else
                0;

            const changes_value = obj.get("contentChanges") orelse return;
            if (std.meta.activeTag(changes_value) != .array) return;
            const changes = changes_value.array;
            if (changes.items.len == 0) return;

            var parsed_changes: std.ArrayList(DocumentStore.ContentChange) = .empty;
            defer parsed_changes.deinit(self.allocator);

            for (changes.items) |change_value| {
                if (std.meta.activeTag(change_value) != .object) return;
                const change_obj = change_value.object;
                const text_value = change_obj.get("text") orelse return;
                if (std.meta.activeTag(text_value) != .string) return;
                const text = text_value.string;

                var change = DocumentStore.ContentChange{ .text = text };
                if (change_obj.get("range")) |range_value| {
                    change.range = parseRange(range_value) catch |err| {
                        std.log.debug("invalid range for {s}: {s}", .{ uri, @errorName(err) });
                        return;
                    };
                }

                try parsed_changes.append(self.allocator, change);
            }

            if (parsed_changes.items.len == 0) return;

            var saw_full_change = false;
            for (parsed_changes.items) |change| {
                if (change.range == null) {
                    saw_full_change = true;
                    break;
                }
            }

            if (saw_full_change) {
                if (parsed_changes.items.len != 1) {
                    std.log.warn("received invalid mix of full and incremental changes for {s}", .{uri});
                    return;
                }
                try self.doc_store.upsert(uri, version, parsed_changes.items[0].text);
            } else {
                self.doc_store.applyContentChanges(uri, version, parsed_changes.items) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.DocumentNotFound,
                    error.InvalidPosition,
                    error.InvalidRange,
                    error.NoChanges,
                    => std.log.warn("failed to apply incremental change for {s}: {s}", .{ uri, @errorName(err) }),
                };
            }

            self.onDocumentChanged(uri);
        }

        fn parseRange(value: std.json.Value) error{InvalidRange}!DocumentStore.Range {
            if (std.meta.activeTag(value) != .object) return error.InvalidRange;
            const range_obj = value.object;
            const start_value = range_obj.get("start") orelse return error.InvalidRange;
            if (std.meta.activeTag(start_value) != .object) return error.InvalidRange;
            const start_obj = start_value.object;
            const end_value = range_obj.get("end") orelse return error.InvalidRange;
            if (std.meta.activeTag(end_value) != .object) return error.InvalidRange;
            const end_obj = end_value.object;
            return DocumentStore.Range{
                .start_line = parseIndex(start_obj, "line") catch return error.InvalidRange,
                .start_character = parseIndex(start_obj, "character") catch return error.InvalidRange,
                .end_line = parseIndex(end_obj, "line") catch return error.InvalidRange,
                .end_character = parseIndex(end_obj, "character") catch return error.InvalidRange,
            };
        }

        fn parseIndex(obj: std.json.ObjectMap, field: []const u8) error{ MissingField, InvalidField }!usize {
            const value = obj.get(field) orelse return error.MissingField;
            if (std.meta.activeTag(value) == .integer) {
                return std.math.cast(usize, value.integer) orelse error.InvalidField;
            }
            if (std.meta.activeTag(value) == .float) {
                const f = value.float;
                if (!std.math.isFinite(f) or f < 0 or f >= @as(f64, @floatFromInt(std.math.maxInt(usize)))) {
                    return error.InvalidField;
                }
                return @intFromFloat(f);
            }
            return error.InvalidField;
        }
    };
}
