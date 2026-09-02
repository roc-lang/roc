//! Handler for LSP `textDocument/inlayHint` requests.
//!
//! Shows the inferred type of bindings that do not write one down. Roc infers
//! most types, and an annotation may still leave parts to inference with `_`,
//! so the type a binding actually has is frequently written nowhere in the
//! file. Bindings that do carry an annotation are skipped: repeating what is
//! already on screen is noise.
//!
//! The editor asks for a line range—whatever is on screen—and re-asks as
//! the view moves, so this only ever renders what is visible.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");

/// An LSP `InlayHint`.
const InlayHint = struct {
    position: struct {
        line: u32,
        character: u32,
    },
    label: []const u8,
    /// 1 = Type, per the protocol's `InlayHintKind`.
    kind: u32 = 1,
    /// The label already begins with `: `, so no extra space is wanted.
    paddingLeft: bool = false,
    paddingRight: bool = false,
};

/// Read a `line` field out of a position object.
fn readLine(
    self: anytype,
    id: *protocol.JsonId,
    position_value: std.json.Value,
) (Allocator.Error || error{WriteFailed})!?u32 {
    if (std.meta.activeTag(position_value) != .object) {
        try self.sendError(id, .invalid_params, "range positions must be objects");
        return null;
    }
    const line_value = position_value.object.get("line") orelse {
        try self.sendError(id, .invalid_params, "range positions must have a line");
        return null;
    };
    if (std.meta.activeTag(line_value) != .integer) {
        try self.sendError(id, .invalid_params, "line must be an integer");
        return null;
    }
    return std.math.cast(u32, line_value.integer) orelse {
        try self.sendError(id, .invalid_params, "line must be a non-negative integer");
        return null;
    };
}

/// Handler for `textDocument/inlayHint` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "inlayHint requires params");
                return;
            };
            if (std.meta.activeTag(params) != .object) {
                try self.sendError(id, .invalid_params, "inlayHint params must be an object");
                return;
            }
            const obj = params.object;

            const text_doc_value = obj.get("textDocument") orelse {
                try self.sendError(id, .invalid_params, "missing textDocument");
                return;
            };
            if (std.meta.activeTag(text_doc_value) != .object) {
                try self.sendError(id, .invalid_params, "textDocument must be an object");
                return;
            }
            const uri_value = text_doc_value.object.get("uri") orelse {
                try self.sendError(id, .invalid_params, "missing uri");
                return;
            };
            if (std.meta.activeTag(uri_value) != .string) {
                try self.sendError(id, .invalid_params, "uri must be a string");
                return;
            }
            const uri = uri_value.string;

            const range_value = obj.get("range") orelse {
                try self.sendError(id, .invalid_params, "missing range");
                return;
            };
            if (std.meta.activeTag(range_value) != .object) {
                try self.sendError(id, .invalid_params, "range must be an object");
                return;
            }
            const start_value = range_value.object.get("start") orelse {
                try self.sendError(id, .invalid_params, "range must have a start");
                return;
            };
            const end_value = range_value.object.get("end") orelse {
                try self.sendError(id, .invalid_params, "range must have an end");
                return;
            };
            const start_line = try readLine(self, id, start_value) orelse return;
            const end_line = try readLine(self, id, end_value) orelse return;
            if (end_line < start_line) {
                try self.sendError(id, .invalid_params, "range end must not precede its start");
                return;
            }

            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else null;

            const found = self.syntax_checker.getInlayHints(
                uri,
                text,
                start_line,
                end_line,
            ) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    std.log.err("inlayHint failed: {s}", .{@errorName(err)});
                    try self.sendNullResponse(id);
                    return;
                },
            };

            // A document that does not build has no inferred types to show.
            // Answering null leaves whatever the editor last drew in place,
            // which is steadier than blanking every hint on each keystroke.
            const result = found orelse {
                try self.sendNullResponse(id);
                return;
            };
            defer result.deinit(self.allocator);

            var hints: std.ArrayList(InlayHint) = .empty;
            defer hints.deinit(self.allocator);
            for (result.hints) |hint| {
                try hints.append(self.allocator, .{
                    .position = .{ .line = hint.line, .character = hint.character },
                    .label = hint.label,
                });
            }

            try self.sendResponse(id, hints.items);
        }
    };
}
