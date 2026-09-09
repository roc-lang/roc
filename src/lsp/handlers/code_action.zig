//! Handler for LSP `textDocument/codeAction` requests.
//!
//! Two actions are offered, both written from the checked types rather than
//! from the text on screen: an annotation carrying a binding's inferred type,
//! and an `expect` that calls a top-level function.
//!
//! Neither is offered unless it can be written in full. A document that does
//! not build has no types to read, and a function whose parameters have no
//! obvious literal value gets no generated test - handing the author source
//! that does not compile is worse than offering nothing.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const workspace_edit = @import("workspace_edit.zig");

const TextEdit = workspace_edit.TextEdit;
const WorkspaceEdit = workspace_edit.WorkspaceEdit;

/// One entry of the response array.
const CodeAction = struct {
    title: []const u8,
    kind: []const u8,
    edit: WorkspaceEdit,
};

/// A `line`/`character` pair read out of the request.
const RequestPosition = struct {
    line: u32,
    character: u32,
};

/// Read a u32 field out of a position object.
fn readPositionField(
    self: anytype,
    id: *protocol.JsonId,
    position: std.json.ObjectMap,
    field: []const u8,
) (Allocator.Error || error{WriteFailed})!?u32 {
    const value = position.get(field) orelse {
        try self.sendError(id, .invalid_params, "range positions must have a line and a character");
        return null;
    };
    if (std.meta.activeTag(value) != .integer) {
        try self.sendError(id, .invalid_params, "range positions must be integers");
        return null;
    }
    return std.math.cast(u32, value.integer) orelse {
        try self.sendError(id, .invalid_params, "range positions must be non-negative");
        return null;
    };
}

/// Read one end of the requested range.
fn readPosition(
    self: anytype,
    id: *protocol.JsonId,
    range: std.json.ObjectMap,
    field: []const u8,
) (Allocator.Error || error{WriteFailed})!?RequestPosition {
    const value = range.get(field) orelse {
        try self.sendError(id, .invalid_params, "range must have a start and an end");
        return null;
    };
    if (std.meta.activeTag(value) != .object) {
        try self.sendError(id, .invalid_params, "range positions must be objects");
        return null;
    }
    const line = try readPositionField(self, id, value.object, "line") orelse return null;
    const character = try readPositionField(self, id, value.object, "character") orelse return null;
    return .{ .line = line, .character = character };
}

/// Whether `requested` selects `kind`, per the protocol's kind hierarchy.
///
/// A requested kind matches the kinds it is a prefix of, but only at a dot:
/// `refactor` selects `refactor.rewrite`, while `refactorx` selects nothing.
fn kindMatches(requested: []const u8, kind: []const u8) bool {
    if (std.mem.eql(u8, requested, kind)) return true;
    return std.mem.startsWith(u8, kind, requested) and
        kind.len > requested.len and
        kind[requested.len] == '.';
}

/// Whether the request's `context.only` filter, if it sent one, admits `kind`.
fn contextAdmits(params: std.json.ObjectMap, kind: []const u8) bool {
    const context = params.get("context") orelse return true;
    if (std.meta.activeTag(context) != .object) return true;
    const only = context.object.get("only") orelse return true;
    if (std.meta.activeTag(only) != .array) return true;

    for (only.array.items) |requested| {
        if (std.meta.activeTag(requested) != .string) continue;
        if (kindMatches(requested.string, kind)) return true;
    }
    return false;
}

/// Handler for `textDocument/codeAction` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "codeAction requires params");
                return;
            };
            if (std.meta.activeTag(params) != .object) {
                try self.sendError(id, .invalid_params, "codeAction params must be an object");
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
            const start = try readPosition(self, id, range_value.object, "start") orelse return;
            const end = try readPosition(self, id, range_value.object, "end") orelse return;

            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else null;

            const found = self.syntax_checker.getCodeActions(
                uri,
                text,
                start.line,
                start.character,
                end.line,
                end.character,
            ) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    std.log.err("codeAction failed: {s}", .{@errorName(err)});
                    try self.sendNullResponse(id);
                    return;
                },
            };

            // A document without types offers nothing. That is an empty list
            // rather than null: the editor asked what it may do here, and the
            // answer is "nothing", not "no answer".
            const result = found orelse {
                try self.sendResponse(id, &[_]CodeAction{});
                return;
            };
            defer result.deinit(self.allocator);

            var actions: std.ArrayList(CodeAction) = .empty;
            defer actions.deinit(self.allocator);
            var edits: std.ArrayList(TextEdit) = .empty;
            defer edits.deinit(self.allocator);

            // Each action carries one edit, and the edits are stored together
            // so every action can point into the same stable list.
            try edits.ensureTotalCapacity(self.allocator, result.actions.len);
            for (result.actions) |action| {
                if (!contextAdmits(obj, action.kind)) continue;
                edits.appendAssumeCapacity(.{
                    .range = workspace_edit.toRange(action.range),
                    .newText = action.new_text,
                });
                try actions.append(self.allocator, .{
                    .title = action.title,
                    .kind = action.kind,
                    .edit = .{ .uri = uri, .edits = edits.items[edits.items.len - 1 ..] },
                });
            }

            try self.sendResponse(id, actions.items);
        }
    };
}
