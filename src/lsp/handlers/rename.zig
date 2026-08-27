//! Handlers for LSP `textDocument/rename` and `textDocument/prepareRename`.
//!
//! Rename rewrites every occurrence of one binding: the binding itself, the
//! name on its type annotation, and every reference to it. The occurrences come
//! from the CIR, so shadowing is respected and same-named bindings elsewhere in
//! the file are left alone.
//!
//! This handler never guesses. If the document does not build, or the position
//! names something other than a plain local binding, it refuses instead of
//! offering a partial rewrite—editing every occurrence but one silently
//! breaks the program, which is worse than doing nothing.
//!
//! Cross-module rename is not supported: only the requested document is edited,
//! and a binding another module can reach is still reported here, so a rename
//! of an exported name must be reviewed by the author. That limit is why the
//! server declares `prepareProvider`, which lets the editor ask first.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const syntax = @import("../syntax.zig");
const position_params = @import("position_params.zig");

/// One text replacement inside a document.
const TextEdit = struct {
    range: Range,
    newText: []const u8,
};

const Position = struct {
    line: u32,
    character: u32,
};

const Range = struct {
    start: Position,
    end: Position,
};

/// An LSP `WorkspaceEdit` carrying edits for a single document.
///
/// `changes` is keyed by document URI, which no static Zig struct can express,
/// so the object is written by hand. The `changes` form is used rather than
/// `documentChanges` because every client supports it, while `documentChanges`
/// is gated on a client capability the server does not currently parse.
const WorkspaceEdit = struct {
    uri: []const u8,
    edits: []const TextEdit,

    pub fn jsonStringify(self: WorkspaceEdit, writer: anytype) error{WriteFailed}!void {
        try writer.beginObject();
        try writer.objectField("changes");
        try writer.beginObject();
        try writer.objectField(self.uri);
        try writer.write(self.edits);
        try writer.endObject();
        try writer.endObject();
    }
};

/// Convert a collected range into the LSP wire shape.
fn toRange(range: syntax.SyntaxChecker.LspRange) Range {
    return .{
        .start = .{ .line = range.start_line, .character = range.start_col },
        .end = .{ .line = range.end_line, .character = range.end_col },
    };
}

/// Handler for `textDocument/rename` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const position = try position_params.parse(self, id, "rename", maybe_params) orelse return;

            const new_name_value = position.obj.get("newName") orelse {
                try self.sendError(id, .invalid_params, "missing newName");
                return;
            };
            if (std.meta.activeTag(new_name_value) != .string) {
                try self.sendError(id, .invalid_params, "newName must be a string");
                return;
            }
            const new_name = new_name_value.string;

            const doc = self.doc_store.get(position.uri);
            const text = if (doc) |d| d.text else null;

            const outcome = self.syntax_checker.getRenameEditsAtPosition(
                position.uri,
                text,
                position.line,
                position.character,
                new_name,
            ) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    std.log.err("rename failed: {s}", .{@errorName(err)});
                    try self.sendError(id, .request_failed, "rename could not be computed for this document");
                    return;
                },
            };

            const resolved = outcome orelse {
                try self.sendError(id, .request_failed, "this document does not currently compile, so it cannot be renamed safely");
                return;
            };

            switch (resolved) {
                .rejected => |rejection| {
                    const message = switch (rejection) {
                        .not_a_local_binding => "only local bindings can be renamed here",
                        .bad_new_name => |reason| reason.message(),
                        .name_already_in_scope => "that name is already used by another binding that is visible here, so renaming would change what the code means",
                        .scope_unavailable => "this binding's scope could not be determined, so the rename could not be checked for safety",
                        .declaration_not_isolated => "this binding's declaration is not written as a plain name, so renaming it would rewrite the surrounding source",
                    };
                    try self.sendError(id, .request_failed, message);
                },
                .edits => |result| {
                    defer result.deinit(self.allocator);

                    var edits: std.ArrayList(TextEdit) = .empty;
                    defer edits.deinit(self.allocator);
                    for (result.regions) |range| {
                        try edits.append(self.allocator, .{
                            .range = toRange(range),
                            .newText = new_name,
                        });
                    }

                    try self.sendResponse(id, WorkspaceEdit{
                        .uri = position.uri,
                        .edits = edits.items,
                    });
                },
            }
        }
    };
}

/// Handler for `textDocument/prepareRename` requests.
pub fn prepareHandler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const position = try position_params.parse(self, id, "prepareRename", maybe_params) orelse return;

            const doc = self.doc_store.get(position.uri);
            const text = if (doc) |d| d.text else null;

            const prepared = self.syntax_checker.prepareRenameAtPosition(
                position.uri,
                text,
                position.line,
                position.character,
            ) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    std.log.err("prepareRename failed: {s}", .{@errorName(err)});
                    try self.sendNullResponse(id);
                    return;
                },
            };

            // A null result tells the editor not to offer renaming here, which
            // is what it should do for anything this server cannot rewrite in
            // full.
            const result = prepared orelse {
                try self.sendNullResponse(id);
                return;
            };
            defer result.deinit(self.allocator);

            const Response = struct {
                range: Range,
                placeholder: []const u8,
            };

            try self.sendResponse(id, Response{
                .range = toRange(result.range),
                .placeholder = result.placeholder,
            });
        }
    };
}
