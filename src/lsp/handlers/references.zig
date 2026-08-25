//! Handler for LSP `textDocument/references` requests.
//!
//! Reports every place a symbol is written. The occurrences come from the CIR,
//! so shadowing is respected: a same-named binding in another scope is not
//! reported, and a reference is resolved to the binding it actually names
//! rather than to matching text.
//!
//! Unlike rename this only reports, so it is not restricted to plain bindings—
//! any pattern the cursor resolves to can have its uses listed.
//!
//! Only the requested document is searched. A binding that other modules import
//! will have uses this does not list, so an empty or short result is not proof
//! that a symbol is unused across a project.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const position_params = @import("position_params.zig");

const Position = struct {
    line: u32,
    character: u32,
};

const Range = struct {
    start: Position,
    end: Position,
};

/// An LSP `Location`: a range within a document.
const Location = struct {
    uri: []const u8,
    range: Range,
};

/// Handler for `textDocument/references` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const position = try position_params.parse(self, id, "references", maybe_params) orelse return;

            // `context.includeDeclaration` is required by the protocol, but
            // treat a client that omits it as asking for everything: listing
            // too much is recoverable, hiding the definition is confusing.
            var include_declaration = true;
            if (position.obj.get("context")) |context_value| {
                if (std.meta.activeTag(context_value) != .object) {
                    try self.sendError(id, .invalid_params, "context must be an object");
                    return;
                }
                if (context_value.object.get("includeDeclaration")) |flag| {
                    if (std.meta.activeTag(flag) != .bool) {
                        try self.sendError(id, .invalid_params, "includeDeclaration must be a boolean");
                        return;
                    }
                    include_declaration = flag.bool;
                }
            }

            const doc = self.doc_store.get(position.uri);
            const text = if (doc) |d| d.text else null;

            const found = self.syntax_checker.getReferencesAtPosition(
                position.uri,
                text,
                position.line,
                position.character,
                include_declaration,
            ) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    std.log.err("references failed: {s}", .{@errorName(err)});
                    try self.sendNullResponse(id);
                    return;
                },
            };

            const result = found orelse {
                try self.sendNullResponse(id);
                return;
            };
            defer result.deinit(self.allocator);

            var locations: std.ArrayList(Location) = .empty;
            defer locations.deinit(self.allocator);
            for (result.regions) |range| {
                try locations.append(self.allocator, .{
                    .uri = position.uri,
                    .range = .{
                        .start = .{ .line = range.start_line, .character = range.start_col },
                        .end = .{ .line = range.end_line, .character = range.end_col },
                    },
                });
            }

            try self.sendResponse(id, locations.items);
        }
    };
}
