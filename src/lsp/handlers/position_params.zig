//! Shared parsing for LSP requests that name a position in a document.
//!
//! Several requests carry the same `textDocument.uri` plus `position` shape.
//! The handlers that need it share this rather than each restating which field
//! may be missing or mistyped.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");

/// A document position, as a request carries it.
pub const PositionParams = struct {
    uri: []const u8,
    line: u32,
    character: u32,
    /// The request's params object, so a caller can read the fields specific to
    /// its request without re-validating what was already checked here.
    obj: std.json.ObjectMap,
};

/// Parse `textDocument.uri` and `position` out of a request.
///
/// Reports the specific missing or mistyped field to the client and returns
/// null when the params do not describe a position.
pub fn parse(
    self: anytype,
    id: *protocol.JsonId,
    method: []const u8,
    maybe_params: ?std.json.Value,
) (Allocator.Error || error{WriteFailed})!?PositionParams {
    // Name the method in the message so a client that sends both requests can
    // tell which one it got wrong.
    var message_buf: [96]u8 = undefined;

    const params = maybe_params orelse {
        const message = std.fmt.bufPrint(&message_buf, "{s} requires params", .{method}) catch "request requires params";
        try self.sendError(id, .invalid_params, message);
        return null;
    };
    if (std.meta.activeTag(params) != .object) {
        const message = std.fmt.bufPrint(&message_buf, "{s} params must be an object", .{method}) catch "request params must be an object";
        try self.sendError(id, .invalid_params, message);
        return null;
    }
    const obj = params.object;

    const text_doc_value = obj.get("textDocument") orelse {
        try self.sendError(id, .invalid_params, "missing textDocument");
        return null;
    };
    if (std.meta.activeTag(text_doc_value) != .object) {
        try self.sendError(id, .invalid_params, "textDocument must be an object");
        return null;
    }
    const uri_value = text_doc_value.object.get("uri") orelse {
        try self.sendError(id, .invalid_params, "missing uri");
        return null;
    };
    if (std.meta.activeTag(uri_value) != .string) {
        try self.sendError(id, .invalid_params, "uri must be a string");
        return null;
    }

    const position_value = obj.get("position") orelse {
        try self.sendError(id, .invalid_params, "missing position");
        return null;
    };
    if (std.meta.activeTag(position_value) != .object) {
        try self.sendError(id, .invalid_params, "position must be an object");
        return null;
    }
    const position_obj = position_value.object;

    const line_value = position_obj.get("line") orelse {
        try self.sendError(id, .invalid_params, "missing line");
        return null;
    };
    if (std.meta.activeTag(line_value) != .integer) {
        try self.sendError(id, .invalid_params, "line must be an integer");
        return null;
    }
    const line: u32 = std.math.cast(u32, line_value.integer) orelse {
        try self.sendError(id, .invalid_params, "line must be a non-negative integer");
        return null;
    };

    const character_value = position_obj.get("character") orelse {
        try self.sendError(id, .invalid_params, "missing character");
        return null;
    };
    if (std.meta.activeTag(character_value) != .integer) {
        try self.sendError(id, .invalid_params, "character must be an integer");
        return null;
    }
    const character: u32 = std.math.cast(u32, character_value.integer) orelse {
        try self.sendError(id, .invalid_params, "character must be a non-negative integer");
        return null;
    };

    return PositionParams{
        .uri = uri_value.string,
        .line = line,
        .character = character,
        .obj = obj,
    };
}
