//! The LSP edit shapes shared by the handlers that rewrite a document.
//!
//! `changes` is keyed by document URI, which no static Zig struct can express,
//! so the object is written by hand. The `changes` form is used rather than
//! `documentChanges` because every client supports it, while `documentChanges`
//! is gated on a client capability the server does not currently parse.

const syntax = @import("../syntax.zig");

/// A position in a document, in the protocol's UTF-16 code units.
pub const Position = struct {
    line: u32,
    character: u32,
};

/// A span between two positions. An empty span marks an insertion point.
pub const Range = struct {
    start: Position,
    end: Position,
};

/// One text replacement inside a document.
pub const TextEdit = struct {
    range: Range,
    newText: []const u8,
};

/// An LSP `WorkspaceEdit` carrying edits for a single document.
pub const WorkspaceEdit = struct {
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
pub fn toRange(range: syntax.SyntaxChecker.LspRange) Range {
    return .{
        .start = .{ .line = range.start_line, .character = range.start_col },
        .end = .{ .line = range.end_line, .character = range.end_col },
    };
}
