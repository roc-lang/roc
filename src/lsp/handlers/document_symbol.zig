//! Handler for LSP `textDocument/documentSymbol` requests.
//!
//! Provides document outline/symbols for the editor sidebar.

const std = @import("std");
const protocol = @import("../protocol.zig");

/// Handler for `textDocument/documentSymbol` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "documentSymbol requires params");
                return;
            };

            const obj = switch (params) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "documentSymbol params must be an object");
                    return;
                },
            };

            // Extract textDocument.uri
            const text_doc_value = obj.get("textDocument") orelse {
                try self.sendError(id, .invalid_params, "missing textDocument");
                return;
            };
            const text_doc = switch (text_doc_value) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "textDocument must be an object");
                    return;
                },
            };
            const uri_value = text_doc.get("uri") orelse {
                try self.sendError(id, .invalid_params, "missing uri");
                return;
            };
            const uri = switch (uri_value) {
                .string => |s| s,
                else => {
                    try self.sendError(id, .invalid_params, "uri must be a string");
                    return;
                },
            };

            // Get the document text from the store (for source extraction)
            const doc = self.doc_store.get(uri);
            const source = if (doc) |d| d.text else {
                try self.sendResponse(id, &[_]SymbolInformation{});
                return;
            };

            // Use the syntax checker to get the canonicalized module
            const symbols = self.syntax_checker.getDocumentSymbols(self.allocator, uri, source) catch {
                try self.sendResponse(id, &[_]SymbolInformation{});
                return;
            };
            defer {
                for (symbols) |*sym| {
                    self.allocator.free(sym.name);
                }
                self.allocator.free(symbols);
            }

            try self.sendResponse(id, symbols);
        }
    };
}

/// LSP SymbolKind values - must be serialized as integers per LSP spec
pub const SymbolKind = enum(u32) {
    file = 1,
    module = 2,
    namespace = 3,
    package = 4,
    class = 5,
    method = 6,
    property = 7,
    field = 8,
    constructor = 9,
    @"enum" = 10,
    interface = 11,
    function = 12,
    variable = 13,
    constant = 14,
    string = 15,
    number = 16,
    boolean = 17,
    array = 18,
    object = 19,
    key = 20,
    null = 21,
    enum_member = 22,
    @"struct" = 23,
    event = 24,
    operator = 25,
    type_parameter = 26,

    /// Custom JSON serialization to output as integer (LSP spec requirement)
    pub fn jsonStringify(self: SymbolKind, jw: anytype) !void {
        try jw.write(@intFromEnum(self));
    }
};

/// A position in a text document (line and character offset).
pub const Position = struct {
    line: u32,
    character: u32,
};

/// A range in a text document (start and end positions).
pub const Range = struct {
    start: Position,
    end: Position,
};

/// A location in a document (URI and range).
pub const Location = struct {
    uri: []const u8,
    range: Range,
};

/// SymbolInformation format (flat list with location)
/// This is the format expected by VS Code's LSP client
pub const SymbolInformation = struct {
    name: []const u8,
    kind: SymbolKind,
    location: Location,
};
