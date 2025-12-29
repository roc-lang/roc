//! LSP server capability definitions for the Roc language server.

/// Aggregates all server capabilities supported by the Roc LSP.
pub const ServerCapabilities = struct {
    positionEncoding: []const u8 = "utf-16",
    textDocumentSync: ?TextDocumentSyncOptions = null,

    pub const TextDocumentSyncOptions = struct {
        openClose: bool = false,
        change: u32 = @intFromEnum(TextDocumentSyncKind.none),
    };

    pub const TextDocumentSyncKind = enum(u32) {
        none = 0,
        full = 1,
        incremental = 2,
    };
};

/// Returns the server capabilities currently implemented.
pub fn buildCapabilities() ServerCapabilities {
    return .{
        .textDocumentSync = .{
            .openClose = true,
            .change = @intFromEnum(ServerCapabilities.TextDocumentSyncKind.incremental),
        },
    };
}
