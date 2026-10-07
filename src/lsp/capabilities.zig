//! LSP server capability definitions for the Roc language server.

const SemanticType = @import("semantic_tokens.zig").SemanticType;

/// Semantic token types supported by the Roc LSP: the legend sent to clients.
/// Token data refers to a type by its index here, so each entry is the name of
/// the `SemanticType` whose value is that index.
pub const TOKEN_TYPES = blk: {
    const types = @typeInfo(SemanticType).@"enum".fields;
    var names: [types.len][]const u8 = undefined;
    for (types) |semantic_type| names[semantic_type.value] = semantic_type.name;
    break :blk names;
};

/// Semantic token modifiers.
/// Order matters - each index is a bit in a token's modifier set.
pub const TOKEN_MODIFIERS = [_][]const u8{
    "declaration", // 0 - the name a type declaration introduces
};

/// Aggregates all server capabilities supported by the Roc LSP.
pub const ServerCapabilities = struct {
    positionEncoding: []const u8 = "utf-16",
    textDocumentSync: ?TextDocumentSyncOptions = null,
    semanticTokensProvider: ?SemanticTokensOptions = null,
    hoverProvider: bool = false,
    definitionProvider: bool = false,
    documentFormattingProvider: bool = false,
    documentSymbolProvider: bool = false,
    foldingRangeProvider: bool = false,
    selectionRangeProvider: bool = false,
    documentHighlightProvider: bool = false,
    completionProvider: ?CompletionOptions = null,
    referencesProvider: bool = false,
    inlayHintProvider: bool = false,
    codeActionProvider: ?CodeActionOptions = null,
    renameProvider: ?RenameOptions = null,

    pub const TextDocumentSyncOptions = struct {
        openClose: bool = false,
        change: u32 = @intFromEnum(TextDocumentSyncKind.none),
    };

    pub const TextDocumentSyncKind = enum(u32) {
        none = 0,
        full = 1,
        incremental = 2,
    };

    pub const SemanticTokensOptions = struct {
        legend: SemanticTokensLegend,
        full: bool = true,
        range: bool = false,
    };

    pub const SemanticTokensLegend = struct {
        tokenTypes: []const []const u8,
        tokenModifiers: []const []const u8,
    };

    pub const CompletionOptions = struct {
        triggerCharacters: []const []const u8 = &.{ ".", ":" },
        resolveProvider: bool = false,
    };

    pub const CodeActionOptions = struct {
        /// The kinds this server can return, so a client asking for a subset
        /// knows in advance whether it is worth asking at all.
        codeActionKinds: []const []const u8 = &.{"refactor.rewrite"},
    };

    pub const RenameOptions = struct {
        /// The server answers `textDocument/prepareRename`, so the editor asks
        /// whether a position is renameable before prompting for a new name.
        prepareProvider: bool = true,
    };
};

/// Returns the server capabilities currently implemented.
pub fn buildCapabilities() ServerCapabilities {
    return .{
        .textDocumentSync = .{
            .openClose = true,
            .change = @intFromEnum(ServerCapabilities.TextDocumentSyncKind.incremental),
        },
        .semanticTokensProvider = .{
            .legend = .{
                .tokenTypes = &TOKEN_TYPES,
                .tokenModifiers = &TOKEN_MODIFIERS,
            },
            .full = true,
        },
        .hoverProvider = true,
        .definitionProvider = true,
        .documentFormattingProvider = true,
        .documentSymbolProvider = true,
        .foldingRangeProvider = true,
        .selectionRangeProvider = true,
        .documentHighlightProvider = true,
        .completionProvider = .{},
        .referencesProvider = true,
        .inlayHintProvider = true,
        .codeActionProvider = .{},
        .renameProvider = .{},
    };
}
