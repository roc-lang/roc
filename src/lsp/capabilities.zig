//! LSP server capability definitions for the Roc language server.

/// Semantic token types supported by the Roc LSP.
/// Order matters - indices into this array are used in token data.
pub const TOKEN_TYPES = [_][]const u8{
    "namespace", // 0 - module names
    "type", // 1 - UpperIdent, type keywords
    "parameter", // 2 - function parameters
    "variable", // 3 - LowerIdent
    "property", // 4 - record fields
    "enumMember", // 5 - tags
    "function", // 6 - function names
    "keyword", // 7 - keywords
    "string", // 8 - string literals
    "number", // 9 - numeric literals
    "operator", // 10 - operators
    "comment", // 11 - comments
};

/// Semantic token modifiers (currently unused).
pub const TOKEN_MODIFIERS = [_][]const u8{};

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
    };
}
