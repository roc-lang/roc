//! Semantic token extraction for the Roc LSP.
//!
//! This module provides functionality to extract semantic tokens from Roc source code
//! and encode them in the LSP delta-encoded format for syntax highlighting.

const std = @import("std");
const tokenize = @import("parse").tokenize;
const base = @import("base");
const line_info = @import("line_info.zig");

const Token = tokenize.Token;
const Tokenizer = tokenize.Tokenizer;
const CommonEnv = base.CommonEnv;
const LineInfo = line_info.LineInfo;

/// Semantic token indices matching TOKEN_TYPES in capabilities.zig.
pub const SemanticType = enum(u32) {
    namespace = 0, // module names
    type = 1, // UpperIdent, type keywords
    parameter = 2, // function parameters (requires AST context)
    variable = 3, // LowerIdent
    property = 4, // record fields
    enumMember = 5, // tags
    function = 6, // function names (requires AST context)
    keyword = 7, // keywords
    string = 8, // string literals
    number = 9, // numeric literals
    operator = 10, // operators
    comment = 11, // comments (stripped by tokenizer)
};

/// A semantic token with absolute position information.
pub const SemanticToken = struct {
    line: u32,
    start_char: u32,
    length: u32,
    token_type: u32,
    modifiers: u32 = 0,
};

/// Maps a Roc Token.Tag to an LSP semantic type index.
/// Returns null for tokens that should not be highlighted (punctuation, etc.).
pub fn tokenTagToSemanticType(tag: Token.Tag) ?u32 {
    return switch (tag) {
        // Keywords
        .KwApp,
        .KwAs,
        .KwCrash,
        .KwDbg,
        .KwElse,
        .KwExpect,
        .KwExposes,
        .KwExposing,
        .KwFor,
        .KwGenerates,
        .KwHas,
        .KwHosted,
        .KwIf,
        .KwImplements,
        .KwImport,
        .KwImports,
        .KwIn,
        .KwInterface,
        .KwMatch,
        .KwModule,
        .KwPackage,
        .KwPackages,
        .KwPlatform,
        .KwProvides,
        .KwRequires,
        .KwReturn,
        .KwTargets,
        .KwVar,
        .KwWhere,
        .KwWhile,
        .KwWith,
        .KwBreak,
        => @intFromEnum(SemanticType.keyword),

        // Type identifiers
        .UpperIdent => @intFromEnum(SemanticType.type),

        // Variable identifiers
        .LowerIdent => @intFromEnum(SemanticType.variable),

        // Property access (record fields)
        .DotLowerIdent,
        .NoSpaceDotLowerIdent,
        => @intFromEnum(SemanticType.property),

        // Tag access (enum members)
        .DotUpperIdent,
        .NoSpaceDotUpperIdent,
        => @intFromEnum(SemanticType.enumMember),

        // Numeric literals
        .Int,
        .Float,
        .DotInt,
        .NoSpaceDotInt,
        .MalformedNumberBadSuffix,
        .MalformedNumberUnicodeSuffix,
        .MalformedNumberNoDigits,
        .MalformedNumberNoExponentDigits,
        => @intFromEnum(SemanticType.number),

        // String literals
        .StringStart,
        .StringEnd,
        .StringPart,
        .MultilineStringStart,
        .SingleQuote,
        .MalformedSingleQuote,
        .MalformedStringPart,
        .MalformedInvalidUnicodeEscapeSequence,
        .MalformedInvalidEscapeSequence,
        => @intFromEnum(SemanticType.string),

        // Operators
        .OpPlus,
        .OpStar,
        .OpPizza,
        .OpAssign,
        .OpBinaryMinus,
        .OpUnaryMinus,
        .OpNotEquals,
        .OpBang,
        .OpAnd,
        .OpAmpersand,
        .OpQuestion,
        .OpDoubleQuestion,
        .OpOr,
        .OpBar,
        .OpDoubleSlash,
        .OpSlash,
        .OpPercent,
        .OpCaret,
        .OpGreaterThanOrEq,
        .OpGreaterThan,
        .OpLessThanOrEq,
        .OpBackArrow,
        .OpLessThan,
        .OpEquals,
        .OpColonEqual,
        .OpDoubleColon,
        .NoSpaceOpQuestion,
        .OpColon,
        .OpArrow,
        .OpFatArrow,
        .OpBackslash,
        .DoubleDot,
        .TripleDot,
        .DotStar,
        => @intFromEnum(SemanticType.operator),

        // Named underscore and opaque names
        .NamedUnderscore,
        .MalformedNamedUnderscoreUnicode,
        => @intFromEnum(SemanticType.variable),

        .OpaqueName,
        .MalformedOpaqueNameUnicode,
        .MalformedOpaqueNameWithoutName,
        => @intFromEnum(SemanticType.type),

        // Unicode identifier variants
        .MalformedUnicodeIdent,
        .MalformedDotUnicodeIdent,
        .MalformedNoSpaceDotUnicodeIdent,
        => @intFromEnum(SemanticType.variable),

        // Punctuation and structural tokens (not highlighted)
        .EndOfFile,
        .OpenRound,
        .CloseRound,
        .OpenSquare,
        .CloseSquare,
        .OpenCurly,
        .CloseCurly,
        .OpenStringInterpolation,
        .CloseStringInterpolation,
        .NoSpaceOpenRound,
        .Comma,
        .Dot,
        .Underscore,
        .MalformedUnknownToken,
        => null,
    };
}

/// Extracts semantic tokens from Roc source code.
/// Returns a list of SemanticToken structs with absolute positions.
pub fn extractSemanticTokens(
    allocator: std.mem.Allocator,
    source: []const u8,
    info: *const LineInfo,
) ![]SemanticToken {
    // Create a CommonEnv for tokenization
    const source_copy = try allocator.dupe(u8, source);
    defer allocator.free(source_copy);

    var env = try CommonEnv.init(allocator, source_copy);
    defer env.deinit(allocator);

    // Create diagnostics buffer (we ignore diagnostics for semantic tokens)
    var diagnostics: [64]tokenize.Diagnostic = undefined;

    // Tokenize the source
    var tokenizer = try Tokenizer.init(&env, allocator, source_copy, &diagnostics);
    defer tokenizer.deinit(allocator);
    try tokenizer.tokenize(allocator);

    // Extract token data
    const tags = tokenizer.output.tokens.items(.tag);
    const regions = tokenizer.output.tokens.items(.region);

    // Build semantic tokens list
    var tokens: std.ArrayListUnmanaged(SemanticToken) = .{};
    errdefer tokens.deinit(allocator);

    for (tags, regions) |tag, region| {
        const semantic_type = tokenTagToSemanticType(tag) orelse continue;

        const start_offset = region.start.offset;
        const end_offset = region.end.offset;
        const length = end_offset - start_offset;

        // Skip zero-length tokens
        if (length == 0) continue;

        // Convert byte offset to line/character position
        const pos = info.positionFromOffset(start_offset) orelse continue;

        try tokens.append(allocator, .{
            .line = pos.line,
            .start_char = pos.character,
            .length = length,
            .token_type = semantic_type,
            .modifiers = 0,
        });
    }

    return tokens.toOwnedSlice(allocator);
}

/// Delta-encodes a list of semantic tokens into the LSP format.
/// The LSP format uses 5 integers per token: [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
/// where deltaLine and deltaStartChar are relative to the previous token.
pub fn deltaEncode(allocator: std.mem.Allocator, tokens: []const SemanticToken) ![]u32 {
    if (tokens.len == 0) {
        return &[_]u32{};
    }

    var result = try allocator.alloc(u32, tokens.len * 5);
    errdefer allocator.free(result);

    var prev_line: u32 = 0;
    var prev_char: u32 = 0;

    for (tokens, 0..) |token, i| {
        const delta_line = token.line - prev_line;
        const delta_char = if (delta_line == 0) token.start_char - prev_char else token.start_char;

        result[i * 5 + 0] = delta_line;
        result[i * 5 + 1] = delta_char;
        result[i * 5 + 2] = token.length;
        result[i * 5 + 3] = token.token_type;
        result[i * 5 + 4] = token.modifiers;

        prev_line = token.line;
        prev_char = token.start_char;
    }

    return result;
}
