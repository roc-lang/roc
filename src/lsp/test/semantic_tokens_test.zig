//! Tests for semantic token extraction and delta encoding.

const std = @import("std");
const semantic_tokens = @import("../semantic_tokens.zig");
const line_info = @import("../line_info.zig");
const tokenize = @import("parse").tokenize;

const Token = tokenize.Token;
const SemanticToken = semantic_tokens.SemanticToken;
const SemanticType = semantic_tokens.SemanticType;
const LineInfo = line_info.LineInfo;

// Token tag mapping tests

test "tokenTagToSemanticType maps keywords" {
    const keyword_tags = [_]Token.Tag{
        .KwIf,
        .KwElse,
        .KwMatch,
        .KwImport,
        .KwModule,
        .KwApp,
        .KwAs,
        .KwReturn,
        .KwBreak,
    };

    for (keyword_tags) |tag| {
        const result = semantic_tokens.tokenTagToSemanticType(tag);
        try std.testing.expect(result != null);
        try std.testing.expectEqual(@intFromEnum(SemanticType.keyword), result.?);
    }
}

test "tokenTagToSemanticType maps type identifiers" {
    const result = semantic_tokens.tokenTagToSemanticType(.UpperIdent);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@intFromEnum(SemanticType.type), result.?);
}

test "tokenTagToSemanticType maps variable identifiers" {
    const result = semantic_tokens.tokenTagToSemanticType(.LowerIdent);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@intFromEnum(SemanticType.variable), result.?);
}

test "tokenTagToSemanticType maps numeric literals" {
    const number_tags = [_]Token.Tag{
        .Int,
        .Float,
        .DotInt,
        .NoSpaceDotInt,
    };

    for (number_tags) |tag| {
        const result = semantic_tokens.tokenTagToSemanticType(tag);
        try std.testing.expect(result != null);
        try std.testing.expectEqual(@intFromEnum(SemanticType.number), result.?);
    }
}

test "tokenTagToSemanticType maps string literals" {
    const string_tags = [_]Token.Tag{
        .StringStart,
        .StringEnd,
        .StringPart,
        .SingleQuote,
    };

    for (string_tags) |tag| {
        const result = semantic_tokens.tokenTagToSemanticType(tag);
        try std.testing.expect(result != null);
        try std.testing.expectEqual(@intFromEnum(SemanticType.string), result.?);
    }
}

test "tokenTagToSemanticType maps operators" {
    const operator_tags = [_]Token.Tag{
        .OpPlus,
        .OpStar,
        .OpAssign,
        .OpBinaryMinus,
        .OpEquals,
        .OpArrow,
    };

    for (operator_tags) |tag| {
        const result = semantic_tokens.tokenTagToSemanticType(tag);
        try std.testing.expect(result != null);
        try std.testing.expectEqual(@intFromEnum(SemanticType.operator), result.?);
    }
}

test "tokenTagToSemanticType maps property access" {
    const property_tags = [_]Token.Tag{
        .DotLowerIdent,
        .NoSpaceDotLowerIdent,
    };

    for (property_tags) |tag| {
        const result = semantic_tokens.tokenTagToSemanticType(tag);
        try std.testing.expect(result != null);
        try std.testing.expectEqual(@intFromEnum(SemanticType.property), result.?);
    }
}

test "tokenTagToSemanticType maps enum members (tags)" {
    const enum_tags = [_]Token.Tag{
        .DotUpperIdent,
        .NoSpaceDotUpperIdent,
    };

    for (enum_tags) |tag| {
        const result = semantic_tokens.tokenTagToSemanticType(tag);
        try std.testing.expect(result != null);
        try std.testing.expectEqual(@intFromEnum(SemanticType.enumMember), result.?);
    }
}

test "tokenTagToSemanticType returns null for punctuation" {
    const punct_tags = [_]Token.Tag{
        .OpenRound,
        .CloseRound,
        .OpenSquare,
        .CloseSquare,
        .OpenCurly,
        .CloseCurly,
        .Comma,
        .Dot,
        .EndOfFile,
    };

    for (punct_tags) |tag| {
        const result = semantic_tokens.tokenTagToSemanticType(tag);
        try std.testing.expect(result == null);
    }
}

// Delta encoding tests

test "deltaEncode empty tokens" {
    const allocator = std.testing.allocator;
    const tokens = &[_]SemanticToken{};
    const result = try semantic_tokens.deltaEncode(allocator, tokens);
    // Empty result should be a zero-length slice
    try std.testing.expectEqual(@as(usize, 0), result.len);
}

test "deltaEncode single token" {
    const allocator = std.testing.allocator;
    const tokens = &[_]SemanticToken{
        .{
            .line = 0,
            .start_char = 0,
            .length = 5,
            .token_type = @intFromEnum(SemanticType.keyword),
            .modifiers = 0,
        },
    };

    const result = try semantic_tokens.deltaEncode(allocator, tokens);
    defer allocator.free(result);

    try std.testing.expectEqual(@as(usize, 5), result.len);
    // [deltaLine, deltaStartChar, length, tokenType, tokenModifiers]
    try std.testing.expectEqual(@as(u32, 0), result[0]); // deltaLine
    try std.testing.expectEqual(@as(u32, 0), result[1]); // deltaStartChar
    try std.testing.expectEqual(@as(u32, 5), result[2]); // length
    try std.testing.expectEqual(@intFromEnum(SemanticType.keyword), result[3]); // tokenType
    try std.testing.expectEqual(@as(u32, 0), result[4]); // tokenModifiers
}

test "deltaEncode same line tokens" {
    const allocator = std.testing.allocator;
    // "if x" -> keyword "if" at 0, variable "x" at 3
    const tokens = &[_]SemanticToken{
        .{
            .line = 0,
            .start_char = 0,
            .length = 2,
            .token_type = @intFromEnum(SemanticType.keyword),
            .modifiers = 0,
        },
        .{
            .line = 0,
            .start_char = 3,
            .length = 1,
            .token_type = @intFromEnum(SemanticType.variable),
            .modifiers = 0,
        },
    };

    const result = try semantic_tokens.deltaEncode(allocator, tokens);
    defer allocator.free(result);

    try std.testing.expectEqual(@as(usize, 10), result.len);

    // First token: absolute position (0,0)
    try std.testing.expectEqual(@as(u32, 0), result[0]); // deltaLine
    try std.testing.expectEqual(@as(u32, 0), result[1]); // deltaStartChar
    try std.testing.expectEqual(@as(u32, 2), result[2]); // length

    // Second token: same line, delta from previous
    try std.testing.expectEqual(@as(u32, 0), result[5]); // deltaLine (same line)
    try std.testing.expectEqual(@as(u32, 3), result[6]); // deltaStartChar (3 - 0 = 3)
    try std.testing.expectEqual(@as(u32, 1), result[7]); // length
}

test "deltaEncode different line tokens" {
    const allocator = std.testing.allocator;
    // Line 0: "x = 1"
    // Line 1: "y = 2"
    const tokens = &[_]SemanticToken{
        .{
            .line = 0,
            .start_char = 0,
            .length = 1,
            .token_type = @intFromEnum(SemanticType.variable),
            .modifiers = 0,
        },
        .{
            .line = 1,
            .start_char = 0,
            .length = 1,
            .token_type = @intFromEnum(SemanticType.variable),
            .modifiers = 0,
        },
    };

    const result = try semantic_tokens.deltaEncode(allocator, tokens);
    defer allocator.free(result);

    try std.testing.expectEqual(@as(usize, 10), result.len);

    // First token: line 0, char 0
    try std.testing.expectEqual(@as(u32, 0), result[0]); // deltaLine
    try std.testing.expectEqual(@as(u32, 0), result[1]); // deltaStartChar

    // Second token: new line, char resets to absolute
    try std.testing.expectEqual(@as(u32, 1), result[5]); // deltaLine (1 - 0 = 1)
    try std.testing.expectEqual(@as(u32, 0), result[6]); // deltaStartChar (absolute on new line)
}

// Integration tests - extractSemanticTokens

test "extractSemanticTokens simple expression" {
    const allocator = std.testing.allocator;
    const source = "x = 42";

    var info = try LineInfo.init(allocator, source);
    defer info.deinit();

    const tokens = try semantic_tokens.extractSemanticTokens(allocator, source, &info);
    defer allocator.free(tokens);

    // Should have: variable "x", operator "=", number "42"
    try std.testing.expect(tokens.len >= 3);

    // Find the variable token
    var found_variable = false;
    var found_operator = false;
    var found_number = false;

    for (tokens) |token| {
        if (token.token_type == @intFromEnum(SemanticType.variable)) {
            found_variable = true;
            try std.testing.expectEqual(@as(u32, 0), token.line);
            try std.testing.expectEqual(@as(u32, 0), token.start_char);
            try std.testing.expectEqual(@as(u32, 1), token.length);
        }
        if (token.token_type == @intFromEnum(SemanticType.operator)) {
            found_operator = true;
        }
        if (token.token_type == @intFromEnum(SemanticType.number)) {
            found_number = true;
            try std.testing.expectEqual(@as(u32, 0), token.line);
            try std.testing.expectEqual(@as(u32, 4), token.start_char);
            try std.testing.expectEqual(@as(u32, 2), token.length);
        }
    }

    try std.testing.expect(found_variable);
    try std.testing.expect(found_operator);
    try std.testing.expect(found_number);
}

test "extractSemanticTokens multiline" {
    const allocator = std.testing.allocator;
    const source = "x = 1\ny = 2";

    var info = try LineInfo.init(allocator, source);
    defer info.deinit();

    const tokens = try semantic_tokens.extractSemanticTokens(allocator, source, &info);
    defer allocator.free(tokens);

    // Should have tokens on both lines
    var line0_count: usize = 0;
    var line1_count: usize = 0;

    for (tokens) |token| {
        if (token.line == 0) line0_count += 1;
        if (token.line == 1) line1_count += 1;
    }

    try std.testing.expect(line0_count > 0);
    try std.testing.expect(line1_count > 0);
}

test "extractSemanticTokens handles keywords" {
    const allocator = std.testing.allocator;
    const source = "if x else y";

    var info = try LineInfo.init(allocator, source);
    defer info.deinit();

    const tokens = try semantic_tokens.extractSemanticTokens(allocator, source, &info);
    defer allocator.free(tokens);

    // Should have: keyword "if", variable "x", keyword "else", variable "y"
    var keyword_count: usize = 0;
    var variable_count: usize = 0;

    for (tokens) |token| {
        if (token.token_type == @intFromEnum(SemanticType.keyword)) {
            keyword_count += 1;
        }
        if (token.token_type == @intFromEnum(SemanticType.variable)) {
            variable_count += 1;
        }
    }

    try std.testing.expectEqual(@as(usize, 2), keyword_count); // if, else
    try std.testing.expectEqual(@as(usize, 2), variable_count); // x, y
}

test "extractSemanticTokens handles types" {
    const allocator = std.testing.allocator;
    const source = "x : Int";

    var info = try LineInfo.init(allocator, source);
    defer info.deinit();

    const tokens = try semantic_tokens.extractSemanticTokens(allocator, source, &info);
    defer allocator.free(tokens);

    // Should have: variable "x", operator ":", type "Int"
    var found_type = false;

    for (tokens) |token| {
        if (token.token_type == @intFromEnum(SemanticType.type)) {
            found_type = true;
            try std.testing.expectEqual(@as(u32, 3), token.length); // "Int" is 3 chars
        }
    }

    try std.testing.expect(found_type);
}

test "extractSemanticTokens empty source" {
    const allocator = std.testing.allocator;
    const source = "";

    var info = try LineInfo.init(allocator, source);
    defer info.deinit();

    const tokens = try semantic_tokens.extractSemanticTokens(allocator, source, &info);
    defer allocator.free(tokens);

    // Empty source should produce no semantic tokens
    try std.testing.expectEqual(@as(usize, 0), tokens.len);
}
