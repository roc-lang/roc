//! Tests for semantic token extraction and delta encoding.

const std = @import("std");
const Allocator = std.mem.Allocator;
const semantic_tokens = @import("lsp").semantic_tokens;
const line_info = @import("lsp").line_info;
const tokenize = @import("parse").tokenize;

const Token = tokenize.Token;
const SemanticToken = semantic_tokens.SemanticToken;
const SemanticType = semantic_tokens.SemanticType;
const LineInfo = line_info.LineInfo;

test "semantic tokens do not read file imports" {
    const allocator = std.testing.allocator;
    const source = "import \"does-not-exist.txt\" as input : Str\nmain = input";

    var info = try LineInfo.init(allocator, source);
    defer info.deinit();

    const tokens = try semantic_tokens.extractSemanticTokensWithImports(
        allocator,
        source,
        &info,
        null,
    );
    defer allocator.free(tokens);

    try std.testing.expect(tokens.len > 0);
}

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

test "extractSemanticTokens keeps field access and method calls distinct" {
    const allocator = std.testing.allocator;
    const source = "field = value.field\nmethod = value.method()";

    var info = try LineInfo.init(allocator, source);
    defer info.deinit();

    const tokens = try semantic_tokens.extractSemanticTokens(allocator, source, &info);
    defer allocator.free(tokens);

    var found_field = false;
    var found_method = false;
    for (tokens) |token| {
        if (token.line == 0 and token.token_type == @intFromEnum(SemanticType.property)) {
            found_field = true;
        }
        if (token.line == 1 and token.token_type == @intFromEnum(SemanticType.function)) {
            found_method = true;
        }
    }

    try std.testing.expect(found_field);
    try std.testing.expect(found_method);
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

test "semantic tokens survive invalid utf8 in the source" {
    const allocator = std.testing.allocator;
    // 0xFF starts no UTF-8 sequence. The tokenizer reports InvalidUtf8InSource
    // and keeps going, so the offsets of every later token are converted
    // against a line holding that byte. That conversion used to abort the
    // server instead of returning a column.
    const source = "main = \"a\xffb\"\nnext = 1";

    var info = try LineInfo.init(allocator, source);
    defer info.deinit();

    const tokens = try semantic_tokens.extractSemanticTokensWithImports(
        allocator,
        source,
        &info,
        null,
    );
    defer allocator.free(tokens);

    try std.testing.expect(tokens.len > 0);
}

// Parse-tree classification

const Expectation = struct {
    /// Zero-based line of the token.
    line: u32,
    /// Text that starts where the token starts; it may run past the token so
    /// that one occurrence on the line can be told from another.
    at: []const u8,
    /// The token's text, or null when nothing may be reported there.
    token: ?[]const u8,
    type: SemanticType = .variable,
    modifiers: u32 = 0,
};

fn lineText(source: []const u8, line: u32) []const u8 {
    var start: usize = 0;
    var current: u32 = 0;
    while (current < line) : (current += 1) {
        start = std.mem.findScalarPos(u8, source, start, '\n').? + 1;
    }
    const end = std.mem.findScalarPos(u8, source, start, '\n') orelse source.len;
    return source[start..end];
}

/// Length in UTF-16 code units, which is how LSP measures columns and lengths.
fn utf16Len(text: []const u8) u32 {
    var view = std.unicode.Utf8View.initUnchecked(text).iterator();
    var len: u32 = 0;
    while (view.nextCodepoint()) |codepoint| len += if (codepoint >= 0x10000) 2 else 1;
    return len;
}

const ExpectError = Allocator.Error || error{ TextNotOnLine, UnexpectedToken, TokenNotFound, TestExpectedEqual };

fn expectTokens(source: []const u8, expectations: []const Expectation) ExpectError!void {
    const allocator = std.testing.allocator;
    var info = try LineInfo.init(allocator, source);
    defer info.deinit();
    const tokens = try semantic_tokens.extractSemanticTokens(allocator, source, &info);
    defer allocator.free(tokens);

    for (expectations) |expected| {
        const text_of_line = lineText(source, expected.line);
        const byte_column = std.mem.find(u8, text_of_line, expected.at) orelse {
            std.debug.print("`{s}` is not on line {d}\n", .{ expected.at, expected.line });
            return error.TextNotOnLine;
        };
        const column = utf16Len(text_of_line[0..byte_column]);
        const found = for (tokens) |token| {
            if (token.line == expected.line and token.start_char == column) break token;
        } else null;

        const text = expected.token orelse {
            if (found != null) {
                std.debug.print("unexpected token at `{s}` on line {d}\n", .{ expected.at, expected.line });
                return error.UnexpectedToken;
            }
            continue;
        };
        const token = found orelse {
            std.debug.print("no token at `{s}` on line {d}\n", .{ expected.at, expected.line });
            return error.TokenNotFound;
        };
        errdefer std.debug.print("at `{s}` on line {d}\n", .{ expected.at, expected.line });
        try std.testing.expectEqual(utf16Len(text), token.length);
        try std.testing.expectEqual(expected.type, @as(SemanticType, @enumFromInt(token.token_type)));
        try std.testing.expectEqual(expected.modifiers, token.modifiers);
    }
}

test "functions, parameters and values are told apart" {
    try expectTokens(
        \\double : I64 -> I64
        \\double = |n| n * 2
        \\limit : I64
        \\limit = 10
        \\result = double(limit)
    , &.{
        .{ .line = 0, .at = "double", .token = "double", .type = .function },
        .{ .line = 1, .at = "double", .token = "double", .type = .function },
        .{ .line = 1, .at = "n|", .token = "n", .type = .parameter },
        .{ .line = 1, .at = "n *", .token = "n", .type = .parameter },
        .{ .line = 2, .at = "limit", .token = "limit", .type = .variable },
        .{ .line = 3, .at = "limit", .token = "limit", .type = .variable },
        .{ .line = 4, .at = "result", .token = "result", .type = .variable },
        .{ .line = 4, .at = "double", .token = "double", .type = .function },
        .{ .line = 4, .at = "limit", .token = "limit", .type = .variable },
    });
}

test "a function is recognised before its definition" {
    try expectTokens(
        \\first = second(1)
        \\second = |n| n
    , &.{
        .{ .line = 0, .at = "second", .token = "second", .type = .function },
    });
}

test "an effectful function keeps its bang" {
    try expectTokens(
        \\log! : Str => {}
        \\log! = |msg| {}
        \\main! = |_| log!("hi")
    , &.{
        .{ .line = 0, .at = "log!", .token = "log!", .type = .function },
        .{ .line = 1, .at = "log!", .token = "log!", .type = .function },
        .{ .line = 2, .at = "log!", .token = "log!", .type = .function },
    });
}

test "a parameter stays a parameter when it is called" {
    try expectTokens(
        \\apply = |transform, value| transform(value)
    , &.{
        .{ .line = 0, .at = "transform(", .token = "transform", .type = .parameter },
        .{ .line = 0, .at = "value)", .token = "value", .type = .parameter },
    });
}

test "a parameter does not leak out of its lambda" {
    try expectTokens(
        \\first = |total| total
        \\total = 1
        \\other = total
    , &.{
        .{ .line = 0, .at = "total|", .token = "total", .type = .parameter },
        .{ .line = 2, .at = "total", .token = "total", .type = .variable },
    });
}

test "destructured parameters are parameters and their fields are properties" {
    try expectTokens(
        \\sum = |{ x, y: renamed }, (a, b), [first, .. as rest]| x
    , &.{
        .{ .line = 0, .at = "x,", .token = "x", .type = .parameter },
        .{ .line = 0, .at = "y:", .token = "y", .type = .property },
        .{ .line = 0, .at = "renamed", .token = "renamed", .type = .parameter },
        .{ .line = 0, .at = "a,", .token = "a", .type = .parameter },
        .{ .line = 0, .at = "b)", .token = "b", .type = .parameter },
        .{ .line = 0, .at = "first", .token = "first", .type = .parameter },
        .{ .line = 0, .at = "rest", .token = "rest", .type = .parameter },
        .{ .line = 0, .at = "x", .token = "x", .type = .parameter },
    });
}

test "tags are enum members and types are types" {
    try expectTokens(
        \\Shape : [Circle(F64), Dot]
        \\area : Shape -> F64
        \\area = |shape| match shape {
        \\    Circle(r) if r > 0.0 => r
        \\    Dot => 0.0
        \\}
        \\flag = True
        \\wrapped = Ok(Dot)
    , &.{
        .{ .line = 0, .at = "Shape", .token = "Shape", .type = .type, .modifiers = semantic_tokens.modifier_declaration },
        .{ .line = 0, .at = "Circle", .token = "Circle", .type = .enumMember },
        .{ .line = 0, .at = "F64", .token = "F64", .type = .type },
        .{ .line = 0, .at = "Dot", .token = "Dot", .type = .enumMember },
        .{ .line = 1, .at = "Shape", .token = "Shape", .type = .type },
        .{ .line = 3, .at = "Circle", .token = "Circle", .type = .enumMember },
        .{ .line = 3, .at = "r)", .token = "r", .type = .variable },
        .{ .line = 3, .at = "r >", .token = "r", .type = .variable },
        .{ .line = 4, .at = "Dot", .token = "Dot", .type = .enumMember },
        .{ .line = 6, .at = "True", .token = "True", .type = .enumMember },
        .{ .line = 7, .at = "Ok", .token = "Ok", .type = .enumMember },
        .{ .line = 7, .at = "Dot", .token = "Dot", .type = .enumMember },
    });
}

test "a qualified tag has a type qualifier" {
    try expectTokens(
        \\flag = Bool.True
        \\kind : Audit.Kind
    , &.{
        .{ .line = 0, .at = "Bool", .token = "Bool", .type = .type },
        .{ .line = 0, .at = ".True", .token = null },
        .{ .line = 0, .at = "True", .token = "True", .type = .enumMember },
        .{ .line = 1, .at = "Audit", .token = "Audit", .type = .type },
        .{ .line = 1, .at = "Kind", .token = "Kind", .type = .type },
    });
}

test "nominal and opaque declarations and their associated items" {
    try expectTokens(
        \\Counter := { count : U64 }.{
        \\    Step :: [Up, Down]
        \\
        \\    bump : Counter -> Counter
        \\    bump = |c| helper(c)
        \\
        \\    helper = |c| c
        \\
        \\    zero : Counter
        \\    zero = { count: 0 }
        \\}
    , &.{
        .{ .line = 0, .at = "Counter", .token = "Counter", .type = .type, .modifiers = semantic_tokens.modifier_declaration },
        .{ .line = 0, .at = ":=", .token = ":=", .type = .operator },
        .{ .line = 0, .at = "count", .token = "count", .type = .property },
        .{ .line = 1, .at = "Step", .token = "Step", .type = .type, .modifiers = semantic_tokens.modifier_declaration },
        .{ .line = 1, .at = "Up", .token = "Up", .type = .enumMember },
        .{ .line = 3, .at = "bump", .token = "bump", .type = .function },
        .{ .line = 3, .at = "Counter", .token = "Counter", .type = .type },
        .{ .line = 4, .at = "helper", .token = "helper", .type = .function },
        .{ .line = 8, .at = "zero", .token = "zero", .type = .variable },
        .{ .line = 9, .at = "count", .token = "count", .type = .property },
    });
}

test "type variables are type parameters" {
    try expectTokens(
        \\Pair(a) : (a, a)
        \\swap : Pair(a), (a -> b) -> List(b)
        \\show : a -> Str where [a.to_str : a -> Str]
    , &.{
        .{ .line = 0, .at = "Pair", .token = "Pair", .type = .type, .modifiers = semantic_tokens.modifier_declaration },
        .{ .line = 0, .at = "a)", .token = "a", .type = .typeParameter },
        .{ .line = 0, .at = "a,", .token = "a", .type = .typeParameter },
        .{ .line = 1, .at = "Pair", .token = "Pair", .type = .type },
        .{ .line = 1, .at = "b)", .token = "b", .type = .typeParameter },
        .{ .line = 1, .at = "List", .token = "List", .type = .type },
        .{ .line = 2, .at = "a ->", .token = "a", .type = .typeParameter },
        .{ .line = 2, .at = "a.to_str", .token = "a", .type = .typeParameter },
        .{ .line = 2, .at = "to_str", .token = "to_str", .type = .function },
    });
}

test "record fields are properties and the dot is not part of the name" {
    try expectTokens(
        \\Config : { host : Str, port : U16 }
        \\make = |h| { host: h, port: 80 }
        \\read = |config| config.host
        \\both = |host, port| { host, port }
        \\update = |config| { ..config, port: 81 }
    , &.{
        .{ .line = 0, .at = "host", .token = "host", .type = .property },
        .{ .line = 0, .at = "port", .token = "port", .type = .property },
        .{ .line = 1, .at = "host", .token = "host", .type = .property },
        .{ .line = 1, .at = "h,", .token = "h", .type = .parameter },
        .{ .line = 2, .at = "config.", .token = "config", .type = .parameter },
        .{ .line = 2, .at = ".host", .token = null },
        .{ .line = 2, .at = "host", .token = "host", .type = .property },
        .{ .line = 3, .at = "host, port }", .token = "host", .type = .property },
        .{ .line = 4, .at = "config,", .token = "config", .type = .parameter },
        .{ .line = 4, .at = "port", .token = "port", .type = .property },
    });
}

test "method calls are functions and the dot is not part of the name" {
    try expectTokens(
        \\size = |items| items.len().to_str()
        \\parsed = U64.from_str("1")
    , &.{
        .{ .line = 0, .at = ".len", .token = null },
        .{ .line = 0, .at = "len", .token = "len", .type = .function },
        .{ .line = 0, .at = "to_str", .token = "to_str", .type = .function },
        .{ .line = 1, .at = "U64", .token = "U64", .type = .type },
        .{ .line = 1, .at = "from_str", .token = "from_str", .type = .function },
    });
}

test "delimiters are not operators" {
    try expectTokens(
        \\id : I64 -> I64
        \\id = |n| n
        \\pair = { a: 1 }
    , &.{
        .{ .line = 0, .at = ":", .token = null },
        .{ .line = 0, .at = "->", .token = "->", .type = .operator },
        .{ .line = 1, .at = "=", .token = "=", .type = .operator },
        .{ .line = 1, .at = "|n", .token = null },
        .{ .line = 1, .at = "| n", .token = null },
        .{ .line = 2, .at = ": 1", .token = null },
    });
}

test "operators" {
    try expectTokens(
        \\check = |a, b| a + b * 2 >= 3 and a != b or !(a == b)
        \\range = 1..=3
        \\other = 1..<3
        \\fallback = |r| r ?? 0
    , &.{
        .{ .line = 0, .at = "+", .token = "+", .type = .operator },
        .{ .line = 0, .at = ">=", .token = ">=", .type = .operator },
        .{ .line = 0, .at = "and", .token = "and", .type = .operator },
        .{ .line = 0, .at = "!=", .token = "!=", .type = .operator },
        .{ .line = 0, .at = "or", .token = "or", .type = .operator },
        .{ .line = 0, .at = "==", .token = "==", .type = .operator },
        .{ .line = 1, .at = "..=", .token = "..=", .type = .operator },
        .{ .line = 2, .at = "..<", .token = "..<", .type = .operator },
        .{ .line = 3, .at = "??", .token = "??", .type = .operator },
    });
}

test "number literals keep their type suffix" {
    try expectTokens(
        \\small = 5.U8
        \\hex = 0x1F.U8
        \\frac = 12.5.Dec
        \\plain = 1_000
        \\second = pair.1
    , &.{
        .{ .line = 0, .at = "5", .token = "5", .type = .number },
        .{ .line = 0, .at = ".U8", .token = ".U8", .type = .number },
        .{ .line = 1, .at = "0x1F", .token = "0x1F", .type = .number },
        .{ .line = 1, .at = ".U8", .token = ".U8", .type = .number },
        .{ .line = 2, .at = "12.5", .token = "12.5", .type = .number },
        .{ .line = 2, .at = ".Dec", .token = ".Dec", .type = .number },
        .{ .line = 3, .at = "1_000", .token = "1_000", .type = .number },
        .{ .line = 4, .at = ".1", .token = null },
        .{ .line = 4, .at = "1", .token = "1", .type = .number },
    });
}

test "keywords" {
    try expectTokens(
        \\run = |items| {
        \\    var $total = 0
        \\    for item in items {
        \\        $total = $total + item
        \\    }
        \\    if $total > 1 { return $total } else { 0 }
        \\}
        \\expect run([1]) == 1
    , &.{
        .{ .line = 1, .at = "var", .token = "var", .type = .keyword },
        .{ .line = 1, .at = "$total", .token = "$total", .type = .variable },
        .{ .line = 2, .at = "for", .token = "for", .type = .keyword },
        .{ .line = 2, .at = "item ", .token = "item", .type = .variable },
        .{ .line = 2, .at = "in ", .token = "in", .type = .keyword },
        .{ .line = 2, .at = "items", .token = "items", .type = .parameter },
        .{ .line = 3, .at = "+ item", .token = "+", .type = .operator },
        .{ .line = 5, .at = "if", .token = "if", .type = .keyword },
        .{ .line = 5, .at = "return", .token = "return", .type = .keyword },
        .{ .line = 5, .at = "else", .token = "else", .type = .keyword },
        .{ .line = 7, .at = "expect", .token = "expect", .type = .keyword },
        .{ .line = 7, .at = "run", .token = "run", .type = .function },
    });
}

test "destructuring assignments bind variables" {
    try expectTokens(
        \\unwrap = |wrapped| {
        \\    Wrapped(inner) = wrapped
        \\    (left, right) = inner
        \\    left
        \\}
    , &.{
        .{ .line = 1, .at = "Wrapped", .token = "Wrapped", .type = .enumMember },
        .{ .line = 1, .at = "inner", .token = "inner", .type = .variable },
        .{ .line = 1, .at = "wrapped", .token = "wrapped", .type = .parameter },
        .{ .line = 2, .at = "left", .token = "left", .type = .variable },
        .{ .line = 2, .at = "inner", .token = "inner", .type = .variable },
        .{ .line = 3, .at = "left", .token = "left", .type = .variable },
    });
}

test "a nominal destructure names a type" {
    try expectTokens(
        \\get_x = |Point.{ x }| x
    , &.{
        .{ .line = 0, .at = "Point", .token = "Point", .type = .type },
        .{ .line = 0, .at = "x }", .token = "x", .type = .parameter },
    });
}

test "imports" {
    try expectTokens(
        \\import pf.Stdout
        \\import Src/Widget.Err as WE
        \\import ./Internal/Client exposing [send, Request as Req]
        \\import "data.txt" as data : Str
    , &.{
        .{ .line = 0, .at = "import", .token = "import", .type = .keyword },
        .{ .line = 0, .at = "pf", .token = "pf", .type = .namespace },
        .{ .line = 0, .at = ".Stdout", .token = null },
        .{ .line = 0, .at = "Stdout", .token = "Stdout", .type = .type },
        .{ .line = 1, .at = "Src", .token = "Src", .type = .type },
        .{ .line = 1, .at = "/", .token = null },
        .{ .line = 1, .at = "Widget", .token = "Widget", .type = .type },
        .{ .line = 1, .at = "Err", .token = "Err", .type = .type },
        .{ .line = 1, .at = "as", .token = "as", .type = .keyword },
        .{ .line = 1, .at = "WE", .token = "WE", .type = .type },
        .{ .line = 2, .at = "Internal", .token = "Internal", .type = .type },
        .{ .line = 2, .at = "Client", .token = "Client", .type = .type },
        .{ .line = 2, .at = "exposing", .token = "exposing", .type = .keyword },
        .{ .line = 2, .at = "Request", .token = "Request", .type = .type },
        .{ .line = 2, .at = "Req]", .token = "Req", .type = .type },
        .{ .line = 3, .at = "data :", .token = "data", .type = .variable },
        .{ .line = 3, .at = "Str", .token = "Str", .type = .type },
    });
}

test "strings leave escapes and interpolated code to their own tokens" {
    try expectTokens(
        \\greet = |name| "hi\n${name.trim()}\u(00A0)!"
    , &.{
        .{ .line = 0, .at = "hi", .token = "hi", .type = .string },
        .{ .line = 0, .at = "\\n", .token = null },
        .{ .line = 0, .at = "name.", .token = "name", .type = .parameter },
        .{ .line = 0, .at = "trim", .token = "trim", .type = .function },
        .{ .line = 0, .at = "\\u", .token = null },
        .{ .line = 0, .at = "!\"", .token = "!", .type = .string },
    });
}

test "token positions and lengths count UTF-16 code units" {
    try expectTokens(
        \\text = "é😀"
        \\after = "😀" == text
    , &.{
        // One code unit for é and two for the emoji, not their UTF-8 byte counts.
        .{ .line = 0, .at = "é", .token = "é😀", .type = .string },
        .{ .line = 1, .at = "==", .token = "==", .type = .operator },
    });
}

test "classification survives a syntax error" {
    try expectTokens(
        \\good = |n| n
        \\bad = = =
        \\after = good(1)
    , &.{
        .{ .line = 0, .at = "good", .token = "good", .type = .function },
        .{ .line = 2, .at = "good", .token = "good", .type = .function },
    });
}

test "tokens are sorted and never overlap" {
    const allocator = std.testing.allocator;
    const source =
        \\Shape : [Circle(F64), Dot]
        \\area = |shape| match shape {
        \\    Circle(r) => "r=${r.to_str()}\n"
        \\    Dot => 5.U8.to_str()
        \\}
    ;
    var info = try LineInfo.init(allocator, source);
    defer info.deinit();
    const tokens = try semantic_tokens.extractSemanticTokens(allocator, source, &info);
    defer allocator.free(tokens);

    try std.testing.expect(tokens.len > 0);
    for (tokens[1..], tokens[0 .. tokens.len - 1]) |token, previous| {
        try std.testing.expect(token.length > 0);
        if (token.line == previous.line) {
            try std.testing.expect(token.start_char >= previous.start_char + previous.length);
        } else {
            try std.testing.expect(token.line > previous.line);
        }
    }
}

test "every advertised token type and modifier has an index" {
    const capabilities = @import("lsp").capabilities;
    inline for (@typeInfo(SemanticType).@"enum".fields) |field| {
        try std.testing.expectEqualStrings(field.name, capabilities.TOKEN_TYPES[field.value]);
    }
    try std.testing.expectEqual(@typeInfo(SemanticType).@"enum".fields.len, capabilities.TOKEN_TYPES.len);
    try std.testing.expectEqualStrings("declaration", capabilities.TOKEN_MODIFIERS[0]);
    try std.testing.expectEqual(@as(u32, 1), semantic_tokens.modifier_declaration);
}
