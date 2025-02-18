//! Helpers for working with S-expressions (symbolic expressions)
//! which are used for our debug Intermediate Representation (IR)
//! and snapshot tests.
const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

/// Represents a token in an S-expression.
pub const Token = union(enum) {
    ident: []const u8,
    value: []const u8,
    lparen,
    rparen,
};

/// Represents an error that can occur during S-expression parsing.
pub const ParseError = error{
    OutOfMemory,
    UnmatchedParentheses,
    EmptyInput,
    InvalidToken,
};

/// Parses an S-expression from a string.
pub fn parse(allocator: Allocator, input: []const u8) ParseError![]Token {
    if (input.len == 0) return ParseError.EmptyInput;

    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var paren_count: i32 = 0;
    var i: usize = 0;
    while (i < input.len) {
        switch (input[i]) {
            ' ', '\t', '\n', '\r' => i += 1,
            '(' => {
                paren_count += 1;

                try tokens.append(.lparen);

                i += 1;
            },
            ')' => {
                paren_count -= 1;

                if (paren_count < 0) return ParseError.UnmatchedParentheses;

                try tokens.append(.rparen);

                i += 1;
            },
            else => {
                const start = i;
                while (i < input.len and !std.ascii.isWhitespace(input[i]) and input[i] != '(' and input[i] != ')') : (i += 1) {}
                const token_str = input[start..i];

                // If the first character is a digit or '-', treat it as a value
                if (token_str.len > 0 and (std.ascii.isDigit(token_str[0]) or token_str[0] == '-')) {
                    const duped = try allocator.dupe(u8, token_str);
                    try tokens.append(.{ .value = duped });
                } else {
                    const duped = try allocator.dupe(u8, token_str);
                    try tokens.append(.{ .ident = duped });
                }
            },
        }
    }

    // Check for unmatched parentheses at the end
    if (paren_count != 0) {
        return ParseError.UnmatchedParentheses;
    }

    return tokens.toOwnedSlice();
}

/// Generates an S-expression string from a list of tokens.
pub fn generate(allocator: Allocator, tokens: []const Token) ParseError![]u8 {
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    for (tokens, 0..) |token, i| {
        switch (token) {
            .lparen => try output.append('('),
            .rparen => {
                try output.append(')');

                // append space only if next token isn't a ')'
                if (i + 1 < tokens.len and tokens[i + 1] != .rparen) {
                    try output.append(' ');
                }
            },
            .ident, .value => |str| {
                try output.appendSlice(str);

                // append space only if next token isn't a ')'
                if (i + 1 < tokens.len and tokens[i + 1] != .rparen) {
                    try output.append(' ');
                }
            },
        }
    }

    return output.toOwnedSlice();
}

pub fn freeTokens(allocator: Allocator, tokens: []Token) void {
    for (tokens) |token| {
        switch (token) {
            .ident, .value => |str| allocator.free(str),
            .lparen, .rparen => {},
        }
    }
    allocator.free(tokens);
}

test "error cases" {
    const allocator = testing.allocator;

    // Empty input
    try testing.expectError(ParseError.EmptyInput, parse(allocator, ""));

    // Unmatched parentheses
    try testing.expectError(ParseError.UnmatchedParentheses, parse(allocator, "("));
    try testing.expectError(ParseError.UnmatchedParentheses, parse(allocator, ")"));
    try testing.expectError(ParseError.UnmatchedParentheses, parse(allocator, "((()"));
}

test "parsing and generating" {
    const allocator = testing.allocator;

    {
        const tokens = try parse(allocator, "()");
        defer freeTokens(allocator, tokens);
        try testing.expectEqual(@as(usize, 2), tokens.len);
        try testing.expect(tokens[0] == .lparen);
        try testing.expect(tokens[1] == .rparen);
    }

    {
        const tokens = try parse(allocator, "(a)");
        defer freeTokens(allocator, tokens);
        try testing.expectEqual(@as(usize, 3), tokens.len);
        try testing.expect(tokens[0] == .lparen);
        try testing.expectEqualStrings("a", tokens[1].ident);
        try testing.expect(tokens[2] == .rparen);
    }

    {
        const tokens = try parse(allocator, "(+ 1 2)");
        defer freeTokens(allocator, tokens);
        try testing.expectEqual(@as(usize, 5), tokens.len);
        try testing.expect(tokens[0] == .lparen);
        try testing.expectEqualStrings("+", tokens[1].ident);
        try testing.expectEqualStrings("1", tokens[2].value);
        try testing.expectEqualStrings("2", tokens[3].value);
        try testing.expect(tokens[4] == .rparen);
    }
}

test "roundtrip" {
    const allocator = testing.allocator;

    const inputs = [_][]const u8{
        "()",
        "(a)",
        "(a b)",
        "(+ 1 2)",
        "(define (square x) (* x x))",
    };

    for (inputs) |input| {
        const tokens = try parse(allocator, input);
        defer freeTokens(allocator, tokens);

        const output = try generate(allocator, tokens);
        defer allocator.free(output);

        // Parse both strings again to compare tokens instead of raw strings
        const input_tokens = try parse(allocator, input);
        defer freeTokens(allocator, input_tokens);
        const output_tokens = try parse(allocator, output);
        defer freeTokens(allocator, output_tokens);

        try testing.expectEqual(input_tokens.len, output_tokens.len);
        for (input_tokens, output_tokens) |in_token, out_token| {
            switch (in_token) {
                .lparen => try testing.expect(out_token == .lparen),
                .rparen => try testing.expect(out_token == .rparen),
                .ident => |in_str| try testing.expectEqualStrings(in_str, out_token.ident),
                .value => |in_str| try testing.expectEqualStrings(in_str, out_token.value),
            }
        }
    }
}

test "complex expressions" {
    const allocator = testing.allocator;

    const inputs = [_][]const u8{
        "(let ((x 1) (y 2)) (+ x y))",
        "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))",
        "(list 1 2 3 4 5)",
        "(quote (a b c))",
        "(lambda (x) (* x x))",
    };

    for (inputs) |input| {
        const tokens = try parse(allocator, input);
        defer freeTokens(allocator, tokens);
        const output = try generate(allocator, tokens);
        defer allocator.free(output);

        // Add specific checks for each case
    }
}

test "whitespace handling" {
    const input = "  (  a   b  \n  c   )  ";
    const tokens = try parse(testing.allocator, input);
    defer freeTokens(testing.allocator, tokens);

    try testing.expectEqual(@as(usize, 5), tokens.len);
    try testing.expect(tokens[0] == .lparen);
    try testing.expectEqualStrings("a", tokens[1].ident);
    try testing.expectEqualStrings("b", tokens[2].ident);
    try testing.expectEqualStrings("c", tokens[3].ident);
    try testing.expect(tokens[4] == .rparen);
}
