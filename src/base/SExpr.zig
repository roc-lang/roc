//! Helpers for working with S-expressions (symbolic expressions)
//! which are used for our debug Intermediate Representation (IR)
//! and snapshot tests.
const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

/// Represents a token in an S-expression.
///
/// This type is comptime generic over two types: `T` for identifiers and `V` for values.
pub fn Token(comptime T: type, comptime V: type) type {
    return union(enum) {
        ident: T,
        value: V,
        lparen,
        rparen,
    };
}

/// Represents an error that can occur during S-expression parsing.
pub const ParseError = error{
    OutOfMemory,
    UnmatchedParentheses,
    EmptyInput,
    InvalidToken,
    ExpectedIdentifier,
    ExpectedOpenParen,
    ExpectedValueOrCloseParen,
};

/// Represents a parser for S-expressions.
pub fn Parser(comptime T: type, comptime V: type) type {
    return struct {
        allocator: Allocator,
        input: []const u8,
        pos: usize,
        tokens: std.ArrayList(Token(T, V)),
        parse_fns: ParseFns,

        const Self = @This();

        /// Provided functions for parsing identifiers and values from bytes.
        pub const ParseFns = struct {
            parseIdent: *const fn ([]const u8) ?T,
            parseValue: *const fn ([]const u8) ?V,
        };

        /// Initializes a new parser instance.
        pub fn init(allocator: Allocator, input: []const u8, parse_fns: ParseFns) Self {
            return .{
                .allocator = allocator,
                .input = input,
                .pos = 0,
                .tokens = std.ArrayList(Token(T, V)).init(allocator),
                .parse_fns = parse_fns,
            };
        }

        /// Deinitializes the parser instance.
        pub fn deinit(self: *Self) void {
            self.tokens.deinit();
        }

        /// Skips whitespace characters in the input.
        fn skipWhitespace(self: *Self) void {
            while (self.pos < self.input.len and std.ascii.isWhitespace(self.input[self.pos])) {
                self.pos += 1;
            }
        }

        /// Returns the current character in the input.
        fn current(self: Self) ?u8 {
            return if (self.pos < self.input.len) self.input[self.pos] else null;
        }

        /// Parses an expression from the input.
        fn parseExpr(self: *Self) ParseError!void {
            self.skipWhitespace();

            if (self.current()) |c| {
                if (c == '(') {
                    try self.tokens.append(.lparen);
                    self.pos += 1;

                    // Parse identifier
                    self.skipWhitespace();
                    const ident_start = self.pos;
                    while (self.pos < self.input.len and
                        !std.ascii.isWhitespace(self.input[self.pos]) and
                        self.input[self.pos] != '(' and
                        self.input[self.pos] != ')')
                    {
                        self.pos += 1;
                    }
                    const ident_str = self.input[ident_start..self.pos];
                    if (ident_str.len == 0) return ParseError.ExpectedIdentifier;

                    if (self.parse_fns.parseIdent(ident_str)) |ident| {
                        try self.tokens.append(.{ .ident = ident });
                    } else {
                        return ParseError.InvalidToken;
                    }

                    // Parse arguments
                    while (true) {
                        self.skipWhitespace();
                        if (self.current()) |next| {
                            if (next == ')') {
                                try self.tokens.append(.rparen);
                                self.pos += 1;
                                break;
                            } else if (next == '(') {
                                try self.parseExpr();
                            } else {
                                // Parse value
                                const value_start = self.pos;
                                while (self.pos < self.input.len and
                                    !std.ascii.isWhitespace(self.input[self.pos]) and
                                    self.input[self.pos] != '(' and
                                    self.input[self.pos] != ')')
                                {
                                    self.pos += 1;
                                }
                                const value_str = self.input[value_start..self.pos];
                                if (value_str.len == 0) return ParseError.ExpectedValueOrCloseParen;

                                if (self.parse_fns.parseValue(value_str)) |value| {
                                    try self.tokens.append(.{ .value = value });
                                } else {
                                    return ParseError.InvalidToken;
                                }
                            }
                        } else {
                            return ParseError.UnmatchedParentheses;
                        }
                    }
                } else {
                    return ParseError.ExpectedOpenParen;
                }
            } else {
                return ParseError.EmptyInput;
            }
        }

        /// Parse the input string into a list of tokens.
        pub fn parse(self: *Self) ParseError![]Token(T, V) {
            if (self.input.len == 0) return ParseError.EmptyInput;

            try self.parseExpr();

            // Check for any remaining non-whitespace characters
            self.skipWhitespace();
            if (self.pos < self.input.len) {
                return ParseError.InvalidToken;
            }

            return self.tokens.toOwnedSlice();
        }
    };
}

pub fn Generator(comptime T: type, comptime V: type) type {
    return struct {
        pub const GenerateFns = struct {
            identToString: fn (T) []const u8,
            valueToString: fn (V) []const u8,
        };

        pub fn generate(
            allocator: Allocator,
            tokens: []const Token(T, V),
            generate_fns: GenerateFns,
        ) ParseError![]u8 {
            var output = std.ArrayList(u8).init(allocator);
            defer output.deinit();

            for (tokens, 0..) |token, i| {
                switch (token) {
                    .lparen => try output.append('('),
                    .rparen => {
                        try output.append(')');
                        if (i + 1 < tokens.len and tokens[i + 1] != .rparen) {
                            try output.append(' ');
                        }
                    },
                    .ident => |ident| {
                        try output.appendSlice(generate_fns.identToString(ident));
                        if (i + 1 < tokens.len and tokens[i + 1] != .rparen) {
                            try output.append(' ');
                        }
                    },
                    .value => |value| {
                        try output.appendSlice(generate_fns.valueToString(value));
                        if (i + 1 < tokens.len and tokens[i + 1] != .rparen) {
                            try output.append(' ');
                        }
                    },
                }
            }

            return output.toOwnedSlice();
        }
    };
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

const TestContext = struct {
    allocator: Allocator,
    value_strings: std.ArrayList([]u8),

    pub fn init(allocator: Allocator) TestContext {
        return .{
            .allocator = allocator,
            .value_strings = std.ArrayList([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *TestContext) void {
        for (self.value_strings.items) |str| {
            self.allocator.free(str);
        }
        self.value_strings.deinit();
    }

    pub fn valueToString(self: *TestContext, value: i64) []const u8 {
        const str = std.fmt.allocPrint(self.allocator, "{d}", .{value}) catch unreachable;
        self.value_strings.append(str) catch unreachable;
        return str;
    }
};

var test_context: TestContext = undefined;

const TestTypes = struct {
    pub const Ident = enum {
        plus,
        minus,
        multiply,
        divide,

        pub fn toString(self: @This()) []const u8 {
            return switch (self) {
                .plus => "+",
                .minus => "-",
                .multiply => "*",
                .divide => "/",
            };
        }

        pub fn fromString(str: []const u8) ?Ident {
            return switch (str[0]) {
                '+' => .plus,
                '-' => .minus,
                '*' => .multiply,
                '/' => .divide,
                else => null,
            };
        }
    };

    pub const Value = i64;

    fn parseIdent(str: []const u8) ?Ident {
        return Ident.fromString(str);
    }

    fn parseValue(str: []const u8) ?Value {
        return std.fmt.parseInt(Value, str, 10) catch null;
    }

    fn identToString(ident: Ident) []const u8 {
        return ident.toString();
    }

    fn valueToString(value: Value) []const u8 {
        return test_context.valueToString(value);
    }

    pub const parse_fns = Parser(Ident, Value).ParseFns{
        .parseIdent = parseIdent,
        .parseValue = parseValue,
    };

    pub const generate_fns = Generator(Ident, Value).GenerateFns{
        .identToString = identToString,
        .valueToString = valueToString,
    };

    /// Helper function to parse and verify for our tests
    pub fn parseAndVerify(
        allocator: Allocator,
        input: []const u8,
        expected_tokens: []const Token(Ident, Value),
    ) !void {

        // Initialize the parser with the provided input and parse functions
        // for our comptime Ident and Value types
        var parser = Parser(Ident, Value).init(allocator, input, parse_fns);
        defer parser.deinit();

        // Try parsing the input
        const tokens = try parser.parse();
        defer allocator.free(tokens);

        // Verify the parsed tokens match the expected tokens
        try testing.expectEqualSlices(Token(Ident, Value), expected_tokens, tokens);

        // Generate the output string from the parsed tokens
        const output = try Generator(Ident, Value).generate(
            allocator,
            tokens,
            generate_fns,
        );
        defer allocator.free(output);

        // Verify the generated output matches the input (round-trip)
        try testing.expectEqualStrings(input, output);
    }
};

test "simple expression" {
    const allocator = testing.allocator;
    const T = TestTypes;

    test_context = TestContext.init(allocator);
    defer test_context.deinit();

    try T.parseAndVerify(allocator, "(+ 1 2)", &[_]Token(T.Ident, T.Value){
        .lparen,
        .{ .ident = .plus },
        .{ .value = 1 },
        .{ .value = 2 },
        .rparen,
    });
}

test "nested expression" {
    const allocator = testing.allocator;
    const T = TestTypes;

    test_context = TestContext.init(allocator);
    defer test_context.deinit();

    try T.parseAndVerify(allocator, "(+ 1 (* 2 3))", &[_]Token(T.Ident, T.Value){
        .lparen,
        .{ .ident = .plus },
        .{ .value = 1 },
        .lparen,
        .{ .ident = .multiply },
        .{ .value = 2 },
        .{ .value = 3 },
        .rparen,
        .rparen,
    });
}

test "complex expression" {
    const allocator = testing.allocator;
    const T = TestTypes;

    test_context = TestContext.init(allocator);
    defer test_context.deinit();

    try T.parseAndVerify(
        allocator,
        "(+ (* 2 3) (/ 10 (- 5 2)))",
        &[_]Token(T.Ident, T.Value){
            .lparen,
            .{ .ident = .plus },
            .lparen,
            .{ .ident = .multiply },
            .{ .value = 2 },
            .{ .value = 3 },
            .rparen,
            .lparen,
            .{ .ident = .divide },
            .{ .value = 10 },
            .lparen,
            .{ .ident = .minus },
            .{ .value = 5 },
            .{ .value = 2 },
            .rparen,
            .rparen,
            .rparen,
        },
    );
}

test "parsing errors" {
    const allocator = testing.allocator;
    const T = TestTypes;

    test_context = TestContext.init(allocator);
    defer test_context.deinit();

    // Empty input
    {
        var parser = Parser(T.Ident, T.Value).init(allocator, "", T.parse_fns);
        defer parser.deinit();
        if (parser.parse()) |tokens| {
            allocator.free(tokens);
            return error.TestExpectedError;
        } else |err| {
            try testing.expectEqual(ParseError.EmptyInput, err);
        }
    }

    // Unmatched parentheses
    {
        var parser = Parser(T.Ident, T.Value).init(allocator, "(+ 1 2", T.parse_fns);
        defer parser.deinit();
        if (parser.parse()) |tokens| {
            allocator.free(tokens);
            return error.TestExpectedError;
        } else |err| {
            try testing.expectEqual(ParseError.UnmatchedParentheses, err);
        }
    }

    // Invalid (unexpected) token
    {
        var parser = Parser(T.Ident, T.Value).init(allocator, "($ 1 2)", T.parse_fns);
        defer parser.deinit();
        if (parser.parse()) |tokens| {
            allocator.free(tokens);
            return error.TestExpectedError;
        } else |err| {
            try testing.expectEqual(ParseError.InvalidToken, err);
        }
    }

    // Expected open parenthesis
    {
        var parser = Parser(T.Ident, T.Value).init(allocator, "1 2 3)", T.parse_fns);
        defer parser.deinit();
        if (parser.parse()) |tokens| {
            allocator.free(tokens);
            return error.TestExpectedError;
        } else |err| {
            try testing.expectEqual(ParseError.ExpectedOpenParen, err);
        }
    }
}
