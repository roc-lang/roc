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
    UnmatchedOpenParen,
    UnmatchedCloseParen,
    EmptyInput,
    InvalidToken,
    ExpectedIdentifier,
    UnexpectedToken,
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

        /// Checks if we've reached the end of input
        fn isAtEnd(self: Self) bool {
            return self.pos >= self.input.len;
        }

        /// Returns the current character without advancing
        fn current(self: Self) ?u8 {
            return if (self.isAtEnd()) null else self.input[self.pos];
        }

        /// Returns true if current character is a delimiter (whitespace or parenthesis)
        fn isDelimiter(self: Self) bool {
            if (self.current()) |c| {
                return std.ascii.isWhitespace(c) or c == '(' or c == ')';
            }
            return true;
        }

        /// Consumes characters until a delimiter is reached and returns the consumed slice
        fn consumeUntilDelimiter(self: *Self) []const u8 {
            const start = self.pos;
            while (!self.isAtEnd() and !self.isDelimiter()) {
                self.pos += 1;
            }
            return self.input[start..self.pos];
        }

        /// Checks if the current character matches the expected one
        fn match(self: *Self, expected: u8) bool {
            if (self.current()) |c| {
                if (c == expected) {
                    self.pos += 1;
                    return true;
                }
            }
            return false;
        }

        fn parseExpr(self: *Self) ParseError!void {
            self.skipWhitespace();

            if (self.current()) |c| {
                if (c == ')') {
                    return ParseError.UnmatchedCloseParen;
                }

                if (c == '(') {
                    try self.tokens.append(.lparen);
                    self.pos += 1;

                    // Skip whitespace before identifier
                    self.skipWhitespace();
                    if (self.isAtEnd()) {
                        return ParseError.UnmatchedOpenParen;
                    }

                    // Must have an identifier after opening paren
                    const ident_str = self.consumeUntilDelimiter();
                    if (ident_str.len == 0) {
                        return ParseError.ExpectedIdentifier;
                    }

                    if (self.parse_fns.parseIdent(ident_str)) |ident| {
                        try self.tokens.append(.{ .ident = ident });
                    } else {
                        return ParseError.InvalidToken;
                    }

                    // Parse arguments
                    while (true) {
                        self.skipWhitespace();

                        // Check for end of input while parsing arguments
                        if (self.isAtEnd()) {
                            return ParseError.UnmatchedOpenParen;
                        }

                        if (self.current()) |next| {
                            if (next == ')') {
                                try self.tokens.append(.rparen);
                                self.pos += 1;
                                break;
                            } else if (next == '(') {
                                // Don't consume the '(' here, let recursive call handle it
                                try self.parseExpr();
                            } else {
                                // Parse value
                                const value_str = self.consumeUntilDelimiter();
                                if (value_str.len == 0) {
                                    return ParseError.InvalidToken;
                                }

                                if (self.parse_fns.parseValue(value_str)) |value| {
                                    try self.tokens.append(.{ .value = value });
                                } else {
                                    return ParseError.InvalidToken;
                                }
                            }
                        } else {
                            return ParseError.UnmatchedOpenParen;
                        }
                    }
                } else {
                    return ParseError.UnexpectedToken;
                }
            } else {
                return ParseError.UnmatchedOpenParen;
            }
        }

        /// Parse the input string into a list of tokens.
        pub fn parse(self: *Self) ParseError![]Token(T, V) {
            // Check for empty or whitespace-only input first
            if (self.input.len == 0) return ParseError.EmptyInput;

            // Skip initial whitespace
            self.skipWhitespace();

            // If we're at the end after skipping whitespace, it was all whitespace
            if (self.pos >= self.input.len) return ParseError.EmptyInput;

            try self.parseExpr();

            // Check for any remaining non-whitespace characters
            self.skipWhitespace();
            if (self.pos < self.input.len) {
                // If we see a closing parenthesis here, it's unmatched
                if (self.input[self.pos] == ')') {
                    return ParseError.UnmatchedCloseParen;
                }
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
                        // Add space after closing paren unless it's the last token
                        // or the next token is another closing paren
                        if (i + 1 < tokens.len and tokens[i + 1] != .rparen) {
                            try output.append(' ');
                        }
                    },
                    .ident => |ident| {
                        try output.appendSlice(generate_fns.identToString(ident));
                        // Add space after ident unless next token is closing paren
                        if (i + 1 < tokens.len and tokens[i + 1] != .rparen) {
                            try output.append(' ');
                        }
                    },
                    .value => |value| {
                        try output.appendSlice(generate_fns.valueToString(value));
                        // Add space after value unless next token is closing paren
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

test "parsing - basic cases" {
    const allocator = testing.allocator;
    const T = TestTypes;

    test_context = TestContext.init(allocator);
    defer test_context.deinit();

    // Test cases with their normalized (expected) output
    const TestCase = struct {
        input: []const u8,
        expected: []const u8,
        tokens: []const Token(T.Ident, T.Value),
    };

    const test_cases = [_]TestCase{
        .{
            .input = "(+ 1 2)",
            .expected = "(+ 1 2)",
            .tokens = &[_]Token(T.Ident, T.Value){
                .lparen,
                .{ .ident = .plus },
                .{ .value = 1 },
                .{ .value = 2 },
                .rparen,
            },
        },
        .{
            .input = "(  +    1     2   )",
            .expected = "(+ 1 2)",
            .tokens = &[_]Token(T.Ident, T.Value){
                .lparen,
                .{ .ident = .plus },
                .{ .value = 1 },
                .{ .value = 2 },
                .rparen,
            },
        },
        .{
            .input = "(+(- 1 2)3)",
            .expected = "(+ (- 1 2) 3)",
            .tokens = &[_]Token(T.Ident, T.Value){
                .lparen,
                .{ .ident = .plus },
                .lparen,
                .{ .ident = .minus },
                .{ .value = 1 },
                .{ .value = 2 },
                .rparen,
                .{ .value = 3 },
                .rparen,
            },
        },
    };

    for (test_cases) |case| {
        var parser = Parser(T.Ident, T.Value).init(allocator, case.input, T.parse_fns);
        defer parser.deinit();

        const tokens = try parser.parse();
        defer allocator.free(tokens);

        try testing.expectEqualSlices(Token(T.Ident, T.Value), case.tokens, tokens);

        const output = try Generator(T.Ident, T.Value).generate(
            allocator,
            tokens,
            T.generate_fns,
        );
        defer allocator.free(output);

        try testing.expectEqualStrings(case.expected, output);
    }
}

test "parsing - nested expressions" {
    const allocator = testing.allocator;
    const T = TestTypes;

    test_context = TestContext.init(allocator);
    defer test_context.deinit();

    // Simple nested expression
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

    // Multiple nested expressions
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

    // Deeply nested expression
    try T.parseAndVerify(
        allocator,
        "(+ 1 (- 2 (* 3 (/ 4 5))))",
        &[_]Token(T.Ident, T.Value){
            .lparen,
            .{ .ident = .plus },
            .{ .value = 1 },
            .lparen,
            .{ .ident = .minus },
            .{ .value = 2 },
            .lparen,
            .{ .ident = .multiply },
            .{ .value = 3 },
            .lparen,
            .{ .ident = .divide },
            .{ .value = 4 },
            .{ .value = 5 },
            .rparen,
            .rparen,
            .rparen,
            .rparen,
        },
    );
}

test "parsing - error cases" {
    const allocator = testing.allocator;
    const T = TestTypes;

    test_context = TestContext.init(allocator);
    defer test_context.deinit();

    // Test cases with expected errors
    const ErrorTestCase = struct {
        input: []const u8,
        expected_error: ParseError,
        description: []const u8, // Added for better test documentation
    };

    const error_cases = [_]ErrorTestCase{
        // Empty input cases
        .{
            .input = "",
            .expected_error = ParseError.EmptyInput,
            .description = "empty string",
        },
        .{
            .input = "    ",
            .expected_error = ParseError.EmptyInput,
            .description = "only whitespace",
        },

        // Unmatched parentheses cases
        .{
            .input = "(+ 1 2))",
            .expected_error = ParseError.UnmatchedCloseParen,
            .description = "extra closing parenthesis",
        },
        .{
            .input = ")",
            .expected_error = ParseError.UnmatchedCloseParen,
            .description = "lone closing parenthesis",
        },
        .{
            .input = "(+ 1 2",
            .expected_error = ParseError.UnmatchedOpenParen,
            .description = "missing closing parenthesis",
        },
        .{
            .input = "((+ 1 2)",
            .expected_error = ParseError.ExpectedIdentifier,
            .description = "should start with an identifier",
        },

        // Invalid token cases
        .{
            .input = "($ 1 2)",
            .expected_error = ParseError.InvalidToken,
            .description = "invalid operator",
        },
        .{
            .input = "(+ a 2)",
            .expected_error = ParseError.InvalidToken,
            .description = "invalid first argument",
        },
        .{
            .input = "(+ 1 b)",
            .expected_error = ParseError.InvalidToken,
            .description = "invalid second argument",
        },

        // Missing required elements cases
        .{
            .input = "()",
            .expected_error = ParseError.ExpectedIdentifier,
            .description = "empty expression",
        },
        .{
            .input = "( )",
            .expected_error = ParseError.ExpectedIdentifier,
            .description = "empty expression with whitespace",
        },

        // Invalid structure cases
        .{
            .input = "1 2 3",
            .expected_error = ParseError.UnexpectedToken,
            .description = "bare values without expression",
        },
        .{
            .input = "+ 1 2",
            .expected_error = ParseError.UnexpectedToken,
            .description = "bare operator expression",
        },
    };

    for (error_cases) |case| {
        var parser = Parser(T.Ident, T.Value).init(allocator, case.input, T.parse_fns);
        defer parser.deinit();

        const result = parser.parse();

        // Add error context
        if (result) |tokens| {
            std.debug.print("\nTest failed: {s}\nInput: '{s}'\nExpected error: {}, but parsing succeeded\n", .{ case.description, case.input, case.expected_error });
            allocator.free(tokens);
            return error.TestUnexpectedSuccess;
        } else |err| {
            if (err != case.expected_error) {
                std.debug.print("\nTest failed: {s}\nInput: '{s}'\nExpected error: {}, got: {}\n", .{ case.description, case.input, case.expected_error, err });
                return error.TestExpectedError;
            }
        }
    }
}

test "generation - formatting" {
    const allocator = testing.allocator;
    const T = TestTypes;

    test_context = TestContext.init(allocator);
    defer test_context.deinit();

    // Test different formatting patterns
    const format_cases = [_][]const u8{
        "(+ 1 2)",
        "(+ (* 2 3) 4)",
        "(+ 1 (* 2 (- 3 4)))",
        "(+ 1 2 3 4 5)",
        "(* (+ 1 2) (- 3 4))",
    };

    for (format_cases) |input| {
        var parser = Parser(T.Ident, T.Value).init(allocator, input, T.parse_fns);
        defer parser.deinit();

        const tokens = try parser.parse();
        defer allocator.free(tokens);

        const output = try Generator(T.Ident, T.Value).generate(
            allocator,
            tokens,
            T.generate_fns,
        );
        defer allocator.free(output);

        try testing.expectEqualStrings(input, output);
    }
}
