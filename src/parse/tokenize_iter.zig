//! Iterator-based tokenization functionality for the Roc parser.
//!
//! This module provides an iterator-based tokenizer that produces tokens one at a time
//! instead of creating a full list in memory. It's designed for Parser2 which only needs
//! 1-token lookahead.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const tokenize = @import("tokenize2.zig");

// Re-export types from the original tokenizer
pub const Token = tokenize.Token;
pub const Diagnostic = tokenize.Diagnostic;
pub const CommonEnv = base.CommonEnv;
pub const StringKind = tokenize.StringKind;

/// Iterator-based tokenizer that produces tokens one at a time
pub const TokenIterator = struct {
    cursor: tokenize.Cursor,
    string_interpolation_stack: std.ArrayListUnmanaged(tokenize.StringKind),
    env: *CommonEnv,
    byte_slices: *collections.ByteSlices,
    finished: bool,

    /// Create a new token iterator
    pub fn init(env: *CommonEnv, gpa: std.mem.Allocator, text: []const u8, messages: []Diagnostic, byte_slices: *collections.ByteSlices) std.mem.Allocator.Error!TokenIterator {
        _ = gpa; // Iterator doesn't allocate at init
        const cursor = tokenize.Cursor.init(text, messages);

        return TokenIterator{
            .cursor = cursor,
            .string_interpolation_stack = .{},
            .env = env,
            .byte_slices = byte_slices,
            .finished = false,
        };
    }

    pub fn deinit(self: *TokenIterator, gpa: std.mem.Allocator) void {
        self.string_interpolation_stack.deinit(gpa);
    }

    /// Get the next token from the iterator
    pub fn next(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!?Token {
        if (self.finished) return null;

        // Check if we're at end of file
        if (self.cursor.pos >= self.cursor.buf.len) {
            self.finished = true;
            return Token{
                .tag = .EndOfFile,
                .region = base.Region.from_raw_offsets(@intCast(self.cursor.buf.len), @intCast(self.cursor.buf.len)),
                .extra = .{ .none = 0 },
            };
        }

        // Tokenize a single token using the existing tokenization logic
        const start_pos = self.cursor.pos;
        const token_result = try self.tokenizeOne(gpa);

        // If no progress was made, we're done
        if (self.cursor.pos == start_pos) {
            self.finished = true;
            return null;
        }

        return token_result;
    }

    /// Get diagnostic messages up to the current position
    pub fn getMessages(self: *const TokenIterator) []const Diagnostic {
        const actual_count = @min(self.cursor.message_count, self.cursor.messages.len);
        return self.cursor.messages[0..actual_count];
    }

    /// Check if there are more messages than the buffer can hold
    pub fn hasDroppedMessages(self: *const TokenIterator) bool {
        return self.cursor.message_count > self.cursor.messages.len;
    }

    /// Tokenize a single token (internal implementation)
    fn tokenizeOne(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        // Skip whitespace and comments first
        self.skipWhitespaceAndComments();

        if (self.cursor.pos >= self.cursor.buf.len) {
            self.finished = true;
            return Token{
                .tag = .EndOfFile,
                .region = base.Region.from_raw_offsets(@intCast(self.cursor.buf.len), @intCast(self.cursor.buf.len)),
                .extra = .{ .none = 0 },
            };
        }

        const start_pos = self.cursor.pos;
        const byte = self.cursor.buf[self.cursor.pos];

        // Use the existing tokenization logic adapted for single-token production
        switch (byte) {
            '"' => return try self.tokenizeString(gpa),
            '\'' => return try self.tokenizeSingleQuote(gpa),
            '0'...'9' => return try self.tokenizeNumber(gpa),
            'a'...'z', 'A'...'Z', '_' => return try self.tokenizeIdent(gpa),
            '(' => return self.tokenizeSimple(.OpenRound),
            ')' => return self.tokenizeSimple(.CloseRound),
            '[' => return self.tokenizeSimple(.OpenSquare),
            ']' => return self.tokenizeSimple(.CloseSquare),
            '{' => return self.tokenizeSimple(.OpenCurly),
            '}' => return self.tokenizeSimple(.CloseCurly),
            ',' => return self.tokenizeSimple(.Comma),
            '.' => return try self.tokenizeDot(gpa),
            '+' => return self.tokenizeSimple(.OpPlus),
            '-' => return try self.tokenizeMinus(gpa),
            '*' => return self.tokenizeSimple(.OpStar),
            '/' => return try self.tokenizeSlash(gpa),
            '%' => return self.tokenizeSimple(.OpPercent),
            '^' => return self.tokenizeSimple(.OpCaret),
            '=' => return try self.tokenizeEquals(gpa),
            '!' => return try self.tokenizeBang(gpa),
            '<' => return try self.tokenizeLessThan(gpa),
            '>' => return try self.tokenizeGreaterThan(gpa),
            '&' => return try self.tokenizeAmpersand(gpa),
            '|' => return try self.tokenizeBar(gpa),
            '?' => return try self.tokenizeQuestion(gpa),
            ':' => return try self.tokenizeColon(gpa),
            '\\' => return self.tokenizeSimple(.OpBackslash),
            '#' => {
                // Comment - skip to end of line
                self.skipComment();
                // Try again after comment
                return try self.tokenizeOne(gpa);
            },
            else => {
                // Handle unexpected character
                self.cursor.pos += 1;
                return Token{
                    .tag = .MalformedUnknownToken,
                    .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                    .extra = .{ .none = 0 },
                };
            },
        }
    }

    fn skipWhitespaceAndComments(self: *TokenIterator) void {
        while (self.cursor.pos < self.cursor.buf.len) {
            const byte = self.cursor.buf[self.cursor.pos];
            if (tokenize.Token.isWhitespace(byte)) {
                self.cursor.pos += 1;
            } else if (byte == '#') {
                self.skipComment();
            } else {
                break;
            }
        }
    }

    fn skipComment(self: *TokenIterator) void {
        while (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] != '\n') {
            self.cursor.pos += 1;
        }
        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '\n') {
            self.cursor.pos += 1; // Skip the newline
        }
    }

    fn tokenizeSimple(self: *TokenIterator, tag: Token.Tag) Token {
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;
        return Token{
            .tag = tag,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    // Placeholder implementations - these would be adapted from the existing tokenizer
    fn tokenizeString(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        // For now, just a simple string tokenization
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1; // Skip opening quote

        while (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] != '"') {
            if (self.cursor.buf[self.cursor.pos] == '\\' and self.cursor.pos + 1 < self.cursor.buf.len) {
                self.cursor.pos += 2; // Skip escape sequence
            } else {
                self.cursor.pos += 1;
            }
        }

        if (self.cursor.pos < self.cursor.buf.len) {
            self.cursor.pos += 1; // Skip closing quote
        }

        // Store in ByteSlices
        const content = self.cursor.buf[start_pos + 1 .. self.cursor.pos - 1];
        const bytes_idx = try self.byte_slices.append(gpa, content);

        return Token{
            .tag = .String,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .bytes_idx = bytes_idx },
        };
    }

    fn tokenizeSingleQuote(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1; // Skip the quote
        // Simple implementation for now
        return Token{
            .tag = .SingleQuote,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeNumber(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        const start_pos = self.cursor.pos;

        // Simple decimal number parsing
        while (self.cursor.pos < self.cursor.buf.len) {
            const byte = self.cursor.buf[self.cursor.pos];
            if (std.ascii.isDigit(byte) or byte == '_') {
                self.cursor.pos += 1;
            } else if (byte == '.' and self.cursor.pos + 1 < self.cursor.buf.len and std.ascii.isDigit(self.cursor.buf[self.cursor.pos + 1])) {
                // Decimal point
                self.cursor.pos += 1;
                while (self.cursor.pos < self.cursor.buf.len and (std.ascii.isDigit(self.cursor.buf[self.cursor.pos]) or self.cursor.buf[self.cursor.pos] == '_')) {
                    self.cursor.pos += 1;
                }
                // Return as float
                const text = self.cursor.buf[start_pos..self.cursor.pos];
                if (std.fmt.parseFloat(f64, text)) |value| {
                    // Try to fit in SmallDec
                    const small_dec = Token.SmallDec{ .numerator = @intFromFloat(value * 100), .denominator_power_of_ten = 2 };
                    return Token{
                        .tag = .Float,
                        .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                        .extra = .{ .frac_literal_small = small_dec },
                    };
                } else |_| {
                    // Store as bytes
                    const bytes_idx = try self.byte_slices.append(gpa, text);
                    return Token{
                        .tag = .Float,
                        .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                        .extra = .{ .bytes_idx = bytes_idx },
                    };
                }
            } else {
                break;
            }
        }

        // Parse as integer
        const text = self.cursor.buf[start_pos..self.cursor.pos];
        if (std.fmt.parseInt(i32, text, 10)) |value| {
            return Token{
                .tag = .Int,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .num_literal_i32 = value },
            };
        } else |_| {
            // Store as bytes for big numbers
            const bytes_idx = try self.byte_slices.append(gpa, text);
            return Token{
                .tag = .Int,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .bytes_idx = bytes_idx },
            };
        }
    }

    fn tokenizeIdent(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        const start_pos = self.cursor.pos;

        // Read identifier characters
        while (self.cursor.pos < self.cursor.buf.len) {
            const byte = self.cursor.buf[self.cursor.pos];
            if (std.ascii.isAlphanumeric(byte) or byte == '_' or byte == '!') {
                self.cursor.pos += 1;
            } else {
                break;
            }
        }

        const text = self.cursor.buf[start_pos..self.cursor.pos];

        // Check for keywords first
        const tag = if (tokenize.Token.keywords.get(text)) |keyword_tag|
            keyword_tag
        else if (std.ascii.isUpper(text[0]))
            Token.Tag.UpperIdent
        else
            Token.Tag.LowerIdent;

        // Intern the identifier
        const ident = base.Ident.for_text(text);
        const ident_idx = try self.env.idents.insert(gpa, ident);

        return Token{
            .tag = tag,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .interned = ident_idx },
        };
    }

    fn tokenizeDot(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len) {
            const next_byte = self.cursor.buf[self.cursor.pos];
            if (next_byte == '.') {
                self.cursor.pos += 1;
                return Token{
                    .tag = .DoubleDot,
                    .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                    .extra = .{ .none = 0 },
                };
            }
        }

        return Token{
            .tag = .Dot,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeMinus(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '>') {
            self.cursor.pos += 1;
            return Token{
                .tag = .OpArrow,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .none = 0 },
            };
        }

        return Token{
            .tag = .OpBinaryMinus,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeSlash(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '/') {
            self.cursor.pos += 1;
            return Token{
                .tag = .OpDoubleSlash,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .none = 0 },
            };
        }

        return Token{
            .tag = .OpSlash,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeEquals(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len) {
            const next_byte = self.cursor.buf[self.cursor.pos];
            if (next_byte == '=') {
                self.cursor.pos += 1;
                return Token{
                    .tag = .OpEquals,
                    .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                    .extra = .{ .none = 0 },
                };
            } else if (next_byte == '>') {
                self.cursor.pos += 1;
                return Token{
                    .tag = .OpFatArrow,
                    .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                    .extra = .{ .none = 0 },
                };
            }
        }

        return Token{
            .tag = .OpAssign,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeBang(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '=') {
            self.cursor.pos += 1;
            return Token{
                .tag = .OpNotEquals,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .none = 0 },
            };
        }

        return Token{
            .tag = .OpBang,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeLessThan(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len) {
            const next_byte = self.cursor.buf[self.cursor.pos];
            if (next_byte == '=') {
                self.cursor.pos += 1;
                return Token{
                    .tag = .OpLessThanOrEq,
                    .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                    .extra = .{ .none = 0 },
                };
            } else if (next_byte == '-') {
                self.cursor.pos += 1;
                return Token{
                    .tag = .OpBackArrow,
                    .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                    .extra = .{ .none = 0 },
                };
            }
        }

        return Token{
            .tag = .OpLessThan,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeGreaterThan(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '=') {
            self.cursor.pos += 1;
            return Token{
                .tag = .OpGreaterThanOrEq,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .none = 0 },
            };
        }

        return Token{
            .tag = .OpGreaterThan,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeAmpersand(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '&') {
            self.cursor.pos += 1;
            return Token{
                .tag = .OpAnd,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .none = 0 },
            };
        }

        return Token{
            .tag = .OpAmpersand,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeBar(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '|') {
            self.cursor.pos += 1;
            return Token{
                .tag = .OpOr,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .none = 0 },
            };
        }

        return Token{
            .tag = .OpBar,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeQuestion(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '?') {
            self.cursor.pos += 1;
            return Token{
                .tag = .OpDoubleQuestion,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .none = 0 },
            };
        }

        return Token{
            .tag = .OpQuestion,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }

    fn tokenizeColon(self: *TokenIterator, gpa: std.mem.Allocator) std.mem.Allocator.Error!Token {
        _ = gpa;
        const start_pos = self.cursor.pos;
        self.cursor.pos += 1;

        if (self.cursor.pos < self.cursor.buf.len and self.cursor.buf[self.cursor.pos] == '=') {
            self.cursor.pos += 1;
            return Token{
                .tag = .OpColonEqual,
                .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
                .extra = .{ .none = 0 },
            };
        }

        return Token{
            .tag = .OpColon,
            .region = base.Region.from_raw_offsets(@intCast(start_pos), @intCast(self.cursor.pos)),
            .extra = .{ .none = 0 },
        };
    }
};
