//! The cursor is our current position in the input text, and it collects messages.
//! Note that instead of allocating its own message list, the caller must pass in a pre-allocated
//! slice of Message. The field `message_count` tracks how many messages have been written.
//! This can grow beyond the length of the slice, and if so, it means there are more messages
//! than the caller has allocated space for. The caller can either ignore these messages or
//! allocate a larger slice and tokenize again.

const std = @import("std");
const Diagnostic = @import("Diagnostic.zig");
const Cursor = @import("Cursor.zig");
const Token = @import("Token.zig");
const Comment = @import("Token.zig").Comment;

buf: []const u8,
pos: u32,
messages: []Diagnostic,
message_count: u32,
tab_width: u8 = 4, // TODO: make this configurable
comment: ?Comment = null,

/// Initialize a Cursor with the given input buffer and a pre-allocated messages slice.
pub fn init(buf: []const u8, messages: []Diagnostic) Cursor {
    return Cursor{
        .buf = buf,
        .pos = 0,
        .messages = messages,
        .message_count = 0,
    };
}

/// Add a diagnostic message at the current cursor position.
pub fn pushMessageHere(self: *Cursor, tag: Diagnostic.Tag) void {
    self.pushMessage(tag, self.pos, self.pos);
}

/// Add a diagnostic message with specified begin and end positions.
pub fn pushMessage(self: *Cursor, tag: Diagnostic.Tag, begin: u32, end: u32) void {
    if (self.message_count < self.messages.len) {
        self.messages[self.message_count] = .{
            .tag = tag,
            .begin = begin,
            .end = end,
        };
    }
    self.message_count += 1;
}

/// Returns the current byte, or null if at the end.
pub fn peek(self: *Cursor) ?u8 {
    if (self.pos < self.buf.len) {
        return self.buf[self.pos];
    }
    return null;
}

/// Returns the byte at the given lookahead offset.
/// Peek at a character at the specified lookahead distance from current position.
pub fn peekAt(self: *Cursor, lookahead: u32) ?u8 {
    if (self.pos + lookahead < self.buf.len) {
        return self.buf[self.pos + lookahead];
    }
    return null;
}

/// Check if the character at lookahead distance is within the specified ASCII range.
pub fn isPeekedCharInRange(self: *Cursor, lookahead: u32, start: u8, end: u8) bool {
    const peeked = self.peekAt(lookahead);

    return if (peeked) |c|
        c >= start and c <= end
    else
        false;
}

/// Requires that the next byte is `ch`, otherwise pushes a message.
pub fn require(self: *Cursor, ch: u8, tag: Diagnostic.Tag) void {
    if (self.peek() == ch) {
        self.pos += 1;
    } else {
        self.pushMessageHere(tag);
    }
}

/// Pop and return the current comment if one exists, clearing it from the cursor.
pub fn popComment(self: *Cursor) ?Comment {
    if (self.comment) |c| {
        self.comment = null;
        return c;
    }
    return null;
}

/// Chomps “trivia” (whitespace, comments, etc.) and returns an optional indent.
/// If the chomped trivia includes a newline, returns the indent of the next (real) line.
/// Otherwise, returns null.
pub fn chompTrivia(self: *Cursor) ?u16 {
    var sawNewline = false;
    var indent: u16 = 0;

    while (self.pos < self.buf.len) {
        const b = self.buf[self.pos];
        if (b == ' ') {
            self.pos += 1;
            if (sawNewline) indent += 1;
        } else if (b == '\t') {
            self.pos += 1;
            if (sawNewline) {
                // round up to the next tab stop
                indent = (indent + self.tab_width) & ~(self.tab_width - 1);
            }
        } else if (b == '\n') {
            self.pos += 1;
            sawNewline = true;
            indent = 0;
            return indent;
        } else if (b == '\r') {
            self.pos += 1;
            sawNewline = true;
            indent = 0;
            if (self.pos < self.buf.len and self.buf[self.pos] == '\n') {
                self.pos += 1;
                return indent;
            } else {
                self.pushMessageHere(.MisplacedCarriageReturn);
            }
        } else if (b == '#') {
            self.pos += 1;
            const comment_start = self.pos;
            while (self.pos < self.buf.len and self.buf[self.pos] != '\n' and self.buf[self.pos] != '\r') {
                self.pos += 1;
            }
            self.comment = Comment{ .begin = comment_start, .end = self.pos };
        } else if (b >= 0 and b <= 31) {
            self.pushMessageHere(.AsciiControl);
            self.pos += 1;
        } else {
            break;
        }
    }
    return null;
}

fn maybeMessageForUppercaseBase(self: *Cursor, b: u8) void {
    if (b == 'X' or b == 'O' or b == 'B') {
        self.pushMessageHere(.UppercaseBase);
    }
}

/// Parse a numeric literal starting with the given initial digit.
pub fn chompNumber(self: *Cursor, initialDigit: u8) Token.Tag {
    // Consume the initial digit.
    std.debug.assert(initialDigit == self.buf[self.pos]);
    self.pos += 1;

    var tok = Token.Tag.Int;
    if (initialDigit == '0') {
        while (true) {
            const c = self.peek() orelse 0;
            switch (c) {
                'x', 'X' => {
                    maybeMessageForUppercaseBase(self, c);
                    self.pos += 1;
                    self.chompIntegerBase16() catch {
                        tok = .MalformedNumberNoDigits;
                    };
                    tok = self.chompNumberSuffix(tok);
                    break;
                },
                'o', 'O' => {
                    maybeMessageForUppercaseBase(self, c);
                    self.pos += 1;
                    self.chompIntegerBase8() catch {
                        tok = .MalformedNumberNoDigits;
                    };
                    tok = self.chompNumberSuffix(tok);
                    break;
                },
                'b', 'B' => {
                    maybeMessageForUppercaseBase(self, c);
                    self.pos += 1;
                    self.chompIntegerBase2() catch {
                        tok = .MalformedNumberNoDigits;
                    };
                    tok = self.chompNumberSuffix(tok);
                    break;
                },
                '0'...'9' => {
                    self.pushMessageHere(.LeadingZero);
                    tok = self.chompNumberBase10();
                    tok = self.chompNumberSuffix(tok);
                    break;
                },
                '_' => {
                    self.pos += 1;
                    continue;
                },
                '.' => {
                    self.pos += 1;
                    self.chompIntegerBase10() catch {}; // This is not an issue, have leading `0.`.
                    tok = .Float;
                    _ = self.chompExponent() catch {
                        tok = .MalformedNumberNoExponentDigits;
                    };
                    tok = self.chompNumberSuffix(tok);
                    break;
                },
                else => {
                    tok = self.chompNumberSuffix(tok);
                    break;
                },
            }
        }
    } else {
        tok = self.chompNumberBase10();
        tok = self.chompNumberSuffix(tok);
    }
    return tok;
}

/// Chomps an exponent including sign and digits, if one if found.
/// Returns true if an exponent was chomped.
/// Will error if the exponent has no digits.
pub fn chompExponent(self: *Cursor) !bool {
    if (self.peek() orelse 0 == 'e' or self.peek() orelse 0 == 'E') {
        self.pos += 1;
        // Optional sign
        if (self.peek() orelse 0 == '+' or self.peek() orelse 0 == '-') {
            self.pos += 1;
        }
        self.chompIntegerBase10() catch {
            return error.EmptyExponent;
        };
        return true;
    }
    return false;
}

/// Chomp what's expected to be the suffix of a number.
/// Returns either the original token hypothesis, or a malformed token tag.
pub fn chompNumberSuffix(self: *Cursor, hypothesis: Token.Tag) Token.Tag {
    if (self.peek()) |c| {
        const is_ident_char = (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_' or c >= 0x80;
        if (!is_ident_char) {
            return hypothesis;
        }
    } else {
        return hypothesis;
    }
    const start = self.pos;
    if (!self.chompIdentGeneral()) {
        return hypothesis.updateIfNotMalformed(.MalformedNumberUnicodeSuffix);
    }
    const suffix = self.buf[start..self.pos];
    if (Token.valid_number_suffixes.get(suffix) == null) {
        return hypothesis.updateIfNotMalformed(.MalformedNumberBadSuffix);
    } else {
        return hypothesis;
    }
}

/// Chomp a number in base 10. The number can be an int or float.
/// Returns the tag of the number type.
/// Will return a malformed node if the exponent is malformed.
/// Before calling this method, a valid leading digit must have been parsed.
pub fn chompNumberBase10(self: *Cursor) Token.Tag {
    var token_type: Token.Tag = .Int;
    self.chompIntegerBase10() catch {}; // This is not an issue, have leading digit.
    if (self.peek() orelse 0 == '.' and (self.isPeekedCharInRange(1, '0', '9') or self.peekAt(1) == 'e' or self.peekAt(1) == 'E')) {
        self.pos += 1;
        self.chompIntegerBase10() catch {}; // This is not an issue, guaranteed to have digits before the decimal point.
        token_type = .Float;
    }
    const has_exponent = self.chompExponent() catch {
        return .MalformedNumberNoExponentDigits;
    };
    if (has_exponent) {
        token_type = .Float;
    }
    return token_type;
}

/// Chomp the digits of an integer in base 10.
/// Will error if the integer has no digits.
pub fn chompIntegerBase10(self: *Cursor) !void {
    var contains_digits = false;
    while (self.peek()) |c| {
        if (c >= '0' and c <= '9') {
            contains_digits = true;
            self.pos += 1;
        } else if (c == '_') {
            self.pos += 1;
        } else {
            break;
        }
    }
    if (!contains_digits) {
        return error.EmptyInteger;
    }
}

/// Chomp the digits of an integer in base 16.
/// Will error if the integer has no digits.
pub fn chompIntegerBase16(self: *Cursor) !void {
    var contains_digits = false;
    while (self.peek()) |c| {
        if ((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            contains_digits = true;
            self.pos += 1;
        } else if (c == '_') {
            self.pos += 1;
        } else {
            break;
        }
    }
    if (!contains_digits) {
        return error.EmptyInteger;
    }
}

/// Chomp the digits of an integer in base 8.
/// Will error if the integer has no digits.
pub fn chompIntegerBase8(self: *Cursor) !void {
    var contains_digits = false;
    while (self.peek()) |c| {
        if (c >= '0' and c <= '7') {
            contains_digits = true;
            self.pos += 1;
        } else if (c == '_') {
            self.pos += 1;
        } else {
            break;
        }
    }
    if (!contains_digits) {
        return error.EmptyInteger;
    }
}

/// Chomp the digits of an integer in base 2.
/// Will error if the integer has no digits.
pub fn chompIntegerBase2(self: *Cursor) !void {
    var contains_digits = false;
    while (self.peek()) |c| {
        if (c == '0' or c == '1') {
            contains_digits = true;
            self.pos += 1;
        } else if (c == '_') {
            self.pos += 1;
        } else {
            break;
        }
    }
    if (!contains_digits) {
        return error.EmptyInteger;
    }
}

/// Chomps an identifier starting with a lowercase letter.
/// Also checks if the resulting identifier is a keyword.
/// Returns the token type - LowerIdent or Kw*
pub fn chompIdentLower(self: *Cursor) Token.Tag {
    const start = self.pos;
    if (!self.chompIdentGeneral()) {
        return .MalformedUnicodeIdent;
    }
    const ident = self.buf[start..self.pos];
    const kw = Token.keywords.get(ident);
    return kw orelse .LowerIdent;
}

/// Chomps a general identifier - either upper or lower case.
/// Doesn't check if the identifier is a keyword, since we assume the caller already
/// determined that was impossible (e.g. because the first character was uppercase),
/// or otherwise not relevant.
///
/// Returns whether the chomped identifier was valid - i.e. didn't contain any non-ascii characters.
pub fn chompIdentGeneral(self: *Cursor) bool {
    var valid = true;
    while (self.pos < self.buf.len) {
        const c = self.buf[self.pos];
        if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_' or c == '!') {
            self.pos += 1;
        } else if (c >= 0x80) {
            valid = false;
            self.pos += 1;
        } else {
            break;
        }
    }
    return valid;
}

/// Consume consecutive decimal digits from the current position.
pub fn chompInteger(self: *Cursor) void {
    while (self.pos < self.buf.len) {
        const c = self.buf[self.pos];
        if (c >= '0' and c <= '9') {
            self.pos += 1;
        } else {
            break;
        }
    }
}

/// Process an escape sequence starting with the given character.
pub fn chompEscapeSequence(self: *Cursor, ch: u8) void {
    std.debug.assert(self.peek() == ch);
    switch (ch) {
        '\\', '"', '\'', 'n', 'r', 't', '$' => {
            self.pos += 1;
        },
        'u' => {
            self.pos += 1;
            self.require('(', .InvalidUnicodeEscapeSequence);
            while (true) {
                if (self.peek() == ')') {
                    self.pos += 1;
                    break;
                } else if (self.peek() != null) {
                    const next = self.peek() orelse 0;
                    if ((next >= '0' and next <= '9') or
                        (next >= 'a' and next <= 'f') or
                        (next >= 'A' and next <= 'F'))
                    {
                        self.pos += 1;
                    } else {
                        self.pushMessageHere(.InvalidUnicodeEscapeSequence);
                        break;
                    }
                } else {
                    break;
                }
            }
        },
        else => {
            self.pushMessageHere(.InvalidEscapeSequence);
            self.pos += 1;
        },
    }
}

/// Parse a single-quoted character literal.
pub fn chompSingleQuoteLiteral(self: *Cursor) void {
    std.debug.assert(self.peek() == '\'');
    const start = self.pos;
    // Skip the initial quote.
    self.pos += 1;
    var escape: bool = false;
    while (self.pos < self.buf.len) {
        const c = self.buf[self.pos];
        if (escape) {
            escape = false;
            self.chompEscapeSequence(c);
        } else {
            if (c == '\\') {
                escape = true;
                self.pos += 1;
            } else if (c == '\n') {
                self.pushMessage(.UnclosedSingleQuote, @intCast(start), @intCast(self.pos));
                return;
            } else if (c == '\'') {
                self.pos += 1;
            } else {
                self.pos += 1;
            }
        }
    }
    self.pushMessage(.UnclosedSingleQuote, start, self.pos);
}
