//! The tokenizer that uses a Cursor and produces a TokenizedBuffer.

const std = @import("std");
const base = @import("../../../base.zig");
const collections = @import("../../../collections.zig");
const tracy = @import("../../../tracy.zig");

const Cursor = @import("Cursor.zig");
const Token = @import("Token.zig");
const TokenizedBuffer = @import("Buffer.zig");
const Diagnostic = @import("Diagnostic.zig");

const BraceKind = Token.BraceKind;
const StringKind = Token.StringKind;
const exitOnOom = collections.utils.exitOnOom;

const Tokenizer = @This();

env: *base.ModuleEnv,
cursor: Cursor,
output: TokenizedBuffer,
stack: std.ArrayListUnmanaged(BraceKind),

/// Creates a new Tokenizer.
/// Note that the caller must also provide a pre-allocated messages buffer.
pub fn init(env: *base.ModuleEnv, text: []const u8, messages: []Diagnostic) Tokenizer {
    const cursor = Cursor.init(text, messages);
    // TODO: tune this more. Syntax grab bag is 3:1.
    // Generally, roc code will be less dense than that.
    const output = TokenizedBuffer.initCapacity(env, text.len);
    return Tokenizer{
        .cursor = cursor,
        .output = output,
        .stack = .{},
        .env = env,
    };
}

pub fn deinit(self: *Tokenizer) void {
    self.output.deinit();
    self.stack.deinit(self.env.gpa);
}

/// The output of the tokenizer.
pub const TokenOutput = struct {
    tokens: TokenizedBuffer,
    messages: []Diagnostic,
    extra_messages_dropped: usize,
};

/// Complete tokenization and return the final output with tokens and diagnostics.
pub fn finishAndDeinit(self: *Tokenizer) TokenOutput {
    self.stack.deinit(self.env.gpa);
    const actual_message_count = @min(self.cursor.message_count, self.cursor.messages.len);
    return .{
        .tokens = self.output,
        .messages = self.cursor.messages[0..actual_message_count],
        .extra_messages_dropped = self.cursor.message_count - actual_message_count,
    };
}

fn pushTokenInternedHere(
    self: *Tokenizer,
    tag: Token.Tag,
    tok_offset: u32,
    text_offset: u32,
) void {
    const text = self.cursor.buf[text_offset..self.cursor.pos];
    const id = self.env.idents.insert(
        self.env.gpa,
        base.Ident.for_text(text),
        base.Region{ .start = base.Region.Position{ .offset = tok_offset }, .end = base.Region.Position{ .offset = self.cursor.pos } },
    );
    self.output.tokens.append(self.env.gpa, .{
        .tag = tag,
        .offset = tok_offset,
        .extra = .{ .interned = id },
    }) catch |err| exitOnOom(err);
}

fn consumeBraceCloseAndContinueStringInterp(self: *Tokenizer, brace: BraceKind) void {
    std.debug.assert(self.cursor.peek() == close_curly or self.cursor.peek() == ']' or self.cursor.peek() == ')');
    const last = self.stack.pop();
    if (last == null) {
        self.cursor.pushMessageHere(.OverClosedBrace);
        self.cursor.pos += 1;
        return;
    }
    switch (last.?) {
        .round => {
            if (brace != .round) {
                self.cursor.pushMessageHere(.MismatchedBrace);
            }
            self.output.pushTokenNormal(.CloseRound, self.cursor.pos, 1);
            self.cursor.pos += 1;
        },
        .square => {
            if (brace != .square) {
                self.cursor.pushMessageHere(.MismatchedBrace);
            }
            self.output.pushTokenNormal(.CloseSquare, self.cursor.pos, 1);
            self.cursor.pos += 1;
        },
        .curly => {
            if (brace != .curly) {
                self.cursor.pushMessageHere(.MismatchedBrace);
            }
            self.output.pushTokenNormal(.CloseCurly, self.cursor.pos, 1);
            self.cursor.pos += 1;
        },
        .string_interpolation => |kind| {
            if (brace != .curly) {
                self.cursor.pushMessageHere(.MismatchedBrace);
            }
            self.output.pushTokenNormal(.CloseStringInterpolation, self.cursor.pos, 1);
            self.cursor.pos += 1;
            self.tokenizeStringLikeLiteralBody(kind);
        },
    }
}

/// The main tokenize loop. This loops over the whole input buffer, tokenizing as it goes.
pub fn tokenize(self: *Tokenizer) void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var sawWhitespace: bool = true;
    while (self.cursor.pos < self.cursor.buf.len) {
        const start = self.cursor.pos;
        const sp = sawWhitespace;
        sawWhitespace = false;
        const b = self.cursor.buf[self.cursor.pos];
        switch (b) {
            // Whitespace & control characters
            0...32, '#' => {
                if (self.cursor.chompTrivia()) |_| {
                    self.output.pushNewline(self.cursor.popComment());
                }
                sawWhitespace = true;
            },

            // Dot (.)
            '.' => {
                const next = self.cursor.peekAt(1);
                if (next) |n| {
                    if (n == '.') {
                        if (self.cursor.peekAt(2) == '.') {
                            self.cursor.pos += 3;
                            self.output.pushTokenNormal(.TripleDot, start, 3);
                        } else {
                            self.cursor.pos += 2;
                            self.output.pushTokenNormal(.DoubleDot, start, 2);
                        }
                    } else if (n >= '0' and n <= '9') {
                        self.cursor.pos += 1;
                        self.cursor.chompInteger();
                        const len = self.cursor.pos - start;
                        self.output.pushTokenNormal(if (sp) .DotInt else .NoSpaceDotInt, start, len);
                    } else if (n >= 'a' and n <= 'z') {
                        var tag: Token.Tag = if (sp) .DotLowerIdent else .NoSpaceDotLowerIdent;
                        self.cursor.pos += 1;
                        const text_start = self.cursor.pos;
                        if (!self.cursor.chompIdentGeneral()) {
                            tag = .MalformedDotUnicodeIdent;
                        }
                        self.pushTokenInternedHere(tag, start, text_start);
                    } else if (n >= 'A' and n <= 'Z') {
                        var tag: Token.Tag = if (sp) .DotUpperIdent else .NoSpaceDotUpperIdent;
                        self.cursor.pos += 1;
                        const text_start = self.cursor.pos;
                        if (!self.cursor.chompIdentGeneral()) {
                            tag = .MalformedDotUnicodeIdent;
                        }
                        self.pushTokenInternedHere(tag, start, text_start);
                    } else if (n >= 0x80 and n <= 0xff) {
                        self.cursor.pos += 1;
                        const text_start = self.cursor.pos;
                        _ = self.cursor.chompIdentGeneral();
                        self.pushTokenInternedHere(.MalformedDotUnicodeIdent, start, text_start);
                    } else if (n == open_curly) {
                        self.cursor.pos += 1;
                        self.output.pushTokenNormal(.Dot, start, 1);
                    } else if (n == '*') {
                        self.cursor.pos += 2;
                        self.output.pushTokenNormal(.DotStar, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        self.output.pushTokenNormal(.Dot, start, 1);
                    }
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.Dot, start, 1);
                }
            },

            // Minus (-)
            '-' => {
                const next = self.cursor.peekAt(1);
                if (next) |n| {
                    if (n == '>') {
                        self.cursor.pos += 2;
                        self.output.pushTokenNormal(.OpArrow, start, 2);
                    } else if (n == ' ' or n == '\t' or n == '\n' or n == '\r' or n == '#') {
                        self.cursor.pos += 1;
                        self.output.pushTokenNormal(.OpBinaryMinus, start, 1);
                    } else if (n >= '0' and n <= '9' and sp) {
                        self.cursor.pos += 1;
                        const tag = self.cursor.chompNumber(n);
                        const len = self.cursor.pos - start;
                        self.output.pushTokenNormal(tag, start, len);
                    } else {
                        self.cursor.pos += 1;
                        const tokenType: Token.Tag = if (sp) .OpUnaryMinus else .OpBinaryMinus;
                        self.output.pushTokenNormal(tokenType, start, 1);
                    }
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(if (sp) .OpUnaryMinus else .OpBinaryMinus, start, 1);
                }
            },

            // Exclamation (!)
            '!' => {
                if (self.cursor.peekAt(1) == '=') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpNotEquals, start, 2);
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.OpBang, start, 1);
                }
            },

            // Ampersand (&)
            '&' => {
                self.cursor.pos += 1;
                self.output.pushTokenNormal(.OpAmpersand, start, 1);
            },

            // Comma (,)
            ',' => {
                self.cursor.pos += 1;
                self.output.pushTokenNormal(.Comma, start, 1);
            },

            // Question mark (?)
            '?' => {
                if (self.cursor.peekAt(1) == '?') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpDoubleQuestion, start, 2);
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(if (sp) .OpQuestion else .NoSpaceOpQuestion, start, 1);
                }
            },

            // Pipe (|)
            '|' => {
                if (self.cursor.peekAt(1) == '>') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpPizza, start, 2);
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.OpBar, start, 1);
                }
            },

            // Plus (+)
            '+' => {
                self.cursor.pos += 1;
                self.output.pushTokenNormal(.OpPlus, start, 1);
            },

            // Star (*)
            '*' => {
                self.cursor.pos += 1;
                self.output.pushTokenNormal(.OpStar, start, 1);
            },

            // Slash (/)
            '/' => {
                if (self.cursor.peekAt(1) == '/') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpDoubleSlash, start, 2);
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.OpSlash, start, 1);
                }
            },

            // Backslash (\)
            '\\' => {
                self.cursor.pos += 1;
                self.output.pushTokenNormal(.OpBackslash, start, 1);
            },

            // Percent (%)
            '%' => {
                self.cursor.pos += 1;
                self.output.pushTokenNormal(.OpPercent, start, 1);
            },

            // Caret (^)
            '^' => {
                self.cursor.pos += 1;
                self.output.pushTokenNormal(.OpCaret, start, 1);
            },

            // Greater-than (>)
            '>' => {
                if (self.cursor.peekAt(1) == '=') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpGreaterThanOrEq, start, 2);
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.OpGreaterThan, start, 1);
                }
            },

            // Less-than (<)
            '<' => {
                if (self.cursor.peekAt(1) == '=') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpLessThanOrEq, start, 2);
                } else if (self.cursor.peekAt(1) == '-') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpBackArrow, start, 2);
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.OpLessThan, start, 1);
                }
            },

            // Equals (=)
            '=' => {
                if (self.cursor.peekAt(1) == '=') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpEquals, start, 2);
                } else if (self.cursor.peekAt(1) == '>') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpFatArrow, start, 2);
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.OpAssign, start, 1);
                }
            },

            // Colon (:)
            ':' => {
                if (self.cursor.peekAt(1) == '=') {
                    self.cursor.pos += 2;
                    self.output.pushTokenNormal(.OpColonEqual, start, 2);
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.OpColon, start, 1);
                }
            },

            '(' => {
                self.cursor.pos += 1;
                self.stack.append(self.env.gpa, .round) catch |err| exitOnOom(err);
                self.output.pushTokenNormal(if (sp) .OpenRound else .NoSpaceOpenRound, start, 1);
            },
            '[' => {
                self.cursor.pos += 1;
                self.stack.append(self.env.gpa, .square) catch |err| exitOnOom(err);
                self.output.pushTokenNormal(.OpenSquare, start, 1);
            },
            open_curly => {
                self.cursor.pos += 1;
                self.stack.append(self.env.gpa, .curly) catch |err| exitOnOom(err);
                self.output.pushTokenNormal(.OpenCurly, start, 1);
            },

            ')' => {
                self.consumeBraceCloseAndContinueStringInterp(.round);
            },
            ']' => {
                self.consumeBraceCloseAndContinueStringInterp(.square);
            },
            close_curly => {
                self.consumeBraceCloseAndContinueStringInterp(.curly);
            },

            '_' => {
                const next = self.cursor.peekAt(1);
                if (next) |n| {
                    if ((n >= 'a' and n <= 'z') or (n >= 'A' and n <= 'Z') or (n >= '0' and n <= '9')) {
                        var tok: Token.Tag = .NamedUnderscore;
                        self.cursor.pos += 2;
                        if (!self.cursor.chompIdentGeneral()) {
                            tok = .MalformedNamedUnderscoreUnicode;
                        }
                        const len = self.cursor.pos - start;
                        self.output.pushTokenNormal(tok, start, len);
                    } else {
                        self.cursor.pos += 1;
                        self.output.pushTokenNormal(.Underscore, start, 1);
                    }
                } else {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.Underscore, start, 1);
                }
            },

            '@' => {
                var tok: Token.Tag = .OpaqueName;
                const next = self.cursor.peekAt(1);
                if (next) |n| {
                    if ((n >= 'a' and n <= 'z') or (n >= 'A' and n <= 'Z') or (n >= '0' and n <= '9') or n == '_' or n >= 0x80) {
                        self.cursor.pos += 1;
                        if (!self.cursor.chompIdentGeneral()) {
                            tok = .MalformedOpaqueNameUnicode;
                        }
                    } else {
                        tok = .MalformedOpaqueNameWithoutName;
                        self.cursor.pos += 1;
                    }
                } else {
                    tok = .MalformedOpaqueNameWithoutName;
                    self.cursor.pos += 1;
                }
                const len = self.cursor.pos - start;
                self.output.pushTokenNormal(tok, start, len);
            },

            // Numbers starting with 0-9
            '0'...'9' => {
                const tag = self.cursor.chompNumber(b);
                const len = self.cursor.pos - start;
                self.output.pushTokenNormal(tag, start, len);
            },

            // Lowercase identifiers
            'a'...'z' => {
                const tag = self.cursor.chompIdentLower();
                const len = self.cursor.pos - start;
                if (tag == .LowerIdent or tag == .MalformedUnicodeIdent) {
                    self.pushTokenInternedHere(tag, start, start);
                } else {
                    self.output.pushTokenNormal(tag, start, len);
                }
            },

            // Uppercase identifiers
            'A'...'Z' => {
                var tag: Token.Tag = .UpperIdent;
                if (!self.cursor.chompIdentGeneral()) {
                    tag = .MalformedUnicodeIdent;
                }
                self.pushTokenInternedHere(tag, start, start);
            },

            '\'' => {
                self.cursor.chompSingleQuoteLiteral();
                self.output.pushTokenNormal(.SingleQuote, start, self.cursor.pos - start);
            },

            '"' => {
                // Note this may return StringStart/StringPart instead of String,
                // in the case of a string interpolation.
                self.tokenizeStringLikeLiteral();
            },

            // first byte of a UTF-8 sequence
            0x80...0xff => {
                _ = self.cursor.chompIdentGeneral();
                self.pushTokenInternedHere(.MalformedUnicodeIdent, start, start);
            },

            // TODO: emit a MalformedOpToken for invalid combinations of operator-like characters

            // Fallback for any unknown token.
            else => {
                const len = 1; // TODO: fast-forward to the next thing that looks like a real token
                self.cursor.pos += len;
                self.output.pushTokenNormal(.MalformedUnknownToken, start, len);
            },
        }
    }

    self.output.pushTokenNormal(.EndOfFile, self.cursor.pos, 0);
}

/// Tokenize a string literal starting with a quote character.
pub fn tokenizeStringLikeLiteral(self: *Tokenizer) void {
    const start = self.cursor.pos;
    std.debug.assert(self.cursor.peek() == '"');
    self.cursor.pos += 1;
    var kind: StringKind = .single_line;
    if (self.cursor.peek() == '"' and self.cursor.peekAt(1) == '"') {
        self.cursor.pos += 2;
        kind = .multi_line;
        self.output.pushTokenNormal(.MultilineStringStart, start, 3);
    } else {
        self.output.pushTokenNormal(.StringStart, start, 1);
    }
    self.tokenizeStringLikeLiteralBody(kind);
}

// Moving curly chars to constants because some editors hate them inline.
const open_curly = '{';
const close_curly = '}';

/// Tokenize the body content of a string literal with the specified kind.
pub fn tokenizeStringLikeLiteralBody(self: *Tokenizer, kind: StringKind) void {
    const start = self.cursor.pos;
    var escape: bool = false;
    while (self.cursor.pos < self.cursor.buf.len) {
        const c = self.cursor.buf[self.cursor.pos];
        if (escape) {
            escape = false;
            self.cursor.chompEscapeSequence(c);
        } else {
            if (c == '$' and self.cursor.peekAt(1) == open_curly) {
                self.output.pushTokenNormal(.StringPart, start, self.cursor.pos - start);
                const dollar_start = self.cursor.pos;
                self.cursor.pos += 2;
                self.output.pushTokenNormal(.OpenStringInterpolation, dollar_start, self.cursor.pos - dollar_start);
                self.stack.append(self.env.gpa, .{ .string_interpolation = kind }) catch |err| exitOnOom(err);
                return;
            } else if (c == '\n') {
                self.output.pushTokenNormal(.StringPart, start, self.cursor.pos - start);
                if (kind == .single_line) {
                    self.cursor.pushMessage(.UnclosedString, @intCast(start), @intCast(self.cursor.pos));
                    self.output.pushTokenNormal(.StringEnd, self.cursor.pos, 0);
                }
                return;
            } else if (kind == .single_line and c == '"') {
                self.output.pushTokenNormal(.StringPart, start, self.cursor.pos - start);
                self.output.pushTokenNormal(.StringEnd, self.cursor.pos, 1);
                self.cursor.pos += 1;
                return;
            } else if (kind == .multi_line and c == '"' and self.cursor.peekAt(1) == '"' and self.cursor.peekAt(2) == '"') {
                self.output.pushTokenNormal(.StringPart, start, self.cursor.pos - start);
                const quote_start = self.cursor.pos;
                self.cursor.pos += 3;
                self.output.pushTokenNormal(.MultilineStringEnd, quote_start, self.cursor.pos - quote_start);
                return;
            } else {
                escape = c == '\\';

                if (!std.ascii.isPrint(c)) {
                    self.cursor.pushMessageHere(.NonPrintableUnicodeInStrLiteral);
                }

                self.cursor.pos += 1;
            }
        }
    }
    if (kind == .single_line) {
        self.cursor.pushMessage(.UnclosedString, start, self.cursor.pos);
    }
    self.output.pushTokenNormal(.StringPart, start, self.cursor.pos - start);
}
