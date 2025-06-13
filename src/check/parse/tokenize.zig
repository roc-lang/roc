const std = @import("std");
const Allocator = std.mem.Allocator;
const collections = @import("../../collections.zig");
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;
const base = @import("../../base.zig");
const tracy = @import("../../tracy.zig");

/// representation of a token in the source code, like '+', 'foo', '=', '{'
/// these are represented by an offset into the bytes of the source code
/// and an extra field that stores either the length of the token or
/// an index into the string interner
pub const Token = struct {
    tag: Tag,
    offset: u32,
    extra: Extra,

    pub const Extra = union {
        length: u32,
        interned: base.Ident.Idx,
    };

    pub const List = std.MultiArrayList(@This());

    pub const Idx = u32;
    pub const Span = struct { span: base.DataSpan };

    pub const Tag = enum(u8) {
        EndOfFile,

        Newline,

        // primitives
        Float,
        StringStart, // the " that starts a string
        StringEnd, // the " that ends a string
        MultilineStringStart, // the """ that starts a multiline string
        MultilineStringEnd, // the """ that ends a multiline string
        StringPart,
        SingleQuote,
        Int,
        MalformedNumberBadSuffix, // malformed, but should be treated similar to an int in the parser
        MalformedNumberUnicodeSuffix, // malformed, but should be treated similar to an int in the parser
        MalformedNumberNoDigits, // malformed, but should be treated similar to an int in the parser
        MalformedNumberNoExponentDigits, // malformed, but should be treated similar to an int in the parser

        // Should be treated as StringPart in the parser, but we forward the error to the ast
        MalformedInvalidUnicodeEscapeSequence,
        MalformedInvalidEscapeSequence,

        UpperIdent,
        LowerIdent,
        MalformedUnicodeIdent,
        Underscore,
        DotLowerIdent,
        DotInt,
        DotUpperIdent,
        NoSpaceDotInt,
        NoSpaceDotLowerIdent,
        NoSpaceDotUpperIdent,
        MalformedDotUnicodeIdent,
        MalformedNoSpaceDotUnicodeIdent,

        NamedUnderscore,
        MalformedNamedUnderscoreUnicode,

        OpaqueName,
        MalformedOpaqueNameUnicode,
        MalformedOpaqueNameWithoutName,

        OpenRound,
        CloseRound,
        OpenSquare,
        CloseSquare,
        OpenCurly,
        CloseCurly,
        OpenStringInterpolation,
        CloseStringInterpolation,
        NoSpaceOpenRound,
        // NoSpaceOpenCurly,

        OpPlus,
        OpStar,
        OpPizza,
        OpAssign,
        OpBinaryMinus, // trailing whitespace
        OpUnaryMinus, // no trailing whitespace
        OpNotEquals,
        OpBang,
        OpAnd,
        OpAmpersand,
        OpQuestion,
        OpDoubleQuestion,
        OpOr,
        OpBar,
        OpDoubleSlash,
        OpSlash,
        OpPercent,
        OpCaret,
        OpGreaterThanOrEq,
        OpGreaterThan,
        OpLessThanOrEq,
        OpBackArrow,
        OpLessThan,
        OpEquals,
        OpColonEqual,
        NoSpaceOpQuestion,

        Comma,
        Dot,
        DoubleDot,
        TripleDot,
        DotStar,
        OpColon,
        OpArrow,
        OpFatArrow,
        OpBackslash,

        // Keywords
        KwApp,
        KwAs,
        KwCrash,
        KwDbg,
        KwDebug,
        KwElse,
        KwExpect,
        KwExposes,
        KwExposing,
        KwFor,
        KwGenerates,
        KwHas,
        KwHosted,
        KwIf,
        KwImplements,
        KwImport,
        KwImports,
        KwIn,
        KwInterface,
        KwMatch,
        KwModule,
        KwPackage,
        KwPackages,
        KwPlatform,
        KwProvides,
        KwRequires,
        KwReturn,
        KwVar,
        KwWhere,
        KwWith,

        MalformedUnknownToken,

        /// Returns true if the node is a keyword.
        pub fn isKeyword(tok: Tag) bool {
            return switch (tok) {
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
                .KwVar,
                .KwWhere,
                .KwWith,
                => true,
                else => false,
            };
        }

        /// Returns true if the node is malformed.
        pub fn isMalformed(tok: Tag) bool {
            // This function explicitly lists all variants to ensure new malformed nodes aren't missed.
            return switch (tok) {
                .EndOfFile,
                .Newline,
                .Float,
                .StringStart,
                .StringEnd,
                .MultilineStringStart,
                .MultilineStringEnd,
                .StringPart,
                .SingleQuote,
                .Int,
                .UpperIdent,
                .LowerIdent,
                .Underscore,
                .DotLowerIdent,
                .DotInt,
                .DotUpperIdent,
                .NoSpaceDotInt,
                .NoSpaceDotLowerIdent,
                .NoSpaceDotUpperIdent,
                .NamedUnderscore,
                .OpaqueName,
                .OpenRound,
                .CloseRound,
                .OpenSquare,
                .CloseSquare,
                .OpenCurly,
                .CloseCurly,
                .OpenStringInterpolation,
                .CloseStringInterpolation,
                .NoSpaceOpenRound,
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
                .NoSpaceOpQuestion,
                .Comma,
                .Dot,
                .DoubleDot,
                .TripleDot,
                .DotStar,
                .OpColon,
                .OpArrow,
                .OpFatArrow,
                .OpBackslash,
                .KwApp,
                .KwAs,
                .KwCrash,
                .KwDbg,
                .KwDebug,
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
                .KwVar,
                .KwWhere,
                .KwWith,
                => false,

                .MalformedDotUnicodeIdent,
                .MalformedInvalidEscapeSequence,
                .MalformedInvalidUnicodeEscapeSequence,
                .MalformedNamedUnderscoreUnicode,
                .MalformedNoSpaceDotUnicodeIdent,
                .MalformedNumberBadSuffix,
                .MalformedNumberNoDigits,
                .MalformedNumberNoExponentDigits,
                .MalformedNumberUnicodeSuffix,
                .MalformedOpaqueNameUnicode,
                .MalformedOpaqueNameWithoutName,
                .MalformedUnicodeIdent,
                .MalformedUnknownToken,
                => true,
            };
        }

        /// This function is used to keep around the first malformed node.
        /// For example, if an integer has no digits and a bad suffix `0bu22`,
        /// we keep the first malformed node that the integer has no digits instead of pointing out the bad suffix.
        fn updateIfNotMalformed(tok: Tag, next: Tag) Tag {
            if (tok.isMalformed()) {
                return tok;
            }
            return next;
        }
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .OpAnd },
        .{ "app", .KwApp },
        .{ "as", .KwAs },
        .{ "crash", .KwCrash },
        .{ "dbg", .KwDbg },
        .{ "else", .KwElse },
        .{ "expect", .KwExpect },
        .{ "exposes", .KwExposes },
        .{ "exposing", .KwExposing },
        .{ "for", .KwFor },
        .{ "generates", .KwGenerates },
        .{ "has", .KwHas },
        .{ "hosted", .KwHosted },
        .{ "if", .KwIf },
        .{ "implements", .KwImplements },
        .{ "import", .KwImport },
        .{ "imports", .KwImports },
        .{ "in", .KwIn },
        .{ "interface", .KwInterface },
        .{ "match", .KwMatch },
        .{ "module", .KwModule },
        .{ "or", .OpOr },
        .{ "package", .KwPackage },
        .{ "packages", .KwPackages },
        .{ "platform", .KwPlatform },
        .{ "provides", .KwProvides },
        .{ "requires", .KwRequires },
        .{ "return", .KwReturn },
        .{ "var", .KwVar },
        .{ "where", .KwWhere },
        .{ "with", .KwWith },
    });

    pub const valid_number_suffixes = std.StaticStringMap(void).initComptime(.{
        .{ "dec", .{} },
        .{ "i128", .{} },
        .{ "i16", .{} },
        .{ "i32", .{} },
        .{ "i64", .{} },
        .{ "i8", .{} },
        .{ "nat", .{} },
        .{ "u128", .{} },
        .{ "u16", .{} },
        .{ "u32", .{} },
        .{ "u64", .{} },
        .{ "u8", .{} },
    });
};

/// The buffer that accumulates tokens.
pub const TokenizedBuffer = struct {
    tokens: Token.List,
    env: *base.ModuleEnv,

    pub fn initCapacity(env: *base.ModuleEnv, capacity: usize) TokenizedBuffer {
        var tokens = Token.List{};
        tokens.ensureTotalCapacity(env.gpa, capacity) catch |err| exitOnOom(err);
        return TokenizedBuffer{
            .tokens = tokens,
            .env = env,
        };
    }

    pub fn deinit(self: *TokenizedBuffer) void {
        self.tokens.deinit(self.env.gpa);
    }

    pub fn resolve(self: *TokenizedBuffer, token: Token.Idx) base.Region {
        const tag = self.tokens.items(.tag)[@intCast(token)];
        const start = self.tokens.items(.offset)[@intCast(token)];
        const extra = self.tokens.items(.extra)[@intCast(token)];
        switch (tag) {
            .LowerIdent,
            .DotLowerIdent,
            .NoSpaceDotLowerIdent,
            .MalformedUnicodeIdent,
            .MalformedDotUnicodeIdent,
            .DotUpperIdent,
            .NoSpaceDotUpperIdent,
            .UpperIdent,
            => {
                return self.env.idents.getRegion(extra.interned);
            },
            else => {
                const end = start + extra.length;
                return .{ .start = base.Region.Position{ .offset = start }, .end = base.Region.Position{ .offset = end } };
            },
        }
    }

    /// Loads the current token if it is an identifier.
    /// Otherwise returns null.
    pub fn resolveIdentifier(self: *TokenizedBuffer, token: Token.Idx) ?base.Ident.Idx {
        const tag = self.tokens.items(.tag)[@intCast(token)];
        const extra = self.tokens.items(.extra)[@intCast(token)];
        switch (tag) {
            .LowerIdent,
            .DotLowerIdent,
            .NoSpaceDotLowerIdent,
            .MalformedUnicodeIdent,
            .MalformedDotUnicodeIdent,
            .DotUpperIdent,
            .NoSpaceDotUpperIdent,
            .UpperIdent,
            => {
                return extra.interned;
            },
            else => {
                return null;
            },
        }
    }

    /// Pushes a token with the given tag, token offset, and extra.
    pub fn pushTokenNormal(self: *TokenizedBuffer, tag: Token.Tag, tok_offset: u32, length: u32) void {
        self.tokens.append(self.env.gpa, .{
            .tag = tag,
            .offset = tok_offset,
            .extra = .{ .length = length },
        }) catch |err| exitOnOom(err);
    }

    pub fn pushNewline(self: *TokenizedBuffer, comment: ?Comment) void {
        var token = Token{
            .tag = .Newline,
            .offset = 0, // store the Comment start - if it is exists here
            .extra = .{ .length = 0 },
        };
        if (comment) |c| {
            token.offset = c.begin;
            token.extra = .{ .length = if (c.end > c.begin) c.end - c.begin else 0 };
        }
        self.tokens.append(self.env.gpa, token) catch |err| exitOnOom(err);
    }

    /// Returns the offset of the token at index `idx`.
    pub fn offset(self: *TokenizedBuffer, idx: u32) u32 {
        // newline tokens don't have offsets - that field is used to store the indent.
        std.debug.assert(self.tokens.items(.tag)[idx] != .Newline);
        return self.tokens.items(.offset)[@intCast(idx)];
    }
};

/// Represents a comment in roc source e.g. `## some comment`
pub const Comment = struct {
    begin: u32,
    end: u32,
};

/// Represents a unicode character parse from the source.
const Unicode = struct {
    tag: Tag,
    length: u32,

    const Tag = enum {
        LetterUpper,
        LetterNotUpper,
        Digit,
        Other,
        Invalid,
    };
};

/// Represents a diagnostic message including its position in the source.
pub const Diagnostic = struct {
    tag: Tag,
    begin: u32,
    end: u32,

    /// Represents the type of diagnostic message.
    pub const Tag = enum {
        MisplacedCarriageReturn,
        AsciiControl,
        LeadingZero,
        UppercaseBase,
        InvalidUnicodeEscapeSequence,
        InvalidEscapeSequence,
        UnclosedString,
        UnclosedSingleQuote,
        OverClosedBrace,
        MismatchedBrace,
        NonPrintableUnicodeInStrLiteral,
    };

    pub fn toStr(self: Diagnostic, gpa: Allocator, source: []const u8, writer: anytype) !void {
        var newlines = try base.DiagnosticPosition.findLineStarts(gpa, source);
        defer newlines.deinit();

        // Get position information
        const info = try base.DiagnosticPosition.position(source, newlines, self.begin, self.end);

        // Strip trailing newline for display
        const display_text = if (info.line_text.len > 0 and
            (info.line_text[info.line_text.len - 1] == '\n' or
                info.line_text[info.line_text.len - 1] == '\r'))
            info.line_text[0 .. info.line_text.len - 1]
        else
            info.line_text;

        var spaces = std.ArrayList(u8).init(gpa);
        defer spaces.deinit();
        for (0..info.start_col_idx) |_| {
            try spaces.append(' ');
        }

        var carets = std.ArrayList(u8).init(gpa);
        defer carets.deinit();

        const caret_length = if (self.end > self.begin) self.end - self.begin else 1;
        for (0..caret_length) |_| {
            try carets.append('^');
        }

        const error_message = try std.fmt.allocPrint(
            gpa,
            "TOKENIZE: ({d}:{d}-{d}:{d}) {s}:\n{s}\n{s}{s}",
            .{
                // add one to display numbers instead of index
                info.start_line_idx + 1,
                info.start_col_idx + 1,
                info.end_line_idx + 1,
                info.end_col_idx + 1,
                @tagName(self.tag),
                display_text,
                spaces.items,
                carets.items,
            },
        );
        defer gpa.free(error_message);

        try writer.writeAll(error_message);
    }
};

/// The cursor is our current position in the input text, and it collects messages.
/// Note that instead of allocating its own message list, the caller must pass in a pre-allocated
/// slice of Message. The field `message_count` tracks how many messages have been written.
/// This can grow beyond the length of the slice, and if so, it means there are more messages
/// than the caller has allocated space for. The caller can either ignore these messages or
/// allocate a larger slice and tokenize again.
pub const Cursor = struct {
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

    fn pushMessageHere(self: *Cursor, tag: Diagnostic.Tag) void {
        self.pushMessage(tag, self.pos, self.pos);
    }

    fn pushMessage(self: *Cursor, tag: Diagnostic.Tag, begin: u32, end: u32) void {
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
    pub fn peekAt(self: *Cursor, lookahead: u32) ?u8 {
        if (self.pos + lookahead < self.buf.len) {
            return self.buf[self.pos + lookahead];
        }
        return null;
    }

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
};

/// The output of the tokenizer.
pub const TokenOutput = struct {
    tokens: TokenizedBuffer,
    messages: []Diagnostic,
    extra_messages_dropped: usize,
};

const BraceKind = union(enum) {
    round,
    square,
    curly,
    string_interpolation: StringKind,

    const List = collections.SafeList(@This());
};

const StringKind = enum {
    single_line,
    multi_line,
};

/// The tokenizer that uses a Cursor and produces a TokenizedBuffer.
pub const Tokenizer = struct {
    cursor: Cursor,
    output: TokenizedBuffer,
    stack: std.ArrayListUnmanaged(BraceKind),
    env: *base.ModuleEnv,

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
};

fn testTokenization(gpa: std.mem.Allocator, input: []const u8, expected: []const Token.Tag) !void {
    var messages: [10]Diagnostic = undefined;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var tokenizer = Tokenizer.init(&env, input, &messages);
    defer tokenizer.deinit();

    tokenizer.tokenize();
    const tokenizedBuffer = tokenizer.output;
    const tokens = tokenizedBuffer.tokens.items(.tag);

    try std.testing.expectEqual(tokens[tokens.len - 1], Token.Tag.EndOfFile);
    try std.testing.expectEqualSlices(Token.Tag, expected[0..expected.len], tokens[0 .. tokens.len - 1]);

    checkTokenizerInvariants(gpa, input, false);
}

/// Assert the invariants of the tokenizer are held.
pub fn checkTokenizerInvariants(gpa: std.mem.Allocator, input: []const u8, debug: bool) void {
    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    // Initial tokenization.
    var messages: [32]Diagnostic = undefined;
    var tokenizer = Tokenizer.init(&env, input, &messages);
    tokenizer.tokenize();
    var output = tokenizer.finishAndDeinit();
    defer output.tokens.deinit();

    if (debug) {
        std.debug.print("Original:\n==========\n{s}\n==========\n\n", .{input});
    }

    if (debug) {
        std.debug.print("Before:\n", .{});
        for (0..output.tokens.tokens.len) |token_index| {
            const token = output.tokens.tokens.get(token_index);
            std.debug.print("\t{any}\n", .{token});
        }
        std.debug.print("\n\n", .{});
    }

    // TODO: apply errors from messages to buffer below.
    // For now, just skip on tokenizer finding a failure.
    if (output.messages.len != 0) {
        return;
    }

    var buf2 = rebuildBufferForTesting(input, &output.tokens, gpa) catch |err| switch (err) {
        error.Unsupported => return,
        error.OutOfMemory => std.debug.panic("OOM", .{}),
    };
    defer buf2.deinit(gpa);

    if (debug) {
        std.debug.print("Intermediate:\n==========\n{s}\n==========\n\n", .{buf2.items});
    }

    // Second tokenization.
    tokenizer = Tokenizer.init(&env, buf2.items, &messages);
    tokenizer.tokenize();
    var output2 = tokenizer.finishAndDeinit();
    defer output2.tokens.deinit();

    if (debug) {
        std.debug.print("After:\n", .{});
        for (0..output2.tokens.tokens.len) |token_index| {
            const token = output2.tokens.tokens.get(token_index);
            std.debug.print("\t{any}\n", .{token});
        }
        std.debug.print("\n\n", .{});
    }
    // Assert same.
    var same = output.tokens.tokens.len == output2.tokens.tokens.len;
    for (0..output.tokens.tokens.len) |token_index| {
        if (token_index >= output2.tokens.tokens.len) {
            same = false;
            break;
        }
        const token = output.tokens.tokens.get(token_index);
        const token2 = output2.tokens.tokens.get(token_index);
        const region1 = output.tokens.resolve(@intCast(token_index));
        const region2 = output2.tokens.resolve(@intCast(token_index));
        const length1 = region1.end.offset - region1.start.offset;
        const length2 = region2.end.offset - region2.start.offset;
        same = same and (token.tag == token2.tag);
        same = same and (token.offset == token2.offset);
        same = same and (length1 == length2);
    }

    if (!same) {
        var prefix_len: usize = 0;
        var suffix_len: usize = 0;
        while (prefix_len < output.tokens.tokens.len and prefix_len < output2.tokens.tokens.len) : (prefix_len += 1) {
            const token = output.tokens.tokens.get(prefix_len);
            const token2 = output2.tokens.tokens.get(prefix_len);
            if (token.tag != token2.tag or token.offset != token2.offset) {
                break;
            }
        }

        while (suffix_len < output.tokens.tokens.len - prefix_len and suffix_len < output2.tokens.tokens.len - prefix_len) : (suffix_len += 1) {
            const token = output.tokens.tokens.get(output.tokens.tokens.len - suffix_len - 1);
            const token2 = output2.tokens.tokens.get(output2.tokens.tokens.len - suffix_len - 1);
            if (token.tag != token2.tag or token.offset != token2.offset) {
                break;
            }
        }

        std.debug.print("...\n", .{});
        for (prefix_len..output.tokens.tokens.len - suffix_len) |token_index| {
            const region = output.tokens.resolve(@intCast(token_index));
            const token = output.tokens.tokens.get(token_index);
            std.debug.print("\x1b[31m\t- {any}\x1b[0m: {s}\n", .{ token, input[token.offset..region.end.offset] });
        }
        for (prefix_len..output2.tokens.tokens.len - suffix_len) |token_index| {
            const region = output2.tokens.resolve(@intCast(token_index));
            const token = output2.tokens.tokens.get(token_index);
            std.debug.print("\x1b[32m\t+ {any}\x1b[0m: {s}\n", .{ token, buf2.items[token.offset..region.end.offset] });
        }
        std.debug.print("...\n", .{});

        std.debug.assert(same);
    }
}

fn rebuildBufferForTesting(buf: []const u8, tokens: *TokenizedBuffer, alloc: std.mem.Allocator) !std.ArrayListUnmanaged(u8) {
    // Create an arraylist to store the new buffer.
    var buf2 = try std.ArrayListUnmanaged(u8).initCapacity(alloc, buf.len);
    errdefer buf2.deinit(alloc);

    // Dump back to buffer.
    // Here we are just printing in the simplest way possible.
    var last_end: usize = 0;
    var prev_token_tag: Token.Tag = .EndOfFile; // placeholder
    for (0..tokens.tokens.len) |token_index| {
        const token = tokens.tokens.get(token_index);
        // EndOfFile and NewLine are special, handle them early.
        // Unlike other tokens they do not store a correct offset and length
        // EndOfFile consumes the entire file. Newline stores the indentation level of the next line.
        if (token.tag == .Newline) {
            // Newlines will be copied with other whitespace
            continue;
        }

        // Copy over limited whitespace.
        // TODO: Long term, we should switch to dummy whitespace, but currently, Roc still has WSS.
        for (last_end..token.offset) |i| {
            // Leave tabs and newlines alone, they are special to roc.
            // Replace everything else with spaces.
            if (buf[i] != '\t' and buf[i] != '\r' and buf[i] != '\n' and buf[i] != '#') {
                try buf2.append(alloc, ' ');
            } else {
                try buf2.append(alloc, buf[i]);
            }
        }
        if (token.tag == .EndOfFile) {
            break;
        }
        const region = tokens.resolve(@intCast(token_index));
        std.debug.assert(region.start.offset == token.offset);
        std.debug.assert(region.end.offset >= region.start.offset);
        std.debug.assert(region.end.offset <= buf.len);
        std.debug.assert(region.start.offset >= last_end);
        last_end = region.end.offset;
        const length = region.end.offset - region.start.offset;
        switch (token.tag) {
            .EndOfFile, .Newline => unreachable,

            .Float => {
                try buf2.append(alloc, '0');
                try buf2.append(alloc, '.');
                for (2..length) |_| {
                    try buf2.append(alloc, '1');
                }
            },
            .SingleQuote => {
                try buf2.append(alloc, '\'');
                for (1..length) |_| {
                    try buf2.append(alloc, '~');
                }
                try buf2.append(alloc, '\'');
            },
            .StringStart, .StringEnd => {
                try buf2.append(alloc, '"');
            },
            .MultilineStringStart, .MultilineStringEnd => {
                try buf2.append(alloc, '"');
                try buf2.append(alloc, '"');
                try buf2.append(alloc, '"');
            },
            .StringPart => {
                for (0..length) |_| {
                    try buf2.append(alloc, '~');
                }
            },
            .OpenStringInterpolation => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '$');
                try buf2.append(alloc, '{');
            },
            .CloseStringInterpolation => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '}');
            },

            .UpperIdent => {
                try buf2.append(alloc, 'Z');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .LowerIdent => {
                for (0..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .Underscore => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '_');
            },
            .DotInt => {
                try buf2.append(alloc, '.');
                for (1..length) |_| {
                    try buf2.append(alloc, '1');
                }
            },
            .DotLowerIdent => {
                try buf2.append(alloc, '.');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .DotUpperIdent => {
                try buf2.append(alloc, '.');
                try buf2.append(alloc, 'Z');
                for (2..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .NoSpaceDotInt => {
                try buf2.append(alloc, '.');
                for (1..length) |_| {
                    try buf2.append(alloc, '1');
                }
            },
            .NoSpaceDotLowerIdent => {
                try buf2.append(alloc, '.');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .NoSpaceDotUpperIdent => {
                try buf2.append(alloc, '.');
                try buf2.append(alloc, 'Z');
                for (2..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .NoSpaceOpenRound => {
                try buf2.append(alloc, '(');
            },

            .NamedUnderscore => {
                try buf2.append(alloc, '_');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .OpaqueName => {
                try buf2.append(alloc, '@');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .Int => {
                // To ensure this value when reprinted tokenizes as an int, add a base if the number is 3 or more characters.
                if (length >= 3) {
                    try buf2.append(alloc, '0');
                    try buf2.append(alloc, 'x');
                    for (2..length) |_| {
                        try buf2.append(alloc, '1');
                    }
                } else {
                    for (0..length) |_| {
                        try buf2.append(alloc, '1');
                    }
                }
            },

            .OpenRound => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '(');
            },
            .CloseRound => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, ')');
            },
            .OpenSquare => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '[');
            },
            .CloseSquare => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, ']');
            },
            .OpenCurly => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '{');
            },
            .CloseCurly => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '}');
            },

            .OpPlus => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '+');
            },
            .OpStar => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '*');
            },
            .OpPizza => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '|');
                try buf2.append(alloc, '>');
            },
            .OpAssign => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '=');
            },
            .OpBinaryMinus => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '-');
            },
            .OpUnaryMinus => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '-');
            },
            .OpNotEquals => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '!');
                try buf2.append(alloc, '=');
            },
            .OpBang => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '!');
            },
            .OpAnd => {
                std.debug.assert(length == 3);
                try buf2.append(alloc, 'a');
                try buf2.append(alloc, 'n');
                try buf2.append(alloc, 'd');
            },
            .OpAmpersand => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '&');
            },
            .OpQuestion, .NoSpaceOpQuestion => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '?');
            },
            .OpDoubleQuestion => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '?');
                try buf2.append(alloc, '?');
            },
            .OpOr => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, 'o');
                try buf2.append(alloc, 'r');
            },
            .OpBar => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '|');
            },
            .OpDoubleSlash => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '/');
                try buf2.append(alloc, '/');
            },
            .OpSlash => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '/');
            },
            .OpPercent => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '%');
            },
            .OpCaret => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '^');
            },
            .OpGreaterThanOrEq => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '>');
                try buf2.append(alloc, '=');
            },
            .OpGreaterThan => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '>');
            },
            .OpLessThanOrEq => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '<');
                try buf2.append(alloc, '=');
            },
            .OpBackArrow => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '<');
                try buf2.append(alloc, '-');
            },
            .OpLessThan => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '<');
            },
            .OpEquals => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '=');
                try buf2.append(alloc, '=');
            },
            .OpColonEqual => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, ':');
                try buf2.append(alloc, '=');
            },

            .Comma => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, ',');
            },
            .Dot => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '.');
            },
            .DoubleDot => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '.');
                try buf2.append(alloc, '.');
            },
            .TripleDot => {
                std.debug.assert(length == 3);
                try buf2.append(alloc, '.');
                try buf2.append(alloc, '.');
                try buf2.append(alloc, '.');
            },
            .DotStar => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '.');
                try buf2.append(alloc, '*');
            },
            .OpColon => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, ':');
            },
            .OpArrow => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '-');
                try buf2.append(alloc, '>');
            },
            .OpFatArrow => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '=');
                try buf2.append(alloc, '>');
            },
            .OpBackslash => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '\\');
            },

            .KwApp => {
                try buf2.appendSlice(alloc, "app");
            },
            .KwAs => {
                try buf2.appendSlice(alloc, "as");
            },
            .KwCrash => {
                try buf2.appendSlice(alloc, "crash");
            },
            .KwDbg => {
                try buf2.appendSlice(alloc, "dbg");
            },
            .KwDebug => {
                try buf2.appendSlice(alloc, "debug");
            },
            .KwElse => {
                try buf2.appendSlice(alloc, "else");
            },
            .KwExpect => {
                try buf2.appendSlice(alloc, "expect");
            },
            .KwExposes => {
                try buf2.appendSlice(alloc, "exposes");
            },
            .KwExposing => {
                try buf2.appendSlice(alloc, "exposing");
            },
            .KwFor => {
                try buf2.appendSlice(alloc, "for");
            },
            .KwGenerates => {
                try buf2.appendSlice(alloc, "generates");
            },
            .KwHas => {
                try buf2.appendSlice(alloc, "has");
            },
            .KwHosted => {
                try buf2.appendSlice(alloc, "hosted");
            },
            .KwIf => {
                try buf2.appendSlice(alloc, "if");
            },
            .KwImplements => {
                try buf2.appendSlice(alloc, "implements");
            },
            .KwImport => {
                try buf2.appendSlice(alloc, "import");
            },
            .KwImports => {
                try buf2.appendSlice(alloc, "imports");
            },
            .KwIn => {
                try buf2.appendSlice(alloc, "in");
            },
            .KwInterface => {
                try buf2.appendSlice(alloc, "interface");
            },
            .KwModule => {
                try buf2.appendSlice(alloc, "module");
            },
            .KwPackage => {
                try buf2.appendSlice(alloc, "package");
            },
            .KwPackages => {
                try buf2.appendSlice(alloc, "packages");
            },
            .KwPlatform => {
                try buf2.appendSlice(alloc, "platform");
            },
            .KwProvides => {
                try buf2.appendSlice(alloc, "provides");
            },
            .KwRequires => {
                try buf2.appendSlice(alloc, "requires");
            },
            .KwReturn => {
                try buf2.appendSlice(alloc, "return");
            },
            .KwVar => {
                try buf2.appendSlice(alloc, "var");
            },
            .KwMatch => {
                try buf2.appendSlice(alloc, "match");
            },
            .KwWhere => {
                try buf2.appendSlice(alloc, "where");
            },
            .KwWith => {
                try buf2.appendSlice(alloc, "with");
            },

            // If the input has malformed tokens, we don't want to assert anything about it (yet)
            .MalformedNumberBadSuffix,
            .MalformedNumberUnicodeSuffix,
            .MalformedNumberNoDigits,
            .MalformedNumberNoExponentDigits,
            .MalformedInvalidUnicodeEscapeSequence,
            .MalformedInvalidEscapeSequence,
            .MalformedUnicodeIdent,
            .MalformedDotUnicodeIdent,
            .MalformedNoSpaceDotUnicodeIdent,
            .MalformedUnknownToken,
            .MalformedNamedUnderscoreUnicode,
            .MalformedOpaqueNameUnicode,
            .MalformedOpaqueNameWithoutName,
            => {
                return error.Unsupported;
            },
        }
        prev_token_tag = token.tag;
    }
    return buf2;
}

test "tokenizer" {
    const gpa = std.testing.allocator;
    try testTokenization(gpa, "42", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "3.14", &[_]Token.Tag{.Float});
    try testTokenization(gpa, ".", &[_]Token.Tag{.Dot});
    try testTokenization(gpa, "..", &[_]Token.Tag{.DoubleDot});
    try testTokenization(gpa, "1..2", &[_]Token.Tag{ .Int, .DoubleDot, .Int });
    try testTokenization(gpa, "...", &[_]Token.Tag{.TripleDot});
    try testTokenization(gpa, "-", &[_]Token.Tag{.OpUnaryMinus});
    try testTokenization(gpa, "-42", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "1e10", &[_]Token.Tag{.Float});
    try testTokenization(gpa, "_ident", &[_]Token.Tag{.NamedUnderscore});
    try testTokenization(gpa, "1..2", &[_]Token.Tag{ .Int, .DoubleDot, .Int });
    try testTokenization(gpa, "3...4", &[_]Token.Tag{ .Int, .TripleDot, .Int });
    try testTokenization(gpa, "1. .2", &[_]Token.Tag{ .Int, .Dot, .DotInt });
    try testTokenization(gpa, "1.2.3", &[_]Token.Tag{ .Float, .NoSpaceDotInt });
    try testTokenization(gpa, "match", &[_]Token.Tag{.KwMatch});
    try testTokenization(gpa, "var", &[_]Token.Tag{.KwVar});
    try testTokenization(gpa, "{a, b}", &[_]Token.Tag{ .OpenCurly, .LowerIdent, .Comma, .LowerIdent, .CloseCurly });
    try testTokenization(gpa, "\"abc\"", &[_]Token.Tag{ .StringStart, .StringPart, .StringEnd });
    try testTokenization(gpa, "\"a${b}c\"", &[_]Token.Tag{
        .StringStart,
        .StringPart,
        .OpenStringInterpolation,
        .LowerIdent,
        .CloseStringInterpolation,
        .StringPart,
        .StringEnd,
    });
    try testTokenization(
        gpa,
        \\"""abc
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart },
    );
    try testTokenization(
        gpa,
        \\"""abc
        \\"""def
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart, .Newline, .MultilineStringStart, .StringPart },
    );
    try testTokenization(
        gpa,
        \\"""abc"""
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart, .MultilineStringEnd },
    );
    try testTokenization(
        gpa,
        \\"""a${b}c
        \\"""def
    ,
        &[_]Token.Tag{
            .MultilineStringStart,
            .StringPart,
            .OpenStringInterpolation,
            .LowerIdent,
            .CloseStringInterpolation,
            .StringPart,
            .Newline,
            .MultilineStringStart,
            .StringPart,
        },
    );
}
