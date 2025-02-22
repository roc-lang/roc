const std = @import("std");
const collections = @import("../../collections.zig");
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;
const base = @import("../../base.zig");

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

    pub const Tag = enum(u8) {
        EndOfFile,

        Newline,

        // primitives
        Float,
        String,
        SingleQuote,
        Int,
        MalformedNumberBadSuffix, // malformed, but should be treated similar to an int in the parser
        MalformedNumberUnicodeSuffix, // malformed, but should be treated similar to an int in the parser

        // a part of a string interpolation; generally you'll see something like:
        // StringBegin, OpenCurly, <expr>, CloseCurly, StringPart, OpenCurly, <expr>, CloseCurly, StringEnd
        StringBegin,
        StringPart,
        StringEnd,

        // Should be treated as StringPart in the parser, but we forward the error to the ast
        MalformedInvalidUnicodeEscapeSequence,
        MalformedInvalidEscapeSequence,

        // These are not technically valid, but we can have the formatter fix them up.
        SingleQuoteBegin,
        SingleQuotePart,
        SingleQuoteEnd,

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
        OpColon,
        OpArrow,
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
        KwGenerates,
        KwHas,
        KwHosted,
        KwIf,
        KwImplements,
        KwImport,
        KwImports,
        KwInterface,
        KwIs,
        KwModule,
        KwPackage,
        KwPackages,
        KwPlatform,
        KwProvides,
        KwRequires,
        KwThen,
        KwTo,
        KwWhen,
        KwWhere,
        KwWith,

        MalformedUnknownToken,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "app", .KwApp },
        .{ "as", .KwAs },
        .{ "crash", .KwCrash },
        .{ "dbg", .KwDbg },
        .{ "else", .KwElse },
        .{ "expect", .KwExpect },
        .{ "exposes", .KwExposes },
        .{ "generates", .KwGenerates },
        .{ "has", .KwHas },
        .{ "hosted", .KwHosted },
        .{ "if", .KwIf },
        .{ "implements", .KwImplements },
        .{ "import", .KwImport },
        .{ "imports", .KwImports },
        .{ "interface", .KwInterface },
        .{ "is", .KwIs },
        .{ "module", .KwModule },
        .{ "package", .KwPackage },
        .{ "packages", .KwPackages },
        .{ "platform", .KwPlatform },
        .{ "provides", .KwProvides },
        .{ "requires", .KwRequires },
        .{ "then", .KwThen },
        .{ "to", .KwTo },
        .{ "when", .KwWhen },
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

    pub fn isKeyword(tok: Tag) bool {
        return switch (tok) {
            .KwApp,
            .KwAs,
            .KwCrash,
            .KwDbg,
            .KwElse,
            .KwExpect,
            .KwExposes,
            .KwGenerates,
            .KwHas,
            .KwHosted,
            .KwIf,
            .KwImplements,
            .KwImport,
            .KwImports,
            .KwInterface,
            .KwIs,
            .KwPackage,
            .KwPackages,
            .KwPlatform,
            .KwProvides,
            .KwRequires,
            .KwThen,
            .KwTo,
            .KwWhen,
            .KwWhere,
            .KwWith,
            => true,
            else => false,
        };
    }
};

/// The buffer that accumulates tokens.
pub const TokenizedBuffer = struct {
    gpa: std.mem.Allocator,
    tokens: Token.List,
    env: *base.ModuleEnv,

    pub fn init(gpa: std.mem.Allocator, env: *base.ModuleEnv) TokenizedBuffer {
        return TokenizedBuffer{
            .gpa = gpa,
            .tokens = Token.List{},
            .env = env,
        };
    }

    pub fn deinit(self: *TokenizedBuffer) void {
        self.tokens.deinit(self.gpa);
    }

    pub fn resolve(self: *TokenizedBuffer, token: Token.Idx) base.Region {
        const tag = self.tokens.items(.tag)[@intCast(token)];
        const start = self.tokens.items(.offset)[@intCast(token)];
        const extra = self.tokens.items(.extra)[@intCast(token)];
        switch (tag) {
            .LowerIdent,
            .DotLowerIdent,
            .NoSpaceDotLowerIdent,
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

    /// Pushes a token with the given tag, token offset, and extra.
    pub fn pushTokenNormal(self: *TokenizedBuffer, tag: Token.Tag, tok_offset: u32, length: u32) void {
        self.tokens.append(self.gpa, .{
            .tag = tag,
            .offset = tok_offset,
            .extra = .{ .length = length },
        }) catch exitOnOom();
    }

    pub fn pushTokenInterned(self: *TokenizedBuffer, tag: Token.Tag, tok_offset: u32, interned: base.Ident.Idx) void {
        self.tokens.append(self.gpa, .{
            .tag = tag,
            .offset = tok_offset,
            .extra = .{ .interned = interned },
        }) catch exitOnOom();
    }

    pub fn pushNewline(self: *TokenizedBuffer, indent: u32) void {
        self.tokens.append(self.gpa, .{
            .tag = .Newline,
            .offset = indent, // store the indent in the offset field
            .extra = .{ .length = 0 },
        }) catch exitOnOom();
    }

    /// Returns the offset of the token at index `idx`.
    pub fn offset(self: *TokenizedBuffer, idx: u32) u32 {
        // newline tokens don't have offsets - that field is used to store the indent.
        std.debug.assert(self.tokens.items(.tag)[idx] != .Newline);
        return self.tokens.items(.offset)[@intCast(idx)];
    }
};

pub const Comment = struct {
    begin: u32,
    end: u32,
};

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

pub const Diagnostic = struct {
    tag: Tag,
    begin: u32,
    end: u32,

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
    };
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
            } else if (b == '\r') {
                self.pos += 1;
                sawNewline = true;
                indent = 0;
                if (self.pos < self.buf.len and self.buf[self.pos] == '\n') {
                    self.pos += 1;
                } else {
                    self.pushMessageHere(.MisplacedCarriageReturn);
                }
            } else if (b == '#') {
                self.pos += 1;
                while (self.pos < self.buf.len and self.buf[self.pos] != '\n' and self.buf[self.pos] != '\r') {
                    self.pos += 1;
                }
            } else if (b >= 0 and b <= 31) {
                self.pushMessageHere(.AsciiControl);
                self.pos += 1;
            } else {
                break;
            }
        }
        if (sawNewline) {
            return indent;
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
                        self.chompIntegerBase16();
                        tok = self.chompNumberSuffix(tok);
                        break;
                    },
                    'o', 'O' => {
                        maybeMessageForUppercaseBase(self, c);
                        self.pos += 1;
                        self.chompIntegerBase8();
                        tok = self.chompNumberSuffix(tok);
                        break;
                    },
                    'b', 'B' => {
                        maybeMessageForUppercaseBase(self, c);
                        self.pos += 1;
                        self.chompIntegerBase2();
                        tok = self.chompNumberSuffix(tok);
                        break;
                    },
                    '0'...'9' => {
                        self.pushMessageHere(.LeadingZero);
                        _ = self.chompNumberBase10();
                        tok = self.chompNumberSuffix(tok);
                        break;
                    },
                    '_' => {
                        self.pos += 1;
                        continue;
                    },
                    '.' => {
                        self.pos += 1;
                        self.chompIntegerBase10();
                        tok = .Float;
                        _ = self.chompExponent();
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

    pub fn chompExponent(self: *Cursor) bool {
        if (self.peek() orelse 0 == 'e' or self.peek() orelse 0 == 'E') {
            self.pos += 1;
            // Optional sign
            if (self.peek() orelse 0 == '+' or self.peek() orelse 0 == '-') {
                self.pos += 1;
            }
            self.chompIntegerBase10();
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
            return .MalformedNumberUnicodeSuffix;
        }
        const suffix = self.buf[start..self.pos];
        if (Token.valid_number_suffixes.get(suffix) == null) {
            return .MalformedNumberBadSuffix;
        } else {
            return hypothesis;
        }
    }

    pub fn chompNumberBase10(self: *Cursor) Token.Tag {
        self.chompIntegerBase10();
        var token_type: Token.Tag = .Int;
        if (self.peek() orelse 0 == '.' and (self.isPeekedCharInRange(1, '0', '9') or self.peekAt(1) == 'e' or self.peekAt(1) == 'E')) {
            self.pos += 1;
            self.chompIntegerBase10();
            token_type = .Float;
        }
        if (self.chompExponent()) {
            token_type = .Float;
        }
        return token_type;
    }

    pub fn chompIntegerBase10(self: *Cursor) void {
        while (self.peek()) |c| {
            if (c >= '0' and c <= '9') {
                self.pos += 1;
            } else if (c == '_') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    pub fn chompIntegerBase16(self: *Cursor) void {
        while (self.peek()) |c| {
            if ((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
                self.pos += 1;
            } else if (c == '_') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    pub fn chompIntegerBase8(self: *Cursor) void {
        while (self.peek()) |c| {
            if (c >= '0' and c <= '7') {
                self.pos += 1;
            } else if (c == '_') {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    pub fn chompIntegerBase2(self: *Cursor) void {
        while (self.peek()) |c| {
            if (c == '0' or c == '1') {
                self.pos += 1;
            } else if (c == '_') {
                self.pos += 1;
            } else {
                break;
            }
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
};

/// The output of the tokenizer.
pub const TokenOutput = struct {
    tokens: TokenizedBuffer,
    messages: []Diagnostic,
    extra_messages_dropped: usize,
};

const BraceKind = enum {
    Round,
    Square,
    Curly,
    StringInterpolation,
    StringInterpolationMultiline,
    SingleQuoteInterpolation,
    SingleQuoteInterpolationMultiline,

    const List = collections.SafeList(@This());
};

/// The tokenizer that uses a Cursor and produces a TokenizedBuffer.
pub const Tokenizer = struct {
    cursor: Cursor,
    output: TokenizedBuffer,
    stack: std.ArrayList(BraceKind),
    env: *base.ModuleEnv,

    /// Creates a new Tokenizer.
    /// Note that the caller must also provide a pre-allocated messages buffer.
    pub fn init(env: *base.ModuleEnv, text: []const u8, messages: []Diagnostic, allocator: std.mem.Allocator) Tokenizer {
        const cursor = Cursor.init(text, messages);
        const output = TokenizedBuffer.init(allocator, env);
        return Tokenizer{
            .cursor = cursor,
            .output = output,
            .stack = std.ArrayList(BraceKind).init(allocator),
            .env = env,
        };
    }

    pub fn deinit(self: *Tokenizer) void {
        self.output.deinit();
        self.stack.deinit();
    }

    pub fn finish_and_deinit(self: Tokenizer) TokenOutput {
        self.stack.deinit();
        const actual_message_count = @min(self.cursor.message_count, self.cursor.messages.len);
        return .{
            .tokens = self.output,
            .messages = self.cursor.messages[0..actual_message_count],
            .extra_messages_dropped = self.cursor.message_count - actual_message_count,
        };
    }

    fn consumeBraceCloseAndContinueStringInterp(self: *Tokenizer, brace: BraceKind) void {
        std.debug.assert(self.cursor.peek() == close_curly or self.cursor.peek() == ']' or self.cursor.peek() == ')');
        if (self.stack.items.len == 0) {
            self.cursor.pushMessageHere(.OverClosedBrace);
            self.cursor.pos += 1;
            return;
        }
        const last = self.stack.items[self.stack.items.len - 1];
        if (last == brace) {
            self.stack.items = self.stack.items[0 .. self.stack.items.len - 1];
        } else {
            self.cursor.pushMessageHere(.MismatchedBrace);
        }
        self.cursor.pos += 1;
        const start = self.cursor.pos;
        if (self.stack.items.len > 0) {
            const brace_kind = self.stack.items[self.stack.items.len - 1];
            const term: u8 = switch (brace_kind) {
                .StringInterpolation, .StringInterpolationMultiline => '"',
                .SingleQuoteInterpolation, .SingleQuoteInterpolationMultiline => '\'',
                else => return,
            };
            const kind: StringKind = switch (brace_kind) {
                .StringInterpolationMultiline, .SingleQuoteInterpolationMultiline => .multi_line,
                else => .single_line,
            };
            _ = self.stack.pop();
            const tok = self.tokenizeStringLikeLiteralBody(.after_interpolation, kind, term, start);
            self.output.pushTokenNormal(tok, start, self.cursor.pos - start);
        }
    }

    /// The main tokenize loop. This loops over the whole input buffer, tokenizing as it goes.
    pub fn tokenize(self: *Tokenizer) void {
        var sawWhitespace: bool = true;
        while (self.cursor.pos < self.cursor.buf.len) {
            const start = self.cursor.pos;
            const sp = sawWhitespace;
            sawWhitespace = false;
            const b = self.cursor.buf[self.cursor.pos];
            switch (b) {
                // Whitespace & control characters
                0...32, '#' => {
                    if (self.cursor.chompTrivia()) |indent| {
                        self.output.pushNewline(indent);
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
                            const text = self.cursor.buf[text_start..self.cursor.pos];
                            const id = self.env.idents.insert(
                                base.Ident.for_text(text),
                                base.Region{ .start = base.Region.Position{ .offset = start }, .end = base.Region.Position{ .offset = self.cursor.pos } },
                            );
                            self.output.pushTokenInterned(tag, start, id);
                        } else if (n >= 'A' and n <= 'Z') {
                            var tag: Token.Tag = if (sp) .DotUpperIdent else .NoSpaceDotUpperIdent;
                            self.cursor.pos += 1;
                            const text_start = self.cursor.pos;
                            if (!self.cursor.chompIdentGeneral()) {
                                tag = .MalformedDotUnicodeIdent;
                            }
                            const text = self.cursor.buf[text_start..self.cursor.pos];
                            const id = self.env.idents.insert(
                                base.Ident.for_text(text),
                                base.Region{ .start = base.Region.Position{ .offset = start }, .end = base.Region.Position{ .offset = self.cursor.pos } },
                            );
                            self.output.pushTokenInterned(tag, start, id);
                        } else if (n >= 0x80 and n <= 0xff) {
                            self.cursor.pos += 1;
                            _ = self.cursor.chompIdentGeneral();
                            self.output.pushTokenNormal(.MalformedDotUnicodeIdent, start, self.cursor.pos - start);
                        } else if (n == open_curly) {
                            self.cursor.pos += 1;
                            self.output.pushTokenNormal(.Dot, start, 1);
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
                    if (self.cursor.peekAt(1) == '&') {
                        self.cursor.pos += 2;
                        self.output.pushTokenNormal(.OpAnd, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        self.output.pushTokenNormal(.OpAmpersand, start, 1);
                    }
                },

                // Comma (,)
                ',' => {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(.Comma, start, 1);
                },

                // Question mark (?)
                '?' => {
                    self.cursor.pos += 1;
                    self.output.pushTokenNormal(if (sp) .OpQuestion else .NoSpaceOpQuestion, start, 1);
                },

                // Pipe (|)
                '|' => {
                    if (self.cursor.peekAt(1) == '|') {
                        self.cursor.pos += 2;
                        self.output.pushTokenNormal(.OpOr, start, 2);
                    } else if (self.cursor.peekAt(1) == '>') {
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
                    self.stack.append(.Round) catch exitOnOom();
                    self.output.pushTokenNormal(if (sp) .NoSpaceOpenRound else .OpenRound, start, 1);
                },
                '[' => {
                    self.cursor.pos += 1;
                    self.stack.append(.Square) catch exitOnOom();
                    self.output.pushTokenNormal(.OpenSquare, start, 1);
                },
                open_curly => {
                    self.cursor.pos += 1;
                    self.stack.append(.Curly) catch exitOnOom();
                    self.output.pushTokenNormal(.OpenCurly, start, 1);
                },

                ')' => {
                    self.output.pushTokenNormal(.CloseRound, start, 1);
                    self.consumeBraceCloseAndContinueStringInterp(.Round);
                },
                ']' => {
                    self.output.pushTokenNormal(.CloseSquare, start, 1);
                    self.consumeBraceCloseAndContinueStringInterp(.Square);
                },
                close_curly => {
                    self.output.pushTokenNormal(.CloseCurly, start, 1);
                    self.consumeBraceCloseAndContinueStringInterp(.Curly);
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
                    if (tag == .LowerIdent) {
                        const text = self.cursor.buf[start..][0..len];
                        const id = self.env.idents.insert(
                            base.Ident.for_text(text),
                            base.Region{ .start = base.Region.Position{ .offset = start }, .end = base.Region.Position{ .offset = self.cursor.pos } },
                        );
                        self.output.pushTokenInterned(tag, start, id);
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
                    const text = self.cursor.buf[start..self.cursor.pos];
                    const id = self.env.idents.insert(
                        base.Ident.for_text(text),
                        base.Region{ .start = base.Region.Position{ .offset = start }, .end = base.Region.Position{ .offset = self.cursor.pos } },
                    );
                    self.output.pushTokenInterned(tag, start, id);
                },

                // String-like literal starting with a single or double quote
                '"', '\'' => {
                    // Note this may return StringBegin/StringPart instead of String,
                    // in the case of a string interpolation.
                    const tok = self.tokenizeStringLikeLiteral(b);
                    self.output.pushTokenNormal(tok, start, self.cursor.pos - start);
                },

                // first byte of a UTF-8 sequence
                0x80...0xff => {
                    _ = self.cursor.chompIdentGeneral();
                    const len = self.cursor.pos - start;
                    self.output.pushTokenNormal(.MalformedUnicodeIdent, start, len);
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

    pub fn tokenizeStringLikeLiteral(self: *Tokenizer, term: u8) Token.Tag {
        const start = self.cursor.pos;
        // Skip the initial quote.
        self.cursor.pos += 1;
        var kind: StringKind = .single_line;
        if (self.cursor.peek() == term and self.cursor.peekAt(1) == term) {
            self.cursor.pos += 2;
            kind = .multi_line;
        }
        return self.tokenizeStringLikeLiteralBody(.start, kind, term, start);
    }

    const StringState = enum {
        start,
        after_interpolation,
    };

    const StringKind = enum {
        single_line,
        multi_line,
    };

    // Moving curly chars to constants because some editors hate them inline.
    const open_curly = '{';
    const close_curly = '}';

    pub fn tokenizeStringLikeLiteralBody(self: *Tokenizer, state: StringState, kind: StringKind, term: u8, start: u32) Token.Tag {
        var escape: bool = false;
        while (self.cursor.pos < self.cursor.buf.len) {
            const c = self.cursor.buf[self.cursor.pos];
            if (escape) {
                switch (c) {
                    '\\', '"', '\'', 'n', 'r', 't' => {
                        escape = false;
                        self.cursor.pos += 1;
                    },
                    'u' => {
                        escape = false;
                        self.cursor.pos += 1;
                        self.cursor.require('(', .InvalidUnicodeEscapeSequence);
                        while (true) {
                            if (self.cursor.peek() == ')') {
                                self.cursor.pos += 1;
                                break;
                            } else if (self.cursor.peek() != null) {
                                const next = self.cursor.peek() orelse 0;
                                if ((next >= '0' and next <= '9') or
                                    (next >= 'a' and next <= 'f') or
                                    (next >= 'A' and next <= 'F'))
                                {
                                    self.cursor.pos += 1;
                                } else {
                                    self.cursor.pushMessageHere(.InvalidUnicodeEscapeSequence);
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                    },
                    else => {
                        self.cursor.pushMessageHere(.InvalidEscapeSequence);
                        escape = false;
                        self.cursor.pos += 1;
                    },
                }
            } else {
                if (c == '\\') {
                    escape = true;
                    self.cursor.pos += 1;
                } else if (c == '$' and self.cursor.peekAt(1) == open_curly) {
                    self.cursor.pos += 1;
                    var brace: BraceKind = undefined;
                    if (term == '"') {
                        switch (kind) {
                            .multi_line => brace = .StringInterpolationMultiline,
                            .single_line => brace = .StringInterpolation,
                        }
                    } else {
                        switch (kind) {
                            .multi_line => brace = .SingleQuoteInterpolationMultiline,
                            .single_line => brace = .SingleQuoteInterpolation,
                        }
                    }
                    self.stack.append(brace) catch exitOnOom();
                    switch (term) {
                        '"' => {
                            switch (state) {
                                .start => return .StringBegin,
                                .after_interpolation => return .StringPart,
                            }
                        },
                        '\'' => {
                            std.debug.assert(term == '\'');
                            switch (state) {
                                .start => return .SingleQuoteBegin,
                                .after_interpolation => return .SingleQuotePart,
                            }
                        },
                        else => std.debug.assert(false),
                    }
                } else if (c == '\n') {
                    if (kind == .single_line) {
                        self.cursor.pushMessage(.UnclosedString, @intCast(start), @intCast(self.cursor.pos));
                        return .StringBegin;
                    } else {
                        self.cursor.pos += 1;
                    }
                } else {
                    if (kind == .single_line and c == term) {
                        self.cursor.pos += 1;
                        switch (state) {
                            .start => return .String,
                            .after_interpolation => return .StringEnd,
                        }
                    } else if (kind == .multi_line and c == term and self.cursor.peekAt(1) == term and self.cursor.peekAt(2) == term) {
                        self.cursor.pos += 3;
                        switch (state) {
                            .start => return .String,
                            .after_interpolation => return .StringEnd,
                        }
                    }
                    self.cursor.pos += 1;
                }
            }
        }
        const diag: Diagnostic.Tag = if (term == '"') .UnclosedString else .UnclosedSingleQuote;
        self.cursor.pushMessage(diag, start, self.cursor.pos);
        if (state == .after_interpolation) {
            return if (term == '"') .StringEnd else .SingleQuoteEnd;
        } else {
            return if (term == '"') .String else .SingleQuote;
        }
    }
};

fn testTokenization(allocator: std.mem.Allocator, input: []const u8, expected: []const Token.Tag) !void {
    var messages: [10]Diagnostic = undefined;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var env = base.ModuleEnv.init(&arena);

    var tokenizer = Tokenizer.init(&env, input, &messages, allocator);
    defer tokenizer.deinit();

    tokenizer.tokenize();
    const tokenizedBuffer = tokenizer.output;
    const tokens = tokenizedBuffer.tokens.items(.tag);

    try std.testing.expectEqual(tokens[tokens.len - 1], Token.Tag.EndOfFile);

    try std.testing.expectEqual(@as(usize, expected.len), tokens.len - 1);

    for (expected[0..expected.len], tokens[0 .. tokens.len - 1]) |exp, actual| {
        try std.testing.expectEqual(exp, actual);
    }
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
}
