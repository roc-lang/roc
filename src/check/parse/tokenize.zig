const std = @import("std");

// Unicode data tables - allows us to identify upper/lowercase letters for non-ASCII characters.
const GenCatData = @import("GenCatData");

pub const Token = struct {
    tag: Tag,
    offset: u32,
    length: u32,

    pub const Tag = enum(u8) {
        EndOfFile,
        Newline,

        // primitives
        Float,
        String,
        SingleQuote,

        // a part of a string interpolation; generally you'll see something like:
        // StringBegin, OpenCurly, <expr>, CloseCurly StringPart, OpenCurly, <expr>, CloseCurly
        StringBegin,
        StringPart,

        // These are not technically valid, but we can have the formatter fix them up.
        SingleQuoteBegin,
        SingleQuotePart,

        UpperIdent,
        LowerIdent,
        Underscore,
        DotLowerIdent,
        DotInt,
        DotUpperIdent,
        NoSpaceDotInt,
        NoSpaceDotLowerIdent,
        NoSpaceDotUpperIdent,

        NamedUnderscore,
        OpaqueName,
        Int,

        OpenRound,
        CloseRound,
        OpenSquare,
        CloseSquare,
        OpenCurly,
        CloseCurly,

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
            .KwExpectFx,
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
    allocator: std.mem.Allocator,
    tokens: std.MultiArrayList(Token),

    pub fn init(allocator: std.mem.Allocator) !TokenizedBuffer {
        return TokenizedBuffer{
            .allocator = allocator,
            .tokens = std.MultiArrayList(Token){},
        };
    }

    pub fn deinit(self: *TokenizedBuffer) void {
        self.tokens.deinit(self.allocator);
    }

    /// Pushes a token with the given tag, token offset, and length.
    pub fn pushToken(self: *TokenizedBuffer, tag: Token.Tag, tok_offset: u32, tok_length: u32) !void {
        try self.tokens.append(self.allocator, .{
            .tag = tag,
            .offset = tok_offset,
            .length = tok_length,
        });
    }

    pub fn pushNewline(self: *TokenizedBuffer, indent: u32) !void {
        try self.tokens.append(self.allocator, .{
            .tag = .Newline,
            .offset = indent, // store the indent in the offset field
            .length = 0,
        });
    }

    /// Returns the offset of the token at index `idx`.
    pub fn offset(self: *TokenizedBuffer, idx: u32) u32 {
        // newline tokens don't have offsets - that field is used to store the indent.
        std.debug.assert(self.tokens.items(.tag) != .Newline);
        return self.tokens.items(.offset)[@intCast(idx)];
    }
};

pub const Comment = struct {
    begin: usize,
    end: usize,
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
        UnknownToken,
        OpaqueNameWithoutName,
        UppercaseBase,
        InvalidUnicodeEscapeSequence,
        InvalidEscapeSequence,
        UnclosedString,
        UnclosedSingleQuote,
        BadNumberSuffix,
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
    message_count: usize,
    gc: *GenCatData,
    tab_width: u8 = 4, // TODO: make this configurable

    /// Initialize a Cursor with the given input buffer and a pre-allocated messages slice.
    pub fn init(buf: []const u8, messages: []Diagnostic, gc: *GenCatData) Cursor {
        return Cursor{
            .buf = buf,
            .pos = 0,
            .messages = messages,
            .message_count = 0,
            .gc = gc,
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
    pub fn peekAt(self: *Cursor, lookahead: usize) ?u8 {
        if (self.pos + lookahead < self.buf.len) {
            return self.buf[self.pos + lookahead];
        }
        return null;
    }

    pub fn isPeekedCharInRange(self: *Cursor, lookahead: usize, start: u8, end: u8) bool {
        const c = self.peekAt(lookahead);
        return c != null and c >= start and c <= end;
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
                while (self.pos < self.buf.len and self.buf[self.pos] != '\n') {
                    self.pos += 1;
                }
            } else if (b >= 0 and b <= 31) {
                self.pushMessageHere(.AsciiControl);
                self.pos += 1;
            } else {
                break;
            }
        }
        if (sawNewline) return indent;
        return null;
    }

    fn maybeMessageForUppercaseBase(self: *Cursor, b: u8) void {
        if (b == 'X' or b == 'O' or b == 'B') {
            self.pushMessageHere(.UppercaseBase);
        }
    }

    /// Decodes a Unicode character starting at `self.pos` and returns its category.
    /// Note this assumes the caller has already peek'd the first byte.
    pub fn decodeUnicode(self: *Cursor, first_byte: u8) Unicode {
        std.debug.assert(first_byte == self.buf[self.pos]);
        const len3 = std.unicode.utf8ByteSequenceLength(first_byte) catch {
            return .{ .tag = .Invalid, .length = 1 };
        };
        const len: u32 = @intCast(len3);
        const remainder: u32 = @intCast(self.buf.len - self.pos);
        if (remainder < len) {
            return .{ .tag = .Invalid, .length = remainder };
        }
        const utf8_char = std.unicode.utf8Decode(self.buf[self.pos..][0..len]) catch {
            return .{ .tag = .Invalid, .length = len };
        };
        switch (self.gc.gc(utf8_char)) {
            .Lu, .Lt => return .{ .tag = .LetterUpper, .length = len },
            .Ll, .Lm, .Lo => return .{ .tag = .LetterNotUpper, .length = len },
            .Nd, .Nl, .No => return .{ .tag = .Digit, .length = len },
            else => return .{ .tag = .Other, .length = len },
        }
    }

    pub fn chompNumber(self: *Cursor, initialDigit: u8) Token.Tag {
        // Consume the initial digit.
        std.debug.assert(initialDigit == self.buf[self.pos]);
        self.pos += 1;

        var tok: Token.Tag = undefined;
        if (initialDigit == '0') {
            while (true) {
                const c = self.peek() orelse 0;
                switch (c) {
                    'x', 'X' => {
                        maybeMessageForUppercaseBase(self, c);
                        self.pos += 1;
                        self.chompIntegerBase16();
                        self.chompNumberSuffix();
                        tok = .Int;
                        break;
                    },
                    'o', 'O' => {
                        maybeMessageForUppercaseBase(self, c);
                        self.pos += 1;
                        self.chompIntegerBase8();
                        self.chompNumberSuffix();
                        tok = .Int;
                        break;
                    },
                    'b', 'B' => {
                        maybeMessageForUppercaseBase(self, c);
                        self.pos += 1;
                        self.chompIntegerBase2();
                        self.chompNumberSuffix();
                        tok = .Int;
                        break;
                    },
                    '0'...'9' => {
                        self.pushMessageHere(.LeadingZero);
                        _ = self.chompNumberBase10();
                        self.chompNumberSuffix();
                        tok = .Int;
                        break;
                    },
                    '_' => {
                        self.pos += 1;
                        continue;
                    },
                    '.' => {
                        self.pos += 1;
                        _ = self.chompIntegerBase10();
                        tok = .Float;
                        _ = self.chompExponent();
                        break;
                    },
                    else => {
                        tok = .Int;
                        break;
                    },
                }
            }
        } else {
            _ = self.chompNumberBase10();
            self.chompNumberSuffix();
            tok = .Int;
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

    pub fn chompNumberSuffix(self: *Cursor) void {
        if (self.peek() == null or !std.ascii.isAlphabetic(self.peek() orelse 0)) {
            return;
        }
        const start = self.pos;
        var pos = self.pos + 1;
        while (pos < self.buf.len) : (pos += 1) {
            const c = self.buf[pos];
            if (std.ascii.isAlphabetic(c) or std.ascii.isDigit(c)) {
                // continue advancing
            } else {
                break;
            }
        }
        const suffix = self.buf[start..pos];
        if (Token.valid_number_suffixes.get(suffix) == null) {
            self.pushMessageHere(.BadNumberSuffix);
        }
        self.pos = pos;
    }

    pub fn chompNumberBase10(self: *Cursor) Token.Tag {
        self.chompIntegerBase10();
        var token_type: Token.Tag = .Int;
        if (self.peek() orelse 0 == '.') {
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
        self.chompIdentGeneral();
        const ident = self.buf[start..self.pos];
        const kw = Token.keywords.get(ident);
        return kw orelse .LowerIdent;
    }

    /// Chomps a general identifier - either upper or lower case.
    /// Doesn't check if the identifier is a keyword, since we assume the caller already
    /// determined that was impossible (e.g. because the first character was uppercase),
    /// or otherwise not relevant.
    pub fn chompIdentGeneral(self: *Cursor) void {
        while (self.pos < self.buf.len) {
            const c = self.buf[self.pos];
            if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_') {
                self.pos += 1;
            } else {
                const info = self.decodeUnicode(c);
                if (info.tag != .Other and info.tag != .Invalid) {
                    self.pos += info.length;
                } else {
                    break;
                }
            }
        }
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
};

/// The tokenizer that uses a Cursor and produces a TokenizedBuffer.
pub const Tokenizer = struct {
    cursor: Cursor,
    output: TokenizedBuffer,
    stack: std.ArrayList(BraceKind),

    /// Creates a new Tokenizer.
    /// Note that the caller must also provide a pre-allocated messages buffer.
    pub fn init(text: []const u8, messages: []Diagnostic, gc: *GenCatData, allocator: std.mem.Allocator) !Tokenizer {
        const cursor = Cursor.init(text, messages, gc);
        const output = try TokenizedBuffer.init(allocator);
        return Tokenizer{
            .cursor = cursor,
            .output = output,
            .stack = std.ArrayList(BraceKind).init(allocator),
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

    fn pushToken(self: *Tokenizer, tag: Token.Tag, start: u32) !void {
        const len = self.cursor.pos - start;
        try self.output.pushToken(tag, start, len);
    }

    fn consumeBraceCloseAndContinueStringInterp(self: *Tokenizer, brace: BraceKind) !void {
        std.debug.assert(self.cursor.peek() == '}' or self.cursor.peek() == ']' or self.cursor.peek() == ')');
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
            const tok = try self.tokenizeStringLikeLiteralBody(.after_interpolation, kind, term, start);
            try self.output.pushToken(tok, start, self.cursor.pos - start);
        }
    }

    /// The main tokenize loop. This loops over the whole input buffer, tokenizing as it goes.
    pub fn tokenize(self: *Tokenizer) !void {
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
                        try self.output.pushNewline(indent);
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
                                try self.output.pushToken(.TripleDot, start, 3);
                            } else {
                                self.cursor.pos += 2;
                                try self.output.pushToken(.DoubleDot, start, 2);
                            }
                        } else if (n >= '0' and n <= '9') {
                            self.cursor.pos += 1;
                            self.cursor.chompInteger();
                            const len = self.cursor.pos - start;
                            try self.output.pushToken(if (sp) .DotInt else .NoSpaceDotInt, start, len);
                        } else if (n >= 'a' and n <= 'z') {
                            self.cursor.pos += 1;
                            self.cursor.chompIdentGeneral();
                            const len = self.cursor.pos - start;
                            try self.output.pushToken(if (sp) .DotLowerIdent else .NoSpaceDotLowerIdent, start, len);
                        } else if (n >= 'A' and n <= 'Z') {
                            self.cursor.pos += 1;
                            self.cursor.chompIdentGeneral();
                            const len = self.cursor.pos - start;
                            try self.output.pushToken(if (sp) .DotUpperIdent else .NoSpaceDotUpperIdent, start, len);
                        } else if (n >= 0b11000000 and n <= 0xff) {
                            self.cursor.pos += 1;
                            const info = self.cursor.decodeUnicode(n);
                            switch (info.tag) {
                                .LetterUpper => {
                                    self.cursor.pos += info.length;
                                    self.cursor.chompIdentGeneral();
                                    const len = self.cursor.pos - start;
                                    try self.output.pushToken(if (sp) .DotUpperIdent else .NoSpaceDotUpperIdent, start, len);
                                },
                                .LetterNotUpper => {
                                    self.cursor.pos += info.length;
                                    self.cursor.chompIdentGeneral();
                                    const len = self.cursor.pos - start;
                                    try self.output.pushToken(if (sp) .DotLowerIdent else .NoSpaceDotLowerIdent, start, len);
                                },
                                else => {
                                    self.cursor.pos += info.length;
                                    self.cursor.pushMessageHere(.UnknownToken);
                                },
                            }
                        } else if (n == '{') {
                            self.cursor.pos += 1;
                            try self.output.pushToken(.Dot, start, 1);
                        } else {
                            self.cursor.pos += 1;
                            try self.output.pushToken(.Dot, start, 1);
                        }
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.Dot, start, 1);
                    }
                },

                // Minus (-)
                '-' => {
                    const next = self.cursor.peekAt(1);
                    if (next) |n| {
                        if (n == '>') {
                            self.cursor.pos += 2;
                            try self.output.pushToken(.OpArrow, start, 2);
                        } else if (n == ' ' or n == '\t' or n == '\n' or n == '\r' or n == '#') {
                            self.cursor.pos += 1;
                            try self.output.pushToken(.OpBinaryMinus, start, 1);
                        } else if (n >= '0' and n <= '9' and sp) {
                            self.cursor.pos += 1;
                            while (self.cursor.pos < self.cursor.buf.len and std.ascii.isDigit(self.cursor.buf[self.cursor.pos])) {
                                self.cursor.pos += 1;
                            }
                            const len = self.cursor.pos - start;
                            try self.output.pushToken(.Int, start, len);
                        } else {
                            self.cursor.pos += 1;
                            const tokenType: Token.Tag = if (sp) .OpUnaryMinus else .OpBinaryMinus;
                            try self.output.pushToken(tokenType, start, 1);
                        }
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(if (sp) .OpUnaryMinus else .OpBinaryMinus, start, 1);
                    }
                },

                // Exclamation (!)
                '!' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpNotEquals, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpBang, start, 1);
                    }
                },

                // Ampersand (&)
                '&' => {
                    if (self.cursor.peekAt(1) == '&') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpAnd, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpAmpersand, start, 1);
                    }
                },

                // Comma (,)
                ',' => {
                    self.cursor.pos += 1;
                    try self.output.pushToken(.Comma, start, 1);
                },

                // Question mark (?)
                '?' => {
                    self.cursor.pos += 1;
                    try self.output.pushToken(.OpQuestion, start, 1);
                },

                // Pipe (|)
                '|' => {
                    if (self.cursor.peekAt(1) == '|') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpOr, start, 2);
                    } else if (self.cursor.peekAt(1) == '>') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpPizza, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpBar, start, 1);
                    }
                },

                // Plus (+)
                '+' => {
                    self.cursor.pos += 1;
                    try self.output.pushToken(.OpPlus, start, 1);
                },

                // Star (*)
                '*' => {
                    self.cursor.pos += 1;
                    try self.output.pushToken(.OpStar, start, 1);
                },

                // Slash (/)
                '/' => {
                    if (self.cursor.peekAt(1) == '/') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpDoubleSlash, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpSlash, start, 1);
                    }
                },

                // Backslash (\)
                '\\' => {
                    self.cursor.pos += 1;
                    try self.output.pushToken(.OpBackslash, start, 1);
                },

                // Percent (%)
                '%' => {
                    self.cursor.pos += 1;
                    try self.output.pushToken(.OpPercent, start, 1);
                },

                // Caret (^)
                '^' => {
                    self.cursor.pos += 1;
                    try self.output.pushToken(.OpCaret, start, 1);
                },

                // Greater-than (>)
                '>' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpGreaterThanOrEq, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpGreaterThan, start, 1);
                    }
                },

                // Less-than (<)
                '<' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpLessThanOrEq, start, 2);
                    } else if (self.cursor.peekAt(1) == '-') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpBackArrow, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpLessThan, start, 1);
                    }
                },

                // Equals (=)
                '=' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpEquals, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpAssign, start, 1);
                    }
                },

                // Colon (:)
                ':' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.output.pushToken(.OpColonEqual, start, 2);
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpColon, start, 1);
                    }
                },

                '(' => {
                    self.cursor.pos += 1;
                    try self.stack.append(.Round);
                    try self.output.pushToken(.OpenRound, start, 1);
                },
                '[' => {
                    self.cursor.pos += 1;
                    try self.stack.append(.Square);
                    try self.output.pushToken(.OpenSquare, start, 1);
                },
                '{' => {
                    self.cursor.pos += 1;
                    try self.stack.append(.Curly);
                    try self.output.pushToken(.OpenCurly, start, 1);
                },

                ')' => {
                    try self.output.pushToken(.CloseRound, start, 1);
                    try self.consumeBraceCloseAndContinueStringInterp(.Round);
                },
                ']' => {
                    try self.output.pushToken(.CloseSquare, start, 1);
                    try self.consumeBraceCloseAndContinueStringInterp(.Square);
                },
                '}' => {
                    try self.output.pushToken(.CloseCurly, start, 1);
                    try self.consumeBraceCloseAndContinueStringInterp(.Curly);
                },

                '_' => {
                    const next = self.cursor.peekAt(1);
                    if (next) |n| {
                        if ((n >= 'a' and n <= 'z') or (n >= 'A' and n <= 'Z') or (n >= '0' and n <= '9')) {
                            self.cursor.pos += 2;
                            self.cursor.chompIdentGeneral();
                            const len = self.cursor.pos - start;
                            try self.output.pushToken(.NamedUnderscore, start, len);
                        } else {
                            self.cursor.pos += 1;
                            try self.output.pushToken(.Underscore, start, 1);
                        }
                    } else {
                        self.cursor.pos += 1;
                        try self.output.pushToken(.Underscore, start, 1);
                    }
                },

                '@' => {
                    const next = self.cursor.peekAt(1);
                    if (next) |n| {
                        if ((n >= 'a' and n <= 'z') or (n >= 'A' and n <= 'Z') or (n >= '0' and n <= '9')) {
                            self.cursor.pos += 2;
                            self.cursor.chompIdentGeneral();
                            const len = self.cursor.pos - start;
                            try self.output.pushToken(.OpaqueName, start, len);
                        } else {
                            self.cursor.pushMessageHere(.OpaqueNameWithoutName);
                            self.cursor.pos += 1;
                            try self.output.pushToken(.OpaqueName, start, 1);
                        }
                    } else {
                        self.cursor.pushMessageHere(.OpaqueNameWithoutName);
                        self.cursor.pos += 1;
                        try self.output.pushToken(.OpaqueName, start, 1);
                    }
                },

                // Numbers starting with 0-9
                '0'...'9' => {
                    _ = self.cursor.chompNumber(b);
                    const len = self.cursor.pos - start;
                    try self.output.pushToken(.Int, start, len);
                },

                // Lowercase identifiers
                'a'...'z' => {
                    const tag = self.cursor.chompIdentLower();
                    const len = self.cursor.pos - start;
                    try self.output.pushToken(tag, start, len);
                },

                // Uppercase identifiers
                'A'...'Z' => {
                    self.cursor.chompIdentGeneral();
                    const len = self.cursor.pos - start;
                    try self.output.pushToken(.UpperIdent, start, len);
                },

                // String-like literal starting with a single or double quote
                '"', '\'' => {
                    // Note this may return StringBegin/StringPart instead of String,
                    // in the case of a string interpolation.
                    const tok = try self.tokenizeStringLikeLiteral(b);
                    try self.output.pushToken(tok, start, self.cursor.pos - start);
                },

                // first byte of a UTF-8 sequence
                0b11000000...0xff => {
                    const info = self.cursor.decodeUnicode(b);
                    switch (info.tag) {
                        .LetterUpper => {
                            self.cursor.pos += info.length;
                            self.cursor.chompIdentGeneral();
                            const len = self.cursor.pos - start;
                            try self.output.pushToken(.UpperIdent, start, len);
                        },
                        .LetterNotUpper => {
                            self.cursor.pos += info.length;
                            self.cursor.chompIdentGeneral();
                            const len = self.cursor.pos - start;
                            try self.output.pushToken(.LowerIdent, start, len);
                        },
                        else => {
                            self.cursor.pos += info.length;
                            self.cursor.pushMessageHere(.UnknownToken);
                        },
                    }
                },

                // Fallback for any unknown token.
                else => {
                    self.cursor.pushMessageHere(.UnknownToken);
                    self.cursor.pos += 1;
                },
            }
        }

        try self.pushToken(.EndOfFile, 0);
    }

    pub fn tokenizeStringLikeLiteral(self: *Tokenizer, term: u8) !Token.Tag {
        const start = self.cursor.pos;
        // Skip the initial quote.
        self.cursor.pos += 1;
        var kind: StringKind = .single_line;
        if (self.cursor.peek() == term and self.cursor.peekAt(1) == term) {
            self.cursor.pos += 2;
            kind = .multi_line;
        }
        return try self.tokenizeStringLikeLiteralBody(.start, kind, term, start);
    }

    const StringState = enum {
        start,
        after_interpolation,
    };

    const StringKind = enum {
        single_line,
        multi_line,
    };

    pub fn tokenizeStringLikeLiteralBody(self: *Tokenizer, state: StringState, kind: StringKind, term: u8, start: u32) !Token.Tag {
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
                } else if (c == '$' and self.cursor.peekAt(1) == '{') {
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
                    try self.stack.append(brace);
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
                        return .String;
                    } else {
                        self.cursor.pos += 1;
                    }
                } else {
                    if (kind == .single_line and c == term) {
                        self.cursor.pos += 1;
                        switch (state) {
                            .start => return .String,
                            .after_interpolation => return .StringPart,
                        }
                    } else if (kind == .multi_line and c == term and self.cursor.peekAt(1) == term and self.cursor.peekAt(2) == term) {
                        self.cursor.pos += 3;
                        switch (state) {
                            .start => return .String,
                            .after_interpolation => return .StringPart,
                        }
                    }
                    self.cursor.pos += 1;
                }
            }
        }
        const diag: Diagnostic.Tag = if (term == '"') .UnclosedString else .UnclosedSingleQuote;
        self.cursor.pushMessage(diag, start, self.cursor.pos);
        if (state == .after_interpolation) {
            return if (term == '"') .StringPart else .SingleQuotePart;
        } else {
            return if (term == '"') .String else .SingleQuote;
        }
    }
};
