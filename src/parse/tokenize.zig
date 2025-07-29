//! Tokenization functionality for the Roc parser.
//!
//! This module provides the tokenizer that converts Roc source code into
//! a stream of tokens for parsing. It handles all Roc language tokens including
//! keywords, identifiers, literals, operators, and punctuation, representing
//! them as offsets into the source code with additional metadata.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const tracy = @import("tracy");
const compile = @import("compile");

const ModuleEnv = compile.ModuleEnv;

/// representation of a token in the source code, like '+', 'foo', '=', '{'
/// these are represented by an offset into the bytes of the source code
/// and an extra field that stores either the length of the token or
/// an index into the string interner
pub const Token = struct {
    tag: Tag,
    region: base.Region,
    extra: Extra,

    pub const Extra = union {
        interned: base.Ident.Idx,
        ident_with_flags: IdentWithFlags,
        none: u32,
    };

    pub const IdentWithFlags = struct {
        ident: base.Ident.Idx,
        starts_with_underscore: bool,
        ends_with_underscore: bool,
    };

    pub const List = std.MultiArrayList(@This());

    pub const Idx = u32;
    pub const Span = struct { span: base.DataSpan };

    pub const Tag = enum(u8) {
        EndOfFile,

        // primitives
        Float,
        StringStart, // the " that starts a string
        StringEnd, // the " that ends a string
        MultilineStringStart, // the """ that starts a multiline string
        MultilineStringEnd, // the """ that ends a multiline string
        StringPart,
        SingleQuote,
        MalformedSingleQuoteUnclosed, // malformed, but should be treated similar to a SingleQuote in the parser
        MalformedSingleQuoteEmpty, // malformed, but should be treated similar to a SingleQuote in the parser
        MalformedSingleQuoteTooLong, // malformed, but should be treated similar to a SingleQuote in the parser
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
                .MalformedSingleQuoteUnclosed,
                .MalformedSingleQuoteEmpty,
                .MalformedSingleQuoteTooLong,
                => true,
            };
        }

        pub fn isInterned(tok: Tag) bool {
            return switch (tok) {
                .UpperIdent,
                .LowerIdent,
                .DotLowerIdent,
                .DotUpperIdent,
                .NoSpaceDotLowerIdent,
                .NoSpaceDotUpperIdent,
                .NamedUnderscore,
                .MalformedNamedUnderscoreUnicode,
                .MalformedNoSpaceDotUnicodeIdent,
                .MalformedUnicodeIdent,
                .MalformedDotUnicodeIdent,
                .MalformedOpaqueNameUnicode,
                .OpaqueName,
                => true,
                else => false,
            };
        }

        pub fn hasUnderscoreFlags(tok: Tag) bool {
            return switch (tok) {
                .LowerIdent,
                .NamedUnderscore,
                => true,
                else => false,
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
    env: *ModuleEnv,

    pub fn initCapacity(env: *ModuleEnv, capacity: usize) std.mem.Allocator.Error!TokenizedBuffer {
        var tokens = Token.List{};
        try tokens.ensureTotalCapacity(env.gpa, capacity);
        return TokenizedBuffer{
            .tokens = tokens,
            .env = env,
        };
    }

    pub fn deinit(self: *TokenizedBuffer) void {
        self.tokens.deinit(self.env.gpa);
    }

    pub fn resolve(self: *const TokenizedBuffer, idx: usize) base.Region {
        return self.tokens.items(.region)[idx];
    }

    /// Loads the current token if it is an identifier.
    /// Otherwise returns null.
    pub fn resolveIdentifier(self: *TokenizedBuffer, token: Token.Idx) ?base.Ident.Idx {
        const tag = self.tokens.items(.tag)[@intCast(token)];
        const extra = self.tokens.items(.extra)[@intCast(token)];
        if (tag.hasUnderscoreFlags()) {
            return extra.ident_with_flags.ident;
        } else if (tag.isInterned()) {
            return extra.interned;
        } else {
            return null;
        }
    }

    /// Gets underscore flags for identifier tokens.
    /// Returns null if token is not an identifier with underscore flags.
    pub fn resolveUnderscoreFlags(self: *TokenizedBuffer, token: Token.Idx) ?struct { starts_with_underscore: bool, ends_with_underscore: bool } {
        const tag = self.tokens.items(.tag)[@intCast(token)];
        if (tag.hasUnderscoreFlags()) {
            const extra = self.tokens.items(.extra)[@intCast(token)];
            return .{
                .starts_with_underscore = extra.ident_with_flags.starts_with_underscore,
                .ends_with_underscore = extra.ident_with_flags.ends_with_underscore,
            };
        } else {
            return null;
        }
    }
};

/// Represents a diagnostic message including its position in the source.
pub const Diagnostic = struct {
    tag: Tag,
    region: base.Region,

    /// Represents the type of diagnostic message.
    pub const Tag = enum {
        MisplacedCarriageReturn,
        AsciiControl,
        LeadingZero,
        UppercaseBase,
        InvalidUnicodeEscapeSequence,
        InvalidEscapeSequence,
        UnclosedString,
        NonPrintableUnicodeInStrLiteral,
        InvalidUtf8InSource,
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
            self.messages[self.message_count] = Diagnostic{
                .tag = tag,
                .region = base.Region.from_raw_offsets(begin, end),
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

    /// Chomps "trivia" (whitespace, comments, etc.) and returns an optional indent.
    /// If the chomped trivia includes a newline, returns the indent of the next (real) line.
    /// Otherwise, returns null.
    pub fn chompTrivia(self: *Cursor) void {
        while (self.pos < self.buf.len) {
            const b = self.buf[self.pos];
            if (b == ' ') {
                self.pos += 1;
            } else if (b == '\t') {
                self.pos += 1;
            } else if (b == '\n') {
                self.pos += 1;
            } else if (b == '\r') {
                self.pos += 1;
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
    }

    pub fn chompNumber(self: *Cursor) Token.Tag {
        const initialDigit = self.buf[self.pos];
        self.pos += 1;

        var tok = Token.Tag.Int;
        if (initialDigit == '0') {
            while (true) {
                const c = self.peek() orelse 0;
                switch (c) {
                    'x', 'X' => {
                        if (c == 'X') {
                            self.pushMessageHere(.UppercaseBase);
                        }
                        self.pos += 1;
                        self.chompIntegerBase16() catch {
                            tok = .MalformedNumberNoDigits;
                        };
                        tok = self.chompNumberSuffix(tok);
                        break;
                    },
                    'o', 'O' => {
                        if (c == 'O') {
                            self.pushMessageHere(.UppercaseBase);
                        }
                        self.pos += 1;
                        self.chompIntegerBase8() catch {
                            tok = .MalformedNumberNoDigits;
                        };
                        tok = self.chompNumberSuffix(tok);
                        break;
                    },
                    'b', 'B' => {
                        if (c == 'B') {
                            self.pushMessageHere(.UppercaseBase);
                        }
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

    pub fn chompEscapeSequence(self: *Cursor) void {
        switch (self.peek() orelse 0) {
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

    pub fn chompSingleQuoteLiteral(self: *Cursor) Token.Tag {
        const State = union(enum) {
            Empty,
            Enough,
            TooLong,
        };
        std.debug.assert(self.peek() == '\'');

        // Skip the initial quote.
        self.pos += 1;
        var state: State = .Empty;

        while (self.pos < self.buf.len) {
            const c = self.buf[self.pos];

            if (c == '\n') {
                break;
            }

            self.pos += 1;

            switch (state) {
                .Empty => switch (c) {
                    '\'' => {
                        return .MalformedSingleQuoteEmpty;
                    },
                    '\\' => {
                        self.chompEscapeSequence();
                        state = .Enough;
                    },
                    else => {
                        self.pos -= 1;
                        _ = self.chompUTF8CodepointWithValidation();
                        state = .Enough;
                    },
                },
                .Enough => switch (c) {
                    '\'' => {
                        return .SingleQuote;
                    },
                    else => {
                        state = .TooLong;
                    },
                },
                .TooLong => switch (c) {
                    '\'' => {
                        return .MalformedSingleQuoteTooLong;
                    },
                    else => {},
                },
            }
        }

        return .MalformedSingleQuoteUnclosed;
    }

    /// Chomps a UTF-8 codepoint and advances the cursor position.
    /// Reports InvalidUtf8InSource diagnostic if the sequence is invalid or incomplete.
    /// Reports NonPrintableUnicodeInStrLiteral for non-printable characters.
    /// Returns the decoded codepoint if valid, null if invalid.
    fn chompUTF8CodepointWithValidation(self: *Cursor) ?u21 {
        std.debug.assert(self.pos < self.buf.len);
        const c = self.buf[self.pos];

        if (c < 0x80) {
            // Single-byte UTF-8 (ASCII)
            // Allow tab (0x09) in addition to standard printable ASCII
            if (!std.ascii.isPrint(c) and c != '\t') {
                self.pushMessageHere(.NonPrintableUnicodeInStrLiteral);
            }
            self.pos += 1;
            return c;
        }

        const utf8_len = std.unicode.utf8ByteSequenceLength(c) catch {
            self.pushMessageHere(.InvalidUtf8InSource);
            self.pos += 1;
            return null;
        };
        if (self.pos + utf8_len > self.buf.len) {
            // Incomplete UTF-8 sequence at end of input
            self.pushMessageHere(.InvalidUtf8InSource);
            self.pos += 1;
            return null;
        }
        const utf8_bytes = self.buf[self.pos .. self.pos + utf8_len];
        const codepoint = std.unicode.utf8Decode(utf8_bytes) catch {
            self.pushMessageHere(.InvalidUtf8InSource);
            self.pos += 1;
            return null;
        };

        // Check if the Unicode codepoint is printable
        // Unicode categories: Cc (control), Cn (unassigned), Co (private use), Cs (surrogate)
        if (codepoint < 0x20 or // C0 control characters
            (codepoint >= 0x7F and codepoint <= 0x9F) or // C1 control characters
            (codepoint >= 0xD800 and codepoint <= 0xDFFF) or // Surrogate pairs (invalid in UTF-8)
            codepoint == 0xFFFE or codepoint == 0xFFFF) // Non-characters
        {
            self.pushMessageHere(.NonPrintableUnicodeInStrLiteral);
        }

        // Valid UTF-8 sequence - advance by the full sequence length
        self.pos += utf8_len;
        return codepoint;
    }
};

/// The output of the tokenizer.
pub const TokenOutput = struct {
    tokens: TokenizedBuffer,
    messages: []Diagnostic,
    extra_messages_dropped: usize,
};

const StringKind = enum {
    single_line,
    multi_line,
};

/// The tokenizer that uses a Cursor and produces a TokenizedBuffer.
pub const Tokenizer = struct {
    cursor: Cursor,
    output: TokenizedBuffer,
    string_interpolation_stack: std.ArrayListUnmanaged(StringKind),
    env: *ModuleEnv,

    /// Creates a new Tokenizer.
    /// Note that the caller must also provide a pre-allocated messages buffer.
    pub fn init(env: *ModuleEnv, text: []const u8, messages: []Diagnostic) std.mem.Allocator.Error!Tokenizer {
        const cursor = Cursor.init(text, messages);
        // TODO: tune this more. Syntax grab bag is 3:1.
        // Generally, roc code will be less dense than that.
        const output = try TokenizedBuffer.initCapacity(env, text.len);
        return Tokenizer{
            .cursor = cursor,
            .output = output,
            .string_interpolation_stack = .{},
            .env = env,
        };
    }

    pub fn deinit(self: *Tokenizer) void {
        self.output.deinit();
        self.string_interpolation_stack.deinit(self.env.gpa);
    }

    pub fn finishAndDeinit(self: *Tokenizer) TokenOutput {
        self.string_interpolation_stack.deinit(self.env.gpa);
        const actual_message_count = @min(self.cursor.message_count, self.cursor.messages.len);
        return .{
            .tokens = self.output,
            .messages = self.cursor.messages[0..actual_message_count],
            .extra_messages_dropped = self.cursor.message_count - actual_message_count,
        };
    }

    /// Pushes a token with the given tag, token offset, and extra.
    fn pushTokenNormalHere(self: *Tokenizer, tag: Token.Tag, tok_offset: Token.Idx) std.mem.Allocator.Error!void {
        std.debug.assert(!tag.isInterned());
        try self.output.tokens.append(self.env.gpa, .{
            .tag = tag,
            .region = base.Region.from_raw_offsets(tok_offset, self.cursor.pos),
            .extra = .{ .none = 0 },
        });
    }

    fn pushTokenInternedHere(
        self: *Tokenizer,
        tag: Token.Tag,
        tok_offset: Token.Idx,
        text_offset: Token.Idx,
    ) !void {
        const text = self.cursor.buf[text_offset..self.cursor.pos];
        const id = try self.env.idents.insert(self.env.gpa, base.Ident.for_text(text));

        // Use underscore flags for type variable identifiers that benefit from fast checking
        if (tag.hasUnderscoreFlags()) {
            // Calculate underscore flags for identifier tokens
            const starts_with_underscore = text.len > 0 and text[0] == '_';
            const ends_with_underscore = text.len > 1 and text[text.len - 1] == '_';

            try self.output.tokens.append(self.env.gpa, .{
                .tag = tag,
                .extra = .{ .ident_with_flags = .{
                    .ident = id,
                    .starts_with_underscore = starts_with_underscore,
                    .ends_with_underscore = ends_with_underscore,
                } },
                .region = base.Region.from_raw_offsets(tok_offset, self.cursor.pos),
            });
        } else {
            std.debug.assert(tag.isInterned());
            try self.output.tokens.append(self.env.gpa, .{
                .tag = tag,
                .extra = .{ .interned = id },
                .region = base.Region.from_raw_offsets(tok_offset, self.cursor.pos),
            });
        }
    }

    /// The main tokenize loop. This loops over the whole input buffer, tokenizing as it goes.
    pub fn tokenize(self: *Tokenizer) std.mem.Allocator.Error!void {
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
                    self.cursor.chompTrivia();
                    sawWhitespace = true;
                },

                // Dot (.)
                '.' => {
                    const next = self.cursor.peekAt(1);
                    if (next) |n| {
                        if (n == '.') {
                            if (self.cursor.peekAt(2) == '.') {
                                self.cursor.pos += 3;
                                try self.pushTokenNormalHere(.TripleDot, start);
                            } else {
                                self.cursor.pos += 2;
                                try self.pushTokenNormalHere(.DoubleDot, start);
                            }
                        } else if (n >= '0' and n <= '9') {
                            self.cursor.pos += 1;
                            self.cursor.chompInteger();
                            try self.pushTokenNormalHere(if (sp) .DotInt else .NoSpaceDotInt, start);
                        } else if (n >= 'a' and n <= 'z') {
                            var tag: Token.Tag = if (sp) .DotLowerIdent else .NoSpaceDotLowerIdent;
                            self.cursor.pos += 1;
                            const text_start = self.cursor.pos;
                            if (!self.cursor.chompIdentGeneral()) {
                                tag = .MalformedDotUnicodeIdent;
                            }
                            try self.pushTokenInternedHere(tag, start, text_start);
                        } else if (n >= 'A' and n <= 'Z') {
                            var tag: Token.Tag = if (sp) .DotUpperIdent else .NoSpaceDotUpperIdent;
                            self.cursor.pos += 1;
                            const text_start = self.cursor.pos;
                            if (!self.cursor.chompIdentGeneral()) {
                                tag = .MalformedDotUnicodeIdent;
                            }
                            try self.pushTokenInternedHere(tag, start, text_start);
                        } else if (n >= 0x80 and n <= 0xff) {
                            self.cursor.pos += 1;
                            const text_start = self.cursor.pos;
                            _ = self.cursor.chompIdentGeneral();
                            try self.pushTokenInternedHere(.MalformedDotUnicodeIdent, start, text_start);
                        } else if (n == open_curly) {
                            self.cursor.pos += 1;
                            try self.pushTokenNormalHere(.Dot, start);
                        } else if (n == '*') {
                            self.cursor.pos += 2;
                            try self.pushTokenNormalHere(.DotStar, start);
                        } else {
                            self.cursor.pos += 1;
                            try self.pushTokenNormalHere(.Dot, start);
                        }
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.Dot, start);
                    }
                },

                // Minus (-)
                '-' => {
                    const next = self.cursor.peekAt(1);
                    if (next) |n| {
                        if (n == '>') {
                            self.cursor.pos += 2;
                            try self.pushTokenNormalHere(.OpArrow, start);
                        } else if (n == ' ' or n == '\t' or n == '\n' or n == '\r' or n == '#') {
                            self.cursor.pos += 1;
                            try self.pushTokenNormalHere(.OpBinaryMinus, start);
                        } else if (n >= '0' and n <= '9') {
                            self.cursor.pos += 1;
                            const tag = self.cursor.chompNumber();
                            try self.pushTokenNormalHere(tag, start);
                        } else {
                            self.cursor.pos += 1;
                            // Look at what follows the minus to determine if it's unary
                            const tokenType: Token.Tag = if (self.canFollowUnaryMinus(n)) .OpUnaryMinus else .OpBinaryMinus;
                            try self.pushTokenNormalHere(tokenType, start);
                        }
                    } else {
                        self.cursor.pos += 1;
                        // No next character - default to unary
                        try self.pushTokenNormalHere(.OpUnaryMinus, start);
                    }
                },

                // Exclamation (!)
                '!' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpNotEquals, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.OpBang, start);
                    }
                },

                // Ampersand (&)
                '&' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.OpAmpersand, start);
                },

                // Comma (,)
                ',' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.Comma, start);
                },

                // Question mark (?)
                '?' => {
                    if (self.cursor.peekAt(1) == '?') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpDoubleQuestion, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(if (sp) .OpQuestion else .NoSpaceOpQuestion, start);
                    }
                },

                // Pipe (|)
                '|' => {
                    if (self.cursor.peekAt(1) == '>') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpPizza, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.OpBar, start);
                    }
                },

                // Plus (+)
                '+' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.OpPlus, start);
                },

                // Star (*)
                '*' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.OpStar, start);
                },

                // Slash (/)
                '/' => {
                    if (self.cursor.peekAt(1) == '/') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpDoubleSlash, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.OpSlash, start);
                    }
                },

                // Backslash (\)
                '\\' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.OpBackslash, start);
                },

                // Percent (%)
                '%' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.OpPercent, start);
                },

                // Caret (^)
                '^' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.OpCaret, start);
                },

                // Greater-than (>)
                '>' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpGreaterThanOrEq, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.OpGreaterThan, start);
                    }
                },

                // Less-than (<)
                '<' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpLessThanOrEq, start);
                    } else if (self.cursor.peekAt(1) == '-') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpBackArrow, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.OpLessThan, start);
                    }
                },

                // Equals (=)
                '=' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpEquals, start);
                    } else if (self.cursor.peekAt(1) == '>') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpFatArrow, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.OpAssign, start);
                    }
                },

                // Colon (:)
                ':' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(.OpColonEqual, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.OpColon, start);
                    }
                },

                '(' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(if (sp) .OpenRound else .NoSpaceOpenRound, start);
                },
                '[' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.OpenSquare, start);
                },
                open_curly => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.OpenCurly, start);
                },

                ')' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.CloseRound, start);
                },
                ']' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.CloseSquare, start);
                },
                close_curly => {
                    self.cursor.pos += 1;
                    if (self.string_interpolation_stack.pop()) |last| {
                        try self.pushTokenNormalHere(.CloseStringInterpolation, start);
                        try self.tokenizeStringLikeLiteralBody(last);
                    } else {
                        try self.pushTokenNormalHere(.CloseCurly, start);
                    }
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
                            try self.pushTokenInternedHere(tok, start, start);
                        } else {
                            self.cursor.pos += 1;
                            try self.pushTokenNormalHere(.Underscore, start);
                        }
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(.Underscore, start);
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
                    if (tok.isInterned()) {
                        try self.pushTokenInternedHere(tok, start, start);
                    } else {
                        try self.pushTokenNormalHere(tok, start);
                    }
                },

                // Numbers starting with 0-9
                '0'...'9' => {
                    const tag = self.cursor.chompNumber();
                    try self.pushTokenNormalHere(tag, start);
                },

                // Lowercase identifiers
                'a'...'z' => {
                    const tag = self.cursor.chompIdentLower();
                    if (tag == .LowerIdent or tag == .MalformedUnicodeIdent) {
                        try self.pushTokenInternedHere(tag, start, start);
                    } else {
                        try self.pushTokenNormalHere(tag, start);
                    }
                },

                // Uppercase identifiers
                'A'...'Z' => {
                    var tag: Token.Tag = .UpperIdent;
                    if (!self.cursor.chompIdentGeneral()) {
                        tag = .MalformedUnicodeIdent;
                    }
                    try self.pushTokenInternedHere(tag, start, start);
                },

                '\'' => {
                    const tag = self.cursor.chompSingleQuoteLiteral();
                    try self.pushTokenNormalHere(tag, start);
                },

                '"' => {
                    // Note this may return StringStart/StringPart instead of String,
                    // in the case of a string interpolation.
                    try self.tokenizeStringLikeLiteral();
                },

                // first byte of a UTF-8 sequence
                0x80...0xff => {
                    _ = self.cursor.chompIdentGeneral();
                    try self.pushTokenInternedHere(.MalformedUnicodeIdent, start, start);
                },

                // TODO: emit a MalformedOpToken for invalid combinations of operator-like characters

                // Fallback for any unknown token.
                else => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.MalformedUnknownToken, start);
                },
            }
        }

        try self.pushTokenNormalHere(.EndOfFile, self.cursor.pos);
    }

    /// Determines if a character can follow a unary minus (i.e., can start an expression)
    fn canFollowUnaryMinus(self: *const Tokenizer, c: u8) bool {
        _ = self;
        return switch (c) {
            // Identifiers
            'a'...'z', 'A'...'Z', '_' => true,
            // Parentheses for grouped expressions
            '(' => true,
            // Unicode characters that might start identifiers
            0x80...0xFF => true,
            // Everything else suggests binary minus
            else => false,
        };
    }

    pub fn tokenizeStringLikeLiteral(self: *Tokenizer) std.mem.Allocator.Error!void {
        const start = self.cursor.pos;
        std.debug.assert(self.cursor.peek() == '"');
        self.cursor.pos += 1;
        var kind: StringKind = .single_line;
        if (self.cursor.peek() == '"' and self.cursor.peekAt(1) == '"') {
            self.cursor.pos += 2;
            kind = .multi_line;
            try self.pushTokenNormalHere(.MultilineStringStart, start);
        } else {
            try self.pushTokenNormalHere(.StringStart, start);
        }
        try self.tokenizeStringLikeLiteralBody(kind);
    }

    // Moving curly chars to constants because some editors hate them inline.
    const open_curly = '{';
    const close_curly = '}';

    pub fn tokenizeStringLikeLiteralBody(self: *Tokenizer, kind: StringKind) std.mem.Allocator.Error!void {
        const start = self.cursor.pos;
        var escape: bool = false;
        while (self.cursor.pos < self.cursor.buf.len) {
            const c = self.cursor.buf[self.cursor.pos];
            if (escape) {
                escape = false;
                self.cursor.chompEscapeSequence();
            } else {
                if (c == '$' and self.cursor.peekAt(1) == open_curly) {
                    try self.pushTokenNormalHere(.StringPart, start);
                    const dollar_start = self.cursor.pos;
                    self.cursor.pos += 2;
                    try self.pushTokenNormalHere(.OpenStringInterpolation, dollar_start);
                    try self.string_interpolation_stack.append(self.env.gpa, kind);
                    return;
                } else if (c == '\n') {
                    try self.pushTokenNormalHere(.StringPart, start);
                    if (kind == .single_line) {
                        self.cursor.pushMessage(.UnclosedString, @intCast(start), @intCast(self.cursor.pos));
                        try self.pushTokenNormalHere(.StringEnd, self.cursor.pos);
                    }
                    return;
                } else if (kind == .single_line and c == '"') {
                    try self.pushTokenNormalHere(.StringPart, start);
                    const string_part_end = self.cursor.pos;
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(.StringEnd, string_part_end);
                    return;
                } else if (kind == .multi_line and c == '"' and self.cursor.peekAt(1) == '"' and self.cursor.peekAt(2) == '"') {
                    try self.pushTokenNormalHere(.StringPart, start);
                    const quote_start = self.cursor.pos;
                    self.cursor.pos += 3;
                    try self.pushTokenNormalHere(.MultilineStringEnd, quote_start);
                    return;
                } else {
                    escape = c == '\\';

                    // Handle UTF-8 sequences with printable character validation
                    _ = self.cursor.chompUTF8CodepointWithValidation();
                }
            }
        }
        if (kind == .single_line) {
            self.cursor.pushMessage(.UnclosedString, start, self.cursor.pos);
        }
        try self.pushTokenNormalHere(.StringPart, start);
    }
};

fn testTokenization(gpa: std.mem.Allocator, input: []const u8, expected: []const Token.Tag) !void {
    var messages: [10]Diagnostic = undefined;

    var env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer env.deinit();

    var tokenizer = try Tokenizer.init(&env, input, &messages);
    defer tokenizer.deinit();

    try tokenizer.tokenize();
    const tokenizedBuffer = tokenizer.output;
    const tokens = tokenizedBuffer.tokens.items(.tag);

    try std.testing.expectEqual(tokens[tokens.len - 1], Token.Tag.EndOfFile);
    try std.testing.expectEqualSlices(Token.Tag, expected[0..expected.len], tokens[0 .. tokens.len - 1]);

    try checkTokenizerInvariants(gpa, input, false);
}

/// Assert the invariants of the tokenizer are held.
pub fn checkTokenizerInvariants(gpa: std.mem.Allocator, input: []const u8, debug: bool) std.mem.Allocator.Error!void {
    var env = try ModuleEnv.init(gpa, gpa.dupe(u8, "") catch unreachable);
    defer env.deinit();

    // Initial tokenization.
    var messages: [32]Diagnostic = undefined;
    var tokenizer = try Tokenizer.init(&env, input, &messages);
    try tokenizer.tokenize();
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
    tokenizer = try Tokenizer.init(&env, buf2.items, &messages);
    try tokenizer.tokenize();
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
        const region1 = output.tokens.resolve(token_index);
        const region2 = output2.tokens.resolve(token_index);
        const length1 = region1.end.offset - region1.start.offset;
        const length2 = region2.end.offset - region2.start.offset;
        same = same and (token.tag == token2.tag);
        same = same and (length1 == length2);
    }

    if (!same) {
        var prefix_len: usize = 0;
        var suffix_len: usize = 0;
        while (prefix_len < output.tokens.tokens.len and prefix_len < output2.tokens.tokens.len) : (prefix_len += 1) {
            const token = output.tokens.tokens.get(prefix_len);
            const token2 = output2.tokens.tokens.get(prefix_len);
            const region1 = output.tokens.resolve(prefix_len);
            const region2 = output2.tokens.resolve(prefix_len);

            if (token.tag != token2.tag or region1.start.offset != region2.start.offset) {
                break;
            }
        }

        while (suffix_len < output.tokens.tokens.len - prefix_len and suffix_len < output2.tokens.tokens.len - prefix_len) : (suffix_len += 1) {
            const token = output.tokens.tokens.get(output.tokens.tokens.len - suffix_len - 1);
            const token2 = output2.tokens.tokens.get(output2.tokens.tokens.len - suffix_len - 1);
            const region1 = output.tokens.resolve(output.tokens.tokens.len - suffix_len - 1);
            const region2 = output2.tokens.resolve(output2.tokens.tokens.len - suffix_len - 1);

            if (token.tag != token2.tag or region1.start.offset != region2.start.offset) {
                break;
            }
        }

        std.debug.print("...\n", .{});
        for (prefix_len..output.tokens.tokens.len - suffix_len) |token_index| {
            const region = output.tokens.resolve(token_index);
            const token = output.tokens.tokens.get(token_index);
            std.debug.print("\x1b[31m\t- {any}\x1b[0m: {s}\n", .{ token, input[region.start.offset..region.end.offset] });
        }
        for (prefix_len..output2.tokens.tokens.len - suffix_len) |token_index| {
            const region = output2.tokens.resolve(token_index);
            const token = output2.tokens.tokens.get(token_index);
            std.debug.print("\x1b[32m\t+ {any}\x1b[0m: {s}\n", .{ token, buf2.items[region.start.offset..region.end.offset] });
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
        // EndOfFile is special, handle it early.
        // Unlike other tokens it does not store a correct offset and length
        // EndOfFile consumes the entire file.

        // Copy over limited whitespace.
        // TODO: Long term, we should switch to dummy whitespace, but currently, Roc still has WSS.
        const region = tokens.resolve(token_index);
        for (last_end..region.start.offset) |i| {
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
        std.debug.assert(region.end.offset >= region.start.offset);
        std.debug.assert(region.end.offset <= buf.len);
        std.debug.assert(region.start.offset >= last_end);
        last_end = region.end.offset;
        const length = region.end.offset - region.start.offset;
        switch (token.tag) {
            .EndOfFile => unreachable,

            .Float => {
                if (buf[region.start.offset] == '-') {
                    try buf2.append(alloc, '-');
                    try buf2.append(alloc, '0');
                    try buf2.append(alloc, '.');
                    for (3..length) |_| {
                        try buf2.append(alloc, '1');
                    }
                } else {
                    try buf2.append(alloc, '0');
                    try buf2.append(alloc, '.');
                    for (2..length) |_| {
                        try buf2.append(alloc, '1');
                    }
                }
            },
            .SingleQuote => {
                if (length == 3) {
                    try buf2.append(alloc, '\'');
                    try buf2.append(alloc, 'A');
                    try buf2.append(alloc, '\'');
                } else if (length == 4) {
                    if (buf[region.start.offset + 1] == '\\') {
                        try buf2.append(alloc, '\'');
                        try buf2.append(alloc, '\\');
                        try buf2.append(alloc, '\\');
                        try buf2.append(alloc, '\'');
                    } else {
                        try buf2.append(alloc, '\'');
                        // Å
                        try buf2.append(alloc, 0xC3);
                        try buf2.append(alloc, 0x85);
                        try buf2.append(alloc, '\'');
                    }
                } else if (length == 5) {
                    try buf2.append(alloc, '\'');
                    // Ἀ
                    try buf2.append(alloc, 0xE1);
                    try buf2.append(alloc, 0xBC);
                    try buf2.append(alloc, 0x88);
                    try buf2.append(alloc, '\'');
                } else if (length == 6) {
                    try buf2.append(alloc, '\'');
                    // 𐙝
                    try buf2.append(alloc, 0xF0);
                    try buf2.append(alloc, 0x90);
                    try buf2.append(alloc, 0x99);
                    try buf2.append(alloc, 0x9D);
                    try buf2.append(alloc, '\'');
                } else {
                    try buf2.append(alloc, '\'');
                    try buf2.append(alloc, '\\');
                    try buf2.append(alloc, 'u');
                    try buf2.append(alloc, '(');
                    for (6..length) |_| {
                        try buf2.append(alloc, 'A');
                    }
                    try buf2.append(alloc, ')');
                    try buf2.append(alloc, '\'');
                }
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
                if (buf[region.start.offset] == '-') {
                    try buf2.append(alloc, '-');
                    for (1..length) |_| {
                        try buf2.append(alloc, '1');
                    }
                } else if (length >= 3) {
                    // To ensure this value when reprinted tokenizes as an int, add a base if the number is 3 or more characters.
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
            .MalformedSingleQuoteEmpty,
            .MalformedSingleQuoteTooLong,
            .MalformedSingleQuoteUnclosed,
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
        &[_]Token.Tag{ .MultilineStringStart, .StringPart, .MultilineStringStart, .StringPart },
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
            .MultilineStringStart,
            .StringPart,
        },
    );
}

test "tokenizer with invalid UTF-8" {
    const gpa = std.testing.allocator;

    // Invalid UTF-8 start byte
    {
        const invalid_utf8 = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xFF, ' ', 'w', 'o', 'r', 'l', 'd', '"' };
        var diagnostics: [10]Diagnostic = undefined;

        var env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit();

        var tokenizer = try Tokenizer.init(&env, &invalid_utf8, &diagnostics);
        defer tokenizer.deinit();
        try tokenizer.tokenize();

        // Should have reported InvalidUtf8InSource
        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.InvalidUtf8InSource, messages[0].tag);
    }

    // Incomplete UTF-8 sequence at end
    {
        const incomplete_utf8 = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xC3, '"' }; // 0xC3 expects another byte
        var diagnostics: [10]Diagnostic = undefined;

        var env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit();

        var tokenizer = try Tokenizer.init(&env, &incomplete_utf8, &diagnostics);
        defer tokenizer.deinit();
        try tokenizer.tokenize();

        // Should have reported InvalidUtf8InSource
        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.InvalidUtf8InSource, messages[0].tag);
    }
}

test "non-printable characters in string literal" {
    const gpa = std.testing.allocator;

    // Non-printable ASCII
    {
        const non_printable = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', '\x01', 'w', 'o', 'r', 'l', 'd', '"' }; // 0x01 is SOH (non-printable)
        var diagnostics: [10]Diagnostic = undefined;

        var env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit();

        var tokenizer = try Tokenizer.init(&env, &non_printable, &diagnostics);
        defer tokenizer.deinit();
        try tokenizer.tokenize();

        // Should have reported NonPrintableUnicodeInStrLiteral
        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.NonPrintableUnicodeInStrLiteral, messages[0].tag);
    }

    // Non-printable (but valid) non-ASCII Unicode characters
    {
        const control_char = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xC2, 0x80, ' ', 'w', 'o', 'r', 'l', 'd', '"' }; // U+0080 (C1 control)
        var diagnostics: [10]Diagnostic = undefined;

        var env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit();

        var tokenizer = try Tokenizer.init(&env, &control_char, &diagnostics);
        defer tokenizer.deinit();
        try tokenizer.tokenize();

        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.NonPrintableUnicodeInStrLiteral, messages[0].tag);
    }

    // No errors should be reported for these
    {
        const valid_chars = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', '\t', ' ', 'w', 'o', 'r', 'l', 'd', '"' };
        var diagnostics: [10]Diagnostic = undefined;

        var env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit();

        var tokenizer = try Tokenizer.init(&env, &valid_chars, &diagnostics);
        defer tokenizer.deinit();
        try tokenizer.tokenize();

        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len == 0);
    }
}
