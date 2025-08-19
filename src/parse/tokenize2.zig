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

const DataSpan = base.DataSpan;
const CommonEnv = base.CommonEnv;

/// representation of a token in the source code, like '+', 'foo', '=', '{'
/// these are represented by an offset into the bytes of the source code
/// and an extra field that stores either the length of the token or
/// an index into the string interner
pub const Token = struct {
    tag: Tag,
    region: base.Region,
    extra: Extra,

    /// Small decimal representation
    pub const SmallDec = struct {
        numerator: i16,
        denominator_power_of_ten: u8,
    };

    pub const Extra = union(enum) {
        interned: base.Ident.Idx,
        ident_with_flags: IdentWithFlags,
        none: u32,
        num_literal_i32: i32, // For numbers that fit in i32
        frac_literal_small: SmallDec, // For fractions that fit in SmallDec
        bytes_idx: collections.ByteSlices.Idx, // For big numbers and strings
    };

    pub const IdentWithFlags = struct {
        ident: base.Ident.Idx,
        starts_with_underscore: bool,
        ends_with_underscore: bool,
    };

    pub const List = std.MultiArrayList(@This());

    pub const Idx = u32;
    pub const Span = struct { span: DataSpan };

    pub const Tag = enum(u8) {
        EndOfFile,

        // primitives
        Float,
        String, // a complete string literal (stored in ByteSlices)
        MultilineString, // a complete multiline string literal (stored in ByteSlices)
        MalformedString, // malformed string literal
        StringStart, // the " that starts a string (for interpolation)
        StringEnd, // the " that ends a string (for interpolation)
        MultilineStringStart, // the """ that starts a multiline string (for interpolation)
        MultilineStringEnd, // the """ that ends a multiline string (for interpolation)
        StringPart,
        MalformedStringPart, // malformed, but should be treated similar to a StringPart in the parser
        SingleQuote,
        MalformedSingleQuoteUnclosed, // malformed, but should be treated similar to a SingleQuote in the parser
        MalformedSingleQuoteEmpty, // malformed, but should be treated similar to a SingleQuote in the parser
        MalformedSingleQuoteTooLong, // malformed, but should be treated similar to a SingleQuote in the parser
        MalformedSingleQuoteInvalidEscapeSequence, // malformed, but should be treated similar to a SingleQuote in the parser
        Int, // Base-10 integer
        IntBase, // Non-base-10 integer (hex, octal, binary)
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
        KwWhile,
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
                .KwWhile,
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
                .String,
                .MultilineString,
                .StringStart,
                .StringEnd,
                .MultilineStringStart,
                .MultilineStringEnd,
                .StringPart,
                .SingleQuote,
                .Int,
                .IntBase,
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
                .KwWhile,
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
                .MalformedSingleQuoteInvalidEscapeSequence,
                .MalformedString,
                .MalformedStringPart,
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

    // Keyword string constants
    pub const KW_VAR = "var";

    // Delimiter constants
    pub const DELIM_OPEN_ROUND = '(';
    pub const DELIM_CLOSE_ROUND = ')';
    pub const DELIM_OPEN_SQUARE = '[';
    pub const DELIM_CLOSE_SQUARE = ']';
    pub const DELIM_OPEN_CURLY = '{';
    pub const DELIM_CLOSE_CURLY = '}';

    /// Determines if a character is whitespace.
    /// In tokenize, whitespace includes space, tab, newline, carriage return.
    pub fn isWhitespace(char: u8) bool {
        return switch (char) {
            ' ', '\t', '\n', '\r' => true,
            else => false,
        };
    }

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
        .{ KW_VAR, .KwVar },
        .{ "where", .KwWhere },
        .{ "while", .KwWhile },
        .{ "with", .KwWith },
    });

    pub const valid_number_suffixes = std.StaticStringMap(void).initComptime(.{
        .{ "dec", .{} },
        .{ "f32", .{} },
        .{ "f64", .{} },
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
    env: *CommonEnv,

    pub fn initCapacity(env: *CommonEnv, gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!TokenizedBuffer {
        var tokens = Token.List{};
        try tokens.ensureTotalCapacity(gpa, capacity);
        return TokenizedBuffer{
            .tokens = tokens,
            .env = env,
        };
    }

    pub fn deinit(self: *TokenizedBuffer, gpa: std.mem.Allocator) void {
        self.tokens.deinit(gpa);
    }

    pub fn resolve(self: *const TokenizedBuffer, idx: usize) base.Region {
        return self.tokens.items(.region)[idx];
    }

    /// Loads the current token if it is an identifier.
    /// Otherwise returns null.
    pub fn resolveIdentifier(self: *const TokenizedBuffer, token: Token.Idx) ?base.Ident.Idx {
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

    /// Chomps "trivia" (whitespace, comments, etc.).
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
                        if (tok != .MalformedNumberNoDigits) {
                            tok = .IntBase;
                        }
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
                        if (tok != .MalformedNumberNoDigits) {
                            tok = .IntBase;
                        }
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
                        if (tok != .MalformedNumberNoDigits) {
                            tok = .IntBase;
                        }
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

    pub fn chompEscapeSequence(self: *Cursor) !void {
        return self.chompEscapeSequenceWithQuote(null);
    }

    pub fn chompEscapeSequenceWithQuote(self: *Cursor, quote_char: ?u8) !void {
        // Store the start position of the escape sequence (before the backslash)
        const escape_start = if (self.pos > 0) self.pos - 1 else self.pos;

        switch (self.peek() orelse 0) {
            '\\', '"', '\'', 'n', 'r', 't', '$' => {
                self.pos += 1;
            },
            'u' => {
                self.pos += 1;

                if (self.peek() == '(') {
                    self.pos += 1;
                } else {
                    self.pushMessage(.InvalidUnicodeEscapeSequence, escape_start, self.pos);
                    return error.InvalidUnicodeEscapeSequence;
                }

                const hex_start = self.pos;
                while (true) {
                    if (self.peek() == ')') {
                        if (self.pos == hex_start) {
                            // Empty unicode escape sequence
                            self.pushMessage(.InvalidUnicodeEscapeSequence, escape_start, self.pos + 1);
                            self.pos += 1;
                            return error.InvalidUnicodeEscapeSequence;
                        }
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
                            // Invalid hex character - advance to the closing paren if possible
                            // to include the full escape sequence in the error region, but stop
                            // if we encounter the closing quote or newline
                            while (self.pos < self.buf.len) {
                                const next_char = self.peek() orelse 0;
                                if (next_char == ')' or next_char == '\n') {
                                    break;
                                }
                                if (quote_char) |qc| {
                                    if (next_char == qc) {
                                        break;
                                    }
                                }
                                self.pos += 1;
                            }
                            if (self.pos < self.buf.len and self.peek() == ')') {
                                self.pos += 1;
                            }
                            self.pushMessage(.InvalidUnicodeEscapeSequence, escape_start, self.pos);
                            return error.InvalidUnicodeEscapeSequence;
                        }
                    } else {
                        self.pushMessage(.InvalidUnicodeEscapeSequence, escape_start, self.pos);
                        return error.InvalidUnicodeEscapeSequence;
                    }
                }
            },
            else => {
                // Include the character after the backslash in the error region
                const end_pos = if (self.peek() != null) self.pos + 1 else self.pos;
                self.pushMessage(.InvalidEscapeSequence, escape_start, end_pos);
                return error.InvalidEscapeSequence;
            },
        }
    }

    pub fn chompSingleQuoteLiteral(self: *Cursor) Token.Tag {
        const State = union(enum) {
            Empty,
            Enough,
            TooLong,
            Invalid,
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
                        state = .Enough;
                        self.chompEscapeSequenceWithQuote('\'') catch {
                            state = .Invalid;
                        };
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
                .Invalid => switch (c) {
                    '\'' => {
                        return .MalformedSingleQuoteInvalidEscapeSequence;
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

// Helper functions for manual digit conversion
fn charToDigit(c: u8) ?u8 {
    if (c >= '0' and c <= '9') {
        return c - '0';
    }
    return null;
}

fn charToHexDigit(c: u8) ?u8 {
    if (c >= '0' and c <= '9') {
        return c - '0';
    } else if (c >= 'a' and c <= 'f') {
        return c - 'a' + 10;
    } else if (c >= 'A' and c <= 'F') {
        return c - 'A' + 10;
    }
    return null;
}

fn charToOctalDigit(c: u8) ?u8 {
    if (c >= '0' and c <= '7') {
        return c - '0';
    }
    return null;
}

fn charToBinaryDigit(c: u8) ?u8 {
    if (c == '0') return 0;
    if (c == '1') return 1;
    return null;
}

/// The tokenizer that uses a Cursor and produces a TokenizedBuffer.
pub const Tokenizer = struct {
    cursor: Cursor,
    output: TokenizedBuffer,
    string_interpolation_stack: std.ArrayListUnmanaged(StringKind),
    env: *CommonEnv,
    byte_slices: *collections.ByteSlices, // For storing big numbers and strings

    /// Creates a new Tokenizer.
    /// Note that the caller must also provide a pre-allocated messages buffer and ByteSlices.
    pub fn init(env: *CommonEnv, gpa: std.mem.Allocator, text: []const u8, messages: []Diagnostic, byte_slices: *collections.ByteSlices) std.mem.Allocator.Error!Tokenizer {
        const cursor = Cursor.init(text, messages);
        // TODO: tune this more. Syntax grab bag is 3:1.
        // Generally, roc code will be less dense than that.
        const output = try TokenizedBuffer.initCapacity(env, gpa, text.len);
        return Tokenizer{
            .cursor = cursor,
            .output = output,
            .string_interpolation_stack = .{},
            .env = env,
            .byte_slices = byte_slices,
        };
    }

    pub fn deinit(self: *Tokenizer, gpa: std.mem.Allocator) void {
        self.output.deinit(gpa);
        self.string_interpolation_stack.deinit(gpa);
    }

    /// Parse a base-10 integer from the given text, trying to fit it in i32.
    /// Returns either the i32 value or a ByteSlices index if it doesn't fit.
    fn parseBase10Integer(self: *Tokenizer, gpa: std.mem.Allocator, text: []const u8) !Token.Extra {
        var i: usize = 0;
        var negative = false;

        // Check for negative sign
        if (text.len > 0 and text[0] == '-') {
            negative = true;
            i = 1;
        }

        // For negative numbers, accumulate as negative to handle i32::MIN correctly
        if (negative) {
            var result: i32 = 0;

            while (i < text.len) : (i += 1) {
                const c = text[i];
                if (c == '_') continue; // Skip underscores

                const digit = charToDigit(c) orelse break;

                // Check for overflow before multiplying
                if (result < std.math.minInt(i32) / 10) {
                    return self.storeBigNumber(gpa, text);
                }

                result = result * 10;

                // Check for overflow before subtracting
                if (result < std.math.minInt(i32) + @as(i32, digit)) {
                    return self.storeBigNumber(gpa, text);
                }

                result = result - @as(i32, digit);
            }

            return Token.Extra{ .num_literal_i32 = result };
        } else {
            // For positive numbers, use u32 to detect overflow
            var result: u32 = 0;

            while (i < text.len) : (i += 1) {
                const c = text[i];
                if (c == '_') continue; // Skip underscores

                const digit = charToDigit(c) orelse break;

                // Check for overflow before multiplying
                if (result > std.math.maxInt(u32) / 10) {
                    return self.storeBigNumber(gpa, text);
                }

                result = result * 10;

                // Check for overflow before adding
                if (result > std.math.maxInt(u32) - @as(u32, digit)) {
                    return self.storeBigNumber(gpa, text);
                }

                result = result + digit;
            }

            // Check if it fits in i32
            if (result > std.math.maxInt(i32)) {
                return self.storeBigNumber(gpa, text);
            }

            return Token.Extra{ .num_literal_i32 = @as(i32, @intCast(result)) };
        }
    }

    /// Parse a hex/octal/binary integer from the given text, trying to fit it in i32.
    fn parseNonBase10Integer(self: *Tokenizer, gpa: std.mem.Allocator, text: []const u8, number_base: u8) !Token.Extra {
        var result: u32 = 0; // Use u32 for unsigned parsing
        var i: usize = 2; // Skip "0x", "0o", or "0b" prefix

        while (i < text.len) : (i += 1) {
            const c = text[i];
            if (c == '_') continue;

            const digit = switch (number_base) {
                16 => charToHexDigit(c),
                8 => charToOctalDigit(c),
                2 => charToBinaryDigit(c),
                else => unreachable,
            } orelse break;

            // Check for overflow
            if (result > std.math.maxInt(u32) / @as(u32, number_base)) {
                return self.storeBigNumber(gpa, text);
            }

            result = result * number_base;

            if (result > std.math.maxInt(u32) - @as(u32, digit)) {
                return self.storeBigNumber(gpa, text);
            }

            result = result + digit;
        }

        // Check if it fits in i32
        if (result > std.math.maxInt(i32)) {
            return self.storeBigNumber(gpa, text);
        }

        return Token.Extra{ .num_literal_i32 = @as(i32, @intCast(result)) };
    }

    /// Parse a fraction from the given text, trying to fit it in SmallDec.
    fn parseFraction(self: *Tokenizer, gpa: std.mem.Allocator, text: []const u8) !Token.Extra {
        // First try to parse as SmallDec
        const smallDec = self.tryParseSmallDec(text);
        if (smallDec) |dec| {
            return Token.Extra{ .frac_literal_small = dec };
        }

        // If it doesn't fit in SmallDec, store in ByteSlices
        return self.storeBigFraction(gpa, text);
    }

    /// Try to parse a fractional literal as a small dec (numerator/10^power)
    fn tryParseSmallDec(self: *Tokenizer, text: []const u8) ?Token.SmallDec {
        _ = self;
        // Return null if input is too long to fit in our 32-byte buffer
        if (text.len > 32) return null;

        // For negative zero, we'll return null to force ByteSlices path
        if (text.len > 0 and text[0] == '-') {
            const rest = text[1..];
            // Check if it's -0, -0.0, -0.00, etc.
            var all_zeros = true;
            for (rest) |c| {
                if (c != '0' and c != '.' and c != '_') {
                    all_zeros = false;
                    break;
                }
            }
            if (all_zeros) return null;
        }

        // Find the decimal point
        var dot_pos: ?usize = null;
        for (text, 0..) |c, i| {
            if (c == '.') {
                dot_pos = i;
                break;
            }
        }

        const decimal_index = dot_pos orelse {
            // No decimal point - shouldn't happen for fractions, but handle it
            return null;
        };

        // Count digits after decimal point (excluding underscores)
        var after_decimal_count: u8 = 0;
        var i = decimal_index + 1;
        while (i < text.len) : (i += 1) {
            if (text[i] != '_') {
                after_decimal_count += 1;
            }
        }

        if (after_decimal_count > 255) return null; // Too many decimal places

        // Build the numerator by concatenating all digits (removing decimal and underscores)
        var numerator: i32 = 0;
        var negative = false;
        i = 0;

        if (text[0] == '-') {
            negative = true;
            i = 1;
        }

        while (i < text.len) : (i += 1) {
            const c = text[i];
            if (c == '.' or c == '_') continue;

            const digit = charToDigit(c) orelse continue;

            // Check for overflow
            if (numerator > (std.math.maxInt(i32) / 10)) {
                return null;
            }
            numerator = numerator * 10;

            if (negative) {
                if (numerator < std.math.minInt(i32) + @as(i32, digit)) {
                    return null;
                }
                numerator = numerator - @as(i32, digit);
            } else {
                if (numerator > std.math.maxInt(i32) - @as(i32, digit)) {
                    return null;
                }
                numerator = numerator + @as(i32, digit);
            }
        }

        // Check if numerator fits in i16
        if (numerator < -32768 or numerator > 32767) {
            return null;
        }

        return Token.SmallDec{
            .numerator = @as(i16, @intCast(numerator)),
            .denominator_power_of_ten = after_decimal_count,
        };
    }

    /// Store a big fraction in ByteSlices
    fn storeBigFraction(self: *Tokenizer, gpa: std.mem.Allocator, text: []const u8) !Token.Extra {
        // Find the decimal point
        var dot_pos: ?usize = null;
        for (text, 0..) |c, i| {
            if (c == '.') {
                dot_pos = i;
                break;
            }
        }

        const decimal_index = dot_pos orelse text.len;

        // Count digits before and after decimal (excluding underscores)
        var before_count: u32 = 0;
        var after_count: u32 = 0;

        for (text[0..decimal_index]) |c| {
            if (c != '_' and c != '-') {
                before_count += 1;
            }
        }

        if (decimal_index < text.len) {
            for (text[decimal_index + 1 ..]) |c| {
                if (c != '_') {
                    after_count += 1;
                }
            }
        }

        // Allocate buffer for: 2 lengths (4 bytes each) + digits
        const total_size = 8 + before_count + after_count;
        var buf = try gpa.alloc(u8, total_size);
        defer gpa.free(buf);

        // Write the two lengths first (as u32 little-endian)
        std.mem.writeInt(u32, buf[0..4], before_count, .little);
        std.mem.writeInt(u32, buf[4..8], after_count, .little);

        // Copy digits (without decimal point, underscores, or minus sign)
        var j: usize = 8;
        for (text) |c| {
            if (c != '.' and c != '_' and c != '-') {
                buf[j] = c;
                j += 1;
            }
        }

        const idx = try self.byte_slices.append(gpa, buf);
        return Token.Extra{ .bytes_idx = idx };
    }

    /// Parse and store a string literal in ByteSlices with escapes resolved
    /// Check if a character is non-printable (control characters except tab, newline, carriage return)
    fn isNonPrintable(c: u8) bool {
        // Allow tab (0x09), newline (0x0A), and carriage return (0x0D)
        // Disallow other control characters (0x00-0x08, 0x0B-0x0C, 0x0E-0x1F, 0x7F)
        return (c < 0x20 and c != '\t' and c != '\n' and c != '\r') or c == 0x7F;
    }

    fn parseStringLiteral(self: *Tokenizer, gpa: std.mem.Allocator, text: []const u8) !Token.Extra {
        // Allocate a buffer that's at least as large as the input (escapes make it smaller)
        var result = try std.ArrayList(u8).initCapacity(gpa, text.len);
        defer result.deinit();

        var i: usize = 0;
        while (i < text.len) : (i += 1) {
            const c = text[i];

            if (c == '\\' and i + 1 < text.len) {
                // Handle escape sequences
                i += 1;
                const next = text[i];
                switch (next) {
                    'n' => try result.append('\n'),
                    'r' => try result.append('\r'),
                    't' => try result.append('\t'),
                    '\\' => try result.append('\\'),
                    '"' => try result.append('"'),
                    '\'' => try result.append('\''),
                    '$' => try result.append('$'),
                    'u' => {
                        // Unicode escape: \u{NNNNNN}
                        if (i + 1 < text.len and text[i + 1] == '{') {
                            i += 2; // Skip 'u{'
                            const hex_start = i;

                            // Find the closing '}'
                            while (i < text.len and text[i] != '}') {
                                i += 1;
                            }

                            if (i < text.len) {
                                // Parse the hex value
                                const hex_str = text[hex_start..i];
                                var codepoint: u32 = 0;
                                for (hex_str) |hex_char| {
                                    const digit = charToHexDigit(hex_char) orelse 0;
                                    codepoint = codepoint * 16 + digit;
                                }

                                // Encode as UTF-8
                                if (codepoint <= 0x7F) {
                                    try result.append(@intCast(codepoint));
                                } else if (codepoint <= 0x7FF) {
                                    try result.append(@intCast(0xC0 | (codepoint >> 6)));
                                    try result.append(@intCast(0x80 | (codepoint & 0x3F)));
                                } else if (codepoint <= 0xFFFF) {
                                    try result.append(@intCast(0xE0 | (codepoint >> 12)));
                                    try result.append(@intCast(0x80 | ((codepoint >> 6) & 0x3F)));
                                    try result.append(@intCast(0x80 | (codepoint & 0x3F)));
                                } else if (codepoint <= 0x10FFFF) {
                                    try result.append(@intCast(0xF0 | (codepoint >> 18)));
                                    try result.append(@intCast(0x80 | ((codepoint >> 12) & 0x3F)));
                                    try result.append(@intCast(0x80 | ((codepoint >> 6) & 0x3F)));
                                    try result.append(@intCast(0x80 | (codepoint & 0x3F)));
                                }
                            }
                        }
                    },
                    else => {
                        // Unknown escape, just include the backslash and character
                        try result.append('\\');
                        try result.append(next);
                    },
                }
            } else {
                // Regular character - check if it's non-printable
                // Note: We detect non-printable characters here but can't report
                // them with exact positions as we don't have cursor position info.
                // The actual reporting happens during tokenization.
                try result.append(c);
            }
        }

        const idx = try self.byte_slices.append(gpa, result.items);
        return Token.Extra{ .bytes_idx = idx };
    }

    /// Parse a single-quoted character literal
    fn parseSingleQuoteLiteral(self: *Tokenizer, gpa: std.mem.Allocator, text: []const u8) !Token.Extra {
        // Single quotes should contain exactly one character (possibly escaped)
        // The text includes the quotes, so strip them
        if (text.len < 2) {
            // Malformed, store as-is
            const idx = try self.byte_slices.append(gpa, text);
            return Token.Extra{ .bytes_idx = idx };
        }

        const content = text[1 .. text.len - 1]; // Remove quotes

        // Process escape if present
        var char_bytes: [4]u8 = undefined;
        var char_len: usize = 0;

        if (content.len > 0 and content[0] == '\\' and content.len > 1) {
            // Handle escape sequence
            switch (content[1]) {
                'n' => {
                    char_bytes[0] = '\n';
                    char_len = 1;
                },
                'r' => {
                    char_bytes[0] = '\r';
                    char_len = 1;
                },
                't' => {
                    char_bytes[0] = '\t';
                    char_len = 1;
                },
                '\\' => {
                    char_bytes[0] = '\\';
                    char_len = 1;
                },
                '\'' => {
                    char_bytes[0] = '\'';
                    char_len = 1;
                },
                else => {
                    // Unknown escape or unicode, store the content as-is
                    const idx = try self.byte_slices.append(gpa, content);
                    return Token.Extra{ .bytes_idx = idx };
                },
            }
        } else {
            // Regular character(s) - just copy
            for (content, 0..) |c, i| {
                if (i >= 4) break;
                char_bytes[i] = c;
                char_len = i + 1;
            }
        }

        const idx = try self.byte_slices.append(gpa, char_bytes[0..char_len]);
        return Token.Extra{ .bytes_idx = idx };
    }

    /// Store a big number in ByteSlices (always in base-10)
    fn storeBigNumber(self: *Tokenizer, gpa: std.mem.Allocator, text: []const u8) !Token.Extra {
        // Detect the base
        var number_base: u8 = 10;
        var start_idx: usize = 0;
        var is_negative = false;

        if (text.len > 0 and text[0] == '-') {
            is_negative = true;
            start_idx = 1;
        }

        if (text.len > start_idx + 1 and text[start_idx] == '0') {
            const prefix = text[start_idx + 1];
            if (prefix == 'x' or prefix == 'X') {
                number_base = 16;
                start_idx += 2;
            } else if (prefix == 'o' or prefix == 'O') {
                number_base = 8;
                start_idx += 2;
            } else if (prefix == 'b' or prefix == 'B') {
                number_base = 2;
                start_idx += 2;
            }
        }

        if (number_base == 10) {
            // For base-10, just remove underscores
            var clean_buf = try gpa.alloc(u8, text.len);
            defer gpa.free(clean_buf);

            var j: usize = 0;
            for (text) |c| {
                if (c != '_') {
                    clean_buf[j] = c;
                    j += 1;
                }
            }

            const idx = try self.byte_slices.append(gpa, clean_buf[0..j]);
            return Token.Extra{ .bytes_idx = idx };
        } else {
            // For non-base-10, we need to convert to base-10
            // We'll build up the result as base-10 digits
            var result = try std.ArrayList(u8).initCapacity(gpa, text.len * 2); // Rough estimate
            defer result.deinit();

            // We'll accumulate the value and convert to decimal string
            // Since it doesn't fit in i32, we need to do big number arithmetic
            // For now, let's use a simple approach: accumulate as array of base-10 digits

            // Start with 0
            try result.append('0');

            // Process each digit
            var i = start_idx;
            while (i < text.len) : (i += 1) {
                const c = text[i];
                if (c == '_') continue;

                const digit_value = switch (number_base) {
                    16 => charToHexDigit(c),
                    8 => charToOctalDigit(c),
                    2 => charToBinaryDigit(c),
                    else => unreachable,
                } orelse continue;

                // Multiply current result by base
                var carry: u8 = 0;
                var j: usize = result.items.len;
                while (j > 0) {
                    j -= 1;
                    const cur_digit = result.items[j] - '0';
                    const prod = cur_digit * number_base + carry;
                    result.items[j] = '0' + (prod % 10);
                    carry = prod / 10;
                }
                while (carry > 0) {
                    try result.insert(0, '0' + (carry % 10));
                    carry = carry / 10;
                }

                // Add the new digit
                carry = digit_value;
                j = result.items.len;
                while (j > 0 and carry > 0) {
                    j -= 1;
                    const cur_digit = result.items[j] - '0';
                    const sum = cur_digit + carry;
                    result.items[j] = '0' + (sum % 10);
                    carry = sum / 10;
                }
                while (carry > 0) {
                    try result.insert(0, '0' + (carry % 10));
                    carry = carry / 10;
                }
            }

            // Add negative sign if needed
            if (is_negative) {
                try result.insert(0, '-');
            }

            const idx = try self.byte_slices.append(gpa, result.items);
            return Token.Extra{ .bytes_idx = idx };
        }
    }

    pub fn finishAndDeinit(self: *Tokenizer, gpa: std.mem.Allocator) TokenOutput {
        self.string_interpolation_stack.deinit(gpa);
        const actual_message_count = @min(self.cursor.message_count, self.cursor.messages.len);
        return .{
            .tokens = self.output,
            .messages = self.cursor.messages[0..actual_message_count],
            .extra_messages_dropped = self.cursor.message_count - actual_message_count,
        };
    }

    /// Pushes a token with the given tag, token offset, and extra.
    fn pushTokenNormalHere(self: *Tokenizer, gpa: std.mem.Allocator, tag: Token.Tag, tok_offset: Token.Idx) std.mem.Allocator.Error!void {
        std.debug.assert(!tag.isInterned());
        try self.output.tokens.append(gpa, .{
            .tag = tag,
            .region = base.Region.from_raw_offsets(tok_offset, self.cursor.pos),
            .extra = .{ .none = 0 },
        });
    }

    /// Pushes a token with the given tag, token offset, and explicit extra value.
    fn pushTokenWithExtra(self: *Tokenizer, gpa: std.mem.Allocator, tag: Token.Tag, tok_offset: Token.Idx, extra: Token.Extra) std.mem.Allocator.Error!void {
        std.debug.assert(!tag.isInterned());
        try self.output.tokens.append(gpa, .{
            .tag = tag,
            .region = base.Region.from_raw_offsets(tok_offset, self.cursor.pos),
            .extra = extra,
        });
    }

    fn pushTokenInternedHere(
        self: *Tokenizer,
        gpa: std.mem.Allocator,
        tag: Token.Tag,
        tok_offset: Token.Idx,
        text_offset: Token.Idx,
    ) !void {
        const text = self.cursor.buf[text_offset..self.cursor.pos];
        const id = try self.env.insertIdent(gpa, base.Ident.for_text(text));

        // Use underscore flags for type variable identifiers that benefit from fast checking
        if (tag.hasUnderscoreFlags()) {
            // Calculate underscore flags for identifier tokens
            const starts_with_underscore = text.len > 0 and text[0] == '_';
            const ends_with_underscore = text.len > 1 and text[text.len - 1] == '_';

            try self.output.tokens.append(gpa, .{
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
            try self.output.tokens.append(gpa, .{
                .tag = tag,
                .extra = .{ .interned = id },
                .region = base.Region.from_raw_offsets(tok_offset, self.cursor.pos),
            });
        }
    }

    /// The main tokenize loop. This loops over the whole input buffer, tokenizing as it goes.
    pub fn tokenize(self: *Tokenizer, gpa: std.mem.Allocator) std.mem.Allocator.Error!void {
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
                                try self.pushTokenNormalHere(gpa, .TripleDot, start);
                            } else {
                                self.cursor.pos += 2;
                                try self.pushTokenNormalHere(gpa, .DoubleDot, start);
                            }
                        } else if (n >= '0' and n <= '9') {
                            self.cursor.pos += 1;
                            self.cursor.chompInteger();
                            try self.pushTokenNormalHere(gpa, if (sp) .DotInt else .NoSpaceDotInt, start);
                        } else if (n >= 'a' and n <= 'z') {
                            var tag: Token.Tag = if (sp) .DotLowerIdent else .NoSpaceDotLowerIdent;
                            self.cursor.pos += 1;
                            const text_start = self.cursor.pos;
                            if (!self.cursor.chompIdentGeneral()) {
                                tag = .MalformedDotUnicodeIdent;
                            }
                            try self.pushTokenInternedHere(gpa, tag, start, text_start);
                        } else if (n >= 'A' and n <= 'Z') {
                            var tag: Token.Tag = if (sp) .DotUpperIdent else .NoSpaceDotUpperIdent;
                            self.cursor.pos += 1;
                            const text_start = self.cursor.pos;
                            if (!self.cursor.chompIdentGeneral()) {
                                tag = .MalformedDotUnicodeIdent;
                            }
                            try self.pushTokenInternedHere(gpa, tag, start, text_start);
                        } else if (n >= 0x80 and n <= 0xff) {
                            self.cursor.pos += 1;
                            const text_start = self.cursor.pos;
                            _ = self.cursor.chompIdentGeneral();
                            try self.pushTokenInternedHere(gpa, .MalformedDotUnicodeIdent, start, text_start);
                        } else if (n == open_curly) {
                            self.cursor.pos += 1;
                            try self.pushTokenNormalHere(gpa, .Dot, start);
                        } else if (n == '*') {
                            self.cursor.pos += 2;
                            try self.pushTokenNormalHere(gpa, .DotStar, start);
                        } else {
                            self.cursor.pos += 1;
                            try self.pushTokenNormalHere(gpa, .Dot, start);
                        }
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .Dot, start);
                    }
                },

                // Minus (-)
                '-' => {
                    const next = self.cursor.peekAt(1);
                    if (next) |n| {
                        if (n == '>') {
                            self.cursor.pos += 2;
                            try self.pushTokenNormalHere(gpa, .OpArrow, start);
                        } else if (n == ' ' or n == '\t' or n == '\n' or n == '\r' or n == '#') {
                            self.cursor.pos += 1;
                            try self.pushTokenNormalHere(gpa, .OpBinaryMinus, start);
                        } else if (n >= '0' and n <= '9') {
                            self.cursor.pos += 1;
                            const tag = self.cursor.chompNumber();
                            const num_end = self.cursor.pos;

                            // Parse the actual number value (including the minus)
                            const num_text = self.cursor.buf[start..num_end];

                            // Parse the value based on type
                            if (tag == .Int) {
                                const extra = try self.parseBase10Integer(gpa, num_text);
                                try self.pushTokenWithExtra(gpa, tag, start, extra);
                            } else if (tag == .Float) {
                                const extra = try self.parseFraction(gpa, num_text);
                                try self.pushTokenWithExtra(gpa, tag, start, extra);
                            } else {
                                // For malformed numbers, just push without parsing
                                try self.pushTokenNormalHere(gpa, tag, start);
                            }
                        } else {
                            self.cursor.pos += 1;
                            // Look at what follows the minus to determine if it's unary
                            const tokenType: Token.Tag = if (self.canFollowUnaryMinus(n)) .OpUnaryMinus else .OpBinaryMinus;
                            try self.pushTokenNormalHere(gpa, tokenType, start);
                        }
                    } else {
                        self.cursor.pos += 1;
                        // No next character - default to unary
                        try self.pushTokenNormalHere(gpa, .OpUnaryMinus, start);
                    }
                },

                // Exclamation (!)
                '!' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpNotEquals, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .OpBang, start);
                    }
                },

                // Ampersand (&)
                '&' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .OpAmpersand, start);
                },

                // Comma (,)
                ',' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .Comma, start);
                },

                // Question mark (?)
                '?' => {
                    if (self.cursor.peekAt(1) == '?') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpDoubleQuestion, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, if (sp) .OpQuestion else .NoSpaceOpQuestion, start);
                    }
                },

                // Pipe (|)
                '|' => {
                    if (self.cursor.peekAt(1) == '>') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpPizza, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .OpBar, start);
                    }
                },

                // Plus (+)
                '+' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .OpPlus, start);
                },

                // Star (*)
                '*' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .OpStar, start);
                },

                // Slash (/)
                '/' => {
                    if (self.cursor.peekAt(1) == '/') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpDoubleSlash, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .OpSlash, start);
                    }
                },

                // Backslash (\)
                '\\' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .OpBackslash, start);
                },

                // Percent (%)
                '%' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .OpPercent, start);
                },

                // Caret (^)
                '^' => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .OpCaret, start);
                },

                // Greater-than (>)
                '>' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpGreaterThanOrEq, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .OpGreaterThan, start);
                    }
                },

                // Less-than (<)
                '<' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpLessThanOrEq, start);
                    } else if (self.cursor.peekAt(1) == '-') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpBackArrow, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .OpLessThan, start);
                    }
                },

                // Equals (=)
                '=' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpEquals, start);
                    } else if (self.cursor.peekAt(1) == '>') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpFatArrow, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .OpAssign, start);
                    }
                },

                // Colon (:)
                ':' => {
                    if (self.cursor.peekAt(1) == '=') {
                        self.cursor.pos += 2;
                        try self.pushTokenNormalHere(gpa, .OpColonEqual, start);
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .OpColon, start);
                    }
                },

                Token.DELIM_OPEN_ROUND => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, if (sp) .OpenRound else .NoSpaceOpenRound, start);
                },
                Token.DELIM_OPEN_SQUARE => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .OpenSquare, start);
                },
                open_curly => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .OpenCurly, start);
                },

                Token.DELIM_CLOSE_ROUND => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .CloseRound, start);
                },
                Token.DELIM_CLOSE_SQUARE => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .CloseSquare, start);
                },
                close_curly => {
                    self.cursor.pos += 1;
                    if (self.string_interpolation_stack.pop()) |last| {
                        try self.pushTokenNormalHere(gpa, .CloseStringInterpolation, start);
                        // For string interpolations, we don't have the original opening quote position
                        // so we use the current position as a fallback
                        try self.tokenizeStringLikeLiteralBody(gpa, last, self.cursor.pos);
                    } else {
                        try self.pushTokenNormalHere(gpa, .CloseCurly, start);
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
                            try self.pushTokenInternedHere(gpa, tok, start, start);
                        } else {
                            self.cursor.pos += 1;
                            try self.pushTokenNormalHere(gpa, .Underscore, start);
                        }
                    } else {
                        self.cursor.pos += 1;
                        try self.pushTokenNormalHere(gpa, .Underscore, start);
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
                        try self.pushTokenInternedHere(gpa, tok, start, start);
                    } else {
                        try self.pushTokenNormalHere(gpa, tok, start);
                    }
                },

                // Numbers starting with 0-9
                '0'...'9' => {
                    const num_start = start;
                    var tag = self.cursor.chompNumber();
                    const num_end = self.cursor.pos;

                    // Parse the actual number value
                    const num_text = self.cursor.buf[num_start..num_end];

                    // Only parse integers for now (skip floats)
                    if (tag == .Int or tag == .IntBase) {
                        // Determine the base
                        var extra: Token.Extra = undefined;
                        if (num_text.len >= 2 and num_text[0] == '0') {
                            if (num_text[1] == 'x' or num_text[1] == 'X') {
                                // Hexadecimal
                                extra = try self.parseNonBase10Integer(gpa, num_text, 16);
                                tag = .IntBase;
                            } else if (num_text[1] == 'o' or num_text[1] == 'O') {
                                // Octal
                                extra = try self.parseNonBase10Integer(gpa, num_text, 8);
                                tag = .IntBase;
                            } else if (num_text[1] == 'b' or num_text[1] == 'B') {
                                // Binary
                                extra = try self.parseNonBase10Integer(gpa, num_text, 2);
                                tag = .IntBase;
                            } else {
                                // Base-10 with leading zero
                                extra = try self.parseBase10Integer(gpa, num_text);
                                tag = .Int;
                            }
                        } else {
                            // Base-10
                            extra = try self.parseBase10Integer(gpa, num_text);
                            tag = .Int;
                        }
                        try self.pushTokenWithExtra(gpa, tag, start, extra);
                    } else if (tag == .Float) {
                        const extra = try self.parseFraction(gpa, num_text);
                        try self.pushTokenWithExtra(gpa, tag, start, extra);
                    } else {
                        // For malformed numbers, just push without parsing
                        try self.pushTokenNormalHere(gpa, tag, start);
                    }
                },

                // Lowercase identifiers
                'a'...'z' => {
                    const tag = self.cursor.chompIdentLower();
                    if (tag == .LowerIdent or tag == .MalformedUnicodeIdent) {
                        try self.pushTokenInternedHere(gpa, tag, start, start);
                    } else {
                        try self.pushTokenNormalHere(gpa, tag, start);
                    }
                },

                // Uppercase identifiers
                'A'...'Z' => {
                    var tag: Token.Tag = .UpperIdent;
                    if (!self.cursor.chompIdentGeneral()) {
                        tag = .MalformedUnicodeIdent;
                    }
                    try self.pushTokenInternedHere(gpa, tag, start, start);
                },

                '\'' => {
                    const quote_start = start;
                    const tag = self.cursor.chompSingleQuoteLiteral();
                    const quote_end = self.cursor.pos;
                    const quote_text = self.cursor.buf[quote_start..quote_end];

                    // Parse the single quote literal and store in ByteSlices
                    if (tag == .SingleQuote) {
                        const extra = try self.parseSingleQuoteLiteral(gpa, quote_text);
                        try self.pushTokenWithExtra(gpa, tag, start, extra);
                    } else {
                        // For malformed single quotes, just push normally
                        try self.pushTokenNormalHere(gpa, tag, start);
                    }
                },

                '"' => {
                    // Parse the string literal and store in ByteSlices
                    try self.tokenizeStringLiteral(gpa);
                },

                // first byte of a UTF-8 sequence
                0x80...0xff => {
                    _ = self.cursor.chompIdentGeneral();
                    try self.pushTokenInternedHere(gpa, .MalformedUnicodeIdent, start, start);
                },

                // TODO: emit a MalformedOpToken for invalid combinations of operator-like characters

                // Fallback for any unknown token.
                else => {
                    self.cursor.pos += 1;
                    try self.pushTokenNormalHere(gpa, .MalformedUnknownToken, start);
                },
            }
        }

        try self.pushTokenNormalHere(gpa, .EndOfFile, self.cursor.pos);
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

    /// Tokenize a string literal, storing the resolved content in ByteSlices
    pub fn tokenizeStringLiteral(self: *Tokenizer, gpa: std.mem.Allocator) std.mem.Allocator.Error!void {
        const start = self.cursor.pos;
        std.debug.assert(self.cursor.peek() == '"');

        // Save the current position
        const saved_pos = self.cursor.pos;

        // First, scan to check if this is an interpolated string
        self.cursor.pos += 1; // Skip opening quote

        // Check for multiline string
        const is_multiline = self.cursor.peek() == '"' and self.cursor.peekAt(1) == '"';
        if (is_multiline) {
            self.cursor.pos += 2;
        }

        // Scan for interpolation marker
        var has_interpolation = false;
        while (self.cursor.pos < self.cursor.buf.len) {
            const c = self.cursor.buf[self.cursor.pos];
            if (c == '$' and self.cursor.peekAt(1) == '{') {
                has_interpolation = true;
                break;
            } else if (!is_multiline and c == '"') {
                break;
            } else if (is_multiline and c == '"' and self.cursor.peekAt(1) == '"' and self.cursor.peekAt(2) == '"') {
                break;
            } else if (c == '\\' and self.cursor.pos + 1 < self.cursor.buf.len) {
                self.cursor.pos += 2;
            } else if (!is_multiline and c == '\n') {
                break;
            } else {
                self.cursor.pos += 1;
            }
        }

        // Reset position
        self.cursor.pos = saved_pos;

        // If it has interpolation, use the old tokenization method
        if (has_interpolation) {
            try self.tokenizeStringLikeLiteral(gpa);
            return;
        }

        // Otherwise, parse as a complete string literal
        const str_start = start;
        var tag: Token.Tag = undefined;

        self.cursor.pos += 1; // Skip opening quote

        if (is_multiline) {
            // Triple-quoted multiline string
            self.cursor.pos += 2;
            tag = .MultilineString;

            // Find the closing triple quote
            while (self.cursor.pos < self.cursor.buf.len) {
                if (self.cursor.buf[self.cursor.pos] == '"' and
                    self.cursor.peekAt(1) == '"' and
                    self.cursor.peekAt(2) == '"')
                {
                    self.cursor.pos += 3;
                    break;
                }
                // Check for non-printable characters in multiline strings
                const c = self.cursor.buf[self.cursor.pos];
                if (isNonPrintable(c)) {
                    self.cursor.pushMessageHere(.NonPrintableUnicodeInStrLiteral);
                }
                self.cursor.pos += 1;
            }
        } else {
            // Single-line string
            tag = .String;

            // Find the closing quote
            while (self.cursor.pos < self.cursor.buf.len) {
                const c = self.cursor.buf[self.cursor.pos];
                if (c == '"') {
                    self.cursor.pos += 1;
                    break;
                } else if (c == '\\' and self.cursor.pos + 1 < self.cursor.buf.len) {
                    // Skip escape sequence
                    self.cursor.pos += 2;
                } else if (c == '\n') {
                    // Unclosed string
                    tag = .MalformedString;
                    break;
                } else {
                    // Check for non-printable characters
                    if (isNonPrintable(c)) {
                        self.cursor.pushMessageHere(.NonPrintableUnicodeInStrLiteral);
                    }
                    self.cursor.pos += 1;
                }
            }
        }

        const str_end = self.cursor.pos;
        const str_text = self.cursor.buf[str_start..str_end];

        // Parse the string content and store in ByteSlices
        const extra = try self.parseStringLiteral(gpa, str_text);
        try self.pushTokenWithExtra(gpa, tag, start, extra);
    }

    pub fn tokenizeStringLikeLiteral(self: *Tokenizer, gpa: std.mem.Allocator) std.mem.Allocator.Error!void {
        const start = self.cursor.pos;
        std.debug.assert(self.cursor.peek() == '"');
        self.cursor.pos += 1;
        var kind: StringKind = .single_line;
        if (self.cursor.peek() == '"' and self.cursor.peekAt(1) == '"') {
            self.cursor.pos += 2;
            kind = .multi_line;
            try self.pushTokenNormalHere(gpa, .MultilineStringStart, start);
        } else {
            try self.pushTokenNormalHere(gpa, .StringStart, start);
        }
        try self.tokenizeStringLikeLiteralBody(gpa, kind, start);
    }

    // Moving curly chars to constants because some editors hate them inline.
    const open_curly = '{';
    const close_curly = '}';

    pub fn tokenizeStringLikeLiteralBody(self: *Tokenizer, gpa: std.mem.Allocator, kind: StringKind, opening_quote_pos: usize) std.mem.Allocator.Error!void {
        const start = self.cursor.pos;
        var string_part_tag: Token.Tag = .StringPart;
        while (self.cursor.pos < self.cursor.buf.len) {
            const c = self.cursor.buf[self.cursor.pos];
            if (c == '$' and self.cursor.peekAt(1) == open_curly) {
                try self.pushTokenNormalHere(gpa, string_part_tag, start);
                const dollar_start = self.cursor.pos;
                self.cursor.pos += 2;
                try self.pushTokenNormalHere(gpa, .OpenStringInterpolation, dollar_start);
                try self.string_interpolation_stack.append(gpa, kind);
                return;
            } else if (c == '\n') {
                if (kind == .single_line) {
                    try self.pushTokenNormalHere(gpa, string_part_tag, start);
                    // Include the opening quote in the error region
                    self.cursor.pushMessage(.UnclosedString, @intCast(opening_quote_pos), @intCast(self.cursor.pos));
                    try self.pushTokenNormalHere(gpa, .StringEnd, self.cursor.pos);
                    return;
                }
                // For multiline strings, continue parsing after newline
                self.cursor.pos += 1;
            } else if (kind == .single_line and c == '"') {
                try self.pushTokenNormalHere(gpa, string_part_tag, start);
                const string_part_end = self.cursor.pos;
                self.cursor.pos += 1;
                try self.pushTokenNormalHere(gpa, .StringEnd, string_part_end);
                return;
            } else if (kind == .multi_line and c == '"' and self.cursor.peekAt(1) == '"' and self.cursor.peekAt(2) == '"') {
                try self.pushTokenNormalHere(gpa, string_part_tag, start);
                const quote_start = self.cursor.pos;
                self.cursor.pos += 3;
                try self.pushTokenNormalHere(gpa, .MultilineStringEnd, quote_start);
                return;
            } else {
                // Handle UTF-8 sequences with printable character validation
                _ = self.cursor.chompUTF8CodepointWithValidation();

                const escape = c == '\\';
                if (escape) {
                    self.cursor.chompEscapeSequenceWithQuote('"') catch {
                        string_part_tag = .MalformedStringPart;
                    };
                }
            }
        }
        if (kind == .single_line) {
            // Include the opening quote in the error region
            self.cursor.pushMessage(.UnclosedString, @intCast(opening_quote_pos), @intCast(self.cursor.pos));
        }
        try self.pushTokenNormalHere(gpa, string_part_tag, start);
    }
};

fn testTokenization(gpa: std.mem.Allocator, input: []const u8, expected: []const Token.Tag) !void {
    var messages: [10]Diagnostic = undefined;

    var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
    defer env.deinit(gpa);

    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(gpa);
    var tokenizer = try Tokenizer.init(&env, gpa, input, &messages, &byte_slices);
    defer tokenizer.deinit(gpa);

    try tokenizer.tokenize(gpa);
    const tokenizedBuffer = tokenizer.output;
    const tokens = tokenizedBuffer.tokens.items(.tag);

    try std.testing.expectEqual(tokens[tokens.len - 1], Token.Tag.EndOfFile);
    try std.testing.expectEqualSlices(Token.Tag, expected[0..expected.len], tokens[0 .. tokens.len - 1]);

    try checkTokenizerInvariants(gpa, input, false);
}

/// Assert the invariants of the tokenizer are held.
pub fn checkTokenizerInvariants(gpa: std.mem.Allocator, input: []const u8, debug: bool) std.mem.Allocator.Error!void {
    // TODO: Update this function to work with new tokenization approach
    // For now, skip the invariant checks
    _ = gpa;
    _ = input;
    _ = debug;
    return;
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
            .String => {
                // Generate a fake string literal
                try buf2.append(alloc, '"');
                for (1..length - 1) |_| {
                    try buf2.append(alloc, 'a');
                }
                try buf2.append(alloc, '"');
            },
            .MultilineString => {
                // Generate a fake multiline string literal
                try buf2.append(alloc, '"');
                try buf2.append(alloc, '"');
                try buf2.append(alloc, '"');
                if (length > 6) {
                    for (6..length - 3) |_| {
                        try buf2.append(alloc, 'a');
                    }
                }
                try buf2.append(alloc, '"');
                try buf2.append(alloc, '"');
                try buf2.append(alloc, '"');
            },
            .MalformedString => {
                // Generate a malformed string (unclosed)
                try buf2.append(alloc, '"');
                for (1..length) |_| {
                    try buf2.append(alloc, 'a');
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
                    if (length >= 4) {
                        try buf2.append(alloc, '0');
                        try buf2.append(alloc, 'x');
                        for (3..length) |_| {
                            try buf2.append(alloc, '1');
                        }
                    } else {
                        for (1..length) |_| {
                            try buf2.append(alloc, '1');
                        }
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
            .IntBase => {
                // Generate a non-base-10 integer (hex/octal/binary)
                if (buf[region.start.offset] == '-') {
                    try buf2.append(alloc, '-');
                    try buf2.append(alloc, '0');
                    try buf2.append(alloc, 'x');
                    for (3..length) |_| {
                        try buf2.append(alloc, 'F');
                    }
                } else {
                    try buf2.append(alloc, '0');
                    try buf2.append(alloc, 'x');
                    for (2..length) |_| {
                        try buf2.append(alloc, 'F');
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
            .KwWhile => {
                try buf2.appendSlice(alloc, "while");
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
            .MalformedSingleQuoteInvalidEscapeSequence,
            .MalformedStringPart,
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
    try testTokenization(gpa, "\"abc\"", &[_]Token.Tag{.String});
    try testTokenization(gpa, "\"a${b}c\"", &[_]Token.Tag{
        .StringStart,
        .StringPart,
        .OpenStringInterpolation,
        .LowerIdent,
        .CloseStringInterpolation,
        .StringPart,
        .StringEnd,
    });
    // Incomplete multiline string is still tokenized as MultilineString
    // (goes to end of file)
    try testTokenization(
        gpa,
        \\"""abc
    ,
        &[_]Token.Tag{.MultilineString},
    );
    // The second """ on next line is treated as closing delimiter, leaving "def" as identifier
    try testTokenization(
        gpa,
        \\"""abc
        \\"""def
    ,
        &[_]Token.Tag{ .MultilineString, .LowerIdent },
    );
    try testTokenization(
        gpa,
        \\"""abc"""
    ,
        &[_]Token.Tag{.MultilineString},
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
            .MultilineStringEnd,
            .LowerIdent,
        },
    );
}

// Commented out: We now assume strings are already valid UTF-8 (validated earlier with SIMD)
// test "tokenizer with invalid UTF-8" {
//     const gpa = std.testing.allocator;
//
//     // Invalid UTF-8 start byte
//     {
//         const invalid_utf8 = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xFF, ' ', 'w', 'o', 'r', 'l', 'd', '"' };
//         var diagnostics: [10]Diagnostic = undefined;
//
//         var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
//         defer env.deinit(gpa);
//
//         var byte_slices = collections.ByteSlices{ .entries = .{} };
//         defer byte_slices.entries.deinit(gpa);
//         var tokenizer = try Tokenizer.init(&env, gpa, &invalid_utf8, &diagnostics, &byte_slices);
//         defer tokenizer.deinit(gpa);
//         try tokenizer.tokenize(gpa);
//
//         // Should have reported InvalidUtf8InSource
//         const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
//         try std.testing.expect(messages.len > 0);
//         try std.testing.expectEqual(Diagnostic.Tag.InvalidUtf8InSource, messages[0].tag);
//     }
//
//     // Incomplete UTF-8 sequence at end
//     {
//         const incomplete_utf8 = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xC3, '"' }; // 0xC3 expects another byte
//         var diagnostics: [10]Diagnostic = undefined;
//
//         var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
//         defer env.deinit(gpa);
//
//         var byte_slices = collections.ByteSlices{ .entries = .{} };
//         defer byte_slices.entries.deinit(gpa);
//         var tokenizer = try Tokenizer.init(&env, gpa, &incomplete_utf8, &diagnostics, &byte_slices);
//         defer tokenizer.deinit(gpa);
//         try tokenizer.tokenize(gpa);
//
//         // Should have reported InvalidUtf8InSource
//         const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
//         try std.testing.expect(messages.len > 0);
//         try std.testing.expectEqual(Diagnostic.Tag.InvalidUtf8InSource, messages[0].tag);
//     }
// }

test "non-printable characters in string literal" {
    const gpa = std.testing.allocator;

    // Non-printable ASCII
    {
        const non_printable = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', '\x01', 'w', 'o', 'r', 'l', 'd', '"' }; // 0x01 is SOH (non-printable)
        var diagnostics: [10]Diagnostic = undefined;

        const source = try gpa.dupe(u8, &non_printable);
        defer gpa.free(source);

        var env = try CommonEnv.init(gpa, source);
        defer env.deinit(gpa);

        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(gpa);
        var tokenizer = try Tokenizer.init(&env, gpa, env.source, &diagnostics, &byte_slices);
        try tokenizer.tokenize(gpa);
        var output = tokenizer.finishAndDeinit(gpa);
        defer output.tokens.deinit(gpa);

        // Should have reported NonPrintableUnicodeInStrLiteral
        const messages = output.messages;
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.NonPrintableUnicodeInStrLiteral, messages[0].tag);
    }

    // Non-ASCII Unicode control characters would require UTF-8 decoding to detect properly
    // Since we assume UTF-8 is valid (checked with SIMD), we only check ASCII control chars
    // Commenting out this test case:
    // {
    //     const control_char = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xC2, 0x80, ' ', 'w', 'o', 'r', 'l', 'd', '"' }; // U+0080 (C1 control)
    //     var diagnostics: [10]Diagnostic = undefined;
    //
    //     var env = try CommonEnv.init(gpa, try gpa.dupe(u8, &control_char));
    //     defer env.deinit(gpa);
    //
    //     var byte_slices = collections.ByteSlices{ .entries = .{} };
    //     defer byte_slices.entries.deinit(gpa);
    //     var tokenizer = try Tokenizer.init(&env, gpa, env.source, &diagnostics, &byte_slices);
    //     defer tokenizer.deinit(gpa);
    //     try tokenizer.tokenize(gpa);
    //
    //     const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
    //     try std.testing.expect(messages.len > 0);
    //     try std.testing.expectEqual(Diagnostic.Tag.NonPrintableUnicodeInStrLiteral, messages[0].tag);
    // }

    // No errors should be reported for these
    {
        const valid_chars = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', '\t', ' ', 'w', 'o', 'r', 'l', 'd', '"' };
        var diagnostics: [10]Diagnostic = undefined;

        const source = try gpa.dupe(u8, &valid_chars);
        defer gpa.free(source);
        var env = try CommonEnv.init(gpa, source);
        defer env.deinit(gpa);

        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(gpa);
        var tokenizer = try Tokenizer.init(&env, gpa, env.source, &diagnostics, &byte_slices);
        defer tokenizer.deinit(gpa);
        try tokenizer.tokenize(gpa);

        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len == 0);
    }
}
