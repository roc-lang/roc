//! representation of a token in the source code, like '+', 'foo', '=', '{'
//! these are represented by an offset into the bytes of the source code
//! and an extra field that stores either the length of the token or
//! an index into the string interner

const std = @import("std");
const base = @import("../../../base.zig");
const collections = @import("../../../collections.zig");

tag: Tag,
offset: u32,
extra: Extra,

/// Additional data associated with a token, either a length or an interned identifier.
pub const Extra = union {
    length: u32,
    interned: base.Ident.Idx,
};

/// A multi-array list type for efficiently storing tokens.
pub const List = std.MultiArrayList(@This());
/// Index type for referencing tokens.
pub const Idx = u32;
/// Represents a span of tokens in the source.
pub const Span = struct { span: base.DataSpan };

/// Token type enumeration identifying different kinds of tokens.
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
    pub fn updateIfNotMalformed(tok: Tag, next: Tag) Tag {
        if (tok.isMalformed()) {
            return tok;
        }
        return next;
    }
};

/// Static map of keyword strings to their corresponding token tags.
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

/// Static map of valid numeric type suffixes (e.g., "i32", "f64").
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

/// Represents a comment in roc source e.g. `## some comment`
pub const Comment = struct {
    begin: u32,
    end: u32,
};

/// Represents a unicode character parse from the source.
pub const Unicode = struct {
    tag: enum {
        LetterUpper,
        LetterNotUpper,
        Digit,
        Other,
        Invalid,
    },
    length: u32,
};

/// Different kinds of braces and brackets used in the language.
pub const BraceKind = union(enum) {
    round,
    square,
    curly,
    string_interpolation: StringKind,

    const List = collections.SafeList(@This());
};

/// Different kinds of string literals supported by the language.
pub const StringKind = enum {
    single_line,
    multi_line,
};
