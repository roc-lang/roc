//! Representation of a token in the source code, like '+', 'foo', '=', '{', etc.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const tracy = @import("tracy");

const DataSpan = base.DataSpan;
const SmallDec = base.SmallDec;
const CommonEnv = base.CommonEnv; // TODO remove this dependency. Just passs
const Ident = base.Ident;
const Region = base.Region;
const ByteSlices = collections.ByteSlices;

pub const Self = @This();

tag: Tag,
payload: Payload,
region: base.Region,

pub const Payload = union {
    interned: base.Ident.Idx,
    ident_with_flags: IdentWithFlags,
    num_literal_i32: i32, // For numbers that fit in i32
    frac_literal_small: SmallDec, // For fractions that fit in SmallDec
    bytes_idx: ByteSlices.Idx, // For big numbers and strings
    none: void,
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
    // Comments and whitespace
    LineComment, // # comment text
    DocComment, // ## doc comment text
    BlankLine, // A line with only whitespace (spaces/tabs)

    // primitives
    Float,
    String, // a complete string literal (stored in ByteSlices)
    MultilineString, // a complete multiline string literal (stored in ByteSlices)
    MalformedString, // malformed string literal
    StringStart, // the " that starts a string (for interpolation)
    StringEnd, // the " that ends a string (for interpolation)
    MultilineStringStart, // the """ that starts a multiline string (for interpolation)
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

    NominalName,
    MalformedNominalNameUnicode,
    MalformedNominalNameWithoutName,

    OpenRound,
    CloseRound,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    OpenStringInterpolation,
    CloseStringInterpolation,

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
    OpThinArrow,
    OpThickArrow,
    OpBackslash,
    OpBackpass,
    MalformedOperator,

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

    EndOfFile,

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
            .LineComment,
            .DocComment,
            .Float,
            .String,
            .MultilineString,
            .StringStart,
            .StringEnd,
            .MultilineStringStart,
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
            .NominalName,
            .OpenRound,
            .CloseRound,
            .OpenSquare,
            .CloseSquare,
            .OpenCurly,
            .CloseCurly,
            .OpenStringInterpolation,
            .CloseStringInterpolation,
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
            .OpThickArrow,
            .OpThinArrow,
            .OpBackslash,
            .OpBackpass,
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
            .BlankLine,
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
            .MalformedNominalNameUnicode,
            .MalformedNominalNameWithoutName,
            .MalformedUnicodeIdent,
            .MalformedUnknownToken,
            .MalformedSingleQuoteUnclosed,
            .MalformedSingleQuoteEmpty,
            .MalformedSingleQuoteTooLong,
            .MalformedSingleQuoteInvalidEscapeSequence,
            .MalformedString,
            .MalformedStringPart,
            .MalformedOperator,
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
            .MalformedNominalNameUnicode,
            .NominalName,
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

pub fn no_payload(tag: Self.Tag, start: u32, end: u32) Self {
    return Self{
        .tag = tag,
        .region = Region.from_raw_offsets(start, end),
        .payload = .{ .none = {} },
    };
}
