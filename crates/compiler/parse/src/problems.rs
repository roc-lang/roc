use roc_region::all::Loc;

pub type Problems = Vec<Loc<Problem>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Problem {
    // UNICODE CODE POINT
    /// TODO Invalid hex code - Unicode code points must be specified using hexadecimal characters (the numbers 0-9 and letters A-F)
    NonHexCharsInUnicodeCodePt,
    /// TODO Invalid Unicode code point. It must be no more than \\u{10FFFF}.
    UnicodeCodePtTooLarge,
    InvalidUnicodeCodePt,
    MalformedEscapedUnicode,
    NoUnicodeDigits,

    // STRING LITERAL
    NewlineInLiteral,
    Tab,
    CarriageReturn,
    NullChar,
    UnsupportedEscapedChar,

    // NUMBER LITERAL
    OutsideSupportedRange,
}
