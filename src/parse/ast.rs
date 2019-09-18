use bumpalo::collections::vec::Vec;
use operator::Operator;
use region::Loc;
use std::fmt::{self, Display, Formatter};

pub type VariantName = str;

/// A parsed expression. This uses lifetimes extensively for two reasons:
///
/// 1. It uses Bump::alloc for all allocations, which returns a reference.
/// 2. It often stores references into the input string instead of allocating.
///
/// This dramatically reduces allocations during parsing. Once parsing is done,
/// we move on to canonicalization, which often needs to allocate more because
/// it's doing things like turning local variables into fully qualified symbols.
/// Once canonicalization is done, the arena and the input string get dropped.
///
/// Because we need to store references, which each take 2 machine words, the
/// smallest this data structure can be in memory is 3 machine words (the third
/// machine word stores the 1-byte union tag in a memory-aligned way). We have
/// a test verifying that it never accidentally exceeds 3 machine words in size.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    // Number Literals
    Float(&'a str),
    Int(&'a str),
    HexInt(&'a str),
    OctalInt(&'a str),
    BinaryInt(&'a str),

    // String Literals
    EmptyStr,
    Str(&'a str),
    BlockStr(&'a [&'a str]),

    // List literals
    EmptyList,
    List(Vec<'a, Loc<Expr<'a>>>),
    // Lookups
    Var(&'a [&'a str], &'a str),
    Variant(&'a [&'a str], &'a str),

    // // Pattern Matching
    When(&'a [(Loc<Pattern<'a>>, Loc<Expr<'a>>)]),
    // Closure(&'a (&'a [Loc<Pattern<'a>>], Loc<Expr<'a>>)),
    // /// basically Assign(Vec<(Loc<Pattern>, Loc<Expr>)>, Loc<Expr>)
    // Assign(&'a (&'a [(Loc<Pattern<'a>>, Loc<Expr<'a>>)], Loc<Expr<'a>>)),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a variant by name, do Apply(Variant(...), ...)
    Apply(&'a (Loc<Expr<'a>>, &'a [Loc<Expr<'a>>])),
    Operator(&'a (Loc<Expr<'a>>, Loc<Operator>, Loc<Expr<'a>>)),

    // Product Types
    EmptyRecord,
    /// e.g. `(expr).foo.bar`
    Field(&'a Expr<'a>, &'a [&'a str]),
    /// e.g. `Foo.Bar.baz.qux`
    QualifiedField(&'a [&'a str], &'a [&'a str]),
    /// e.g. `.foo`
    AccessorFunction(&'a str),

    // Conditionals
    If(&'a Loc<Expr<'a>>),
    Then(&'a Loc<Expr<'a>>),
    Else(&'a Loc<Expr<'a>>),
    Case(&'a Loc<Expr<'a>>),

    // Problems
    MalformedIdent(&'a str),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier(&'a str),

    // Variant
    Variant(&'a VariantName),
    AppliedVariant(&'a (Loc<&'a VariantName>, [Loc<Pattern<'a>>])),

    // Literal
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(&'a str),
    EmptyRecordLiteral,
    Underscore,
}

#[test]
fn expr_size() {
    // The size of the Expr data structure should be exactly 5 machine words.
    // This test helps avoid regressions wich accidentally increase its size!
    assert_eq!(
        std::mem::size_of::<Expr>(),
        // TODO [move this comment to an issue] We should be able to get this
        // down to 2, which would mean we could fit 4 of these nodes in a single
        // 64-byte cache line instead of only being able to fit 1.
        //
        // Doing this would require, among other things:
        // 1. Making a str replacement where the length is stored as u32 instead of usize,
        //    to leave room for the tagged union's u8 tag.
        //    (Alternatively could store it as (&'a &'a str), but ew.)
        // 2. Similarly, making a slice replacement like that str replacement, and
        //    also where it doesn't share the bytes with anything else - so its
        //    elements can be consumed without having to clone them (unlike a slice).
        //    That's the only reason we're using Vec right now instead of slices -
        //    if we used slices, we'd have to clone their elements during canonicalization
        //    just to iterate over them and canonicalize them normally.
        // 3. Figuring out why (&'a (Foo, Bar)) by default takes up 24 bytes in Rust.
        //    I assume it's because the struct is being stored inline instead of
        //    as a pointer, but in this case we actually do want the pointer!
        //    We want to have the lifetime and we want to avoid using the unsafe keyword,
        //    but we also want this to only store 1 pointer in the AST node.
        //    Hopefully there's a way!
        //
        // It's also possible that 4 machine words might yield better performance
        // than 2, due to more data structures being inlinable, and therefore
        // having fewer pointers to chase. This seems worth investigating as well.
        std::mem::size_of::<usize>() * 5
    );
}

#[test]
fn pattern_size() {
    // The size of the Pattern data structure should be exactly 3 machine words.
    // This test helps avoid regressions wich accidentally increase its size!
    //
    // Worth noting that going up to 4 machine words is probably not a big deal;
    // an 8-byte cache line will only fit 2 of these regardless.
    assert_eq!(
        std::mem::size_of::<Pattern>(),
        // TODO [move this comment to an issue] We should be able to get this
        // down to 2, which would mean we could fit 4 of these nodes in a single
        // 64-byte cache line instead of only being able to fit 2.
        //
        // Doing this would require, among other things:
        // 1. Making a str replacement where the length is stored as u32 instead of usize,
        //    to leave room for the tagged union's u8 tag.
        //    (Alternatively could store it as (&'a &'a str), but ew.)
        // 2. Figuring out why &'a (Foo, Bar) by default takes up 24 bytes in Rust.
        //    I assume it's because the struct is being stored inline instead of
        //    as a pointer, but in this case we actually do want the pointer!
        //    We want to have the lifetime and we want to avoid using the unsafe keyword,
        //    but we also want this to only store 1 pointer in the AST node.
        //    Hopefully there's a way!
        //
        // It's also possible that going up to 4 machine words might yield even
        // better performance, due to more data structures being inlinable,
        // and therefore having fewer pointers to chase. This seems worth
        // investigating as well.
        std::mem::size_of::<usize>() * 3
    );
}

/// What we're currently attempting to parse, e.g.
/// "currently attempting to parse a list." This helps error messages!
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Attempting {
    List,
    Keyword,
    StringLiteral,
    RecordLiteral,
    InterpolatedString,
    NumberLiteral,
    UnicodeEscape,
    Expression,
    Module,
    Identifier,
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::Expr::*;

        match self {
            EmptyStr => write!(f, "\"\""),
            Str(string) => write!(f, "\"{}\"", string),
            BlockStr(lines) => write!(f, "\"\"\"{}\"\"\"", lines.join("\n")),
            Int(string) => string.fmt(f),
            Float(string) => string.fmt(f),
            HexInt(string) => write!(f, "0x{}", string),
            BinaryInt(string) => write!(f, "0b{}", string),
            OctalInt(string) => write!(f, "0o{}", string),
            EmptyRecord => write!(f, "{}", "{}"),
            other => panic!("TODO implement Display for AST variant {:?}", other),
        }
    }
}
