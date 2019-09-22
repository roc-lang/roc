use bumpalo::collections::vec::Vec;
use bumpalo::collections::String;
use bumpalo::Bump;
use operator::Operator;
use region::{Loc, Region};

#[derive(Clone, Debug, PartialEq)]
pub enum Module<'a> {
    Api(&'a [&'a str], &'a str, Vec<'a, Def<'a>>),
    App(&'a [&'a str], &'a str),
}
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
    Str(&'a str),
    BlockStr(&'a [&'a str]),
    /// e.g. `(expr).foo.bar`
    Field(&'a Loc<Expr<'a>>, Vec<'a, &'a str>),
    /// e.g. `Foo.Bar.baz.qux`
    QualifiedField(&'a [&'a str], &'a [&'a str]),
    /// e.g. `.foo`
    AccessorFunction(&'a str),

    // Collection Literals
    List(Vec<'a, Loc<Expr<'a>>>),
    Record(Vec<'a, Loc<Expr<'a>>>),
    AssignField(Loc<&'a str>, &'a Loc<Expr<'a>>),

    // Lookups
    Var(&'a [&'a str], &'a str),
    Variant(&'a [&'a str], &'a str),

    // Pattern Matching
    When(&'a [(Loc<Pattern<'a>>, Loc<Expr<'a>>)]),
    Closure(&'a (Vec<'a, Loc<Pattern<'a>>>, Loc<Expr<'a>>)),
    /// Multiple defs in a row
    Defs(&'a (Vec<'a, Def<'a>>, Loc<Expr<'a>>)),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a variant by name, do Apply(Variant(...), ...)
    Apply(&'a (Loc<Expr<'a>>, Vec<'a, Loc<Expr<'a>>>)),
    Operator(&'a (Loc<Expr<'a>>, Loc<Operator>, Loc<Expr<'a>>)),

    // Conditionals
    If(&'a Loc<Expr<'a>>),
    Then(&'a Loc<Expr<'a>>),
    Else(&'a Loc<Expr<'a>>),
    Case(&'a Loc<Expr<'a>>),

    // Blank Space (e.g. comments, spaces, newlines) before or after an expression.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Expr<'a>, &'a [CommentOrNewline<'a>]),

    // Problems
    MalformedIdent(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Def<'a> {
    AnnotationOnly,
    BodyOnly(Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),
    AnnotatedBody(Loc<Pattern<'a>>, &'a Loc<Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum CommentOrNewline<'a> {
    Newline,
    LineComment(&'a str),
    BlockComment(&'a [&'a str]),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier(&'a str),

    // Variant, optionally qualified
    Variant(&'a [&'a str], &'a str),
    Apply(&'a (Loc<&'a Pattern<'a>>, [Loc<Pattern<'a>>])),
    /// This is Loc<Pattern> rather than Loc<str> so we can record comments
    /// around the destructured names, e.g. { x ### x does stuff ###, y }
    /// In practice, these patterns will always be Identifier
    RecordDestructure(Vec<'a, Loc<Pattern<'a>>>),

    // Literal
    IntLiteral(i64),
    FloatLiteral(f64),
    StrLiteral(&'a str),
    EmptyRecordLiteral,
    Underscore,

    // Space
    SpaceBefore(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Pattern<'a>, &'a [CommentOrNewline<'a>]),
}

pub trait Spaceable<'a> {
    fn before(&'a self, &'a [CommentOrNewline<'a>]) -> Self;
    fn after(&'a self, &'a [CommentOrNewline<'a>]) -> Self;

    fn with_spaces_before(&'a self, spaces: &'a [CommentOrNewline<'a>], region: Region) -> Loc<Self>
    where
        Self: Sized,
    {
        Loc {
            region,
            value: self.before(spaces),
        }
    }

    fn with_spaces_after(&'a self, spaces: &'a [CommentOrNewline<'a>], region: Region) -> Loc<Self>
    where
        Self: Sized,
    {
        Loc {
            region,
            value: self.after(spaces),
        }
    }
}

impl<'a> Spaceable<'a> for Expr<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Expr::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Expr::SpaceAfter(self, spaces)
    }
}

impl<'a> Spaceable<'a> for Pattern<'a> {
    fn before(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Pattern::SpaceBefore(self, spaces)
    }
    fn after(&'a self, spaces: &'a [CommentOrNewline<'a>]) -> Self {
        Pattern::SpaceAfter(self, spaces)
    }
}

#[test]
fn expr_size() {
    // The size of the Expr data structure should be exactly 6 machine words.
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
        std::mem::size_of::<usize>() * 6
    );
}

#[test]
fn pattern_size() {
    // The size of the Pattern data structure should be exactly 4 machine words.
    // This test helps avoid regressions wich accidentally increase its size!
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
        std::mem::size_of::<usize>() * 5
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
    RecordFieldLabel,
    InterpolatedString,
    NumberLiteral,
    UnicodeEscape,
    Expression,
    Module,
    Identifier,
}

impl<'a> Expr<'a> {
    pub fn loc_ref(&'a self, region: Region) -> Loc<&'a Self> {
        Loc {
            region,
            value: self,
        }
    }

    pub fn loc(self, region: Region) -> Loc<Self> {
        Loc {
            region,
            value: self,
        }
    }
}

pub fn format<'a>(arena: &'a Bump, expr: &'a Expr<'a>, _indent: u16) -> String<'a> {
    use self::Expr::*;

    let mut buf = String::new_in(arena);

    match expr {
        Str(string) => {
            buf.push('"');
            buf.push_str(string);
            buf.push('"');
        }
        BlockStr(lines) => {
            buf.push_str("\"\"\"");
            for line in lines.iter() {
                buf.push_str(line);
            }
            buf.push_str("\"\"\"");
        }
        Int(string) => buf.push_str(string),
        Float(string) => buf.push_str(string),
        HexInt(string) => {
            buf.push('0');
            buf.push('x');
            buf.push_str(string);
        }
        BinaryInt(string) => {
            buf.push('0');
            buf.push('b');
            buf.push_str(string);
        }
        OctalInt(string) => {
            buf.push('0');
            buf.push('o');
            buf.push_str(string);
        }
        Record(fields) => {
            buf.push('{');

            for _field in fields {
                panic!("TODO implement Display for record fields.");
            }

            buf.push('}');
        }
        other => panic!("TODO implement Display for AST variant {:?}", other),
    }

    buf
}
