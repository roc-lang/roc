use operator::Operator;
use parse::problems::Problem;
use region::Loc;
use std::fmt::{self, Display, Formatter};

pub type Ident = str;
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
    Int(Lossless<'a, i64>),
    Float(Lossless<'a, f64>),

    // String Literals
    EmptyStr,
    Str(Lossless<'a, &'a str>),
    /// basically InterpolatedStr(Vec<(String, Loc<Expr>)>, String)
    InterpolatedStr(
        &'a (
            &'a [(Lossless<'a, &'a str>, Loc<Expr<'a>>)],
            Lossless<'a, &'a str>,
        ),
    ),

    // List literals
    EmptyList,
    List(&'a [Loc<Expr<'a>>]),

    // Lookups
    Var(&'a Ident),

    // Pattern Matching
    Case(&'a (Loc<Expr<'a>>, [(Loc<Pattern<'a>>, Loc<Expr<'a>>)])),
    Closure(&'a (&'a [Loc<Pattern<'a>>], Loc<Expr<'a>>)),
    /// basically Assign(Vec<(Loc<Pattern>, Loc<Expr>)>, Loc<Expr>)
    Assign(&'a (&'a [(Loc<Pattern<'a>>, Loc<Expr<'a>>)], Loc<Expr<'a>>)),

    // Application
    Call(&'a (Loc<Expr<'a>>, [Loc<Expr<'a>>])),
    ApplyVariant(&'a (&'a VariantName, [Loc<Expr<'a>>])),
    Variant(&'a VariantName),

    // Product Types
    EmptyRecord,

    // Sugar
    If(&'a (Loc<Expr<'a>>, Loc<Expr<'a>>, Loc<Expr<'a>>)),
    Operator(&'a (Loc<Expr<'a>>, Loc<Operator>, Loc<Expr<'a>>)),

    // Runtime errors
    MalformedStr(Box<[Loc<Problem>]>),
    MalformedInt(Problem),
    MalformedFloat(Problem),
}

/// Sometimes when parsing, we want to retain the raw input string we parsed,
/// so we can restore what the user entered losslessly when formatting.
///
/// For example, when parsing the integer 1_234_567, we want to retain where
/// the undescores were in the raw input; otherwise, when running the formatter,
/// these would disappear.
#[derive(Clone, Debug, PartialEq)]
pub struct Lossless<'a, T> {
    pub raw: &'a str,
    pub value: T,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    // Identifier
    Identifier(&'a Ident),

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
        // down to 4, which would mean we could fit two of these in a 64-byte 
        // cache line instead of only being able to fit 1 like we can now.
        //
        // Doing this would require, among other things:
        // 1. Making a str replacement where the length is stored as u16 instead of usize,
        //    so Lossless takes up 2 machine words instead of 3.
        //    (Alternatively could store raw as (&'a &'a str), but ew.)
        // 2. Figuring out why &'a (Foo, Bar) by default takes up 24 bytes in Rust.
        //    I assume it's because the struct is being stored inline instead of
        //    as a pointer, but in this case we actually do want the pointer!
        //    We want to have the lifetime and we want to avoid using the unsafe keyword,
        //    but we also want this to only store 1 pointer in the AST node.
        //    Hopefully there's a way!
        //
        // It's also possible that aiming for 2 machine words might yield even
        // better performance, due to being able to fit 4 nodes in one cache
        // line instead of 2. This would require at least one of the following:
        //
        // * Using &Lossless for number and string literals.
        // * Parameterizing Expr on whether it needs Lossless (which is only
        //   used in formatting), so the data structure can take up less space 
        //   in memory when we aren't formatting.
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
            // Number Literals
            Int(ll_num) => ll_num.raw.fmt(f),
            Float(ll_num) => ll_num.raw.fmt(f), 

            // String Literals
            EmptyStr => write!(f, "\"\""),
            Str(ll_str) => ll_str.raw.fmt(f),
            InterpolatedStr((pairs, ll_trailing_str)) => {
                for (ll_str, loc_expr) in *pairs {
                    ll_str.raw.fmt(f)?;

                    (*loc_expr).value.fmt(f)?;
                }

                ll_trailing_str.raw.fmt(f)
            }

            // List literals
            EmptyList => write!(f, "[]"),
            List(exprs) => {
                write!(f, "[ ")?;

                for loc_expr in *exprs {
                    loc_expr.value.fmt(f)?;
                }

                write!(f, "] ")
            }
            // Lookups
            Var(ident) => ident.fmt(f),

            _ => panic!("TODO more formatting cases")
            //     // Pattern Matching
            //     Case(&'a (Loc<Expr<'a>>, [(Loc<Pattern<'a>>, Loc<Expr<'a>>)])),
            //     Closure(&'a (&'a [Loc<Pattern<'a>>], Loc<Expr<'a>>)),
            //     /// basically Assign(Vec<(Loc<Pattern>, Loc<Expr>)>, Loc<Expr>)
            //     Assign(&'a (&'a [(Loc<Pattern<'a>>, Loc<Expr<'a>>)], Loc<Expr<'a>>)),

            //     // Application
            //     Call(&'a (Loc<Expr<'a>>, [Loc<Expr<'a>>])),
            //     ApplyVariant(&'a (&'a VariantName, [Loc<Expr<'a>>])),
            //     Variant(&'a VariantName),

            //     // Product Types
            //     EmptyRecord,

            //     // Sugar
            //     If(&'a (Loc<Expr<'a>>, Loc<Expr<'a>>, Loc<Expr<'a>>)),
            //     Operator(&'a (Loc<Expr<'a>>, Loc<Operator>, Loc<Expr<'a>>)),

            //     // Runtime errors
            //     MalformedStr(Box<[Loc<Problem>]>),
            //     MalformedInt(Problem),
            //     MalformedFloat(Problem),
        }
    }
}
