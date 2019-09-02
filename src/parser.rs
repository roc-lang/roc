use bumpalo::collections::string::String;
use bumpalo::Bump;
use operator::Operator;
use region;
use std::mem;

// Strategy:
//
// 1. Let space parsers check indentation. They should expect indentation to only ever increase (right?) when
//    doing a many_whitespaces or many1_whitespaces. Multline strings can have separate whitespace parsers.
// 2. For any expression that has subexpressions (e.g. ifs, parens, operators) record their indentation levels
//    by doing .and(position()) followed by .and_then() which says "I can have a declaration inside me as
//    long as the entire decl is indented more than me."
// 3. Make an alternative to RangeStreamOnce where uncons_while barfs on \t (or maybe just do this in whitespaces?)

type Loc<T> = region::Located<T>;

/// Struct which represents a position in a source file.
#[derive(Debug, Clone)]
pub struct State<'a> {
    /// The raw input string.
    pub input: &'a str,

    /// Current line of the input
    pub line: u32,
    /// Current column of the input
    pub column: u32,

    /// Current indentation level, in columns
    /// (so no indent is col 1 - this saves an arithmetic operation.)
    pub indent_col: u32,

    // true at the beginning of each line, then false after encountering
    // the first nonspace char on that line.
    pub is_indenting: bool,
}

type Ident = str;
type VariantName = str;

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
    Int(i64),
    Float(f64),

    // String Literals
    EmptyStr,
    Str(&'a str),
    /// basically InterpolatedStr(Vec<(String, Loc<Ident>)>, String)
    InterpolatedStr(&'a (&'a [(&'a str, Loc<&'a Ident>)], &'a str)),

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

#[derive(Clone, Debug, PartialEq)]
pub enum CanExpr {
    // Literals
    Int(i64),
    Float(f64),
    EmptyStr,
    Str(Box<str>),
    Char(char),
    List(Vec<Loc<CanExpr>>),
    EmptyList,
    EmptyRecord,
}

// fn _canonicalize<'a>(raw: &'a str, expr: Expr<'a>) -> CanExpr {
//     use self::CanExpr::*;

//     match expr {
//         Expr::Int(num) => Int(num),
//         Expr::Float(num) => Float(num),
//         Expr::EmptyRecord => EmptyRecord,
//         Expr::ShortStr(bytes) => {
//             let boxed: Box<str> = unsafe {
//                 // This is safe because these bytes were read directly out
//                 // of a utf-8 string, along appropriate code point boundaries.
//                 std::str::from_utf8_unchecked(&bytes)
//             }.into();

//             Str(boxed)
//         },
//         Expr::MedStr(offset, len) => {
//             let boxed: Box<str> = raw[offset..(offset + len as usize)].into();

//             Str(boxed)
//         }
//         Expr::LongStr(boxed_str) => Str((*boxed_str).into()),
//         Expr::EmptyStr => EmptyStr,
//         Expr::EmptyList => EmptyList,
//         _ => panic!("disco")
//     }
// }

#[test]
fn expr_size() {
    // The size of the Expr data structure should be exactly 3 machine words.
    // This test helps avoid regressions wich accidentally increase its size!
    assert_eq!(
        std::mem::size_of::<Expr>(),
        std::mem::size_of::<usize>() * 3
    );
}

#[test]
fn pattern_size() {
    // The size of the Pattern data structure should be exactly 3 machine words.
    // This test helps avoid regressions wich accidentally increase its size!
    assert_eq!(
        std::mem::size_of::<Pattern>(),
        std::mem::size_of::<usize>() * 3
    );
}

type ParseResult<'a, Output> = Result<(State<'a>, Output), (State<'a>, Attempting)>;

trait Parser<'a, Output> {
    fn parse(&self, &'a Bump, &'a State<'a>, attempting: Attempting) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a Bump, &'a State<'a>, Attempting) -> ParseResult<'a, Output>,
{
    fn parse(
        &self,
        arena: &'a Bump,
        state: &'a State<'a>,
        attempting: Attempting,
    ) -> ParseResult<'a, Output> {
        self(arena, state, attempting)
    }
}

fn map<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
where
    P: Parser<'a, Before>,
    F: Fn(Before) -> After,
{
    move |arena, state, attempting| {
        parser
            .parse(arena, state, attempting)
            .map(|(next_state, output)| (next_state, transform(output)))
    }
}

fn attempt<'a, P, Val>(attempting: Attempting, parser: P) -> impl Parser<'a, Val>
where
    P: Parser<'a, Val>,
{
    move |arena, state, _| parser.parse(arena, state, attempting)
}

/// A keyword with no newlines in it.
fn keyword<'a>(kw: &'static str) -> impl Parser<'a, ()> {
    // We can't have newlines because we don't attempt to advance the row
    // in the state, only the column.
    debug_assert!(!kw.contains("\n"));

    move |_arena: &'a Bump, state: &'a State<'a>, attempting| {
        let input = state.input;

        match input.get(0..kw.len()) {
            Some(next) if next == kw => {
                let len = kw.len();

                Ok((
                    State {
                        input: &input[len..],
                        column: state.column + len as u32,

                        ..*state
                    },
                    (),
                ))
            }
            _ => Err((state.clone(), attempting)),
        }
    }
}

fn satisfies<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |arena: &'a Bump, state: &'a State<'a>, attempting| {
        if let Ok((next_state, output)) = parser.parse(arena, state, attempting) {
            if predicate(&output) {
                return Ok((next_state, output));
            }
        }

        Err((state.clone(), attempting))
    }
}

fn any<'a>(arena: &'a Bump, state: &'a State<'a>, attempting: Attempting) -> ParseResult<'a, char> {
    let input = state.input;

    match input.chars().next() {
        Some(ch) => {
            let len = ch.len_utf8();
            let mut new_state = State {
                input: &input[len..],

                ..*state
            };

            if ch == '\n' {
                new_state.line = new_state.line + 1;
                new_state.column = 0;
            }

            Ok((new_state, ch))
        }
        _ => Err((state.clone(), attempting)),
    }
}

fn whitespace<'a>() -> impl Parser<'a, char> {
    satisfies(any, |ch| ch.is_whitespace())
}

/// What we're currently attempting to parse, e.g.
/// "currently attempting to parse a list." This helps error messages!
#[derive(Debug, Clone, Copy)]
pub enum Attempting {
    List,
    Keyword,
    StringLiteral,
}

// fn string_literal<'a>(arena: &'a Bump, state: &'a State<'a>, attempting: Attempting) -> Expr {
//     between(char('"'), char('"'),
//         zero_or_more(
//             choice((
//                 // Handle the edge cases where the interpolation happens
//                 // to be at the very beginning of the string literal,
//                 // or immediately following the previous interpolation.
//                 attempt(string("\\("))
//                     .with(value("".to_string()))
//                     .and(located(ident()).skip(char(')'))),

//                 // Parse a bunch of non-interpolated characters until we hit \(
//                 one_or_more(string_body())
//                     .map(|chars: Vec<char>| chars.into_iter().collect::<String>())
//                     .and(choice((
//                         attempt(string("\\(").with(located(ident()).skip(char(')')))),
//                         // If we never encountered \( then we hit the end of
//                         // the string literal. Use empty Ident here because
//                         // we're going to pop this Ident off the array anyhow.
//                         located(value("".to_string()))
//                     ))),
//             ))
//     )
//     .map(|mut pairs| {
//         match pairs.pop() {
//             None => Expr::EmptyStr,
//             Some(( trailing_str, located_name )) => {
//                 let mut ident_pairs = pairs.into_iter().map(|(string, located_name)| {
//                     ( string, located_name.map(|name| Ident::Unqualified(name.clone())) )
//                 }).collect::<Vec<(String, Located<Ident>)>>();

//                 if located_name.value.is_empty() {
//                     if ident_pairs.is_empty() {
//                         // We didn't find any interpolation at all. This is a string literal!
//                         Expr::Str(trailing_str.to_string())
//                     } else {
//                         Expr::InterpolatedStr(ident_pairs, trailing_str.to_string())
//                     }
//                 } else {
//                     // This is an interpolated string where the interpolation
//                     // happened to occur at the very end of the literal.

//                     // Put the tuple back.
//                     ident_pairs.push((
//                         trailing_str,
//                         located_name.map(|name| Ident::Unqualified(name.clone()))
//                     ));

//                     Expr::InterpolatedStr(ident_pairs, "".to_string())
//                 }
//             }
//         }
//     }))
// }

fn string_literal<'a>() -> impl Parser<'a, Expr<'a>> {
    |arena: &'a Bump, state: &'a State<'a>, attempting| {
        let mut chars = state.input.chars();

        // String literals must start with a quote.
        // If this doesn't, it must not be a string literal!
        if chars.next() != Some('"') {
            return Err((state.clone(), attempting));
        }

        // If we have precisely an empty string here, don't bother allocating
        // a buffer; instead, return EmptyStr immediately.
        if (&mut chars).peekable().peek() == Some(&'"') {
            return Ok((
                State {
                    input: &state.input[2..],
                    column: state.column + 2,

                    ..*state
                },
                Expr::EmptyStr,
            ));
        }

        // We'll store the accumulated string in this buffer.
        let mut buf = String::new_in(arena);

        while let Some(ch) = chars.next() {
            match ch {
                // If it's a backslash, escape things.
                '\\' => match chars.next() {
                    Some('\\') => buf.push('\\'),
                    Some('"') => buf.push('"'),
                    Some('t') => buf.push('\t'),
                    Some('n') => buf.push('\n'),
                    Some('r') => buf.push('\r'),
                    Some('u') => {
                        // TODO don't bail out on invalid unicode sequences!
                        // Instead, parse successfully as a Problem - like,
                        // this string has a problem with it, but that doesn't
                        // mean we have to fail parsing.
                        panic!("TODO handle unicode chars")
                    }
                    Some('(') => panic!("TODO handle string interpolation"),
                    Some(unsupported) => {
                        // TODO don't bail out here! Instead, parse successfully
                        // as a Problem - like, this string has a problem with
                        // it, but that doesn't mean we have to fail parsing.
                        panic!("TODO bad escaped char {}", unsupported)
                    }
                    None => {
                        // We ran out of characters before finding a closed quote;
                        // let the loop finish normally, so we end up returning
                        // the error that the string was not terminated.
                        //
                        // (There's the separate problem of a trailing backslash,
                        // but often that will get fixed in the course of
                        // addressing the missing closed quote.)
                        ()
                    }
                },
                '"' => {
                    // We found a closed quote; this is the end of the string!
                    let len_with_quotes = buf.len() + 2;
                    let expr = Expr::Str(buf.into_bump_str());

                    return Ok((
                        State {
                            input: &state.input[len_with_quotes..],
                            column: state.column + len_with_quotes as u32,

                            ..*state
                        },
                        expr,
                    ));
                }
                normal_char => buf.push(normal_char),
            }
        }

        // We ran out of characters before finding a closed quote
        Err((state.clone(), Attempting::StringLiteral))
    }
}
