use bumpalo::collections::Vec;
use parse::ast::Spaceable;
use parse::blankspace::space0_around;
use parse::parser::{sep_by0, skip_first, skip_second, Parser};
use region::Located;

/// Parse zero or more elements between two braces (e.g. square braces).
/// Elements can be optionally surrounded by spaces, and are separated by a
/// delimiter (e.g comma-separated). Braces and delimiters get discarded.
pub fn collection<'a, Elem, OpeningBrace, ClosingBrace, Delimiter, S>(
    opening_brace: OpeningBrace,
    elem: Elem,
    delimiter: Delimiter,
    closing_brace: ClosingBrace,
    min_indent: u16,
) -> impl Parser<'a, Vec<'a, Located<S>>>
where
    OpeningBrace: Parser<'a, ()>,
    Elem: Parser<'a, Located<S>>,
    Elem: 'a,
    Delimiter: Parser<'a, ()>,
    S: Spaceable<'a>,
    S: 'a,
    ClosingBrace: Parser<'a, ()>,
{
    // TODO allow trailing commas before the closing delimiter, *but* without
    // losing any comments or newlines! This will require parsing them and then,
    // if they are present, merging them into the final Spaceable.
    skip_first(
        opening_brace,
        skip_second(
            sep_by0(delimiter, space0_around(elem, min_indent)),
            closing_brace,
        ),
    )
}
