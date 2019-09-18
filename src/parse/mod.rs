pub mod ast;
pub mod blankspace;
pub mod ident;
pub mod keyword;
pub mod module;
pub mod number_literal;
pub mod parser;
pub mod problems;
pub mod string_literal;

use bumpalo::collections::Vec;
use bumpalo::Bump;
use operator::Operator;
use parse::ast::{Attempting, Expr};
use parse::blankspace::{space0, space0_before, space1_before};
use parse::ident::{ident, Ident};
use parse::number_literal::number_literal;
use parse::parser::{
    and, attempt, char, either, loc, map, map_with_arena, one_of4, one_of6, one_or_more, optional,
    skip_first, string, unexpected, unexpected_eof, Either, ParseResult, Parser, State,
};
use parse::string_literal::string_literal;
use region::Located;

pub fn expr<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    // Recursive parsers must not directly invoke functions which return (impl Parser),
    // as this causes rustc to stack overflow.
    move |arena, state| parse_expr(min_indent, arena, state)
}

fn parse_expr_body_without_operators<'a>(
    min_indent: u16,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Expr<'a>> {
    one_of6(
        string_literal(),
        record_literal(),
        number_literal(),
        when(min_indent),
        conditional(min_indent),
        ident_etc(min_indent),
    )
    .parse(arena, state)
}

fn parse_expr<'a>(min_indent: u16, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Expr<'a>> {
    let expr_parser = map_with_arena(
        and(
            // First parse the body without operators, then try to parse possible operators after.
            loc(move |arena, state| parse_expr_body_without_operators(min_indent, arena, state)),
            optional(and(
                and(space0(min_indent), loc(operator())),
                space0_before(
                    move |arena, state| parse_expr(min_indent, arena, state),
                    min_indent,
                ),
            )),
        ),
        |arena, (loc_expr1, opt_operator)| match opt_operator {
            Some(((spaces_before_op, loc_op), loc_expr2)) => {
                let loc_expr1 = if spaces_before_op.is_empty() {
                    loc_expr1
                } else {
                    Expr::with_spaces_after(arena, loc_expr1, spaces_before_op)
                };
                let tuple = arena.alloc((loc_expr1, loc_op, loc_expr2));

                Expr::Operator(tuple)
            }
            None => loc_expr1.value,
        },
    );

    attempt(Attempting::Expression, expr_parser).parse(arena, state)
}

fn function_arg<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    // Don't parse operators, because they have a higher precedence than function application.
    // If we encounter one, we're done parsing function args!
    move |arena, state| parse_expr_body_without_operators(min_indent, arena, state)
}

pub fn when<'a>(_min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map(string(keyword::WHEN), |_| {
        panic!("TODO implement WHEN");
    })
}

pub fn conditional<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    // TODO figure out how to remove this code duplication in a way rustc
    // accepts. I tried making a helper functions and couldn't resolve the
    // lifetime errors, so I manually inlined them and moved on.
    one_of4(
        map_with_arena(
            skip_first(
                string(keyword::IF),
                space1_before(expr(min_indent), min_indent),
            ),
            |arena, loc_expr| Expr::If(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::THEN),
                space1_before(expr(min_indent), min_indent),
            ),
            |arena, loc_expr| Expr::Then(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::ELSE),
                space1_before(expr(min_indent), min_indent),
            ),
            |arena, loc_expr| Expr::Else(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::CASE),
                space1_before(expr(min_indent), min_indent),
            ),
            |arena, loc_expr| Expr::Case(arena.alloc(loc_expr)),
        ),
    )
}
pub fn loc_function_args<'a>(min_indent: u16) -> impl Parser<'a, Vec<'a, Located<Expr<'a>>>> {
    one_or_more(space1_before(function_arg(min_indent), min_indent))
}

/// When we parse an ident like `foo ` it could be any of these:
///
/// 1. A standalone variable with trailing whitespace (e.g. because an operator is next)
/// 2. The beginning of a function call (e.g. `foo bar baz`)
/// 3. The beginning of a defniition (e.g. `foo =`)
/// 4. The beginning of a type annotation (e.g. `foo :`)
/// 5. A reserved keyword (e.g. `if ` or `case `), meaning we should do something else.
pub fn ident_etc<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map_with_arena(
        and(
            loc(ident()),
            optional(either(
                // There may optionally be function args after this ident
                loc_function_args(min_indent),
                // If there aren't any args, there may be a '=' or ':' after it.
                // (It's a syntax error to write e.g. `foo bar =` - so if there
                // were any args, there is definitely no need to parse '=' or ':'!)
                and(space0(min_indent), either(char('='), char(':'))),
            )),
        ),
        |arena, (loc_ident, opt_extras)| {
            // This appears to be a var, keyword, or function application.

            match opt_extras {
                Some(Either::First(loc_args)) => {
                    let loc_expr = Located {
                        region: loc_ident.region,
                        value: ident_to_expr(loc_ident.value),
                    };

                    Expr::Apply(arena.alloc((loc_expr, loc_args.into_bump_slice())))
                }
                Some(Either::Second((_space_list, Either::First(())))) => {
                    panic!("TODO handle def, making sure not to drop comments!");
                }
                Some(Either::Second((_space_list, Either::Second(())))) => {
                    panic!("TODO handle annotation, making sure not to drop comments!");
                }
                None => ident_to_expr(loc_ident.value),
            }
        },
    )
}

fn ident_to_expr<'a>(src: Ident<'a>) -> Expr<'a> {
    match src {
        Ident::Var(info) => Expr::Var(info.module_parts, info.value),
        Ident::Variant(info) => Expr::Variant(info.module_parts, info.value),
        Ident::Field(info) => Expr::QualifiedField(info.module_parts, info.value),
        Ident::AccessorFunction(string) => Expr::AccessorFunction(string),
        Ident::Malformed(string) => Expr::MalformedIdent(string),
    }
}

pub fn operator<'a>() -> impl Parser<'a, Operator> {
    one_of4(
        map(char('+'), |_| Operator::Plus),
        map(char('-'), |_| Operator::Minus),
        map(char('*'), |_| Operator::Star),
        map(char('/'), |_| Operator::Slash),
    )
}

pub fn record_literal<'a>() -> impl Parser<'a, Expr<'a>> {
    move |_arena: &'a Bump, state: State<'a>| {
        let mut chars = state.input.chars();

        match chars.next() {
            Some('{') => (),
            Some(other_char) => {
                return Err(unexpected(other_char, 0, state, Attempting::RecordLiteral));
            }
            None => {
                return Err(unexpected_eof(0, Attempting::RecordLiteral, state));
            }
        }

        match chars.next() {
            Some('}') => {
                let next_state = state.advance_without_indenting(2)?;

                Ok((Expr::EmptyRecord, next_state))
            }
            Some(other_char) => Err(unexpected(other_char, 0, state, Attempting::RecordLiteral)),
            None => Err(unexpected_eof(0, Attempting::RecordLiteral, state)),
        }
    }
}
