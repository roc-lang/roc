pub mod ast;
pub mod ident;
pub mod keyword;
pub mod module;
pub mod number_literal;
pub mod parser;
pub mod problems;
pub mod string_literal;

use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use operator::Operator;
use parse::ast::{Attempting, Expr};
use parse::ident::{ident, Ident};
use parse::number_literal::number_literal;
use parse::parser::{
    and, attempt, loc, map, map_with_arena, one_of3, one_of4, one_of6, optional, string,
    unexpected, unexpected_eof, Either, ParseResult, Parser, State,
};
use parse::string_literal::string_literal;
use region::Located;

pub fn expr<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    // Recursive parsers must not directly invoke functions which return (impl Parser),
    // as this causes rustc to stack overflow.
    move |arena, state| parse_expr(min_indent, arena, state)
}

fn parse_expr<'a>(min_indent: u16, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Expr<'a>> {
    let expr_parser = map_with_arena(
        and(
            loc(one_of6(
                record_literal(),
                number_literal(),
                string_literal(),
                when(min_indent),
                conditional(min_indent),
                ident_etc(min_indent),
            )),
            optional(and(
                loc(operator()),
                loc(move |arena, state| parse_expr(min_indent, arena, state)),
            )),
        ),
        |arena, (loc_expr1, opt_operator)| match opt_operator {
            Some((loc_op, loc_expr2)) => {
                let tuple = arena.alloc((loc_expr1, loc_op, loc_expr2));

                Expr::Operator(tuple)
            }
            None => loc_expr1.value,
        },
    );

    attempt(Attempting::Expression, expr_parser).parse(arena, state)
}

pub fn loc_function_args<'a>(min_indent: u16) -> impl Parser<'a, &'a [Located<Expr<'a>>]> {
    move |arena, state| {
        panic!("TODO stop early if we see an operator after the whitespace - precedence!");
        // zero_or_more(after(one_or_more(whitespace(min_indent)), function_arg()))
    }
}

pub fn when<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    map(string(keyword::WHEN), |_| {
        panic!("TODO implement WHEN");
    })
}

pub fn conditional<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    one_of4(
        cond_help(keyword::IF, Expr::If, min_indent),
        cond_help(keyword::THEN, Expr::Then, min_indent),
        cond_help(keyword::ELSE, Expr::Else, min_indent),
        cond_help(keyword::CASE, Expr::Case, min_indent),
    )
}

fn cond_help<'a, F>(name: &str, wrap_expr: F, min_indent: u16) -> impl Parser<'a, Expr<'a>>
where
    F: Fn(&'a Located<Expr<'a>>) -> Expr<'a>,
{
    map(
        after(
            after(string(name), skip1_whitespace(min_indent)),
            loc(expr(min_indent)),
        ),
        wrap_expr,
    )
}

/// When we parse an ident like `foo ` it could be any of these:
///
/// 1. A standalone variable with trailing whitespace (e.g. because an operator is next)
/// 2. The beginning of a function call (e.g. `foo bar baz`)
/// 3. The beginning of a defniition (e.g. `foo =`)
/// 4. A reserved keyword (e.g. `if ` or `case `), meaning we should do something else.
pub fn ident_etc<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    let followed_by_equals = after(zero_or_more(whitespace(min_indent), char('=')));

    map_with_arena(
        and(
            loc(ident()),
            either(followed_by_equals, loc_function_args(min_indent)),
        ),
        |arena, (loc_ident, equals_or_loc_args)| {
            match equals_or_loc_args {
                Either::First(()) => {
                    // We have now parsed the beginning of a def (e.g. `foo =`)
                }
                Either::Second(loc_args) => {
                    // This appears to be a var, keyword, or function application.
                    let name_expr = match loc_ident.value {
                        Ident::Var(info) => Expr::Var(info.module_parts, info.value),
                        Ident::Variant(info) => Expr::Variant(info.module_parts, info.value),
                        Ident::Field(info) => Expr::QualifiedField(info.module_parts, info.value),
                        Ident::AccessorFunction(string) => Expr::AccessorFunction(string),
                        Ident::Malformed(string) => Expr::MalformedIdent(string),
                    };

                    if loc_args.is_empty() {
                        name_expr
                    } else {
                        let loc_expr = Located {
                            region: loc_ident.region,
                            value: name_expr,
                        };

                        Expr::Apply(arena.alloc((loc_expr, loc_args)))
                    }
                }
            }
        },
    )
}

pub fn operator<'a>() -> impl Parser<'a, Operator> {
    one_of3(
        map(string("+"), |_| Operator::Plus),
        map(string("-"), |_| Operator::Minus),
        map(string("*"), |_| Operator::Star),
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
