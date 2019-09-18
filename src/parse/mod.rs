pub mod ast;
pub mod blankspace;
pub mod ident;
pub mod keyword;
pub mod module;
pub mod number_literal;
pub mod parser;
pub mod problems;
pub mod string_literal;

use bumpalo::Bump;
use operator::Operator;
use parse::ast::{Attempting, Expr};
use parse::blankspace::{space0, space1_before};
use parse::ident::{ident, Ident};
use parse::number_literal::number_literal;
use parse::parser::{
    and, attempt, ch, either, loc, map, map_with_arena, one_of3, one_of4, one_of6, optional,
    skip_first, string, unexpected, unexpected_eof, Either, ParseResult, Parser, State,
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
            loc(move |arena, state| parse_expr_body_without_operators(min_indent, arena, state)),
            optional(and(
                and(space0(min_indent), and(loc(operator()), space0(min_indent))),
                loc(move |arena, state| parse_expr(min_indent, arena, state)),
            )),
        ),
        |arena, (loc_expr1, opt_operator)| match opt_operator {
            Some(((spaces_before_op, (loc_op, spaces_after_op)), loc_expr2)) => {
                let region1 = loc_expr1.region.clone();
                let region2 = loc_expr2.region.clone();
                let loc_expr1 = if spaces_before_op.is_empty() {
                    loc_expr1
                } else {
                    Expr::with_spaces_after(arena.alloc(loc_expr1), spaces_before_op).loc(region1)
                };
                let loc_expr2 = if spaces_after_op.is_empty() {
                    loc_expr2
                } else {
                    Expr::with_spaces_after(arena.alloc(loc_expr2), spaces_after_op).loc(region2)
                };
                let tuple = arena.alloc((loc_expr1, loc_op, loc_expr2));

                Expr::Operator(tuple)
            }
            None => loc_expr1.value,
        },
    );

    attempt(Attempting::Expression, expr_parser).parse(arena, state)
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

pub fn loc_function_args<'a>(_min_indent: u16) -> impl Parser<'a, &'a [Located<Expr<'a>>]> {
    move |_arena, _state| {
        panic!("TODO stop early if we see an operator after the whitespace - precedence!");
        // zero_or_more(after(one_or_more(whitespace(min_indent)), function_arg()))
    }
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
                loc(space1_before(expr(min_indent), min_indent)),
            ),
            |arena, loc_expr| Expr::If(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::THEN),
                loc(space1_before(expr(min_indent), min_indent)),
            ),
            |arena, loc_expr| Expr::Then(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::ELSE),
                loc(space1_before(expr(min_indent), min_indent)),
            ),
            |arena, loc_expr| Expr::Else(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::CASE),
                loc(space1_before(expr(min_indent), min_indent)),
            ),
            |arena, loc_expr| Expr::Case(arena.alloc(loc_expr)),
        ),
    )
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
            either(
                // Check if this is either a def or type annotation
                and(space0(min_indent), either(ch('='), ch(':'))),
                // Check if this is function application
                loc_function_args(min_indent),
            ),
        ),
        |arena, (loc_ident, equals_or_loc_args)| {
            match equals_or_loc_args {
                Either::First((_space_list, Either::First(()))) => {
                    // We have now parsed the beginning of a def (e.g. `foo =`)
                    panic!("TODO parse def, making sure not to drop comments!");
                }
                Either::First((_space_list, Either::Second(()))) => {
                    // We have now parsed the beginning of a type annotation (e.g. `foo :`)
                    panic!("TODO parse type annotation, making sure not to drop comments!");
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
