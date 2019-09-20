pub mod ast;
pub mod blankspace;
pub mod ident;
pub mod keyword;
pub mod module;
pub mod number_literal;
pub mod parser;
pub mod problems;
pub mod string_literal;

use bumpalo::collections::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use operator::Operator;
use parse::ast::{Attempting, Expr};
use parse::blankspace::{space0, space0_around, space0_before, space1_before};
use parse::ident::{ident, Ident};
use parse::number_literal::number_literal;
use parse::parser::{
    and, attempt, between, char, either, loc, map, map_with_arena, one_of4, one_of8, one_or_more,
    optional, sep_by0, skip_first, skip_second, string, unexpected, unexpected_eof, Either,
    ParseResult, Parser, State,
};
use parse::string_literal::string_literal;
use region::Located;

pub fn expr<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    // Recursive parsers must not directly invoke functions which return (impl Parser),
    // as this causes rustc to stack overflow. Thus, parse_expr must be a
    // separate function which recurses by calling itself directly.
    move |arena, state| parse_expr(min_indent, arena, state)
}

fn loc_parse_expr_body_without_operators<'a>(
    min_indent: u16,
    arena: &'a Bump,
    state: State<'a>,
) -> ParseResult<'a, Located<Expr<'a>>> {
    one_of8(
        loc_parenthetical_expr(min_indent),
        loc(string_literal()),
        loc(number_literal()),
        loc(record_literal(min_indent)),
        loc(list_literal(min_indent)),
        loc(when(min_indent)),
        loc(conditional(min_indent)),
        loc(ident_etc(min_indent)),
    )
    .parse(arena, state)
}

fn parse_expr<'a>(min_indent: u16, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Expr<'a>> {
    let expr_parser = map_with_arena(
        and(
            // First parse the body without operators, then try to parse possible operators after.
            move |arena, state| loc_parse_expr_body_without_operators(min_indent, arena, state),
            // Parse the operator, with optional spaces before it.
            //
            // Since spaces can only wrap an Expr, not an Operator, we have to first
            // parse the spaces and then attach them retroactively to the expression
            // preceding the operator (the one we parsed before considering operators).
            optional(and(
                and(space0(min_indent), loc(operator())),
                // The spaces *after* the operator can be attached directly to
                // the expression following the operator.
                space0_before(
                    loc(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                ),
            )),
        ),
        |arena, (loc_expr1, opt_operator)| match opt_operator {
            Some(((spaces_before_op, loc_op), loc_expr2)) => {
                let loc_expr1 = if spaces_before_op.is_empty() {
                    loc_expr1
                } else {
                    // Attach the spaces retroactively to the expression preceding the operator.
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

pub fn loc_parenthetical_expr<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    map_with_arena(
        loc(and(
            between(
                char('('),
                space0_around(
                    loc(move |arena, state| parse_expr(min_indent, arena, state)),
                    min_indent,
                ),
                char(')'),
            ),
            optional(either(
                // There may optionally be function args after the ')'
                // e.g. ((foo bar) baz)
                loc_function_args(min_indent),
                // If there aren't any args, there may be a '=' or ':' after it.
                //
                // (It's a syntax error to write e.g. `foo bar =` - so if there
                // were any args, there is definitely no need to parse '=' or ':'!)
                //
                // Also, there may be a '.' for field access (e.g. `(foo).bar`),
                // but we only want to look for that if there weren't any args,
                // as if there were any args they'd have consumed it anyway
                // e.g. in `((foo bar) baz.blah)` the `.blah` will be consumed by the `baz` parser
                either(
                    one_or_more(skip_first(char('.'), field_label())),
                    and(space0(min_indent), either(char('='), char(':'))),
                ),
            )),
        )),
        |arena, loc_expr_with_extras| {
            // We parse the parenthetical expression *and* the arguments after it
            // in one region, so that (for example) the region for Apply includes its args.
            let (loc_expr, opt_extras) = loc_expr_with_extras.value;

            match opt_extras {
                Some(Either::First(loc_args)) => Located {
                    region: loc_expr_with_extras.region,
                    value: Expr::Apply(arena.alloc((loc_expr, loc_args))),
                },
                // '=' after optional spaces
                Some(Either::Second(Either::Second((_space_list, Either::First(()))))) => {
                    panic!("TODO handle def, making sure not to drop comments!");
                }
                // ':' after optional spaces
                Some(Either::Second(Either::Second((_space_list, Either::Second(()))))) => {
                    panic!("TODO handle annotation, making sure not to drop comments!");
                }
                // '.' and a record field immediately after ')', no optional spaces
                Some(Either::Second(Either::First(fields))) => Located {
                    region: loc_expr_with_extras.region,
                    value: Expr::Field(arena.alloc(loc_expr), fields),
                },
                None => loc_expr,
            }
        },
    )
}

fn loc_function_arg<'a>(min_indent: u16) -> impl Parser<'a, Located<Expr<'a>>> {
    // Don't parse operators, because they have a higher precedence than function application.
    // If we encounter one, we're done parsing function args!
    move |arena, state| loc_parse_expr_body_without_operators(min_indent, arena, state)
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
                space1_before(loc(expr(min_indent)), min_indent),
            ),
            |arena, loc_expr| Expr::If(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::THEN),
                space1_before(loc(expr(min_indent)), min_indent),
            ),
            |arena, loc_expr| Expr::Then(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::ELSE),
                space1_before(loc(expr(min_indent)), min_indent),
            ),
            |arena, loc_expr| Expr::Else(arena.alloc(loc_expr)),
        ),
        map_with_arena(
            skip_first(
                string(keyword::CASE),
                space1_before(loc(expr(min_indent)), min_indent),
            ),
            |arena, loc_expr| Expr::Case(arena.alloc(loc_expr)),
        ),
    )
}
pub fn loc_function_args<'a>(min_indent: u16) -> impl Parser<'a, Vec<'a, Located<Expr<'a>>>> {
    one_or_more(space1_before(loc_function_arg(min_indent), min_indent))
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

                    Expr::Apply(arena.alloc((loc_expr, loc_args)))
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

pub fn list_literal<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    let elems = collection(
        char('['),
        loc(expr(min_indent)),
        char(','),
        char(']'),
        min_indent,
    );

    attempt(Attempting::List, map(elems, Expr::List))
}

pub fn record_literal<'a>(min_indent: u16) -> impl Parser<'a, Expr<'a>> {
    let field = map_with_arena(
        and(
            loc(skip_second(field_label(), char(':'))),
            space0_before(
                loc(move |arena, state| parse_expr(min_indent, arena, state)),
                min_indent,
            ),
        ),
        |arena, (label, loc_expr)| Expr::AssignField(label, arena.alloc(loc_expr)),
    );
    let fields = collection(char('{'), loc(field), char(','), char('}'), min_indent);

    attempt(Attempting::List, map(fields, Expr::Record))
}

/// A record field, e.g. "email" in `.email` or in `email:`
pub fn field_label<'a>() -> impl Parser<'a, &'a str> {
    move |arena, state: State<'a>| {
        let mut chars = state.input.chars();

        // Field labels must start with a lowercase letter.
        let first_letter = match chars.next() {
            Some(ch) => {
                if ch.is_alphabetic() && ch.is_lowercase() {
                    ch
                } else {
                    return Err(unexpected(ch, 0, state, Attempting::RecordFieldLabel));
                }
            }
            None => {
                return Err(unexpected_eof(0, Attempting::RecordFieldLabel, state));
            }
        };

        let mut buf = String::with_capacity_in(1, arena);

        buf.push(first_letter);

        while let Some(ch) = chars.next() {
            // After the first character, only these are allowed:
            //
            // * Unicode alphabetic chars - you might include `鹏` if that's clear to your readers
            // * ASCII digits - e.g. `1` but not `¾`, both of which pass .is_numeric()
            // * A ':' indicating the end of the field
            if ch.is_alphabetic() || ch.is_ascii_digit() {
                buf.push(ch);
            } else {
                // This is the end of the field. We're done!
                break;
            }
        }

        let chars_parsed = buf.len();

        Ok((
            buf.into_bump_str(),
            state.advance_without_indenting(chars_parsed)?,
        ))
    }
}

/// Parse zero or more elements between two braces (e.g. square braces).
/// Elements can be optionally surrounded by spaces, and are separated by a
/// delimiter (e.g comma-separated). Braces and delimiters get discarded.
pub fn collection<'a, Elem, OpeningBrace, ClosingBrace, Sep>(
    opening_brace: OpeningBrace,
    elem: Elem,
    sep: Sep,
    closing_brace: ClosingBrace,
    min_indent: u16,
) -> impl Parser<'a, Vec<'a, Located<Expr<'a>>>>
where
    OpeningBrace: Parser<'a, ()>,
    Elem: Parser<'a, Located<Expr<'a>>>,
    Sep: Parser<'a, ()>,
    ClosingBrace: Parser<'a, ()>,
{
    skip_first(
        opening_brace,
        skip_second(sep_by0(sep, space0_around(elem, min_indent)), closing_brace),
    )
}
