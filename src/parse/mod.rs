pub mod ast;
pub mod ident;
pub mod module;
pub mod number_literal;
pub mod parser;
pub mod problems;
pub mod string_literal;

use bumpalo::Bump;
use operator::Operator;
use parse::ast::{Attempting, Expr};
use parse::number_literal::number_literal;
use parse::parser::{
    and, attempt, lazy, loc, map, map_with_arena, one_of3, optional, string, unexpected,
    unexpected_eof, val, Parser, State,
};
use parse::string_literal::string_literal;

pub fn expr<'a>() -> impl Parser<'a, Expr<'a>> {
    map_with_arena(
        and(
            attempt(
                Attempting::Expression,
                loc(one_of3(
                    record_literal(),
                    number_literal(),
                    string_literal(),
                )),
            ),
            optional(and(loc(operator()), loc(val(Expr::Str("blah"))))),
        ),
        |arena, (loc_expr1, opt_operator)| match opt_operator {
            Some((loc_op, loc_expr2)) => {
                let tuple = arena.alloc((loc_expr1, loc_op, loc_expr2));

                Expr::Operator(tuple)
            }
            None => loc_expr1.value,
        },
    )
}

pub fn operator<'a>() -> impl Parser<'a, Operator> {
    val(Operator::Plus)
    // one_of3(
    //     map(string("+"), |_| Operator::Plus),
    //     map(string("-"), |_| Operator::Minus),
    //     map(string("*"), |_| Operator::Star),
    // )
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
