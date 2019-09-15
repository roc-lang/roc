pub mod ast;
pub mod ident;
pub mod number_literal;
pub mod parser;
pub mod problems;
pub mod string_literal;

use parse::ast::{Attempting, Expr};
use parse::number_literal::number_literal;
use parse::parser::{attempt, one_of2, Parser};
use parse::string_literal::string_literal;

pub fn expr<'a>() -> impl Parser<'a, Expr<'a>> {
    attempt(
        Attempting::Expression,
        one_of2(number_literal(), string_literal()),
    )
}
