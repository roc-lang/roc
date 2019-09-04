pub mod ast;
pub mod parser;
pub mod problems;
pub mod string_literal;

use parse::ast::Expr;
use parse::parser::Parser;
use parse::string_literal::string_literal;

pub fn expr<'a>() -> impl Parser<'a, Expr<'a>> {
    string_literal()
}
