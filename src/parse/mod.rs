pub mod ast;
pub mod number_literal;
pub mod parser;
pub mod problems;
pub mod string_literal;

use parse::ast::Expr;
// use parse::number_literal::number_literal;
use parse::parser::Parser;
use parse::string_literal::string_literal;

pub fn expr<'a>() -> impl Parser<'a, Expr<'a>> {
    // parser::one_of2(string_literal(), number_literal())
    string_literal()
}

const KW_IF: &'static str = "if";
