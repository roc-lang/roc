pub mod ast;
pub mod parser;
pub mod problems;
pub mod string_literal;

use parse::ast::Expr;
use parse::parser::Parser;
use parse::string_literal::string_literal;

pub fn expr<'a, 'p>() -> impl Parser<'a, 'p, Expr<'a>>
where
    'p: 'a,
{
    string_literal()
}
