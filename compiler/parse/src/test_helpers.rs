use crate::ast;
use crate::module::module_defs;
// use crate::module::module_defs;
use crate::parser::Parser;
use crate::parser::{State, SyntaxError};
use bumpalo::collections::Vec as BumpVec;
use bumpalo::Bump;
use roc_region::all::Located;

pub fn parse_expr_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<ast::Expr<'a>, SyntaxError<'a>> {
    parse_loc_with(arena, input).map(|loc_expr| loc_expr.value)
}

#[allow(dead_code)]
pub fn parse_loc_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<Located<ast::Expr<'a>>, SyntaxError<'a>> {
    let state = State::new(input.trim().as_bytes());

    match crate::expr::test_parse_expr(0, arena, state) {
        Ok(loc_expr) => Ok(loc_expr),
        Err(fail) => Err(SyntaxError::Expr(fail)),
    }
}

pub fn parse_defs_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<BumpVec<'a, Located<ast::Def<'a>>>, SyntaxError<'a>> {
    let state = State::new(input.trim().as_bytes());

    match module_defs().parse(arena, state) {
        Ok(tuple) => Ok(tuple.1),
        Err(tuple) => Err(tuple.1),
    }
}
