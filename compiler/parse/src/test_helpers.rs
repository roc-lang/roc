use crate::ast;
use crate::module::module_defs;
use crate::parser::{Parser, State, SyntaxError};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_region::all::Located;

pub fn parse_expr_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<ast::Expr<'a>, SyntaxError<'a>> {
    parse_loc_with(arena, input).map(|loc_expr| loc_expr.value)
}

#[allow(dead_code)]
pub fn parse_defs_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<Vec<'a, Located<ast::Def<'a>>>, SyntaxError<'a>> {
    let state = State::new_in(arena, input.trim().as_bytes());
    let answer = module_defs().parse(arena, state);
    answer
        .map(|(_, loc_expr, _)| loc_expr)
        .map_err(|(_, fail, _)| fail)
}

#[allow(dead_code)]
pub fn parse_loc_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<Located<ast::Expr<'a>>, SyntaxError<'a>> {
    let state = State::new_in(arena, input.trim().as_bytes());

    match crate::expr::test_parse_expr(0, arena, state) {
        Ok((loc_expr, _state)) => Ok(loc_expr),
        Err(fail) => Err(SyntaxError::Expr(fail)),
    }
}
