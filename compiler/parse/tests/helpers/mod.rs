extern crate bumpalo;

use self::bumpalo::Bump;
use roc_parse::ast::{self, Attempting};
use roc_parse::blankspace::space0_before;
use roc_parse::parser::{loc, Fail, Parser, State};
use roc_region::all::Located;

#[allow(dead_code)]
pub fn parse_with<'a>(arena: &'a Bump, input: &'a str) -> Result<ast::Expr<'a>, Fail> {
    parse_loc_with(arena, input).map(|loc_expr| loc_expr.value)
}

#[allow(dead_code)]
pub fn parse_loc_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Located<ast::Expr<'a>>, Fail> {
    let state = State::new(input.trim().as_bytes(), Attempting::Module);
    let parser = space0_before(loc(roc_parse::expr::expr(0)), 0);
    let answer = parser.parse(&arena, state);

    answer
        .map(|(loc_expr, _)| loc_expr)
        .map_err(|(fail, _)| fail)
}
