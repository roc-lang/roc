use crate::module::module_defs;
use crate::parser::Parser;
use crate::parser::SourceError;
use crate::parser::SyntaxError;
use crate::state::State;
use bumpalo::Bump;
use roc_ast2::Defs;
use roc_ast2::Expr;
use roc_ast2::Header;
use roc_ast2::SpacesBefore;
use roc_region::all::Loc;
use roc_region::all::Position;

pub fn parse_expr_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Expr<'a>, SyntaxError<'a>> {
    parse_loc_with(arena, input)
        .map(|loc_expr| loc_expr.value)
        .map_err(|e| e.problem)
}

#[allow(dead_code)]
pub fn parse_loc_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<Loc<Expr<'a>>, SourceError<'a, SyntaxError<'a>>> {
    let state = State::new(input.trim().as_bytes());

    match crate::expr::test_parse_expr(0, arena, state.clone()) {
        Ok(loc_expr) => Ok(loc_expr),
        Err(fail) => Err(SyntaxError::Expr(fail, Position::default()).into_source_error(&state)),
    }
}

pub fn parse_defs_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Defs<'a>, SyntaxError<'a>> {
    let state = State::new(input.trim().as_bytes());

    let min_indent = 0;

    match module_defs().parse(arena, state, min_indent) {
        Ok(tuple) => Ok(tuple.1),
        Err(tuple) => Err(tuple.1),
    }
}

pub fn parse_header_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<SpacesBefore<'a, Header<'a>>, SyntaxError<'a>> {
    let state = State::new(input.trim().as_bytes());

    match crate::module::parse_header(arena, state.clone()) {
        Ok((header, _)) => Ok(header),
        Err(fail) => Err(SyntaxError::Header(fail.problem)),
    }
}
