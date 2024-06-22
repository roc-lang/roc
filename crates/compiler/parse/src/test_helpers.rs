use crate::ast;
use crate::ast::Defs;
use crate::module::parse_module_defs;
use crate::parser::SourceError;
use crate::parser::SyntaxError;
use crate::state::State;
use bumpalo::Bump;
use roc_region::all::Loc;
use roc_region::all::Position;

pub fn parse_expr_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<ast::Expr<'a>, SyntaxError<'a>> {
    parse_loc_with(arena, input)
        .map(|loc_expr| loc_expr.value)
        .map_err(|e| e.problem)
}

#[allow(dead_code)]
pub fn parse_loc_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<Loc<ast::Expr<'a>>, SourceError<'a, SyntaxError<'a>>> {
    let state = State::new(input.trim().as_bytes());

    match crate::expr::test_parse_expr(0, arena, state.clone()) {
        Ok(loc_expr) => Ok(loc_expr),
        Err(fail) => Err(SyntaxError::Expr(fail, Position::default()).into_source_error(&state)),
    }
}

pub fn parse_defs_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Defs<'a>, SyntaxError<'a>> {
    let state = State::new(input.trim().as_bytes());

    parse_module_defs(arena, state, Defs::default())
}

pub fn parse_header_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<ast::Module<'a>, SyntaxError<'a>> {
    let state = State::new(input.trim().as_bytes());

    match crate::module::parse_header(arena, state.clone()) {
        Ok((header, _)) => Ok(header),
        Err(fail) => Err(SyntaxError::Header(fail.problem)),
    }
}
