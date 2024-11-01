use crate::ast;
use crate::ast::Defs;
use crate::ast::Header;
use crate::ast::SpacesBefore;
use crate::header::parse_module_defs;
use crate::parser::SourceError;
use crate::parser::SyntaxError;
use crate::state::State;
use bumpalo::Bump;
use roc_region::all::Loc;

pub fn parse_expr_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<ast::Expr<'a>, SyntaxError<'a>> {
    let state = State::new(input.as_bytes());
    match crate::expr::test_parse_expr(arena, state) {
        Ok(loc_expr) => Ok(loc_expr.value),
        Err(fail) => Err(fail),
    }
}

pub fn parse_loc_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<Loc<ast::Expr<'a>>, SourceError<'a, SyntaxError<'a>>> {
    let state = State::new(input.as_bytes());
    match crate::expr::test_parse_expr(arena, state.clone()) {
        Ok(loc_expr) => Ok(loc_expr),
        Err(fail) => Err(fail.into_source_error(&state)),
    }
}

pub fn parse_defs_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Defs<'a>, SyntaxError<'a>> {
    let state = State::new(input.as_bytes());

    parse_module_defs(arena, state, Defs::default())
}

pub fn parse_header_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<SpacesBefore<'a, Header<'a>>, SyntaxError<'a>> {
    let state = State::new(input.as_bytes());

    match crate::header::parse_header(arena, state.clone()) {
        Ok((header, _)) => Ok(header),
        Err(fail) => Err(SyntaxError::Header(fail.problem)),
    }
}
