use crate::ast;
use crate::ast::Defs;
use crate::ast::Header;
use crate::ast::Pattern;
use crate::ast::SpacesBefore;
use crate::header::parse_module_defs;
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
    let state = State::new(input.as_bytes());

    match crate::expr::test_parse_expr(0, arena, state.clone()) {
        Ok(loc_expr) => Ok(loc_expr),
        Err(fail) => Err(SyntaxError::Expr(fail, Position::default()).into_source_error(&state)),
    }
}

pub fn parse_pattern_with<'a>(
    arena: &'a Bump,
    input: &'a str,
) -> Result<Loc<Pattern<'a>>, SourceError<'a, SyntaxError<'a>>> {
    let state = State::new(input.as_bytes());

    match crate::pattern::test_parse_pattern(0, arena, state.clone()) {
        Ok(loc_patt) => Ok(loc_patt),
        Err(fail) => Err(SyntaxError::Pattern(fail).into_source_error(&state)),
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
