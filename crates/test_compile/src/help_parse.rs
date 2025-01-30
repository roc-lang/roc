use bumpalo::Bump;
use roc_parse::{
    ast,
    blankspace::space0_before_optional_after,
    expr::{expr_end, loc_expr_block},
    parser::{skip_second, EExpr, Parser, SourceError, SyntaxError},
    state::State,
};
use roc_region::all::{Loc, Position};

use crate::deindent::trim_and_deindent;

pub struct ParseExpr {
    arena: Bump,
}

impl Default for ParseExpr {
    fn default() -> Self {
        Self {
            arena: Bump::with_capacity(4096),
        }
    }
}

impl ParseExpr {
    pub fn parse_expr<'a>(&'a self, input: &'a str) -> Result<ast::Expr<'a>, SyntaxError<'a>> {
        self.parse_loc_expr(input)
            .map(|loc_expr| loc_expr.value)
            .map_err(|e| e.problem)
    }

    pub fn parse_loc_expr<'a>(
        &'a self,
        input: &'a str,
    ) -> Result<Loc<ast::Expr<'a>>, SourceError<'a, SyntaxError<'a>>> {
        let original_bytes = trim_and_deindent(&self.arena, input).as_bytes();
        let state = State::new(original_bytes);

        let parser = skip_second(
            space0_before_optional_after(
                loc_expr_block(false),
                EExpr::IndentStart,
                EExpr::IndentEnd,
            ),
            expr_end(),
        );

        match parser.parse(&self.arena, state, 0) {
            Ok((_, loc_expr, _)) => Ok(loc_expr),
            Err((_, fail)) => Err(SourceError {
                problem: SyntaxError::Expr(fail, Position::default()),
                bytes: original_bytes,
            }),
        }
    }

    pub fn into_arena(self) -> Bump {
        self.arena
    }

    pub fn arena(&self) -> &Bump {
        &self.arena
    }
}
