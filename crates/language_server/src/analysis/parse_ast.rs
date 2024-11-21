use bumpalo::Bump;
use roc_fmt::Buf;
use roc_parse::{ast::FullAst, header::parse_full_ast, parser::SyntaxError, state::State};
use roc_region::all::Loc;

use self::format::FormattedAst;

use super::tokens::{IterTokens, Token};

mod format;

pub struct Ast<'a> {
    arena: &'a Bump,
    full_ast: FullAst<'a>,
}

impl<'a> Ast<'a> {
    pub fn parse(arena: &'a Bump, src: &'a str) -> Result<Ast<'a>, SyntaxError<'a>> {
        let full_ast = parse_full_ast(arena, State::new(src.as_bytes()))?;
        Ok(Ast { full_ast, arena })
    }

    pub fn fmt(&self) -> FormattedAst<'a> {
        let mut buf = Buf::new_in(self.arena);

        roc_fmt::header::fmt_header(&mut buf, &self.full_ast.header);

        roc_fmt::def::fmt_stmts(&mut buf, &self.full_ast.stmts, 0);

        buf.fmt_end_of_file();

        FormattedAst::new(buf)
    }

    pub fn semantic_tokens(&self) -> impl IntoIterator<Item = Loc<Token>> + '_ {
        let header_tokens = self.full_ast.header.item.iter_tokens(self.arena);
        let body_tokens = self
            .full_ast
            .stmts
            .item
            .iter()
            .flat_map(|s| s.item.iter_tokens(self.arena));

        header_tokens.into_iter().chain(body_tokens)
    }
}
