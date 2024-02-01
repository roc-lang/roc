use bumpalo::Bump;
use roc_fmt::Buf;
use roc_parse::{
    ast::{Defs, Module},
    parser::SyntaxError,
};
use roc_region::all::Loc;

use self::format::FormattedAst;

use super::tokens::{IterTokens, Token};

mod format;

pub struct Ast<'a> {
    arena: &'a Bump,
    module: Module<'a>,
    defs: Defs<'a>,
}

impl<'a> Ast<'a> {
    pub fn parse(arena: &'a Bump, src: &'a str) -> Result<Ast<'a>, SyntaxError<'a>> {
        use roc_parse::{
            module::{parse_header, parse_module_defs},
            state::State,
        };

        let (module, state) = parse_header(arena, State::new(src.as_bytes()))
            .map_err(|e| SyntaxError::Header(e.problem))?;

        let defs = parse_module_defs(arena, state, Defs::default())?;

        Ok(Ast {
            module,
            defs,
            arena,
        })
    }

    pub fn fmt(&self) -> FormattedAst<'a> {
        let mut buf = Buf::new_in(self.arena);

        roc_fmt::module::fmt_module(&mut buf, &self.module);

        roc_fmt::def::fmt_defs(&mut buf, &self.defs, 0);

        buf.fmt_end_of_file();

        FormattedAst::new(buf)
    }

    pub fn semantic_tokens(&self) -> impl IntoIterator<Item = Loc<Token>> + '_ {
        let header_tokens = self.module.iter_tokens(self.arena);
        let body_tokens = self.defs.iter_tokens(self.arena);

        header_tokens.into_iter().chain(body_tokens)
    }
}
