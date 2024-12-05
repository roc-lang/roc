use bumpalo::Bump;
use roc_fmt::{Buf, MigrationFlags};
use roc_parse::{
    ast::{Defs, Header, SpacesBefore},
    header::parse_module_defs,
    parser::SyntaxError,
};
use roc_region::all::Loc;

use self::format::FormattedAst;

use super::tokens::{IterTokens, Token};

mod format;

pub struct Ast<'a> {
    arena: &'a Bump,
    module: SpacesBefore<'a, Header<'a>>,
    defs: Defs<'a>,
}

impl<'a> Ast<'a> {
    pub fn parse(arena: &'a Bump, src: &'a str) -> Result<Ast<'a>, SyntaxError<'a>> {
        use roc_parse::{header::parse_header, state::State};

        let (module, state) = parse_header(arena, State::new(src.as_bytes()))
            .map_err(|e| SyntaxError::Header(e.problem))?;

        let (header, defs) = module.item.upgrade_header_imports(arena);

        let defs = parse_module_defs(arena, state, defs)?;

        Ok(Ast {
            module: SpacesBefore {
                before: module.before,
                item: header,
            },
            defs,
            arena,
        })
    }

    pub fn fmt(&self, flags: MigrationFlags) -> FormattedAst<'a> {
        let mut buf = Buf::new_in(self.arena, flags);

        roc_fmt::header::fmt_header(&mut buf, &self.module);

        roc_fmt::def::fmt_defs(&mut buf, &self.defs, 0);

        buf.fmt_end_of_file();

        FormattedAst::new(buf)
    }

    pub fn semantic_tokens(&self) -> impl IntoIterator<Item = Loc<Token>> + '_ {
        let header_tokens = self.module.item.iter_tokens(self.arena);
        let body_tokens = self.defs.iter_tokens(self.arena);

        header_tokens.into_iter().chain(body_tokens)
    }
}
