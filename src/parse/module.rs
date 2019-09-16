use ident::Ident;
use parse::ast::{Expr, Pattern};

pub struct Module<'a> {
    pub name: Ident,
    pub exposes: Vec<Ident>,
    pub uses: Vec<Ident>,
    pub decls: Vec<Decl<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl<'a> {
    Def(Pattern<'a>, Expr<'a>, Expr<'a>),
    // TODO Alias
    // TODO SumType
}
