use expr::{Pattern, Ident};

pub struct Module {
    name: Ident,
    exposes: Vec<Ident>,
    uses: Vec<Ident>,
    decls: Vec<Decl>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Assign(Pattern, Box<Expr>, Box<Expr>),
    // TODO Alias
    // TODO SumType
}
