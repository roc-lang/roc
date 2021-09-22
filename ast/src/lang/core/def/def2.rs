use crate::{lang::core::{expr::expr2::Expr2, pattern::Pattern2}, pool::pool::NodeId};


// A top level definition, not inside a function. For example: `main = "Hello, world!"`
#[derive(Debug)]
pub enum Def2 {
    // ValueDef example: `main = "Hello, world!"`. identifier -> `main`, expr -> "Hello, world!"
    ValueDef {
        identifier_id: NodeId<Pattern2>,
        expr_id: NodeId<Expr2>,
    },
    Blank,
}

pub type DefId = NodeId<Def2>;