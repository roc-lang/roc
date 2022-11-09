use roc_module::symbol::IdentId;
use std::fmt::Write as _; // import without risk of name clashing

use crate::{
    lang::core::expr::{expr2::Expr2, expr2_to_string::expr2_to_string},
    mem_pool::pool::{NodeId, Pool},
};

// A top level definition, not inside a function. For example: `main = "Hello, world!"`
#[derive(Debug)]
pub enum Def2 {
    // ValueDef example: `main = "Hello, world!"`. identifier -> `main`, expr -> "Hello, world!"
    ValueDef {
        identifier_id: IdentId,
        expr_id: NodeId<Expr2>,
    },
    Blank,
    CommentsBefore {
        comments: String,
        def_id: DefId,
    },
    CommentsAfter {
        comments: String,
        def_id: DefId,
    },
}

pub type DefId = NodeId<Def2>;

pub fn def2_to_string(node_id: DefId, pool: &Pool) -> String {
    let mut full_string = String::new();
    let def2 = pool.get(node_id);

    match def2 {
        Def2::ValueDef {
            identifier_id,
            expr_id,
        } => {
            let _ = write!(
                full_string,
                "Def2::ValueDef(identifier_id: >>{:?}), expr_id: >>{:?})",
                identifier_id,
                expr2_to_string(*expr_id, pool)
            );
        }
        Def2::Blank => {
            full_string.push_str("Def2::Blank");
        }
        Def2::CommentsBefore {
            comments,
            def_id: _,
        } => full_string.push_str(comments),
        Def2::CommentsAfter {
            comments,
            def_id: _,
        } => full_string.push_str(comments),
    }

    full_string
}
