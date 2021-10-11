use roc_module::symbol::{IdentId};
use snafu::{Backtrace, Snafu};

use crate::lang::core::ast::ASTNodeId;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum ASTError {
    #[snafu(display(
        "ASTNodeIdWithoutExprId: The expr_id_opt in ASTNode({:?}) was `None` but I was expexting `Some(ExprId)` .",
        ast_node_id
    ))]
    ASTNodeIdWithoutExprId {
        ast_node_id: ASTNodeId,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "UnexpectedASTNode: required a {} at this position, node was a {}.",
        required_node_type,
        encountered_node_type
    ))]
    UnexpectedASTNode {
        required_node_type: String,
        encountered_node_type: String,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "UnexpectedPattern2Variant: required a {} at this position, Pattern2 was a {}.",
        required_pattern2,
        encountered_pattern2,
    ))]
    UnexpectedPattern2Variant {
        required_pattern2: String,
        encountered_pattern2: String,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "IdentIdNotFound: I could not find IdentId {:?} in env.ident_ids {:?}.",
        ident_id,
        env_ident_ids_str
    ))]
    IdentIdNotFound {
        ident_id: IdentId,
        env_ident_ids_str: String,
        backtrace: Backtrace,
    },
}

pub type ASTResult<T, E = ASTError> = std::result::Result<T, E>;
