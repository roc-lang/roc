

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
        "UnexpectedPattern2Variant: required a {} at this position, Pattern2 was a {}.",
        required_pattern2,
        encountered_pattern2,
    ))]
    UnexpectedPattern2Variant {
        required_pattern2: String,
        encountered_pattern2: String,
        backtrace: Backtrace,
    },
}

pub type ASTResult<T, E = ASTError> = std::result::Result<T, E>;