use roc_module::{ident::Ident, module_err::ModuleError};
use roc_parse::parser::SyntaxError;
use roc_region::all::{Loc, Region};
use snafu::{Backtrace, Snafu};

use crate::lang::core::ast::ASTNodeId;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum ASTError {
    #[snafu(display(
        "ASTNodeIdWithoutExprId: The expr_id_opt in ASTNode({:?}) was `None` but I was expecting `Some(ExprId)` .",
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
    #[snafu(display("IdentExistsError: {}", msg))]
    IdentExistsError { msg: String },

    WrapModuleError {
        #[snafu(backtrace)]
        source: ModuleError,
    },

    #[snafu(display("SyntaxError: {}", msg))]
    SyntaxErrorNoBacktrace { msg: String },
}

pub type ASTResult<T, E = ASTError> = std::result::Result<T, E>;

impl From<ModuleError> for ASTError {
    fn from(module_err: ModuleError) -> Self {
        Self::WrapModuleError { source: module_err }
    }
}

impl From<(Region, Loc<Ident>)> for ASTError {
    fn from(ident_exists_err: (Region, Loc<Ident>)) -> Self {
        Self::IdentExistsError {
            msg: format!("{ident_exists_err:?}"),
        }
    }
}

impl<'a> From<SyntaxError<'a>> for ASTError {
    fn from(syntax_err: SyntaxError) -> Self {
        Self::SyntaxErrorNoBacktrace {
            msg: format!("{syntax_err:?}"),
        }
    }
}
