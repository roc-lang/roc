use roc_module::{ident::Ident, module_err::ModuleError};
use roc_parse::parser::SyntaxError;
use roc_region::all::{Located, Region};
use snafu::{Backtrace, ErrorCompat, Snafu};

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
    #[snafu(display("IdentExistsError: {}", msg))]
    IdentExistsError { msg: String },
    #[snafu(display("ModuleError: {}", msg))]
    ModuleErrorNoBacktrace { msg: String },
    #[snafu(display("SyntaxError: {}", msg))]
    SyntaxErrorNoBacktrace { msg: String },
}

pub type ASTResult<T, E = ASTError> = std::result::Result<T, E>;

impl From<ModuleError> for ASTError {
    fn from(module_err: ModuleError) -> Self {
        let msg = format!("{}", module_err);

        if let Some(backtrace_ref) = module_err.backtrace() {
            todo!("Error:{}\nBacktrace:{:?}", msg, backtrace_ref); //see snafu#313 
            /*Self::UIErrorBacktrace {
                msg,
                backtrace: *backtrace_ref
            }*/
        } else {
            Self::ModuleErrorNoBacktrace {
                msg
            }
        }
    }
}

impl From<(Region, Located<Ident>)> for ASTError {
    fn from(ident_exists_err: (Region, Located<Ident>)) -> Self {

        Self::IdentExistsError {
            msg: format!("{:?}", ident_exists_err)
        }
    }
}


impl<'a> From<SyntaxError<'a>> for ASTError {
    fn from(syntax_err: SyntaxError) -> Self {
        Self::SyntaxErrorNoBacktrace {
            msg: format!("{:?}", syntax_err)
        }
    }
}
