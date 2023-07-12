use roc_error_utils::UtilError;
use snafu::{Backtrace, NoneError, ResultExt, Snafu};

use crate::slow_pool::MarkNodeId;

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum MarkError {
    #[snafu(display(
        "CaretNotFound: No carets were found in the expected node with id {}",
        node_id
    ))]
    CaretNotFound {
        node_id: MarkNodeId,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "ExpectedTextNode: the function {} expected a Text node, got {} instead.",
        function_name,
        node_type
    ))]
    ExpectedTextNode {
        function_name: String,
        node_type: String,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "MarkNodeIdWithoutCorrespondingASTNodeId: MarkupNode with id {} was not found in MarkIdAstIdMap, available keys are: {}.",
        node_id,
        keys_str
    ))]
    MarkNodeIdWithoutCorrespondingASTNodeId {
        node_id: MarkNodeId,
        keys_str: String,
        backtrace: Backtrace,
    },
    #[snafu(display("NestedNodeMissingChild: expected to find child with id {} in Nested MarkupNode, but it was missing. Id's of the children are {:?}.", node_id, children_ids))]
    NestedNodeMissingChild {
        node_id: MarkNodeId,
        children_ids: Vec<MarkNodeId>,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "NestedNodeRequired: required a Nested node at this position, node was a {}.",
        node_type
    ))]
    NestedNodeRequired {
        node_type: String,
        backtrace: Backtrace,
    },
    #[snafu(display("UtilError: {}", msg))]
    UtilErrorBacktrace { msg: String, backtrace: Backtrace },
}

pub type MarkResult<T, E = MarkError> = std::result::Result<T, E>;

impl From<UtilError> for MarkError {
    fn from(util_err: UtilError) -> Self {
        let msg = format!("{util_err}");

        // hack to handle MarkError derive
        let dummy_res: Result<(), NoneError> = Err(NoneError {});
        dummy_res
            .context(UtilErrorBacktraceSnafu { msg })
            .unwrap_err()
    }
}
