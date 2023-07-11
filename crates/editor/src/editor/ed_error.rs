use crate::ui::text::text_pos::TextPos;
use colored::*;
use roc_ast::ast_error::ASTError;
use roc_ast::lang::core::ast::ASTNodeId;
use roc_code_markup::markup_error::MarkError;
use roc_code_markup::slow_pool::MarkNodeId;
use roc_module::module_err::ModuleError;
use snafu::{Backtrace, ErrorCompat, Snafu};

//import errors as follows:
// `use crate::error::OutOfBounds;`
// *not* `use crate::error::EdError::OutOfBounds;`
// see https://github.com/shepmaster/snafu/issues/211

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum EdError {
    #[snafu(display(
        "ASTNodeIdWithoutDefId: The expr_id_opt in ASTNode({:?}) was `None` but I was expecting `Some(DefId)` .",
        ast_node_id
    ))]
    ASTNodeIdWithoutDefId {
        ast_node_id: ASTNodeId,
        backtrace: Backtrace,
    },

    #[snafu(display(
        "ASTNodeIdWithoutExprId: The expr_id_opt in ASTNode({:?}) was `None` but I was expecting `Some(ExprId)` .",
        ast_node_id
    ))]
    ASTNodeIdWithoutExprId {
        ast_node_id: ASTNodeId,
        backtrace: Backtrace,
    },

    #[snafu(display(
        "CaretNotFound: No carets were found in the expected node with id {}",
        node_id
    ))]
    CaretNotFound {
        node_id: MarkNodeId,
        backtrace: Backtrace,
    },

    #[snafu(display("ClipboardReadFailed: could not get clipboard contents: {}", err_msg))]
    ClipboardReadFailed {
        err_msg: String,
    },

    #[snafu(display("ClipboardWriteFailed: could not set clipboard contents: {}", err_msg))]
    ClipboardWriteFailed {
        err_msg: String,
    },

    #[snafu(display(
        "ClipboardInitFailed: could not initialize ClipboardContext: {}.",
        err_msg
    ))]
    ClipboardInitFailed {
        err_msg: String,
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
        "EmptyCodeString: I need to have a code string (code_str) that contains either an app, interface, package, or platform header. The code string was empty.",
    ))]
    EmptyCodeString {
        backtrace: Backtrace,
    },

    #[snafu(display("FailedToUpdateIdentIdName: {}", err_str))]
    FailedToUpdateIdentIdName {
        err_str: String,
        backtrace: Backtrace,
    },

    #[snafu(display("GetContentOnNestedNode: tried to get string content from Nested MarkupNode. Can only get content from Text or Blank nodes."))]
    GetContentOnNestedNode {
        backtrace: Backtrace,
    },

    #[snafu(display(
        "IndexOfFailed: Element {} was not found in collection {}.",
        elt_str,
        collection_str
    ))]
    IndexOfFailed {
        elt_str: String,
        collection_str: String,
        backtrace: Backtrace,
    },

    #[snafu(display("KeyNotFound: key {} was not found in HashMap.", key_str,))]
    KeyNotFound {
        key_str: String,
        backtrace: Backtrace,
    },

    #[snafu(display(
        "MissingParent: MarkupNode with id {} should have a parent but there was none.",
        node_id
    ))]
    MissingParent {
        node_id: MarkNodeId,
        backtrace: Backtrace,
    },

    #[snafu(display(
        "MissingSelection: ed_model.selected_expr2_id was Some(ExprId) but ed_model.caret_w_sel_vec did not contain any Some(Selection)."
    ))]
    MissingSelection {
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

    #[snafu(display("NestedNodeWithoutChildren: tried to retrieve child from Nested MarkupNode with id {} but it had no children.", node_id))]
    NestedNodeWithoutChildren {
        node_id: MarkNodeId,
        backtrace: Backtrace,
    },

    #[snafu(display("NoDefMarkNodeBeforeLineNr: I could not find a MarkupNode whose root parent points to a DefId located before the given line number: {}.", line_nr))]
    NoDefMarkNodeBeforeLineNr {
        line_nr: usize,
        backtrace: Backtrace,
    },

    #[snafu(display("NodeWithoutAttributes: expected to have a node with attributes. This is a Nested MarkupNode, only Text and Blank nodes have attributes."))]
    NodeWithoutAttributes {
        backtrace: Backtrace,
    },

    #[snafu(display(
        "NodeIdNotInGridNodeMap: MarkNodeId {} was not found in ed_model.grid_node_map.",
        node_id
    ))]
    NodeIdNotInGridNodeMap {
        node_id: MarkNodeId,
        backtrace: Backtrace,
    },

    #[snafu(display(
        "NoNodeAtCaretPosition: there was no node at the current caret position {:?}.",
        caret_pos,
    ))]
    NoNodeAtCaretPosition {
        caret_pos: TextPos,
        backtrace: Backtrace,
    },

    #[snafu(display(
        "OutOfBounds: index {} was out of bounds for {} with length {}.",
        index,
        collection_name,
        len
    ))]
    OutOfBounds {
        index: usize,
        collection_name: String,
        len: usize,
        backtrace: Backtrace,
    },

    #[snafu(display("RecordWithoutFields: expected record to have at least one field because it is not an EmptyRecord."))]
    RecordWithoutFields {
        backtrace: Backtrace,
    },

    #[snafu(display(
        "RocCheckFailed: `cargo run check`/`roc check` detected errors(see terminal)."
    ))]
    RocCheckFailed,

    #[snafu(display("ParseError: Failed to parse AST: SyntaxError: {}.", syntax_err))]
    SrcParseError {
        syntax_err: String,
        backtrace: Backtrace,
    },

    #[snafu(display("StringParseError: {}", msg))]
    StringParseError {
        msg: String,
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
        "UnexpectedEmptyPoolVec: expected PoolVec {} to have at least one element.",
        descriptive_vec_name
    ))]
    UnexpectedEmptyPoolVec {
        descriptive_vec_name: String,
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

    #[snafu(display("ASTError: {}", msg))]
    ASTErrorBacktrace {
        msg: String,
        backtrace: Backtrace,
    },
    #[snafu(display("UIError: {}", msg))]
    UIErrorBacktrace {
        msg: String,
        backtrace: Backtrace,
    },
    #[snafu(display("MarkError: {}", msg))]
    MarkErrorBacktrace {
        msg: String,
        backtrace: Backtrace,
    },
    WrapASTError {
        #[snafu(backtrace)]
        source: ASTError,
    },
    WrapUIError {
        #[snafu(backtrace)]
        source: UIError,
    },
    WrapMarkError {
        #[snafu(backtrace)]
        source: MarkError,
    },
    WrapModuleError {
        #[snafu(backtrace)]
        source: ModuleError,
    },
    WrapIoError {
        source: std::io::Error,
    },
}

pub type EdResult<T, E = EdError> = std::result::Result<T, E>;

pub fn print_err(err: &EdError) {
    eprintln!("{}", format!("{err}").truecolor(255, 0, 0));

    if let Some(backtrace) = ErrorCompat::backtrace(err) {
        eprintln!("{}", color_backtrace(backtrace));
    }
}

fn color_backtrace(backtrace: &snafu::Backtrace) -> String {
    let backtrace_str = format!("{backtrace}");
    let backtrace_split = backtrace_str.split('\n');
    let irrelevant_src = vec![".cargo", "registry", ".rustup", "rustc"];

    let mut ret_str = String::new();
    let mut prev_line_opt: Option<String> = None;

    for line in backtrace_split {
        let new_line = if line.contains("src") {
            if !contains_one_of(line, &irrelevant_src) {
                if let Some(prev_line) = prev_line_opt {
                    prev_line_opt = Some(format!("{}", prev_line.truecolor(255, 30, 30)));
                }
                format!("{}\n", line.truecolor(255, 100, 100))
            } else {
                format!("{line}\n")
            }
        } else {
            format!("{line}\n")
        };

        if let Some(prev_line) = prev_line_opt {
            ret_str.push_str(&prev_line);
        }
        prev_line_opt = Some(new_line);
    }

    ret_str
}

fn contains_one_of(main_str: &str, contain_slice: &[&str]) -> bool {
    for contain_str in contain_slice {
        if main_str.contains(contain_str) {
            return true;
        }
    }

    false
}

impl From<EdError> for String {
    fn from(ed_error: EdError) -> Self {
        format!("{ed_error}")
    }
}

use crate::ui::ui_error::UIError;

impl From<UIError> for EdError {
    fn from(ui_err: UIError) -> Self {
        Self::WrapUIError { source: ui_err }
    }
}

impl From<MarkError> for EdError {
    fn from(mark_err: MarkError) -> Self {
        Self::WrapMarkError { source: mark_err }
    }
}

impl From<ASTError> for EdError {
    fn from(ast_err: ASTError) -> Self {
        Self::WrapASTError { source: ast_err }
    }
}

impl From<ModuleError> for EdError {
    fn from(module_err: ModuleError) -> Self {
        Self::WrapModuleError { source: module_err }
    }
}

impl From<std::io::Error> for EdError {
    fn from(io_err: std::io::Error) -> Self {
        Self::WrapIoError { source: io_err }
    }
}
