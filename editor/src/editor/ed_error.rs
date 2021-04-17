use crate::editor::slow_pool::MarkNodeId;
use crate::ui::ui_error::UIResult;
use colored::*;
use snafu::{Backtrace, ErrorCompat, NoneError, ResultExt, Snafu};

//import errors as follows:
// `use crate::error::OutOfBounds;`
// *not* `use crate::error::EdError::OutOfBounds;`
// see https://github.com/shepmaster/snafu/issues/211

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum EdError {
    #[snafu(display(
        "CaretNotFound: No carets were found in the expected node with id {}",
        node_id
    ))]
    CaretNotFound {
        node_id: MarkNodeId,
        backtrace: Backtrace,
    },

    #[snafu(display("ClipboardReadFailed: could not get clipboard contents: {}", err_msg))]
    ClipboardReadFailed { err_msg: String },

    #[snafu(display("ClipboardWriteFailed: could not set clipboard contents: {}", err_msg))]
    ClipboardWriteFailed { err_msg: String },

    #[snafu(display(
        "ClipboardInitFailed: could not initialize ClipboardContext: {}.",
        err_msg
    ))]
    ClipboardInitFailed { err_msg: String },

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

    #[snafu(display("GetContentOnNestedNode: tried to get string content from Nested MarkupNode. Can only get content from Text or Blank nodes."))]
    GetContentOnNestedNode { backtrace: Backtrace },

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
        "MissingSelection: ed_model.selected_expr2_id was Some(NodeId<Expr2>) but ed_model.caret_w_sel_vec did not contain any Some(Selection)."
    ))]
    MissingSelection { backtrace: Backtrace },

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

    #[snafu(display("NodeWithoutAttributes: expected to have a node with attributes. This is a Nested MarkupNode, only Text and Blank nodes have attributes."))]
    NodeWithoutAttributes { backtrace: Backtrace },

    #[snafu(display(
        "NodeIdNotInGridNodeMap: MarkNodeId {} was not found in ed_model.grid_node_map.",
        node_id
    ))]
    NodeIdNotInGridNodeMap {
        node_id: MarkNodeId,
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

    #[snafu(display("ParseError: Failed to parse AST: SyntaxError: {}.", syntax_err))]
    ParseError { syntax_err: String },

    #[snafu(display("RecordWithoutFields: expected record to have at least one field because it is not an EmpyRecord."))]
    RecordWithoutFields { backtrace: Backtrace },

    #[snafu(display("UIError: {}", msg))]
    UIErrorBacktrace { msg: String, backtrace: Backtrace },
}

pub type EdResult<T, E = EdError> = std::result::Result<T, E>;

pub fn print_err(err: &EdError) {
    eprintln!("{}", format!("{}", err).truecolor(255, 0, 0));

    if let Some(backtrace) = ErrorCompat::backtrace(err) {
        eprintln!("{}", color_backtrace(backtrace));
    }
}

pub fn print_ui_err(err: &UIError) {
    eprintln!("{}", format!("{}", err).truecolor(255, 0, 0));

    if let Some(backtrace) = ErrorCompat::backtrace(err) {
        eprintln!("{}", color_backtrace(backtrace));
    }
}

fn color_backtrace(backtrace: &snafu::Backtrace) -> String {
    let backtrace_str = format!("{}", backtrace);
    let backtrace_split = backtrace_str.split('\n');
    let irrelevant_src = vec![".cargo", "registry", ".rustup", "rustc"];

    let mut ret_str = String::new();
    let mut prev_line_opt: Option<String> = None;

    for line in backtrace_split {
        let new_line = if line.contains("src") {
            if !contains_one_of(&line, &irrelevant_src) {
                if let Some(prev_line) = prev_line_opt {
                    prev_line_opt = Some(format!("{}", prev_line.truecolor(255, 30, 30)));
                }
                format!("{}\n", line.truecolor(255, 100, 100))
            } else {
                format!("{}\n", line)
            }
        } else {
            format!("{}\n", line)
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
        format!("{}", ed_error)
    }
}

use crate::ui::ui_error::UIError;

impl From<UIError> for EdError {
    fn from(ui_err: UIError) -> Self {
        let msg = format!("{}", ui_err);

        // hack to handle EdError derive
        let dummy_res: Result<(), NoneError> = Err(NoneError {});
        dummy_res.context(UIErrorBacktrace { msg }).unwrap_err()
    }
}

pub fn from_ui_res<T>(ui_res: UIResult<T>) -> EdResult<T> {
    match ui_res {
        Ok(t) => Ok(t),
        Err(ui_err) => Err(EdError::from(ui_err)),
    }
}
