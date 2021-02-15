use colored::*;
use snafu::{Backtrace, ErrorCompat, Snafu};
use crate::ui::ui_error::UIError;

//import errors as follows:
// `use crate::error::OutOfBounds;`
// *not* `use crate::error::EdError::OutOfBounds;`
// see https://github.com/shepmaster/snafu/issues/211

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum EdError {
    #[snafu(display("ClipboardReadFailed: could not get clipboard contents: {}", err_msg))]
    ClipboardReadFailed { err_msg: String },

    #[snafu(display("ClipboardWriteFailed: could not set clipboard contents: {}", err_msg))]
    ClipboardWriteFailed { err_msg: String },

    #[snafu(display(
        "ClipboardInitFailed: could not initialize ClipboardContext: {}.",
        err_msg
    ))]
    ClipboardInitFailed { err_msg: String },

    #[snafu(display("InvalidSelection: {}.", err_msg))]
    InvalidSelection {
        err_msg: String,
        backtrace: Backtrace,
    },

    #[snafu(display("MissingGlyphDims: glyph_dim_rect_opt was None for model. It needs to be set using the example_code_glyph_rect function."))]
    MissingGlyphDims { backtrace: Backtrace },

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

impl From<UIError> for EdError {
    fn from(ui_error: UIError) -> Self {
        //TODO
    }
}
