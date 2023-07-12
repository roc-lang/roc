use std::io;

use snafu::{Backtrace, Snafu};

//import errors as follows:
// `use crate::error::OutOfBounds;`
// *not* `use crate::error::EdError::OutOfBounds;`
// see https://github.com/shepmaster/snafu/issues/211

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum UIError {
    #[snafu(display(
        "LineInsertionFailed: line_nr ({}) needs to be <= nr_of_lines ({}).",
        line_nr,
        nr_of_lines
    ))]
    LineInsertionFailed {
        line_nr: usize,
        nr_of_lines: usize,
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

    #[snafu(display("InvalidSelection: {}.", err_msg))]
    InvalidSelection {
        err_msg: String,
        backtrace: Backtrace,
    },

    #[snafu(display(
        "FileOpenFailed: failed to open file with path {} with the following error: {}.",
        path_str,
        err_msg
    ))]
    FileOpenFailed { path_str: String, err_msg: String },

    #[snafu(display(
        "FileWriteFailed: failed to write to file with path {}, I got this IO error: {}.",
        path_str,
        source
    ))]
    FileWriteFailed { source: io::Error, path_str: String },

    #[snafu(display("TextBufReadFailed: the file {} could be opened but we encountered the following error while trying to read it: {}.", path_str, err_msg))]
    TextBufReadFailed { path_str: String, err_msg: String },

    #[snafu(display("MissingGlyphDims: glyph_dim_rect_opt was None. It needs to be set using the example_code_glyph_rect function."))]
    MissingGlyphDims { backtrace: Backtrace },
}

pub type UIResult<T, E = UIError> = std::result::Result<T, E>;

impl From<UIError> for String {
    fn from(ui_error: UIError) -> Self {
        format!("{ui_error}")
    }
}
