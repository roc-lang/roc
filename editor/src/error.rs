use snafu::{Backtrace, Snafu, ErrorCompat};
use colored::*;

//import errors as follows:
// `use crate::error::OutOfBounds;`
// *not* `use crate::error::EdError::OutOfBounds;`
// see https://github.com/shepmaster/snafu/issues/211

#[derive(Debug, Snafu)]
#[snafu(visibility(pub))]
pub enum EdError {
    #[snafu(display(
        "OutOfBounds: index {} was out of bounds for Vec with length {}.",
        index,
        vec_len
    ))]
    OutOfBounds { 
        index: usize,
        vec_len: usize,
        backtrace: Backtrace 
    },
    #[snafu(display(
        "InvalidSelection: {}",
        err_msg
    ))]
    InvalidSelection { 
        err_msg: String,
        backtrace: Backtrace 
    },
}

pub type EdResult<T, E = EdError> = std::result::Result<T, E>;

pub fn print_err(err: &EdError) {
    eprintln!("{}", format!("{}", err).truecolor(255, 0, 0));

    if let Some(backtrace) = ErrorCompat::backtrace(err) {
        eprintln!("{}", color_backtrace(backtrace));
    }
}

fn color_backtrace(backtrace: &snafu::Backtrace) -> String {
    let backtrace_str = format!("{}", backtrace);
    let backtrace_split = backtrace_str.split("\n");
    let irrelevant_src = vec![".cargo", "registry", ".rustup", "rustc"];

    let mut ret_str = String::new();
    let mut prev_line_opt: Option<String> = None;

    for line in backtrace_split {

        let new_line = 
            if line.contains("src") {
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

