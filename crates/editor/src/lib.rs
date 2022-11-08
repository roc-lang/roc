//! Roc's editor. Work In Progress.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant, clippy::upper_case_acronyms)]

#[cfg_attr(test, macro_use)]
extern crate pest;
#[cfg_attr(test, macro_use)]
extern crate pest_derive;

mod editor;
mod graphics;
mod ui;
mod window;

use std::io;
use std::path::Path;

pub fn launch(project_dir_path_opt: Option<&Path>) -> io::Result<()> {
    editor::main::launch(project_dir_path_opt)
}
