#![warn(clippy::all, clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

extern crate pest;
#[cfg_attr(test, macro_use)]
extern crate pest_derive;
#[cfg_attr(test, macro_use)]
extern crate indoc;

mod graphics;
mod lang;
mod editor;
mod ui;

use std::io;
use std::path::Path;

pub fn launch(filepaths: &[&Path]) -> io::Result<()> {
    editor::main::launch(filepaths)
}