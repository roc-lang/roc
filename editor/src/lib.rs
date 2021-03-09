#![warn(clippy::all, clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

#[cfg_attr(test, macro_use)]
extern crate indoc;
extern crate pest;
#[cfg_attr(test, macro_use)]
extern crate pest_derive;

mod editor;
mod graphics;
pub mod lang; //TODO remove pub for unused warnings
mod ui;
mod window;

use std::io;
use std::path::Path;

pub fn launch(filepaths: &[&'static Path]) -> io::Result<()> {
    editor::main::launch(filepaths)
}
