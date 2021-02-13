// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

use crate::ui::ui_error::UIError::{FileOpenFailed, TextBufReadFailed};
use crate::ui::ui_error::UIResult;
use crate::ui::ui_error::OutOfBounds;
use crate::ui::text::text_pos::{TextPos};
use crate::ui::text::selection::{ValidSelection};
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use ropey::Rope;
use snafu::{ensure, OptionExt};
use std::fmt;
use std::fs::File;
use std::io;
use std::path::Path;

pub trait Lines {
    fn get_line(&self, line_nr: usize) -> UIResult<&str>;

    fn line_len(&self, line_nr: usize) -> UIResult<usize>;

    fn nr_of_lines(&self) -> usize;

    fn nr_of_chars(&self) -> usize;

    // TODO use pool allocation here
    fn all_lines<'a>(&self, arena: &'a Bump) -> BumpString<'a>;
}

pub trait SelectableLines {
    fn get_selection(&self) -> UIResult<Option<&str>>;

    fn last_text_pos(&self) -> TextPos;
}

pub trait MutSelectableLines {
    fn insert_char(&mut self, caret_pos: TextPos, new_char: &char) -> UIResult<()>;

    fn insert_str(&mut self, caret_pos: TextPos, new_str: &str) -> UIResult<()>;

    fn pop_char(&mut self, caret_pos: TextPos);

    fn del_selection(&mut self) -> UIResult<()>;
}
