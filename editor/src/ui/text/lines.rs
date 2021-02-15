// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

use crate::ui::ui_error::{
    UIResult,
};
use crate::ui::text::{
    text_pos::{TextPos},
    selection::{Selection, RawSelection},
};
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;

pub trait Lines {
    fn get_line(&self, line_nr: usize) -> UIResult<&str>;

    fn line_len(&self, line_nr: usize) -> UIResult<usize>;

    fn nr_of_lines(&self) -> usize;

    fn nr_of_chars(&self) -> usize;

    // TODO use pool allocation here
    fn all_lines<'a>(&self, arena: &'a Bump) -> BumpString<'a>;
}

pub trait SelectableLines {
    fn get_caret(self) -> TextPos;

    fn set_caret(&mut self, caret_pos: TextPos);

    fn get_selection(&self) -> Option<Selection>;

    fn get_selected_str(&self) -> UIResult<Option<&str>>;

    fn set_raw_sel(&mut self, raw_sel: RawSelection) -> UIResult<()>;

    fn last_text_pos(&self) -> TextPos;
}

pub trait MutSelectableLines {
    fn insert_char(&mut self, caret_pos: TextPos, new_char: &char) -> UIResult<()>;

    fn insert_str(&mut self, caret_pos: TextPos, new_str: &str) -> UIResult<()>;

    fn pop_char(&mut self, caret_pos: TextPos);

    fn del_selection(&mut self) -> UIResult<()>;
}
