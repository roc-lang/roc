// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

use crate::ui::ui_error::{
    UIResult,
};
use crate::ui::text::{
    text_pos::{TextPos},
    selection::{Selection, RawSelection},
    caret_w_select::CaretWSelect,
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

    fn move_caret_left(&mut self, shift_pressed: bool) -> UIResult<CaretWSelect>;

    fn move_caret_right(&mut self, shift_pressed: bool) -> UIResult<CaretWSelect>;

    fn move_caret_up(&mut self, shift_pressed: bool) -> UIResult<CaretWSelect>;

    fn move_caret_down(&mut self, shift_pressed: bool) -> UIResult<CaretWSelect>;

    fn get_selection(&self) -> Option<Selection>;

    fn is_selection_active(&self) -> bool;

    fn get_selected_str(&self) -> UIResult<Option<&str>>;

    fn set_raw_sel(&mut self, raw_sel: RawSelection) -> UIResult<()>;

    fn set_sel_none(&mut self);

    fn last_text_pos(&self) -> TextPos;
}

pub trait MutSelectableLines {
    fn insert_char(&mut self, new_char: &char) -> UIResult<()>;

    fn insert_str(&mut self, new_str: &str) -> UIResult<()>;

    fn pop_char(&mut self);

    fn del_selection(&mut self) -> UIResult<()>;
}
