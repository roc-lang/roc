// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

use crate::ui::text::{
    selection::{RawSelection, Selection},
    text_pos::TextPos,
};
use crate::ui::ui_error::UIResult;
use crate::window::keyboard_input::Modifiers;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use winit::event::{VirtualKeyCode};

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

    fn move_caret_left(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_right(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_up(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_down(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_home(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn move_caret_end(&mut self, modifiers: &Modifiers) -> UIResult<()>;

    fn get_selection(&self) -> Option<Selection>;

    fn is_selection_active(&self) -> bool;

    fn get_selected_str(&self) -> UIResult<Option<&str>>;

    fn set_raw_sel(&mut self, raw_sel: RawSelection) -> UIResult<()>;

    fn set_sel_none(&mut self);

    fn select_all(&mut self) -> UIResult<()>;

    fn last_text_pos(&self) -> TextPos;

    fn handle_key_down(
        &mut self,
        modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
    ) -> UIResult<()>;
}

pub trait MutSelectableLines {
    fn insert_char(&mut self, new_char: &char) -> UIResult<()>;

    // could be for insertion, backspace, del...
    fn handle_new_char(&mut self, received_char: &char) -> UIResult<()>;

    fn insert_str(&mut self, new_str: &str) -> UIResult<()>;

    fn pop_char(&mut self) -> UIResult<()>;

    fn del_selection(&mut self) -> UIResult<()>;
}
