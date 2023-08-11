// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

#![allow(dead_code)]

use crate::ui::text::{
    caret_w_select::CaretWSelect,
    lines,
    lines::{Lines, MutSelectableLines, SelectableLines},
    selection::{validate_raw_sel, RawSelection, Selection},
    text_pos::TextPos,
};
use crate::ui::ui_error::{OutOfBoundsSnafu, UIResult};
use crate::ui::util::is_newline;
use crate::window::keyboard_input::{no_mods, Modifiers};
use bumpalo::Bump;
use snafu::ensure;
use std::{fmt, path::Path};
use winit::event::{VirtualKeyCode, VirtualKeyCode::*};

use super::text_buffer::TextBuffer;

pub struct BigTextArea {
    pub caret_w_select: CaretWSelect,
    text_buffer: TextBuffer,
    pub path_str: String,
    arena: Bump,
}

impl BigTextArea {
    fn check_bounds(&self, char_indx: usize) -> UIResult<()> {
        ensure!(
            char_indx <= self.text_buffer.nr_of_chars(),
            OutOfBoundsSnafu {
                index: char_indx,
                collection_name: "TextBuffer",
                len: self.text_buffer.nr_of_chars()
            }
        );

        Ok(())
    }
}

impl Lines for BigTextArea {
    fn get_line_ref(&self, line_nr: usize) -> UIResult<&str> {
        self.text_buffer.get_line_ref(line_nr)
    }

    fn line_len(&self, line_nr: usize) -> UIResult<usize> {
        self.get_line_ref(line_nr).map(|line| line.len())
    }

    fn nr_of_lines(&self) -> usize {
        self.text_buffer.nr_of_lines()
    }

    fn nr_of_chars(&self) -> usize {
        self.text_buffer.nr_of_chars()
    }

    fn all_lines_as_string(&self) -> String {
        self.text_buffer.all_lines_ref().join("\n")
    }

    fn is_last_line(&self, line_nr: usize) -> bool {
        line_nr == self.nr_of_lines() - 1
    }

    fn last_char(&self, line_nr: usize) -> UIResult<Option<char>> {
        Ok(self.get_line_ref(line_nr)?.chars().last())
    }
}

impl SelectableLines for BigTextArea {
    fn get_caret(&self) -> TextPos {
        self.caret_w_select.caret_pos
    }

    fn set_caret(&mut self, caret_pos: TextPos) {
        self.caret_w_select.caret_pos = caret_pos;
    }

    fn move_caret_left(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        self.caret_w_select = lines::move_caret_left(self, self.caret_w_select, modifiers)?;

        Ok(())
    }

    fn move_caret_right(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        self.caret_w_select = lines::move_caret_right(self, self.caret_w_select, modifiers)?;

        Ok(())
    }

    fn move_caret_up(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        self.caret_w_select = lines::move_caret_up(self, self.caret_w_select, modifiers)?;

        Ok(())
    }

    fn move_caret_down(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        self.caret_w_select = lines::move_caret_down(self, self.caret_w_select, modifiers)?;

        Ok(())
    }

    fn move_caret_home(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        self.caret_w_select = lines::move_caret_home(self, self.caret_w_select, modifiers)?;

        Ok(())
    }

    fn move_caret_end(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        self.caret_w_select = lines::move_caret_end(self, self.caret_w_select, modifiers)?;

        Ok(())
    }

    fn get_selection(&self) -> Option<Selection> {
        self.caret_w_select.selection_opt
    }

    fn is_selection_active(&self) -> bool {
        self.get_selection().is_some()
    }

    fn get_selected_str(&self) -> UIResult<Option<String>> {
        if let Some(val_sel) = self.caret_w_select.selection_opt {
            Ok(Some(self.text_buffer.get_selected_str(val_sel)?))
        } else {
            Ok(None)
        }
    }

    fn set_raw_sel(&mut self, raw_sel: RawSelection) -> UIResult<()> {
        self.caret_w_select.selection_opt = Some(validate_raw_sel(raw_sel)?);

        Ok(())
    }

    fn set_caret_w_sel(&mut self, caret_w_sel: CaretWSelect) {
        self.caret_w_select = caret_w_sel;
    }

    fn set_sel_none(&mut self) {
        self.caret_w_select.selection_opt = None;
    }

    fn select_all(&mut self) -> UIResult<()> {
        if self.nr_of_chars() > 0 {
            let last_pos = self.last_text_pos()?;

            self.set_raw_sel(RawSelection {
                start_pos: TextPos { line: 0, column: 0 },
                end_pos: last_pos,
            })?;

            self.set_caret(last_pos);
        }

        Ok(())
    }

    fn last_text_pos(&self) -> UIResult<TextPos> {
        let line_nr = self.nr_of_lines() - 1;
        let last_col = self.line_len(line_nr)?;

        Ok(TextPos {
            line: line_nr,
            column: last_col,
        })
    }

    fn handle_key_down(
        &mut self,
        modifiers: &Modifiers,
        virtual_keycode: VirtualKeyCode,
    ) -> UIResult<()> {
        match virtual_keycode {
            Left => self.move_caret_left(modifiers),
            Up => self.move_caret_up(modifiers),
            Right => self.move_caret_right(modifiers),
            Down => self.move_caret_down(modifiers),

            A => {
                if modifiers.cmd_or_ctrl() {
                    self.select_all()
                } else {
                    Ok(())
                }
            }
            Home => self.move_caret_home(modifiers),
            End => self.move_caret_end(modifiers),
            _ => Ok(()),
        }
    }
}

impl MutSelectableLines for BigTextArea {
    fn insert_char(&mut self, new_char: &char) -> UIResult<()> {
        if self.is_selection_active() {
            self.del_selection()?;
        }

        self.insert_str(&new_char.to_string())?;

        if is_newline(new_char) {
            self.set_caret(TextPos {
                line: self.caret_w_select.caret_pos.line + 1,
                column: 0,
            });
        } else {
            self.move_caret_right(&no_mods())?;
        }

        self.set_sel_none();

        Ok(())
    }

    fn handle_new_char(&mut self, received_char: &char) -> UIResult<()> {
        match received_char {
            '\u{8}' | '\u{7f}' => {
                // On Linux, '\u{8}' is backspace,
                // on macOS '\u{7f}'.

                self.backspace()?
            }

            '\u{1}' // Ctrl + A
            | '\u{3}' // Ctrl + C
            | '\u{16}' // Ctrl + V
            | '\u{18}' // Ctrl + X
            | '\u{e000}'..='\u{f8ff}' // http://www.unicode.org/faq/private_use.html
            | '\u{f0000}'..='\u{ffffd}' // ^
            | '\u{100000}'..='\u{10fffd}' // ^
            => {
                // chars that can be ignored
            }

            _ => {
                self.insert_char(received_char)?;
            }
        }

        Ok(())
    }

    fn insert_str(&mut self, new_str: &str) -> UIResult<()> {
        let caret_pos = self.caret_w_select.caret_pos;

        self.text_buffer.insert_str(caret_pos, new_str)?;

        Ok(())
    }

    fn backspace(&mut self) -> UIResult<()> {
        if self.is_selection_active() {
            self.del_selection()?;
        } else {
            let old_caret_pos = self.caret_w_select.caret_pos;

            self.move_caret_left(&no_mods())?;

            self.text_buffer.backspace_char(old_caret_pos)?;
        }

        Ok(())
    }

    fn del_selection(&mut self) -> UIResult<()> {
        if let Some(selection) = self.caret_w_select.selection_opt {
            self.text_buffer.del_selection(selection)?;

            self.set_caret(selection.start_pos);

            self.set_sel_none();
        }

        Ok(())
    }
}

impl Default for BigTextArea {
    fn default() -> Self {
        let caret_w_select = CaretWSelect::default();
        let text_buffer = TextBuffer { lines: Vec::new() };
        let path_str = "".to_owned();
        let arena = Bump::new();

        Self {
            caret_w_select,
            text_buffer,
            path_str,
            arena,
        }
    }
}

pub fn from_path(path: &Path) -> UIResult<BigTextArea> {
    let text_buffer = TextBuffer::from_path(path)?;
    let path_str = path_to_string(path);

    Ok(BigTextArea {
        text_buffer,
        path_str,
        ..Default::default()
    })
}

#[allow(dead_code)]
// used by tests but will also be used in the future
pub fn from_str_vec(lines: Vec<String>) -> BigTextArea {
    BigTextArea {
        text_buffer: TextBuffer { lines },
        ..Default::default()
    }
}

fn path_to_string(path: &Path) -> String {
    let mut path_str = String::new();
    path_str.push_str(&path.to_string_lossy());

    path_str
}

// need to explicitly omit arena
impl fmt::Debug for BigTextArea {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BigTextArea")
            .field("caret_w_select", &self.caret_w_select)
            .field("text_buffer", &self.text_buffer)
            .field("path_str", &self.path_str)
            .finish()
    }
}

#[cfg(test)]
pub mod test_big_sel_text {
    use crate::ui::text::caret_w_select::test_caret_w_select::convert_dsl_to_selection;
    use crate::ui::text::caret_w_select::test_caret_w_select::convert_selection_to_dsl;
    use crate::ui::text::{
        big_text_area::BigTextArea,
        lines::{Lines, MutSelectableLines, SelectableLines},
        text_pos::TextPos,
    };
    use crate::ui::ui_error::{OutOfBoundsSnafu, UIResult};
    use crate::window::keyboard_input::{no_mods, Modifiers};
    use snafu::OptionExt;
    use std::slice::SliceIndex;

    use super::from_str_vec;

    fn shift_pressed() -> Modifiers {
        Modifiers {
            shift: true,
            ..Default::default()
        }
    }

    fn insert_at_pos(lines: &mut [String], pos: TextPos, insert_char: char) -> UIResult<()> {
        let line = get_mut_res(pos.line, lines)?;
        line.insert(pos.column, insert_char);

        Ok(())
    }

    // It's much nicer to have get_mut return a Result with clear error than an Option
    fn get_mut_res<T>(
        index: usize,
        vec: &mut [T],
    ) -> UIResult<&mut <usize as SliceIndex<[T]>>::Output> {
        let vec_len = vec.len();

        let elt_ref = vec.get_mut(index).context(OutOfBoundsSnafu {
            index,
            collection_name: "Slice",
            len: vec_len,
        })?;

        Ok(elt_ref)
    }

    pub fn big_text_from_dsl_str(lines: &[String]) -> BigTextArea {
        from_str_vec(
            lines
                .iter()
                .map(|line| line.replace(&['❮', '❯', '┃'][..], ""))
                .collect::<Vec<String>>(),
        )
    }

    pub fn all_lines_vec(big_sel_text: &BigTextArea) -> Vec<String> {
        let mut lines: Vec<String> = Vec::new();

        for i in 0..big_sel_text.nr_of_lines() {
            lines.push(big_sel_text.get_line_ref(i).unwrap().to_string());
        }

        lines
    }

    pub fn gen_big_text(lines: &[&str]) -> Result<BigTextArea, String> {
        let lines_string_slice: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
        let mut big_text = big_text_from_dsl_str(&lines_string_slice);
        let caret_w_select = convert_dsl_to_selection(&lines_string_slice).unwrap();

        big_text.caret_w_select = caret_w_select;

        Ok(big_text)
    }

    fn assert_insert(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
        new_char: char,
    ) -> Result<(), String> {
        let mut big_text = gen_big_text(pre_lines_str)?;

        if let Err(e) = big_text.handle_new_char(&new_char) {
            return Err(e.to_string());
        }

        let actual_lines = all_lines_vec(&big_text);
        let dsl_slice = convert_selection_to_dsl(big_text.caret_w_select, actual_lines).unwrap();
        assert_eq!(dsl_slice, expected_post_lines_str);

        Ok(())
    }

    #[test]
    fn insert_new_char_simple() -> Result<(), String> {
        assert_insert(&["┃"], &["a┃"], 'a')?;
        assert_insert(&["┃"], &[" ┃"], ' ')?;
        assert_insert(&["a┃"], &["aa┃"], 'a')?;
        assert_insert(&["a┃"], &["a ┃"], ' ')?;
        assert_insert(&["a┃", ""], &["ab┃", ""], 'b')?;
        assert_insert(&["a", "┃"], &["a", "b┃"], 'b')?;
        assert_insert(&["a", "b", "c┃"], &["a", "b", "cd┃"], 'd')?;

        Ok(())
    }

    #[test]
    fn insert_new_char_mid() -> Result<(), String> {
        assert_insert(&["ab┃d"], &["abc┃d"], 'c')?;
        assert_insert(&["a┃cd"], &["ab┃cd"], 'b')?;
        assert_insert(&["abc", "┃e"], &["abc", "d┃e"], 'd')?;
        assert_insert(&["abc", "def", "┃ "], &["abc", "def", "g┃ "], 'g')?;
        assert_insert(&["abc", "def", "┃ "], &["abc", "def", " ┃ "], ' ')?;

        Ok(())
    }

    #[test]
    fn simple_backspace() -> Result<(), String> {
        assert_insert(&["┃"], &["┃"], '\u{8}')?;
        assert_insert(&[" ┃"], &["┃"], '\u{8}')?;
        assert_insert(&["a┃"], &["┃"], '\u{8}')?;
        assert_insert(&["ab┃"], &["a┃"], '\u{8}')?;
        assert_insert(&["a┃", ""], &["┃", ""], '\u{8}')?;
        assert_insert(&["ab┃", ""], &["a┃", ""], '\u{8}')?;
        assert_insert(&["a", "┃"], &["a┃"], '\u{8}')?;
        assert_insert(&["a", "b", "c┃"], &["a", "b", "┃"], '\u{8}')?;
        assert_insert(&["a", "b", "┃"], &["a", "b┃"], '\u{8}')?;

        Ok(())
    }

    #[test]
    fn selection_backspace() -> Result<(), String> {
        assert_insert(&["❮a❯┃"], &["┃"], '\u{8}')?;
        assert_insert(&["a❮a❯┃"], &["a┃"], '\u{8}')?;
        assert_insert(&["❮aa❯┃"], &["┃"], '\u{8}')?;
        assert_insert(&["a❮b c❯┃"], &["a┃"], '\u{8}')?;
        assert_insert(&["❮abc❯┃", ""], &["┃", ""], '\u{8}')?;
        assert_insert(&["a", "❮abc❯┃"], &["a", "┃"], '\u{8}')?;
        assert_insert(&["❮a", "abc❯┃"], &["┃"], '\u{8}')?;
        assert_insert(&["a❮b", "cdef ghij❯┃"], &["a┃"], '\u{8}')?;
        assert_insert(&["❮a", "b", "c❯┃"], &["┃"], '\u{8}')?;
        assert_insert(&["a", "❮b", "❯┃"], &["a", "┃"], '\u{8}')?;
        assert_insert(
            &["abc", "d❮ef", "ghi❯┃", "jkl"],
            &["abc", "d┃", "jkl"],
            '\u{8}',
        )?;
        assert_insert(
            &["abc", "❮def", "ghi❯┃", "jkl"],
            &["abc", "┃", "jkl"],
            '\u{8}',
        )?;
        assert_insert(
            &["abc", "", "❮def", "ghi❯┃", "jkl"],
            &["abc", "", "┃", "jkl"],
            '\u{8}',
        )?;
        assert_insert(&["❮abc", "", "def", "ghi", "jkl❯┃"], &["┃"], '\u{8}')?;

        Ok(())
    }

    #[test]
    fn insert_with_selection() -> Result<(), String> {
        assert_insert(&["❮a❯┃"], &["z┃"], 'z')?;
        assert_insert(&["a❮a❯┃"], &["az┃"], 'z')?;
        assert_insert(&["❮aa❯┃"], &["z┃"], 'z')?;
        assert_insert(&["a❮b c❯┃"], &["az┃"], 'z')?;
        assert_insert(&["❮abc❯┃", ""], &["z┃", ""], 'z')?;
        assert_insert(&["a", "❮abc❯┃"], &["a", "z┃"], 'z')?;
        assert_insert(&["❮a", "abc❯┃"], &["z┃"], 'z')?;
        assert_insert(&["a❮b", "cdef ghij❯┃"], &["az┃"], 'z')?;
        assert_insert(&["❮a", "b", "c❯┃"], &["z┃"], 'z')?;
        assert_insert(&["a", "❮b", "❯┃"], &["a", "z┃"], 'z')?;
        assert_insert(
            &["abc", "d❮ef", "ghi❯┃", "jkl"],
            &["abc", "dz┃", "jkl"],
            'z',
        )?;
        assert_insert(&["abc", "❮def", "ghi❯┃", "jkl"], &["abc", "z┃", "jkl"], 'z')?;
        assert_insert(
            &["abc", "", "❮def", "ghi❯┃", "jkl"],
            &["abc", "", "z┃", "jkl"],
            'z',
        )?;
        assert_insert(&["❮abc", "", "def", "ghi", "jkl❯┃"], &["z┃"], 'z')?;

        Ok(())
    }

    fn assert_select_all(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
    ) -> Result<(), String> {
        let mut big_text = gen_big_text(pre_lines_str)?;

        big_text.select_all().unwrap();

        let big_text_lines = all_lines_vec(&big_text);
        let post_lines_str = convert_selection_to_dsl(big_text.caret_w_select, big_text_lines)?;

        assert_eq!(post_lines_str, expected_post_lines_str);

        Ok(())
    }

    #[test]
    fn select_all() -> Result<(), String> {
        assert_select_all(&["┃"], &["┃"])?;
        assert_select_all(&["┃a"], &["❮a❯┃"])?;
        assert_select_all(&["a┃"], &["❮a❯┃"])?;
        assert_select_all(&["abc d┃ef ghi"], &["❮abc def ghi❯┃"])?;
        assert_select_all(&["❮a❯┃"], &["❮a❯┃"])?;
        assert_select_all(&["┃❮a❯"], &["❮a❯┃"])?;
        assert_select_all(&["┃❮abc def ghi❯"], &["❮abc def ghi❯┃"])?;
        assert_select_all(&["a", "❮b", "❯┃"], &["❮a", "b", "❯┃"])?;
        assert_select_all(&["a", "❮b❯┃", ""], &["❮a", "b", "❯┃"])?;
        assert_select_all(&["a", "┃❮b", "❯"], &["❮a", "b", "❯┃"])?;
        assert_select_all(
            &["abc", "def", "gh┃i", "jkl"],
            &["❮abc", "def", "ghi", "jkl❯┃"],
        )?;
        assert_select_all(
            &["┃❮abc", "def", "ghi", "jkl❯"],
            &["❮abc", "def", "ghi", "jkl❯┃"],
        )?;

        Ok(())
    }

    type MoveCaretFun = fn(&mut BigTextArea, &Modifiers) -> UIResult<()>;

    // Convert nice string representations and compare results
    fn assert_move(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
        modifiers: &Modifiers,
        move_fun: MoveCaretFun,
    ) -> Result<(), String> {
        let expected_post_lines: Vec<String> = expected_post_lines_str
            .iter()
            .map(|l| l.to_string())
            .collect();

        let mut big_text = gen_big_text(pre_lines_str)?;

        move_fun(&mut big_text, modifiers)?;

        let lines_vec = all_lines_vec(&big_text)
            .iter()
            .map(|l| l.replace('\n', ""))
            .collect();
        let post_lines_res = convert_selection_to_dsl(big_text.caret_w_select, lines_vec);

        match post_lines_res {
            Ok(post_lines) => {
                assert_eq!(expected_post_lines, post_lines);
                Ok(())
            }
            Err(e) => Err(format!("{e:?}")),
        }
    }

    #[test]
    fn move_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["┃"], &["┃"], &no_mods(), move_caret_right)?;
        assert_move(&["a┃"], &["a┃"], &no_mods(), move_caret_right)?;
        assert_move(&["┃A"], &["A┃"], &no_mods(), move_caret_right)?;
        assert_move(&["┃abc"], &["a┃bc"], &no_mods(), move_caret_right)?;
        assert_move(&["a┃bc"], &["ab┃c"], &no_mods(), move_caret_right)?;
        assert_move(&["abc┃"], &["abc┃"], &no_mods(), move_caret_right)?;
        assert_move(&["┃ abc"], &[" ┃abc"], &no_mods(), move_caret_right)?;
        assert_move(&["abc┃ "], &["abc ┃"], &no_mods(), move_caret_right)?;
        assert_move(&["abc┃", "d"], &["abc", "┃d"], &no_mods(), move_caret_right)?;
        assert_move(&["abc┃", ""], &["abc", "┃"], &no_mods(), move_caret_right)?;
        assert_move(
            &["abc", "┃def"],
            &["abc", "d┃ef"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def┃ "],
            &["abc", "def ┃"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def ┃", "ghi"],
            &["abc", "def ", "┃ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def┃", ""],
            &["abc", "def", "┃"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def", "ghi┃", "jkl"],
            &["abc", "def", "ghi", "┃jkl"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def", "┃ghi", "jkl"],
            &["abc", "def", "g┃hi", "jkl"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def", "g┃hi", "jkl"],
            &["abc", "def", "gh┃i", "jkl"],
            &no_mods(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn move_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["┃"], &["┃"], &no_mods(), move_caret_left)?;
        assert_move(&["┃a"], &["┃a"], &no_mods(), move_caret_left)?;
        assert_move(&["┃A"], &["┃A"], &no_mods(), move_caret_left)?;
        assert_move(&["a┃bc"], &["┃abc"], &no_mods(), move_caret_left)?;
        assert_move(&["ab┃c"], &["a┃bc"], &no_mods(), move_caret_left)?;
        assert_move(&["abc┃"], &["ab┃c"], &no_mods(), move_caret_left)?;
        assert_move(&[" ┃abc"], &["┃ abc"], &no_mods(), move_caret_left)?;
        assert_move(&["abc ┃"], &["abc┃ "], &no_mods(), move_caret_left)?;
        assert_move(&["abc", "┃d"], &["abc┃", "d"], &no_mods(), move_caret_left)?;
        assert_move(&["abc", "┃"], &["abc┃", ""], &no_mods(), move_caret_left)?;
        assert_move(
            &["abc", "d┃ef"],
            &["abc", "┃def"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def ┃"],
            &["abc", "def┃ "],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def ", "┃ghi"],
            &["abc", "def ┃", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "┃"],
            &["abc", "def┃", ""],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "ghi", "┃jkl"],
            &["abc", "def", "ghi┃", "jkl"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "g┃hi", "jkl"],
            &["abc", "def", "┃ghi", "jkl"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "gh┃i", "jkl"],
            &["abc", "def", "g┃hi", "jkl"],
            &no_mods(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn move_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["┃"], &["┃"], &no_mods(), move_caret_up)?;
        assert_move(&["┃a"], &["┃a"], &no_mods(), move_caret_up)?;
        assert_move(&["A┃"], &["┃A"], &no_mods(), move_caret_up)?;
        assert_move(&["a┃bc"], &["┃abc"], &no_mods(), move_caret_up)?;
        assert_move(&["ab┃c"], &["┃abc"], &no_mods(), move_caret_up)?;
        assert_move(&["abc┃"], &["┃abc"], &no_mods(), move_caret_up)?;
        assert_move(
            &["┃abc", "def"],
            &["┃abc", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "┃def"],
            &["┃abc", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "d┃ef"],
            &["a┃bc", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de┃f"],
            &["ab┃c", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def┃"],
            &["abc┃", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ", "┃ghi"],
            &["abc", "┃def ", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ", "g┃hi"],
            &["abc", "d┃ef ", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ", "gh┃i"],
            &["abc", "de┃f ", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ", "ghi┃"],
            &["abc", "def┃ ", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de", "ghi┃"],
            &["abc", "de┃", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(&["abc", "de┃"], &["ab┃c", "de"], &no_mods(), move_caret_up)?;
        assert_move(&["abc", "d┃e"], &["a┃bc", "de"], &no_mods(), move_caret_up)?;
        assert_move(&["abc", "┃de"], &["┃abc", "de"], &no_mods(), move_caret_up)?;
        assert_move(
            &["ab", "cdef", "ghijkl", "mnopqrst┃"],
            &["ab", "cdef", "ghijkl┃", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl┃", "mnopqrst"],
            &["ab", "cdef┃", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "┃mnopqrst"],
            &["ab", "cdef", "┃ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &[" ab", " ┃cdef", "ghijkl", "mnopqrst"],
            &[" ┃ab", " cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "mnopqr┃st"],
            &["ab", "cdef", "ghijkl┃", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cde┃f", "ghijkl", "mnopqrst"],
            &["ab┃", "cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr", "st┃"],
            &["abcdefgh", "ijklmn", "op┃qr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr┃", "st"],
            &["abcdefgh", "ijkl┃mn", "opqr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn┃", "opqr", "st"],
            &["abcdef┃gh", "ijklmn", "opqr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh┃", "ijklmn", "opqr", "st"],
            &["┃abcdefgh", "ijklmn", "opqr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefg┃h", "ijklmn", "opqr", "st"],
            &["┃abcdefgh", "ijklmn", "opqr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a┃bcdefgh", "ijklmn", "opqr", "st"],
            &["┃abcdefgh", "ijklmn", "opqr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["┃abcdefgh", "ijklmn", "opqr", "st"],
            &["┃abcdefgh", "ijklmn", "opqr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc def gh ┃"],
            &["┃abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc de┃f gh "],
            &["┃abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab┃c def gh "],
            &["┃abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a┃bc def gh "],
            &["┃abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn move_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["┃"], &["┃"], &no_mods(), move_caret_down)?;
        assert_move(&["┃a"], &["a┃"], &no_mods(), move_caret_down)?;
        assert_move(&["A┃"], &["A┃"], &no_mods(), move_caret_down)?;
        assert_move(&["a┃bc"], &["abc┃"], &no_mods(), move_caret_down)?;
        assert_move(&["ab┃c"], &["abc┃"], &no_mods(), move_caret_down)?;
        assert_move(&["abc┃"], &["abc┃"], &no_mods(), move_caret_down)?;
        assert_move(&["abc┃ "], &["abc ┃"], &no_mods(), move_caret_down)?;
        assert_move(
            &["abc", "┃def"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "d┃ef"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de┃f"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "def┃"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["┃abc", "def"],
            &["abc", "┃def"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a┃bc", "def"],
            &["abc", "d┃ef"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃c", "def"],
            &["abc", "de┃f"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc┃", "def"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "┃def ", "ghi"],
            &["abc", "def ", "┃ghi"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "d┃ef ", "ghi"],
            &["abc", "def ", "g┃hi"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de┃f ", "ghi"],
            &["abc", "def ", "gh┃i"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "def┃ ", "ghi"],
            &["abc", "def ", "ghi┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "def ┃", "ghi"],
            &["abc", "def ", "ghi┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de┃", "ghi"],
            &["abc", "de", "gh┃i"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc┃", "de"],
            &["abc", "de┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃c", "de"],
            &["abc", "de┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a┃bc", "de"],
            &["abc", "d┃e"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["┃abc", "de"],
            &["abc", "┃de"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃", "cdef", "ghijkl", "mnopqrst"],
            &["ab", "cd┃ef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef┃", "ghijkl", "mnopqrst"],
            &["ab", "cdef", "ghij┃kl", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl┃", "mnopqrst"],
            &["ab", "cdef", "ghijkl", "mnopqr┃st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &[" ┃ab", " cdef", "ghijkl", "mnopqrst"],
            &[" ab", " ┃cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "┃cdef", "ghijkl", "mnopqrst"],
            &["ab", "cdef", "┃ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef", "┃ghijkl", "mnopqrst"],
            &["ab", "cdef", "ghijkl", "┃mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh┃", "ijklmn", "opqr", "st"],
            &["abcdefgh", "ijklmn┃", "opqr", "st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn┃", "opqr", "st"],
            &["abcdefgh", "ijklmn", "opqr┃", "st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr┃", "st"],
            &["abcdefgh", "ijklmn", "opqr", "st┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr", "┃st"],
            &["abcdefgh", "ijklmn", "opqr", "st┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc def gh ┃"],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc de┃f gh "],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃c def gh "],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a┃bc def gh "],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["┃abc def gh "],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn move_home() -> Result<(), String> {
        let move_caret_home = BigTextArea::move_caret_home;
        assert_move(&["┃"], &["┃"], &no_mods(), move_caret_home)?;
        assert_move(&["a┃"], &["┃a"], &no_mods(), move_caret_home)?;
        assert_move(&["┃a"], &["┃a"], &no_mods(), move_caret_home)?;
        assert_move(&[" ┃a"], &["┃ a"], &no_mods(), move_caret_home)?;
        assert_move(&["┃ a"], &[" ┃a"], &no_mods(), move_caret_home)?;
        assert_move(&[" a┃"], &[" ┃a"], &no_mods(), move_caret_home)?;
        assert_move(&[" abc ┃"], &[" ┃abc "], &no_mods(), move_caret_home)?;
        assert_move(&["\tabc ┃"], &["\t┃abc "], &no_mods(), move_caret_home)?;
        assert_move(&["\t┃abc "], &["┃\tabc "], &no_mods(), move_caret_home)?;
        assert_move(&["┃\tabc "], &["\t┃abc "], &no_mods(), move_caret_home)?;
        assert_move(
            &[" abc def\tghi┃"],
            &[" ┃abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &[" ┃abc def\tghi"],
            &["┃ abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["┃ abc def\tghi"],
            &[" ┃abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;

        assert_move(
            &["abc", "de┃", "ghi"],
            &["abc", "┃de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", " d┃e", "ghi"],
            &["abc", " ┃de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "┃ de", "ghi"],
            &["abc", " ┃de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", " ┃de", "ghi"],
            &["abc", "┃ de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc┃", "de", "ghi"],
            &["┃abc", "de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &[" ┃abc", "de", "ghi"],
            &["┃ abc", "de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["┃ abc", "de", "ghi"],
            &[" ┃abc", "de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "de", "ghi┃"],
            &["abc", "de", "┃ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "de", " ┃ghi"],
            &["abc", "de", "┃ ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "de", "┃ ghi"],
            &["abc", "de", " ┃ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc ", "de ", "┃ghi "],
            &["abc ", "de ", "┃ghi "],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc ", "┃de ", "ghi "],
            &["abc ", "┃de ", "ghi "],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["┃abc ", "de ", "ghi "],
            &["┃abc ", "de ", "ghi "],
            &no_mods(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn move_end() -> Result<(), String> {
        let move_caret_end = BigTextArea::move_caret_end;
        assert_move(&["┃"], &["┃"], &no_mods(), move_caret_end)?;
        assert_move(&["┃a"], &["a┃"], &no_mods(), move_caret_end)?;
        assert_move(&["a┃"], &["a┃"], &no_mods(), move_caret_end)?;
        assert_move(&[" a┃ "], &[" a ┃"], &no_mods(), move_caret_end)?;
        assert_move(&["┃ abc "], &[" abc ┃"], &no_mods(), move_caret_end)?;
        assert_move(&["┃\tabc "], &["\tabc ┃"], &no_mods(), move_caret_end)?;
        assert_move(
            &[" abc d┃ef\tghi"],
            &[" abc def\tghi┃"],
            &no_mods(),
            move_caret_end,
        )?;

        assert_move(
            &["abc", "┃de", "ghi"],
            &["abc", "de┃", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc", " d┃e", "ghi"],
            &["abc", " de┃", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["┃abc", "de", "ghi"],
            &["abc┃", "de", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc", "de", "g┃hi"],
            &["abc", "de", "ghi┃"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc ", "de ", "ghi┃ "],
            &["abc ", "de ", "ghi ┃"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc ", "┃de ", "ghi "],
            &["abc ", "de ┃", "ghi "],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc ┃", "de ", "ghi "],
            &["abc ┃", "de ", "ghi "],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc ", "de ┃", "ghi "],
            &["abc ", "de ┃", "ghi "],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc ", "de ", "ghi ┃"],
            &["abc ", "de ", "ghi ┃"],
            &no_mods(),
            move_caret_end,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["┃"], &["┃"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a┃"], &["a┃"], &shift_pressed(), move_caret_right)?;
        assert_move(&["┃A"], &["❮A❯┃"], &shift_pressed(), move_caret_right)?;
        assert_move(&["┃abc"], &["❮a❯┃bc"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a┃bc"], &["a❮b❯┃c"], &shift_pressed(), move_caret_right)?;
        assert_move(&["abc┃"], &["abc┃"], &shift_pressed(), move_caret_right)?;
        assert_move(&["┃ abc"], &["❮ ❯┃abc"], &shift_pressed(), move_caret_right)?;
        assert_move(&["abc┃ "], &["abc❮ ❯┃"], &shift_pressed(), move_caret_right)?;
        assert_move(
            &["abc┃", "d"],
            &["abc❮", "❯┃d"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc┃", ""],
            &["abc❮", "❯┃"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "┃def"],
            &["abc", "❮d❯┃ef"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def┃ "],
            &["abc", "def❮ ❯┃"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def ┃", "ghi"],
            &["abc", "def ❮", "❯┃ghi"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def┃", ""],
            &["abc", "def❮", "❯┃"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def", "ghi┃", "jkl"],
            &["abc", "def", "ghi❮", "❯┃jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def", "┃ghi", "jkl"],
            &["abc", "def", "❮g❯┃hi", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def", "g┃hi", "jkl"],
            &["abc", "def", "g❮h❯┃i", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["┃"], &["┃"], &shift_pressed(), move_caret_left)?;
        assert_move(&["a┃"], &["┃❮a❯"], &shift_pressed(), move_caret_left)?;
        assert_move(&["┃A"], &["┃A"], &shift_pressed(), move_caret_left)?;
        assert_move(&["┃abc"], &["┃abc"], &shift_pressed(), move_caret_left)?;
        assert_move(&["a┃bc"], &["┃❮a❯bc"], &shift_pressed(), move_caret_left)?;
        assert_move(&["abc┃"], &["ab┃❮c❯"], &shift_pressed(), move_caret_left)?;
        assert_move(&[" ┃abc"], &["┃❮ ❯abc"], &shift_pressed(), move_caret_left)?;
        assert_move(&["abc ┃"], &["abc┃❮ ❯"], &shift_pressed(), move_caret_left)?;
        assert_move(
            &["abc┃", "d"],
            &["ab┃❮c❯", "d"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "┃d"],
            &["abc┃❮", "❯d"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "┃"],
            &["abc┃❮", "❯"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", " ┃def"],
            &["abc", "┃❮ ❯def"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "d┃ef"],
            &["abc", "┃❮d❯ef"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "de┃f "],
            &["abc", "d┃❮e❯f "],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "┃"],
            &["abc", "def┃❮", "❯"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "┃ghi", "jkl"],
            &["abc", "def┃❮", "❯ghi", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "g┃hi", "jkl"],
            &["abc", "def", "┃❮g❯hi", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "gh┃i", "jkl"],
            &["abc", "def", "g┃❮h❯i", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "ghi┃", "jkl"],
            &["abc", "def", "gh┃❮i❯", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["┃"], &["┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["┃a"], &["❮a❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["A┃"], &["A┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["a┃bc"], &["a❮bc❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["ab┃c"], &["ab❮c❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["abc┃"], &["abc┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["abc┃ "], &["abc❮ ❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(
            &["abc", "┃def"],
            &["abc", "❮def❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "d┃ef"],
            &["abc", "d❮ef❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de┃f"],
            &["abc", "de❮f❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "def┃"],
            &["abc", "def┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["┃abc", "def"],
            &["❮abc", "❯┃def"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a┃bc", "def"],
            &["a❮bc", "d❯┃ef"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃c", "def"],
            &["ab❮c", "de❯┃f"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc┃", "def"],
            &["abc❮", "def❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "┃def ", "ghi"],
            &["abc", "❮def ", "❯┃ghi"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "d┃ef ", "ghi"],
            &["abc", "d❮ef ", "g❯┃hi"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de┃f ", "ghi"],
            &["abc", "de❮f ", "gh❯┃i"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "def┃ ", "ghi"],
            &["abc", "def❮ ", "ghi❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "def ┃", "ghi"],
            &["abc", "def ❮", "ghi❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de┃", "ghi"],
            &["abc", "de❮", "gh❯┃i"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc┃", "de"],
            &["abc❮", "de❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃c", "de"],
            &["ab❮c", "de❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a┃bc", "de"],
            &["a❮bc", "d❯┃e"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["┃abc", "de"],
            &["❮abc", "❯┃de"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃", "cdef", "ghijkl", "mnopqrst"],
            &["ab❮", "cd❯┃ef", "ghijkl", "mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef┃", "ghijkl", "mnopqrst"],
            &["ab", "cdef❮", "ghij❯┃kl", "mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl┃", "mnopqrst"],
            &["ab", "cdef", "ghijkl❮", "mnopqr❯┃st"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &[" ┃ab", " cdef", "ghijkl", "mnopqrst"],
            &[" ❮ab", " ❯┃cdef", "ghijkl", "mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "┃cdef", "ghijkl", "mnopqrst"],
            &["ab", "❮cdef", "❯┃ghijkl", "mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef", "┃ghijkl", "mnopqrst"],
            &["ab", "cdef", "❮ghijkl", "❯┃mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh┃", "ijklmn", "opqr", "st"],
            &["abcdefgh❮", "ijklmn❯┃", "opqr", "st"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn┃", "opqr", "st"],
            &["abcdefgh", "ijklmn❮", "opqr❯┃", "st"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr┃", "st"],
            &["abcdefgh", "ijklmn", "opqr❮", "st❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr", "┃st"],
            &["abcdefgh", "ijklmn", "opqr", "❮st❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc def gh ┃"],
            &["abc def gh ┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc de┃f gh "],
            &["abc de❮f gh ❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃c def gh "],
            &["ab❮c def gh ❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a┃bc def gh "],
            &["a❮bc def gh ❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["┃abc def gh "],
            &["❮abc def gh ❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["┃"], &["┃"], &shift_pressed(), move_caret_up)?;
        assert_move(&["┃a"], &["┃a"], &shift_pressed(), move_caret_up)?;
        assert_move(&["A┃"], &["┃❮A❯"], &shift_pressed(), move_caret_up)?;
        assert_move(&["a┃bc"], &["┃❮a❯bc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["ab┃c"], &["┃❮ab❯c"], &shift_pressed(), move_caret_up)?;
        assert_move(&["abc┃"], &["┃❮abc❯"], &shift_pressed(), move_caret_up)?;
        assert_move(
            &["┃abc", "def"],
            &["┃abc", "def"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "┃def"],
            &["┃❮abc", "❯def"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "d┃ef"],
            &["a┃❮bc", "d❯ef"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de┃f"],
            &["ab┃❮c", "de❯f"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def┃"],
            &["abc┃❮", "def❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ", "┃ghi"],
            &["abc", "┃❮def ", "❯ghi"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ", "g┃hi"],
            &["abc", "d┃❮ef ", "g❯hi"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ", "gh┃i"],
            &["abc", "de┃❮f ", "gh❯i"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ", "ghi┃"],
            &["abc", "def┃❮ ", "ghi❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de", "ghi┃"],
            &["abc", "de┃❮", "ghi❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de┃"],
            &["ab┃❮c", "de❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "d┃e"],
            &["a┃❮bc", "d❯e"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "┃de"],
            &["┃❮abc", "❯de"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "mnopqrst┃"],
            &["ab", "cdef", "ghijkl┃❮", "mnopqrst❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl┃", "mnopqrst"],
            &["ab", "cdef┃❮", "ghijkl❯", "mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "┃mnopqrst"],
            &["ab", "cdef", "┃❮ghijkl", "❯mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &[" ab", " ┃cdef", "ghijkl", "mnopqrst"],
            &[" ┃❮ab", " ❯cdef", "ghijkl", "mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "mnopqr┃st"],
            &["ab", "cdef", "ghijkl┃❮", "mnopqr❯st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cde┃f", "ghijkl", "mnopqrst"],
            &["ab┃❮", "cde❯f", "ghijkl", "mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr", "st┃"],
            &["abcdefgh", "ijklmn", "op┃❮qr", "st❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr┃", "st"],
            &["abcdefgh", "ijkl┃❮mn", "opqr❯", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn┃", "opqr", "st"],
            &["abcdef┃❮gh", "ijklmn❯", "opqr", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh┃", "ijklmn", "opqr", "st"],
            &["┃❮abcdefgh❯", "ijklmn", "opqr", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefg┃h", "ijklmn", "opqr", "st"],
            &["┃❮abcdefg❯h", "ijklmn", "opqr", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["a┃bcdefgh", "ijklmn", "opqr", "st"],
            &["┃❮a❯bcdefgh", "ijklmn", "opqr", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["┃abcdefgh", "ijklmn", "opqr", "st"],
            &["┃abcdefgh", "ijklmn", "opqr", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc def gh ┃"],
            &["┃❮abc def gh ❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc de┃f gh "],
            &["┃❮abc de❯f gh "],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab┃c def gh "],
            &["┃❮ab❯c def gh "],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["a┃bc def gh "],
            &["┃❮a❯bc def gh "],
            &shift_pressed(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_home() -> Result<(), String> {
        let move_caret_home = BigTextArea::move_caret_home;
        assert_move(&["┃"], &["┃"], &shift_pressed(), move_caret_home)?;
        assert_move(&["┃a"], &["┃a"], &shift_pressed(), move_caret_home)?;
        assert_move(&["a┃"], &["┃❮a❯"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" a┃"], &[" ┃❮a❯"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" ┃a"], &["┃❮ ❯a"], &shift_pressed(), move_caret_home)?;
        assert_move(&["\ta┃"], &["\t┃❮a❯"], &shift_pressed(), move_caret_home)?;
        assert_move(&["abc┃"], &["┃❮abc❯"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" abc┃"], &[" ┃❮abc❯"], &shift_pressed(), move_caret_home)?;
        assert_move(
            &["\tabc┃"],
            &["\t┃❮abc❯"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(&["ab┃c"], &["┃❮ab❯c"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" ab┃c"], &[" ┃❮ab❯c"], &shift_pressed(), move_caret_home)?;
        assert_move(
            &["\tab┃c"],
            &["\t┃❮ab❯c"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &[" abc def ghi┃"],
            &[" ┃❮abc def ghi❯"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc def ghi┃"],
            &["┃❮abc def ghi❯"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc def ┃ghi"],
            &["┃❮abc def ❯ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;

        assert_move(
            &["abc", "def", "ghi┃"],
            &["abc", "def", "┃❮ghi❯"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "def┃", "ghi"],
            &["abc", "┃❮def❯", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc┃", "def", "ghi"],
            &["┃❮abc❯", "def", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["┃abc", "def", "ghi"],
            &["┃abc", "def", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "┃def", "ghi"],
            &["abc", "┃def", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "def", "┃ghi"],
            &["abc", "def", "┃ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &[" ┃abc", "def", "ghi"],
            &["┃❮ ❯abc", "def", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", " ┃def", "ghi"],
            &["abc", "┃❮ ❯def", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "def", " ┃ghi"],
            &["abc", "def", "┃❮ ❯ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["┃ abc", "def", "ghi"],
            &["❮ ❯┃abc", "def", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "┃ def", "ghi"],
            &["abc", "❮ ❯┃def", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "def", "┃ ghi"],
            &["abc", "def", "❮ ❯┃ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_end() -> Result<(), String> {
        let move_caret_end = BigTextArea::move_caret_end;
        assert_move(&["┃"], &["┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["┃a"], &["❮a❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["a┃"], &["a┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&[" a ┃"], &[" a ┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["┃ a "], &["❮ a ❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["┃abc"], &["❮abc❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["┃abc\t"], &["❮abc\t❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["┃abc "], &["❮abc ❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["┃ abc "], &["❮ abc ❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&[" abc┃ "], &[" abc❮ ❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&[" ab┃c"], &[" ab❮c❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(
            &["abc", "def", "┃ghi"],
            &["abc", "def", "❮ghi❯┃"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abc", "┃def", "ghi"],
            &["abc", "❮def❯┃", "ghi"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["┃abc", "def", "ghi"],
            &["❮abc❯┃", "def", "ghi"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abc", "def", "┃ghi "],
            &["abc", "def", "❮ghi ❯┃"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abc", "┃def ", "ghi"],
            &["abc", "❮def ❯┃", "ghi"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["┃abc ", "def", "ghi"],
            &["❮abc ❯┃", "def", "ghi"],
            &shift_pressed(),
            move_caret_end,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["❮A❯┃"], &["A┃"], &no_mods(), move_caret_right)?;
        assert_move(&["❮a❯┃bc"], &["a┃bc"], &no_mods(), move_caret_right)?;
        assert_move(&["a❮b❯┃c"], &["ab┃c"], &no_mods(), move_caret_right)?;
        assert_move(&["ab❮c❯┃"], &["abc┃"], &no_mods(), move_caret_right)?;
        assert_move(&["❮ ❯┃abc"], &[" ┃abc"], &no_mods(), move_caret_right)?;
        assert_move(&["┃❮ ❯abc"], &[" ┃abc"], &no_mods(), move_caret_right)?;
        assert_move(&["a┃❮b❯c"], &["ab┃c"], &no_mods(), move_caret_right)?;
        assert_move(
            &["abc❮", "❯┃d"],
            &["abc", "┃d"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc┃❮", "❯d"],
            &["abc", "┃d"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(&["abc┃❮", "❯"], &["abc", "┃"], &no_mods(), move_caret_right)?;
        assert_move(
            &["abc", "❮d❯┃ef"],
            &["abc", "d┃ef"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def", "ghi❮", "❯┃jkl"],
            &["abc", "def", "ghi", "┃jkl"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(&["❮ab❯┃c"], &["ab┃c"], &no_mods(), move_caret_right)?;
        assert_move(&["❮abc❯┃"], &["abc┃"], &no_mods(), move_caret_right)?;
        assert_move(
            &["ab┃❮c", "❯def", "ghi"],
            &["abc", "┃def", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["ab❮c", "❯┃def", "ghi"],
            &["abc", "┃def", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["a┃❮bc", "❯def", "ghi"],
            &["abc", "┃def", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["┃❮abc", "❯def", "ghi"],
            &["abc", "┃def", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["a┃❮bc", "d❯ef", "ghi"],
            &["abc", "d┃ef", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["┃❮abc", "def❯", "ghi"],
            &["abc", "def┃", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["❮ab", "cdef", "ghijkl", "mnopqrst❯┃"],
            &["ab", "cdef", "ghijkl", "mnopqrst┃"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["┃❮ab", "cdef", "ghijkl", "mnopqrst❯"],
            &["ab", "cdef", "ghijkl", "mnopqrst┃"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["ab", "c❮def", "ghijkl", "mno❯┃pqrst"],
            &["ab", "cdef", "ghijkl", "mno┃pqrst"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["ab", "c┃❮def", "ghijkl", "mno❯pqrst"],
            &["ab", "cdef", "ghijkl", "mno┃pqrst"],
            &no_mods(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["❮A❯┃"], &["┃A"], &no_mods(), move_caret_left)?;
        /*assert_move(&["❮a❯┃bc"], &["┃abc"], &no_mods(), move_caret_left)?;
        assert_move(&["a❮b❯┃c"], &["a┃bc"], &no_mods(), move_caret_left)?;
        assert_move(&["ab❮c❯┃"], &["ab┃c"], &no_mods(), move_caret_left)?;
        assert_move(&["❮ ❯┃abc"], &["┃ abc"], &no_mods(), move_caret_left)?;
        assert_move(&["┃❮ ❯abc"], &["┃ abc"], &no_mods(), move_caret_left)?;
        assert_move(&["a┃❮b❯c"], &["a┃bc"], &no_mods(), move_caret_left)?;
        assert_move(
            &["abc❮", "❯┃d"],
            &["abc┃", "d"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc┃❮", "❯d"],
            &["abc┃", "d"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc┃❮", "❯"],
            &["abc┃", ""],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "❮d❯┃ef"],
            &["abc", "┃def"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "ghi❮", "❯┃jkl"],
            &["abc", "def", "ghi┃", "jkl"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(&["❮ab❯┃c"], &["┃abc"], &no_mods(), move_caret_left)?;
        assert_move(&["❮abc❯┃"], &["┃abc"], &no_mods(), move_caret_left)?;
        assert_move(
            &["ab┃❮c", "❯def", "ghi"],
            &["ab┃c", "def", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["ab❮c", "❯┃def", "ghi"],
            &["ab┃c", "def", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["a┃❮bc", "❯def", "ghi"],
            &["a┃bc", "def", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["┃❮abc", "❯def", "ghi"],
            &["┃abc", "def", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["a┃❮bc", "d❯ef", "ghi"],
            &["a┃bc", "def", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["┃❮abc", "def❯", "ghi"],
            &["┃abc", "def", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["❮ab", "cdef", "ghijkl", "mnopqrst❯┃"],
            &["┃ab", "cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["┃❮ab", "cdef", "ghijkl", "mnopqrst❯"],
            &["┃ab", "cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["ab", "c❮def", "ghijkl", "mno❯┃pqrst"],
            &["ab", "c┃def", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["ab", "c┃❮def", "ghijkl", "mno❯pqrst"],
            &["ab", "c┃def", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_left,
        )?;*/

        Ok(())
    }

    #[test]
    fn end_selection_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["❮a❯┃"], &["a┃"], &no_mods(), move_caret_down)?;
        assert_move(&["┃❮a❯"], &["a┃"], &no_mods(), move_caret_down)?;
        assert_move(&["a┃❮bc❯"], &["abc┃"], &no_mods(), move_caret_down)?;
        assert_move(&["ab❮c❯┃"], &["abc┃"], &no_mods(), move_caret_down)?;
        assert_move(&["abc┃❮ ❯"], &["abc ┃"], &no_mods(), move_caret_down)?;
        assert_move(
            &["abc", "┃❮def❯"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "d┃❮ef❯"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de┃❮f❯"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["❮abc", "❯┃def"],
            &["abc", "┃def"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a❮bc", "d❯┃ef"],
            &["abc", "d┃ef"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃❮c", "de❯f"],
            &["abc", "de┃f"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc❮", "def❯┃"],
            &["abc", "def┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "┃❮def ", "❯ghi"],
            &["abc", "def ", "┃ghi"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "d❮ef ", "g❯┃hi"],
            &["abc", "def ", "g┃hi"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de❮f ", "gh❯┃i"],
            &["abc", "def ", "gh┃i"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "def❮ ", "ghi❯┃"],
            &["abc", "def ", "ghi┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "def ❮", "ghi❯┃"],
            &["abc", "def ", "ghi┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de❮", "gh❯┃i"],
            &["abc", "de", "gh┃i"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc┃❮", "de❯"],
            &["abc", "de┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab❮c", "de❯┃"],
            &["abc", "de┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a┃❮bc", "d❯e"],
            &["abc", "d┃e"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["❮abc", "❯┃de"],
            &["abc", "┃de"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab❮", "cd❯┃ef", "ghijkl", "mnopqrst"],
            &["ab", "cd┃ef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef┃❮", "ghij❯kl", "mnopqrst"],
            &["ab", "cdef", "ghij┃kl", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl❮", "mnopqr❯┃st"],
            &["ab", "cdef", "ghijkl", "mnopqr┃st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &[" ❮ab", " ❯┃cdef", "ghijkl", "mnopqrst"],
            &[" ab", " ┃cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "┃❮cdef", "❯ghijkl", "mnopqrst"],
            &["ab", "cdef", "┃ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef", "❮ghijkl", "❯┃mnopqrst"],
            &["ab", "cdef", "ghijkl", "┃mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh❮", "ijklmn❯┃", "opqr", "st"],
            &["abcdefgh", "ijklmn┃", "opqr", "st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn❮", "opqr❯┃", "st"],
            &["abcdefgh", "ijklmn", "opqr┃", "st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr❮", "st❯┃"],
            &["abcdefgh", "ijklmn", "opqr", "st┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr", "❮st❯┃"],
            &["abcdefgh", "ijklmn", "opqr", "st┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc de❮f gh ❯┃"],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃❮c def gh ❯"],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a❮bc def gh ❯┃"],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["❮abc def gh ❯┃"],
            &["abc def gh ┃"],
            &no_mods(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["❮a❯┃"], &["┃a"], &no_mods(), move_caret_up)?;
        assert_move(&["┃❮a❯"], &["┃a"], &no_mods(), move_caret_up)?;
        assert_move(&["a┃❮bc❯"], &["a┃bc"], &no_mods(), move_caret_up)?;
        assert_move(&["ab❮c❯┃"], &["ab┃c"], &no_mods(), move_caret_up)?;
        assert_move(&["abc┃❮ ❯"], &["abc┃ "], &no_mods(), move_caret_up)?;
        assert_move(
            &["abc", "┃❮def❯"],
            &["abc", "┃def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "d┃❮ef❯"],
            &["abc", "d┃ef"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de┃❮f❯"],
            &["abc", "de┃f"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["❮abc", "❯┃def"],
            &["┃abc", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a❮bc", "d❯┃ef"],
            &["a┃bc", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab┃❮c", "de❯f"],
            &["ab┃c", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc❮", "def❯┃"],
            &["abc┃", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "┃❮def ", "❯ghi"],
            &["abc", "┃def ", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "d❮ef ", "g❯┃hi"],
            &["abc", "d┃ef ", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de┃❮f ", "gh❯i"],
            &["abc", "de┃f ", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def❮ ", "ghi❯┃"],
            &["abc", "def┃ ", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def ❮", "ghi❯┃"],
            &["abc", "def ┃", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de❮", "gh❯┃i"],
            &["abc", "de┃", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc┃❮", "de❯"],
            &["abc┃", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab❮c", "de❯┃"],
            &["ab┃c", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a┃❮bc", "d❯e"],
            &["a┃bc", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["❮abc", "❯┃de"],
            &["┃abc", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab❮", "cd❯┃ef", "ghijkl", "mnopqrst"],
            &["ab┃", "cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef┃❮", "ghij❯kl", "mnopqrst"],
            &["ab", "cdef┃", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl❮", "mnopqr❯┃st"],
            &["ab", "cdef", "ghijkl┃", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &[" ❮ab", " ❯┃cdef", "ghijkl", "mnopqrst"],
            &[" ┃ab", " cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "┃❮cdef", "❯ghijkl", "mnopqrst"],
            &["ab", "┃cdef", "ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "❮ghijkl", "❯┃mnopqrst"],
            &["ab", "cdef", "┃ghijkl", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh❮", "ijklmn❯┃", "opqr", "st"],
            &["abcdefgh┃", "ijklmn", "opqr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn❮", "opqr❯┃", "st"],
            &["abcdefgh", "ijklmn┃", "opqr", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr❮", "st❯┃"],
            &["abcdefgh", "ijklmn", "opqr┃", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "opqr", "❮st❯┃"],
            &["abcdefgh", "ijklmn", "opqr", "┃st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc de❮f gh ❯┃"],
            &["abc de┃f gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab┃❮c def gh ❯"],
            &["ab┃c def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a❮bc def gh ❯┃"],
            &["a┃bc def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["❮abc def gh ❯┃"],
            &["┃abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_home() -> Result<(), String> {
        let move_caret_home = BigTextArea::move_caret_home;
        assert_move(&["❮a❯┃"], &["┃a"], &no_mods(), move_caret_home)?;
        assert_move(&["┃❮a❯"], &["┃a"], &no_mods(), move_caret_home)?;
        assert_move(&[" ┃❮a❯"], &["┃ a"], &no_mods(), move_caret_home)?;
        assert_move(&["┃❮ a❯"], &[" ┃a"], &no_mods(), move_caret_home)?;
        assert_move(&[" ❮a❯┃"], &[" ┃a"], &no_mods(), move_caret_home)?;
        assert_move(&[" a❮bc ❯┃"], &[" ┃abc "], &no_mods(), move_caret_home)?;
        assert_move(&["\t❮abc ❯┃"], &["\t┃abc "], &no_mods(), move_caret_home)?;
        assert_move(&["\t┃❮abc❯ "], &["┃\tabc "], &no_mods(), move_caret_home)?;
        assert_move(&["┃❮\tabc❯ "], &["\t┃abc "], &no_mods(), move_caret_home)?;
        assert_move(
            &["❮ abc def\tghi❯┃"],
            &[" ┃abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &[" ┃❮abc❯ def\tghi"],
            &["┃ abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["┃❮ abc def\tghi❯"],
            &[" ┃abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;

        assert_move(
            &["abc", "d❮e❯┃", "ghi"],
            &["abc", "┃de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", " ❮d❯┃e", "ghi"],
            &["abc", " ┃de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["❮abc", "❯┃ de", "ghi"],
            &["abc", " ┃de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", " ┃❮de❯", "ghi"],
            &["abc", "┃ de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc┃❮", "de", "ghi❯"],
            &["┃abc", "de", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_end() -> Result<(), String> {
        let move_caret_end = BigTextArea::move_caret_end;
        assert_move(&["┃❮a❯"], &["a┃"], &no_mods(), move_caret_end)?;
        assert_move(&["❮a❯┃"], &["a┃"], &no_mods(), move_caret_end)?;
        assert_move(&[" a┃❮ ❯"], &[" a ┃"], &no_mods(), move_caret_end)?;
        assert_move(&["❮ a❯┃ "], &[" a ┃"], &no_mods(), move_caret_end)?;
        assert_move(&[" ❮a❯┃ "], &[" a ┃"], &no_mods(), move_caret_end)?;
        assert_move(&["❮ a ❯┃"], &[" a ┃"], &no_mods(), move_caret_end)?;
        assert_move(&["┃❮ a❯bc "], &[" abc ┃"], &no_mods(), move_caret_end)?;
        assert_move(&["┃❮\tabc❯ "], &["\tabc ┃"], &no_mods(), move_caret_end)?;
        assert_move(
            &[" abc d┃❮ef\tg❯hi"],
            &[" abc def\tghi┃"],
            &no_mods(),
            move_caret_end,
        )?;

        assert_move(
            &["abc", "┃❮de", "ghi❯"],
            &["abc", "de┃", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["❮abc", " d❯┃e", "ghi"],
            &["abc", " de┃", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["┃❮abc", "de", "ghi❯"],
            &["abc┃", "de", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc", "de", "g┃❮hi❯"],
            &["abc", "de", "ghi┃"],
            &no_mods(),
            move_caret_end,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["❮a❯┃bc"], &["❮ab❯┃c"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a❮b❯┃c"], &["a❮bc❯┃"], &shift_pressed(), move_caret_right)?;
        assert_move(&["❮ab❯┃c"], &["❮abc❯┃"], &shift_pressed(), move_caret_right)?;
        assert_move(
            &["❮ ❯┃abc"],
            &["❮ a❯┃bc"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(&["❮abc❯┃"], &["❮abc❯┃"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a❮bc❯┃"], &["a❮bc❯┃"], &shift_pressed(), move_caret_right)?;
        assert_move(&["ab❮c❯┃"], &["ab❮c❯┃"], &shift_pressed(), move_caret_right)?;
        assert_move(
            &["abc❮", "❯┃d"],
            &["abc❮", "d❯┃"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["ab❮c❯┃", ""],
            &["ab❮c", "❯┃"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["ab❮c❯┃", "d"],
            &["ab❮c", "❯┃d"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "def", "ghi❮", "❯┃jkl"],
            &["abc", "def", "ghi❮", "j❯┃kl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["ab❮c", "def", "ghi", "❯┃jkl"],
            &["ab❮c", "def", "ghi", "j❯┃kl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["ab❮c", "def", "❯┃ghi", "jkl"],
            &["ab❮c", "def", "g❯┃hi", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["❮abc", "def", "ghi", "jk❯┃l"],
            &["❮abc", "def", "ghi", "jkl❯┃"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["❮abc", "def", "ghi", "jkl❯┃"],
            &["❮abc", "def", "ghi", "jkl❯┃"],
            &shift_pressed(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["ab┃❮c❯"], &["a┃❮bc❯"], &shift_pressed(), move_caret_left)?;
        assert_move(&["a┃❮bc❯"], &["┃❮abc❯"], &shift_pressed(), move_caret_left)?;
        assert_move(&["┃❮abc❯"], &["┃❮abc❯"], &shift_pressed(), move_caret_left)?;
        assert_move(&["┃❮ab❯c"], &["┃❮ab❯c"], &shift_pressed(), move_caret_left)?;
        assert_move(&["┃❮a❯bc"], &["┃❮a❯bc"], &shift_pressed(), move_caret_left)?;
        assert_move(
            &[" ┃❮a❯bc"],
            &["┃❮ a❯bc"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc┃❮", "❯d"],
            &["ab┃❮c", "❯d"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "┃❮d❯"],
            &["abc┃❮", "d❯"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["ab┃❮c", "❯"],
            &["a┃❮bc", "❯"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def┃❮", "ghi", "j❯kl"],
            &["abc", "de┃❮f", "ghi", "j❯kl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["a┃❮bc", "def", "ghi", "jkl❯"],
            &["┃❮abc", "def", "ghi", "jkl❯"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def", "ghi", "┃❮jkl❯"],
            &["abc", "def", "ghi┃❮", "jkl❯"],
            &shift_pressed(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["ab┃❮c❯"], &["┃❮abc❯"], &shift_pressed(), move_caret_up)?;
        assert_move(&["a┃❮bc❯"], &["┃❮abc❯"], &shift_pressed(), move_caret_up)?;
        assert_move(&["┃❮abc❯"], &["┃❮abc❯"], &shift_pressed(), move_caret_up)?;
        assert_move(&["┃❮ab❯c"], &["┃❮ab❯c"], &shift_pressed(), move_caret_up)?;
        assert_move(&["┃❮a❯bc"], &["┃❮a❯bc"], &shift_pressed(), move_caret_up)?;
        assert_move(&[" ┃❮a❯bc"], &["┃❮ a❯bc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["ab❮c❯┃"], &["┃❮ab❯c"], &shift_pressed(), move_caret_up)?;
        assert_move(&["❮a❯┃"], &["┃a"], &shift_pressed(), move_caret_up)?;
        assert_move(&["❮a❯┃bc"], &["┃abc"], &shift_pressed(), move_caret_up)?;
        assert_move(
            &["❮a❯┃bc", "d"],
            &["┃abc", "d"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de❮f❯┃"],
            &["abc┃❮", "de❯f"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "de┃❮f❯"],
            &["ab┃❮c", "def❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab┃❮c", "def❯"],
            &["┃❮abc", "def❯"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "❮mnopqr❯┃st"],
            &["ab", "cdef", "ghijkl┃❮", "❯mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "❮mnopqrs❯┃t"],
            &["ab", "cdef", "ghijkl┃❮", "❯mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "┃❮o❯pqr", "st"],
            &["abcdefgh", "┃❮ijklmn", "o❯pqr", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["❮ab❯┃c"], &["❮abc❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["❮a❯┃bc"], &["❮abc❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["❮abc❯┃"], &["❮abc❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["┃❮ab❯c"], &["ab❮c❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(&["┃❮a❯bc"], &["a❮bc❯┃"], &shift_pressed(), move_caret_down)?;
        assert_move(
            &["❮a❯┃bc", "d"],
            &["❮abc", "d❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["❮a❯┃bc", "de"],
            &["❮abc", "d❯┃e"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["❮abc", "d❯┃e"],
            &["❮abc", "de❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["❮a❯┃bc", ""],
            &["❮abc", "❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "❮mnopqr❯┃st"],
            &["ab", "cdef", "ghijkl", "❮mnopqrst❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a❮b", "cdef", "ghijkl", "mnopqr❯┃st"],
            &["a❮b", "cdef", "ghijkl", "mnopqrst❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["❮ab", "cdef", "ghijkl", "mnopqrst❯┃"],
            &["❮ab", "cdef", "ghijkl", "mnopqrst❯┃"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcd❮efgh❯┃", "ijklmn", "opqr", "st"],
            &["abcd❮efgh", "ijklmn❯┃", "opqr", "st"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcd❮e❯┃fgh", "ijklmn", "opqr", "st"],
            &["abcd❮efgh", "ijklm❯┃n", "opqr", "st"],
            &shift_pressed(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_home() -> Result<(), String> {
        let move_caret_home = SelectableLines::move_caret_home;

        assert_move(&["ab┃❮c❯"], &["┃❮abc❯"], &shift_pressed(), move_caret_home)?;
        assert_move(&["a┃❮bc❯"], &["┃❮abc❯"], &shift_pressed(), move_caret_home)?;
        assert_move(&["┃❮abc❯"], &["┃❮abc❯"], &shift_pressed(), move_caret_home)?;
        assert_move(&["┃❮ab❯c"], &["┃❮ab❯c"], &shift_pressed(), move_caret_home)?;
        assert_move(&["┃❮a❯bc"], &["┃❮a❯bc"], &shift_pressed(), move_caret_home)?;
        assert_move(
            &[" ┃❮a❯bc"],
            &["┃❮ a❯bc"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(&["ab❮c❯┃"], &["┃❮ab❯c"], &shift_pressed(), move_caret_home)?;
        assert_move(
            &["abc", "de❮f❯┃"],
            &["abc", "┃❮de❯f"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "de┃❮f❯"],
            &["abc", "┃❮def❯"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["ab┃❮c", "def❯"],
            &["┃❮abc", "def❯"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &[" ab┃❮c", "def❯"],
            &[" ┃❮abc", "def❯"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &[" ┃❮abc", "def❯"],
            &["┃❮ abc", "def❯"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "┃❮mnopqr❯st"],
            &["ab", "cdef", "ghijkl", "┃❮mnopqr❯st"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["ab", "cdef", "gh┃❮ijkl❯", "mnopqrst"],
            &["ab", "cdef", "┃❮ghijkl❯", "mnopqrst"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abcdefgh", "ijklmn", "op❮qr❯┃", "st"],
            &["abcdefgh", "ijklmn", "┃❮op❯qr", "st"],
            &shift_pressed(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_end() -> Result<(), String> {
        let move_caret_end = SelectableLines::move_caret_end;

        assert_move(&["❮ab❯┃c"], &["❮abc❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["❮a❯┃bc"], &["❮abc❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["❮abc❯┃"], &["❮abc❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["┃❮ab❯c"], &["ab❮c❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(&["┃❮a❯bc"], &["a❮bc❯┃"], &shift_pressed(), move_caret_end)?;
        assert_move(
            &["❮a❯┃bc", "d"],
            &["❮abc❯┃", "d"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["❮a❯┃bc ", "de"],
            &["❮abc ❯┃", "de"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["❮abc", "d❯┃e"],
            &["❮abc", "de❯┃"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["ab", "cdef", "ghijkl", "❮mnopqr❯┃st"],
            &["ab", "cdef", "ghijkl", "❮mnopqrst❯┃"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["a❮b", "cdef", "ghijkl", "mnopqr❯┃st"],
            &["a❮b", "cdef", "ghijkl", "mnopqrst❯┃"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["❮ab", "cdef", "ghijkl", "mnopqrst❯┃"],
            &["❮ab", "cdef", "ghijkl", "mnopqrst❯┃"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abcd❮e❯┃fgh", "ijklmn", "opqr", "st"],
            &["abcd❮efgh❯┃", "ijklmn", "opqr", "st"],
            &shift_pressed(),
            move_caret_end,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["ab┃❮c❯"], &["abc┃"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a┃❮bc❯"], &["ab┃❮c❯"], &shift_pressed(), move_caret_right)?;
        assert_move(&["┃❮abc❯"], &["a┃❮bc❯"], &shift_pressed(), move_caret_right)?;
        assert_move(
            &["┃❮abc", "def", "ghi", "jkl❯"],
            &["a┃❮bc", "def", "ghi", "jkl❯"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "d┃❮ef", "❯ghi", "jkl"],
            &["abc", "de┃❮f", "❯ghi", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc", "de┃❮f❯", "ghi", "jkl"],
            &["abc", "def┃", "ghi", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["ab❮c❯┃"], &["ab┃c"], &shift_pressed(), move_caret_left)?;
        assert_move(&["a❮bc❯┃"], &["a❮b❯┃c"], &shift_pressed(), move_caret_left)?;
        assert_move(&["❮abc❯┃"], &["❮ab❯┃c"], &shift_pressed(), move_caret_left)?;
        assert_move(
            &["❮abc", "def", "ghi", "jkl❯┃"],
            &["❮abc", "def", "ghi", "jk❯┃l"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["┃❮abc", "def", "ghi", "jkl❯"],
            &["┃❮abc", "def", "ghi", "jkl❯"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "def❮", "❯┃ghi", "jkl"],
            &["abc", "def┃", "ghi", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc", "d❮ef", "gh❯┃i", "jkl"],
            &["abc", "d❮ef", "g❯┃hi", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["❮abc❯┃"], &["┃abc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["❮ab❯┃c"], &["┃abc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["❮a❯┃bc"], &["┃abc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["┃abc"], &["┃abc"], &shift_pressed(), move_caret_up)?;
        assert_move(
            &["❮abc", "def❯┃"],
            &["❮abc❯┃", "def"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["❮abc", "de❯┃f"],
            &["❮ab❯┃c", "def"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["❮abc", "def", "ghi", "jkl❯┃"],
            &["❮abc", "def", "ghi❯┃", "jkl"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "def", "ghi❮", "jkl❯┃"],
            &["abc", "def", "ghi┃", "jkl"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc", "d❮ef", "ghi", "jk❯┃l"],
            &["abc", "d❮ef", "gh❯┃i", "jkl"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["❮abc", "d❯┃ef", "ghi", "jkl"],
            &["❮a❯┃bc", "def", "ghi", "jkl"],
            &shift_pressed(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["┃❮abc❯"], &["abc┃"], &shift_pressed(), move_caret_down)?;
        assert_move(
            &["┃❮abc", "def❯"],
            &["abc", "┃❮def❯"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a┃❮bc", "def❯"],
            &["abc", "d┃❮ef❯"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["┃❮abc", "def", "ghi❯"],
            &["abc", "┃❮def", "ghi❯"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃❮c", "def", "ghi❯"],
            &["abc", "de┃❮f", "ghi❯"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc", "de┃❮f", "ghi❯"],
            &["abc", "def", "gh┃❮i❯"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdef┃❮", "ghij", "kl❯"],
            &["abcdef", "ghij┃❮", "kl❯"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcde┃❮f", "ghij", "kl❯"],
            &["abcdef", "ghij┃❮", "kl❯"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab┃❮cdef", "ghij", "kl❯"],
            &["abcdef", "gh┃❮ij", "kl❯"],
            &shift_pressed(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_home() -> Result<(), String> {
        let move_caret_home = SelectableLines::move_caret_home;

        assert_move(&["❮abc❯┃"], &["┃abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&["❮ab❯┃c"], &["┃abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&["❮a❯┃bc"], &["┃abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" ❮abc❯┃"], &[" ┃abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" ❮ab❯┃c"], &[" ┃abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" ❮a❯┃bc"], &[" ┃abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&["ab❮c❯┃"], &["┃❮ab❯c"], &shift_pressed(), move_caret_home)?;
        assert_move(&["a❮b❯┃c"], &["┃❮a❯bc"], &shift_pressed(), move_caret_home)?;
        assert_move(&["a❮bc❯┃"], &["┃❮a❯bc"], &shift_pressed(), move_caret_home)?;

        assert_move(
            &["❮abc", "def❯┃"],
            &["❮abc", "❯┃def"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["❮abc", " de❯┃f"],
            &["❮abc", " ❯┃def"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["❮abc", "def", "ghi", "jkl❯┃"],
            &["❮abc", "def", "ghi", "❯┃jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "def", "ghi❮", "jkl❯┃"],
            &["abc", "def", "ghi❮", "❯┃jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc", "d❮ef", " ghi", " jk❯┃l"],
            &["abc", "d❮ef", " ghi", " ❯┃jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["❮abc", "d❯┃ef", "ghi", "jkl"],
            &["❮abc", "❯┃def", "ghi", "jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["❮abc", "d❯┃ef", "ghi", "jkl"],
            &["❮abc", "❯┃def", "ghi", "jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_end() -> Result<(), String> {
        let move_caret_end = SelectableLines::move_caret_end;

        assert_move(&["┃❮abc❯"], &["abc┃"], &shift_pressed(), move_caret_end)?;
        assert_move(
            &["┃❮abc", "def❯"],
            &["abc┃❮", "def❯"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["a┃❮bc", "def❯"],
            &["abc┃❮", "def❯"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["a┃❮bc", "def", "ghi❯"],
            &["abc┃❮", "def", "ghi❯"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["ab┃❮c", "def", "ghi❯"],
            &["abc┃❮", "def", "ghi❯"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abc", "de┃❮f", "ghi❯"],
            &["abc", "def┃❮", "ghi❯"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abcdef┃❮", "ghij", "kl❯"],
            &["abcdef┃❮", "ghij", "kl❯"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["┃❮ abcdef", "ghij", "kl❯"],
            &[" abcdef┃❮", "ghij", "kl❯"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abcdef", "ghij", "┃❮kl❯"],
            &["abcdef", "ghij", "kl┃"],
            &shift_pressed(),
            move_caret_end,
        )?;

        Ok(())
    }
}
