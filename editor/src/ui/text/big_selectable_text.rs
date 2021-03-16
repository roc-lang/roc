// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

#![allow(dead_code)]

use crate::ui::text::{
    caret_w_select::CaretWSelect,
    lines::{Lines, MutSelectableLines, SelectableLines},
    selection::{validate_raw_sel, validate_selection, RawSelection, Selection},
    text_pos::TextPos,
};
use crate::ui::ui_error::{
    OutOfBounds,
    UIError::{FileOpenFailed, TextBufReadFailed},
    UIResult,
};
use crate::ui::util::is_newline;
use crate::window::keyboard_input::{no_mods, Modifiers};
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use ropey::Rope;
use snafu::ensure;
use std::{
    cmp::{max, min},
    fmt,
    fs::File,
    io,
    path::Path,
};
use winit::event::{VirtualKeyCode, VirtualKeyCode::*};

pub struct BigSelectableText {
    pub caret_w_select: CaretWSelect,
    text_rope: Rope,
    pub path_str: String,
    arena: Bump,
}

impl BigSelectableText {
    fn check_bounds(&self, char_indx: usize) -> UIResult<()> {
        ensure!(
            char_indx <= self.text_rope.len_chars(),
            OutOfBounds {
                index: char_indx,
                collection_name: "Rope",
                len: self.text_rope.len_chars()
            }
        );

        Ok(())
    }

    fn pos_to_char_indx(&self, pos: TextPos) -> usize {
        self.text_rope.line_to_char(pos.line) + pos.column
    }

    fn char_indx_to_pos(&self, char_indx: usize) -> TextPos {
        let line = self.text_rope.char_to_line(char_indx);

        let char_idx_line_start = self.pos_to_char_indx(TextPos { line, column: 0 });

        let column = char_indx - char_idx_line_start;

        TextPos { line, column }
    }

    fn sel_to_tup(&self, val_sel: Selection) -> (usize, usize) {
        let start_char_indx = self.pos_to_char_indx(val_sel.start_pos);
        let end_char_indx = self.pos_to_char_indx(val_sel.end_pos);

        (start_char_indx, end_char_indx)
    }
}

fn validate_sel_opt(start_pos: TextPos, end_pos: TextPos) -> UIResult<Option<Selection>> {
    Ok(Some(validate_selection(start_pos, end_pos)?))
}

impl Lines for BigSelectableText {
    fn get_line(&self, line_nr: usize) -> UIResult<&str> {
        ensure!(
            line_nr < self.nr_of_lines(),
            OutOfBounds {
                index: line_nr,
                collection_name: "BigSelectableText",
                len: self.nr_of_lines(),
            }
        );

        let rope_slice = self.text_rope.line(line_nr);

        if let Some(line_str_ref) = rope_slice.as_str() {
            Ok(line_str_ref)
        } else {
            // happens very rarely
            let line_str = rope_slice.chunks().collect::<String>();
            let arena_str_ref = self.arena.alloc(line_str);
            Ok(arena_str_ref)
        }
    }

    fn line_len(&self, line_nr: usize) -> UIResult<usize> {
        self.get_line(line_nr).map(|line| line.len())
    }

    fn nr_of_lines(&self) -> usize {
        self.text_rope.len_lines()
    }

    fn nr_of_chars(&self) -> usize {
        self.text_rope.len_chars()
    }

    // TODO use pool allocation here
    // expensive function, don't use it if it can be done with a specialized, more efficient function
    fn all_lines<'a>(&self, arena: &'a Bump) -> BumpString<'a> {
        let mut lines = BumpString::with_capacity_in(self.text_rope.len_chars(), arena);

        for line in self.text_rope.lines() {
            lines.extend(line.as_str());
        }

        lines
    }
}

impl SelectableLines for BigSelectableText {
    fn get_caret(self) -> TextPos {
        self.caret_w_select.caret_pos
    }

    fn set_caret(&mut self, caret_pos: TextPos) {
        self.caret_w_select.caret_pos = caret_pos;
    }

    fn move_caret_left(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let old_selection_opt = self.get_selection();
        let old_caret_pos = self.caret_w_select.caret_pos;
        let old_line_nr = old_caret_pos.line;
        let old_col_nr = old_caret_pos.column;

        let shift_pressed = modifiers.shift;

        let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
            match old_selection_opt {
                Some(old_selection) => {
                    (old_selection.start_pos.line, old_selection.start_pos.column)
                }
                None => unreachable!(),
            }
        } else if old_col_nr == 0 {
            if old_line_nr == 0 {
                (0, 0)
            } else {
                let curr_line_len = self.line_len(old_line_nr - 1)?;

                (old_line_nr - 1, curr_line_len - 1)
            }
        } else {
            (old_line_nr, old_col_nr - 1)
        };

        let new_caret_pos = TextPos {
            line: line_nr,
            column: col_nr,
        };

        let new_selection_opt = if shift_pressed {
            if let Some(old_selection) = old_selection_opt {
                if old_caret_pos >= old_selection.end_pos {
                    if new_caret_pos == old_selection.start_pos {
                        None
                    } else {
                        validate_sel_opt(old_selection.start_pos, new_caret_pos)?
                    }
                } else {
                    validate_sel_opt(
                        TextPos {
                            line: line_nr,
                            column: col_nr,
                        },
                        old_selection.end_pos,
                    )?
                }
            } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
                validate_sel_opt(
                    TextPos {
                        line: line_nr,
                        column: col_nr,
                    },
                    TextPos {
                        line: old_line_nr,
                        column: old_col_nr,
                    },
                )?
            } else {
                None
            }
        } else {
            None
        };

        self.caret_w_select = CaretWSelect::new(new_caret_pos, new_selection_opt);

        Ok(())
    }

    fn move_caret_right(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let old_selection_opt = self.get_selection();
        let old_caret_pos = self.caret_w_select.caret_pos;
        let old_line_nr = old_caret_pos.line;
        let old_col_nr = old_caret_pos.column;

        let shift_pressed = modifiers.shift;

        let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
            match old_selection_opt {
                Some(old_selection) => (old_selection.end_pos.line, old_selection.end_pos.column),
                None => unreachable!(),
            }
        } else {
            let curr_line = self.get_line(old_line_nr)?;

            if let Some(last_char) = curr_line.chars().last() {
                if is_newline(&last_char) {
                    if old_col_nr + 1 > curr_line.len() - 1 {
                        (old_line_nr + 1, 0)
                    } else {
                        (old_line_nr, old_col_nr + 1)
                    }
                } else if old_col_nr < curr_line.len() {
                    (old_line_nr, old_col_nr + 1)
                } else {
                    (old_line_nr, old_col_nr)
                }
            } else {
                (old_line_nr, old_col_nr)
            }
        };

        let new_caret_pos = TextPos {
            line: line_nr,
            column: col_nr,
        };

        let new_selection_opt = if shift_pressed {
            if let Some(old_selection) = old_selection_opt {
                if old_caret_pos <= old_selection.start_pos {
                    if new_caret_pos == old_selection.end_pos {
                        None
                    } else {
                        validate_sel_opt(new_caret_pos, old_selection.end_pos)?
                    }
                } else {
                    validate_sel_opt(
                        old_selection.start_pos,
                        TextPos {
                            line: line_nr,
                            column: col_nr,
                        },
                    )?
                }
            } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
                validate_sel_opt(
                    TextPos {
                        line: old_line_nr,
                        column: old_col_nr,
                    },
                    TextPos {
                        line: line_nr,
                        column: col_nr,
                    },
                )?
            } else {
                None
            }
        } else {
            None
        };

        self.caret_w_select = CaretWSelect::new(new_caret_pos, new_selection_opt);

        Ok(())
    }

    fn move_caret_up(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let old_selection_opt = self.get_selection();
        let old_caret_pos = self.caret_w_select.caret_pos;
        let old_line_nr = old_caret_pos.line;
        let old_col_nr = old_caret_pos.column;

        let shift_pressed = modifiers.shift;

        let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
            match old_selection_opt {
                Some(old_selection) => {
                    (old_selection.start_pos.line, old_selection.start_pos.column)
                }
                None => unreachable!(),
            }
        } else if old_line_nr == 0 {
            (old_line_nr, 0)
        } else {
            let prev_line_len = self.line_len(old_line_nr - 1)?;

            if prev_line_len <= old_col_nr {
                (old_line_nr - 1, prev_line_len - 1)
            } else {
                (old_line_nr - 1, old_col_nr)
            }
        };

        let new_caret_pos = TextPos {
            line: line_nr,
            column: col_nr,
        };

        let new_selection_opt = if shift_pressed {
            if let Some(old_selection) = old_selection_opt {
                if old_selection.end_pos <= old_caret_pos {
                    if new_caret_pos == old_selection.start_pos {
                        None
                    } else {
                        validate_sel_opt(
                            min(old_selection.start_pos, new_caret_pos),
                            max(old_selection.start_pos, new_caret_pos),
                        )?
                    }
                } else {
                    validate_sel_opt(new_caret_pos, old_selection.end_pos)?
                }
            } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
                validate_sel_opt(
                    min(old_caret_pos, new_caret_pos),
                    max(old_caret_pos, new_caret_pos),
                )?
            } else {
                None
            }
        } else {
            None
        };

        self.caret_w_select = CaretWSelect::new(new_caret_pos, new_selection_opt);

        Ok(())
    }

    fn move_caret_down(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let old_selection_opt = self.get_selection();
        let old_caret_pos = self.caret_w_select.caret_pos;
        let old_line_nr = old_caret_pos.line;
        let old_col_nr = old_caret_pos.column;

        let shift_pressed = modifiers.shift;

        let (line_nr, col_nr) = if old_selection_opt.is_some() && !shift_pressed {
            match old_selection_opt {
                Some(old_selection) => (old_selection.end_pos.line, old_selection.end_pos.column),
                None => unreachable!(),
            }
        } else if old_line_nr + 1 >= self.nr_of_lines() {
            let curr_line_len = self.line_len(old_line_nr)?;

            (old_line_nr, curr_line_len)
        } else {
            let next_line = self.get_line(old_line_nr + 1)?;

            if next_line.len() <= old_col_nr {
                if let Some(last_char) = next_line.chars().last() {
                    if is_newline(&last_char) {
                        (old_line_nr + 1, next_line.len() - 1)
                    } else {
                        (old_line_nr + 1, next_line.len())
                    }
                } else {
                    (old_line_nr + 1, 0)
                }
            } else {
                (old_line_nr + 1, old_col_nr)
            }
        };

        let new_caret_pos = TextPos {
            line: line_nr,
            column: col_nr,
        };

        let new_selection_opt = if shift_pressed {
            if let Some(old_selection) = old_selection_opt {
                if old_caret_pos <= old_selection.start_pos {
                    if new_caret_pos == old_selection.end_pos {
                        None
                    } else {
                        validate_sel_opt(
                            min(old_selection.end_pos, new_caret_pos),
                            max(old_selection.end_pos, new_caret_pos),
                        )?
                    }
                } else {
                    validate_sel_opt(old_selection.start_pos, new_caret_pos)?
                }
            } else if !(old_line_nr == line_nr && old_col_nr == col_nr) {
                validate_sel_opt(
                    min(old_caret_pos, new_caret_pos),
                    max(old_caret_pos, new_caret_pos),
                )?
            } else {
                None
            }
        } else {
            None
        };

        self.caret_w_select = CaretWSelect::new(new_caret_pos, new_selection_opt);

        Ok(())
    }

    fn move_caret_home(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let curr_line_nr = self.caret_w_select.caret_pos.line;
        let old_col_nr = self.caret_w_select.caret_pos.column;

        let curr_line_str = self.get_line(curr_line_nr)?;
        let line_char_iter = curr_line_str.chars();

        let mut first_no_space_char_col = 0;
        let mut non_space_found = false;

        for c in line_char_iter {
            if !c.is_whitespace() {
                non_space_found = true;
                break;
            } else {
                first_no_space_char_col += 1;
            }
        }

        if !non_space_found {
            first_no_space_char_col = 0;
        }

        let new_col_nr = if first_no_space_char_col == old_col_nr {
            0
        } else {
            first_no_space_char_col
        };

        self.caret_w_select.move_caret_w_mods(
            TextPos {
                line: curr_line_nr,
                column: new_col_nr,
            },
            modifiers,
        )
    }

    fn move_caret_end(&mut self, modifiers: &Modifiers) -> UIResult<()> {
        let curr_line_nr = self.caret_w_select.caret_pos.line;
        let curr_line_len = self.line_len(curr_line_nr)?;

        let new_col = if let Some(last_char) = self.last_char(curr_line_nr)? {
            if is_newline(&last_char) {
                curr_line_len - 1
            } else {
                curr_line_len
            }
        } else {
            0
        };

        let new_pos = TextPos {
            line: curr_line_nr,
            column: new_col,
        };

        self.caret_w_select.move_caret_w_mods(new_pos, modifiers)
    }

    fn get_selection(&self) -> Option<Selection> {
        self.caret_w_select.selection_opt
    }

    fn is_selection_active(&self) -> bool {
        self.get_selection().is_some()
    }

    fn get_selected_str(&self) -> UIResult<Option<&str>> {
        if let Some(val_sel) = self.caret_w_select.selection_opt {
            let (start_char_indx, end_char_indx) = self.sel_to_tup(val_sel);

            self.check_bounds(end_char_indx)?;

            let rope_slice = self.text_rope.slice(start_char_indx..end_char_indx);

            if let Some(line_str_ref) = rope_slice.as_str() {
                Ok(Some(line_str_ref))
            } else {
                // happens very rarely
                let line_str = rope_slice.chunks().collect::<String>();
                let arena_str_ref = self.arena.alloc(line_str);
                Ok(Some(arena_str_ref))
            }
        } else {
            Ok(None)
        }
    }

    fn set_raw_sel(&mut self, raw_sel: RawSelection) -> UIResult<()> {
        self.caret_w_select.selection_opt = Some(validate_raw_sel(raw_sel)?);

        Ok(())
    }

    fn set_sel_none(&mut self) {
        self.caret_w_select.selection_opt = None;
    }

    fn select_all(&mut self) -> UIResult<()> {
        if self.nr_of_chars() > 0 {
            let last_pos = self.last_text_pos();

            self.set_raw_sel(RawSelection {
                start_pos: TextPos { line: 0, column: 0 },
                end_pos: last_pos,
            })?;

            self.set_caret(last_pos);
        }

        Ok(())
    }

    fn last_text_pos(&self) -> TextPos {
        self.char_indx_to_pos(self.nr_of_chars())
    }

    fn last_char(&self, line_nr: usize) -> UIResult<Option<char>> {
        Ok(self.get_line(line_nr)?.chars().last())
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
                if modifiers.ctrl {
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

impl MutSelectableLines for BigSelectableText {
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

                self.pop_char()?
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
        let char_indx = self.pos_to_char_indx(caret_pos);

        self.check_bounds(char_indx)?;

        self.text_rope.insert(char_indx, new_str);

        Ok(())
    }

    fn pop_char(&mut self) -> UIResult<()> {
        if self.is_selection_active() {
            self.del_selection()?;
        } else {
            let old_caret_pos = self.caret_w_select.caret_pos;
            let char_indx = self.pos_to_char_indx(old_caret_pos);

            self.move_caret_left(&no_mods())?;

            if (char_indx > 0) && char_indx <= self.text_rope.len_chars() {
                self.text_rope.remove((char_indx - 1)..char_indx);
            }
        }

        Ok(())
    }

    fn del_selection(&mut self) -> UIResult<()> {
        if let Some(selection) = self.caret_w_select.selection_opt {
            let (start_char_indx, end_char_indx) = self.sel_to_tup(selection);

            self.check_bounds(end_char_indx)?;

            self.text_rope.remove(start_char_indx..end_char_indx);

            self.set_caret(selection.start_pos);

            self.set_sel_none();
        }

        Ok(())
    }
}

impl Default for BigSelectableText {
    fn default() -> Self {
        let caret_w_select = CaretWSelect::default();
        let text_rope = Rope::from_str("");
        let path_str = "".to_owned();
        let arena = Bump::new();

        Self {
            caret_w_select,
            text_rope,
            path_str,
            arena,
        }
    }
}

pub fn from_path(path: &Path) -> UIResult<BigSelectableText> {
    let text_rope = rope_from_path(path)?;
    let path_str = path_to_string(path);

    Ok(BigSelectableText {
        text_rope,
        path_str,
        ..Default::default()
    })
}

#[allow(dead_code)]
// used by tests but will also be used in the future
pub fn from_str(text: &str) -> BigSelectableText {
    let text_rope = Rope::from_str(text);

    BigSelectableText {
        text_rope,
        ..Default::default()
    }
}

fn path_to_string(path: &Path) -> String {
    let mut path_str = String::new();
    path_str.push_str(&path.to_string_lossy());

    path_str
}

fn rope_from_path(path: &Path) -> UIResult<Rope> {
    match File::open(path) {
        Ok(file) => {
            let buf_reader = &mut io::BufReader::new(file);
            match Rope::from_reader(buf_reader) {
                Ok(rope) => Ok(rope),
                Err(e) => Err(TextBufReadFailed {
                    path_str: path_to_string(path),
                    err_msg: e.to_string(),
                }),
            }
        }
        Err(e) => Err(FileOpenFailed {
            path_str: path_to_string(path),
            err_msg: e.to_string(),
        }),
    }
}

// need to explicitly omit arena
impl fmt::Debug for BigSelectableText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BigSelectableText")
            .field("caret_w_select", &self.caret_w_select)
            .field("text_rope", &self.text_rope)
            .field("path_str", &self.path_str)
            .finish()
    }
}

#[cfg(test)]
pub mod test_big_sel_text {
    use crate::ui::text::{
        big_selectable_text::from_str,
        big_selectable_text::BigSelectableText,
        caret_w_select::CaretWSelect,
        lines::{Lines, MutSelectableLines, SelectableLines},
        selection::validate_selection,
        text_pos::TextPos,
    };
    use crate::ui::ui_error::{OutOfBounds, UIResult};
    use crate::window::keyboard_input::{no_mods, Modifiers};
    use core::cmp::Ordering;
    use pest::Parser;
    use snafu::OptionExt;
    use std::{collections::HashMap, slice::SliceIndex};

    fn shift_pressed() -> Modifiers {
        Modifiers {
            shift: true,
            ..Default::default()
        }
    }

    // replace vec methods that return Option with ones that return Result and proper Error
    pub fn slice_get<T>(
        index: usize,
        slice: &[T],
    ) -> UIResult<&<usize as SliceIndex<[T]>>::Output> {
        let elt_ref = slice.get(index).context(OutOfBounds {
            index,
            collection_name: "Slice",
            len: slice.len(),
        })?;

        Ok(elt_ref)
    }

    #[derive(Parser)]
    #[grammar = "../tests/selection.pest"]
    pub struct LineParser;

    // show selection and caret position as symbols in lines for easy testing
    pub fn convert_selection_to_dsl(
        caret_w_select: CaretWSelect,
        lines: &mut [String],
    ) -> UIResult<&[String]> {
        let selection_opt = caret_w_select.selection_opt;
        let caret_pos = caret_w_select.caret_pos;

        if let Some(sel) = selection_opt {
            let mut to_insert = vec![(sel.start_pos, '['), (sel.end_pos, ']'), (caret_pos, '|')];
            let symbol_map: HashMap<char, usize> =
                [('[', 2), (']', 0), ('|', 1)].iter().cloned().collect();

            // sort for nice printing
            to_insert.sort_by(|a, b| {
                let pos_cmp = a.0.cmp(&b.0);
                if pos_cmp == Ordering::Equal {
                    symbol_map.get(&a.1).cmp(&symbol_map.get(&b.1))
                } else {
                    pos_cmp
                }
            });

            // insert symbols into text lines
            for i in 0..to_insert.len() {
                let (pos, insert_char) = *slice_get(i, &to_insert)?;

                insert_at_pos(lines, pos, insert_char)?;

                // shift position of following symbols now that symbol is inserted
                for j in i..to_insert.len() {
                    let (old_pos, _) = get_mut_res(j, &mut to_insert)?;

                    if old_pos.line == pos.line {
                        old_pos.column += 1;
                    }
                }
            }
        } else {
            insert_at_pos(lines, caret_pos, '|')?;
        }

        Ok(lines)
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

        let elt_ref = vec.get_mut(index).context(OutOfBounds {
            index,
            collection_name: "Slice",
            len: vec_len,
        })?;

        Ok(elt_ref)
    }

    pub fn big_text_from_dsl_str(lines: &[String]) -> BigSelectableText {
        from_str(
            &lines
                .iter()
                .map(|line| line.replace(&['[', ']', '|'][..], ""))
                .collect::<Vec<String>>()
                .join(""),
        )
    }

    pub fn all_lines_vec(big_sel_text: &BigSelectableText) -> Vec<String> {
        let mut lines: Vec<String> = Vec::new();

        for i in 0..big_sel_text.nr_of_lines() {
            lines.push(big_sel_text.get_line(i).unwrap().to_string());
        }

        lines
    }

    // Retrieve selection and position from formatted string
    pub fn convert_dsl_to_selection(lines: &[String]) -> Result<CaretWSelect, String> {
        let lines_str: String = lines.join("");

        let parsed = LineParser::parse(Rule::linesWithSelect, &lines_str)
            .expect("Selection test DSL parsing failed");

        let mut caret_opt: Option<(usize, usize)> = None;
        let mut sel_start_opt: Option<(usize, usize)> = None;
        let mut sel_end_opt: Option<(usize, usize)> = None;
        let mut line_nr = 0;
        let mut col_nr = 0;

        for line in parsed {
            for elt in line.into_inner() {
                match elt.as_rule() {
                    Rule::optCaret => {
                        if elt.as_span().as_str() == "|" {
                            if caret_opt.is_some() {
                                return Err(
                                    "Multiple carets found, there should be only one".to_owned()
                                );
                            } else {
                                caret_opt = Some((line_nr, col_nr));
                            }
                        }
                    }
                    Rule::optSelStart => {
                        if sel_start_opt.is_some() {
                            if elt.as_span().as_str() == "[" {
                                return Err("Found start of selection more than once, there should be only one".to_owned());
                            }
                        } else if elt.as_span().as_str() == "[" {
                            sel_start_opt = Some((line_nr, col_nr));
                        }
                    }
                    Rule::optSelEnd => {
                        if sel_end_opt.is_some() {
                            if elt.as_span().as_str() == "]" {
                                return Err("Found end of selection more than once, there should be only one".to_owned());
                            }
                        } else if elt.as_span().as_str() == "]" {
                            sel_end_opt = Some((line_nr, col_nr));
                        }
                    }
                    Rule::text => {
                        let split_str = elt
                            .as_span()
                            .as_str()
                            .split('\n')
                            .into_iter()
                            .collect::<Vec<&str>>();

                        if split_str.len() > 1 {
                            line_nr += split_str.len() - 1;
                            col_nr = 0
                        }
                        if let Some(last_str) = split_str.last() {
                            col_nr += last_str.len()
                        }
                    }
                    _ => {}
                }
            }
        }

        // Make sure return makes sense
        if let Some((line, column)) = caret_opt {
            let caret_pos = TextPos { line, column };
            if sel_start_opt.is_none() && sel_end_opt.is_none() {
                Ok(CaretWSelect::new(caret_pos, None))
            } else if let Some((start_line, start_column)) = sel_start_opt {
                if let Some((end_line, end_column)) = sel_end_opt {
                    Ok(CaretWSelect::new(
                        caret_pos,
                        Some(
                            validate_selection(
                                TextPos {
                                    line: start_line,
                                    column: start_column,
                                },
                                TextPos {
                                    line: end_line,
                                    column: end_column,
                                },
                            )
                            .unwrap(),
                        ),
                    ))
                } else {
                    Err("Selection end ']' was not found, but selection start '[' was. Bad input string.".to_owned())
                }
            } else {
                Err("Selection start '[' was not found, but selection end ']' was. Bad input string.".to_owned())
            }
        } else {
            Err("No caret was found in lines.".to_owned())
        }
    }

    pub fn gen_big_text(lines: &[&str]) -> Result<BigSelectableText, String> {
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

        let mut actual_lines = all_lines_vec(&big_text);
        let dsl_slice =
            convert_selection_to_dsl(big_text.caret_w_select, &mut actual_lines).unwrap();
        assert_eq!(dsl_slice, expected_post_lines_str);

        Ok(())
    }

    #[test]
    fn insert_new_char_simple() -> Result<(), String> {
        assert_insert(&["|"], &["a|"], 'a')?;
        assert_insert(&["|"], &[" |"], ' ')?;
        assert_insert(&["a|"], &["aa|"], 'a')?;
        assert_insert(&["a|"], &["a |"], ' ')?;
        assert_insert(&["a|\n", ""], &["ab|\n", ""], 'b')?;
        assert_insert(&["a|\n", ""], &["ab|\n", ""], 'b')?;
        assert_insert(&["a\n", "|"], &["a\n", "b|"], 'b')?;
        assert_insert(&["a\n", "b\n", "c|"], &["a\n", "b\n", "cd|"], 'd')?;

        Ok(())
    }

    #[test]
    fn insert_new_char_mid() -> Result<(), String> {
        assert_insert(&["ab|d"], &["abc|d"], 'c')?;
        assert_insert(&["a|cd"], &["ab|cd"], 'b')?;
        assert_insert(&["abc\n", "|e"], &["abc\n", "d|e"], 'd')?;
        assert_insert(&["abc\n", "def\n", "| "], &["abc\n", "def\n", "g| "], 'g')?;
        assert_insert(&["abc\n", "def\n", "| "], &["abc\n", "def\n", " | "], ' ')?;

        Ok(())
    }

    #[test]
    fn simple_backspace() -> Result<(), String> {
        assert_insert(&["|"], &["|"], '\u{8}')?;
        assert_insert(&[" |"], &["|"], '\u{8}')?;
        assert_insert(&["a|"], &["|"], '\u{8}')?;
        assert_insert(&["ab|"], &["a|"], '\u{8}')?;
        assert_insert(&["a|\n", ""], &["|\n", ""], '\u{8}')?;
        assert_insert(&["ab|\n", ""], &["a|\n", ""], '\u{8}')?;
        assert_insert(&["a\n", "|"], &["a|"], '\u{8}')?;
        assert_insert(&["a\n", "b\n", "c|"], &["a\n", "b\n", "|"], '\u{8}')?;
        assert_insert(&["a\n", "b\n", "|"], &["a\n", "b|"], '\u{8}')?;

        Ok(())
    }

    #[test]
    fn selection_backspace() -> Result<(), String> {
        assert_insert(&["[a]|"], &["|"], '\u{8}')?;
        assert_insert(&["a[a]|"], &["a|"], '\u{8}')?;
        assert_insert(&["[aa]|"], &["|"], '\u{8}')?;
        assert_insert(&["a[b c]|"], &["a|"], '\u{8}')?;
        assert_insert(&["[abc]|\n", ""], &["|\n", ""], '\u{8}')?;
        assert_insert(&["a\n", "[abc]|"], &["a\n", "|"], '\u{8}')?;
        assert_insert(&["[a\n", "abc]|"], &["|"], '\u{8}')?;
        assert_insert(&["a[b\n", "cdef ghij]|"], &["a|"], '\u{8}')?;
        assert_insert(&["[a\n", "b\n", "c]|"], &["|"], '\u{8}')?;
        assert_insert(&["a\n", "[b\n", "]|"], &["a\n", "|"], '\u{8}')?;
        assert_insert(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            &["abc\n", "d|\n", "jkl"],
            '\u{8}',
        )?;
        assert_insert(
            &["abc\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "|\n", "jkl"],
            '\u{8}',
        )?;
        assert_insert(
            &["abc\n", "\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "\n", "|\n", "jkl"],
            '\u{8}',
        )?;
        assert_insert(
            &["[abc\n", "\n", "def\n", "ghi\n", "jkl]|"],
            &["|"],
            '\u{8}',
        )?;

        Ok(())
    }

    #[test]
    fn insert_with_selection() -> Result<(), String> {
        assert_insert(&["[a]|"], &["z|"], 'z')?;
        assert_insert(&["a[a]|"], &["az|"], 'z')?;
        assert_insert(&["[aa]|"], &["z|"], 'z')?;
        assert_insert(&["a[b c]|"], &["az|"], 'z')?;
        assert_insert(&["[abc]|\n", ""], &["z|\n", ""], 'z')?;
        assert_insert(&["a\n", "[abc]|"], &["a\n", "z|"], 'z')?;
        assert_insert(&["[a\n", "abc]|"], &["z|"], 'z')?;
        assert_insert(&["a[b\n", "cdef ghij]|"], &["az|"], 'z')?;
        assert_insert(&["[a\n", "b\n", "c]|"], &["z|"], 'z')?;
        assert_insert(&["a\n", "[b\n", "]|"], &["a\n", "z|"], 'z')?;
        assert_insert(
            &["abc\n", "d[ef\n", "ghi]|\n", "jkl"],
            &["abc\n", "dz|\n", "jkl"],
            'z',
        )?;
        assert_insert(
            &["abc\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "z|\n", "jkl"],
            'z',
        )?;
        assert_insert(
            &["abc\n", "\n", "[def\n", "ghi]|\n", "jkl"],
            &["abc\n", "\n", "z|\n", "jkl"],
            'z',
        )?;
        assert_insert(&["[abc\n", "\n", "def\n", "ghi\n", "jkl]|"], &["z|"], 'z')?;

        Ok(())
    }

    fn assert_select_all(
        pre_lines_str: &[&str],
        expected_post_lines_str: &[&str],
    ) -> Result<(), String> {
        let mut big_text = gen_big_text(pre_lines_str)?;

        big_text.select_all().unwrap();

        let mut big_text_lines = all_lines_vec(&big_text);
        let post_lines_str =
            convert_selection_to_dsl(big_text.caret_w_select, &mut big_text_lines)?;

        assert_eq!(post_lines_str, expected_post_lines_str);

        Ok(())
    }

    #[test]
    fn select_all() -> Result<(), String> {
        assert_select_all(&["|"], &["|"])?;
        assert_select_all(&["|a"], &["[a]|"])?;
        assert_select_all(&["a|"], &["[a]|"])?;
        assert_select_all(&["abc d|ef ghi"], &["[abc def ghi]|"])?;
        assert_select_all(&["[a]|"], &["[a]|"])?;
        assert_select_all(&["|[a]"], &["[a]|"])?;
        assert_select_all(&["|[abc def ghi]"], &["[abc def ghi]|"])?;
        assert_select_all(&["a\n", "[b\n", "]|"], &["[a\n", "b\n", "]|"])?;
        assert_select_all(&["a\n", "[b]|\n", ""], &["[a\n", "b\n", "]|"])?;
        assert_select_all(&["a\n", "|[b\n", "]"], &["[a\n", "b\n", "]|"])?;
        assert_select_all(
            &["abc\n", "def\n", "gh|i\n", "jkl"],
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
        )?;
        assert_select_all(
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
        )?;

        Ok(())
    }

    type MoveCaretFun = fn(&mut BigSelectableText, &Modifiers) -> UIResult<()>;

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

        let mut lines_vec = all_lines_vec(&big_text);
        let post_lines_res = convert_selection_to_dsl(big_text.caret_w_select, &mut lines_vec);

        match post_lines_res {
            Ok(post_lines) => {
                assert_eq!(expected_post_lines, post_lines);
                Ok(())
            }
            Err(e) => Err(format!("{:?}", e)),
        }
    }

    #[test]
    fn move_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["|"], &["|"], &no_mods(), move_caret_right)?;
        assert_move(&["a|"], &["a|"], &no_mods(), move_caret_right)?;
        assert_move(&["|A"], &["A|"], &no_mods(), move_caret_right)?;
        assert_move(&["|abc"], &["a|bc"], &no_mods(), move_caret_right)?;
        assert_move(&["a|bc"], &["ab|c"], &no_mods(), move_caret_right)?;
        assert_move(&["abc|"], &["abc|"], &no_mods(), move_caret_right)?;
        assert_move(&["| abc"], &[" |abc"], &no_mods(), move_caret_right)?;
        assert_move(&["abc| "], &["abc |"], &no_mods(), move_caret_right)?;
        assert_move(
            &["abc|\n", "d"],
            &["abc\n", "|d"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc|\n", ""],
            &["abc\n", "|"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "|def"],
            &["abc\n", "d|ef"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def| "],
            &["abc\n", "def |"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def |\n", "ghi"],
            &["abc\n", "def \n", "|ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def|\n", ""],
            &["abc\n", "def\n", "|"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &["abc\n", "def\n", "ghi\n", "|jkl"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def\n", "|ghi\n", "jkl"],
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &["abc\n", "def\n", "gh|i\n", "jkl"],
            &no_mods(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn move_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["|"], &["|"], &no_mods(), move_caret_left)?;
        assert_move(&["|a"], &["|a"], &no_mods(), move_caret_left)?;
        assert_move(&["|A"], &["|A"], &no_mods(), move_caret_left)?;
        assert_move(&["a|bc"], &["|abc"], &no_mods(), move_caret_left)?;
        assert_move(&["ab|c"], &["a|bc"], &no_mods(), move_caret_left)?;
        assert_move(&["abc|"], &["ab|c"], &no_mods(), move_caret_left)?;
        assert_move(&[" |abc"], &["| abc"], &no_mods(), move_caret_left)?;
        assert_move(&["abc |"], &["abc| "], &no_mods(), move_caret_left)?;
        assert_move(
            &["abc\n", "|d"],
            &["abc|\n", "d"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "|"],
            &["abc|\n", ""],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["abc\n", "|def"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def |"],
            &["abc\n", "def| "],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def \n", "|ghi"],
            &["abc\n", "def |\n", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "|"],
            &["abc\n", "def|\n", ""],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi\n", "|jkl"],
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &["abc\n", "def\n", "|ghi\n", "jkl"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "gh|i\n", "jkl"],
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &no_mods(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn move_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["|"], &["|"], &no_mods(), move_caret_up)?;
        assert_move(&["|a"], &["|a"], &no_mods(), move_caret_up)?;
        assert_move(&["A|"], &["|A"], &no_mods(), move_caret_up)?;
        assert_move(&["a|bc"], &["|abc"], &no_mods(), move_caret_up)?;
        assert_move(&["ab|c"], &["|abc"], &no_mods(), move_caret_up)?;
        assert_move(&["abc|"], &["|abc"], &no_mods(), move_caret_up)?;
        assert_move(
            &["|abc\n", "def"],
            &["|abc\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "|def"],
            &["|abc\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["a|bc\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de|f"],
            &["ab|c\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def|"],
            &["abc|\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def \n", "|ghi"],
            &["abc\n", "|def \n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def \n", "g|hi"],
            &["abc\n", "d|ef \n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def \n", "gh|i"],
            &["abc\n", "de|f \n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def \n", "ghi|"],
            &["abc\n", "def| \n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de\n", "ghi|"],
            &["abc\n", "de|\n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de|"],
            &["ab|c\n", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "d|e"],
            &["a|bc\n", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "|de"],
            &["|abc\n", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqrst|"],
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &["ab\n", "cdef|\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "|mnopqrst"],
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &[" ab\n", " |cdef\n", "ghijkl\n", "mnopqrst"],
            &[" |ab\n", " cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqr|st"],
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cde|f\n", "ghijkl\n", "mnopqrst"],
            &["ab|\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            &["abcdefgh\n", "ijklmn\n", "op|qr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &["abcdefgh\n", "ijkl|mn\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &["abcdef|gh\n", "ijklmn\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefg|h\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a|bcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc def gh |"],
            &["|abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc de|f gh "],
            &["|abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab|c def gh "],
            &["|abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a|bc def gh "],
            &["|abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn move_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["|"], &["|"], &no_mods(), move_caret_down)?;
        assert_move(&["|a"], &["a|"], &no_mods(), move_caret_down)?;
        assert_move(&["A|"], &["A|"], &no_mods(), move_caret_down)?;
        assert_move(&["a|bc"], &["abc|"], &no_mods(), move_caret_down)?;
        assert_move(&["ab|c"], &["abc|"], &no_mods(), move_caret_down)?;
        assert_move(&["abc|"], &["abc|"], &no_mods(), move_caret_down)?;
        assert_move(&["abc| "], &["abc |"], &no_mods(), move_caret_down)?;
        assert_move(
            &["abc\n", "|def"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de|f"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "def|"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["|abc\n", "def"],
            &["abc\n", "|def"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a|bc\n", "def"],
            &["abc\n", "d|ef"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|c\n", "def"],
            &["abc\n", "de|f"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc|\n", "def"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "|def \n", "ghi"],
            &["abc\n", "def \n", "|ghi"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "d|ef \n", "ghi"],
            &["abc\n", "def \n", "g|hi"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de|f \n", "ghi"],
            &["abc\n", "def \n", "gh|i"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "def| \n", "ghi"],
            &["abc\n", "def \n", "ghi|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "def |\n", "ghi"],
            &["abc\n", "def \n", "ghi|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de|\n", "ghi"],
            &["abc\n", "de\n", "gh|i"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc|\n", "de"],
            &["abc\n", "de|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|c\n", "de"],
            &["abc\n", "de|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a|bc\n", "de"],
            &["abc\n", "d|e"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["|abc\n", "de"],
            &["abc\n", "|de"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cd|ef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef|\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghij|kl\n", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqr|st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &[" |ab\n", " cdef\n", "ghijkl\n", "mnopqrst"],
            &[" ab\n", " |cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "|cdef\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "|mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "|st"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc def gh |"],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc de|f gh "],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|c def gh "],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a|bc def gh "],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["|abc def gh "],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn move_home() -> Result<(), String> {
        let move_caret_home = BigSelectableText::move_caret_home;
        assert_move(&["|"], &["|"], &no_mods(), move_caret_home)?;
        assert_move(&["a|"], &["|a"], &no_mods(), move_caret_home)?;
        assert_move(&["|a"], &["|a"], &no_mods(), move_caret_home)?;
        assert_move(&[" |a"], &["| a"], &no_mods(), move_caret_home)?;
        assert_move(&["| a"], &[" |a"], &no_mods(), move_caret_home)?;
        assert_move(&[" a|"], &[" |a"], &no_mods(), move_caret_home)?;
        assert_move(&[" abc |"], &[" |abc "], &no_mods(), move_caret_home)?;
        assert_move(&["\tabc |"], &["\t|abc "], &no_mods(), move_caret_home)?;
        assert_move(&["\t|abc "], &["|\tabc "], &no_mods(), move_caret_home)?;
        assert_move(&["|\tabc "], &["\t|abc "], &no_mods(), move_caret_home)?;
        assert_move(
            &[" abc def\tghi|"],
            &[" |abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &[" |abc def\tghi"],
            &["| abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["| abc def\tghi"],
            &[" |abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;

        assert_move(
            &["abc\n", "de|\n", "ghi"],
            &["abc\n", "|de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", " d|e\n", "ghi"],
            &["abc\n", " |de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "| de\n", "ghi"],
            &["abc\n", " |de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", " |de\n", "ghi"],
            &["abc\n", "| de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc|\n", "de\n", "ghi"],
            &["|abc\n", "de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &[" |abc\n", "de\n", "ghi"],
            &["| abc\n", "de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["| abc\n", "de\n", "ghi"],
            &[" |abc\n", "de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "de\n", "ghi|"],
            &["abc\n", "de\n", "|ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "de\n", " |ghi"],
            &["abc\n", "de\n", "| ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "de\n", "| ghi"],
            &["abc\n", "de\n", " |ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc \n", "de \n", "|ghi "],
            &["abc \n", "de \n", "|ghi "],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc \n", "|de \n", "ghi "],
            &["abc \n", "|de \n", "ghi "],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["|abc \n", "de \n", "ghi "],
            &["|abc \n", "de \n", "ghi "],
            &no_mods(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn move_end() -> Result<(), String> {
        let move_caret_end = BigSelectableText::move_caret_end;
        assert_move(&["|"], &["|"], &no_mods(), move_caret_end)?;
        assert_move(&["|a"], &["a|"], &no_mods(), move_caret_end)?;
        assert_move(&["a|"], &["a|"], &no_mods(), move_caret_end)?;
        assert_move(&[" a| "], &[" a |"], &no_mods(), move_caret_end)?;
        assert_move(&["| abc "], &[" abc |"], &no_mods(), move_caret_end)?;
        assert_move(&["|\tabc "], &["\tabc |"], &no_mods(), move_caret_end)?;
        assert_move(
            &[" abc d|ef\tghi"],
            &[" abc def\tghi|"],
            &no_mods(),
            move_caret_end,
        )?;

        assert_move(
            &["abc\n", "|de\n", "ghi"],
            &["abc\n", "de|\n", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc\n", " d|e\n", "ghi"],
            &["abc\n", " de|\n", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["|abc\n", "de\n", "ghi"],
            &["abc|\n", "de\n", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc\n", "de\n", "g|hi"],
            &["abc\n", "de\n", "ghi|"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc \n", "de \n", "ghi| "],
            &["abc \n", "de \n", "ghi |"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc \n", "|de \n", "ghi "],
            &["abc \n", "de |\n", "ghi "],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc |\n", "de \n", "ghi "],
            &["abc |\n", "de \n", "ghi "],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc \n", "de |\n", "ghi "],
            &["abc \n", "de |\n", "ghi "],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc \n", "de \n", "ghi |"],
            &["abc \n", "de \n", "ghi |"],
            &no_mods(),
            move_caret_end,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["|"], &["|"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a|"], &["a|"], &shift_pressed(), move_caret_right)?;
        assert_move(&["|A"], &["[A]|"], &shift_pressed(), move_caret_right)?;
        assert_move(&["|abc"], &["[a]|bc"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a|bc"], &["a[b]|c"], &shift_pressed(), move_caret_right)?;
        assert_move(&["abc|"], &["abc|"], &shift_pressed(), move_caret_right)?;
        assert_move(&["| abc"], &["[ ]|abc"], &shift_pressed(), move_caret_right)?;
        assert_move(&["abc| "], &["abc[ ]|"], &shift_pressed(), move_caret_right)?;
        assert_move(
            &["abc|\n", "d"],
            &["abc[\n", "]|d"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc|\n", ""],
            &["abc[\n", "]|"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "|def"],
            &["abc\n", "[d]|ef"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def| "],
            &["abc\n", "def[ ]|"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def |\n", "ghi"],
            &["abc\n", "def [\n", "]|ghi"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def|\n", ""],
            &["abc\n", "def[\n", "]|"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def\n", "|ghi\n", "jkl"],
            &["abc\n", "def\n", "[g]|hi\n", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &["abc\n", "def\n", "g[h]|i\n", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["|"], &["|"], &shift_pressed(), move_caret_left)?;
        assert_move(&["a|"], &["|[a]"], &shift_pressed(), move_caret_left)?;
        assert_move(&["|A"], &["|A"], &shift_pressed(), move_caret_left)?;
        assert_move(&["|abc"], &["|abc"], &shift_pressed(), move_caret_left)?;
        assert_move(&["a|bc"], &["|[a]bc"], &shift_pressed(), move_caret_left)?;
        assert_move(&["abc|"], &["ab|[c]"], &shift_pressed(), move_caret_left)?;
        assert_move(&[" |abc"], &["|[ ]abc"], &shift_pressed(), move_caret_left)?;
        assert_move(&["abc |"], &["abc|[ ]"], &shift_pressed(), move_caret_left)?;
        assert_move(
            &["abc|\n", "d"],
            &["ab|[c]\n", "d"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "|d"],
            &["abc|[\n", "]d"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "|"],
            &["abc|[\n", "]"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", " |def"],
            &["abc\n", "|[ ]def"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["abc\n", "|[d]ef"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "de|f "],
            &["abc\n", "d|[e]f "],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "|"],
            &["abc\n", "def|[\n", "]"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "|ghi\n", "jkl"],
            &["abc\n", "def|[\n", "]ghi\n", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "g|hi\n", "jkl"],
            &["abc\n", "def\n", "|[g]hi\n", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "gh|i\n", "jkl"],
            &["abc\n", "def\n", "g|[h]i\n", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &["abc\n", "def\n", "gh|[i]\n", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["|"], &["|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["|a"], &["[a]|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["A|"], &["A|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["a|bc"], &["a[bc]|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["ab|c"], &["ab[c]|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["abc|"], &["abc|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["abc| "], &["abc[ ]|"], &shift_pressed(), move_caret_down)?;
        assert_move(
            &["abc\n", "|def"],
            &["abc\n", "[def]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["abc\n", "d[ef]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de|f"],
            &["abc\n", "de[f]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "def|"],
            &["abc\n", "def|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["|abc\n", "def"],
            &["[abc\n", "]|def"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a|bc\n", "def"],
            &["a[bc\n", "d]|ef"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|c\n", "def"],
            &["ab[c\n", "de]|f"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc|\n", "def"],
            &["abc[\n", "def]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "|def \n", "ghi"],
            &["abc\n", "[def \n", "]|ghi"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "d|ef \n", "ghi"],
            &["abc\n", "d[ef \n", "g]|hi"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de|f \n", "ghi"],
            &["abc\n", "de[f \n", "gh]|i"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "def| \n", "ghi"],
            &["abc\n", "def[ \n", "ghi]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "def |\n", "ghi"],
            &["abc\n", "def [\n", "ghi]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de|\n", "ghi"],
            &["abc\n", "de[\n", "gh]|i"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc|\n", "de"],
            &["abc[\n", "de]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|c\n", "de"],
            &["ab[c\n", "de]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a|bc\n", "de"],
            &["a[bc\n", "d]|e"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["|abc\n", "de"],
            &["[abc\n", "]|de"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            &["ab[\n", "cd]|ef\n", "ghijkl\n", "mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef|\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef[\n", "ghij]|kl\n", "mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghijkl[\n", "mnopqr]|st"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &[" |ab\n", " cdef\n", "ghijkl\n", "mnopqrst"],
            &[" [ab\n", " ]|cdef\n", "ghijkl\n", "mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "|cdef\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "[cdef\n", "]|ghijkl\n", "mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "[ghijkl\n", "]|mnopqrst"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &["abcdefgh[\n", "ijklmn]|\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &["abcdefgh\n", "ijklmn[\n", "opqr]|\n", "st"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "opqr[\n", "st]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "|st"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "[st]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc def gh |"],
            &["abc def gh |"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc de|f gh "],
            &["abc de[f gh ]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|c def gh "],
            &["ab[c def gh ]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a|bc def gh "],
            &["a[bc def gh ]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["|abc def gh "],
            &["[abc def gh ]|"],
            &shift_pressed(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["|"], &["|"], &shift_pressed(), move_caret_up)?;
        assert_move(&["|a"], &["|a"], &shift_pressed(), move_caret_up)?;
        assert_move(&["A|"], &["|[A]"], &shift_pressed(), move_caret_up)?;
        assert_move(&["a|bc"], &["|[a]bc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["ab|c"], &["|[ab]c"], &shift_pressed(), move_caret_up)?;
        assert_move(&["abc|"], &["|[abc]"], &shift_pressed(), move_caret_up)?;
        assert_move(
            &["|abc\n", "def"],
            &["|abc\n", "def"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "|def"],
            &["|[abc\n", "]def"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "d|ef"],
            &["a|[bc\n", "d]ef"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de|f"],
            &["ab|[c\n", "de]f"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def|"],
            &["abc|[\n", "def]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def \n", "|ghi"],
            &["abc\n", "|[def \n", "]ghi"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def \n", "g|hi"],
            &["abc\n", "d|[ef \n", "g]hi"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def \n", "gh|i"],
            &["abc\n", "de|[f \n", "gh]i"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def \n", "ghi|"],
            &["abc\n", "def|[ \n", "ghi]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de\n", "ghi|"],
            &["abc\n", "de|[\n", "ghi]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de|"],
            &["ab|[c\n", "de]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "d|e"],
            &["a|[bc\n", "d]e"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "|de"],
            &["|[abc\n", "]de"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqrst|"],
            &["ab\n", "cdef\n", "ghijkl|[\n", "mnopqrst]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &["ab\n", "cdef|[\n", "ghijkl]\n", "mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "|mnopqrst"],
            &["ab\n", "cdef\n", "|[ghijkl\n", "]mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &[" ab\n", " |cdef\n", "ghijkl\n", "mnopqrst"],
            &[" |[ab\n", " ]cdef\n", "ghijkl\n", "mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqr|st"],
            &["ab\n", "cdef\n", "ghijkl|[\n", "mnopqr]st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cde|f\n", "ghijkl\n", "mnopqrst"],
            &["ab|[\n", "cde]f\n", "ghijkl\n", "mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            &["abcdefgh\n", "ijklmn\n", "op|[qr\n", "st]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &["abcdefgh\n", "ijkl|[mn\n", "opqr]\n", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &["abcdef|[gh\n", "ijklmn]\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &["|[abcdefgh]\n", "ijklmn\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefg|h\n", "ijklmn\n", "opqr\n", "st"],
            &["|[abcdefg]h\n", "ijklmn\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["a|bcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &["|[a]bcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &["|abcdefgh\n", "ijklmn\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc def gh |"],
            &["|[abc def gh ]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc de|f gh "],
            &["|[abc de]f gh "],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab|c def gh "],
            &["|[ab]c def gh "],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["a|bc def gh "],
            &["|[a]bc def gh "],
            &shift_pressed(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_home() -> Result<(), String> {
        let move_caret_home = BigSelectableText::move_caret_home;
        assert_move(&["|"], &["|"], &shift_pressed(), move_caret_home)?;
        assert_move(&["|a"], &["|a"], &shift_pressed(), move_caret_home)?;
        assert_move(&["a|"], &["|[a]"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" a|"], &[" |[a]"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" |a"], &["|[ ]a"], &shift_pressed(), move_caret_home)?;
        assert_move(&["\ta|"], &["\t|[a]"], &shift_pressed(), move_caret_home)?;
        assert_move(&["abc|"], &["|[abc]"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" abc|"], &[" |[abc]"], &shift_pressed(), move_caret_home)?;
        assert_move(
            &["\tabc|"],
            &["\t|[abc]"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(&["ab|c"], &["|[ab]c"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" ab|c"], &[" |[ab]c"], &shift_pressed(), move_caret_home)?;
        assert_move(
            &["\tab|c"],
            &["\t|[ab]c"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &[" abc def ghi|"],
            &[" |[abc def ghi]"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc def ghi|"],
            &["|[abc def ghi]"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc def |ghi"],
            &["|[abc def ]ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;

        assert_move(
            &["abc\n", "def\n", "ghi|"],
            &["abc\n", "def\n", "|[ghi]"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "def|\n", "ghi"],
            &["abc\n", "|[def]\n", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc|\n", "def\n", "ghi"],
            &["|[abc]\n", "def\n", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["|abc\n", "def\n", "ghi"],
            &["|abc\n", "def\n", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "|def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "def\n", "|ghi"],
            &["abc\n", "def\n", "|ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &[" |abc\n", "def\n", "ghi"],
            &["|[ ]abc\n", "def\n", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", " |def\n", "ghi"],
            &["abc\n", "|[ ]def\n", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "def\n", " |ghi"],
            &["abc\n", "def\n", "|[ ]ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["| abc\n", "def\n", "ghi"],
            &["[ ]|abc\n", "def\n", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "| def\n", "ghi"],
            &["abc\n", "[ ]|def\n", "ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "def\n", "| ghi"],
            &["abc\n", "def\n", "[ ]|ghi"],
            &shift_pressed(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn start_selection_end() -> Result<(), String> {
        let move_caret_end = BigSelectableText::move_caret_end;
        assert_move(&["|"], &["|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["|a"], &["[a]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["a|"], &["a|"], &shift_pressed(), move_caret_end)?;
        assert_move(&[" a |"], &[" a |"], &shift_pressed(), move_caret_end)?;
        assert_move(&["| a "], &["[ a ]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["|abc"], &["[abc]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["|abc\t"], &["[abc\t]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["|abc "], &["[abc ]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["| abc "], &["[ abc ]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&[" abc| "], &[" abc[ ]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&[" ab|c"], &[" ab[c]|"], &shift_pressed(), move_caret_end)?;
        assert_move(
            &["abc\n", "def\n", "|ghi"],
            &["abc\n", "def\n", "[ghi]|"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abc\n", "|def\n", "ghi"],
            &["abc\n", "[def]|\n", "ghi"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["|abc\n", "def\n", "ghi"],
            &["[abc]|\n", "def\n", "ghi"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abc\n", "def\n", "|ghi "],
            &["abc\n", "def\n", "[ghi ]|"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abc\n", "|def \n", "ghi"],
            &["abc\n", "[def ]|\n", "ghi"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["|abc \n", "def\n", "ghi"],
            &["[abc ]|\n", "def\n", "ghi"],
            &shift_pressed(),
            move_caret_end,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["[A]|"], &["A|"], &no_mods(), move_caret_right)?;
        assert_move(&["[a]|bc"], &["a|bc"], &no_mods(), move_caret_right)?;
        assert_move(&["a[b]|c"], &["ab|c"], &no_mods(), move_caret_right)?;
        assert_move(&["ab[c]|"], &["abc|"], &no_mods(), move_caret_right)?;
        assert_move(&["[ ]|abc"], &[" |abc"], &no_mods(), move_caret_right)?;
        assert_move(&["|[ ]abc"], &[" |abc"], &no_mods(), move_caret_right)?;
        assert_move(&["a|[b]c"], &["ab|c"], &no_mods(), move_caret_right)?;
        assert_move(
            &["abc[\n", "]|d"],
            &["abc\n", "|d"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc|[\n", "]d"],
            &["abc\n", "|d"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc|[\n", "]"],
            &["abc\n", "|"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "[d]|ef"],
            &["abc\n", "d|ef"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            &["abc\n", "def\n", "ghi\n", "|jkl"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(&["[ab]|c"], &["ab|c"], &no_mods(), move_caret_right)?;
        assert_move(&["[abc]|"], &["abc|"], &no_mods(), move_caret_right)?;
        assert_move(
            &["ab|[c\n", "]def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["ab[c\n", "]|def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["a|[bc\n", "]def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["|[abc\n", "]def\n", "ghi"],
            &["abc\n", "|def\n", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["a|[bc\n", "d]ef\n", "ghi"],
            &["abc\n", "d|ef\n", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["|[abc\n", "def]\n", "ghi"],
            &["abc\n", "def|\n", "ghi"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqrst|"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["|[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]"],
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqrst|"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["ab\n", "c[def\n", "ghijkl\n", "mno]|pqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "mno|pqrst"],
            &no_mods(),
            move_caret_right,
        )?;
        assert_move(
            &["ab\n", "c|[def\n", "ghijkl\n", "mno]pqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "mno|pqrst"],
            &no_mods(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["[A]|"], &["|A"], &no_mods(), move_caret_left)?;
        assert_move(&["[a]|bc"], &["|abc"], &no_mods(), move_caret_left)?;
        assert_move(&["a[b]|c"], &["a|bc"], &no_mods(), move_caret_left)?;
        assert_move(&["ab[c]|"], &["ab|c"], &no_mods(), move_caret_left)?;
        assert_move(&["[ ]|abc"], &["| abc"], &no_mods(), move_caret_left)?;
        assert_move(&["|[ ]abc"], &["| abc"], &no_mods(), move_caret_left)?;
        assert_move(&["a|[b]c"], &["a|bc"], &no_mods(), move_caret_left)?;
        assert_move(
            &["abc[\n", "]|d"],
            &["abc|\n", "d"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc|[\n", "]d"],
            &["abc|\n", "d"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc|[\n", "]"],
            &["abc|\n", ""],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "[d]|ef"],
            &["abc\n", "|def"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(&["[ab]|c"], &["|abc"], &no_mods(), move_caret_left)?;
        assert_move(&["[abc]|"], &["|abc"], &no_mods(), move_caret_left)?;
        assert_move(
            &["ab|[c\n", "]def\n", "ghi"],
            &["ab|c\n", "def\n", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["ab[c\n", "]|def\n", "ghi"],
            &["ab|c\n", "def\n", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["a|[bc\n", "]def\n", "ghi"],
            &["a|bc\n", "def\n", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["|[abc\n", "]def\n", "ghi"],
            &["|abc\n", "def\n", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["a|[bc\n", "d]ef\n", "ghi"],
            &["a|bc\n", "def\n", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["|[abc\n", "def]\n", "ghi"],
            &["|abc\n", "def\n", "ghi"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &["|ab\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["|[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]"],
            &["|ab\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["ab\n", "c[def\n", "ghijkl\n", "mno]|pqrst"],
            &["ab\n", "c|def\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_left,
        )?;
        assert_move(
            &["ab\n", "c|[def\n", "ghijkl\n", "mno]pqrst"],
            &["ab\n", "c|def\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["[a]|"], &["a|"], &no_mods(), move_caret_down)?;
        assert_move(&["|[a]"], &["a|"], &no_mods(), move_caret_down)?;
        assert_move(&["a|[bc]"], &["abc|"], &no_mods(), move_caret_down)?;
        assert_move(&["ab[c]|"], &["abc|"], &no_mods(), move_caret_down)?;
        assert_move(&["abc|[ ]"], &["abc |"], &no_mods(), move_caret_down)?;
        assert_move(
            &["abc\n", "|[def]"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "d|[ef]"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de|[f]"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["[abc\n", "]|def"],
            &["abc\n", "|def"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a[bc\n", "d]|ef"],
            &["abc\n", "d|ef"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|[c\n", "de]f"],
            &["abc\n", "de|f"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc[\n", "def]|"],
            &["abc\n", "def|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "|[def \n", "]ghi"],
            &["abc\n", "def \n", "|ghi"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "d[ef \n", "g]|hi"],
            &["abc\n", "def \n", "g|hi"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de[f \n", "gh]|i"],
            &["abc\n", "def \n", "gh|i"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "def[ \n", "ghi]|"],
            &["abc\n", "def \n", "ghi|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "def [\n", "ghi]|"],
            &["abc\n", "def \n", "ghi|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de[\n", "gh]|i"],
            &["abc\n", "de\n", "gh|i"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc|[\n", "de]"],
            &["abc\n", "de|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab[c\n", "de]|"],
            &["abc\n", "de|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a|[bc\n", "d]e"],
            &["abc\n", "d|e"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["[abc\n", "]|de"],
            &["abc\n", "|de"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab[\n", "cd]|ef\n", "ghijkl\n", "mnopqrst"],
            &["ab\n", "cd|ef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef|[\n", "ghij]kl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "ghij|kl\n", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl[\n", "mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl\n", "mnopqr|st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &[" [ab\n", " ]|cdef\n", "ghijkl\n", "mnopqrst"],
            &[" ab\n", " |cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "|[cdef\n", "]ghijkl\n", "mnopqrst"],
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "[ghijkl\n", "]|mnopqrst"],
            &["ab\n", "cdef\n", "ghijkl\n", "|mnopqrst"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh[\n", "ijklmn]|\n", "opqr\n", "st"],
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn[\n", "opqr]|\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr[\n", "st]|"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "[st]|"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "st|"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["abc de[f gh ]|"],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|[c def gh ]"],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["a[bc def gh ]|"],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;
        assert_move(
            &["[abc def gh ]|"],
            &["abc def gh |"],
            &no_mods(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["[a]|"], &["|a"], &no_mods(), move_caret_up)?;
        assert_move(&["|[a]"], &["|a"], &no_mods(), move_caret_up)?;
        assert_move(&["a|[bc]"], &["a|bc"], &no_mods(), move_caret_up)?;
        assert_move(&["ab[c]|"], &["ab|c"], &no_mods(), move_caret_up)?;
        assert_move(&["abc|[ ]"], &["abc| "], &no_mods(), move_caret_up)?;
        assert_move(
            &["abc\n", "|[def]"],
            &["abc\n", "|def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "d|[ef]"],
            &["abc\n", "d|ef"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de|[f]"],
            &["abc\n", "de|f"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["[abc\n", "]|def"],
            &["|abc\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a[bc\n", "d]|ef"],
            &["a|bc\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab|[c\n", "de]f"],
            &["ab|c\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc[\n", "def]|"],
            &["abc|\n", "def"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "|[def \n", "]ghi"],
            &["abc\n", "|def \n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "d[ef \n", "g]|hi"],
            &["abc\n", "d|ef \n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de|[f \n", "gh]i"],
            &["abc\n", "de|f \n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def[ \n", "ghi]|"],
            &["abc\n", "def| \n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def [\n", "ghi]|"],
            &["abc\n", "def |\n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de[\n", "gh]|i"],
            &["abc\n", "de|\n", "ghi"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc|[\n", "de]"],
            &["abc|\n", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab[c\n", "de]|"],
            &["ab|c\n", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a|[bc\n", "d]e"],
            &["a|bc\n", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["[abc\n", "]|de"],
            &["|abc\n", "de"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab[\n", "cd]|ef\n", "ghijkl\n", "mnopqrst"],
            &["ab|\n", "cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef|[\n", "ghij]kl\n", "mnopqrst"],
            &["ab\n", "cdef|\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl[\n", "mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl|\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &[" [ab\n", " ]|cdef\n", "ghijkl\n", "mnopqrst"],
            &[" |ab\n", " cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "|[cdef\n", "]ghijkl\n", "mnopqrst"],
            &["ab\n", "|cdef\n", "ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "[ghijkl\n", "]|mnopqrst"],
            &["ab\n", "cdef\n", "|ghijkl\n", "mnopqrst"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh[\n", "ijklmn]|\n", "opqr\n", "st"],
            &["abcdefgh|\n", "ijklmn\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn[\n", "opqr]|\n", "st"],
            &["abcdefgh\n", "ijklmn|\n", "opqr\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr[\n", "st]|"],
            &["abcdefgh\n", "ijklmn\n", "opqr|\n", "st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "[st]|"],
            &["abcdefgh\n", "ijklmn\n", "opqr\n", "|st"],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["abc de[f gh ]|"],
            &["abc de|f gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["ab|[c def gh ]"],
            &["ab|c def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["a[bc def gh ]|"],
            &["a|bc def gh "],
            &no_mods(),
            move_caret_up,
        )?;
        assert_move(
            &["[abc def gh ]|"],
            &["|abc def gh "],
            &no_mods(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_home() -> Result<(), String> {
        let move_caret_home = BigSelectableText::move_caret_home;
        assert_move(&["[a]|"], &["|a"], &no_mods(), move_caret_home)?;
        assert_move(&["|[a]"], &["|a"], &no_mods(), move_caret_home)?;
        assert_move(&[" |[a]"], &["| a"], &no_mods(), move_caret_home)?;
        assert_move(&["|[ a]"], &[" |a"], &no_mods(), move_caret_home)?;
        assert_move(&[" [a]|"], &[" |a"], &no_mods(), move_caret_home)?;
        assert_move(&[" a[bc ]|"], &[" |abc "], &no_mods(), move_caret_home)?;
        assert_move(&["\t[abc ]|"], &["\t|abc "], &no_mods(), move_caret_home)?;
        assert_move(&["\t|[abc] "], &["|\tabc "], &no_mods(), move_caret_home)?;
        assert_move(&["|[\tabc] "], &["\t|abc "], &no_mods(), move_caret_home)?;
        assert_move(
            &["[ abc def\tghi]|"],
            &[" |abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &[" |[abc] def\tghi"],
            &["| abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["|[ abc def\tghi]"],
            &[" |abc def\tghi"],
            &no_mods(),
            move_caret_home,
        )?;

        assert_move(
            &["abc\n", "d[e]|\n", "ghi"],
            &["abc\n", "|de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", " [d]|e\n", "ghi"],
            &["abc\n", " |de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["[abc\n", "]| de\n", "ghi"],
            &["abc\n", " |de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", " |[de\n]", "ghi"],
            &["abc\n", "| de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;
        assert_move(
            &["abc|[\n", "de\n", "ghi]"],
            &["|abc\n", "de\n", "ghi"],
            &no_mods(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn end_selection_end() -> Result<(), String> {
        let move_caret_end = BigSelectableText::move_caret_end;
        assert_move(&["|[a]"], &["a|"], &no_mods(), move_caret_end)?;
        assert_move(&["[a]|"], &["a|"], &no_mods(), move_caret_end)?;
        assert_move(&[" a|[ ]"], &[" a |"], &no_mods(), move_caret_end)?;
        assert_move(&["[ a]| "], &[" a |"], &no_mods(), move_caret_end)?;
        assert_move(&[" [a]| "], &[" a |"], &no_mods(), move_caret_end)?;
        assert_move(&["[ a ]|"], &[" a |"], &no_mods(), move_caret_end)?;
        assert_move(&["|[ a]bc "], &[" abc |"], &no_mods(), move_caret_end)?;
        assert_move(&["|[\tabc] "], &["\tabc |"], &no_mods(), move_caret_end)?;
        assert_move(
            &[" abc d|[ef\tg]hi"],
            &[" abc def\tghi|"],
            &no_mods(),
            move_caret_end,
        )?;

        assert_move(
            &["abc\n", "|[de\n", "ghi]"],
            &["abc\n", "de|\n", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["[abc\n", " d]|e\n", "ghi"],
            &["abc\n", " de|\n", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["|[abc\n", "de\n", "ghi]"],
            &["abc|\n", "de\n", "ghi"],
            &no_mods(),
            move_caret_end,
        )?;
        assert_move(
            &["abc\n", "de\n", "g|[hi]"],
            &["abc\n", "de\n", "ghi|"],
            &no_mods(),
            move_caret_end,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["[a]|bc"], &["[ab]|c"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a[b]|c"], &["a[bc]|"], &shift_pressed(), move_caret_right)?;
        assert_move(&["[ab]|c"], &["[abc]|"], &shift_pressed(), move_caret_right)?;
        assert_move(
            &["[ ]|abc"],
            &["[ a]|bc"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(&["[abc]|"], &["[abc]|"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a[bc]|"], &["a[bc]|"], &shift_pressed(), move_caret_right)?;
        assert_move(&["ab[c]|"], &["ab[c]|"], &shift_pressed(), move_caret_right)?;
        assert_move(
            &["abc[\n", "]|d"],
            &["abc[\n", "d]|"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["ab[c]|\n", ""],
            &["ab[c\n", "]|"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["ab[c]|\n", "d"],
            &["ab[c\n", "]|d"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            &["abc\n", "def\n", "ghi[\n", "j]|kl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["ab[c\n", "def\n", "ghi\n", "]|jkl"],
            &["ab[c\n", "def\n", "ghi\n", "j]|kl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["ab[c\n", "def\n", "]|ghi\n", "jkl"],
            &["ab[c\n", "def\n", "g]|hi\n", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jk]|l"],
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &shift_pressed(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["ab|[c]"], &["a|[bc]"], &shift_pressed(), move_caret_left)?;
        assert_move(&["a|[bc]"], &["|[abc]"], &shift_pressed(), move_caret_left)?;
        assert_move(&["|[abc]"], &["|[abc]"], &shift_pressed(), move_caret_left)?;
        assert_move(&["|[ab]c"], &["|[ab]c"], &shift_pressed(), move_caret_left)?;
        assert_move(&["|[a]bc"], &["|[a]bc"], &shift_pressed(), move_caret_left)?;
        assert_move(
            &[" |[a]bc"],
            &["|[ a]bc"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc|[\n", "]d"],
            &["ab|[c\n", "]d"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "|[d]"],
            &["abc|[\n", "d]"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["ab|[c\n", "]"],
            &["a|[bc\n", "]"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def|[\n", "ghi\n", "j]kl"],
            &["abc\n", "de|[f\n", "ghi\n", "j]kl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["a|[bc\n", "def\n", "ghi\n", "jkl]"],
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi\n", "|[jkl]"],
            &["abc\n", "def\n", "ghi|[\n", "jkl]"],
            &shift_pressed(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["ab|[c]"], &["|[abc]"], &shift_pressed(), move_caret_up)?;
        assert_move(&["a|[bc]"], &["|[abc]"], &shift_pressed(), move_caret_up)?;
        assert_move(&["|[abc]"], &["|[abc]"], &shift_pressed(), move_caret_up)?;
        assert_move(&["|[ab]c"], &["|[ab]c"], &shift_pressed(), move_caret_up)?;
        assert_move(&["|[a]bc"], &["|[a]bc"], &shift_pressed(), move_caret_up)?;
        assert_move(&[" |[a]bc"], &["|[ a]bc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["ab[c]|"], &["|[ab]c"], &shift_pressed(), move_caret_up)?;
        assert_move(&["[a]|"], &["|a"], &shift_pressed(), move_caret_up)?;
        assert_move(&["[a]|bc"], &["|abc"], &shift_pressed(), move_caret_up)?;
        assert_move(
            &["[a]|bc\n", "d"],
            &["|abc\n", "d"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de[f]|"],
            &["abc|[\n", "de]f"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "de|[f]"],
            &["ab|[c\n", "def]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab|[c\n", "def]"],
            &["|[abc\n", "def]"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl|[\n", "]mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqrs]|t"],
            &["ab\n", "cdef\n", "ghijkl|[\n", "]mnopqrst"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "|[o]pqr\n", "st"],
            &["abcdefgh\n", "|[ijklmn\n", "o]pqr\n", "st"],
            &shift_pressed(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["[ab]|c"], &["[abc]|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["[a]|bc"], &["[abc]|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["[abc]|"], &["[abc]|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["|[ab]c"], &["ab[c]|"], &shift_pressed(), move_caret_down)?;
        assert_move(&["|[a]bc"], &["a[bc]|"], &shift_pressed(), move_caret_down)?;
        assert_move(
            &["[a]|bc\n", "d"],
            &["[abc\n", "d]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["[a]|bc\n", "de"],
            &["[abc\n", "d]|e"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["[abc\n", "d]|e"],
            &["[abc\n", "de]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["[a]|bc\n", ""],
            &["[abc\n", "]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqrst]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a[b\n", "cdef\n", "ghijkl\n", "mnopqr]|st"],
            &["a[b\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcd[efgh]|\n", "ijklmn\n", "opqr\n", "st"],
            &["abcd[efgh\n", "ijklmn]|\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcd[e]|fgh\n", "ijklmn\n", "opqr\n", "st"],
            &["abcd[efgh\n", "ijklm]|n\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_home() -> Result<(), String> {
        let move_caret_home = SelectableLines::move_caret_home;

        assert_move(&["ab|[c]"], &["|[abc]"], &shift_pressed(), move_caret_home)?;
        assert_move(&["a|[bc]"], &["|[abc]"], &shift_pressed(), move_caret_home)?;
        assert_move(&["|[abc]"], &["|[abc]"], &shift_pressed(), move_caret_home)?;
        assert_move(&["|[ab]c"], &["|[ab]c"], &shift_pressed(), move_caret_home)?;
        assert_move(&["|[a]bc"], &["|[a]bc"], &shift_pressed(), move_caret_home)?;
        assert_move(
            &[" |[a]bc"],
            &["|[ a]bc"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(&["ab[c]|"], &["|[ab]c"], &shift_pressed(), move_caret_home)?;
        assert_move(
            &["abc\n", "de[f]|"],
            &["abc\n", "|[de]f"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "de|[f]"],
            &["abc\n", "|[def]"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["ab|[c\n", "def]"],
            &["|[abc\n", "def]"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &[" ab|[c\n", "def]"],
            &[" |[abc\n", "def]"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &[" |[abc\n", "def]"],
            &["|[ abc\n", "def]"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "|[mnopqr]st"],
            &["ab\n", "cdef\n", "ghijkl\n", "|[mnopqr]st"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "gh|[ijkl]\n", "mnopqrst"],
            &["ab\n", "cdef\n", "|[ghijkl]\n", "mnopqrst"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abcdefgh\n", "ijklmn\n", "op[qr]|\n", "st"],
            &["abcdefgh\n", "ijklmn\n", "|[op]qr\n", "st"],
            &shift_pressed(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn extend_selection_end() -> Result<(), String> {
        let move_caret_end = SelectableLines::move_caret_end;

        assert_move(&["[ab]|c"], &["[abc]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["[a]|bc"], &["[abc]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["[abc]|"], &["[abc]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["|[ab]c"], &["ab[c]|"], &shift_pressed(), move_caret_end)?;
        assert_move(&["|[a]bc"], &["a[bc]|"], &shift_pressed(), move_caret_end)?;
        assert_move(
            &["[a]|bc\n", "d"],
            &["[abc]|\n", "d"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["[a]|bc \n", "de"],
            &["[abc ]|\n", "de"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["[abc\n", "d]|e"],
            &["[abc\n", "de]|"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqr]|st"],
            &["ab\n", "cdef\n", "ghijkl\n", "[mnopqrst]|"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["a[b\n", "cdef\n", "ghijkl\n", "mnopqr]|st"],
            &["a[b\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &["[ab\n", "cdef\n", "ghijkl\n", "mnopqrst]|"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abcd[e]|fgh\n", "ijklmn\n", "opqr\n", "st"],
            &["abcd[efgh]|\n", "ijklmn\n", "opqr\n", "st"],
            &shift_pressed(),
            move_caret_end,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_right() -> Result<(), String> {
        let move_caret_right = SelectableLines::move_caret_right;

        assert_move(&["ab|[c]"], &["abc|"], &shift_pressed(), move_caret_right)?;
        assert_move(&["a|[bc]"], &["ab|[c]"], &shift_pressed(), move_caret_right)?;
        assert_move(&["|[abc]"], &["a|[bc]"], &shift_pressed(), move_caret_right)?;
        assert_move(
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            &["a|[bc\n", "def\n", "ghi\n", "jkl]"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "d|[ef\n", "]ghi\n", "jkl"],
            &["abc\n", "de|[f\n", "]ghi\n", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;
        assert_move(
            &["abc\n", "de|[f]\n", "ghi\n", "jkl"],
            &["abc\n", "def|\n", "ghi\n", "jkl"],
            &shift_pressed(),
            move_caret_right,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_left() -> Result<(), String> {
        let move_caret_left = SelectableLines::move_caret_left;

        assert_move(&["ab[c]|"], &["ab|c"], &shift_pressed(), move_caret_left)?;
        assert_move(&["a[bc]|"], &["a[b]|c"], &shift_pressed(), move_caret_left)?;
        assert_move(&["[abc]|"], &["[ab]|c"], &shift_pressed(), move_caret_left)?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &["[abc\n", "def\n", "ghi\n", "jk]|l"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            &["|[abc\n", "def\n", "ghi\n", "jkl]"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "def[\n", "]|ghi\n", "jkl"],
            &["abc\n", "def|\n", "ghi\n", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;
        assert_move(
            &["abc\n", "d[ef\n", "gh]|i\n", "jkl"],
            &["abc\n", "d[ef\n", "g]|hi\n", "jkl"],
            &shift_pressed(),
            move_caret_left,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_up() -> Result<(), String> {
        let move_caret_up = SelectableLines::move_caret_up;

        assert_move(&["[abc]|"], &["|abc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["[ab]|c"], &["|abc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["[a]|bc"], &["|abc"], &shift_pressed(), move_caret_up)?;
        assert_move(&["|abc"], &["|abc"], &shift_pressed(), move_caret_up)?;
        assert_move(
            &["[abc\n", "def]|"],
            &["[abc]|\n", "def"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["[abc\n", "de]|f"],
            &["[ab]|c\n", "def"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &["[abc\n", "def\n", "ghi]|\n", "jkl"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "jkl]|"],
            &["abc\n", "def\n", "ghi|\n", "jkl"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["abc\n", "d[ef\n", "ghi\n", "jk]|l"],
            &["abc\n", "d[ef\n", "gh]|i\n", "jkl"],
            &shift_pressed(),
            move_caret_up,
        )?;
        assert_move(
            &["[abc\n", "d]|ef\n", "ghi\n", "jkl"],
            &["[a]|bc\n", "def\n", "ghi\n", "jkl"],
            &shift_pressed(),
            move_caret_up,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_down() -> Result<(), String> {
        let move_caret_down = SelectableLines::move_caret_down;

        assert_move(&["|[abc]"], &["abc|"], &shift_pressed(), move_caret_down)?;
        assert_move(
            &["|[abc\n", "def]"],
            &["abc\n", "|[def]"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["a|[bc\n", "def]"],
            &["abc\n", "d|[ef]"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["|[abc\n", "def\n", "ghi]"],
            &["abc\n", "|[def\n", "ghi]"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|[c\n", "def\n", "ghi]"],
            &["abc\n", "de|[f\n", "ghi]"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abc\n", "de|[f\n", "ghi]"],
            &["abc\n", "def\n", "gh|[i]"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcdef|[\n", "ghij\n", "kl]"],
            &["abcdef\n", "ghij|[\n", "kl]"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["abcde|[f\n", "ghij\n", "kl]"],
            &["abcdef\n", "ghij|[\n", "kl]"],
            &shift_pressed(),
            move_caret_down,
        )?;
        assert_move(
            &["ab|[cdef\n", "ghij\n", "kl]"],
            &["abcdef\n", "gh|[ij\n", "kl]"],
            &shift_pressed(),
            move_caret_down,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_home() -> Result<(), String> {
        let move_caret_home = SelectableLines::move_caret_home;

        assert_move(&["[abc]|"], &["|abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&["[ab]|c"], &["|abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&["[a]|bc"], &["|abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" [abc]|"], &[" |abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" [ab]|c"], &[" |abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&[" [a]|bc"], &[" |abc"], &shift_pressed(), move_caret_home)?;
        assert_move(&["ab[c]|"], &["|[ab]c"], &shift_pressed(), move_caret_home)?;
        assert_move(&["a[b]|c"], &["|[a]bc"], &shift_pressed(), move_caret_home)?;
        assert_move(&["a[bc]|"], &["|[a]bc"], &shift_pressed(), move_caret_home)?;

        assert_move(
            &["[abc\n", "def]|"],
            &["[abc\n", "]|def"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["[abc\n", " de]|f"],
            &["[abc\n", " ]|def"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["[abc\n", "def\n", "ghi\n", "jkl]|"],
            &["[abc\n", "def\n", "ghi\n", "]|jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "def\n", "ghi[\n", "jkl]|"],
            &["abc\n", "def\n", "ghi[\n", "]|jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["abc\n", "d[ef\n", " ghi\n", " jk]|l"],
            &["abc\n", "d[ef\n", " ghi\n", " ]|jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["[abc\n", "d]|ef\n", "ghi\n", "jkl"],
            &["[abc\n", "]|def\n", "ghi\n", "jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;
        assert_move(
            &["[abc\n", "d]|ef\n", "ghi\n", "jkl"],
            &["[abc\n", "]|def\n", "ghi\n", "jkl"],
            &shift_pressed(),
            move_caret_home,
        )?;

        Ok(())
    }

    #[test]
    fn shrink_selection_end() -> Result<(), String> {
        let move_caret_end = SelectableLines::move_caret_end;

        assert_move(&["|[abc]"], &["abc|"], &shift_pressed(), move_caret_end)?;
        assert_move(
            &["|[abc\n", "def]"],
            &["abc|[\n", "def]"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["a|[bc\n", "def]"],
            &["abc|[\n", "def]"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["a|[bc\n", "def\n", "ghi]"],
            &["abc|[\n", "def\n", "ghi]"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["ab|[c\n", "def\n", "ghi]"],
            &["abc|[\n", "def\n", "ghi]"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abc\n", "de|[f\n", "ghi]"],
            &["abc\n", "def|[\n", "ghi]"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abcdef|[\n", "ghij\n", "kl]"],
            &["abcdef|[\n", "ghij\n", "kl]"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["|[ abcdef\n", "ghij\n", "kl]"],
            &[" abcdef|[\n", "ghij\n", "kl]"],
            &shift_pressed(),
            move_caret_end,
        )?;
        assert_move(
            &["abcdef\n", "ghij\n", "|[kl]"],
            &["abcdef\n", "ghij\n", "kl|"],
            &shift_pressed(),
            move_caret_end,
        )?;

        Ok(())
    }
}
