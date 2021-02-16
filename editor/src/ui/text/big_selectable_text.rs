// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

use crate::ui::ui_error::{
    UIError::{FileOpenFailed, TextBufReadFailed},
    UIResult,
    OutOfBounds,
};
use crate::ui::text::{
    text_pos::{TextPos},
    selection::{Selection, RawSelection, validate_raw_sel},
    caret_w_select::{CaretWSelect},
    lines::{Lines, SelectableLines, MutSelectableLines},
};
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use ropey::Rope;
use snafu::{ensure};
use std::{
    fmt,
    fs::File,
    io,
    path::Path
};

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

    fn sel_to_tup(&self, val_sel: Selection) -> UIResult<(usize, usize)> {
        let start_char_indx = self.pos_to_char_indx(val_sel.start_pos);
        let end_char_indx = self.pos_to_char_indx(val_sel.end_pos);

        Ok((start_char_indx, end_char_indx))
    }
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

    fn get_selection(&self) -> Option<Selection> {
        self.caret_w_select.selection_opt
    }

    fn is_selection_active(&self) -> bool {
        self.get_selection().is_some()
    }

    fn get_selected_str(&self) -> UIResult<Option<&str>> {
        if let Some(val_sel) = self.caret_w_select.selection_opt {
            let (start_char_indx, end_char_indx) = self.sel_to_tup(val_sel)?;

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

    fn last_text_pos(&self) -> TextPos {
        self.char_indx_to_pos(self.nr_of_chars())
    }
}

impl MutSelectableLines for BigSelectableText {
    fn insert_char(&mut self, new_char: &char) -> UIResult<()> {
        self.insert_str(&new_char.to_string())
    }

    fn insert_str(&mut self, new_str: &str) -> UIResult<()> {
        let caret_pos = self.caret_w_select.caret_pos;
        let char_indx = self.pos_to_char_indx(caret_pos);

        self.check_bounds(char_indx)?;

        self.text_rope.insert(char_indx, new_str);

        Ok(())
    }

    fn pop_char(&mut self) {
        let caret_pos = self.caret_w_select.caret_pos;
        let char_indx = self.pos_to_char_indx(caret_pos);

        if (char_indx > 0) && char_indx <= self.text_rope.len_chars() {
            self.text_rope.remove((char_indx - 1)..char_indx);
        }
    }

    fn del_selection(&mut self) -> UIResult<()> {
        if let Some(selection) = self.caret_w_select.selection_opt {
            let (start_char_indx, end_char_indx) = self.sel_to_tup(selection)?;

            self.check_bounds(end_char_indx)?;
    
            self.text_rope.remove(start_char_indx..end_char_indx);

            self.set_caret(selection.start_pos);
        }

        Ok(())
    }
}

pub fn from_path(path: &Path) -> UIResult<BigSelectableText> {
    let caret_w_select = CaretWSelect::default();
    let text_rope = rope_from_path(path)?;
    let path_str = path_to_string(path);
    let arena = Bump::new();

    Ok(BigSelectableText {
        caret_w_select,
        text_rope,
        path_str,
        arena,
    })
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
