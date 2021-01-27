// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

use crate::error::EdError::{FileOpenFailed, TextBufReadFailed};
use crate::error::EdResult;
use crate::error::OutOfBounds;
use crate::mvc::ed_model::{Position, RawSelection};
use crate::selection::validate_selection;
use bumpalo::collections::String as BumpString;
use bumpalo::Bump;
use ropey::Rope;
use snafu::{ensure, OptionExt};
use std::fmt;
use std::fs::File;
use std::io;
use std::path::Path;

pub struct TextBuffer {
    pub text_rope: Rope,
    pub path_str: String,
    pub mem_arena: Bump,
}

impl TextBuffer {
    pub fn insert_char(&mut self, caret_pos: Position, new_char: &char) -> EdResult<()> {
        self.insert_str(caret_pos, &new_char.to_string())
    }

    pub fn  insert_str(&mut self, caret_pos: Position, new_str: &str) -> EdResult<()> {
        let char_indx = self.pos_to_char_indx(caret_pos);

        self.check_bounds(char_indx)?;

        self.text_rope.insert(char_indx, new_str);

        Ok(())
    }

    pub fn pop_char(&mut self, caret_pos: Position) {
        let char_indx = self.pos_to_char_indx(caret_pos);

        if (char_indx > 0) && char_indx <= self.text_rope.len_chars() {
            self.text_rope.remove((char_indx - 1)..char_indx);
        }
    }

    pub fn del_selection(&mut self, raw_sel: RawSelection) -> EdResult<()> {
        let (start_char_indx, end_char_indx) = self.sel_to_tup(raw_sel)?;

        self.check_bounds(end_char_indx)?;

        self.text_rope.remove(start_char_indx..end_char_indx);

        Ok(())
    }

    pub fn get_selection(&self, raw_sel: RawSelection) -> EdResult<&str> {
        let (start_char_indx, end_char_indx) = self.sel_to_tup(raw_sel)?;

        self.check_bounds(end_char_indx)?;

        let rope_slice = self.text_rope.slice(start_char_indx..end_char_indx);

        if let Some(line_str_ref) = rope_slice.as_str() {
            Ok(line_str_ref)
        } else {
            // happens very rarely
            let line_str = rope_slice.chunks().collect::<String>();
            let arena_str_ref = self.mem_arena.alloc(line_str);
            Ok(arena_str_ref)
        }
    }

    fn check_bounds(&self, char_indx: usize) -> EdResult<()> {
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

    pub fn line(&self, line_nr: usize) -> Option<&str> {
        if line_nr < self.nr_of_lines() {
            let rope_slice = self.text_rope.line(line_nr);

            if let Some(line_str_ref) = rope_slice.as_str() {
                Some(line_str_ref)
            } else {
                // happens very rarely
                let line_str = rope_slice.chunks().collect::<String>();
                let arena_str_ref = self.mem_arena.alloc(line_str);
                Some(arena_str_ref)
            }
        } else {
            None
        }
    }

    pub fn line_len(&self, line_nr: usize) -> Option<usize> {
        self.line(line_nr)
            .map(
                |line| line.len()
            )
    }

    pub fn line_len_res(&self, line_nr: usize) -> EdResult<usize> {
        self.line_len(line_nr).context(OutOfBounds {
            index: line_nr,
            collection_name: "Rope",
            len: self.text_rope.len_lines(),
        })
    }

    pub fn nr_of_lines(&self) -> usize {
        self.text_rope.len_lines()
    }

    // expensive function, don't use it if it can be done with a specialized, more efficient function
    // TODO use pool allocation here
    pub fn all_lines<'a>(&self, arena: &'a Bump) -> BumpString<'a> {
        let mut lines = BumpString::with_capacity_in(self.text_rope.len_chars(), arena);

        for line in self.text_rope.lines() {
            lines.extend(line.as_str());
        }

        lines
    }

    fn pos_to_char_indx(&self, pos: Position) -> usize {
        self.text_rope.line_to_char(pos.line) + pos.column
    }

    fn sel_to_tup(&self, raw_sel: RawSelection) -> EdResult<(usize, usize)> {
        let valid_sel = validate_selection(raw_sel)?;
        let start_char_indx = self.pos_to_char_indx(valid_sel.selection.start_pos);
        let end_char_indx = self.pos_to_char_indx(valid_sel.selection.end_pos);

        Ok((start_char_indx, end_char_indx))
    }
}

pub fn from_path(path: &Path) -> EdResult<TextBuffer> {
    let text_rope = rope_from_path(path)?;
    let path_str = path_to_string(path);
    let mem_arena = Bump::new();

    Ok(TextBuffer {
        text_rope,
        path_str,
        mem_arena,
    })
}

fn path_to_string(path: &Path) -> String {
    let mut path_str = String::new();
    path_str.push_str(&path.to_string_lossy());

    path_str
}

fn rope_from_path(path: &Path) -> EdResult<Rope> {
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

impl fmt::Debug for TextBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TextBuffer")
            .field("text_rope", &self.text_rope)
            .field("path_str", &self.path_str)
            .finish()
    }
}
