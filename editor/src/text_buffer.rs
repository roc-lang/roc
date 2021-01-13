// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

use crate::error::EdError::{FileOpenFailed, TextBufReadFailed};
use crate::error::EdResult;
use crate::error::OutOfBounds;
use crate::mvc::ed_model::{Position, RawSelection};
use crate::selection::validate_selection;
use ropey::Rope;
use snafu::{ensure, OptionExt};
use std::fs::File;
use std::io;
use std::path::Path;

#[derive(Debug)]
pub struct TextBuffer {
    pub text_rope: Rope,
    pub path_str: String,
}

impl TextBuffer {
    pub fn insert_char(&mut self, caret_pos: Position, new_char: &char) -> EdResult<()> {
        let char_indx = self.pos_to_char_indx(caret_pos);

        ensure!(
            char_indx <= self.text_rope.len_chars(),
            OutOfBounds {
                index: char_indx,
                collection_name: "Rope",
                len: self.text_rope.len_chars()
            }
        );

        self.text_rope.insert(char_indx, &new_char.to_string());

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

        ensure!(
            end_char_indx <= self.text_rope.len_chars(),
            OutOfBounds {
                index: end_char_indx,
                collection_name: "Rope",
                len: self.text_rope.len_chars()
            }
        );

        self.text_rope.remove(start_char_indx..end_char_indx);

        Ok(())
    }

    pub fn line(&self, line_nr: usize) -> Option<&str> {
        if line_nr < self.text_rope.len_lines() {
            self.text_rope.line(line_nr).as_str()
        } else {
            None
        }
    }

    pub fn line_len(&self, line_nr: usize) -> Option<usize> {
        if line_nr < self.text_rope.len_lines() {
            Some(self.text_rope.line(line_nr).len_chars())
        } else {
            None
        }
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
    // TODO use bump allocation here
    pub fn all_lines(&self) -> String {
        let mut lines = String::new();

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
    // TODO benchmark different file reading methods, see #886
    let text_rope = rope_from_path(path)?;
    let path_str = path_to_string(path);

    Ok(TextBuffer {
        text_rope,
        path_str,
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
