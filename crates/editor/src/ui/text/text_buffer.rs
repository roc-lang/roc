use std::path::Path;

use crate::ui::{
    ui_error::{OutOfBoundsSnafu, TextBufReadFailedSnafu, UIResult},
    util::{path_to_string, reader_from_path},
};
use snafu::ensure;

use super::{selection::Selection, text_pos::TextPos};
use std::io::BufRead;

// Do not use for large amounts of text.
// This should become a trait in the future and be implemented by a SmallTextBuffer and Rope(for large amounts of text)
#[derive(Debug)]
pub struct TextBuffer {
    pub lines: Vec<String>,
}

impl TextBuffer {
    pub fn from_path(path: &Path) -> UIResult<Self> {
        let buf_reader = reader_from_path(path)?;
        let mut lines: Vec<String> = Vec::new();

        for line in buf_reader.lines() {
            match line {
                Ok(line_str) => lines.push(line_str),
                Err(e) => {
                    TextBufReadFailedSnafu {
                        path_str: path_to_string(path),
                        err_msg: e.to_string(),
                    }
                    .fail()?;
                }
            }
        }

        Ok(TextBuffer { lines })
    }

    pub fn nr_of_chars(&self) -> usize {
        let mut nr_of_chars = 0;

        for line in self.lines.iter() {
            nr_of_chars += line.len();
        }

        nr_of_chars
    }

    pub fn nr_of_lines(&self) -> usize {
        self.lines.len()
    }

    pub fn get_line_ref(&self, line_nr: usize) -> UIResult<&str> {
        self.ensure_bounds(line_nr)?;
        // safe unwrap because we checked the length
        Ok(self.lines.get(line_nr).unwrap())
    }

    pub fn line_len(&self, line_nr: usize) -> UIResult<usize> {
        Ok(self.get_line_ref(line_nr)?.len())
    }

    fn ensure_bounds(&self, line_nr: usize) -> UIResult<()> {
        ensure!(
            line_nr < self.nr_of_lines(),
            OutOfBoundsSnafu {
                index: line_nr,
                collection_name: "TextBuffer",
                len: self.nr_of_lines(),
            }
        );

        Ok(())
    }

    fn ensure_bounds_txt_pos(&self, txt_pos: TextPos) -> UIResult<()> {
        ensure!(
            txt_pos.line < self.nr_of_lines(),
            OutOfBoundsSnafu {
                index: txt_pos.line,
                collection_name: "TextBuffer",
                len: self.nr_of_lines(),
            }
        );

        let line_ref = self.get_line_ref(txt_pos.line)?;
        let line_len = line_ref.len();

        ensure!(
            txt_pos.column <= line_len,
            OutOfBoundsSnafu {
                index: txt_pos.column,
                collection_name: format!("Line in TextBuffer: {line_ref}"),
                len: line_len,
            }
        );

        Ok(())
    }

    pub fn all_lines_ref(&self) -> &[String] {
        &self.lines
    }

    pub fn get_selected_str(&self, selection: Selection) -> UIResult<String> {
        let start_line_nr = selection.start_pos.line;
        let start_col_nr = selection.start_pos.column;

        let end_line_nr = selection.end_pos.line;
        let end_col_nr = selection.end_pos.column;

        let mut selected_str = String::new();

        if end_line_nr > start_line_nr {
            selected_str.push_str(&self.get_line_ref(start_line_nr)?[start_col_nr..]);

            for line_nr in start_line_nr + 1..end_line_nr - 1 {
                selected_str.push_str(self.get_line_ref(line_nr)?);
            }

            selected_str.push_str(&self.get_line_ref(end_line_nr)?[..end_col_nr]);
        } else {
            // start_line_nr == end_line_nr
            selected_str.push_str(&self.get_line_ref(start_line_nr)?[start_col_nr..end_col_nr]);
        }

        Ok(selected_str)
    }

    pub fn insert_str(&mut self, txt_pos: TextPos, new_str: &str) -> UIResult<()> {
        self.ensure_bounds_txt_pos(txt_pos)?;

        // safe unwrap because we checked the length
        self.lines
            .get_mut(txt_pos.line)
            .unwrap()
            .insert_str(txt_pos.column, new_str);

        Ok(())
    }

    pub fn backspace_char(&mut self, txt_pos: TextPos) -> UIResult<()> {
        if txt_pos.column > 0 {
            let prev_col_pos = TextPos {
                line: txt_pos.line,
                column: txt_pos.column - 1,
            };

            self.ensure_bounds_txt_pos(prev_col_pos)?;

            let line_ref = self.lines.get_mut(prev_col_pos.line).unwrap(); // safe because of earlier bounds check

            line_ref.remove(prev_col_pos.column);
        } else if txt_pos.line > 0 {
            self.lines.remove(txt_pos.line);
        }

        Ok(())
    }

    pub fn del_selection(&mut self, selection: Selection) -> UIResult<()> {
        self.ensure_bounds_txt_pos(selection.start_pos)?;
        self.ensure_bounds_txt_pos(selection.end_pos)?;

        let start_line_nr = selection.start_pos.line;
        let start_col_nr = selection.start_pos.column;
        let end_line_nr = selection.end_pos.line;
        let end_col_nr = selection.end_pos.column;

        if end_line_nr > start_line_nr {
            // remove in reverse to prevent shifting indices
            if end_col_nr == self.line_len(end_line_nr)? {
                self.lines.remove(end_line_nr);
            } else {
                let line_ref = self.lines.get_mut(end_line_nr).unwrap(); // safe because of earlier bounds check
                line_ref.replace_range(..end_col_nr, "");
            }

            self.lines.drain(start_line_nr + 1..end_line_nr);

            let line_ref = self.lines.get_mut(start_line_nr).unwrap(); // safe because of earlier bounds check
            line_ref.replace_range(start_col_nr.., "")
        } else {
            // selection.end_pos.line == selection.start_pos.line
            let line_ref = self.lines.get_mut(selection.start_pos.line).unwrap(); // safe because of earlier bounds check

            line_ref.replace_range(selection.start_pos.column..selection.end_pos.column, "")
        }

        Ok(())
    }
}
