
// Adapted from https://github.com/cessen/ropey by Nathan Vegdahl, licensed under the MIT license

use crate::error::EdResult;
use crate::error::EdError::{TextBufReadFailed, FileOpenFailed};
use crate::tea::ed_model::{Position, RawSelection};
use crate::selection::{validate_selection};
use std::fs::File;
use std::io;
use std::path::Path;
use std::ops::Range;
use ropey::{Rope};

#[derive(Debug)]
pub struct TextBuffer {
    text_rope: Rope,
    path_str: String,
    dirty: bool // true if text has been changed, false if all actions following change have executed
}

impl TextBuffer {
    pub fn pop_char(&mut self, cursor_pos: Position) {
        let char_indx = self.pos_to_char_indx(cursor_pos);
        self.text_rope.remove(char_indx..char_indx);
    }
    
    pub fn del_selection(&mut self, raw_sel: RawSelection) -> EdResult<()> {
        let range = self.sel_to_range(raw_sel)?;
        self.text_rope.remove(range);
        Ok(())
    }

    fn pos_to_char_indx(&self, pos: Position) -> usize {
        self.text_rope.line_to_char(pos.line) + pos.column
    }

    fn sel_to_range(&self, raw_sel: RawSelection) -> EdResult<Range<usize>> {
        let valid_sel = validate_selection(raw_sel)?;
        let start_char_indx = self.pos_to_char_indx(valid_sel.selection.start_pos);
        let end_char_indx = self.pos_to_char_indx(valid_sel.selection.start_pos);

        Ok(start_char_indx..end_char_indx)
    }

    pub fn from_path(&self, path: &Path) -> EdResult<TextBuffer> {
        // TODO benchmark different file reading methods, see #886
        let text_rope = rope_from_path(path)?;
        let path_str = path_to_string(path);
    
        Ok(TextBuffer {
            text_rope,
            path_str,
            dirty: false
        })
    }
}

fn path_to_string(path: &Path) -> String {
    let mut path_str = String::new();
    path_str.push_str(&path.to_string_lossy());

    path_str
}

fn rope_from_path(path: &Path) -> EdResult<Rope> {
    match File::open(path) {
        Ok(file) => {
            let mut buf_reader = &mut io::BufReader::new(file);
            match Rope::from_reader(buf_reader) {
                Ok(rope) => 
                    Ok(rope),
                Err(e) =>
                    Err(TextBufReadFailed {
                        path_str: path_to_string(path),
                        err_msg: e.to_string()
                    })
            }
        }
        Err(e) => {
            Err(FileOpenFailed {
                path_str: path_to_string(path),
                err_msg: e.to_string()
            })
        }
    }
}