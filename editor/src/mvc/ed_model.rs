use crate::error::EdResult;
use crate::graphics::primitives::rect::Rect;
use crate::text_buffer;
use crate::text_buffer::TextBuffer;
use std::cmp::Ordering;
use std::fmt;
use std::path::Path;

#[derive(Debug)]
pub struct EdModel {
    pub text_buf: TextBuffer,
    pub caret_pos: Position,
    pub selection_opt: Option<RawSelection>,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool,
}

pub fn init_model(file_path: &Path) -> EdResult<EdModel> {
    Ok(EdModel {
        text_buf: text_buffer::from_path(file_path)?,
        caret_pos: Position { line: 0, column: 0 },
        selection_opt: None,
        glyph_dim_rect_opt: None,
        has_focus: true,
    })
}

pub fn get_selected_str(ed_model: &EdModel) -> EdResult<Option<&str>> {
    if let Some(curr_selection) = ed_model.selection_opt {
        let selected_str = ed_model.text_buf.get_selection(curr_selection)?;

        Ok(Some(selected_str))
    } else {
        Ok(None)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.line, self.column).cmp(&(other.line, other.column))
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        (self.line, self.column) == (other.line, other.column)
    }
}

impl Eq for Position {}

#[derive(Debug, Copy, Clone)]
pub struct RawSelection {
    pub start_pos: Position,
    pub end_pos: Position,
}

impl std::fmt::Display for RawSelection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "RawSelection: start_pos: line:{} col:{}, end_pos: line:{} col:{}",
            self.start_pos.line, self.start_pos.column, self.end_pos.line, self.end_pos.column
        )
    }
}
