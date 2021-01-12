use crate::graphics::primitives::rect::Rect;
use crate::text_buffer;
use crate::text_buffer::TextBuffer;
use crate::error::EdResult;
use std::path::Path;
use std::cmp::Ordering;

#[derive(Debug)]
pub struct EdModel {
    pub text_buf: TextBuffer,
    pub caret_pos: Position,
    pub selection_opt: Option<RawSelection>,
    pub glyph_dim_rect_opt: Option<Rect>,
    pub has_focus: bool
}

pub fn init_model(file_path: &Path) -> EdResult<EdModel> {
    Ok(EdModel {
        text_buf: text_buffer::from_path(file_path)?,
        caret_pos: Position { line: 0, column: 0 },
        selection_opt: None,
        glyph_dim_rect_opt: None,
        has_focus: true
    })
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
