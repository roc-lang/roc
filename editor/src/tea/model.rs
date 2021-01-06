use std::cmp::Ordering;
use crate::graphics::primitives::rect::Rect;

#[derive(Debug)]
pub struct Model {
    pub lines: Vec<String>,
    pub caret_pos: Position,
    pub selection_opt: Option<RawSelection>,
    pub glyph_dim_rect_opt: Option<Rect>
}

pub fn init_model() -> Model {
    Model {
        lines: vec![String::new()],
        caret_pos: Position { line: 0, column: 0 },
        selection_opt: None,
        glyph_dim_rect_opt: None
    }
}

//Is model.rs the right place for these structs?
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
