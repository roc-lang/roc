use std::cmp::Ordering;

#[derive(Debug, Copy, Clone)]
pub struct TextPos {
    pub line: usize,
    pub column: usize,
}

impl TextPos {
    pub fn increment_col(&self) -> TextPos {
        TextPos {
            line: self.line,
            column: self.column + 1,
        }
    }
}

impl Ord for TextPos {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.line, self.column).cmp(&(other.line, other.column))
    }
}

impl PartialOrd for TextPos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for TextPos {
    fn eq(&self, other: &Self) -> bool {
        (self.line, self.column) == (other.line, other.column)
    }
}

impl Eq for TextPos {}
