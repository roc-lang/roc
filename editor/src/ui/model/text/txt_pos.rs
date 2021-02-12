
use std::cmp::Ordering;

#[derive(Debug, Copy, Clone)]
pub struct TxtPos {
    pub line: usize,
    pub column: usize,
}

impl Ord for TxtPos {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.line, self.column).cmp(&(other.line, other.column))
    }
}

impl PartialOrd for TxtPos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for TxtPos {
    fn eq(&self, other: &Self) -> bool {
        (self.line, self.column) == (other.line, other.column)
    }
}

impl Eq for TxtPos {}