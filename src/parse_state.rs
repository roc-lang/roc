use combine::lib::fmt;

use combine::stream::Resetable;
use combine::stream::state::{Positioner, RangePositioner};

/// Struct which represents a position in a source file.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct IndentablePosition {
    /// Current line of the input
    pub row: i32,
    /// Current column of the input
    pub column: i32,

    /// Current indentation level, in columns (so no indent is col 1 - this saves an arithmetic operation.)
    pub indent_col : i32,

    // true at the beginning of each line, then false after encountering the first nonspace char.
    pub is_indenting: bool,
}


clone_resetable! { () IndentablePosition }

impl Default for IndentablePosition {
    fn default() -> Self {
        IndentablePosition { row: 1, column: 1, indent_col: 1, is_indenting: true }
    }
}

impl fmt::Display for IndentablePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "row: {}, column: {}, indent_col: {}, is_indenting: {}",
            self.row, self.column, self.indent_col, self.is_indenting)
    }
}

impl IndentablePosition {
    pub fn new() -> Self {
        IndentablePosition::default()
    }
}

impl Positioner<char> for IndentablePosition {
    type Position = IndentablePosition;

    #[inline(always)]
    fn position(&self) -> IndentablePosition {
        self.clone()
    }

    #[inline]
    fn update(&mut self, item: &char) {
        match *item {
            '\n' => {
                self.column = 1;
                self.row += 1;
                self.indent_col = 1;
                self.is_indenting = true;
            },
            ' ' => {
                self.column += 1;
            },
            _ => {
                if self.is_indenting {
                    // As soon as we hit a nonspace char, we're done indenting.
                    // It doesn't count as an indent until we hit a nonspace character though!
                    // Until that point it's still a blank line, not an indented one.
                    self.indent_col = self.column;
                    self.is_indenting = false;
                }

                self.column += 1;
            }
        }
    }
}

impl<'a> RangePositioner<char, &'a str> for IndentablePosition {
    fn update_range(&mut self, range: &&'a str) {
        for c in range.chars() {
            self.update(&c);
        }
    }
}
