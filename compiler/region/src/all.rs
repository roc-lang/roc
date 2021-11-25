use std::fmt;

/// TODO replace Located with this
pub type Loc<T> = Located<T>;

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct Region {
    pub start_line: u32,
    pub end_line: u32,
    pub start_col: u16,
    pub end_col: u16,
}

impl Region {
    pub const fn zero() -> Self {
        Region {
            start_line: 0,
            end_line: 0,
            start_col: 0,
            end_col: 0,
        }
    }

    pub const fn new(start_line: u32, end_line: u32, start_col: u16, end_col: u16) -> Self {
        Self {
            start_line,
            end_line,
            start_col,
            end_col,
        }
    }

    pub fn contains(&self, other: &Self) -> bool {
        use std::cmp::Ordering::*;
        match self.start_line.cmp(&other.start_line) {
            Greater => false,
            Equal => match self.end_line.cmp(&other.end_line) {
                Less => false,
                Equal => self.start_col <= other.start_col && self.end_col >= other.end_col,
                Greater => self.start_col >= other.start_col,
            },
            Less => match self.end_line.cmp(&other.end_line) {
                Less => false,
                Equal => self.end_col >= other.end_col,
                Greater => true,
            },
        }
    }

    pub fn is_empty(&self) -> bool {
        self.end_line == self.start_line && self.start_col == self.end_col
    }

    pub fn span_across(start: &Region, end: &Region) -> Self {
        Region {
            start_line: start.start_line,
            end_line: end.end_line,
            start_col: start.start_col,
            end_col: end.end_col,
        }
    }

    pub fn across_all<'a, I>(regions: I) -> Self
    where
        I: IntoIterator<Item = &'a Region>,
    {
        let mut it = regions.into_iter();

        if let Some(first) = it.next() {
            let mut result = *first;

            for r in it {
                result = Self::span_across(&result, r);
            }

            result
        } else {
            Self::zero()
        }
    }

    pub fn lines_between(&self, other: &Region) -> u32 {
        if self.end_line <= other.start_line {
            other.start_line - self.end_line
        } else if self.start_line >= other.end_line {
            self.start_line - other.end_line
        } else {
            // intersection
            0
        }
    }

    pub const fn from_row_col(row: u32, col: u16) -> Self {
        Region {
            start_col: col,
            start_line: row,
            end_col: col + 1,
            end_line: row,
        }
    }

    pub const fn from_rows_cols(
        start_line: u32,
        start_col: u16,
        end_line: u32,
        end_col: u16,
    ) -> Self {
        Region {
            start_line,
            end_line,
            start_col,
            end_col,
        }
    }

    pub const fn start(&self) -> Position {
        Position {
            row: self.start_line,
            col: self.start_col,
        }
    }

    pub const fn end(&self) -> Position {
        Position {
            row: self.end_line,
            col: self.end_col,
        }
    }

    pub const fn between(start: Position, end: Position) -> Self {
        Self::from_rows_cols(start.row, start.col, end.row, end.col)
    }
}

#[test]
fn region_size() {
    // Region is used all over the place. Avoid increasing its size!
    assert_eq!(std::mem::size_of::<Region>(), 12);
}

impl fmt::Debug for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.start_line == 0 && self.start_col == 0 && self.end_line == 0 && self.end_col == 0 {
            // In tests, it's super common to set all Located values to 0.
            // Also in tests, we don't want to bother printing the locations
            // because it makes failed assertions much harder to read.
            write!(f, "…")
        } else {
            write!(
                f,
                "|L {}-{}, C {}-{}|",
                self.start_line, self.end_line, self.start_col, self.end_col,
            )
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct Position {
    pub row: u32,
    pub col: u16,
}

#[derive(Clone, Eq, Copy, PartialEq, PartialOrd, Ord, Hash)]
pub struct Located<T> {
    pub region: Region,
    pub value: T,
}

impl<T> Located<T> {
    pub fn new(
        start_line: u32,
        end_line: u32,
        start_col: u16,
        end_col: u16,
        value: T,
    ) -> Located<T> {
        let region = Region {
            start_line,
            end_line,
            start_col,
            end_col,
        };
        Located { region, value }
    }

    pub fn at(region: Region, value: T) -> Located<T> {
        Located { region, value }
    }

    pub fn at_zero(value: T) -> Located<T> {
        let region = Region::zero();
        Located { region, value }
    }
}

impl<T> Located<T> {
    pub fn with_value<U>(&self, value: U) -> Located<U> {
        Located {
            region: self.region,
            value,
        }
    }

    pub fn map<U, F>(&self, transform: F) -> Located<U>
    where
        F: (FnOnce(&T) -> U),
    {
        Located {
            region: self.region,
            value: transform(&self.value),
        }
    }
}

impl<T> fmt::Debug for Located<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let region = self.region;

        if region.start_line == 0
            && region.start_col == 0
            && region.end_line == 0
            && region.end_col == 0
        {
            // In tests, it's super common to set all Located values to 0.
            // Also in tests, we don't want to bother printing the locations
            // because it makes failed assertions much harder to read.
            self.value.fmt(f)
        } else if f.alternate() {
            write!(f, "{:?} {:#?}", region, self.value)
        } else {
            write!(f, "{:?} {:?}", region, self.value)
        }
    }
}
