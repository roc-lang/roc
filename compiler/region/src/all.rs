use std::fmt;

/// TODO replace Located with this
pub type Loc<T> = Located<T>;

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Region {
    pub start_line: u32,
    pub end_line: u32,
    pub start_col: u16,
    pub end_col: u16,
}

impl Region {
    pub fn zero() -> Self {
        Region {
            start_line: 0,
            end_line: 0,
            start_col: 0,
            end_col: 0,
        }
    }

    pub fn new(start_line: u32, end_line: u32, start_col: u16, end_col: u16) -> Self {
        Self {
            start_line,
            start_col,
            end_line,
            end_col,
        }
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

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
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
            start_col,
            end_line,
            end_col,
        };
        Located { value, region }
    }

    pub fn at(region: Region, value: T) -> Located<T> {
        Located { value, region }
    }

    pub fn at_zero(value: T) -> Located<T> {
        let region = Region::zero();
        Located { value, region }
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
        } else {
            write!(f, "{:?} {:?}", region, self.value)
        }
    }
}
