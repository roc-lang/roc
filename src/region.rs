use std::fmt;

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct Region {
    pub start_line: u32,
    pub start_col: u32,

    pub end_line: u32,
    pub end_col: u32,
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
                "|L {}, C {} - L {}, C {}|",
                self.start_line, self.start_col, self.end_line, self.end_col,
            )
        }
    }
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct Located<T> {
    pub region: Region,
    pub value: T,
}

impl<T> Located<T> {
    pub fn new(value: T, region: Region) -> Located<T> {
        Located { value, region }
    }
}

impl<T> Located<T> {
    pub fn with_value<U>(&self, value: U) -> Located<U> {
        Located {
            region: self.region.clone(),
            value: value,
        }
    }

    pub fn map<U, F>(&self, transform: F) -> Located<U>
    where
        F: (FnOnce(&T) -> U),
    {
        Located {
            region: self.region.clone(),
            value: transform(&self.value),
        }
    }
}

impl<T> fmt::Debug for Located<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let region = self.region.clone();

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
