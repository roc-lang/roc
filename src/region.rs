use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Region {
    pub start_line: u32,
    pub start_col: u32,

    pub end_line: u32,
    pub end_col: u32,
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord)]
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
        Located { region: self.region, value: value }
    }

    pub fn map<U, F>(&self, transform: F) -> Located<U>
        where F: (FnOnce(&T) -> U)
    {
        Located { region: self.region, value: transform(&self.value) }
    }
}


impl<T> fmt::Debug for Located<T>
where T: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let region = self.region;

        if region.start_line == 0 && region.start_col == 0 && region.end_line == 0 && region.end_col == 0 {
            // In tests, it's super common to set all Located values to 0.
            // Also in tests, we don't want to bother printing the locations
            // because it makes failed assertions much harder to read.
            self.value.fmt(f)
        } else {
            write!(f, "|L {}, C {} - L {}, C {}| {:?}",
                region.start_line, region.start_col,
                region.end_line, region.end_col,
                self.value
            )
        }
    }
}