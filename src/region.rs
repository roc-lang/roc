#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Region {
    pub start_line: u32,
    pub start_col: u32,

    pub end_line: u32,
    pub end_col: u32,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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