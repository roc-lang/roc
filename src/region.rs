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