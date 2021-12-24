use std::fmt::{self, Debug};

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct Region {
    start_line: u32,
    end_line: u32,
    start_col: u16,
    end_col: u16,
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

    pub const fn new(start: Position, end: Position) -> Self {
        Self {
            start_line: start.line,
            end_line: end.line,
            start_col: start.column,
            end_col: end.column,
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

    pub const fn from_pos(pos: Position) -> Self {
        Region {
            start_col: pos.column,
            start_line: pos.line,
            end_col: pos.column + 1,
            end_line: pos.line,
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
            line: self.start_line,
            column: self.start_col,
        }
    }

    pub const fn end(&self) -> Position {
        Position {
            line: self.end_line,
            column: self.end_col,
        }
    }

    pub const fn between(start: Position, end: Position) -> Self {
        Self::from_rows_cols(start.line, start.column, end.line, end.column)
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

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct Position {
    pub line: u32,
    pub column: u16,
}

impl Position {
    pub const fn zero() -> Position {
        Position { line: 0, column: 0 }
    }
    
    pub fn new(line: u32, column: u16) -> Position {
        Position { line, column }
    }

    #[must_use]
    pub fn bump_column(self, count: u16) -> Self {
        Self {
            line: self.line,
            column: self.column + count,
        }
    }

    #[must_use]
    pub fn bump_invisible(self, _count: u16) -> Self {
        // This WILL affect the byte offset once we switch to that
        Self {
            line: self.line,
            column: self.column,
        }
    }

    #[must_use]
    pub fn bump_newline(self) -> Self {
        Self {
            line: self.line + 1,
            column: 0,
        }
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct LineColumn {
    pub line: u32,
    pub column: u16,
}

impl LineColumn {
    pub const fn zero() -> Self {
        LineColumn {
            line: 0,
            column: 0,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct LineColumnRegion {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl LineColumnRegion {
    pub const fn zero() -> Self {
        LineColumnRegion {
            start: LineColumn::zero(),
            end: LineColumn::zero(),
        }
    }

    pub fn contains(&self, other: &Self) -> bool {
        use std::cmp::Ordering::*;
        match self.start.line.cmp(&other.start.line) {
            Greater => false,
            Equal => match self.end.line.cmp(&other.end.line) {
                Less => false,
                Equal => self.start.column <= other.start.column && self.end.column >= other.end.column,
                Greater => self.start.column >= other.start.column,
            },
            Less => match self.end.line.cmp(&other.end.line) {
                Less => false,
                Equal => self.end.column >= other.end.column,
                Greater => true,
            },
        }
    }

    pub fn is_empty(&self) -> bool {
        self.end.line == self.start.line && self.start.column == self.end.column
    }

    pub fn span_across(start: &LineColumnRegion, end: &LineColumnRegion) -> Self {
        LineColumnRegion {
            start: start.start,
            end: end.end,
        }
    }

    pub fn across_all<'a, I>(regions: I) -> Self
    where
        I: IntoIterator<Item = &'a LineColumnRegion>,
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

    pub fn lines_between(&self, other: &LineColumnRegion) -> u32 {
        if self.end.line <= other.start.line {
            other.start.line - self.end.line
        } else if self.start.line >= other.end.line {
            self.start.line - other.end.line
        } else {
            // intersection
            0
        }
    }

    pub const fn start(&self) -> LineColumn {
        self.start
    }

    pub const fn end(&self) -> LineColumn {
        self.end
    }
}

#[derive(Clone, Eq, Copy, PartialEq, PartialOrd, Ord, Hash)]
pub struct Loc<T> {
    pub region: Region,
    pub value: T,
}

impl<T> Loc<T> {
    pub fn new(start: Position, end: Position, value: T) -> Loc<T> {
        let region = Region::new(start, end);
        Loc { region, value }
    }

    pub fn at(region: Region, value: T) -> Loc<T> {
        Loc { region, value }
    }

    pub fn at_zero(value: T) -> Loc<T> {
        let region = Region::zero();
        Loc { region, value }
    }
}

impl<T> Loc<T> {
    pub fn with_value<U>(&self, value: U) -> Loc<U> {
        Loc {
            region: self.region,
            value,
        }
    }

    pub fn map<U, F>(&self, transform: F) -> Loc<U>
    where
        F: (FnOnce(&T) -> U),
    {
        Loc {
            region: self.region,
            value: transform(&self.value),
        }
    }
}

impl<T> fmt::Debug for Loc<T>
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

pub struct LineInfo {
    line_offsets: Vec<u32>,
}

impl LineInfo {
    pub fn new(src: &str) -> LineInfo {
        let mut line_offsets = Vec::new();
        line_offsets.push(0);
        line_offsets.extend(src.match_indices('\n').map(|(offset, _)| offset as u32 + 1));
        LineInfo {
            line_offsets,
        }
    }

    pub fn convert_offset(&self, offset: u32) -> LineColumn {
        let search = self.line_offsets.binary_search(&offset);
        let line = match search {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let column = offset - self.line_offsets[line];
        LineColumn { line: line as u32, column: column as u16 }
    }

    pub fn convert_pos(&self, pos: Position) -> LineColumn {
        // TODO
        LineColumn {
            line: pos.line,
            column: pos.column,
        }
    }

    pub fn convert_region(&self, region: Region) -> LineColumnRegion {
        LineColumnRegion {
            start: self.convert_pos(region.start()),
            end: self.convert_pos(region.end()),
        }
    }
}

#[test]
fn test_line_info() {

    fn char_at_line<'a>(lines: &[&'a str], line_column: LineColumn) -> &'a str {
        let line = line_column.line as usize;
        let line_text = if line < lines.len() {
            lines[line]
        } else {
            ""
        };
        let column = line_column.column as usize;
        if column == line_text.len() {
            "\n"
        } else {
            &line_text[column .. column + 1]
        }
    }

    fn check_correctness(lines: &[&str]) {
        let mut input = String::new();
        for (i, line) in lines.iter().enumerate() {
            if i > 0 {
                input.push('\n');
            }
            input.push_str(line);
        }
        let info = LineInfo::new(&input);

        let mut last: Option<LineColumn> = None;
        
        for offset in 0..=input.len() {
            let expected = if offset < input.len() {
                &input[offset..offset + 1]
            } else {
                "\n" // HACK! pretend there's an extra newline on the end, strictly so we can do the comparison
            };
            println!("checking {:?} {:?}, expecting {:?}", input, offset, expected);
            let line_column = info.convert_offset(offset as u32);
            assert!(Some(line_column) > last, "{:?} > {:?}", Some(line_column), last);
            assert_eq!(
                expected,
                char_at_line(lines, line_column));
            last = Some(line_column);
        }

        assert_eq!(
            info.convert_offset(input.len() as u32),
            LineColumn {
                line: lines.len().saturating_sub(1) as u32,
                column: lines.last().map(|l| l.len()).unwrap_or(0) as u16,
            }
        )
    }

    check_correctness(&[
        "",
        "abc",
        "def",
        "",
        "gi",
    ]);

    check_correctness(&[]);

    check_correctness(&["a"]);

    check_correctness(&[
        "",
        "",
    ]);
}
