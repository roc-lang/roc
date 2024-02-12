use std::fmt::{self, Debug};

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct Region {
    start: Position,
    end: Position,
}

impl Region {
    pub const fn zero() -> Self {
        Region {
            start: Position::zero(),
            end: Position::zero(),
        }
    }

    pub const fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn contains(&self, other: &Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    pub fn contains_pos(&self, pos: Position) -> bool {
        self.start <= pos && self.end >= pos
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub const fn len(&self) -> u32 {
        self.end.offset - self.start.offset
    }

    pub fn span_across(start: &Region, end: &Region) -> Self {
        Region {
            start: start.start,
            end: end.end,
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

    pub const fn from_pos(pos: Position) -> Self {
        Region {
            start: pos,
            end: pos.bump_column(1),
        }
    }

    pub const fn start(&self) -> Position {
        self.start
    }

    pub const fn end(&self) -> Position {
        self.end
    }

    pub const fn between(start: Position, end: Position) -> Self {
        Self::new(start, end)
    }
}

// Region is used all over the place. Avoid increasing its size!
static_assertions::assert_eq_size!([u8; 8], Region);

impl fmt::Debug for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.start == Position::zero() && self.end == Position::zero() {
            // In tests, it's super common to set all Located values to 0.
            // Also in tests, we don't want to bother printing the locations
            // because it makes failed assertions much harder to read.
            write!(f, "…")
        } else {
            write!(f, "@{}-{}", self.start.offset, self.end.offset,)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Position {
    pub offset: u32,
}

impl Position {
    pub const fn zero() -> Position {
        Position { offset: 0 }
    }

    pub const fn new(offset: u32) -> Position {
        Position { offset }
    }

    #[must_use]
    pub const fn bump_column(self, count: u32) -> Self {
        Self {
            offset: self.offset + count,
        }
    }

    #[must_use]
    pub fn bump_invisible(self, count: u32) -> Self {
        Self {
            offset: self.offset + count,
        }
    }

    #[must_use]
    pub fn bump_newline(self) -> Self {
        Self {
            offset: self.offset + 1,
        }
    }

    #[must_use]
    pub const fn sub(self, count: u32) -> Self {
        Self {
            offset: self.offset - count,
        }
    }

    pub fn byte_offset(&self) -> usize {
        self.offset as usize
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@")?;
        self.offset.fmt(f)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

impl LineColumn {
    pub const fn zero() -> Self {
        LineColumn { line: 0, column: 0 }
    }

    #[must_use]
    pub const fn bump_column(self, count: u32) -> Self {
        Self {
            line: self.line,
            column: self.column + count,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Default)]
pub struct LineColumnRegion {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl LineColumnRegion {
    pub const fn new(start: LineColumn, end: LineColumn) -> Self {
        LineColumnRegion { start, end }
    }

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
                Equal => {
                    self.start.column <= other.start.column && self.end.column >= other.end.column
                }
                Greater => self.start.column >= other.start.column,
            },
            Less => match self.end.line.cmp(&other.end.line) {
                Less => false,
                Equal => self.end.column >= other.end.column,
                Greater => true,
            },
        }
    }

    pub fn includes(&self, lc: LineColumn) -> bool {
        self.contains(&Self::from_pos(lc))
    }

    pub const fn from_pos(pos: LineColumn) -> Self {
        Self {
            start: pos,
            end: pos.bump_column(1),
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

impl fmt::Debug for LineColumnRegion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.start.line == 0
            && self.start.column == 0
            && self.end.line == 0
            && self.end.column == 0
        {
            // In tests, it's super common to set all Located values to 0.
            // Also in tests, we don't want to bother printing the locations
            // because it makes failed assertions much harder to read.
            write!(f, "…")
        } else {
            write!(
                f,
                "|L {}-{}, C {}-{}|",
                self.start.line, self.end.line, self.start.column, self.end.column,
            )
        }
    }
}

#[derive(Clone, Eq, Copy, PartialEq, PartialOrd, Ord, Hash)]
pub struct Loc<T> {
    pub region: Region,
    pub value: T,
}

impl<T> Loc<T> {
    pub const fn new(start: u32, end: u32, value: T) -> Loc<T> {
        let region = Region::new(Position::new(start), Position::new(end));
        Loc { region, value }
    }

    pub const fn at(region: Region, value: T) -> Loc<T> {
        Loc { region, value }
    }

    pub const fn at_zero(value: T) -> Loc<T> {
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

    pub fn map_owned<U, F>(self, transform: F) -> Loc<U>
    where
        F: (FnOnce(T) -> U),
    {
        Loc {
            region: self.region,
            value: transform(self.value),
        }
    }

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        self.region.start.byte_offset()..self.region.end.byte_offset()
    }
}

impl<T> fmt::Debug for Loc<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let region = self.region;

        if region.start == Position::zero() && region.end == Position::zero() {
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

#[derive(Debug, Clone)]
pub struct LineInfo {
    line_offsets: Vec<u32>,
}

impl LineInfo {
    pub fn new(src: &str) -> LineInfo {
        let mut line_offsets = vec![0];
        line_offsets.extend(src.match_indices('\n').map(|(offset, _)| offset as u32 + 1));
        LineInfo { line_offsets }
    }

    pub fn convert_offset(&self, offset: u32) -> LineColumn {
        let search = self.line_offsets.binary_search(&offset);
        let line = match search {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let column = offset - self.line_offsets[line];
        LineColumn {
            line: line as u32,
            column,
        }
    }

    pub fn convert_pos(&self, pos: Position) -> LineColumn {
        self.convert_offset(pos.offset)
    }

    pub fn convert_region(&self, region: Region) -> LineColumnRegion {
        LineColumnRegion {
            start: self.convert_pos(region.start()),
            end: self.convert_pos(region.end()),
        }
    }

    pub fn convert_line_column(&self, lc: LineColumn) -> Position {
        let offset = self.line_offsets[lc.line as usize] + lc.column;
        Position::new(offset)
    }

    pub fn convert_line_column_region(&self, lc_region: LineColumnRegion) -> Region {
        let start = self.convert_line_column(lc_region.start);
        let end = self.convert_line_column(lc_region.end);
        Region::new(start, end)
    }

    pub fn num_lines(&self) -> u32 {
        self.line_offsets.len() as u32
    }
}

#[test]
fn test_line_info() {
    fn char_at_line<'a>(lines: &[&'a str], line_column: LineColumn) -> &'a str {
        let line = line_column.line as usize;
        let line_text = if line < lines.len() { lines[line] } else { "" };
        let column = line_column.column as usize;
        if column == line_text.len() {
            "\n"
        } else {
            &line_text[column..column + 1]
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
            println!("checking {input:?} {offset:?}, expecting {expected:?}");
            let line_column = info.convert_offset(offset as u32);
            assert!(
                Some(line_column) > last,
                "{:?} > {:?}",
                Some(line_column),
                last
            );
            assert_eq!(expected, char_at_line(lines, line_column));
            last = Some(line_column);
        }

        assert_eq!(
            info.convert_offset(input.len() as u32),
            LineColumn {
                line: lines.len().saturating_sub(1) as u32,
                column: lines.last().map(|l| l.len()).unwrap_or(0) as u32,
            }
        )
    }

    check_correctness(&["", "abc", "def", "", "gi"]);

    check_correctness(&[]);

    check_correctness(&["a"]);

    check_correctness(&["", ""]);
}
