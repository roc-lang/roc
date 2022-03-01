use roc_region::all::{Position, Region};
use std::fmt;

/// A position in a source file.
#[derive(Clone)]
pub struct State<'a> {
    /// The raw input bytes from the file.
    /// Beware: original_bytes[0] always points the the start of the file.
    /// Use bytes()[0] to access the current byte the parser is inspecting
    original_bytes: &'a [u8],

    /// Offset in original_bytes that the parser is currently inspecting
    offset: usize,

    /// Position of the start of the current line
    line_start: Position,

    /// Current indentation level, in columns
    /// (so no indent is col 1 - this saves an arithmetic operation.)
    pub indent_column: u32,
}

impl<'a> State<'a> {
    pub fn new(bytes: &'a [u8]) -> State<'a> {
        State {
            original_bytes: bytes,
            offset: 0,
            line_start: Position::zero(),
            indent_column: 0,
        }
    }

    pub fn original_bytes(&self) -> &'a [u8] {
        self.original_bytes
    }

    pub(crate) fn bytes(&self) -> &'a [u8] {
        &self.original_bytes[self.offset..]
    }

    pub fn column(&self) -> u32 {
        self.pos().offset - self.line_start.offset
    }

    #[must_use]
    pub(crate) fn advance(&self, offset: usize) -> State<'a> {
        let mut state = self.clone();
        state.offset += offset;
        state
    }

    #[must_use]
    pub(crate) fn advance_newline(&self) -> State<'a> {
        let mut state = self.clone();
        state.offset += 1;
        state.line_start = state.pos();
        state
    }

    /// Returns the current position
    pub const fn pos(&self) -> Position {
        Position::new(self.offset as u32)
    }

    /// Returns whether the parser has reached the end of the input
    pub const fn has_reached_end(&self) -> bool {
        self.offset == self.original_bytes.len()
    }

    /// Returns a Region corresponding to the current state, but
    /// with the the end column advanced by the given amount. This is
    /// useful when parsing something "manually" (using input.chars())
    /// and thus wanting a Region while not having access to loc().
    pub fn len_region(&self, length: u32) -> Region {
        Region::new(self.pos(), self.pos().bump_column(length))
    }
}

impl<'a> fmt::Debug for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "State {{")?;

        match std::str::from_utf8(self.bytes()) {
            Ok(string) => write!(f, "\n\tbytes: [utf8] {:?}", string)?,
            Err(_) => write!(f, "\n\tbytes: [invalid utf8] {:?}", self.bytes())?,
        }

        write!(f, "\n\t(offset): {:?},", self.pos())?;
        write!(f, "\n\tindent_column: {}", self.indent_column)?;
        write!(f, "\n}}")
    }
}

#[test]
fn state_size() {
    // State should always be under 8 machine words, so it fits in a typical
    // cache line.
    let state_size = std::mem::size_of::<State>();
    let maximum = std::mem::size_of::<usize>() * 8;
    assert!(state_size <= maximum, "{:?} <= {:?}", state_size, maximum);
}
