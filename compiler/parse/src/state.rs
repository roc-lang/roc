use crate::parser::Progress::*;
use crate::parser::{BadInputError, Col, Progress, Row};
use bumpalo::Bump;
use roc_region::all::{Position, Region};
use std::fmt;

/// A position in a source file.
#[derive(Clone)]
pub struct State<'a> {
    /// The raw input bytes from the file.
    bytes: &'a [u8],

    /// Current line of the input
    pub line: u32,
    /// Current column of the input
    pub column: u16,

    /// Current indentation level, in columns
    /// (so no indent is col 1 - this saves an arithmetic operation.)
    pub indent_col: u16,
}

impl<'a> State<'a> {
    pub fn new(bytes: &'a [u8]) -> State<'a> {
        State {
            bytes,
            line: 0,
            column: 0,
            indent_col: 0,
        }
    }

    pub fn bytes(&self) -> &'a [u8] {
        self.bytes
    }

    #[must_use]
    pub fn advance(&self, offset: usize) -> State<'a> {
        let mut state = self.clone();
        state.bytes = &state.bytes[offset..];
        state
    }

    /// Returns whether the parser has reached the end of the input
    pub const fn get_position(&self) -> Position {
        Position {
            row: self.line,
            col: self.column,
        }
    }

    /// Returns whether the parser has reached the end of the input
    pub const fn has_reached_end(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Use advance_spaces to advance with indenting.
    /// This assumes we are *not* advancing with spaces, or at least that
    /// any spaces on the line were preceded by non-spaces - which would mean
    /// they weren't eligible to indent anyway.
    pub fn advance_without_indenting_e<TE, E>(
        self,
        quantity: usize,
        to_error: TE,
    ) -> Result<Self, (Progress, E, Self)>
    where
        TE: Fn(BadInputError, Row, Col) -> E,
    {
        self.advance_without_indenting_ee(quantity, |r, c| {
            to_error(BadInputError::LineTooLong, r, c)
        })
    }

    pub fn advance_without_indenting_ee<TE, E>(
        self,
        quantity: usize,
        to_error: TE,
    ) -> Result<Self, (Progress, E, Self)>
    where
        TE: Fn(Row, Col) -> E,
    {
        match (self.column as usize).checked_add(quantity) {
            Some(column_usize) if column_usize <= u16::MAX as usize => {
                Ok(State {
                    bytes: &self.bytes[quantity..],
                    column: column_usize as u16,
                    // Once we hit a nonspace character, we are no longer indenting.
                    ..self
                })
            }
            _ => Err((NoProgress, to_error(self.line, self.column), self)),
        }
    }

    /// Returns a Region corresponding to the current state, but
    /// with the end_col advanced by the given amount. This is
    /// useful when parsing something "manually" (using input.chars())
    /// and thus wanting a Region while not having access to loc().
    pub fn len_region(&self, length: u16) -> Region {
        Region {
            start_col: self.column,
            start_line: self.line,
            end_col: self
                .column
                .checked_add(length)
                .unwrap_or_else(|| panic!("len_region overflowed")),
            end_line: self.line,
        }
    }

    /// Return a failing ParseResult for the given FailReason
    pub fn fail<T, X>(
        self,
        _arena: &'a Bump,
        progress: Progress,
        reason: X,
    ) -> Result<(Progress, T, Self), (Progress, X, Self)> {
        Err((progress, reason, self))
    }
}

impl<'a> fmt::Debug for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "State {{")?;

        match std::str::from_utf8(self.bytes) {
            Ok(string) => write!(f, "\n\tbytes: [utf8] {:?}", string)?,
            Err(_) => write!(f, "\n\tbytes: [invalid utf8] {:?}", self.bytes)?,
        }

        write!(f, "\n\t(line, col): ({}, {}),", self.line, self.column)?;
        write!(f, "\n\tindent_col: {}", self.indent_col)?;
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
