use crate::parser::Progress::*;
use crate::parser::{BadInputError, Progress};
use bumpalo::Bump;
use roc_region::all::{Position, Region};
use std::fmt;

/// A position in a source file.
#[derive(Clone)]
pub struct State<'a> {
    /// The raw input bytes from the file.
    bytes: &'a [u8],

    /// Current position within the input (line/column)
    pub pos: Position,

    /// Current indentation level, in columns
    /// (so no indent is col 1 - this saves an arithmetic operation.)
    pub indent_column: u16,
}

impl<'a> State<'a> {
    pub fn new(bytes: &'a [u8]) -> State<'a> {
        State {
            bytes,
            pos: Position::default(),
            indent_column: 0,
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

    /// Returns the current position
    // TODO: replace this with just accessing the field
    pub const fn get_position(&self) -> Position {
        self.pos
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
        TE: Fn(BadInputError, Position) -> E,
    {
        self.advance_without_indenting_ee(quantity, |p| to_error(BadInputError::LineTooLong, p))
    }

    pub fn advance_without_indenting_ee<TE, E>(
        self,
        quantity: usize,
        to_error: TE,
    ) -> Result<Self, (Progress, E, Self)>
    where
        TE: Fn(Position) -> E,
    {
        match (self.pos.column as usize).checked_add(quantity) {
            Some(column_usize) if column_usize <= u16::MAX as usize => {
                Ok(State {
                    bytes: &self.bytes[quantity..],
                    pos: Position {
                        line: self.pos.line,
                        column: column_usize as u16,
                    },
                    // Once we hit a nonspace character, we are no longer indenting.
                    ..self
                })
            }
            _ => Err((NoProgress, to_error(self.pos), self)),
        }
    }

    /// Returns a Region corresponding to the current state, but
    /// with the the end column advanced by the given amount. This is
    /// useful when parsing something "manually" (using input.chars())
    /// and thus wanting a Region while not having access to loc().
    pub fn len_region(&self, length: u16) -> Region {
        Region::new(
            self.pos,
            Position {
                line: self.pos.line,
                column: self
                    .pos
                    .column
                    .checked_add(length)
                    .unwrap_or_else(|| panic!("len_region overflowed")),
            },
        )
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

        write!(
            f,
            "\n\t(line, col): ({}, {}),",
            self.pos.line, self.pos.column
        )?;
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
