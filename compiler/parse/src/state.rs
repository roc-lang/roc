use crate::parser::Progress::*;
use crate::parser::{BadInputError, Col, Progress, Row};
use crate::token::Token;
use bumpalo::Bump;
use roc_region::all::{Position, Region};
use std::fmt;

/// A position in a source file.
#[derive(Clone)]
pub struct State<'a> {
    /// The raw input bytes from the file.
    bytes: &'a [u8],

    in_compound_token: bool,

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
            in_compound_token: false,
            line: 0,
            column: 0,
            indent_col: 0,
        }
    }

    pub fn bytes(&self) -> &'a [u8] {
        self.bytes
    }

    pub fn expect_compound_token<T, E>(mut self, expected_token: Token, f: impl FnOnce(Self) -> Result<(T, Self), (Progress, E, Self)>) -> Result<(T, Self), (Progress, E, Self)> {
        self.in_compound_token = true;
        let me = self.clone();

        let begin = self.bytes.len();
        let (t, mut new) = match f(self) {
            Ok(v) => v,
            Err((p, e, mut s)) => {
                s.in_compound_token = false;
                return Err((p, e, s))
            }
        };
        let end = new.bytes.len();

        let _ = me.expect_token(expected_token, begin - end);

        new.in_compound_token = false;

        Ok((t, new))
    }

    fn expect_token(&self, expected: Token, expected_len: usize) {
        let (tok, len) = Token::lex_single(expected, self.bytes()).unwrap();
        assert_eq!(tok, expected,
            "token mismatch: [{}]", std::str::from_utf8(&self.bytes()[0..expected_len]).unwrap());
        assert_eq!(len, expected_len,
            "token len issue: [{}]", std::str::from_utf8(&self.bytes()[0..expected_len]).unwrap());
    }

    #[must_use]
    pub fn advance(&self, expected_token: Option<Token>, offset: usize) -> State<'a> {
        // println!("{:?}", std::str::from_utf8(&self.bytes[..offset]).unwrap());

        if !self.in_compound_token {
            if let Some(expected) = expected_token {
                self.expect_token(expected, offset)
            }
        }

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
        expected_token: Option<Token>,
        quantity: usize,
        to_error: TE,
    ) -> Result<Self, (Progress, E, Self)>
    where
        TE: Fn(BadInputError, Row, Col) -> E,
    {
        self.advance_without_indenting_ee(expected_token, quantity, |r, c| {
            to_error(BadInputError::LineTooLong, r, c)
        })
    }

    pub fn advance_without_indenting_ee<TE, E>(
        mut self,
        expected_token: Option<Token>,
        quantity: usize,
        to_error: TE,
    ) -> Result<Self, (Progress, E, Self)>
    where
        TE: Fn(Row, Col) -> E,
    {
        match (self.column as usize).checked_add(quantity) {
            Some(column_usize) if column_usize <= u16::MAX as usize => {
                self.column = column_usize as u16;
                Ok(self.advance(expected_token, quantity))
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
