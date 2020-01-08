use crate::parse::ast::Attempting;
use crate::region::{Located, Region};
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use std::{char, u16};

/// A position in a source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct State<'a> {
    /// The raw input string.
    pub input: &'a str,

    /// Current line of the input
    pub line: u32,
    /// Current column of the input
    pub column: u16,

    /// Current indentation level, in columns
    /// (so no indent is col 1 - this saves an arithmetic operation.)
    pub indent_col: u16,

    // true at the beginning of each line, then false after encountering
    // the first nonspace char on that line.
    pub is_indenting: bool,

    pub attempting: Attempting,

    /// The original length of the string, before any bytes were consumed.
    /// This is used internally by the State::bytes_consumed() function.
    ///
    /// TODO make this private, in a way that doesn't break macros!
    pub original_len: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Either<First, Second> {
    First(First),
    Second(Second),
}

impl<'a> State<'a> {
    pub fn new(input: &'a str, attempting: Attempting) -> State<'a> {
        State {
            input,
            line: 0,
            column: 0,
            indent_col: 0,
            is_indenting: true,
            attempting,
            original_len: input.len(),
        }
    }

    pub fn check_indent(self, min_indent: u16) -> Result<Self, (Fail, Self)> {
        if self.indent_col < min_indent {
            Err((
                Fail {
                    attempting: self.attempting,
                    reason: FailReason::OutdentedTooFar,
                },
                self,
            ))
        } else {
            Ok(self)
        }
    }

    /// Returns the total number of bytes consumed since the parser began parsing.
    ///
    /// So if the parser has consumed 8 bytes, this function will return 8.
    pub fn bytes_consumed(&self) -> usize {
        self.original_len - self.input.len()
    }

    /// Increments the line, then resets column, indent_col, and is_indenting.
    /// Advances the input by 1, to consume the newline character.
    pub fn newline(&self) -> Result<Self, (Fail, Self)> {
        match self.line.checked_add(1) {
            Some(line) => Ok(State {
                input: &self.input[1..],
                line,
                column: 0,
                indent_col: 0,
                is_indenting: true,
                attempting: self.attempting,
                original_len: self.original_len,
            }),
            None => Err((
                Fail {
                    reason: FailReason::TooManyLines,
                    attempting: self.attempting,
                },
                self.clone(),
            )),
        }
    }

    /// Use advance_spaces to advance with indenting.
    /// This assumes we are *not* advancing with spaces, or at least that
    /// any spaces on the line were preceded by non-spaces - which would mean
    /// they weren't eligible to indent anyway.
    pub fn advance_without_indenting(&self, quantity: usize) -> Result<Self, (Fail, Self)> {
        match (self.column as usize).checked_add(quantity) {
            Some(column_usize) if column_usize <= u16::MAX as usize => {
                Ok(State {
                    input: &self.input[quantity..],
                    line: self.line,
                    column: column_usize as u16,
                    indent_col: self.indent_col,
                    // Once we hit a nonspace character, we are no longer indenting.
                    is_indenting: false,
                    attempting: self.attempting,
                    original_len: self.original_len,
                })
            }
            _ => Err(line_too_long(self.attempting, self.clone())),
        }
    }
    /// Advance the parser while also indenting as appropriate.
    /// This assumes we are only advancing with spaces, since they can indent.
    pub fn advance_spaces(&self, spaces: usize) -> Result<Self, (Fail, Self)> {
        match (self.column as usize).checked_add(spaces) {
            Some(column_usize) if column_usize <= u16::MAX as usize => {
                // Spaces don't affect is_indenting; if we were previously indneting,
                // we still are, and if we already finished indenting, we're still done.
                let is_indenting = self.is_indenting;

                // If we're indenting, spaces indent us further.
                let indent_col = if is_indenting {
                    // This doesn't need to be checked_add because it's always true that
                    // indent_col <= col, so if this could possibly overflow, we would
                    // already have errored out from the column calculation.
                    //
                    // Leaving debug assertions in case this invariant someday disappers.
                    debug_assert!(u16::MAX - self.indent_col >= spaces as u16);
                    debug_assert!(spaces <= u16::MAX as usize);

                    self.indent_col + spaces as u16
                } else {
                    self.indent_col
                };

                Ok(State {
                    input: &self.input[spaces..],
                    line: self.line,
                    column: column_usize as u16,
                    indent_col,
                    is_indenting,
                    attempting: self.attempting,
                    original_len: self.original_len,
                })
            }
            _ => Err(line_too_long(self.attempting, self.clone())),
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
}

#[test]
fn state_size() {
    // State should always be under 8 machine words, so it fits in a typical
    // cache line.
    assert!(std::mem::size_of::<State>() <= std::mem::size_of::<usize>() * 8);
}

pub type ParseResult<'a, Output> = Result<(Output, State<'a>), (Fail, State<'a>)>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FailReason {
    Unexpected(char, Region),
    OutdentedTooFar,
    ConditionFailed,
    LineTooLong(u32 /* which line was too long */),
    TooManyLines,
    Eof(Region),
    InvalidPattern,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fail {
    pub attempting: Attempting,
    pub reason: FailReason,
}

pub trait Parser<'a, Output> {
    fn parse(&self, _: &'a Bump, _: State<'a>) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a Bump, State<'a>) -> ParseResult<'a, Output>,
{
    fn parse(&self, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Output> {
        self(arena, state)
    }
}

pub fn allocated<'a, P, Val>(parser: P) -> impl Parser<'a, &'a Val>
where
    P: Parser<'a, Val>,
    Val: 'a,
{
    move |arena, state: State<'a>| {
        let (answer, state) = parser.parse(arena, state)?;

        Ok((&*arena.alloc(answer), state))
    }
}

pub fn not_followed_by<'a, P, ByParser, By, Val>(parser: P, by: ByParser) -> impl Parser<'a, Val>
where
    ByParser: Parser<'a, By>,
    P: Parser<'a, Val>,
{
    move |arena, state: State<'a>| {
        let original_state = state.clone();

        parser.parse(arena, state).and_then(|(answer, state)| {
            let after_parse = state.clone();

            match by.parse(arena, state) {
                Ok((_, state)) => Err((
                    Fail {
                        attempting: state.attempting,
                        reason: FailReason::ConditionFailed,
                    },
                    original_state,
                )),
                Err(_) => Ok((answer, after_parse)),
            }
        })
    }
}

pub fn not<'a, P, Val>(parser: P) -> impl Parser<'a, ()>
where
    P: Parser<'a, Val>,
{
    move |arena, state: State<'a>| {
        let original_state = state.clone();

        match parser.parse(arena, state) {
            Ok((_, _)) => Err((
                Fail {
                    reason: FailReason::ConditionFailed,
                    attempting: original_state.attempting,
                },
                original_state,
            )),
            Err((_, _)) => Ok(((), original_state)),
        }
    }
}

pub fn lookahead<'a, Peek, P, PeekVal, Val>(peek: Peek, parser: P) -> impl Parser<'a, Val>
where
    Peek: Parser<'a, PeekVal>,
    P: Parser<'a, Val>,
{
    move |arena, state: State<'a>| {
        let original_state = state.clone();

        peek.parse(arena, state)
            .and_then(|_| parser.parse(arena, original_state))
    }
}

pub fn and_then<'a, P1, P2, F, Before, After>(parser: P1, transform: F) -> impl Parser<'a, After>
where
    P1: Parser<'a, Before>,
    P2: Parser<'a, After>,
    F: Fn(Before) -> P2,
{
    move |arena, state| {
        parser
            .parse(arena, state)
            .and_then(|(output, next_state)| transform(output).parse(arena, next_state))
    }
}

pub fn then<'a, P1, F, Before, After>(parser: P1, transform: F) -> impl Parser<'a, After>
where
    P1: Parser<'a, Before>,
    After: 'a,
    F: Fn(&'a Bump, State<'a>, Before) -> ParseResult<'a, After>,
{
    move |arena, state| {
        parser
            .parse(arena, state)
            .and_then(|(output, next_state)| transform(arena, next_state, output))
    }
}

pub fn unexpected_eof(
    chars_consumed: usize,
    attempting: Attempting,
    state: State<'_>,
) -> (Fail, State<'_>) {
    checked_unexpected(chars_consumed, state, |region| Fail {
        reason: FailReason::Eof(region),
        attempting,
    })
}

pub fn unexpected(
    ch: char,
    chars_consumed: usize,
    state: State<'_>,
    attempting: Attempting,
) -> (Fail, State<'_>) {
    checked_unexpected(chars_consumed, state, |region| Fail {
        reason: FailReason::Unexpected(ch, region),
        attempting,
    })
}

/// Check for line overflow, then compute a new Region based on chars_consumed
/// and provide it as a way to construct a Problem.
/// If maximum line length was exceeded, return a Problem indicating as much.
#[inline(always)]
fn checked_unexpected<F>(
    chars_consumed: usize,
    state: State<'_>,
    problem_from_region: F,
) -> (Fail, State<'_>)
where
    F: FnOnce(Region) -> Fail,
{
    match (state.column as usize).checked_add(chars_consumed) {
        // Crucially, this is < u16::MAX and not <= u16::MAX. This means if
        // column ever gets set to u16::MAX, we will automatically bail out
        // with LineTooLong - which is exactly what we want! Once a line has
        // been discovered to be too long, we don't want to parse anything else
        // until that's fixed.
        Some(end_col) if end_col < u16::MAX as usize => {
            let region = Region {
                start_col: state.column,
                end_col: end_col as u16,
                start_line: state.line,
                end_line: state.line,
            };

            (problem_from_region(region), state)
        }
        _ => line_too_long(state.attempting, state),
    }
}

fn line_too_long(attempting: Attempting, state: State<'_>) -> (Fail, State<'_>) {
    let reason = FailReason::LineTooLong(state.line);
    let fail = Fail { reason, attempting };
    // Set column to MAX and advance the parser to end of input.
    // This way, all future parsers will fail on EOF, and then
    // unexpected_eof will take them back here - thus propagating
    // the initial LineTooLong error all the way to the end, even if
    // (for example) the LineTooLong initially occurs in the middle of
    // a one_of chain, which would otherwise prevent it from propagating.
    let column = u16::MAX;
    let input = state.input.get(0..state.input.len()).unwrap();
    let state = State {
        input,
        line: state.line,
        indent_col: state.indent_col,
        is_indenting: state.is_indenting,
        column,
        attempting,
        original_len: state.original_len,
    };

    (fail, state)
}

/// A single char.
pub fn char<'a>(expected: char) -> impl Parser<'a, ()> {
    move |_arena, state: State<'a>| match state.input.chars().next() {
        Some(actual) if expected == actual => Ok(((), state.advance_without_indenting(1)?)),
        Some(other_ch) => Err(unexpected(other_ch, 0, state, Attempting::Keyword)),
        _ => Err(unexpected_eof(0, Attempting::Keyword, state)),
    }
}

/// A hardcoded keyword string with no newlines in it.
pub fn string<'a>(keyword: &'static str) -> impl Parser<'a, ()> {
    // We can't have newlines because we don't attempt to advance the row
    // in the state, only the column.
    debug_assert!(!keyword.contains('\n'));

    move |_arena, state: State<'a>| {
        let input = state.input;
        let len = keyword.len();

        // TODO do this comparison in one SIMD instruction (on supported systems)
        match input.get(0..len) {
            Some(next_str) if next_str == keyword => {
                Ok(((), state.advance_without_indenting(len)?))
            }
            _ => Err(unexpected_eof(0, Attempting::Keyword, state)),
        }
    }
}

/// Parse zero or more values separated by a delimiter (e.g. a comma) whose
/// values are discarded
pub fn sep_by0<'a, P, D, Val>(delimiter: D, parser: P) -> impl Parser<'a, Vec<'a, Val>>
where
    D: Parser<'a, ()>,
    P: Parser<'a, Val>,
{
    move |arena, state: State<'a>| {
        let original_attempting = state.attempting;

        match parser.parse(arena, state) {
            Ok((first_output, next_state)) => {
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    match delimiter.parse(arena, state) {
                        Ok(((), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state) {
                                Ok((next_output, next_state)) => {
                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((fail, state)) => {
                                    // If the delimiter parsed, but the following
                                    // element did not, that's a fatal error.
                                    return Err((
                                        Fail {
                                            attempting: original_attempting,
                                            ..fail
                                        },
                                        state,
                                    ));
                                }
                            }
                        }
                        Err((_, old_state)) => return Ok((buf, old_state)),
                    }
                }
            }
            Err((_, new_state)) => Ok((Vec::new_in(arena), new_state)),
        }
    }
}

/// Parse one or more values separated by a delimiter (e.g. a comma) whose
/// values are discarded
pub fn sep_by1<'a, P, D, Val>(delimiter: D, parser: P) -> impl Parser<'a, Vec<'a, Val>>
where
    D: Parser<'a, ()>,
    P: Parser<'a, Val>,
{
    move |arena, state: State<'a>| {
        let original_attempting = state.attempting;

        match parser.parse(arena, state) {
            Ok((first_output, next_state)) => {
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    match delimiter.parse(arena, state) {
                        Ok(((), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state) {
                                Ok((next_output, next_state)) => {
                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((fail, state)) => {
                                    // If the delimiter parsed, but the following
                                    // element did not, that's a fatal error.
                                    return Err((
                                        Fail {
                                            attempting: original_attempting,
                                            ..fail
                                        },
                                        state,
                                    ));
                                }
                            }
                        }
                        Err((_, old_state)) => return Ok((buf, old_state)),
                    }
                }
            }
            Err((fail, new_state)) => Err((
                Fail {
                    attempting: original_attempting,
                    ..fail
                },
                new_state,
            )),
        }
    }
}

pub fn satisfies<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |arena: &'a Bump, state: State<'a>| {
        if let Ok((output, next_state)) = parser.parse(arena, state.clone()) {
            if predicate(&output) {
                return Ok((output, next_state));
            }
        }

        Err((
            Fail {
                reason: FailReason::ConditionFailed,
                attempting: state.attempting,
            },
            state,
        ))
    }
}

pub fn optional<'a, P, T>(parser: P) -> impl Parser<'a, Option<T>>
where
    P: Parser<'a, T>,
{
    move |arena: &'a Bump, state: State<'a>| {
        // We have to clone this because if the optional parser fails,
        // we need to revert back to the original state.
        let original_state = state.clone();

        match parser.parse(arena, state) {
            Ok((out1, state)) => Ok((Some(out1), state)),
            Err(_) => Ok((None, original_state)),
        }
    }
}

// MACRO COMBINATORS
//
// Using some combinators together results in combinatorial type explosion
// which makes things take forever to compile. Using macros instead avoids this!

#[macro_export]
macro_rules! loc {
    ($parser:expr) => {
        move |arena, state: $crate::parse::parser::State<'a>| {
            use $crate::region::{Located, Region};

            let start_col = state.column;
            let start_line = state.line;

            match $parser.parse(arena, state) {
                Ok((value, state)) => {
                    let end_col = state.column;
                    let end_line = state.line;
                    let region = Region {
                        start_col,
                        start_line,
                        end_col,
                        end_line,
                    };

                    Ok((Located { region, value }, state))
                }
                Err((fail, state)) => Err((fail, state)),
            }
        }
    };
}

/// If the first one parses, ignore its output and move on to parse with the second one.
#[macro_export]
macro_rules! skip_first {
    ($p1:expr, $p2:expr) => {
        move |arena, state: $crate::parse::parser::State<'a>| {
            use $crate::parse::parser::Fail;

            let original_attempting = state.attempting;

            match $p1.parse(arena, state) {
                Ok((_, state)) => match $p2.parse(arena, state) {
                    Ok((out2, state)) => Ok((out2, state)),
                    Err((fail, state)) => Err((
                        Fail {
                            attempting: original_attempting,
                            ..fail
                        },
                        state,
                    )),
                },
                Err((fail, state)) => Err((
                    Fail {
                        attempting: original_attempting,
                        ..fail
                    },
                    state,
                )),
            }
        }
    };
}

/// If the first one parses, parse the second one; if it also parses, use the
/// output from the first one.
#[macro_export]
macro_rules! skip_second {
    ($p1:expr, $p2:expr) => {
        move |arena, state: $crate::parse::parser::State<'a>| {
            use $crate::parse::parser::Fail;

            let original_attempting = state.attempting;

            match $p1.parse(arena, state) {
                Ok((out1, state)) => match $p2.parse(arena, state) {
                    Ok((_, state)) => Ok((out1, state)),
                    Err((fail, state)) => Err((
                        Fail {
                            attempting: original_attempting,
                            ..fail
                        },
                        state,
                    )),
                },
                Err((fail, state)) => Err((
                    Fail {
                        attempting: original_attempting,
                        ..fail
                    },
                    state,
                )),
            }
        }
    };
}

/// Parse zero or more elements between two braces (e.g. square braces).
/// Elements can be optionally surrounded by spaces, and are separated by a
/// delimiter (e.g comma-separated). Braces and delimiters get discarded.
#[macro_export]
macro_rules! collection {
    ($opening_brace:expr, $elem:expr, $delimiter:expr, $closing_brace:expr, $min_indent:expr) => {
        skip_first!(
            $opening_brace,
            skip_first!(
                // We specifically allow space characters inside here, so that
                // `[  ]` can be successfully parsed as an empty list, and then
                // changed by the formatter back into `[]`.
                //
                // We don't allow newlines or comments in the middle of empty
                // collections because those are normally stored in an Expr,
                // and there's no Expr in which to store them in an empty collection!
                //
                // We could change the AST to add extra storage specifically to
                // support empty literals containing newlines or comments, but this
                // does not seem worth even the tiniest regression in compiler performance.
                zero_or_more!($crate::parse::parser::char(' ')),
                skip_second!(
                    $crate::parse::parser::sep_by0(
                        $delimiter,
                        $crate::parse::blankspace::space0_around($elem, $min_indent)
                    ),
                    $closing_brace
                )
            )
        )
    };
}

#[macro_export]
macro_rules! and {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::parse::parser::State<'a>| {
            use $crate::parse::parser::Fail;

            // We have to clone this because if the first parser passes and then
            // the second one fails, we need to revert back to the original state.
            let original_state = state.clone();

            match $p1.parse(arena, state) {
                Ok((out1, state)) => match $p2.parse(arena, state) {
                    Ok((out2, state)) => Ok(((out1, out2), state)),
                    Err((fail, _)) => Err((
                        Fail {
                            attempting: original_state.attempting,
                            ..fail
                        },
                        original_state,
                    )),
                },
                Err((fail, state)) => Err((
                    Fail {
                        attempting: original_state.attempting,
                        ..fail
                    },
                    state,
                )),
            }
        }
    };
}

#[macro_export]
macro_rules! one_of {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::parse::parser::State<'a>| {
            let original_attempting = state.attempting;

            match $p1.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((_, state)) => $p2.parse(
                    arena,
                    State {
                        // Try again, using the original `attempting` value.
                        // We don't care what the failed first attempt was trying to do.
                        attempting: original_attempting,
                        ..state
                    },
                ),
            }
        }
    };

    ($p1:expr, $($others:expr),+) => {
        one_of!($p1, one_of!($($others),+))
    };
}

#[macro_export]
macro_rules! map {
    ($parser:expr, $transform:expr) => {
        move |arena, state| {
            $parser
                .parse(arena, state)
                .map(|(output, next_state)| ($transform(output), next_state))
        }
    };
}

#[macro_export]
macro_rules! map_with_arena {
    ($parser:expr, $transform:expr) => {
        move |arena, state| {
            $parser
                .parse(arena, state)
                .map(|(output, next_state)| ($transform(arena, output), next_state))
        }
    };
}

#[macro_export]
macro_rules! zero_or_more {
    ($parser:expr) => {
        move |arena, state| {
            use bumpalo::collections::Vec;

            match $parser.parse(arena, state) {
                Ok((first_output, next_state)) => {
                    let mut state = next_state;
                    let mut buf = Vec::with_capacity_in(1, arena);

                    buf.push(first_output);

                    loop {
                        match $parser.parse(arena, state) {
                            Ok((next_output, next_state)) => {
                                state = next_state;
                                buf.push(next_output);
                            }
                            Err((_, old_state)) => return Ok((buf, old_state)),
                        }
                    }
                }
                Err((_, new_state)) => Ok((Vec::new_in(arena), new_state)),
            }
        }
    };
}

#[macro_export]
macro_rules! one_or_more {
    ($parser:expr) => {
        move |arena, state| {
            use bumpalo::collections::Vec;

            match $parser.parse(arena, state) {
                Ok((first_output, next_state)) => {
                    let mut state = next_state;
                    let mut buf = Vec::with_capacity_in(1, arena);

                    buf.push(first_output);

                    loop {
                        match $parser.parse(arena, state) {
                            Ok((next_output, next_state)) => {
                                state = next_state;
                                buf.push(next_output);
                            }
                            Err((_, old_state)) => return Ok((buf, old_state)),
                        }
                    }
                }
                Err((_, new_state)) => Err(unexpected_eof(0, new_state.attempting, new_state)),
            }
        }
    };
}

#[macro_export]
macro_rules! attempt {
    ($attempting:expr, $parser:expr) => {
        move |arena, state: $crate::parse::parser::State<'a>| {
            use crate::parse::parser::State;

            let original_attempting = state.attempting;

            $parser
                .parse(
                    arena,
                    State {
                        attempting: $attempting,
                        ..state
                    },
                )
                .map(|(answer, state)| {
                    // If the parser suceeded, go back to what we were originally attempting.
                    // (If it failed, that's exactly where we care what we were attempting!)
                    (
                        answer,
                        State {
                            attempting: original_attempting,
                            ..state
                        },
                    )
                })
        }
    };
}

#[macro_export]
macro_rules! either {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::parse::parser::State<'a>| {
            use $crate::parse::parser::Fail;

            let original_attempting = state.attempting;

            match $p1.parse(arena, state) {
                Ok((output, state)) => Ok((Either::First(output), state)),
                Err((_, state)) => match $p2.parse(arena, state) {
                    Ok((output, state)) => Ok((Either::Second(output), state)),
                    Err((fail, state)) => Err((
                        Fail {
                            attempting: original_attempting,
                            ..fail
                        },
                        state,
                    )),
                },
            }
        }
    };
}

/// Parse everything between two braces (e.g. parentheses), skipping both braces
/// and keeping only whatever was parsed in between them.
#[macro_export]
macro_rules! between {
    ($opening_brace:expr, $parser:expr, $closing_brace:expr) => {
        skip_first!($opening_brace, skip_second!($parser, $closing_brace))
    };
}

#[macro_export]
macro_rules! record_field {
    ($val_parser:expr, $min_indent:expr) => {
        move |arena: &'a bumpalo::Bump,
              state: $crate::parse::parser::State<'a>|
              -> $crate::parse::parser::ParseResult<
            'a,
            $crate::parse::ast::AssignedField<'a, _>,
        > {
            use $crate::parse::ast::AssignedField::*;
            use $crate::parse::blankspace::{space0, space0_before};
            use $crate::parse::ident::lowercase_ident;

            // You must have a field name, e.g. "email"
            let (loc_label, state) = loc!(lowercase_ident()).parse(arena, state)?;

            let (spaces, state) = space0($min_indent).parse(arena, state)?;
            // Having a value is optional; both `{ email }` and `{ email: blah }` work.
            // (This is true in both literals and types.)
            let (opt_loc_val, state) = $crate::parse::parser::optional(skip_first!(
                char(':'),
                space0_before($val_parser, $min_indent)
            ))
            .parse(arena, state)?;

            let answer = match opt_loc_val {
                Some(loc_val) => LabeledValue(loc_label, spaces, arena.alloc(loc_val)),
                // If no value was provided, record it as a Var.
                // Canonicalize will know what to do with a Var later.
                None => {
                    if !spaces.is_empty() {
                        SpaceAfter(arena.alloc(LabelOnly(loc_label)), spaces)
                    } else {
                        LabelOnly(loc_label)
                    }
                }
            };

            Ok((answer, state))
        }
    };
}

#[macro_export]
macro_rules! record_without_update {
    ($val_parser:expr, $min_indent:expr) => {
        collection!(
            char('{'),
            loc!(record_field!($val_parser, $min_indent)),
            char(','),
            char('}'),
            $min_indent
        )
    };
}

#[macro_export]
macro_rules! record {
    ($val_parser:expr, $min_indent:expr) => {
        skip_first!(
            $crate::parse::parser::char('{'),
            and!(
                // You can optionally have an identifier followed by an '&' to
                // make this a record update, e.g. { Foo.user & username: "blah" }.
                $crate::parse::parser::optional(skip_second!(
                    $crate::parse::blankspace::space0_around(
                        // We wrap the ident in an Expr here,
                        // so that we have a Spaceable value to work with,
                        // and then in canonicalization verify that it's an Expr::Var
                        // (and not e.g. an `Expr::Access`) and extract its string.
                        loc!(map_with_arena!(
                            $crate::parse::ident(),
                            $crate::parse::ident_to_expr
                        )),
                        $min_indent
                    ),
                    $crate::parse::parser::char('&')
                )),
                loc!(skip_first!(
                    // We specifically allow space characters inside here, so that
                    // `{  }` can be successfully parsed as an empty record, and then
                    // changed by the formatter back into `{}`.
                    //
                    // We don't allow newlines or comments in the middle of empty
                    // collections because those are normally stored in an Expr,
                    // and there's no Expr in which to store them in an empty collection!
                    //
                    // We could change the AST to add extra storage specifically to
                    // support empty literals containing newlines or comments, but this
                    // does not seem worth even the tiniest regression in compiler performance.
                    zero_or_more!($crate::parse::parser::char(' ')),
                    skip_second!(
                        $crate::parse::parser::sep_by0(
                            $crate::parse::parser::char(','),
                            $crate::parse::blankspace::space0_around(
                                loc!(record_field!($val_parser, $min_indent)),
                                $min_indent
                            )
                        ),
                        $crate::parse::parser::char('}')
                    )
                ))
            )
        )
    };
}

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn and<'a, P1, P2, A, B>(p1: P1, p2: P2) -> impl Parser<'a, (A, B)>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
    P1: 'a,
    P2: 'a,
    A: 'a,
    B: 'a,
{
    and!(p1, p2)
}

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn loc<'a, P, Val>(parser: P) -> impl Parser<'a, Located<Val>>
where
    P: Parser<'a, Val>,
{
    loc!(parser)
}

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn map<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
where
    P: Parser<'a, Before>,
    F: Fn(Before) -> After,
{
    map!(parser, transform)
}

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn map_with_arena<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
where
    P: Parser<'a, Before>,
    P: 'a,
    F: Fn(&'a Bump, Before) -> After,
    F: 'a,
    Before: 'a,
    After: 'a,
{
    map_with_arena!(parser, transform)
}

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn attempt<'a, P, Val>(attempting: Attempting, parser: P) -> impl Parser<'a, Val>
where
    P: Parser<'a, Val>,
{
    attempt!(attempting, parser)
}
