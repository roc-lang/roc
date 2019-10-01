use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use parse::ast::Attempting;
use region::{Located, Region};
use std::{char, u16};

/// A position in a source file.
#[derive(Debug, Clone, PartialEq)]
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

    /// Increments the line, then resets column, indent_col, and is_indenting.
    /// Advances the input by 1, to consume the newline character.
    pub fn newline(&self) -> Result<Self, (Fail, Self)> {
        match self.line.checked_add(1) {
            Some(line) => Ok(State {
                input: &self.input[1..],
                line,
                column: 0,
                indent_col: 1,
                is_indenting: true,
                attempting: self.attempting,
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
                })
            }
            _ => Err(line_too_long(self.attempting, self.clone())),
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fail {
    pub attempting: Attempting,
    pub reason: FailReason,
}

pub trait Parser<'a, Output> {
    fn parse(&self, &'a Bump, State<'a>) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a Bump, State<'a>) -> ParseResult<'a, Output>,
{
    fn parse(&self, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Output> {
        self(arena, state)
    }
}

pub fn not_followed_by<'a, P, ByParser, By, Val>(parser: P, by: ByParser) -> impl Parser<'a, Val>
where
    ByParser: Parser<'a, By>,
    P: Parser<'a, Val>,
{
    move |arena, state: State<'a>| {
        parser.parse(arena, state).and_then(|(answer, state)| {
            let original_state = state.clone();

            match by.parse(arena, state) {
                Ok((_, state)) => Err((
                    Fail {
                        attempting: state.attempting,
                        reason: FailReason::ConditionFailed,
                    },
                    original_state,
                )),
                Err(_) => Ok((answer, original_state)),
            }
        })
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

#[cfg(not(debug_assertions))]
pub fn map<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
where
    P: Parser<'a, Before>,
    F: Fn(Before) -> After,
{
    map_impl(parser, transform)
}

#[inline(always)]
fn map_impl<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
where
    P: Parser<'a, Before>,
    F: Fn(Before) -> After,
{
    move |arena, state| {
        parser
            .parse(arena, state)
            .map(|(output, next_state)| (transform(output), next_state))
    }
}

#[cfg(not(debug_assertions))]
pub fn attempt<'a, P, Val>(attempting: Attempting, parser: P) -> impl Parser<'a, Val>
where
    P: Parser<'a, Val>,
{
    attempt_impl(attempting, parser)
}

#[inline(always)]
pub fn attempt_impl<'a, P, Val>(attempting: Attempting, parser: P) -> impl Parser<'a, Val>
where
    P: Parser<'a, Val>,
{
    move |arena, state| {
        parser.parse(
            arena,
            State {
                attempting,
                ..state
            },
        )
    }
}

#[cfg(not(debug_assertions))]
pub fn loc<'a, P, Val>(parser: P) -> impl Parser<'a, Located<Val>>
where
    P: Parser<'a, Val>,
{
    loc_impl(parser)
}

#[inline(always)]
pub fn loc_impl<'a, P, Val>(parser: P) -> impl Parser<'a, Located<Val>>
where
    P: Parser<'a, Val>,
{
    move |arena, state: State<'a>| {
        let start_col = state.column;
        let start_line = state.line;

        match parser.parse(arena, state) {
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
}

#[cfg(not(debug_assertions))]
pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<'a, A>>
where
    P: Parser<'a, A>,
{
    zero_or_more_impl(parser)
}

#[inline(always)]
pub fn zero_or_more_impl<'a, P, A>(parser: P) -> impl Parser<'a, Vec<'a, A>>
where
    P: Parser<'a, A>,
{
    move |arena, state| match parser.parse(arena, state) {
        Ok((first_output, next_state)) => {
            let mut state = next_state;
            let mut buf = Vec::with_capacity_in(1, arena);

            buf.push(first_output);

            loop {
                match parser.parse(arena, state) {
                    Ok((next_output, next_state)) => {
                        state = next_state;
                        buf.push(next_output);
                    }
                    Err((_, old_state)) => return Ok((buf, old_state)),
                }
            }
        }
        Err((_, new_state)) => return Ok((Vec::new_in(arena), new_state)),
    }
}

#[cfg(not(debug_assertions))]
pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<'a, A>>
where
    P: Parser<'a, A>,
{
    one_or_more_impl(parser)
}

#[inline(always)]
pub fn one_or_more_impl<'a, P, A>(parser: P) -> impl Parser<'a, Vec<'a, A>>
where
    P: Parser<'a, A>,
{
    move |arena, state| match parser.parse(arena, state) {
        Ok((first_output, next_state)) => {
            let mut state = next_state;
            let mut buf = Vec::with_capacity_in(1, arena);

            buf.push(first_output);

            loop {
                match parser.parse(arena, state) {
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

pub fn unexpected_eof<'a>(
    chars_consumed: usize,
    attempting: Attempting,
    state: State<'a>,
) -> (Fail, State<'a>) {
    checked_unexpected(chars_consumed, state, |region| Fail {
        reason: FailReason::Eof(region),
        attempting,
    })
}

pub fn unexpected<'a>(
    ch: char,
    chars_consumed: usize,
    state: State<'a>,
    attempting: Attempting,
) -> (Fail, State<'a>) {
    checked_unexpected(chars_consumed, state, |region| Fail {
        reason: FailReason::Unexpected(ch, region),
        attempting,
    })
}

/// Check for line overflow, then compute a new Region based on chars_consumed
/// and provide it as a way to construct a Problem.
/// If maximum line length was exceeded, return a Problem indicating as much.
#[inline(always)]
fn checked_unexpected<'a, F>(
    chars_consumed: usize,
    state: State<'a>,
    problem_from_region: F,
) -> (Fail, State<'a>)
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

fn line_too_long<'a>(attempting: Attempting, state: State<'a>) -> (Fail, State<'a>) {
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
    };

    (fail, state)
}

/// A single char.
#[cfg(not(debug_assertions))]
pub fn char<'a>(expected: char) -> impl Parser<'a, ()> {
    char_impl(expected)
}

#[inline(always)]
pub fn char_impl<'a>(expected: char) -> impl Parser<'a, ()> {
    move |_arena, state: State<'a>| match state.input.chars().next() {
        Some(actual) if expected == actual => Ok(((), state.advance_without_indenting(1)?)),
        Some(other_ch) => Err(unexpected(other_ch, 0, state, Attempting::Keyword)),
        _ => Err(unexpected_eof(0, Attempting::Keyword, state)),
    }
}

/// A hardcoded keyword string with no newlines in it.
#[cfg(not(debug_assertions))]
pub fn string<'a>(keyword: &'static str) -> impl Parser<'a, ()> {
    string_impl(keyword)
}

#[inline(always)]
pub fn string_impl<'a>(keyword: &'static str) -> impl Parser<'a, ()> {
    // We can't have newlines because we don't attempt to advance the row
    // in the state, only the column.
    debug_assert!(!keyword.contains("\n"));

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

/// Parse everything between two braces (e.g. parentheses), skipping both braces
/// and keeping only whatever was parsed in between them.
pub fn between<'a, P, OpeningBrace, ClosingBrace, Val>(
    opening_brace: OpeningBrace,
    parser: P,
    closing_brace: ClosingBrace,
) -> impl Parser<'a, Val>
where
    OpeningBrace: Parser<'a, ()>,
    P: Parser<'a, Val>,
    ClosingBrace: Parser<'a, ()>,
{
    skip_first(opening_brace, skip_second(parser, closing_brace))
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
            Err((_, new_state)) => return Ok((Vec::new_in(arena), new_state)),
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

#[cfg(not(debug_assertions))]
pub fn and<'a, P1, P2, A, B>(p1: P1, p2: P2) -> impl Parser<'a, (A, B)>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
{
    and_impl(p1, p2)
}

#[inline(always)]
fn and_impl<'a, P1, P2, A, B>(p1: P1, p2: P2) -> impl Parser<'a, (A, B)>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
{
    move |arena: &'a Bump, state: State<'a>| {
        // We have to clone this because if the first parser passes and then
        // the second one fails, we need to revert back to the original state.
        let original_state = state.clone();

        match p1.parse(arena, state) {
            Ok((out1, state)) => match p2.parse(arena, state) {
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
}

#[cfg(not(debug_assertions))]
pub fn either<'a, P1, P2, A, B>(p1: P1, p2: P2) -> impl Parser<'a, Either<A, B>>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
{
    either_impl(p1, p2)
}

#[inline(always)]
pub fn either_impl<'a, P1, P2, A, B>(p1: P1, p2: P2) -> impl Parser<'a, Either<A, B>>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            Ok((output, state)) => Ok((Either::First(output), state)),
            Err((_, state)) => match p2.parse(arena, state) {
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
}

/// If the first one parses, ignore its output and move on to parse with the second one.
pub fn skip_first<'a, P1, P2, A, B>(p1: P1, p2: P2) -> impl Parser<'a, B>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            Ok((_, state)) => match p2.parse(arena, state) {
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
}

/// If the first one parses, parse the second one; if it also parses, use the
/// output from the first one.
pub fn skip_second<'a, P1, P2, A, B>(p1: P1, p2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            Ok((out1, state)) => match p2.parse(arena, state) {
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
}

pub fn optional<'a, P, T>(parser: P) -> impl Parser<'a, Option<T>>
where
    P: Parser<'a, T>,
{
    move |arena: &'a Bump, state: State<'a>| match parser.parse(arena, state) {
        Ok((out1, state)) => Ok((Some(out1), state)),
        Err((_, state)) => Ok((None, state)),
    }
}

pub fn one_of2<'a, P1, P2, A>(p1: P1, p2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            valid @ Ok(_) => valid,
            Err((_, state)) => match p2.parse(arena, state) {
                valid @ Ok(_) => valid,
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
}

pub fn one_of3<'a, P1, P2, P3, A>(p1: P1, p2: P2, p3: P3) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
    P3: Parser<'a, A>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            valid @ Ok(_) => valid,
            Err((_, state)) => match p2.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((_, state)) => match p3.parse(arena, state) {
                    valid @ Ok(_) => valid,
                    Err((fail, state)) => Err((
                        Fail {
                            attempting: original_attempting,
                            ..fail
                        },
                        state,
                    )),
                },
            },
        }
    }
}

pub fn one_of4<'a, P1, P2, P3, P4, A>(p1: P1, p2: P2, p3: P3, p4: P4) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
    P3: Parser<'a, A>,
    P4: Parser<'a, A>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            valid @ Ok(_) => valid,
            Err((_, state)) => match p2.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((_, state)) => match p3.parse(arena, state) {
                    valid @ Ok(_) => valid,
                    Err((_, state)) => match p4.parse(arena, state) {
                        valid @ Ok(_) => valid,
                        Err((fail, state)) => Err((
                            Fail {
                                attempting: original_attempting,
                                ..fail
                            },
                            state,
                        )),
                    },
                },
            },
        }
    }
}

pub fn one_of5<'a, P1, P2, P3, P4, P5, A>(
    p1: P1,
    p2: P2,
    p3: P3,
    p4: P4,
    p5: P5,
) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
    P3: Parser<'a, A>,
    P4: Parser<'a, A>,
    P5: Parser<'a, A>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            valid @ Ok(_) => valid,
            Err((_, state)) => match p2.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((_, state)) => match p3.parse(arena, state) {
                    valid @ Ok(_) => valid,
                    Err((_, state)) => match p4.parse(arena, state) {
                        valid @ Ok(_) => valid,
                        Err((_, state)) => match p5.parse(arena, state) {
                            valid @ Ok(_) => valid,
                            Err((fail, state)) => Err((
                                Fail {
                                    attempting: original_attempting,
                                    ..fail
                                },
                                state,
                            )),
                        },
                    },
                },
            },
        }
    }
}

pub fn one_of6<'a, P1, P2, P3, P4, P5, P6, A>(
    p1: P1,
    p2: P2,
    p3: P3,
    p4: P4,
    p5: P5,
    p6: P6,
) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
    P3: Parser<'a, A>,
    P4: Parser<'a, A>,
    P5: Parser<'a, A>,
    P6: Parser<'a, A>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            valid @ Ok(_) => valid,
            Err((_, state)) => match p2.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((_, state)) => match p3.parse(arena, state) {
                    valid @ Ok(_) => valid,
                    Err((_, state)) => match p4.parse(arena, state) {
                        valid @ Ok(_) => valid,
                        Err((_, state)) => match p5.parse(arena, state) {
                            valid @ Ok(_) => valid,
                            Err((_, state)) => match p6.parse(arena, state) {
                                valid @ Ok(_) => valid,
                                Err((fail, state)) => Err((
                                    Fail {
                                        attempting: original_attempting,
                                        ..fail
                                    },
                                    state,
                                )),
                            },
                        },
                    },
                },
            },
        }
    }
}

pub fn one_of7<'a, P1, P2, P3, P4, P5, P6, P7, A>(
    p1: P1,
    p2: P2,
    p3: P3,
    p4: P4,
    p5: P5,
    p6: P6,
    p7: P7,
) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
    P3: Parser<'a, A>,
    P4: Parser<'a, A>,
    P5: Parser<'a, A>,
    P6: Parser<'a, A>,
    P7: Parser<'a, A>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            valid @ Ok(_) => valid,
            Err((_, state)) => match p2.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((_, state)) => match p3.parse(arena, state) {
                    valid @ Ok(_) => valid,
                    Err((_, state)) => match p4.parse(arena, state) {
                        valid @ Ok(_) => valid,
                        Err((_, state)) => match p5.parse(arena, state) {
                            valid @ Ok(_) => valid,
                            Err((_, state)) => match p6.parse(arena, state) {
                                valid @ Ok(_) => valid,
                                Err((_, state)) => match p7.parse(arena, state) {
                                    valid @ Ok(_) => valid,
                                    Err((fail, state)) => Err((
                                        Fail {
                                            attempting: original_attempting,
                                            ..fail
                                        },
                                        state,
                                    )),
                                },
                            },
                        },
                    },
                },
            },
        }
    }
}

pub fn one_of8<'a, P1, P2, P3, P4, P5, P6, P7, P8, A>(
    p1: P1,
    p2: P2,
    p3: P3,
    p4: P4,
    p5: P5,
    p6: P6,
    p7: P7,
    p8: P8,
) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
    P3: Parser<'a, A>,
    P4: Parser<'a, A>,
    P5: Parser<'a, A>,
    P6: Parser<'a, A>,
    P7: Parser<'a, A>,
    P8: Parser<'a, A>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            valid @ Ok(_) => valid,
            Err((_, state)) => match p2.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((_, state)) => match p3.parse(arena, state) {
                    valid @ Ok(_) => valid,
                    Err((_, state)) => match p4.parse(arena, state) {
                        valid @ Ok(_) => valid,
                        Err((_, state)) => match p5.parse(arena, state) {
                            valid @ Ok(_) => valid,
                            Err((_, state)) => match p6.parse(arena, state) {
                                valid @ Ok(_) => valid,
                                Err((_, state)) => match p7.parse(arena, state) {
                                    valid @ Ok(_) => valid,
                                    Err((_, state)) => match p8.parse(arena, state) {
                                        valid @ Ok(_) => valid,
                                        Err((fail, state)) => Err((
                                            Fail {
                                                attempting: original_attempting,
                                                ..fail
                                            },
                                            state,
                                        )),
                                    },
                                },
                            },
                        },
                    },
                },
            },
        }
    }
}

pub fn one_of9<'a, P1, P2, P3, P4, P5, P6, P7, P8, P9, A>(
    p1: P1,
    p2: P2,
    p3: P3,
    p4: P4,
    p5: P5,
    p6: P6,
    p7: P7,
    p8: P8,
    p9: P9,
) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
    P3: Parser<'a, A>,
    P4: Parser<'a, A>,
    P5: Parser<'a, A>,
    P6: Parser<'a, A>,
    P7: Parser<'a, A>,
    P8: Parser<'a, A>,
    P9: Parser<'a, A>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            valid @ Ok(_) => valid,
            Err((_, state)) => match p2.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((_, state)) => match p3.parse(arena, state) {
                    valid @ Ok(_) => valid,
                    Err((_, state)) => match p4.parse(arena, state) {
                        valid @ Ok(_) => valid,
                        Err((_, state)) => match p5.parse(arena, state) {
                            valid @ Ok(_) => valid,
                            Err((_, state)) => match p6.parse(arena, state) {
                                valid @ Ok(_) => valid,
                                Err((_, state)) => match p7.parse(arena, state) {
                                    valid @ Ok(_) => valid,
                                    Err((_, state)) => match p8.parse(arena, state) {
                                        valid @ Ok(_) => valid,
                                        Err((_, state)) => match p9.parse(arena, state) {
                                            valid @ Ok(_) => valid,
                                            Err((fail, state)) => Err((
                                                Fail {
                                                    attempting: original_attempting,
                                                    ..fail
                                                },
                                                state,
                                            )),
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        }
    }
}

// DEBUG COMBINATORS
//
// These use dyn for runtime dynamic dispatch. It prevents combinatoric
// explosions in types (and thus monomorphization, and thus build time),
// but has runtime overhead, so we only use these in debug builds.

#[cfg(debug_assertions)]
pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

#[cfg(debug_assertions)]
impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

#[cfg(debug_assertions)]
impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Output> {
        self.parser.parse(arena, state)
    }
}

#[cfg(debug_assertions)]
pub fn map<'a, P, F, Before, After>(parser: P, transform: F) -> BoxedParser<'a, After>
where
    P: Parser<'a, Before>,
    F: Fn(Before) -> After,
    F: 'a,
    P: 'a,
    Before: 'a,
    After: 'a,
{
    BoxedParser::new(map_impl(parser, transform))
}

#[cfg(debug_assertions)]
pub fn and<'a, P1, P2, A, B>(p1: P1, p2: P2) -> BoxedParser<'a, (A, B)>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
    P1: 'a,
    P2: 'a,
    A: 'a,
    B: 'a,
{
    BoxedParser::new(and_impl(p1, p2))
}

#[cfg(not(debug_assertions))]
pub fn map_with_arena<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
where
    P: Parser<'a, Before>,
    F: Fn(&'a Bump, Before) -> After,
{
    map_with_arena_impl(parser, transform)
}

#[inline(always)]
fn map_with_arena_impl<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
where
    P: Parser<'a, Before>,
    F: Fn(&'a Bump, Before) -> After,
{
    move |arena, state| {
        parser
            .parse(arena, state)
            .map(|(output, next_state)| (transform(arena, output), next_state))
    }
}

#[cfg(debug_assertions)]
pub fn map_with_arena<'a, P, F, Before, After>(parser: P, transform: F) -> BoxedParser<'a, After>
where
    P: Parser<'a, Before>,
    P: 'a,
    F: Fn(&'a Bump, Before) -> After,
    F: 'a,
    Before: 'a,
    After: 'a,
{
    BoxedParser::new(map_with_arena_impl(parser, transform))
}

#[cfg(debug_assertions)]
pub fn loc<'a, P, Val>(parser: P) -> BoxedParser<'a, Located<Val>>
where
    P: Parser<'a, Val>,
    P: 'a,
    Val: 'a,
{
    BoxedParser::new(loc_impl(parser))
}

#[cfg(debug_assertions)]
pub fn attempt<'a, P, Val>(attempting: Attempting, parser: P) -> BoxedParser<'a, Val>
where
    P: Parser<'a, Val>,
    P: 'a,
    Val: 'a,
{
    BoxedParser::new(attempt_impl(attempting, parser))
}

#[cfg(debug_assertions)]
pub fn zero_or_more<'a, P, A>(parser: P) -> BoxedParser<'a, Vec<'a, A>>
where
    P: Parser<'a, A>,
    P: 'a,
{
    BoxedParser::new(zero_or_more_impl(parser))
}

#[cfg(debug_assertions)]
pub fn char<'a>(expected: char) -> BoxedParser<'a, ()> {
    BoxedParser::new(char_impl(expected))
}

#[cfg(debug_assertions)]
pub fn string<'a>(keyword: &'static str) -> BoxedParser<'a, ()> {
    BoxedParser::new(string_impl(keyword))
}

#[cfg(debug_assertions)]
pub fn either<'a, P1, P2, A, B>(p1: P1, p2: P2) -> BoxedParser<'a, Either<A, B>>
where
    P1: Parser<'a, A>,
    P1: 'a,
    P2: Parser<'a, B>,
    P2: 'a,
    A: 'a,
    B: 'a,
{
    BoxedParser::new(either_impl(p1, p2))
}

#[cfg(debug_assertions)]
pub fn one_or_more<'a, P, A>(parser: P) -> BoxedParser<'a, Vec<'a, A>>
where
    P: Parser<'a, A>,
    P: 'a,
{
    BoxedParser::new(one_or_more_impl(parser))
}
