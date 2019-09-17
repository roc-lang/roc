use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use parse::ast::Attempting;
use region::{Located, Region};
use std::char;

// Strategy:
//
// 1. Let space parsers check indentation. They should expect indentation to only ever increase (right?) when
//    doing a many_whitespaces or many1_whitespaces. Multline strings can have separate whitespace parsers.
// 2. For any expression that has subexpressions (e.g. ifs, parens, operators) record their indentation levels
//    by doing .and(position()) followed by .and_then() which says "I can have a declaration inside me as
//    long as the entire decl is indented more than me."
// 3. Make an alternative to RangeStreamOnce where uncons_while barfs on \t (or maybe just do this in whitespaces?)

/// Struct which represents a position in a source file.
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

impl<'a> State<'a> {
    pub fn new(input: &'a str, attempting: Attempting) -> State<'a> {
        State {
            input,
            line: 0,
            column: 0,
            indent_col: 1,
            is_indenting: true,
            attempting,
        }
    }

    /// Increments the line, then resets column, indent_col, and is_indenting.
    /// This does *not* advance the input.
    pub fn newline(&self) -> Result<Self, (Fail, Self)> {
        match self.line.checked_add(1) {
            Some(line) => Ok(State {
                input: self.input,
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
            Some(column_usize) if column_usize <= std::u16::MAX as usize => {
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
            _ => Err((
                Fail {
                    reason: FailReason::LineTooLong(self.line),
                    attempting: self.attempting,
                },
                self.clone(),
            )),
        }
    }
    /// Advance the parser while also indenting as appropriate.
    /// This assumes we are only advancing with spaces, since they can indent.
    pub fn advance_spaces(&self, spaces: usize) -> Result<Self, (Fail, Self)> {
        match (self.column as usize).checked_add(spaces) {
            Some(column_usize) if column_usize <= std::u16::MAX as usize => {
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
                    debug_assert!(std::u16::MAX - self.indent_col >= spaces as u16);
                    debug_assert!(spaces <= std::u16::MAX as usize);

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
            _ => Err((
                Fail {
                    reason: FailReason::LineTooLong(self.line),
                    attempting: self.attempting,
                },
                self.clone(),
            )),
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
    UnexpectedKeyword(Keyword),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    If,
    Then,
    Else,
    Case,
    When,
}

impl Keyword {
    pub fn from_str(kw: &str) -> Option<Keyword> {
        match kw {
            "if" => Some(Keyword::If),
            "then" => Some(Keyword::Then),
            "else" => Some(Keyword::Else),
            "case" => Some(Keyword::Case),
            "when" => Some(Keyword::When),
            _ => None,
        }
    }
}

pub trait Parser<'a, Output> {
    fn parse(&self, &'a Bump, State<'a>) -> ParseResult<'a, Output>;
}

pub struct BoxedParser<'a, Output> {
    parser: &'a (dyn Parser<'a, Output> + 'a),
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(arena: &'a Bump, parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: arena.alloc(parser),
        }
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a Bump, State<'a>) -> ParseResult<'a, Output>,
{
    fn parse(&self, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Output> {
        self(arena, state)
    }
}

pub fn val<'a, Val>(value: Val) -> impl Parser<'a, Val>
where
    Val: Clone,
{
    move |_, state| Ok((value.clone(), state))
}

/// Needed for recursive parsers
pub fn lazy<'a, F, P, Val>(get_parser: F) -> impl Parser<'a, Val>
where
    F: Fn() -> P,
    P: Parser<'a, Val>,
{
    move |arena, state| get_parser().parse(arena, state)
}

pub fn map<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
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

pub fn map_with_arena<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
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

pub fn attempt<'a, P, Val>(attempting: Attempting, parser: P) -> impl Parser<'a, Val>
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

pub fn loc<'a, P, Val>(parser: P) -> impl Parser<'a, Located<Val>>
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

pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<'a, A>>
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
        Some(end_col) if end_col <= std::u16::MAX as usize => {
            let region = Region {
                start_col: state.column,
                end_col: end_col as u16,
                start_line: state.line,
                end_line: state.line,
            };

            (problem_from_region(region), state)
        }
        _ => {
            let reason = FailReason::LineTooLong(state.line);
            let attempting = state.attempting;
            let fail = Fail { reason, attempting };

            (fail, state)
        }
    }
}

/// A string with no newlines in it.
pub fn string<'a>(string: &'static str) -> impl Parser<'a, ()> {
    // We can't have newlines because we don't attempt to advance the row
    // in the state, only the column.
    debug_assert!(!string.contains("\n"));

    move |_arena: &'a Bump, state: State<'a>| {
        let input = state.input;
        let len = string.len();

        // TODO do this comparison in one SIMD instruction (on supported systems)
        match input.get(0..len) {
            Some(next_str) if next_str == string => Ok(((), state.advance_without_indenting(len)?)),
            _ => Err(unexpected_eof(len, Attempting::Keyword, state)),
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

// pub fn any<'a>(
//     _arena: &'a Bump,
//     state: State<'a>,
//     attempting: Attempting,
// ) -> ParseResult<'a, char> {
//     let input = state.input;

//     match input.chars().next() {
//         Some(ch) => {
//             let len = ch.len_utf8();
//             let mut new_state = State {
//                 input: &input[len..],

//                 ..state.clone()
//             };

//             if ch == '\n' {
//                 new_state.line = new_state.line + 1;
//                 new_state.column = 0;
//             }

//             Ok((new_state, ch))
//         }
//         _ => Err((state.clone(), attempting)),
//     }
// }

// fn whitespace<'a>() -> impl Parser<'a, char> {
//     // TODO advance the state appropriately, in terms of line, col, indenting, etc.
//     satisfies(any, |ch| ch.is_whitespace())
// }

pub fn and<'a, P1, P2, A, B>(p1: P1, p2: P2) -> impl Parser<'a, (A, B)>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
{
    move |arena: &'a Bump, state: State<'a>| {
        let original_attempting = state.attempting;

        match p1.parse(arena, state) {
            Ok((out1, state)) => match p2.parse(arena, state) {
                Ok((out2, state)) => Ok(((out1, out2), state)),
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
