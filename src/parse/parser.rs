use bumpalo::Bump;
use parse::ast::Attempting;
use parse::problems::Problems;
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
    pub column: u32,

    /// Current indentation level, in columns
    /// (so no indent is col 1 - this saves an arithmetic operation.)
    pub indent_col: u32,

    // true at the beginning of each line, then false after encountering
    // the first nonspace char on that line.
    pub is_indenting: bool,
}

#[test]
fn state_size() {
    // State should always be under 8 machine words, so it fits in a typical
    // cache line.
    assert!(std::mem::size_of::<State>() <= std::mem::size_of::<usize>() * 8);
}

impl<'a> State<'a> {
    pub fn from_input(input: &'a str) -> State<'a> {
        State {
            input,
            line: 0,
            column: 0,
            indent_col: 1,
            is_indenting: true,
        }
    }
}

pub type ParseResult<'a, Output> = Result<(State<'a>, Output), (State<'a>, Attempting)>;

pub trait Parser<'a, 'p, Output> {
    fn parse(
        &self,
        &'a Bump,
        &'a State<'a>,
        problems: &'p mut Problems,
        attempting: Attempting,
    ) -> ParseResult<'a, Output>;
}

impl<'a, 'p, F, Output> Parser<'a, 'p, Output> for F
where
    F: Fn(&'a Bump, &'a State<'a>, &'p mut Problems, Attempting) -> ParseResult<'a, Output>,
{
    fn parse(
        &self,
        arena: &'a Bump,
        state: &'a State<'a>,
        problems: &'p mut Problems,
        attempting: Attempting,
    ) -> ParseResult<'a, Output> {
        self(arena, state, problems, attempting)
    }
}

fn map<'a, 'p, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, 'p, After>
where
    P: Parser<'a, 'p, Before>,
    F: Fn(Before) -> After,
    'p: 'a,
{
    move |arena, state, problems, attempting| {
        parser
            .parse(arena, state, problems, attempting)
            .map(|(next_state, output)| (next_state, transform(output)))
    }
}

fn attempt<'a, 'p, P, Val>(attempting: Attempting, parser: P) -> impl Parser<'a, 'p, Val>
where
    P: Parser<'a, 'p, Val>,
    'p: 'a,
{
    move |arena, state, problems, _| parser.parse(arena, state, problems, attempting)
}

/// A keyword with no newlines in it.
fn keyword<'a, 'p>(kw: &'static str) -> impl Parser<'a, 'p, ()>
where
    'p: 'a,
{
    // We can't have newlines because we don't attempt to advance the row
    // in the state, only the column.
    debug_assert!(!kw.contains("\n"));

    move |_arena: &'a Bump, state: &'a State<'a>, _problems, attempting| {
        let input = state.input;

        match input.get(0..kw.len()) {
            Some(next) if next == kw => {
                let len = kw.len();

                Ok((
                    State {
                        input: &input[len..],
                        column: state.column + len as u32,

                        ..state.clone()
                    },
                    (),
                ))
            }
            _ => Err((state.clone(), attempting)),
        }
    }
}

fn satisfies<'a, 'p, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, 'p, A>
where
    P: Parser<'a, 'p, A>,
    F: Fn(&A) -> bool,
    'p: 'a,
{
    move |arena: &'a Bump, state: &'a State<'a>, problems, attempting| {
        if let Ok((next_state, output)) = parser.parse(arena, state, problems, attempting) {
            if predicate(&output) {
                return Ok((next_state, output));
            }
        }

        Err((state.clone(), attempting))
    }
}

fn any<'a, 'p>(
    _arena: &'a Bump,
    state: &'a State<'a>,
    _problems: &'p mut Problems,
    attempting: Attempting,
) -> ParseResult<'a, char> {
    let input = state.input;

    match input.chars().next() {
        Some(ch) => {
            let len = ch.len_utf8();
            let mut new_state = State {
                input: &input[len..],

                ..state.clone()
            };

            if ch == '\n' {
                new_state.line = new_state.line + 1;
                new_state.column = 0;
            }

            Ok((new_state, ch))
        }
        _ => Err((state.clone(), attempting)),
    }
}

fn whitespace<'a, 'p>() -> impl Parser<'a, 'p, char>
where
    'p: 'a,
{
    satisfies(any, |ch| ch.is_whitespace())
}

#[inline(always)]
fn is_ascii_number(ch: char) -> bool {
    let ascii_val = ch as u8;

    // the ASCII numbers 0-9
    ascii_val >= 48 && ascii_val <= 57
}
