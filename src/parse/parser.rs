use bumpalo::Bump;
use parse::ast::Attempting;
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

    /// Increments the line, then resets column, indent_col, and is_indenting.
    /// This does *not* advance the input.
    pub fn newline(&self) -> Self {
        let line = self
            .line
            .checked_add(1)
            .unwrap_or_else(panic_max_line_count_exceeded);

        State {
            input: self.input,
            line,
            column: 0,
            indent_col: 1,
            is_indenting: true,
        }
    }

    /// Use advance_spaces to advance with indenting.
    /// This assumes we are *not* advancing with spaces, or at least that
    /// any spaces on the line were preceded by non-spaces - which would mean
    /// they weren't eligible to indent anyway.
    pub fn advance_without_indenting(&self, quantity: usize) -> Self {
        let column_usize = (self.column as usize)
            .checked_add(quantity)
            .unwrap_or_else(panic_max_line_length_exceeded);

        if column_usize > std::u16::MAX as usize {
            panic_max_line_length_exceeded();
        }

        State {
            input: &self.input[quantity..],
            line: self.line,
            column: column_usize as u16,
            indent_col: self.indent_col,
            // Once we hit a nonspace character, we are no longer indenting.
            is_indenting: false,
        }
    }
    /// Advance the parser while also indenting as appropriate.
    /// This assumes we are only advancing with spaces, since they can indent.
    pub fn advance_spaces(&self, spaces: usize) -> Self {
        // We'll cast this to u16 later.
        debug_assert!(spaces <= std::u16::MAX as usize);

        let column_usize = (self.column as usize)
            .checked_add(spaces)
            .unwrap_or_else(panic_max_line_length_exceeded);

        if column_usize > std::u16::MAX as usize {
            panic_max_line_length_exceeded();
        }

        // Spaces don't affect is_indenting; if we were previously indneting,
        // we still are, and if we already finished indenting, we're still done.
        let is_indenting = self.is_indenting;

        // If we're indenting, spaces indent us further.
        let indent_col = if is_indenting {
            // This doesn't need to be checked_add because it's always true that
            // indent_col <= col, so if this could possibly overflow, we would
            // already have panicked from the column calculation.
            //
            // Leaving a debug_assert! in case this invariant someday disappers.
            debug_assert!(std::u16::MAX - self.indent_col >= spaces as u16);

            self.indent_col + spaces as u16
        } else {
            self.indent_col
        };

        State {
            input: &self.input[spaces..],
            line: self.line,
            column: column_usize as u16,
            indent_col,
            is_indenting,
        }
    }
}

#[inline(never)]
fn panic_max_line_count_exceeded() -> u32 {
    panic!(
        "Maximum line count exceeded. Roc only supports compiling files with at most {} lines.",
        std::u32::MAX
    )
}

#[inline(never)]
fn panic_max_line_length_exceeded() -> usize {
    panic!(
"Maximum line length exceeded. Roc only supports compiling files whose lines each contain no more than {} characters.",
        std::u16::MAX
    )
}

#[test]
fn state_size() {
    // State should always be under 8 machine words, so it fits in a typical
    // cache line.
    assert!(std::mem::size_of::<State>() <= std::mem::size_of::<usize>() * 8);
}

pub type ParseResult<'a, Output> = Result<(State<'a>, Output), (State<'a>, Attempting)>;

pub trait Parser<'a, Output> {
    fn parse(&self, &'a Bump, State<'a>, attempting: Attempting) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a Bump, State<'a>, Attempting) -> ParseResult<'a, Output>,
{
    fn parse(
        &self,
        arena: &'a Bump,
        state: State<'a>,
        attempting: Attempting,
    ) -> ParseResult<'a, Output> {
        self(arena, state, attempting)
    }
}

pub fn map<'a, P, F, Before, After>(parser: P, transform: F) -> impl Parser<'a, After>
where
    P: Parser<'a, Before>,
    F: Fn(Before) -> After,
{
    move |arena, state, attempting| {
        parser
            .parse(arena, state, attempting)
            .map(|(next_state, output)| (next_state, transform(output)))
    }
}

pub fn attempt<'a, P, Val>(attempting: Attempting, parser: P) -> impl Parser<'a, Val>
where
    P: Parser<'a, Val>,
{
    move |arena, state, _| parser.parse(arena, state, attempting)
}

/// A keyword with no newlines in it.
pub fn keyword<'a>(kw: &'static str) -> impl Parser<'a, ()> {
    // We can't have newlines because we don't attempt to advance the row
    // in the state, only the column.
    debug_assert!(!kw.contains("\n"));

    move |_arena: &'a Bump, state: State<'a>, attempting| {
        let input = state.input;

        match input.get(0..kw.len()) {
            Some(next) if next == kw => {
                let len = kw.len();

                Ok((state.advance_without_indenting(len), ()))
            }
            _ => Err((state.clone(), attempting)),
        }
    }
}

pub fn satisfies<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |arena: &'a Bump, state: State<'a>, attempting| {
        if let Ok((next_state, output)) = parser.parse(arena, state, attempting) {
            if predicate(&output) {
                return Ok((next_state, output));
            }
        }

        Err((state.clone(), attempting))
    }
}

pub fn any<'a>(
    _arena: &'a Bump,
    state: State<'a>,
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

fn whitespace<'a>() -> impl Parser<'a, char> {
    // TODO advance the state appropriately, in terms of line, col, indenting, etc.
    satisfies(any, |ch| ch.is_whitespace())
}
