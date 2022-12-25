use crate::state::State;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_ast2::{EExpr, EHeader, EInput, EPattern, EType};
use roc_region::all::{Loc, Position, Region};
use Progress::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Either<First, Second> {
    First(First),
    Second(Second),
}

impl<F: Copy, S: Copy> Copy for Either<F, S> {}

pub type ParseResult<'a, Output, Error> = Result<(Progress, Output, State<'a>), (Progress, Error)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Progress {
    MadeProgress,
    NoProgress,
}

impl Progress {
    pub fn from_lengths(before: usize, after: usize) -> Self {
        Self::from_consumed(before - after)
    }
    pub fn from_consumed(chars_consumed: usize) -> Self {
        Self::progress_when(chars_consumed != 0)
    }

    pub fn progress_when(made_progress: bool) -> Self {
        if made_progress {
            Progress::MadeProgress
        } else {
            Progress::NoProgress
        }
    }

    pub fn or(&self, other: Self) -> Self {
        if (*self == MadeProgress) || (other == MadeProgress) {
            MadeProgress
        } else {
            NoProgress
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxError<'a> {
    Unexpected(Region),
    OutdentedTooFar,
    TooManyLines,
    Eof(Region),
    InvalidPattern,
    BadUtf8,
    ReservedKeyword(Region),
    ArgumentsBeforeEquals(Region),
    NotYetImplemented(String),
    Todo,
    Type(EType<'a>),
    Pattern(EPattern<'a>),
    Expr(EExpr<'a>, Position),
    Header(EHeader<'a>),
    Space(EInput),
    NotEndOfFile(Position),
}

impl<'a, T> SourceError<'a, T> {
    pub fn new(problem: T, state: &State<'a>) -> Self {
        Self {
            problem,
            bytes: state.original_bytes(),
        }
    }

    pub fn map_problem<E>(self, f: impl FnOnce(T) -> E) -> SourceError<'a, E> {
        SourceError {
            problem: f(self.problem),
            bytes: self.bytes,
        }
    }

    pub fn into_file_error(self, filename: std::path::PathBuf) -> FileError<'a, T> {
        FileError {
            problem: self,
            filename,
        }
    }
}

impl<'a> SyntaxError<'a> {
    pub fn into_source_error(self, state: &State<'a>) -> SourceError<'a, SyntaxError<'a>> {
        SourceError {
            problem: self,
            bytes: state.original_bytes(),
        }
    }

    pub fn into_file_error(
        self,
        filename: std::path::PathBuf,
        state: &State<'a>,
    ) -> FileError<'a, SyntaxError<'a>> {
        self.into_source_error(state).into_file_error(filename)
    }
}

#[derive(Debug)]
pub struct SourceError<'a, T> {
    pub problem: T,
    pub bytes: &'a [u8],
}

#[derive(Debug)]
pub struct FileError<'a, T> {
    pub problem: SourceError<'a, T>,
    pub filename: std::path::PathBuf,
}

pub trait Parser<'a, Output, Error> {
    fn parse(
        &self,
        arena: &'a Bump,
        state: State<'a>,
        min_indent: u32,
    ) -> ParseResult<'a, Output, Error>;

    #[cfg(not(feature = "parse_debug_trace"))]
    #[inline(always)]
    fn trace(self, _message: &'static str) -> Self
    where
        Self: Sized,
        Output: std::fmt::Debug,
        Error: std::fmt::Debug,
    {
        self
    }

    #[cfg(feature = "parse_debug_trace")]
    fn trace(self, message: &'static str) -> Traced<'a, Output, Error, Self>
    where
        Self: Sized,
        Output: std::fmt::Debug,
        Error: std::fmt::Debug,
    {
        Traced {
            parser: self,
            message,
            _phantom: Default::default(),
        }
    }
}

impl<'a, F, Output, Error> Parser<'a, Output, Error> for F
where
    Error: 'a,
    F: Fn(&'a Bump, State<'a>, u32) -> ParseResult<'a, Output, Error>,
{
    fn parse(
        &self,
        arena: &'a Bump,
        state: State<'a>,
        min_indent: u32,
    ) -> ParseResult<'a, Output, Error> {
        self(arena, state, min_indent)
    }
}

#[cfg(feature = "parse_debug_trace")]
pub struct Traced<'a, O, E, P: Parser<'a, O, E>> {
    parser: P,
    message: &'static str,
    _phantom: std::marker::PhantomData<&'a (O, E)>,
}

#[cfg(feature = "parse_debug_trace")]
impl<'a, O: std::fmt::Debug, E: std::fmt::Debug, P: Parser<'a, O, E>> Parser<'a, O, E>
    for Traced<'a, O, E, P>
where
    E: 'a,
{
    fn parse(&self, arena: &'a Bump, state: State<'a>, min_indent: u32) -> ParseResult<'a, O, E> {
        use std::cell::RefCell;

        thread_local! {
            pub static INDENT: RefCell<usize> = RefCell::new(0);
        }

        // This should be enough for anyone. Right? RIGHT?
        let indent_text =
            "| ; : ! | ; : ! | ; : ! | ; : ! | ; : ! | ; : ! | ; : ! | ; : ! | ; : ! ";

        let cur_indent = INDENT.with(|i| *i.borrow());

        println!(
            "{:<5?}: {}{:<50}",
            state.pos(),
            &indent_text[..cur_indent * 2],
            self.message
        );

        let previous_state = state.clone();
        INDENT.with(|i| *i.borrow_mut() += 1);
        let res = self.parser.parse(arena, state, min_indent);
        INDENT.with(|i| *i.borrow_mut() = cur_indent);

        let (progress, value, state) = match &res {
            Ok((progress, result, state)) => (progress, Ok(result), state),
            Err((progress, error)) => (progress, Err(error), &previous_state),
        };

        println!(
            "{:<5?}: {}{:<50} {:<15} {:?}",
            state.pos(),
            &indent_text[..cur_indent * 2],
            self.message,
            format!("{:?}", progress),
            value
        );

        res
    }
}

pub fn allocated<'a, P, Val, Error>(parser: P) -> impl Parser<'a, &'a Val, Error>
where
    Error: 'a,
    P: Parser<'a, Val, Error>,
    Val: 'a,
{
    move |arena, state: State<'a>, min_indent: u32| {
        let (progress, answer, state) = parser.parse(arena, state, min_indent)?;

        Ok((progress, &*arena.alloc(answer), state))
    }
}

pub fn and_then<'a, P1, P2, F, Before, After, Error>(
    parser: P1,
    transform: F,
) -> impl Parser<'a, After, Error>
where
    P1: Parser<'a, Before, Error>,
    P2: Parser<'a, After, Error>,
    F: Fn(Progress, Before) -> P2,
    Error: 'a,
{
    move |arena, state, min_indent| {
        parser
            .parse(arena, state, min_indent)
            .and_then(|(progress, output, next_state)| {
                transform(progress, output).parse(arena, next_state, min_indent)
            })
    }
}

pub fn then<'a, P1, F, Before, After, E>(parser: P1, transform: F) -> impl Parser<'a, After, E>
where
    P1: Parser<'a, Before, E>,
    After: 'a,
    E: 'a,
    F: Fn(&'a Bump, State<'a>, Progress, Before) -> ParseResult<'a, After, E>,
{
    move |arena, state, min_indent| {
        parser
            .parse(arena, state, min_indent)
            .and_then(|(progress, output, next_state)| {
                transform(arena, next_state, progress, output)
            })
    }
}

pub fn keyword_e<'a, ToError, E>(keyword: &'static str, if_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    move |_, mut state: State<'a>, _min_indent| {
        let width = keyword.len();

        if !state.bytes().starts_with(keyword.as_bytes()) {
            return Err((NoProgress, if_error(state.pos())));
        }

        // the next character should not be an identifier character
        // to prevent treating `whence` or `iffy` as keywords
        match state.bytes().get(width) {
            Some(next) if *next == b' ' || *next == b'#' || *next == b'\n' || *next == b'\r' => {
                state = state.advance(width);
                Ok((MadeProgress, (), state))
            }
            None => {
                state = state.advance(width);
                Ok((MadeProgress, (), state))
            }
            Some(_) => Err((NoProgress, if_error(state.pos()))),
        }
    }
}

/// Parse zero or more values separated by a delimiter (e.g. a comma) whose
/// values are discarded
pub fn sep_by0<'a, P, D, Val, Error>(
    delimiter: D,
    parser: P,
) -> impl Parser<'a, Vec<'a, Val>, Error>
where
    D: Parser<'a, (), Error>,
    P: Parser<'a, Val, Error>,
    Error: 'a,
{
    move |arena, state: State<'a>, min_indent: u32| {
        let original_state = state.clone();

        let start_bytes_len = state.bytes().len();

        match parser.parse(arena, state, min_indent) {
            Ok((elem_progress, first_output, next_state)) => {
                // in practice, we want elements to make progress
                debug_assert_eq!(elem_progress, MadeProgress);

                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    match delimiter.parse(arena, state.clone(), min_indent) {
                        Ok((_, (), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state.clone(), min_indent) {
                                Ok((element_progress, next_output, next_state)) => {
                                    // in practice, we want elements to make progress
                                    debug_assert_eq!(element_progress, MadeProgress);

                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((_, fail)) => {
                                    // If the delimiter parsed, but the following
                                    // element did not, that's a fatal error.
                                    let progress = Progress::from_lengths(
                                        start_bytes_len,
                                        next_state.bytes().len(),
                                    );

                                    return Err((progress, fail));
                                }
                            }
                        }
                        Err((delim_progress, fail)) => match delim_progress {
                            MadeProgress => return Err((MadeProgress, fail)),
                            NoProgress => return Ok((NoProgress, buf, state)),
                        },
                    }
                }
            }
            Err((element_progress, fail)) => match element_progress {
                MadeProgress => Err((MadeProgress, fail)),
                NoProgress => Ok((NoProgress, Vec::new_in(arena), original_state)),
            },
        }
    }
}

/// Parse zero or more values separated by a delimiter (e.g. a comma)
/// with an optional trailing delimiter whose values are discarded
pub fn trailing_sep_by0<'a, P, D, Val, Error>(
    delimiter: D,
    parser: P,
) -> impl Parser<'a, Vec<'a, Val>, Error>
where
    D: Parser<'a, (), Error>,
    P: Parser<'a, Val, Error>,
    Error: 'a,
{
    move |arena, state: State<'a>, min_indent: u32| {
        let original_state = state.clone();
        let start_bytes_len = state.bytes().len();

        match parser.parse(arena, state, min_indent) {
            Ok((progress, first_output, next_state)) => {
                // in practice, we want elements to make progress
                debug_assert_eq!(progress, MadeProgress);
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    match delimiter.parse(arena, state.clone(), min_indent) {
                        Ok((_, (), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state.clone(), min_indent) {
                                Ok((element_progress, next_output, next_state)) => {
                                    // in practice, we want elements to make progress
                                    debug_assert_eq!(element_progress, MadeProgress);

                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((_, _fail)) => {
                                    // If the delimiter parsed, but the following
                                    // element did not, that means we saw a trailing comma
                                    let progress = Progress::from_lengths(
                                        start_bytes_len,
                                        next_state.bytes().len(),
                                    );
                                    return Ok((progress, buf, next_state));
                                }
                            }
                        }
                        Err((delim_progress, fail)) => match delim_progress {
                            MadeProgress => return Err((MadeProgress, fail)),
                            NoProgress => return Ok((NoProgress, buf, state)),
                        },
                    }
                }
            }
            Err((element_progress, fail)) => match element_progress {
                MadeProgress => Err((MadeProgress, fail)),
                NoProgress => Ok((NoProgress, Vec::new_in(arena), original_state)),
            },
        }
    }
}

/// Parse one or more values separated by a delimiter (e.g. a comma) whose
/// values are discarded
pub fn sep_by1<'a, P, D, Val, Error>(
    delimiter: D,
    parser: P,
) -> impl Parser<'a, Vec<'a, Val>, Error>
where
    D: Parser<'a, (), Error>,
    P: Parser<'a, Val, Error>,
    Error: 'a,
{
    move |arena, state: State<'a>, min_indent: u32| {
        let start_bytes_len = state.bytes().len();

        match parser.parse(arena, state, min_indent) {
            Ok((progress, first_output, next_state)) => {
                debug_assert_eq!(progress, MadeProgress);
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    let old_state = state.clone();
                    match delimiter.parse(arena, state, min_indent) {
                        Ok((_, (), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state, min_indent) {
                                Ok((_, next_output, next_state)) => {
                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((_, fail)) => {
                                    return Err((MadeProgress, fail));
                                }
                            }
                        }
                        Err((delim_progress, fail)) => {
                            match delim_progress {
                                MadeProgress => {
                                    // fail if the delimiter made progress
                                    return Err((MadeProgress, fail));
                                }
                                NoProgress => {
                                    let progress = Progress::from_lengths(
                                        start_bytes_len,
                                        old_state.bytes().len(),
                                    );
                                    return Ok((progress, buf, old_state));
                                }
                            }
                        }
                    }
                }
            }
            Err((fail_progress, fail)) => Err((fail_progress, fail)),
        }
    }
}

/// Parse one or more values separated by a delimiter (e.g. a comma) whose
/// values are discarded
pub fn sep_by1_e<'a, P, V, D, Val, Error>(
    delimiter: D,
    parser: P,
    to_element_error: V,
) -> impl Parser<'a, Vec<'a, Val>, Error>
where
    D: Parser<'a, (), Error>,
    P: Parser<'a, Val, Error>,
    V: Fn(Position) -> Error,
    Error: 'a,
{
    move |arena, state: State<'a>, min_indent: u32| {
        let original_state = state.clone();
        let start_bytes_len = state.bytes().len();

        match parser.parse(arena, state, min_indent) {
            Ok((progress, first_output, next_state)) => {
                debug_assert_eq!(progress, MadeProgress);
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    let old_state = state.clone();
                    match delimiter.parse(arena, state, min_indent) {
                        Ok((_, (), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state.clone(), min_indent) {
                                Ok((_, next_output, next_state)) => {
                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((MadeProgress, fail)) => {
                                    return Err((MadeProgress, fail));
                                }
                                Err((NoProgress, _fail)) => {
                                    return Err((NoProgress, to_element_error(next_state.pos())));
                                }
                            }
                        }
                        Err((delim_progress, fail)) => {
                            match delim_progress {
                                MadeProgress => {
                                    // fail if the delimiter made progress
                                    return Err((MadeProgress, fail));
                                }
                                NoProgress => {
                                    let progress = Progress::from_lengths(
                                        start_bytes_len,
                                        old_state.bytes().len(),
                                    );
                                    return Ok((progress, buf, old_state));
                                }
                            }
                        }
                    }
                }
            }

            Err((MadeProgress, fail)) => Err((MadeProgress, fail)),
            Err((NoProgress, _fail)) => Err((NoProgress, to_element_error(original_state.pos()))),
        }
    }
}

pub fn optional<'a, P, T, E>(parser: P) -> impl Parser<'a, Option<T>, E>
where
    P: Parser<'a, T, E>,
    E: 'a,
{
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| {
        // We have to clone this because if the optional parser fails,
        // we need to revert back to the original state.
        let original_state = state.clone();

        match parser.parse(arena, state, min_indent) {
            Ok((progress, out1, state)) => Ok((progress, Some(out1), state)),
            Err((MadeProgress, e)) => Err((MadeProgress, e)),
            Err((NoProgress, _)) => Ok((NoProgress, None, original_state)),
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
        move |arena, state: $crate::state::State<'a>, min_indent: u32| {
            use roc_region::all::{Loc, Region};

            let start = state.pos();

            match $parser.parse(arena, state, min_indent) {
                Ok((progress, value, state)) => {
                    let end = state.pos();
                    let region = Region::new(start, end);

                    Ok((progress, Loc { region, value }, state))
                }
                Err(err) => Err(err),
            }
        }
    };
}

/// If the first one parses, ignore its output and move on to parse with the second one.
#[macro_export]
macro_rules! skip_first {
    ($p1:expr, $p2:expr) => {
        move |arena, state: $crate::state::State<'a>, min_indent: u32| match $p1
            .parse(arena, state, min_indent)
        {
            Ok((p1, _, state)) => match $p2.parse(arena, state, min_indent) {
                Ok((p2, out2, state)) => Ok((p1.or(p2), out2, state)),
                Err((p2, fail)) => Err((p1.or(p2), fail)),
            },
            Err((progress, fail)) => Err((progress, fail)),
        }
    };
}

/// If the first one parses, parse the second one; if it also parses, use the
/// output from the first one.
#[macro_export]
macro_rules! skip_second {
    ($p1:expr, $p2:expr) => {
        move |arena, state: $crate::state::State<'a>, min_indent: u32| match $p1
            .parse(arena, state, min_indent)
        {
            Ok((p1, out1, state)) => match $p2.parse(arena, state, min_indent) {
                Ok((p2, _, state)) => Ok((p1.or(p2), out1, state)),
                Err((p2, fail)) => Err((p1.or(p2), fail)),
            },
            Err((progress, fail)) => Err((progress, fail)),
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
                // roc_collections because those are normally stored in an Expr,
                // and there's no Expr in which to store them in an empty collection!
                //
                // We could change the AST to add extra storage specifically to
                // support empty literals containing newlines or comments, but this
                // does not seem worth even the tiniest regression in compiler performance.
                zero_or_more!($crate::parser::ascii_char(b' ')),
                skip_second!(
                    $crate::parser::sep_by0(
                        $delimiter,
                        $crate::blankspace::space0_around($elem, $min_indent)
                    ),
                    $closing_brace
                )
            )
        )
    };
}

#[macro_export]
macro_rules! collection_trailing_sep_e {
    ($opening_brace:expr, $elem:expr, $delimiter:expr, $closing_brace:expr, $indent_problem:expr, $space_before:expr) => {
        map_with_arena!(
            skip_first!(
                $opening_brace,
                and!(
                    and!(
                        space0_e($indent_problem),
                        $crate::parser::trailing_sep_by0(
                            $delimiter,
                            $crate::blankspace::space0_before_optional_after(
                                $elem,
                                $indent_problem,
                                $indent_problem
                            )
                        )
                    ),
                    skip_second!(
                        $crate::parser::reset_min_indent($crate::blankspace::space0_e(
                            $indent_problem
                        )),
                        $closing_brace
                    )
                )
            ),
            |arena: &'a bumpalo::Bump,
             ((spaces, mut parsed_elems), mut final_comments): (
                (
                    &'a [$crate::ast2_for_macros::CommentOrNewline<'a>],
                    bumpalo::collections::vec::Vec<'a, Loc<_>>
                ),
                &'a [$crate::ast2_for_macros::CommentOrNewline<'a>]
            )| {
                if !spaces.is_empty() {
                    if let Some(first) = parsed_elems.first_mut() {
                        first.value = $space_before(arena.alloc(first.value), spaces)
                    } else {
                        assert!(final_comments.is_empty());
                        final_comments = spaces;
                    }
                }

                $crate::ast2_for_macros::Collection::with_items_and_comments(
                    arena,
                    parsed_elems.into_bump_slice(),
                    final_comments,
                )
            }
        )
    };
}

#[macro_export]
macro_rules! succeed {
    ($value:expr) => {
        move |_arena: &'a bumpalo::Bump, state: $crate::state::State<'a>, _min_indent: u32| {
            Ok((NoProgress, $value, state))
        }
    };
}

pub fn fail_when<'a, T, T2, E, F, P>(f: F, p: P) -> impl Parser<'a, T, E>
where
    T: 'a,
    T2: 'a,
    E: 'a,
    F: Fn(Position) -> E,
    P: Parser<'a, T2, E>,
{
    move |arena: &'a bumpalo::Bump, state: State<'a>, min_indent: u32| {
        let original_state = state.clone();
        match p.parse(arena, state, min_indent) {
            Ok((_, _, _)) => Err((MadeProgress, f(original_state.pos()))),
            Err((progress, err)) => Err((progress, err)),
        }
    }
}

pub fn fail<'a, T, E, F>(f: F) -> impl Parser<'a, T, E>
where
    T: 'a,
    E: 'a,
    F: Fn(Position) -> E,
{
    move |_arena: &'a bumpalo::Bump, state: State<'a>, _min_indent: u32| {
        Err((NoProgress, f(state.pos())))
    }
}

#[macro_export]
macro_rules! and {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>, min_indent: u32| match $p1
            .parse(arena, state, min_indent)
        {
            Ok((p1, out1, state)) => match $p2.parse(arena, state, min_indent) {
                Ok((p2, out2, state)) => Ok((p1.or(p2), (out1, out2), state)),
                Err((p2, fail)) => Err((p1.or(p2), fail)),
            },
            Err((progress, fail)) => Err((progress, fail)),
        }
    };
}

/// Take as input something that looks like a struct literal where values are parsers
/// and return a parser that runs each parser and returns a struct literal with the
/// results.
#[macro_export]
macro_rules! record {
    ($name:ident $(:: $name_ext:ident)* { $($field:ident: $parser:expr),* $(,)? }) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>, min_indent: u32| {
            let mut state = state;
            let mut progress = NoProgress;
            $(
                let (new_progress, $field, new_state) = $parser.parse(arena, state, min_indent)?;
                state = new_state;
                progress = progress.or(new_progress);
            )*
            Ok((progress, $name $(:: $name_ext)* { $($field),* }, state))
        }
    };
}

/// Similar to `and`, but we modify the min_indent of the second parser to be
/// 1 greater than the line_indent() at the start of the first parser.
#[macro_export]
macro_rules! indented_seq {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>, _min_indent: u32| {
            let start_indent = state.line_indent();

            // TODO: we should account for min_indent here, but this doesn't currently work
            // because min_indent is sometimes larger than it really should be, which is in turn
            // due to uses of `increment_indent`.
            //
            // let p1_indent = std::cmp::max(start_indent, min_indent);

            let p1_indent = start_indent;
            let p2_indent = p1_indent + 1;

            match $p1.parse(arena, state, p1_indent) {
                Ok((p1, (), state)) => match $p2.parse(arena, state, p2_indent) {
                    Ok((p2, out2, state)) => Ok((p1.or(p2), out2, state)),
                    Err((p2, fail)) => Err((p1.or(p2), fail)),
                },
                Err((progress, fail)) => Err((progress, fail)),
            }
        }
    };
}

/// Similar to `and`, but we modify the min_indent of the second parser to be
/// 1 greater than the column() at the start of the first parser.
#[macro_export]
macro_rules! absolute_indented_seq {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>, _min_indent: u32| {
            let start_indent = state.column();

            let p1_indent = start_indent;
            let p2_indent = p1_indent + 1;

            match $p1.parse(arena, state, p1_indent) {
                Ok((p1, out1, state)) => match $p2.parse(arena, state, p2_indent) {
                    Ok((p2, out2, state)) => Ok((p1.or(p2), (out1, out2), state)),
                    Err((p2, fail)) => Err((p1.or(p2), fail)),
                },
                Err((progress, fail)) => Err((progress, fail)),
            }
        }
    };
}

#[macro_export]
macro_rules! one_of {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>, min_indent: u32| {
            let original_state = state.clone();

            match $p1.parse(arena, state, min_indent) {
                valid @ Ok(_) => valid,
                Err((MadeProgress, fail)) => Err((MadeProgress, fail)),
                Err((NoProgress, _)) => $p2.parse(arena, original_state, min_indent),
            }
        }
    };

    ($p1:expr, $($others:expr),+) => {
        one_of!($p1, one_of!($($others),+))
    };
    ($p1:expr, $($others:expr),+ $(,)?) => {
        one_of!($p1, $($others),+)
    };
}

#[macro_export]
macro_rules! one_of_with_error {
    ($toerror:expr; $p1:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>, min_indent: u32| {
            let original_state = state.clone();

            match $p1.parse(arena, state, min_indent) {
                valid @ Ok(_) => valid,
                Err((MadeProgress, fail)) => Err((MadeProgress, fail)),
                Err((NoProgress, _)) => Err((MadeProgress, $toerror(original_state.pos()))),
            }
        }
    };

    ($toerror:expr; $p1:expr, $($others:expr),+) => {
        one_of_with_error!($toerror, $p1, one_of_with_error!($($others),+))
    };
}

pub fn reset_min_indent<'a, P, T, X: 'a>(parser: P) -> impl Parser<'a, T, X>
where
    P: Parser<'a, T, X>,
{
    move |arena, state, _min_indent| parser.parse(arena, state, 0)
}

pub fn set_min_indent<'a, P, T, X: 'a>(min_indent: u32, parser: P) -> impl Parser<'a, T, X>
where
    P: Parser<'a, T, X>,
{
    move |arena, state, _m| parser.parse(arena, state, min_indent)
}

pub fn increment_min_indent<'a, P, T, X: 'a>(parser: P) -> impl Parser<'a, T, X>
where
    P: Parser<'a, T, X>,
{
    move |arena, state, min_indent| parser.parse(arena, state, min_indent + 1)
}

pub fn line_min_indent<'a, P, T, X: 'a>(parser: P) -> impl Parser<'a, T, X>
where
    P: Parser<'a, T, X>,
{
    move |arena, state: State<'a>, min_indent| {
        let min_indent = std::cmp::max(state.line_indent(), min_indent);
        parser.parse(arena, state, min_indent)
    }
}

pub fn absolute_column_min_indent<'a, P, T, X: 'a>(parser: P) -> impl Parser<'a, T, X>
where
    P: Parser<'a, T, X>,
{
    move |arena, state: State<'a>, _min_indent| {
        let min_indent = state.column() + 1;
        parser.parse(arena, state, min_indent)
    }
}

pub fn specialize<'a, F, P, T, X, Y>(map_error: F, parser: P) -> impl Parser<'a, T, Y>
where
    F: Fn(X, Position) -> Y,
    P: Parser<'a, T, X>,
    Y: 'a,
{
    move |a, state: State<'a>, min_indent| {
        let original_state = state.clone();
        match parser.parse(a, state, min_indent) {
            Ok(t) => Ok(t),
            Err((p, error)) => Err((p, map_error(error, original_state.pos()))),
        }
    }
}

pub fn specialize_ref<'a, F, P, T, X, Y>(map_error: F, parser: P) -> impl Parser<'a, T, Y>
where
    F: Fn(&'a X, Position) -> Y,
    P: Parser<'a, T, X>,
    Y: 'a,
    X: 'a,
{
    move |a, state: State<'a>, min_indent| {
        let original_state = state.clone();
        match parser.parse(a, state, min_indent) {
            Ok(t) => Ok(t),
            Err((p, error)) => Err((p, map_error(a.alloc(error), original_state.pos()))),
        }
    }
}

pub fn word1<'a, ToError, E>(word: u8, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(word, b'\n');

    move |_arena: &'a Bump, state: State<'a>, _min_indent: u32| match state.bytes().first() {
        Some(x) if *x == word => {
            let state = state.advance(1);
            Ok((MadeProgress, (), state))
        }
        _ => Err((NoProgress, to_error(state.pos()))),
    }
}

pub fn word1_indent<'a, ToError, E>(word: u8, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(word, b'\n');

    move |_arena: &'a Bump, state: State<'a>, min_indent: u32| {
        if min_indent > state.column() {
            return Err((NoProgress, to_error(state.pos())));
        }

        match state.bytes().first() {
            Some(x) if *x == word => {
                let state = state.advance(1);
                Ok((MadeProgress, (), state))
            }
            _ => Err((NoProgress, to_error(state.pos()))),
        }
    }
}

pub fn word2<'a, ToError, E>(word_1: u8, word_2: u8, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(word_1, b'\n');
    debug_assert_ne!(word_2, b'\n');

    let needle = [word_1, word_2];

    move |_arena: &'a Bump, state: State<'a>, _min_indent: u32| {
        if state.bytes().starts_with(&needle) {
            let state = state.advance(2);
            Ok((MadeProgress, (), state))
        } else {
            Err((NoProgress, to_error(state.pos())))
        }
    }
}

pub fn word3<'a, ToError, E>(
    word_1: u8,
    word_2: u8,
    word_3: u8,
    to_error: ToError,
) -> impl Parser<'a, (), E>
where
    ToError: Fn(Position) -> E,
    E: 'a,
{
    debug_assert_ne!(word_1, b'\n');
    debug_assert_ne!(word_2, b'\n');
    debug_assert_ne!(word_3, b'\n');

    let needle = [word_1, word_2, word_3];

    move |_arena: &'a Bump, state: State<'a>, _min_indent: u32| {
        if state.bytes().starts_with(&needle) {
            let state = state.advance(3);
            Ok((MadeProgress, (), state))
        } else {
            Err((NoProgress, to_error(state.pos())))
        }
    }
}

#[macro_export]
macro_rules! word1_check_indent {
    ($word:expr, $word_problem:expr, $min_indent:expr, $indent_problem:expr) => {
        and!(
            word1($word, $word_problem),
            $crate::parser::check_indent($min_indent, $indent_problem)
        )
    };
}

#[macro_export]
macro_rules! map {
    ($parser:expr, $transform:expr) => {
        move |arena, state, min_indent| {
            $parser
                .parse(arena, state, min_indent)
                .map(|(progress, output, next_state)| (progress, $transform(output), next_state))
        }
    };
}

#[macro_export]
macro_rules! map_with_arena {
    ($parser:expr, $transform:expr) => {
        move |arena, state, min_indent| {
            $parser
                .parse(arena, state, min_indent)
                .map(|(progress, output, next_state)| {
                    (progress, $transform(arena, output), next_state)
                })
        }
    };
}

#[macro_export]
macro_rules! zero_or_more {
    ($parser:expr) => {
        move |arena, state: State<'a>, min_indent: u32| {
            use bumpalo::collections::Vec;

            let original_state = state.clone();

            let start_bytes_len = state.bytes().len();

            match $parser.parse(arena, state, min_indent) {
                Ok((_, first_output, next_state)) => {
                    let mut state = next_state;
                    let mut buf = Vec::with_capacity_in(1, arena);

                    buf.push(first_output);

                    loop {
                        let old_state = state.clone();
                        match $parser.parse(arena, state, min_indent) {
                            Ok((_, next_output, next_state)) => {
                                state = next_state;
                                buf.push(next_output);
                            }
                            Err((fail_progress, fail)) => {
                                match fail_progress {
                                    MadeProgress => {
                                        // made progress on an element and then failed; that's an error
                                        return Err((MadeProgress, fail));
                                    }
                                    NoProgress => {
                                        // the next element failed with no progress
                                        // report whether we made progress before
                                        let progress = Progress::from_lengths(start_bytes_len, old_state.bytes().len());
                                        return Ok((progress, buf, old_state));
                                    }
                                }
                            }
                        }
                    }
                }
                Err((fail_progress, fail)) => {
                    match fail_progress {
                        MadeProgress => {
                            // made progress on an element and then failed; that's an error
                            Err((MadeProgress, fail))
                        }
                        NoProgress => {
                            // the first element failed (with no progress), but that's OK
                            // because we only need to parse 0 elements
                            Ok((NoProgress, Vec::new_in(arena), original_state))
                        }
                    }
                }
            }
        }
    };
}

#[macro_export]
macro_rules! one_or_more {
    ($parser:expr, $to_error:expr) => {
        move |arena, state: State<'a>, min_indent: u32| {
            use bumpalo::collections::Vec;

            match $parser.parse(arena, state.clone(), min_indent) {
                Ok((_, first_output, next_state)) => {
                    let mut state = next_state;
                    let mut buf = Vec::with_capacity_in(1, arena);

                    buf.push(first_output);

                    loop {
                        let old_state = state.clone();
                        match $parser.parse(arena, state, min_indent) {
                            Ok((_, next_output, next_state)) => {
                                state = next_state;
                                buf.push(next_output);
                            }
                            Err((NoProgress, _)) => {
                                return Ok((MadeProgress, buf, old_state));
                            }
                            Err((MadeProgress, fail)) => {
                                return Err((MadeProgress, fail));
                            }
                        }
                    }
                }
                Err((progress, _)) => Err((progress, $to_error(state.pos()))),
            }
        }
    };
}

#[macro_export]
macro_rules! debug {
    ($parser:expr) => {
        move |arena, state: $crate::state::State<'a>, min_indent: u32| {
            dbg!($parser.parse(arena, state, min_indent))
        }
    };
}

#[macro_export]
macro_rules! either {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::state::State<'a>, min_indent: u32| {
            let original_state = state.clone();
            match $p1.parse(arena, state, min_indent) {
                Ok((progress, output, state)) => {
                    Ok((progress, $crate::parser::Either::First(output), state))
                }
                Err((NoProgress, _)) => {
                    match $p2.parse(arena, original_state.clone(), min_indent) {
                        Ok((progress, output, state)) => {
                            Ok((progress, $crate::parser::Either::Second(output), state))
                        }
                        Err((progress, fail)) => Err((progress, fail)),
                    }
                }
                Err((MadeProgress, fail)) => Err((MadeProgress, fail)),
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

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn and<'a, P1, P2, A, B, E>(p1: P1, p2: P2) -> impl Parser<'a, (A, B), E>
where
    P1: Parser<'a, A, E>,
    P2: Parser<'a, B, E>,
    P1: 'a,
    P2: 'a,
    A: 'a,
    B: 'a,
    E: 'a,
{
    and!(p1, p2)
}

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn loc<'a, P, Val, Error>(parser: P) -> impl Parser<'a, Loc<Val>, Error>
where
    P: Parser<'a, Val, Error>,
    Error: 'a,
{
    loc!(parser)
}

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn map_with_arena<'a, P, F, Before, After, E>(
    parser: P,
    transform: F,
) -> impl Parser<'a, After, E>
where
    P: Parser<'a, Before, E>,
    P: 'a,
    F: Fn(&'a Bump, Before) -> After,
    F: 'a,
    Before: 'a,
    After: 'a,
    E: 'a,
{
    map_with_arena!(parser, transform)
}

pub fn backtrackable<'a, P, Val, Error>(parser: P) -> impl Parser<'a, Val, Error>
where
    P: Parser<'a, Val, Error>,
    Error: 'a,
{
    move |arena: &'a Bump, state: State<'a>, min_indent: u32| match parser
        .parse(arena, state, min_indent)
    {
        Ok((_, a, s1)) => Ok((NoProgress, a, s1)),
        Err((_, f)) => Err((NoProgress, f)),
    }
}
