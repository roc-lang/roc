use crate::ast::Attempting;
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use encode_unicode::CharExt;
use roc_region::all::{Located, Region};
use std::fmt;
use std::str::from_utf8;
use std::{char, u16};
use Progress::*;

/// A position in a source file.
#[derive(Clone, PartialEq, Eq)]
pub struct State<'a> {
    /// The raw input bytes from the file.
    pub bytes: &'a [u8],

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

    pub context_stack: &'a ContextStack<'a>,

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
    pub fn new_in(arena: &'a Bump, bytes: &'a [u8], _attempting: Attempting) -> State<'a> {
        State {
            bytes,
            line: 0,
            column: 0,
            indent_col: 0,
            is_indenting: true,
            context_stack: arena.alloc(ContextStack::Nil),
            original_len: bytes.len(),
        }
    }

    pub fn check_indent(
        self,
        _arena: &'a Bump,
        min_indent: u16,
    ) -> Result<Self, (SyntaxError<'a>, Self)> {
        if self.indent_col < min_indent {
            Err((SyntaxError::OutdentedTooFar, self))
        } else {
            Ok(self)
        }
    }

    pub fn check_indent_e<TE, E>(
        self,
        _arena: &'a Bump,
        min_indent: u16,
        to_error: TE,
        row: Row,
        col: Col,
    ) -> Result<Self, (E, Self)>
    where
        TE: Fn(Row, Col) -> E,
    {
        if self.indent_col < min_indent {
            Err((to_error(row, col), self))
        } else {
            Ok(self)
        }
    }

    /// Returns the total number of bytes consumed since the parser began parsing.
    ///
    /// So if the parser has consumed 8 bytes, this function will return 8.
    pub fn bytes_consumed(&self) -> usize {
        self.original_len - self.bytes.len()
    }

    /// Returns whether the parser has reached the end of the input
    pub fn has_reached_end(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Increments the line, then resets column, indent_col, and is_indenting.
    /// Advances the input by 1, to consume the newline character.
    pub fn newline(&self, arena: &'a Bump) -> Result<Self, (Progress, SyntaxError<'a>, Self)> {
        self.newline_e(arena, |_, _, _| SyntaxError::TooManyLines)
    }

    pub fn newline_e<TE, E>(
        &self,
        arena: &'a Bump,
        to_error: TE,
    ) -> Result<Self, (Progress, E, Self)>
    where
        TE: Fn(BadInputError, Row, Col) -> E,
    {
        match self.line.checked_add(1) {
            Some(line) => Ok(State {
                bytes: &self.bytes[1..],
                line,
                column: 0,
                indent_col: 0,
                is_indenting: true,
                original_len: self.original_len,
                context_stack: arena.alloc(self.context_stack.clone()),
            }),
            None => Err((
                Progress::NoProgress,
                to_error(BadInputError::TooManyLines, self.line, self.column),
                self.clone(),
            )),
        }
    }

    /// Use advance_spaces to advance with indenting.
    /// This assumes we are *not* advancing with spaces, or at least that
    /// any spaces on the line were preceded by non-spaces - which would mean
    /// they weren't eligible to indent anyway.
    pub fn advance_without_indenting(
        self,
        quantity: usize,
    ) -> Result<Self, (Progress, SyntaxError<'a>, Self)> {
        self.advance_without_indenting_ee(quantity, |line, _| SyntaxError::LineTooLong(line))
    }

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
                    is_indenting: false,
                    ..self
                })
            }
            _ => Err(line_too_long_e(self.clone(), to_error)),
        }
    }

    pub fn advance_spaces(
        &self,
        arena: &'a Bump,
        spaces: usize,
    ) -> Result<Self, (Progress, SyntaxError<'a>, Self)> {
        self.advance_spaces_e(arena, spaces, |line, _| SyntaxError::LineTooLong(line))
    }

    /// Advance the parser while also indenting as appropriate.
    /// This assumes we are only advancing with spaces, since they can indent.
    pub fn advance_spaces_e<TE, E>(
        &self,
        arena: &'a Bump,
        spaces: usize,
        to_error: TE,
    ) -> Result<Self, (Progress, E, Self)>
    where
        TE: Fn(Row, Col) -> E,
    {
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
                    bytes: &self.bytes[spaces..],
                    line: self.line,
                    column: column_usize as u16,
                    indent_col,
                    is_indenting,
                    context_stack: arena.alloc(self.context_stack.clone()),
                    original_len: self.original_len,
                })
            }
            _ => Err(line_too_long_e(self.clone(), to_error)),
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

        match from_utf8(self.bytes) {
            Ok(string) => write!(f, "\n\tbytes: [utf8] {:?}", string)?,
            Err(_) => write!(f, "\n\tbytes: [invalid utf8] {:?}", self.bytes)?,
        }

        write!(f, "\n\t(line, col): ({}, {}),", self.line, self.column)?;
        write!(f, "\n\tindent_col: {}", self.indent_col)?;
        write!(f, "\n\tis_indenting: {:?}", self.is_indenting)?;
        write!(f, "\n\toriginal_len: {}", self.original_len)?;
        write!(f, "\n\tcontext stack: {:?}", self.context_stack)?;
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

pub type ParseResult<'a, Output, Error> =
    Result<(Progress, Output, State<'a>), (Progress, Error, State<'a>)>;

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
    ConditionFailed,
    LineTooLong(u32 /* which line was too long */),
    TooManyLines,
    Eof(Region),
    InvalidPattern,
    BadUtf8,
    ReservedKeyword(Region),
    ArgumentsBeforeEquals(Region),
    NotYetImplemented(String),
    TODO,
    Type(Type<'a>),
    Pattern(EPattern<'a>),
    Expr(EExpr<'a>),
    Space(BadInputError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BadInputError {
    HasTab,
    ///
    LineTooLong,
    TooManyLines,
    ///
    ///
    BadUtf8,
}

pub fn bad_input_to_syntax_error<'a>(
    bad_input: BadInputError,
    row: Row,
    _col: Col,
) -> SyntaxError<'a> {
    use crate::parser::BadInputError::*;
    match bad_input {
        HasTab => SyntaxError::NotYetImplemented("call error on tabs".to_string()),
        LineTooLong => SyntaxError::LineTooLong(row),
        TooManyLines => SyntaxError::TooManyLines,
        BadUtf8 => SyntaxError::BadUtf8,
    }
}

impl<'a> SyntaxError<'a> {
    pub fn into_parse_problem(
        self,
        filename: std::path::PathBuf,
        bytes: &'a [u8],
    ) -> ParseProblem<'a, SyntaxError<'a>> {
        ParseProblem {
            line: 0,
            column: 0,
            problem: self,
            filename,
            bytes,
        }
    }
}

pub type Row = u32;
pub type Col = u16;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EExpr<'a> {
    Start(Row, Col),
    End(Row, Col),
    Space(BadInputError, Row, Col),

    Dot(Row, Col),
    Access(Row, Col),
    UnaryNot(Row, Col),
    UnaryNegate(Row, Col),
    BinOp(roc_module::operator::BinOp, Row, Col),

    Def(&'a SyntaxError<'a>, Row, Col),
    Type(Type<'a>, Row, Col),
    Pattern(&'a EPattern<'a>, Row, Col),
    IndentDefBody(Row, Col),
    IndentEquals(Row, Col),
    IndentAnnotation(Row, Col),
    Equals(Row, Col),
    Colon(Row, Col),
    Ident(Row, Col),
    ElmStyleFunction(Region, Row, Col),
    MalformedPattern(Row, Col),
    QualifiedTag(Row, Col),

    Syntax(&'a SyntaxError<'a>, Row, Col),

    When(When<'a>, Row, Col),
    If(If<'a>, Row, Col),

    Lambda(ELambda<'a>, Row, Col),

    InParens(EInParens<'a>, Row, Col),
    Record(ERecord<'a>, Row, Col),
    Str(EString<'a>, Row, Col),
    Number(Number, Row, Col),
    List(List<'a>, Row, Col),

    IndentStart(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Number {
    End,
    LineTooLong,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EString<'a> {
    Open(Row, Col),

    CodePointOpen(Row, Col),
    CodePointEnd(Row, Col),

    Space(BadInputError, Row, Col),
    EndlessSingle(Row, Col),
    EndlessMulti(Row, Col),
    UnknownEscape(Row, Col),
    Format(&'a SyntaxError<'a>, Row, Col),
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// pub enum Escape {
//     EscapeUnknown,
//     BadUnicodeFormat(u16),
//     BadUnicodeCode(u16),
//     BadUnicodeLength(u16, i32, i32),
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ERecord<'a> {
    End(Row, Col),
    Open(Row, Col),

    Updateable(Row, Col),
    Field(Row, Col),
    Colon(Row, Col),
    QuestionMark(Row, Col),
    Bar(Row, Col),
    Ampersand(Row, Col),

    // TODO remove
    Syntax(&'a SyntaxError<'a>, Row, Col),

    Space(BadInputError, Row, Col),

    IndentOpen(Row, Col),
    IndentColon(Row, Col),
    IndentBar(Row, Col),
    IndentAmpersand(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EInParens<'a> {
    End(Row, Col),
    Open(Row, Col),
    ///
    Expr(&'a EExpr<'a>, Row, Col),

    ///
    Space(BadInputError, Row, Col),
    ///
    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ELambda<'a> {
    Space(BadInputError, Row, Col),
    Start(Row, Col),
    Arrow(Row, Col),
    Comma(Row, Col),
    Arg(Row, Col),
    // TODO make EEXpr
    Pattern(EPattern<'a>, Row, Col),
    Body(&'a EExpr<'a>, Row, Col),
    IndentArrow(Row, Col),
    IndentBody(Row, Col),
    IndentArg(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum List<'a> {
    Open(Row, Col),
    End(Row, Col),
    Space(BadInputError, Row, Col),

    Syntax(&'a SyntaxError<'a>, Row, Col),
    Expr(&'a EExpr<'a>, Row, Col),

    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum When<'a> {
    Space(BadInputError, Row, Col),
    When(Row, Col),
    Is(Row, Col),
    Pattern(EPattern<'a>, Row, Col),
    Arrow(Row, Col),
    Bar(Row, Col),

    IfToken(Row, Col),
    IfGuard(&'a EExpr<'a>, Row, Col),

    Condition(&'a EExpr<'a>, Row, Col),
    Branch(&'a EExpr<'a>, Row, Col),
    Syntax(&'a SyntaxError<'a>, Row, Col),

    IndentIs(Row, Col),
    IndentCondition(Row, Col),
    IndentPattern(Row, Col),
    IndentArrow(Row, Col),
    IndentBranch(Row, Col),
    IndentIfGuard(Row, Col),
    PatternAlignment(u16, Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum If<'a> {
    Space(BadInputError, Row, Col),
    If(Row, Col),
    Then(Row, Col),
    Else(Row, Col),
    // TODO make EEXpr
    Condition(&'a EExpr<'a>, Row, Col),
    ThenBranch(&'a EExpr<'a>, Row, Col),
    ElseBranch(&'a EExpr<'a>, Row, Col),
    Syntax(&'a SyntaxError<'a>, Row, Col),

    IndentCondition(Row, Col),
    IndentIf(Row, Col),
    IndentThenToken(Row, Col),
    IndentElseToken(Row, Col),
    IndentThenBranch(Row, Col),
    IndentElseBranch(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPattern<'a> {
    Record(PRecord<'a>, Row, Col),
    Underscore(Row, Col),

    Start(Row, Col),
    End(Row, Col),
    Space(BadInputError, Row, Col),

    PInParens(PInParens<'a>, Row, Col),

    IndentStart(Row, Col),
    IndentEnd(Row, Col),
    AsIndentStart(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PRecord<'a> {
    End(Row, Col),
    Open(Row, Col),

    Field(Row, Col),
    Colon(Row, Col),
    Optional(Row, Col),
    Pattern(&'a EPattern<'a>, Row, Col),
    // TODO remove
    Syntax(&'a SyntaxError<'a>, Row, Col),

    Space(BadInputError, Row, Col),

    IndentOpen(Row, Col),
    IndentColon(Row, Col),
    IndentOptional(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PInParens<'a> {
    End(Row, Col),
    Open(Row, Col),
    ///
    // TODO remove
    Syntax(&'a SyntaxError<'a>, Row, Col),

    ///
    Space(BadInputError, Row, Col),
    ///
    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'a> {
    TRecord(TRecord<'a>, Row, Col),
    TTagUnion(TTagUnion<'a>, Row, Col),
    TInParens(TInParens<'a>, Row, Col),
    TApply(TApply, Row, Col),
    TVariable(TVariable, Row, Col),
    TWildcard(Row, Col),
    ///
    TStart(Row, Col),
    TEnd(Row, Col),
    TSpace(BadInputError, Row, Col),
    TFunctionArgument(Row, Col),
    ///
    TIndentStart(Row, Col),
    TIndentEnd(Row, Col),
    TAsIndentStart(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TRecord<'a> {
    End(Row, Col),
    Open(Row, Col),

    Field(Row, Col),
    Colon(Row, Col),
    Optional(Row, Col),
    Type(&'a Type<'a>, Row, Col),

    Space(BadInputError, Row, Col),

    IndentOpen(Row, Col),
    IndentColon(Row, Col),
    IndentOptional(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TTagUnion<'a> {
    End(Row, Col),
    Open(Row, Col),

    Type(&'a Type<'a>, Row, Col),

    Space(BadInputError, Row, Col),

    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TInParens<'a> {
    End(Row, Col),
    Open(Row, Col),
    ///
    Type(&'a Type<'a>, Row, Col),

    ///
    Space(BadInputError, Row, Col),
    ///
    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TApply {
    ///
    StartNotUppercase(Row, Col),
    End(Row, Col),
    Space(BadInputError, Row, Col),
    ///
    DoubleDot(Row, Col),
    TrailingDot(Row, Col),
    StartIsNumber(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TVariable {
    ///
    StartNotLowercase(Row, Col),
    End(Row, Col),
    Space(BadInputError, Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContextStack<'a> {
    Cons(ContextItem, &'a ContextStack<'a>),
    Nil,
}

impl<'a> ContextStack<'a> {
    pub fn uncons(&'a self) -> Option<(ContextItem, &'a Self)> {
        match self {
            ContextStack::Cons(item, rest) => Some((*item, rest)),
            ContextStack::Nil => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContextItem {
    pub line: u32,
    pub column: u16,
    pub context: Attempting,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeadEnd<'a, T> {
    pub line: u32,
    pub column: u16,
    pub problem: T,
    pub context_stack: ContextStack<'a>,
}

/// use std vec to escape the arena's lifetime bound
/// since this is only used when there is in fact an error
/// I think this is fine
#[derive(Debug)]
pub struct ParseProblem<'a, T> {
    pub line: u32,
    pub column: u16,
    pub problem: T,
    pub filename: std::path::PathBuf,
    pub bytes: &'a [u8],
}

pub fn fail<'a, T>() -> impl Parser<'a, T, SyntaxError<'a>> {
    move |_arena, state: State<'a>| Err((NoProgress, SyntaxError::ConditionFailed, state))
}

pub trait Parser<'a, Output, Error> {
    fn parse(&self, _: &'a Bump, _: State<'a>) -> ParseResult<'a, Output, Error>;
}

impl<'a, F, Output, Error> Parser<'a, Output, Error> for F
where
    Error: 'a,
    F: Fn(&'a Bump, State<'a>) -> ParseResult<'a, Output, Error>,
{
    fn parse(&self, arena: &'a Bump, state: State<'a>) -> ParseResult<'a, Output, Error> {
        self(arena, state)
    }
}

pub fn allocated<'a, P, Val, Error>(parser: P) -> impl Parser<'a, &'a Val, Error>
where
    Error: 'a,
    P: Parser<'a, Val, Error>,
    Val: 'a,
{
    move |arena, state: State<'a>| {
        let (progress, answer, state) = parser.parse(arena, state)?;

        Ok((progress, &*arena.alloc(answer), state))
    }
}

pub fn not_followed_by<'a, P, ByParser, By, Val>(
    parser: P,
    by: ByParser,
) -> impl Parser<'a, Val, SyntaxError<'a>>
where
    ByParser: Parser<'a, By, SyntaxError<'a>>,
    P: Parser<'a, Val, SyntaxError<'a>>,
{
    move |arena, state: State<'a>| {
        let original_state = state.clone();

        parser
            .parse(arena, state)
            .and_then(|(progress, answer, state)| {
                let after_parse = state.clone();

                match by.parse(arena, state) {
                    Ok((_, _, _state)) => {
                        Err((NoProgress, SyntaxError::ConditionFailed, original_state))
                    }
                    Err(_) => Ok((progress, answer, after_parse)),
                }
            })
    }
}

pub fn not<'a, P, Val>(parser: P) -> impl Parser<'a, (), SyntaxError<'a>>
where
    P: Parser<'a, Val, SyntaxError<'a>>,
{
    move |arena, state: State<'a>| {
        let original_state = state.clone();

        match parser.parse(arena, state) {
            Ok((_, _, _)) => Err((NoProgress, SyntaxError::ConditionFailed, original_state)),
            Err((_, _, _)) => Ok((NoProgress, (), original_state)),
        }
    }
}

pub fn not_e<'a, P, TE, E, X, Val>(parser: P, to_error: TE) -> impl Parser<'a, (), E>
where
    TE: Fn(Row, Col) -> E,
    P: Parser<'a, Val, X>,
    E: 'a,
{
    move |arena, state: State<'a>| {
        let original_state = state.clone();

        match parser.parse(arena, state) {
            Ok((_, _, _)) => Err((
                NoProgress,
                to_error(original_state.line, original_state.column),
                original_state,
            )),
            Err((_, _, _)) => Ok((NoProgress, (), original_state)),
        }
    }
}

pub fn lookahead<'a, Peek, P, PeekVal, Val, Error>(
    peek: Peek,
    parser: P,
) -> impl Parser<'a, Val, Error>
where
    Error: 'a,
    Peek: Parser<'a, PeekVal, Error>,
    P: Parser<'a, Val, Error>,
{
    move |arena, state: State<'a>| {
        let original_state = state.clone();

        peek.parse(arena, state)
            .and_then(|_| parser.parse(arena, original_state))
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
    move |arena, state| {
        parser
            .parse(arena, state)
            .and_then(|(progress, output, next_state)| {
                transform(progress, output).parse(arena, next_state)
            })
    }
}

pub fn and_then_with_indent_level<'a, P1, P2, F, Before, After, E>(
    parser: P1,
    transform: F,
) -> impl Parser<'a, After, E>
where
    P1: Parser<'a, Before, E>,
    P2: Parser<'a, After, E>,
    F: Fn(Progress, Before, u16) -> P2,
    E: 'a,
{
    move |arena, state| {
        parser
            .parse(arena, state)
            .and_then(|(progress, output, next_state)| {
                transform(progress, output, next_state.indent_col).parse(arena, next_state)
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
    move |arena, state| {
        parser
            .parse(arena, state)
            .and_then(|(progress, output, next_state)| {
                transform(arena, next_state, progress, output)
            })
    }
}

pub fn unexpected_eof<'a>(
    _arena: &'a Bump,
    state: State<'a>,
    chars_consumed: usize,
) -> (Progress, SyntaxError<'a>, State<'a>) {
    checked_unexpected(state, chars_consumed, SyntaxError::Eof)
}

pub fn unexpected(
    chars_consumed: usize,
    _attempting: Attempting,
    state: State,
) -> (Progress, SyntaxError, State) {
    // NOTE state is the last argument because chars_consumed often depends on the state's fields
    // having state be the final argument prevents borrowing issues
    checked_unexpected(state, chars_consumed, |region| {
        SyntaxError::Unexpected(region)
    })
}

/// Check for line overflow, then compute a new Region based on chars_consumed
/// and provide it as a way to construct a Problem.
/// If maximum line length was exceeded, return a Problem indicating as much.
#[inline(always)]
fn checked_unexpected<'a, F>(
    state: State<'a>,
    chars_consumed: usize,
    problem_from_region: F,
) -> (Progress, SyntaxError<'a>, State<'a>)
where
    F: FnOnce(Region) -> SyntaxError<'a>,
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

            (Progress::NoProgress, problem_from_region(region), state)
        }
        _ => {
            let (_progress, fail, state) = line_too_long(state);
            (Progress::NoProgress, fail, state)
        }
    }
}

fn line_too_long_e<TE, E>(state: State, to_error: TE) -> (Progress, E, State)
where
    TE: Fn(Row, Col) -> E,
{
    let problem = to_error(state.line, state.column);
    // Set column to MAX and advance the parser to end of input.
    // This way, all future parsers will fail on EOF, and then
    // unexpected_eof will take them back here - thus propagating
    // the initial LineTooLong error all the way to the end, even if
    // (for example) the LineTooLong initially occurs in the middle of
    // a one_of chain, which would otherwise prevent it from propagating.
    let column = u16::MAX;
    let bytes = state.bytes.get(0..state.bytes.len()).unwrap();
    let state = State {
        bytes,
        line: state.line,
        column,
        ..state
    };

    // TODO do we make progress in this case?
    // isn't this error fatal?
    (Progress::NoProgress, problem, state)
}

fn line_too_long(state: State) -> (Progress, SyntaxError, State) {
    line_too_long_e(state, |line, _| SyntaxError::LineTooLong(line))
}

/// A single ASCII char that isn't a newline.
/// (For newlines, use newline_char(), which handles line numbers)
pub fn ascii_char<'a>(expected: u8) -> impl Parser<'a, (), SyntaxError<'a>> {
    // Make sure this really is not a newline!
    debug_assert_ne!(expected, b'\n');

    move |arena, state: State<'a>| match state.bytes.first() {
        Some(&actual) if expected == actual => Ok((
            Progress::MadeProgress,
            (),
            state.advance_without_indenting(1)?,
        )),
        Some(_) => Err(unexpected(0, Attempting::Keyword, state)),
        _ => Err(unexpected_eof(arena, state, 0)),
    }
}

/// A single '\n' character.
/// Use this instead of ascii_char('\n') because it properly handles
/// incrementing the line number.
pub fn newline_char<'a>() -> impl Parser<'a, (), SyntaxError<'a>> {
    move |arena, state: State<'a>| match state.bytes.first() {
        Some(b'\n') => Ok((Progress::MadeProgress, (), state.newline(arena)?)),
        Some(_) => Err(unexpected(0, Attempting::Keyword, state)),
        _ => Err(unexpected_eof(arena, state, 0)),
    }
}

/// One or more ASCII hex digits. (Useful when parsing unicode escape codes,
/// which must consist entirely of ASCII hex digits.)
pub fn ascii_hex_digits<'a>() -> impl Parser<'a, &'a str, SyntaxError<'a>> {
    move |arena, state: State<'a>| {
        let mut buf = bumpalo::collections::String::new_in(arena);

        for &byte in state.bytes.iter() {
            if (byte as char).is_ascii_hexdigit() {
                buf.push(byte as char);
            } else if buf.is_empty() {
                // We didn't find any hex digits!
                return Err(unexpected(0, Attempting::Keyword, state));
            } else {
                let state = state.advance_without_indenting(buf.len())?;

                return Ok((Progress::MadeProgress, buf.into_bump_str(), state));
            }
        }

        Err(unexpected_eof(arena, state, 0))
    }
}

/// A single UTF-8-encoded char. This will both parse *and* validate that the
/// char is valid UTF-8, but it will *not* advance the state.
pub fn peek_utf8_char<'a>(state: &State) -> Result<(char, usize), SyntaxError<'a>> {
    if !state.bytes.is_empty() {
        match char::from_utf8_slice_start(state.bytes) {
            Ok((ch, len_utf8)) => Ok((ch, len_utf8)),
            Err(_) => Err(SyntaxError::BadUtf8),
        }
    } else {
        Err(SyntaxError::Eof(
            Region::zero(), /* TODO get a better region */
        ))
    }
}

/// A single UTF-8-encoded char. This will both parse *and* validate that the
/// char is valid UTF-8, but it will *not* advance the state.
pub fn peek_utf8_char_e<EOF, TE, E>(
    state: &State,
    end_of_file: EOF,
    to_error: TE,
) -> Result<(char, usize), E>
where
    TE: Fn(BadInputError, Row, Col) -> E,
    EOF: Fn(Row, Col) -> E,
{
    if !state.bytes.is_empty() {
        match char::from_utf8_slice_start(state.bytes) {
            Ok((ch, len_utf8)) => Ok((ch, len_utf8)),
            Err(_) => Err(to_error(BadInputError::BadUtf8, state.line, state.column)),
        }
    } else {
        Err(end_of_file(state.line, state.column))
    }
}

/// A single UTF-8-encoded char, with an offset. This will both parse *and*
/// validate that the char is valid UTF-8, but it will *not* advance the state.
pub fn peek_utf8_char_at<'a>(
    state: &State,
    offset: usize,
) -> Result<(char, usize), SyntaxError<'a>> {
    if state.bytes.len() > offset {
        let bytes = &state.bytes[offset..];

        match char::from_utf8_slice_start(bytes) {
            Ok((ch, len_utf8)) => Ok((ch, len_utf8)),
            Err(_) => Err(SyntaxError::BadUtf8),
        }
    } else {
        Err(SyntaxError::Eof(
            Region::zero(), /* TODO get a better region */
        ))
    }
}

pub fn keyword<'a>(
    keyword: &'static str,
    _min_indent: u16,
) -> impl Parser<'a, (), SyntaxError<'a>> {
    move |arena, state: State<'a>| {
        let initial_state = state.clone();
        // first parse the keyword characters
        let (_, _, after_keyword_state) = ascii_string(keyword).parse(arena, state)?;

        // then we must have at least one space character
        // TODO this is potentially wasteful if there are a lot of spaces
        match peek_utf8_char(&after_keyword_state) {
            Ok((next, _width)) if next == ' ' || next == '#' || next == '\n' => {
                // give back the state after parsing the keyword, but before the whitespace
                // that way we can attach the whitespace to whatever follows
                Ok((MadeProgress, (), after_keyword_state))
            }
            _ => {
                // this is not a keyword, maybe it's `whence` or `iffy`
                // anyway, make no progress and return the initial state
                // so we can try something else
                Err((NoProgress, SyntaxError::ConditionFailed, initial_state))
            }
        }
    }
}

pub fn keyword_e<'a, ToError, E>(keyword: &'static str, if_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Row, Col) -> E,
    E: 'a,
{
    move |arena, state: State<'a>| {
        let initial_state = state.clone();
        // first parse the keyword characters
        let (_, _, after_keyword_state) = ascii_string(keyword)
            .parse(arena, state)
            .map_err(|(_, _, state)| (NoProgress, if_error(state.line, state.column), state))?;

        // then we must have at least one space character
        // TODO this is potentially wasteful if there are a lot of spaces
        match peek_utf8_char(&after_keyword_state) {
            Ok((next, _width)) if next == ' ' || next == '#' || next == '\n' => {
                // give back the state after parsing the keyword, but before the whitespace
                // that way we can attach the whitespace to whatever follows
                Ok((MadeProgress, (), after_keyword_state))
            }
            _ => {
                // this is not a keyword, maybe it's `whence` or `iffy`
                // anyway, make no progress and return the initial state
                // so we can try something else
                Err((
                    NoProgress,
                    if_error(initial_state.line, initial_state.column),
                    initial_state,
                ))
            }
        }
    }
}

/// A hardcoded string with no newlines, consisting only of ASCII characters
pub fn ascii_string<'a>(keyword: &'static str) -> impl Parser<'a, (), SyntaxError<'a>> {
    // Verify that this really is exclusively ASCII characters.
    // The `unsafe` block in this function relies upon this assumption!
    //
    // Also, this can't have newlines because we don't attempt to advance
    // the row in the state, only the column.
    debug_assert!(keyword.chars().all(|ch| ch.len_utf8() == 1 && ch != '\n'));

    move |_arena, state: State<'a>| {
        let len = keyword.len();

        // TODO do this comparison in one SIMD instruction (on supported systems)
        if state.bytes.starts_with(keyword.as_bytes()) {
            Ok((
                Progress::MadeProgress,
                (),
                state.advance_without_indenting(len)?,
            ))
        } else {
            let (_, fail, state) = unexpected(len, Attempting::Keyword, state);
            Err((NoProgress, fail, state))
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
    move |arena, state: State<'a>| {
        let start_bytes_len = state.bytes.len();

        match parser.parse(arena, state) {
            Ok((elem_progress, first_output, next_state)) => {
                // in practice, we want elements to make progress
                debug_assert_eq!(elem_progress, MadeProgress);

                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    match delimiter.parse(arena, state) {
                        Ok((_, (), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state) {
                                Ok((element_progress, next_output, next_state)) => {
                                    // in practice, we want elements to make progress
                                    debug_assert_eq!(element_progress, MadeProgress);

                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((_, fail, state)) => {
                                    // If the delimiter parsed, but the following
                                    // element did not, that's a fatal error.
                                    let progress =
                                        Progress::from_lengths(start_bytes_len, state.bytes.len());

                                    return Err((progress, fail, state));
                                }
                            }
                        }
                        Err((delim_progress, fail, old_state)) => match delim_progress {
                            MadeProgress => return Err((MadeProgress, fail, old_state)),
                            NoProgress => return Ok((NoProgress, buf, old_state)),
                        },
                    }
                }
            }
            Err((element_progress, fail, new_state)) => match element_progress {
                MadeProgress => Err((MadeProgress, fail, new_state)),
                NoProgress => Ok((NoProgress, Vec::new_in(arena), new_state)),
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
    move |arena, state: State<'a>| {
        let start_bytes_len = state.bytes.len();

        match parser.parse(arena, state) {
            Ok((progress, first_output, next_state)) => {
                // in practice, we want elements to make progress
                debug_assert_eq!(progress, MadeProgress);
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    match delimiter.parse(arena, state) {
                        Ok((_, (), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state) {
                                Ok((element_progress, next_output, next_state)) => {
                                    // in practice, we want elements to make progress
                                    debug_assert_eq!(element_progress, MadeProgress);

                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((_, _fail, old_state)) => {
                                    // If the delimiter parsed, but the following
                                    // element did not, that means we saw a trailing comma
                                    let progress = Progress::from_lengths(
                                        start_bytes_len,
                                        old_state.bytes.len(),
                                    );
                                    return Ok((progress, buf, old_state));
                                }
                            }
                        }
                        Err((delim_progress, fail, old_state)) => match delim_progress {
                            MadeProgress => return Err((MadeProgress, fail, old_state)),
                            NoProgress => return Ok((NoProgress, buf, old_state)),
                        },
                    }
                }
            }
            Err((element_progress, fail, new_state)) => match element_progress {
                MadeProgress => Err((MadeProgress, fail, new_state)),
                NoProgress => Ok((NoProgress, Vec::new_in(arena), new_state)),
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
    move |arena, state: State<'a>| {
        let start_bytes_len = state.bytes.len();

        match parser.parse(arena, state) {
            Ok((progress, first_output, next_state)) => {
                debug_assert_eq!(progress, MadeProgress);
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    match delimiter.parse(arena, state) {
                        Ok((_, (), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state) {
                                Ok((_, next_output, next_state)) => {
                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((element_progress, fail, state)) => {
                                    return Err((element_progress, fail, state));
                                }
                            }
                        }
                        Err((delim_progress, fail, old_state)) => {
                            match delim_progress {
                                MadeProgress => {
                                    // fail if the delimiter made progress
                                    return Err((MadeProgress, fail, old_state));
                                }
                                NoProgress => {
                                    let progress = Progress::from_lengths(
                                        start_bytes_len,
                                        old_state.bytes.len(),
                                    );
                                    return Ok((progress, buf, old_state));
                                }
                            }
                        }
                    }
                }
            }
            Err((fail_progress, fail, new_state)) => Err((fail_progress, fail, new_state)),
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
    V: Fn(Row, Col) -> Error,
    Error: 'a,
{
    move |arena, state: State<'a>| {
        let start_bytes_len = state.bytes.len();

        match parser.parse(arena, state) {
            Ok((progress, first_output, next_state)) => {
                debug_assert_eq!(progress, MadeProgress);
                let mut state = next_state;
                let mut buf = Vec::with_capacity_in(1, arena);

                buf.push(first_output);

                loop {
                    match delimiter.parse(arena, state) {
                        Ok((_, (), next_state)) => {
                            // If the delimiter passed, check the element parser.
                            match parser.parse(arena, next_state) {
                                Ok((_, next_output, next_state)) => {
                                    state = next_state;
                                    buf.push(next_output);
                                }
                                Err((MadeProgress, fail, state)) => {
                                    return Err((MadeProgress, fail, state));
                                }
                                Err((NoProgress, _fail, state)) => {
                                    return Err((
                                        NoProgress,
                                        to_element_error(state.line, state.column),
                                        state,
                                    ));
                                }
                            }
                        }
                        Err((delim_progress, fail, old_state)) => {
                            match delim_progress {
                                MadeProgress => {
                                    // fail if the delimiter made progress
                                    return Err((MadeProgress, fail, old_state));
                                }
                                NoProgress => {
                                    let progress = Progress::from_lengths(
                                        start_bytes_len,
                                        old_state.bytes.len(),
                                    );
                                    return Ok((progress, buf, old_state));
                                }
                            }
                        }
                    }
                }
            }

            Err((MadeProgress, fail, state)) => Err((MadeProgress, fail, state)),
            Err((NoProgress, _fail, state)) => Err((
                NoProgress,
                to_element_error(state.line, state.column),
                state,
            )),
        }
    }
}

pub fn fail_when_progress<T, E>(
    progress: Progress,
    fail: E,
    value: T,
    state: State<'_>,
) -> ParseResult<'_, T, E> {
    match progress {
        MadeProgress => Err((MadeProgress, fail, state)),
        NoProgress => Ok((NoProgress, value, state)),
    }
}

pub fn satisfies<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A, SyntaxError<'a>>
where
    P: Parser<'a, A, SyntaxError<'a>>,
    F: Fn(&A) -> bool,
{
    move |arena: &'a Bump, state: State<'a>| match parser.parse(arena, state.clone()) {
        Ok((progress, output, next_state)) if predicate(&output) => {
            Ok((progress, output, next_state))
        }
        Ok((progress, _, _)) | Err((progress, _, _)) => {
            Err((progress, SyntaxError::ConditionFailed, state))
        }
    }
}

pub fn optional<'a, P, T, E>(parser: P) -> impl Parser<'a, Option<T>, E>
where
    P: Parser<'a, T, E>,
    E: 'a,
{
    move |arena: &'a Bump, state: State<'a>| {
        // We have to clone this because if the optional parser fails,
        // we need to revert back to the original state.
        let original_state = state.clone();

        match parser.parse(arena, state) {
            Ok((progress, out1, state)) => Ok((progress, Some(out1), state)),
            Err((_, _, _)) => {
                // NOTE this will backtrack
                // TODO can we get rid of some of the potential backtracking?
                Ok((NoProgress, None, original_state))
            }
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
        move |arena, state: $crate::parser::State<'a>| {
            use roc_region::all::{Located, Region};

            let start_col = state.column;
            let start_line = state.line;

            match $parser.parse(arena, state) {
                Ok((progress, value, state)) => {
                    let end_col = state.column;
                    let end_line = state.line;
                    let region = Region {
                        start_col,
                        start_line,
                        end_col,
                        end_line,
                    };

                    Ok((progress, Located { region, value }, state))
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
        move |arena, state: $crate::parser::State<'a>| {
            let original_state = state.clone();

            match $p1.parse(arena, state) {
                Ok((p1, _, state)) => match $p2.parse(arena, state) {
                    Ok((p2, out2, state)) => Ok((p1.or(p2), out2, state)),
                    Err((p2, fail, _)) => Err((p1.or(p2), fail, original_state)),
                },
                Err((progress, fail, _)) => Err((progress, fail, original_state)),
            }
        }
    };
}

/// If the first one parses, parse the second one; if it also parses, use the
/// output from the first one.
#[macro_export]
macro_rules! skip_second {
    ($p1:expr, $p2:expr) => {
        move |arena, state: $crate::parser::State<'a>| {
            let original_state = state.clone();

            match $p1.parse(arena, state) {
                Ok((p1, out1, state)) => match $p2.parse(arena, state) {
                    Ok((p2, _, state)) => Ok((p1.or(p2), out1, state)),
                    Err((p2, fail, _)) => Err((p1.or(p2), fail, original_state)),
                },
                Err((progress, fail, _)) => Err((progress, fail, original_state)),
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

/// Parse zero or more elements between two braces (e.g. square braces).
/// Elements can be optionally surrounded by spaces, and are separated by a
/// delimiter (e.g comma-separated) with optionally a trailing delimiter.
/// Braces and delimiters get discarded.
#[macro_export]
macro_rules! collection_trailing_sep {
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
                    and!(
                        $crate::parser::trailing_sep_by0(
                            $delimiter,
                            $crate::blankspace::space0_around($elem, $min_indent)
                        ),
                        $crate::blankspace::space0($min_indent)
                    ),
                    $closing_brace
                )
            )
        )
    };
}

#[macro_export]
macro_rules! collection_trailing_sep_e {
    ($opening_brace:expr, $elem:expr, $delimiter:expr, $closing_brace:expr, $min_indent:expr, $open_problem:expr, $space_problem:expr, $indent_problem:expr) => {
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
                zero_or_more!($crate::parser::word1(b' ', |row, col| $space_problem(
                    crate::parser::BadInputError::LineTooLong,
                    row,
                    col
                ))),
                |arena, state| {
                    let (_, elements, state) =
                                    and!(
                                        $crate::parser::trailing_sep_by0(
                                            $delimiter,
                                            $crate::blankspace::space0_around_ee(
                                                $elem,
                                                $min_indent,
                                                $space_problem,
                                                $indent_problem,
                                                $indent_problem
                                            )
                                        ),
                                        $crate::blankspace::space0_e($min_indent, $space_problem, $indent_problem)
                                    ).parse(arena, state)?;

                    let (_,_, state) =
                            if elements.0.is_empty() {
                                one_of_with_error![$open_problem; $closing_brace].parse(arena, state)?
                            } else {
                                $closing_brace.parse(arena, state)?
                            };


                    Ok((MadeProgress, elements, state))
                }
            )
        )
    };
}

#[macro_export]
macro_rules! and {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::parser::State<'a>| {
            // We have to clone this because if the first parser passes and then
            // the second one fails, we need to revert back to the original state.
            let original_state = state.clone();

            match $p1.parse(arena, state) {
                Ok((p1, out1, state)) => match $p2.parse(arena, state) {
                    Ok((p2, out2, state)) => Ok((p1.or(p2), (out1, out2), state)),
                    Err((p2, fail, _)) => Err((p1.or(p2), fail, original_state)),
                },
                Err((progress, fail, state)) => Err((progress, fail, state)),
            }
        }
    };
}

#[macro_export]
macro_rules! one_of {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::parser::State<'a>| {

            match $p1.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((MadeProgress, fail, state)) => Err((MadeProgress, fail, state)),
                Err((NoProgress, _, state)) => $p2.parse( arena, state),
            }
        }
    };

    ($p1:expr, $($others:expr),+) => {
        one_of!($p1, one_of!($($others),+))
    };
}

#[macro_export]
macro_rules! one_of_with_error {
    ($toerror:expr; $p1:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::parser::State<'a>| {

            match $p1.parse(arena, state) {
                valid @ Ok(_) => valid,
                Err((MadeProgress, fail, state)) => Err((MadeProgress, fail, state )),
                Err((NoProgress, _, state)) => Err((MadeProgress, $toerror(state.line, state.column), state)),
            }
        }
    };

    ($toerror:expr; $p1:expr, $($others:expr),+) => {
        one_of_with_error!($toerror, $p1, one_of_with_error!($($others),+))
    };
}

pub fn specialize<'a, F, P, T, X, Y>(map_error: F, parser: P) -> impl Parser<'a, T, Y>
where
    F: Fn(X, Row, Col) -> Y,
    P: Parser<'a, T, X>,
    Y: 'a,
{
    move |a, s| match parser.parse(a, s) {
        Ok(t) => Ok(t),
        Err((p, error, s)) => Err((p, map_error(error, s.line, s.column), s)),
    }
}

pub fn specialize_ref<'a, F, P, T, X, Y>(map_error: F, parser: P) -> impl Parser<'a, T, Y>
where
    F: Fn(&'a X, Row, Col) -> Y,
    P: Parser<'a, T, X>,
    Y: 'a,
    X: 'a,
{
    move |a, s| match parser.parse(a, s) {
        Ok(t) => Ok(t),
        Err((p, error, s)) => Err((p, map_error(a.alloc(error), s.line, s.column), s)),
    }
}

pub fn word1<'a, ToError, E>(word: u8, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Row, Col) -> E,
    E: 'a,
{
    debug_assert_ne!(word, b'\n');

    move |_arena: &'a Bump, state: State<'a>| match state.bytes.get(0) {
        Some(x) if *x == word => Ok((
            MadeProgress,
            (),
            State {
                bytes: &state.bytes[1..],
                column: state.column + 1,
                ..state
            },
        )),
        _ => Err((NoProgress, to_error(state.line, state.column), state)),
    }
}

pub fn word2<'a, ToError, E>(word_1: u8, word_2: u8, to_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Row, Col) -> E,
    E: 'a,
{
    debug_assert_ne!(word_1, b'\n');
    debug_assert_ne!(word_2, b'\n');

    let needle = [word_1, word_2];

    move |_arena: &'a Bump, state: State<'a>| {
        if state.bytes.starts_with(&needle) {
            Ok((
                MadeProgress,
                (),
                State {
                    bytes: &state.bytes[2..],
                    column: state.column + 2,
                    ..state
                },
            ))
        } else {
            Err((NoProgress, to_error(state.line, state.column), state))
        }
    }
}

pub fn check_indent<'a, TE, E>(min_indent: u16, to_problem: TE) -> impl Parser<'a, (), E>
where
    TE: Fn(Row, Col) -> E,
    E: 'a,
{
    move |_arena, state: State<'a>| {
        dbg!(state.indent_col, min_indent);
        if state.indent_col < min_indent {
            Err((NoProgress, to_problem(state.line, state.column), state))
        } else {
            Ok((NoProgress, (), state))
        }
    }
}

#[macro_export]
macro_rules! word1_check_indent {
    ($word:expr, $word_problem:expr, $min_indent:expr, $indent_problem:expr) => {
        and!(
            word1($word, $word_problem),
            crate::parser::check_indent($min_indent, $indent_problem)
        )
    };
}

#[allow(dead_code)]
fn in_context<'a, AddContext, P1, P2, Start, A, X, Y>(
    add_context: AddContext,
    parser_start: P1,
    parser_rest: P2,
) -> impl Parser<'a, A, Y>
where
    AddContext: Fn(X, Row, Col) -> Y,
    P1: Parser<'a, Start, Y>,
    P2: Parser<'a, A, X>,
    Y: 'a,
{
    move |arena, state| {
        let (_, _, state) = parser_start.parse(arena, state)?;

        match parser_rest.parse(arena, state) {
            Ok((progress, value, state)) => Ok((progress, value, state)),
            Err((progress, fail, state)) => {
                Err((progress, add_context(fail, state.line, state.column), state))
            }
        }
    }
}

#[macro_export]
macro_rules! map {
    ($parser:expr, $transform:expr) => {
        move |arena, state| {
            $parser
                .parse(arena, state)
                .map(|(progress, output, next_state)| (progress, $transform(output), next_state))
        }
    };
}

#[macro_export]
macro_rules! map_with_arena {
    ($parser:expr, $transform:expr) => {
        move |arena, state| {
            $parser
                .parse(arena, state)
                .map(|(progress, output, next_state)| {
                    (progress, $transform(arena, output), next_state)
                })
        }
    };
}

#[macro_export]
macro_rules! zero_or_more {
    ($parser:expr) => {
        move |arena, state: State<'a>| {
            use bumpalo::collections::Vec;

            let start_bytes_len = state.bytes.len();

            match $parser.parse(arena, state) {
                Ok((_, first_output, next_state)) => {
                    let mut state = next_state;
                    let mut buf = Vec::with_capacity_in(1, arena);

                    buf.push(first_output);

                    loop {
                        match $parser.parse(arena, state) {
                            Ok((_, next_output, next_state)) => {
                                state = next_state;
                                buf.push(next_output);
                            }
                            Err((fail_progress, fail, old_state)) => {
                                match fail_progress {
                                    MadeProgress => {
                                        // made progress on an element and then failed; that's an error
                                        return Err((MadeProgress, fail, old_state));
                                    }
                                    NoProgress => {
                                        // the next element failed with no progress
                                        // report whether we made progress before
                                        let progress = Progress::from_lengths(start_bytes_len, old_state.bytes.len());
                                        return Ok((progress, buf, old_state));
                                    }
                                }
                            }
                        }
                    }
                }
                Err((fail_progress, fail, new_state)) => {
                    match fail_progress {
                        MadeProgress => {
                            // made progress on an element and then failed; that's an error
                            Err((MadeProgress, fail, new_state))
                        }
                        NoProgress => {
                            // the first element failed (with no progress), but that's OK
                            // because we only need to parse 0 elements
                            Ok((NoProgress, Vec::new_in(arena), new_state))
                        }
                    }
                }
            }
        }
    };
}

#[macro_export]
macro_rules! one_or_more {
    ($parser:expr) => {
        move |arena, state: State<'a>| {
            use bumpalo::collections::Vec;

            match $parser.parse(arena, state) {
                Ok((_, first_output, next_state)) => {
                    let mut state = next_state;
                    let mut buf = Vec::with_capacity_in(1, arena);

                    buf.push(first_output);

                    loop {
                        match $parser.parse(arena, state) {
                            Ok((_, next_output, next_state)) => {
                                state = next_state;
                                buf.push(next_output);
                            }
                            Err((progress, fail, old_state)) => {
                                return $crate::parser::fail_when_progress(
                                    progress, fail, buf, old_state,
                                )
                            }
                        }
                    }
                }
                Err((progress, _, new_state)) => {
                    debug_assert_eq!(progress, NoProgress, "{:?}", &new_state);
                    Err($crate::parser::unexpected_eof(arena, new_state, 0))
                }
            }
        }
    };
}

#[macro_export]
macro_rules! debug {
    ($parser:expr) => {
        move |arena, state: $crate::parser::State<'a>| dbg!($parser.parse(arena, state))
    };
}

#[macro_export]
macro_rules! attempt {
    ($attempting:expr, $parser:expr) => {
        move |arena: &'a Bump, mut state: $crate::parser::State<'a>| {
            let item = $crate::parser::ContextItem {
                context: $attempting,
                line: state.line,
                column: state.column,
            };

            state.context_stack = arena.alloc($crate::parser::ContextStack::Cons(
                item,
                state.context_stack,
            ));

            $parser
                .parse(arena, state)
                .map(|(progress, answer, mut state)| {
                    // If the parser suceeded, go back to what we were originally attempting.
                    // (If it failed, that's exactly where we care what we were attempting!)
                    match state.context_stack.uncons() {
                        Some((_item, rest)) => {
                            state.context_stack = rest;
                        }
                        None => unreachable!("context stack contains at least one element"),
                    }

                    (progress, answer, state)
                })
        }
    };
}

#[macro_export]
macro_rules! either {
    ($p1:expr, $p2:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::parser::State<'a>| match $p1
            .parse(arena, state)
        {
            Ok((progress, output, state)) => {
                Ok((progress, $crate::parser::Either::First(output), state))
            }
            Err((NoProgress, _, state)) => match $p2.parse(arena, state) {
                Ok((progress, output, state)) => {
                    Ok((progress, $crate::parser::Either::Second(output), state))
                }
                Err((progress, fail, state)) => Err((progress, fail, state)),
            },
            Err((MadeProgress, fail, state)) => Err((MadeProgress, fail, state)),
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
pub fn loc<'a, P, Val, Error>(parser: P) -> impl Parser<'a, Located<Val>, Error>
where
    P: Parser<'a, Val, Error>,
    Error: 'a,
{
    loc!(parser)
}

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn map<'a, P, F, Before, After, E>(parser: P, transform: F) -> impl Parser<'a, After, E>
where
    P: Parser<'a, Before, E>,
    F: Fn(Before) -> After,
    E: 'a,
{
    map!(parser, transform)
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

/// For some reason, some usages won't compile unless they use this instead of the macro version
#[inline(always)]
pub fn attempt<'a, P, Val, Error>(attempting: Attempting, parser: P) -> impl Parser<'a, Val, Error>
where
    P: Parser<'a, Val, Error>,
    Error: 'a,
{
    attempt!(attempting, parser)
}

pub fn parse_utf8<'a>(bytes: &[u8]) -> Result<&str, SyntaxError<'a>> {
    match from_utf8(bytes) {
        Ok(string) => Ok(string),
        Err(_) => Err(SyntaxError::BadUtf8),
    }
}

pub fn end_of_file<'a>() -> impl Parser<'a, (), SyntaxError<'a>> {
    |_arena: &'a Bump, state: State<'a>| {
        if state.has_reached_end() {
            Ok((NoProgress, (), state))
        } else {
            Err((NoProgress, SyntaxError::ConditionFailed, state))
        }
    }
}

pub fn backtrackable<'a, P, Val, Error>(parser: P) -> impl Parser<'a, Val, Error>
where
    P: Parser<'a, Val, Error>,
    Error: 'a,
{
    move |arena: &'a Bump, state: State<'a>| {
        let old_state = state.clone();

        match parser.parse(arena, state) {
            Ok((_, a, s1)) => Ok((NoProgress, a, s1)),
            Err((_, f, _)) => Err((NoProgress, f, old_state)),
        }
    }
}
