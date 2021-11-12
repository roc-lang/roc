use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_region::all::{Located, Position, Region};
use std::fmt;
use Progress::*;

/// A position in a source file.
#[derive(Clone, Copy, PartialEq, Eq)]
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum Either<First, Second> {
    First(First),
    Second(Second),
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
    Todo,
    Type(EType<'a>),
    Pattern(EPattern<'a>),
    Expr(EExpr<'a>),
    Header(EHeader<'a>),
    Space(BadInputError),
    NotEndOfFile(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EHeader<'a> {
    Provides(EProvides<'a>, Row, Col),
    Exposes(EExposes, Row, Col),
    Imports(EImports, Row, Col),
    Requires(ERequires<'a>, Row, Col),
    Packages(EPackages<'a>, Row, Col),
    Effects(EEffects<'a>, Row, Col),

    Space(BadInputError, Row, Col),
    Start(Row, Col),
    ModuleName(Row, Col),
    AppName(EString<'a>, Row, Col),
    PlatformName(EPackageName, Row, Col),
    IndentStart(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EProvides<'a> {
    Provides(Row, Col),
    Open(Row, Col),
    To(Row, Col),
    IndentProvides(Row, Col),
    IndentTo(Row, Col),
    IndentListStart(Row, Col),
    IndentListEnd(Row, Col),
    IndentPackage(Row, Col),
    ListStart(Row, Col),
    ListEnd(Row, Col),
    Identifier(Row, Col),
    Package(EPackageOrPath<'a>, Row, Col),
    Space(BadInputError, Row, Col),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EExposes {
    Exposes(Row, Col),
    Open(Row, Col),
    IndentExposes(Row, Col),
    IndentListStart(Row, Col),
    IndentListEnd(Row, Col),
    ListStart(Row, Col),
    ListEnd(Row, Col),
    Identifier(Row, Col),
    Space(BadInputError, Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ERequires<'a> {
    Requires(Row, Col),
    Open(Row, Col),
    IndentRequires(Row, Col),
    IndentListStart(Row, Col),
    IndentListEnd(Row, Col),
    ListStart(Row, Col),
    ListEnd(Row, Col),
    TypedIdent(ETypedIdent<'a>, Row, Col),
    Rigid(Row, Col),
    Space(BadInputError, Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypedIdent<'a> {
    Space(BadInputError, Row, Col),
    HasType(Row, Col),
    IndentHasType(Row, Col),
    Name(Row, Col),
    Type(EType<'a>, Row, Col),
    IndentType(Row, Col),
    Identifier(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPackages<'a> {
    Open(Row, Col),
    Space(BadInputError, Row, Col),
    Packages(Row, Col),
    IndentPackages(Row, Col),
    ListStart(Row, Col),
    ListEnd(Row, Col),
    IndentListStart(Row, Col),
    IndentListEnd(Row, Col),
    PackageEntry(EPackageEntry<'a>, Row, Col),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EPackageName {
    MissingSlash(Row, Col),
    Account(Row, Col),
    Pkg(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPackageOrPath<'a> {
    BadPath(EString<'a>, Row, Col),
    BadPackage(EPackageName, Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPackageEntry<'a> {
    BadPackageOrPath(EPackageOrPath<'a>, Row, Col),
    Shorthand(Row, Col),
    Colon(Row, Col),
    IndentPackageOrPath(Row, Col),
    Space(BadInputError, Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EEffects<'a> {
    Space(BadInputError, Row, Col),
    Effects(Row, Col),
    Open(Row, Col),
    IndentEffects(Row, Col),
    ListStart(Row, Col),
    ListEnd(Row, Col),
    IndentListStart(Row, Col),
    IndentListEnd(Row, Col),
    TypedIdent(ETypedIdent<'a>, Row, Col),
    ShorthandDot(Row, Col),
    Shorthand(Row, Col),
    TypeName(Row, Col),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EImports {
    Open(Row, Col),
    Imports(Row, Col),
    IndentImports(Row, Col),
    IndentListStart(Row, Col),
    IndentListEnd(Row, Col),
    ListStart(Row, Col),
    ListEnd(Row, Col),
    Identifier(Row, Col),
    ExposingDot(Row, Col),
    ShorthandDot(Row, Col),
    Shorthand(Row, Col),
    ModuleName(Row, Col),
    Space(BadInputError, Row, Col),
    IndentSetStart(Row, Col),
    IndentSetEnd(Row, Col),
    SetStart(Row, Col),
    SetEnd(Row, Col),
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
        prefix: &'a str,
        bytes: &'a [u8],
    ) -> ParseProblem<'a, SyntaxError<'a>> {
        ParseProblem {
            line: 0,
            column: 0,
            problem: self,
            filename,
            bytes,
            prefix,
        }
    }
}

pub type Row = u32;
pub type Col = u16;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EExpr<'a> {
    Start(Row, Col),
    End(Row, Col),
    BadExprEnd(Row, Col),
    Space(BadInputError, Row, Col),

    Dot(Row, Col),
    Access(Row, Col),
    UnaryNot(Row, Col),
    UnaryNegate(Row, Col),
    BadOperator(&'a [u8], Row, Col),

    DefMissingFinalExpr(Row, Col),
    DefMissingFinalExpr2(&'a EExpr<'a>, Row, Col),
    Type(EType<'a>, Row, Col),
    Pattern(&'a EPattern<'a>, Row, Col),
    IndentDefBody(Row, Col),
    IndentEquals(Row, Col),
    IndentAnnotation(Row, Col),
    Equals(Row, Col),
    Colon(Row, Col),
    DoubleColon(Row, Col),
    Ident(Row, Col),
    ElmStyleFunction(Region, Row, Col),
    MalformedPattern(Row, Col),
    QualifiedTag(Row, Col),
    BackpassComma(Row, Col),
    BackpassArrow(Row, Col),

    When(EWhen<'a>, Row, Col),
    If(EIf<'a>, Row, Col),

    Expect(EExpect<'a>, Row, Col),

    Lambda(ELambda<'a>, Row, Col),
    Underscore(Row, Col),

    InParens(EInParens<'a>, Row, Col),
    Record(ERecord<'a>, Row, Col),
    Str(EString<'a>, Row, Col),
    Number(ENumber, Row, Col),
    List(EList<'a>, Row, Col),

    IndentStart(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ENumber {
    End,
    LineTooLong,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EString<'a> {
    Open(Row, Col),

    CodePtOpen(Row, Col),
    CodePtEnd(Row, Col),

    Space(BadInputError, Row, Col),
    EndlessSingle(Row, Col),
    EndlessMulti(Row, Col),
    UnknownEscape(Row, Col),
    Format(&'a EExpr<'a>, Row, Col),
    FormatEnd(Row, Col),
}

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
    Expr(&'a EExpr<'a>, Row, Col),

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
pub enum EList<'a> {
    Open(Row, Col),
    End(Row, Col),
    Space(BadInputError, Row, Col),

    Expr(&'a EExpr<'a>, Row, Col),

    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EWhen<'a> {
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

    IndentIs(Row, Col),
    IndentCondition(Row, Col),
    IndentPattern(Row, Col),
    IndentArrow(Row, Col),
    IndentBranch(Row, Col),
    IndentIfGuard(Row, Col),
    PatternAlignment(u16, Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EIf<'a> {
    Space(BadInputError, Row, Col),
    If(Row, Col),
    Then(Row, Col),
    Else(Row, Col),
    // TODO make EEXpr
    Condition(&'a EExpr<'a>, Row, Col),
    ThenBranch(&'a EExpr<'a>, Row, Col),
    ElseBranch(&'a EExpr<'a>, Row, Col),

    IndentCondition(Row, Col),
    IndentIf(Row, Col),
    IndentThenToken(Row, Col),
    IndentElseToken(Row, Col),
    IndentThenBranch(Row, Col),
    IndentElseBranch(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EExpect<'a> {
    Space(BadInputError, Row, Col),
    Expect(Row, Col),
    Condition(&'a EExpr<'a>, Row, Col),
    Continuation(&'a EExpr<'a>, Row, Col),
    IndentCondition(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EPattern<'a> {
    Record(PRecord<'a>, Row, Col),
    Underscore(Row, Col),

    Start(Row, Col),
    End(Row, Col),
    Space(BadInputError, Row, Col),

    PInParens(PInParens<'a>, Row, Col),
    NumLiteral(ENumber, Row, Col),

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
    Expr(&'a EExpr<'a>, Row, Col),

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
    Pattern(&'a EPattern<'a>, Row, Col),

    Space(BadInputError, Row, Col),
    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EType<'a> {
    TRecord(ETypeRecord<'a>, Row, Col),
    TTagUnion(ETypeTagUnion<'a>, Row, Col),
    TInParens(ETypeInParens<'a>, Row, Col),
    TApply(ETypeApply, Row, Col),
    TBadTypeVariable(Row, Col),
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
pub enum ETypeRecord<'a> {
    End(Row, Col),
    Open(Row, Col),

    Field(Row, Col),
    Colon(Row, Col),
    Optional(Row, Col),
    Type(&'a EType<'a>, Row, Col),

    Space(BadInputError, Row, Col),

    IndentOpen(Row, Col),
    IndentColon(Row, Col),
    IndentOptional(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeTagUnion<'a> {
    End(Row, Col),
    Open(Row, Col),

    Type(&'a EType<'a>, Row, Col),

    Space(BadInputError, Row, Col),

    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeInParens<'a> {
    End(Row, Col),
    Open(Row, Col),
    ///
    Type(&'a EType<'a>, Row, Col),

    ///
    Space(BadInputError, Row, Col),
    ///
    IndentOpen(Row, Col),
    IndentEnd(Row, Col),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ETypeApply {
    ///
    StartNotUppercase(Row, Col),
    End(Row, Col),
    Space(BadInputError, Row, Col),
    ///
    DoubleDot(Row, Col),
    TrailingDot(Row, Col),
    StartIsNumber(Row, Col),
}

#[derive(Debug)]
pub struct ParseProblem<'a, T> {
    pub line: u32,
    pub column: u16,
    pub problem: T,
    pub filename: std::path::PathBuf,
    pub bytes: &'a [u8],
    /// prefix is usually the header (for parse problems in the body), or empty
    pub prefix: &'a str,
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

pub fn keyword_e<'a, ToError, E>(keyword: &'static str, if_error: ToError) -> impl Parser<'a, (), E>
where
    ToError: Fn(Row, Col) -> E,
    E: 'a,
{
    move |_, mut state: State<'a>| {
        let width = keyword.len();

        if !state.bytes.starts_with(keyword.as_bytes()) {
            return Err((NoProgress, if_error(state.line, state.column), state));
        }

        // the next character should not be an identifier character
        // to prevent treating `whence` or `iffy` as keywords
        match state.bytes.get(width) {
            Some(next) if *next == b' ' || *next == b'#' || *next == b'\n' => {
                state.column += width as u16;
                state.bytes = &state.bytes[width..];
                Ok((MadeProgress, (), state))
            }
            None => {
                state.column += width as u16;
                state.bytes = &state.bytes[width..];
                Ok((MadeProgress, (), state))
            }
            Some(_) => Err((NoProgress, if_error(state.line, state.column), state)),
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
                                Err((_, fail, state)) => {
                                    return Err((MadeProgress, fail, state));
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

pub fn optional<'a, P, T, E>(parser: P) -> impl Parser<'a, Option<T>, E>
where
    P: Parser<'a, T, E>,
    E: 'a,
{
    move |arena: &'a Bump, state: State<'a>| {
        // We have to clone this because if the optional parser fails,
        // we need to revert back to the original state.
        let original_state = state;

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
                        start_line,
                        end_line,
                        start_col,
                        end_col,
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

#[macro_export]
macro_rules! collection_trailing_sep_e {
    ($opening_brace:expr, $elem:expr, $delimiter:expr, $closing_brace:expr, $min_indent:expr, $open_problem:expr, $space_problem:expr, $indent_problem:expr, $space_before:expr) => {
        skip_first!(
            $opening_brace,
            |arena, state| {
                let (_, spaces, state) = space0_e($min_indent, $space_problem, $indent_problem)
                    .parse(arena, state)?;

                let (_, (mut parsed_elems, mut final_comments), state) =
                                and!(
                                    $crate::parser::trailing_sep_by0(
                                        $delimiter,
                                        $crate::blankspace::space0_before_optional_after(
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
                        if parsed_elems.is_empty() {
                            one_of_with_error![$open_problem; $closing_brace].parse(arena, state)?
                        } else {
                            $closing_brace.parse(arena, state)?
                        };

                if !spaces.is_empty() {
                    if let Some(first) = parsed_elems.first_mut() {
                        first.value = $space_before(arena.alloc(first.value), spaces)
                    } else {
                        assert!(final_comments.is_empty());
                        final_comments = spaces;
                    }
                }

                let collection = $crate::ast::Collection::with_items_and_comments(
                    arena,
                    parsed_elems.into_bump_slice(),
                    final_comments);

                Ok((MadeProgress, collection, state))
            }
        )
    };
}

#[macro_export]
macro_rules! succeed {
    ($value:expr) => {
        move |_arena: &'a bumpalo::Bump, state: $crate::parser::State<'a>| {
            Ok((NoProgress, $value, state))
        }
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
    ($p1:expr, $($others:expr),+ $(,)?) => {
        one_of!($p1, $($others),+)
    };
}

#[macro_export]
macro_rules! maybe {
    ($p1:expr) => {
        move |arena: &'a bumpalo::Bump, state: $crate::parser::State<'a>| match $p1
            .parse(arena, state)
        {
            Ok((progress, value, state)) => Ok((progress, Some(value), state)),
            Err((MadeProgress, fail, state)) => Err((MadeProgress, fail, state)),
            Err((NoProgress, _, state)) => Ok((NoProgress, None, state)),
        }
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
    ($parser:expr, $to_error:expr) => {
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
                            Err((NoProgress, _, old_state)) => {
                                return Ok((MadeProgress, buf, old_state));
                            }
                            Err((MadeProgress, fail, old_state)) => {
                                return Err((MadeProgress, fail, old_state));
                            }
                        }
                    }
                }
                Err((progress, _, new_state)) => Err((
                    progress,
                    $to_error(new_state.line, new_state.column),
                    new_state,
                )),
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
    move |arena: &'a Bump, state: State<'a>| {
        let old_state = state;

        match parser.parse(arena, state) {
            Ok((_, a, s1)) => Ok((NoProgress, a, s1)),
            Err((_, f, _)) => Err((NoProgress, f, old_state)),
        }
    }
}
